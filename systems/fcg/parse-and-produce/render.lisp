(in-package :fcg)

;; ############################################################################
;; public interface:
;; ############################################################################

(export '(extract-struct string gesture stem meets precedes boundaries syn-subunits))

(defun extract-struct (unit-name s)
  "Extracts all units in s starting from unit with unit-name."
  (let ((top (structure-unit s unit-name)))
    (cons top
	  (loop for n in (feature-value (get-subunits-feature top)) append
		(extract-struct n s)))))

(defmethod render ((struct list) (mode (eql :default)) &key &allow-other-keys)
  (new-render struct))

(defmethod render ((cfs coupled-feature-structure) (mode t) &key &allow-other-keys)
  (render cfs :default))

(defmethod render ((cfs coupled-feature-structure) (mode (eql :default)) &key &allow-other-keys)
  (render (right-pole-structure cfs) :default))

(defmethod render ((struct list) (mode (eql :no-shuffle)) &key &allow-other-keys)
  (new-render struct :shuffle nil))

(defmethod render ((cfs coupled-feature-structure) (mode (eql :no-shuffle)) &key &allow-other-keys)
  (new-render (right-pole-structure cfs) :shuffle nil))

(defun get-lexeme (struct lb)
  (let ((r nil))
    (loop for unit in struct until r do
	 (setq r (find lb 
		       (find-all 'lexeme (unit-feature-value unit 'form) :key #'first)
		       :key #'second)))
    r))

;; Rendering happens as follows: First a tree of render-nodes is
;; constructed. Each node corresponds to a unit, and has a string
;; slot, a subnodes slot, a parent slot and an orderings slot. The
;; orderings slot is either t (the subunits may still occur in any
;; order), or a list of allowed orderings.

;; Nodes for units that do not occur in the initial structure but
;; occur in a string specifications are also added to the tree at the
;; same level as the nodes in which it is related to via form constraints.
 
;; Finally, meets and precedes constraints are handled by deleting
;; orderings not allowed by them from the nodes in the tree.

(defstruct (render-node (:conc-name rn-))
  name
  string 
  parent
  subnodes
  (orderings t))

(defmethod print-object ((node render-node) stream)
  (if *print-pretty* 
      (pprint-logical-block (stream nil)
	(format stream "<render-node name ~a,~:_ string ~:a,~:_ parent ~a,~:_ subnodes ~a,~:_ orderings ~a~:_>"
		(rn-name node) (rn-string node)  
		(when (rn-parent node) (rn-name (rn-parent node)))
		(mapcar #'rn-name (rn-subnodes node))
		(rn-orderings node)))
      (format stream "<render-node name ~a,~:_ string ~:a,~:_ parent ~a,~:_ subnodes ~a,~:_ orderings ~a~:_>"
	      (rn-name node) (rn-string node)  
	      (when (rn-parent node) (rn-name (rn-parent node)))
	      (mapcar #'rn-name (rn-subnodes node))
	      (rn-orderings node))))

(defun print-nodes (from-node)
  (format t "~%--------------------------------------")
  (pprint from-node t)
  (dolist (sn (rn-subnodes from-node))
    (print-nodes sn)))

(defun render-upnodes (node)
  "Return all nodes starting from node-name up to the topnode in that
order."
  (if (rn-parent node) 
      (cons node (render-upnodes (rn-parent node)))
      (list node)))

(defun render-branches (node1 node2)
  "Return the node in which the paths to node1 and node2 branch,
together with the downward paths from the branching node to the
respective input nodes as lists with first elements the subnodes of the
branching node."
  (let ((un1 (when node1 (render-upnodes node1)))
	(un2 (when node2 (render-upnodes node2)))
	(r nil))
    (cond ((find node1 un2)
	   (values node1 
		   nil
		   (nreverse (subseq un2 0 (position node1 un2)))))
	  ((find node2 un1)
	   (values node2 
		   (nreverse (subseq un1 0 (position node2 un1)))
		   nil))
	  (t (loop for n1 in un1 until r do
                   (when (find n1 un2) (setq r n1)))
	     (values r 
		     (nreverse (subseq un1 0 (position r un1)))
		     (nreverse (subseq un2 0 (position r un2))))))))

(defun find-render-node (name from-node)
  (if (eq name (rn-name from-node))
    from-node
    (let ((r nil))
      (loop for subnode in (rn-subnodes from-node) 
            until r
            do (setq r (find-render-node name subnode)))
      r)))

(defun structure->render-node (structure from-unit &optional parent-node)
  (when from-unit
    (let ((result (make-render-node :name (unit-name from-unit)
				    :parent parent-node)))
      (setf (rn-subnodes result)
	    (mapcar #'(lambda (unit-name)
			(or (structure->render-node structure 
						    (structure-unit structure unit-name)
						    result)
			    (make-render-node :name unit-name
					      :parent result)))
		    (feature-value (get-subunits-feature from-unit))))
      result)))

(defun render-add-strings (structure top-node from-node from-unit &key (stems? t))
  (declare (ignore from-unit from-node))
  #+dbg
  (assert (eq (rn-name from-node)
	      (unit-name from-unit)))
  (let ((strings (extract-strings structure))
	(stems (extract-stems structure))
	(or-strings (extract-or-strings structure))
	(or-stems (extract-or-stems structure))
	(meets (extract-meets-constraints structure))
	(precedes (extract-precedes-constraints structure)))
    (loop for seq in (if stems?
                       (list strings or-strings stems or-stems)
                       (list strings or-strings)) do
          (dolist (s seq)
            (let ((n (find-render-node (second s) top-node)))
              (cond ((not n)
                     (loop for c in (find-all (second s) meets :key #'second) 
                           until n do (setq n (find-render-node (third c) top-node)))
                     (loop for c in (find-all (second s) meets :key #'third) 
                           until n do (setq n (find-render-node (second c) top-node)))
                     (loop for c in (find-all (second s) precedes :key #'second) 
                           until n do (setq n (find-render-node (third c) top-node)))
                     (loop for c in (find-all (second s) precedes :key #'third) 
                           until n do (setq n (find-render-node (second c) top-node)))
                     (push (make-render-node :name (second s) 
                                             :string s 
                                             :parent (if n 
                                                       n ;; natural line broke
                                                       ;;(rn-parent n) ;; natural line works
                                                       top-node
                                                       ))
                           (rn-subnodes (if n 
                                          n 
                                          ;;(rn-parent n) 
                                          top-node))))
                    ((null (rn-string n))
                     (setf (rn-string n) s))))))))

(defun maybe-init-orderings (rn)
  (when rn
    (if (eq (rn-orderings rn) t)
      (setf (rn-orderings rn) 
            (permutations-of-length 
             (cons (rn-name rn)
                   (mapcar #'rn-name 
                           (rn-subnodes rn)))
             (+ (length (rn-subnodes rn)) 1))))
    (rn-orderings rn)))

(defun force-meets (top left-branch right-branch &optional strict)
  ;;  (format t "~%fm ~A~%   ~A~%   ~A" top left-branch right-branch)
  (when top
    (cond ((and (null left-branch) right-branch) ;; (meets top right-branch)
           ;; (format t "~%~%top=~A,~%  rb=~A" top right-branch)	   
	   (if strict
             (setf (rn-orderings top)
                   (delete-if-not #'(lambda (o) 
                                      (= (position (rn-name (first right-branch)) o)
                                         (+ (position (rn-name top) o) 1)))
                                  ;; all possible orderings in the top
                                  (maybe-init-orderings top)))
             ;; this part is added so the correct ordering is chosen for the precedes constraints
             ;; right now the (maybe-init-orderings top) contain the orderings that satisfy only
             ;; meets constraints. to deal with precedes constraints we need to make sure that the 
             ;; top unit PRECEDES the precedes unit (here: (first right-branch)):
             (loop for order in (maybe-init-orderings top)
                   when 
                   (> (position (rn-name (first right-branch)) order)
                      (position (rn-name top) order))
                   do (setf (rn-orderings top) (list order)))))

	  ((and left-branch (null right-branch)) ;; (meets left-branch top)
           ;;	   (format t "~%~%lb=~A,~%  top=~A" left-branch top)
	   (setf (rn-orderings top)
		 (delete-if-not #'(lambda (o)
				    (= (position (rn-name (first left-branch)) o)
				       (- (position (rn-name top) o) 1)))
				(maybe-init-orderings top)))
	   (when strict 
	     (dolist (n (rest (butlast left-branch)))
	       (setf (rn-orderings n) (delete-if-not #'(lambda (o)
							 (equal (rn-name (first right-branch))
								(first (last o))))
						     (maybe-init-orderings n))))))
	  ((and left-branch right-branch)
           ;;	   (format t "~%------------->~%top=~A~%   lb=~A~%   rb=~A" top left-branch right-branch)
	   (setf (rn-orderings top)
		 (delete-if-not (if strict 
                                  #'(lambda (o) 
                                      (= (position (rn-name (first left-branch)) o)
                                         (- (position (rn-name (first right-branch)) o) 1)))
                                  #'(lambda (o) 
                                      (< (position (rn-name (first left-branch)) o)
                                         (position (rn-name (first right-branch)) o))))
				(maybe-init-orderings top)))
	   (when strict 
	     (dolist (n (rest (butlast right-branch)))
	       (setf (rn-orderings n) (delete-if-not #'(lambda (o)
							 (equal (rn-name (first right-branch))
								(first o)))
						     (maybe-init-orderings n)))))
	   (when strict 
	     (dolist (n (rest (butlast left-branch)))
	       (setf (rn-orderings n) (delete-if-not #'(lambda (o)
							 (equal (rn-name (first right-branch))
								(first (last o))))
						     (maybe-init-orderings n))))))
	  (t  nil))))

(defun render-handle-meets (structure topnode)
  (dolist (p (extract-meets-constraints structure))
    (multiple-value-bind (top left-branch right-branch)
	(render-branches (find-render-node (second p) topnode)
			 (find-render-node (third p) topnode))
      (force-meets top left-branch right-branch t))))

(defun render-handle-precedes (structure topnode)
  (dolist (p (extract-precedes-constraints structure))
    (multiple-value-bind (top left-branch right-branch)
	(render-branches (find-render-node (second p) topnode)
			 (find-render-node (third p) topnode))
      (force-meets top left-branch right-branch nil))))

(defun render-extract-strings (topnode &optional (all nil) &key (shuffle t))
  (cond ((null (rn-subnodes topnode))
	 (when (rn-string topnode)
	   (list (list (rn-string topnode)))))
	(all (loop for o in (maybe-init-orderings topnode) 
                   append
                   (let ((r nil))
                     (dolist (name o)
                       (if (eq name (rn-name topnode))
                         (when (rn-string topnode)
                           (if (null r)
                             (setq r (list (list (rn-string topnode))))
                             (setq r (mapcar #'(lambda (e)
                                                 (append e (list (rn-string topnode))))
                                             r))))
                         (if (null r)
                           (setq r (render-extract-strings 
                                    (find name (rn-subnodes topnode) :key #'rn-name)
                                    t :shuffle shuffle))
                           (setq r (loop for sr in (render-extract-strings 
                                                    (find name (rn-subnodes topnode) :key #'rn-name)
                                                    t :shuffle shuffle)
					 append (mapcar #'(lambda (e)
							    (append e sr))
							r))))))
                     r)))
	(t (list (if (eq t (rn-orderings topnode))
                   (loop with subnodes = (cons topnode (rn-subnodes topnode))
                         for subnode in (if shuffle 
                                          (permutate-list subnodes)
                                          subnodes)
                         append
                         (if (eq subnode topnode)
                           (when (rn-string topnode)
                             (list (rn-string topnode)))
                           (first (render-extract-strings subnode nil 
                                                          :shuffle shuffle))))
                   (loop for name in (random-elt (rn-orderings topnode)) append
                         (if (eq name (rn-name topnode))
                           (when (rn-string topnode)
                             (list (rn-string topnode)))
                           (first (render-extract-strings (find name (rn-subnodes topnode) :key #'rn-name)
                                                          nil :shuffle shuffle)))))))))

(defun new-render (syn &key (shuffle t))
  (setq syn (copy-tree syn))
  (let ((topnode (structure->render-node syn (get-root syn))))
    (render-add-strings syn topnode topnode (get-root syn))
    ;;  (format t "~%tn=~A" topnode)
    (render-handle-meets syn topnode)
    ;;   (format t "~%tn=~A" topnode)
    (render-handle-precedes syn topnode)
    ;;    (format t "~%tn=~A" topnode)
    (mapcar #'third 
            (delete "" (first (render-extract-strings topnode nil :shuffle shuffle)) 
                    :key #'third
                    :test #'string=))))

;;;; RENDER IN ROOT-MODE (EXPERIMENTAL)
;;;; --------------------------------------------------------------------------------------------------
;;;; This render function was written by Remi in order to support the root in production.
;;;; The code is still in experimental phase.
(defun insert-precede (order lst)
  (cond
   ((null lst) order) ;; We reached the end of the list, so just insert the units.
   ((eql (first lst) (first order)) ;; The first unit is already found early in the list.
    (if (member (second order) lst) ;; Do not change anything if the second unit is already there
      lst
      (append order (rest lst)))) ;; If not, add the second unit as well.
   ((eql (first lst) (second order)) ;; If the second unit is found, insert the first unit here.
    (cons (first order) (remove (first order) lst)))
   (t
    (cons (first lst) (insert-precede order (rest lst))))))

;; Overwrite this existing-function (original has a bug):
(defun insert-precedes (order chains)
  "Check whether all the precedes constraints are also satisfied."
  (cond
   ((null chains) (list order))
   ((find order chains :test #'subsetp) chains)
   ((member (first order) (first chains))
    (if (find (second order) (rest chains) :test #'member)
      chains
      (cons (append (first chains) (list (second order))) (rest chains))))
   ((member (second order) (first chains))
    (if (rest chains)
      (let* ((preceding-chain (find (first order) (rest chains) :test #'member))
             (pos-of-prec-chain-in-rest-chains (position preceding-chain (rest chains) :test #'equal))
             (preceding-chains (subseq (rest chains) 0 (+ 1 pos-of-prec-chain-in-rest-chains)))
             (following-chains (subseq (rest chains) (+ 1 pos-of-prec-chain-in-rest-chains))))
        (if preceding-chain
          (append  preceding-chains (first chains)                   
                   (when following-chains
                     (remove preceding-chain following-chains :test #'equal)))
          (cons (list (first order)) chains)))
      (cons (list (first order) (first (car chains))) chains)))
   (t
    (cons (first chains) (insert-precedes order (rest chains))))))

(defun insert-meets (meets-lst lst)
  "Take care of the adjacency constraints."
  (if (null lst) ;; If the list is currently empty...
    (insert-meets meets-lst (flatten meets-lst)) ;; Then order the 
    (let* ((meets (assoc (first lst) meets-lst))
           (next-unit (second meets)))
      (if next-unit
        (cons (first lst)
              (insert-meets (remove meets meets-lst :test #'equal)
                            (cons next-unit (rest lst))))
        (cons (first lst) (insert-meets meets-lst (rest lst)))))))

(defun insert-meets-constraint (meeting-units lst)
  "Insert the meets-constraint in its proper position."
  (cond
   ((null lst) (list meeting-units))
   ((member meeting-units lst :test #'equal) lst)
   ((eql (first meeting-units) (second (first lst)))
    (sort-order-constraints (rest lst) ;; Dangerous recursion...
                            (list (first lst) meeting-units)))
   ((eql (second meeting-units) (first (first lst)))
    (sort-order-constraints lst (list meeting-units)))
   (t
    (cons (first lst) (insert-meets-constraint meeting-units (rest lst))))))

(export '(render-in-root-mode
          sort-order-constraints
          make-chains
          flatten-sequence
          enforce-first-unit))

(defun sort-order-constraints (meets-constraints &optional solution)
  "Make sure that the meets-constraints are in proper order."
  (if (null meets-constraints)
    solution
    (sort-order-constraints (rest meets-constraints)
                            (insert-meets-constraint (first meets-constraints) solution))))

(defun make-chain (lst-1 lst-2)
  "Merge the two lists by appending list-1 to the rest of lst-2."
  (append lst-1 (rest lst-2)))

(defun make-chains (meeting-units)
  "If there are chains of meets-constraints, group them as one list."
  (when meeting-units
    (let* ((meets-1 (first meeting-units))
           (meets-2 (assoc (first (last meets-1)) meeting-units)))
      (if meets-2
        (make-chains (cons (make-chain meets-1 meets-2)
                           (remove meets-2 (rest meeting-units) :test #'equal)))
        (cons (first meeting-units) (make-chains (rest meeting-units)))))))

(defun enforce-first-unit (unit chains)
  "Pick out the chain that starts with the initial unit and put it in front."
  (let ((chain (assoc unit chains)))
    (if chain
      (cons chain (remove chain chains :test #'equal))
      (cons (list unit) chains))))

(defun flatten-sequence (chains)
  "Return a flat list of unique unit-names."
  (let (result)
    (dolist (chain chains)
      (if (symbolp chain)
        (pushnew chain result)
        (dolist (unit-name chain)
          (pushnew unit-name result))))
    (reverse result)))

(defun render-in-root-mode (syn)
  "Default render mode in FCG."  
  (let (sequence strings meets precedes first-unit)
    
    (dolist (unit syn)
      (let ((form (unit-feature-value unit 'form)))
        (dolist (value form)
          (let ((f (first value)))
            (cond
             ((eql f 'string) (push (rest value) strings))
             ((eql f 'meets) (push (rest value) meets))
             ((eql f 'precedes) (push (rest value) precedes))
             ((eql f 'sequence) (setf first-unit (second value)))
             (t
              nil))))))
    
    (setf sequence (sort-order-constraints meets))
    (setf sequence (make-chains sequence))
    (when first-unit
      (setf sequence (enforce-first-unit first-unit sequence)))
    (dolist (order (sort-order-constraints precedes))
      (setf sequence (insert-precedes order sequence)))
    (when first-unit
      (setf sequence (cons (list first-unit)
                           (loop for element in sequence
                                 collect (if (eql first-unit (first element))
                                           (rest element)
                                           element)))))
    (let* ((flat-sequence (flatten-sequence sequence))
           (ordered-forms (loop for unit in flat-sequence
                                for f = (assoc unit strings)
                                when f
                                collect (second f)))
           (other-strings (loop for unit in strings
                                unless (member (first unit) flat-sequence)
                                collect (second unit))))
      (append ordered-forms other-strings))))

(defmethod render ((cfs coupled-feature-structure) (mode (eql :render-in-root-mode)) &key &allow-other-keys)
  (render-in-root-mode (right-pole-structure cfs)))

(defmethod render ((struct list)  (mode (eql :render-in-root-mode)) &key &allow-other-keys)
  (render-in-root-mode struct))

(defmethod render ((cfs coupled-feature-structure) (mode (eql :render-in-one-pole-mode)) &key &allow-other-keys)
  (warn "deprecated method, use :render-string-meets-precedes or :render-string-meets now.")
  (render-in-root-mode (left-pole-structure cfs)))

(defmethod render ((cfs coupled-feature-structure) (mode (eql :render-string-meets-precedes)) &key &allow-other-keys)
  "Renders a coupled-feature-structure using string; meets and precedes constraints."
  (render-in-root-mode (left-pole-structure cfs)))

(defmethod render ((cfs coupled-feature-structure) (mode (eql :render-string-meets)) &key &allow-other-keys)
  "Renders a coupled-feature-structure using string and meets constraints."
  (render-in-root-mode (left-pole-structure cfs)))

;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;                                    :render-with-scope
;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(export '(order-constraints-by-predicate order-units-locally reduce-scoped-constraints
                                         expand-non-terminal retain-only-terminals
                                         find-scope get-scope-from-form-constraint fields adjacent))

(defun order-constraints-by-predicate (constraints predicates &optional (result))
  "Groups the constraints in the same order as they appear in the predicates argument."
  (dolist (constraint constraints)
    (let* ((key (position (first constraint) predicates :test #'string=))
           (entry (when key (assoc key result)))
           (new-entry `(,key ,@(cons constraint (rest entry)))))
      (setf result
            (substitute-or-cons new-entry result))))
  (mappend #'rest (sort result #'< :key #'first)))

;;; (order-constraints-by-type (shuffle '((precedes a b)
;;;                                       (meets a b)
;;;                                       (precedes a c)
;;;                                       (precedes b c)
;;;                                       (meets b c)))
;;;                                     '(meets precedes)))
;;; >>> ((MEETS A B) (MEETS B C) (PRECEDES B C) (PRECEDES A C) (PRECEDES A B))

;;; Methods for locally handling an ordering constraint.
;;; --------------------------------------------------------------------
(defgeneric order-units-locally (constraint scoped-constraints predicate))

(defmethod order-units-locally (constraint scoped-constraints (predicate (eql 'last)))
  (declare  (ignore constraint predicate))
  scoped-constraints)

(defmethod order-units-locally (constraint scoped-constraints (predicate (eql 'first)))
  "This function only looks one level deep at the moment."
  (let* ((parent (third constraint))
         (first-unit (second constraint))
         (entry (assoc parent scoped-constraints)))
    (if entry
      (let ((old (find first-unit (rest entry) :test #'deep-member)))
        (if old
          (let ((new-entry (remove old (rest entry) :test #'equal))
                (new-old (cons first-unit (remove first-unit old))))
            (substitute (cons parent (cons new-old new-entry)) entry scoped-constraints :test #'equal))
          (substitute-or-cons (list parent (cons (list first-unit) (rest entry)))
                              scoped-constraints)))
      (cons (list parent (list first-unit)) scoped-constraints))))

(defmethod order-units-locally (constraint scoped-constraints (predicate (eql 'fields)))
  "Handle a fields constraint."
  (let* ((scope (get-scope-from-form-constraint constraint))
         (fields-as-lists (loop for field in (rest (butlast constraint))
                                unless (variable-p field) ;; Ignore fields that are not filled.
                                collect (if (listp field)
                                          field
                                          (list field))))
         (n 1))
    (dolist (field fields-as-lists) ;; e.g. ((det)(adj-1 adj-2)(n) (post-mod))
      (let ((remainder (flatten (subseq fields-as-lists n)))) ;; e.g. (adj-1 adj-2 n post-mod)
        (when remainder
          (dolist (unit1 field) ;; e.g. (adj-1 adj-2)
            (dolist (unit2 remainder)
              (setf scoped-constraints
                    (order-units-locally `(precedes ,unit1 ,unit2 ,scope) scoped-constraints 'precedes))))))
      (incf n))
    scoped-constraints))                    

(defmethod order-units-locally (constraint scoped-constraints (predicate (eql 'precedes)))
  "Handle the precedes constraints."
  (let* ((parent (fourth constraint))
         (lst (rest (assoc parent scoped-constraints)))
         (children (subseq constraint 1 3))
         (first-pos (position (first children) lst :test #'deep-member))
         (second-pos (position (second children) lst :test #'deep-member))
         (new-entry (cons parent (cond
                                  ;; Both units are new, so we add them to the list.
                                  ((none first-pos second-pos) (cons (list (first children))
                                                                     (cons (rest children) lst)))
                                  ;; Only the first unit is new, so we add it to the beginning of the list.
                                  ((null first-pos) (cons (list (first children)) lst))
                                  ;; Only the second unit is new, so we add it at the end of the list.
                                  ((null second-pos) (append lst (list (rest children))))
                                  ;; The precedes constraint is violated, we solve it by moving the first one up the list.
                                  ((> first-pos second-pos)
                                   (let ((second-entry (nth first-pos lst)))
                                     (append (subseq lst 0 second-pos)
                                             (list second-entry)
                                             (remove second-entry (subseq lst second-pos) :test #'equal))))
                                  (t ;; The constraint is already ok, so return the list.
                                     lst)))))
    (substitute-or-cons new-entry scoped-constraints)))

(defmethod order-units-locally (constraint scoped-constraints (predicate (eql 'meets)))
  "Meets constraints make a local chain."
  (let* ((parent (fourth constraint))
         (lst (rest (assoc parent scoped-constraints)))
         (child1 (second constraint))
         (child2 (third constraint))
         (first-entry (find child1 lst :test #'deep-member))
         (second-entry (find child2 lst :test #'deep-member)))
    (if (none first-entry second-entry)
      (push `((,child1) (,child2)) lst)
      (labels ((enforce-constraint (list result)
                 (cond ((equal first-entry (first list))
                        (append (reverse result)
                                (list (if second-entry (append first-entry second-entry)
                                        (append first-entry (list (list child2)))))
                                (remove second-entry (rest list) :test #'equal)))
                       ((equal second-entry (first list))
                        (append (reverse result)
                                (list (if first-entry (append first-entry (list (list second-entry)))
                                        (cons (list child1) second-entry)))
                                (remove first-entry (rest list) :test #'equal)))
                       (t
                        (enforce-constraint (rest list) (cons (list (first list)) result))))))
        (setf lst (enforce-constraint lst nil))))
    (substitute-or-cons (cons parent lst) scoped-constraints)))

(defun find-child-sublist (elt lst &key (test #'eql))
  "Recursively go through nested lists to find an element, and the return the nested list where it can be found."
  (cond ((atom lst) nil)
        ((member elt lst :test test) lst)
        (t
         (or (find-child-sublist elt (first lst) :test test)
             (find-child-sublist elt (rest lst) :test test)))))
;; (find-child-sublist 'a '((((((((a)))))))))
;; (find-child-sublist 'a '((((((((b a)))))))))

(defun singleton-p (lst)
  (and lst (null (rest lst))))

(defun enforce-adjacency-help (first-entry second-entry child1 child2)
  "Checks which adjacency is allowed and chooses that one."
  (let ((flat-first-entry (flatten first-entry))
        (flat-second-entry (flatten second-entry)))
    (cond
     ;; First handle the cases where one of the two is a singleton.
     ((and (singleton-p first-entry) (equal child2 (first flat-second-entry)))
      (list (cons first-entry second-entry)))
     ((and (singleton-p first-entry) (equal child2 (last-elt flat-second-entry)))
      (list (append second-entry (list first-entry))))
     ((and (singleton-p second-entry) (equal child1 (first flat-first-entry)))
      (list (cons second-entry first-entry)))
     ((and (singleton-p second-entry) (equal child1 (last-elt flat-first-entry)))
      (list (append first-entry (list second-entry))))
     ;; This case should not happen, so we'll give a warning and a bad result.
     ((or (singleton-p first-entry) (singleton-p second-entry))
      (format t "Warning: you have a conflicting adjacency constraint while rendering.")
      `((,@(if (singleton-p first-entry) (list first-entry) first-entry)
         ,@(if (singleton-p second-entry) (list second-entry) second-entry))))
     ;; Now we check the possibilities if both are nested.
     ((or (equal child1 (first flat-first-entry))
          (equal child2 (last flat-second-entry)))
      `((,@second-entry ,@first-entry)))
     ((or (equal child1 (last-elt flat-first-entry))
          (equal child2 (first flat-second-entry)))
      `((,@first-entry ,@second-entry)))
     (t ;; This case should never happen so we'll print a warning.
        (format t "Warning: you have a conflicting adjacency constraint while rendering.")
        `((,@first-entry ,@second-entry))))))

(defmethod order-units-locally (constraint scoped-constraints (predicate (eql 'adjacent)))
  ;; The adjacent constraint should be applied AFTER the more specific ordering constraints
  ;; meets, precedes, fields, first and last.
  (setf *scoped-constraints* scoped-constraints)
  (let* ((parent (find-scope constraint predicate))
         (lst (rest (assoc parent scoped-constraints)))
         (child1 (list (second constraint)))
         (child2 (list (third constraint)))
         (first-entry (or (find child1 lst :test #'equal) ;; Simple case...
                          (find-child-sublist child1 lst :test #'equal) ;; Meets-constraints involved...
                          child1)) ;; Maybe no constraints exist yet for this unit.
         (second-entry (or (find child2 lst :test #'equal) ;; Simple case...
                           (find-child-sublist child2 lst :test #'equal) ;; Meets-constraints involved.
                           child2))) ;; Maybe no constraints exist yet for this unit.
    (if (none first-entry second-entry) ;; We have a completely new constraint so we can skip the whole search.
      (push `((,(first random-order))(,(second random-order))) lst)
      ;; But probably we already have previous information.
      ;; At this point, the scoped constraints are already ordered by meets, precedes,
      ;; and other constraints. It is therefore important to respect that order.
      ;;      Precedes constraints are currently handled at a one-level depth. That is,
      ;; the constraint (precedes a b scope) would by now be translated into the following
      ;; structure: ((scope (a) (b))). Meets constraints, on the other hand, lead to nested
      ;; lists. So if there is also a (meets b c scope), then we would have the following:
      ;; ((scope (a) ((b) (c)))). The order in a nested list should never change, whereas we
      ;; assume here that the order on the first level (the precedes-level) can still be changed.
      ;; It is therefore up to the grammar engineer to worry about conflicts.
      ;;     For example, the constraint (adjacent a b scope) is acceptable, because that should lead to:
      ;; ((scope ((a) (b) (c)))). The constraint (adjacent a c scope), however, would be unacceptable
      ;; because it would lead to ((scope ((b) (c) (a)))), which violates the precedes constraint.
      ;; ------------------------------------------------------------------------------------------------
      (labels ((enforce-adjacency (list result)
                 (cond ((null list) ;; This case should never happen, but we'll include it for safety.
                        (reverse result))
                       ((equal first-entry (first list)) ;; We come across the first-entry first.
                        (append (reverse result)
                                (if (and (singleton-p first-entry) (singleton-p second-entry)) ;; Simple case.
                                  (list (list first-entry second-entry))
                                  ;; If either of the entries is nested, we call a helper function.
                                  (enforce-adjacency-help first-entry second-entry (first child1) (first child2)))
                                (remove second-entry (rest list) :test #'equal)))
                       ((equal second-entry (first list)) ;; We come across the second-entry first.
                        (append (reverse result)
                                (if (and (singleton-p first-entry) (singleton-p second-entry))
                                  (list (list second-entry first-entry))
                                  (enforce-adjacency-help second-entry first-entry (first child2) (first child1)))
                                (remove first-entry (rest list) :test #'equal)))
                       (t
                        (enforce-adjacency (rest list) (cons (list (first list)) result))))))
        (setf lst (enforce-adjacency lst nil))))
    (substitute-or-cons (cons parent lst) scoped-constraints)))

(defun deep-find-atom (atom lst &key (test #'eql))
  "Recursively looks for an atom in a list, and returns it if found."
  (cond
   ((null lst) nil)
   ((atom lst) (if (funcall test atom lst)
                 lst nil))
   (t
    (or (deep-find-atom atom (first lst) :test test)
        (deep-find-atom atom (rest lst) :test test)))))

(defun reduce-scoped-constraints (lst)
  "Try to fit in the constraints with each other."
  (let ((result (copy-list lst))
        (scope-units (mapcar #'first lst)))
    (dolist (scope-unit scope-units)
      (let ((subst? nil)
            (new-result nil)
            (subpart (assoc scope-unit result)))
        (dolist (entry result)
          (if (eql scope-unit (first entry))
            (push entry new-result)
            ;; Strange: eql does not always work for the unit-names...
            (let ((old (deep-find-atom scope-unit (rest entry) :test #'string=)))
              (if old
                (progn (setf subst? t)
                  (push (subst subpart (list old) entry :test #'equal) new-result))
                (push entry new-result)))))
        (if subst? (setf result (remove-if #'(lambda(x)
                                               (eql (first x) scope-unit))
                                           new-result))
          (setf result new-result))))
    result))

(defun expand-non-terminal (x struct hierarchy-features)
  "Expand a non-terminal to its lowest subunits."
  (cond ((null x) nil)
        ((symbolp x)
         (let ((subunits (unit-feature-value (assoc x struct) hierarchy-features)))
           (if subunits
             (expand-non-terminal subunits struct hierarchy-features)
             (list x))))
        (t
         (append (expand-non-terminal (first x) struct hierarchy-features)
                 (expand-non-terminal (rest x) struct hierarchy-features)))))

(defun get-leaves (tree &optional result)
  "Return all the leaves of a tree."
  (cond ((symbolp tree) (cons-if tree (flatten (reverse result))))
        ((symbolp (first tree))
         (get-leaves (rest tree) (if (rest tree)
                                   result
                                   (cons (first tree) result))))
        (t
         (append (get-leaves (first tree) result)
                 (get-leaves (rest tree))))))

(defun retain-only-terminals (tree non-terminals terminals struct hierarchy-features)
  "Given a tree, only keep the terminal leaves."
  (let ((result nil))
    (dolist (symbol (get-leaves tree)) ;; Throw away the parent-node symbol.
      (cond ((member symbol terminals) (push symbol result))
            ((member symbol non-terminals) nil)
            (t
             ;; There is a non-terminal that has not been expanded yet.
             (setf result (append (expand-non-terminal symbol struct hierarchy-features) result)))))
    (reverse result)))

;;; Note by Remi: If the last element is the convention, we can make this a normal function again.
(defgeneric find-scope (constraint constraint-name))

(defmethod find-scope (constraint (constraint-name (eql 'meets)))
  ;; (meets det adj NP)
  (last-elt constraint))

(defmethod find-scope (constraint (constraint-name (eql 'precedes)))
  ;; (precedes det n NP)
  (last-elt constraint))

(defmethod find-scope (constraint (constraint-name (eql 'first)))
  ;; (first NP article)
  (last-elt constraint))

(defmethod find-scope (constraint (constraint-name (eql 'fields)))
  ;; (fields field-1 ... field-n scope)
  (last-elt constraint))

(defmethod find-scope (constraint (constraint-name (eql 'last)))
  ;; (fields field-1 ... field-n scope)
  (last-elt constraint))

(defmethod find-scope (constraint (constraint-name (eql 'adjacent)))
  ;; (adjacent unit-1 unit-2 scope)
  (last-elt constraint))

(defmethod find-scope (constraint (constraint-name t))
  ;; By default, take the last element.
  (last-elt constraint))

(defun get-scope-from-form-constraint (constraint)
  "Calls the generic function find-scope."
  (let ((constraint-name (first constraint)))
    (find-scope constraint constraint-name)))

(defun remove-duplicate-units (list-of-unit-names &optional result)
  "Given a list of unit names, keep only the first mention of the unit respecting the order of the list."
  (dolist (unit list-of-unit-names)
    (if (member unit result)
      nil
      (push unit result)))
  (reverse result))

(defun test-form-constraints (form-constraints)
  (let ((scoped-constraints nil)
        (unscoped-constraints nil))
  (dolist (constraint form-constraints scoped-constraints)
    (let ((scope (get-scope-from-form-constraint constraint))
          (form-predicate (first constraint)))
      (if (or (null scope) (variable-p scope))
        (push constraint unscoped-constraints)
        (setf scoped-constraints
              (order-units-locally constraint scoped-constraints form-predicate)))))))

(defgeneric filter-form-constraint (form-constraints predicate))

(defmethod filter-form-constraint ((form-constraints list) (predicate (eql 'adjacent)))
  (let (result)
    (dolist (constraint form-constraints)
      (unless (and (eql predicate (first constraint))
                   (loop for x in result
                         when (and (eql (first x) predicate)
                                                          (member (second constraint) x)
                                                          (member (third constraint) x)
                                                          (eql (last-elt x) (last-elt constraint)))
                         return t))
        (push constraint result)))
    (reverse result)))
;;(render english-grammar::*test-units* :render-with-scope :node english-grammar::*testnode*)

(defmethod filter-form-constraint (form-constraints (predicate t))
  ;; By default we do not need to filter.
  form-constraints)

(defun filter-form-constraints (form-constraints form-predicates)
  "Some form constraints need to be filtered out."
  (loop for form-predicate in form-predicates
        do (setf form-constraints (filter-form-constraint form-constraints form-predicate))
        finally (return form-constraints)))

;;; The actual render methods.
;;; --------------------------------------------------------------------
(defmethod render ((struct list)  (mode (eql :render-with-scope)) &key node &allow-other-keys)
  ;; Preamble: gather some data from the units.
  (let* ((form-predicates (get-updating-references node))
         (form-constraints
          ;; Filter the constraints if necessary
          (filter-form-constraints
           ;; Order the constraints by predicate.
           (order-constraints-by-predicate 
            ;; To ensure random results in case of underspecification.
            (shuffle (fcg-extract-selected-form-constraints struct form-predicates))
            form-predicates)
           form-predicates))
         (terminals-and-strings (mapcar #'rest (fcg-extract-selected-form-constraints struct '(string))))
         (terminals (mapcar #'first terminals-and-strings))
         (non-terminals nil)
         (scoped-constraints nil) (unscoped-constraints nil))
    ;; First we collect all scoped constraints in an alist.
    (dolist (constraint form-constraints)
      (let ((scope (get-scope-from-form-constraint constraint))
            (form-predicate (first constraint)))
        (if (or (null scope) (variable-p scope))
          (push constraint unscoped-constraints)
          (setf scoped-constraints
                (order-units-locally constraint scoped-constraints form-predicate)))))

    ;; All first symbols in the scoped constraints are non-terminals, unless they are associated with a string.
    (setf non-terminals (loop for elt in scoped-constraints
                              unless (find (first elt) terminals :test #'string=)
                              collect (first elt)))

    ;; To do: unscoped constraints. These are currently ignored.
    (when unscoped-constraints (format t "WARNING: Unscoped constraints!! ~a" unscoped-constraints))
    
    ;; Try to build (partial) trees and flatten these to their terminal symbols (i.e. unit -> "string").
    ;; I am not very comfortable with the remove-duplicate-units yet, but problems with it will pop up in testing.
    (setf scoped-constraints
          (remove-duplicate-units (loop for tree in (reduce-scoped-constraints scoped-constraints)
                                        append (retain-only-terminals tree non-terminals terminals struct
                                                                      (get-configuration node :hierarchy-features)))))
    (let ((utterance (shuffle (append (set-difference terminals scoped-constraints) (list scoped-constraints)))))
      (remove nil (loop for unit in (flatten utterance)
            collect (second (assoc unit terminals-and-strings)))))))

(defmethod render ((cfs coupled-feature-structure) (mode (eql :render-with-scope)) &key node &allow-other-keys)
  (render (left-pole-structure cfs) mode :node node))

