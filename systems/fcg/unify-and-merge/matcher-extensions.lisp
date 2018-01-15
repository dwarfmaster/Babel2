(in-package :fcg)

(export '(special-operator representation clean-fn unify-fn merge-fn operator-p))

(defparameter *special-operators* (make-hash-table))

(defclass special-operator ()
  ((representation :type symbol :initarg :representation
		   :initform nil :accessor representation)
   (clean-fn :type (or null function) :initarg :clean-fn
	     :initform nil :accessor clean-fn)
   (unify-fn :type (or null function) :initarg :unify-fn
	     :initform nil :accessor unify-fn)
   (merge-fn :type (or null function) :initarg :merge-fn
	     :initform nil :accessor merge-fn)))

(defgeneric add-special-operator (special-operator)
  (:documentation "Adds a special operator to the
  *special-operators*."))

(defmethod add-special-operator ((operator special-operator))
  (setf (gethash (representation operator) *special-operators*)
	operator))

(defun get-so (operator-symbol)
  "Returns the special-operator object given its representation symbol"
  (gethash operator-symbol *special-operators*))

(defmethod unify-fn ((operator null))
  "The unify-fn of null is nil."
  nil)

(defmethod unify-fn ((operator t))
  "The unify-fn of t is nil."
  nil)

(defmethod unify-fn ((operator symbol))
  "Returns the unify-fn for a special-operator representation symbol."
  (unify-fn (get-so operator)))

(defmethod merge-fn ((operator null))
  "The merge-fn of null is nil."
  nil)

(defmethod merge-fn ((operator t))
  "The merge-fn of t is nil."
  nil)

(defmethod merge-fn ((operator symbol))
  "Returns the merge-fn for a special-operator representation symbol."
  (merge-fn (get-so operator)))

(defmethod clean-fn ((operator null))
  "The clean-fn of null is nil."
  (clean-fn (get-so operator)))

(defmethod clean-fn ((operator t))
  "The clean-fn of null is nil."
  (clean-fn (get-so operator)))

(defmethod clean-fn ((operator symbol))
  "Returns the clean-fn for a special-operator representation symbol.
   The clean-fn is used by remove-special-operators."
  (clean-fn (get-so operator)))

(defun operator-p (operator-symbol)
  "Retruns true if the operator-symbol is known as a special operator."
  (and (symbolp operator-symbol)
       (get-so operator-symbol)))
    
;; Code for special operators for matcher and merger.

;; ############################################################################
;; 'OR' special operator

;; This special operator is not in the 2015 FCG notation anymore, but is still in this
;; file for the case we would want it back. Rumour has it that it is not straightforward in
;; merging. If none of the 'or' options were there, which one should be added?

#|
(export '(OR))

(defun unify-or (or y bindings-list &key cxn-inventory)
  (delete-duplicates (loop for c in (rest or) append
                           (unify c y bindings-list :cxn-inventory cxn-inventory))
		     :test #'equal))

(defun merge-or (or y bindings &key cutoff (merge-fn #'fcg-merge) (remove-special-operators t) cxn-inventory)
  (declare (ignore merge-fn remove-special-operators))
  (cond ((and (consp y) (eq 'or (first y)))
	 (let ((r (intersection (rest or)
				(rest y)
				:test #'(lambda (x y)
					  (unify-simple x y bindings :cxn-inventory cxn-inventory)))))
	   (cond ((= 1 (length r))
		  (make-merge-result r (list bindings)))
		 ((null r) +fail+)
		 (t (make-merge-result (cons 'OR r) (list bindings))))))
	((null y) 
	 (break "Merge-or, strange case...")
	 (make-merge-result (second or) (list bindings) (second or)))
	(t (let ((res nil))
	     (dolist (c (rest or))
	       (dolist (m (fcg-merge c y bindings :cutoff cutoff :cxn-inventory cxn-inventory))
		 (let ((prev (find (mr-expr m) res :key #'first :test #'equal)))
		   (if prev
		       (setf (mr-bsl prev) (append (mr-bsl m) (mr-bsl prev)))
		       (push m res)))))
	     res))))

(defun clean-or (or bindings)
  (values (cons 'or 
		(loop for e in (rest or) collect
                      (multiple-value-bind (ne nbs)
			 (remove-special-operators e bindings)
		       (setq bindings nbs)
		       ne)))
	  bindings))

(add-special-operator (make-instance 'special-operator
				     :representation 'OR
				     :clean-fn #'clean-or
				     :unify-fn #'unify-or
				     :merge-fn #'merge-or))
|#

;; ############################################################################
;; I. The includes or '== extension: (== x_1 .. x_n) should unify with
;; every list that contains at least the elements x_1 to x_n

(defun subset-p (subset superset bindings-list &key (unify-fn #'unify) cxn-inventory)
  "   Determines whether SUBSET (a list) is a subset of SUPERSET (also a list)
in the form of a BINDINGS-LIST. Every BINDINGS returned assures that 

   (subsetp (substitute-bindings BINDINGS SUBSET) 
   	    (substitute-bindings BINDINGS SUPSERSET)
            :test UNIFY-FN) 
is true.

Example:

   (subset-p '((x ?a) u (v ?b ?c)) '((x a) u ?u (v b ?r)) (list +no-bindings+) :unify-fn #'unify)

returns the following BINDINGS:

 (1) ((?C . ?R) (?B . B) (?U . U) (?A . A))
 (2) ((?C . ?R) (?B . B) (?U X ?A)) 
 (3) ((?C . ?R) (?B . B) (?A . A))) 
 (4) ((?U V ?B ?C) (?A . A))

resulting in the following table after substitution:
   BINDINGS | subset                |  extra in superset
   -----------------------------------------------------
      (1)   | ((x a) u (v b ?r))    |  u
      (2)   | ((x ?a) u (v b ?r))   |  (x a)
      (3)   | ((x a) u (v b ?r))    |  ?u
      (4)   | ((x a) u (v ?b ?c))   |  (v b ?r) 
   ------------------------------------------
"
  (declare (type list subset)
	   (optimize (speed 3) (safety 0)))
  (cond ((null subset) bindings-list)
	((<= (length subset) (length superset))
	 ;; first see which superset elements unify with the first subset element:
	 (let (supersets-first nof-first)
	   (dolist (superset-elt superset)
	     (let ((bsl (funcall unify-fn (first subset) superset-elt bindings-list :cxn-inventory cxn-inventory)))
	       (when bsl (push (cons superset-elt bsl) supersets-first))))
	   ;; now check how many times the first element occurs
	   (setq nof-first (count (first subset) subset :test #'equalp))
	   ;; we can only succeed if there are enough candidates:
;; 	           (format t "~%first=~A~%supersets-first=~A" 
;; 	            	       (second subset)
;; 	            	       supersets-first)
	   (when (<= nof-first (length supersets-first))
	     ;; before proceeding, first clean up the first:
	     (setq subset (remove (first subset) subset :test #'equalp))
;;	     	 (format t "~%subset now: ~A" subset)
	     ;; now for every combination of nof-first elements removed
	     ;; from the superset we have a possible solution:
	     (loop for c in (delete-duplicates
			     (combinations-of-length supersets-first nof-first)
			     :test #'equal)
		;; 	    do (format t "~%c=~A~%=>~A" c (bucket-difference 
		;; 					   superset c
		;; 					   :test #'(lambda (se sfe) 
		;; 						     (format t "~%testing ~A AND ~A, => ~A" 
		;; 							     se (car sfe) (eq se (car sfe)))
		;; 						     (eq se (car sfe)))))
		append
		(subset-p subset
			  (bucket-difference superset c
					     :test #'(lambda (se sfe) (eq se (car sfe))))
			  (reduce #'merge-bindings-lists
				  (rest c)
				  :key #'cdr
				  :initial-value (cdr (first c)))
			  :unify-fn unify-fn
                          :cxn-inventory cxn-inventory)))))
	(t +fail+)))

;; ############################################################################
;; '== special operator (includes)

(export '==)

(defun unify-includes (pattern source bindings-list &key cxn-inventory)
  ;; pattern is of the form (== ...) => like subset on (rest pattern) if source is a list.
  (declare (type list pattern)
	   (optimize (speed 3) (safety 0)))
  (cond ((listp source)
	 (subset-p (rest pattern) source bindings-list :cxn-inventory cxn-inventory))
	((variable-p source) 
	 (let ((res nil))
	   (dolist (bindings bindings-list)
	     (let ((subres (unify-variable source pattern bindings :cxn-inventory cxn-inventory)))
	       (unless (fail? subres)
		 (push subres res))))
	   res))
	(t +fail+)))

(defun merge-includes (pattern source bindings &key cutoff (merge-fn #'fcg-merge)
                               (remove-special-operators t) cxn-inventory)
  (declare (list pattern))
  (when (listp source)
    (make-subset (rest pattern) source bindings :cutoff cutoff :merge-fn merge-fn
		 :remove-special-operators remove-special-operators :cxn-inventory cxn-inventory)))
  
(add-special-operator (make-instance 'special-operator
				     :representation '==
				     :unify-fn #'unify-includes
				     :merge-fn #'merge-includes))

;; ############################################################################
;; '==p special operator (permutation)

(export '==p)

(defun unify-permutation (p y bsl &key cxn-inventory)
  (if (and (listp y) (= (- (length p) 1) (length y)))
    (unify-includes p y bsl :cxn-inventory cxn-inventory)
    +fail+))

(defun merge-permutation (pattern source bindings &key cutoff (merge-fn #'fcg-merge)
                                  (remove-special-operators t) cxn-inventory)
  (when (listp source)
    (delete-if-not #'(lambda (mr) (= (length (the list (mr-expr mr))) (- (length pattern) 1)))
		   (make-subset (rest pattern) source bindings :cutoff cutoff :merge-fn merge-fn
				:remove-special-operators remove-special-operators :cxn-inventory cxn-inventory))))

(add-special-operator (make-instance 'special-operator
				     :representation '==p
				     :unify-fn #'unify-permutation
				     :merge-fn #'merge-permutation))

;; ############################################################################
;; '==1 special operator (includes-uniquely)

(export '==1)

(defun unify-includes-uniquely (includes-list y bsl &key cxn-inventory)
  (unless (valid-includes-uniquely-list? y +no-bindings+ :cxn-inventory cxn-inventory)
    (setq bsl (remove-if-not #'(lambda (bs) (valid-includes-uniquely-list? y bs :cxn-inventory cxn-inventory))
 			     bsl)))
  (when bsl
    (subset-p (rest includes-list) y bsl :unify-fn #'unify-unique-elements :cxn-inventory cxn-inventory)))


(defun valid-includes-uniquely-list? (list bs &key cxn-inventory)
  (setq list (substitute-bindings bs list))
  (and (listp list)
       (let ((success t))
	 (loop 
          for p1 from 0 below (- (length list) 1)
          while success
          do (loop for p2 from (+ p1 1) below (length list)
                   while success
                   do (setq success 
                            (not (compatible-unique-elements?
                                  (elt list p1)
                                  (elt list p2)
                                  bs 
                                  :cxn-inventory cxn-inventory)))))
	 success)))

(defun compatible-unique-elements? (pe se &optional (bs +no-bindings+) &key cxn-inventory)
  ;;  (format t "~%cue ~A ~A" se pe)
  (cond ((atom pe) (unify-simple pe se bs :cxn-inventory cxn-inventory))
	((and (consp se) (consp pe) (eq 'or (first se)) (eq 'or (first pe)))
	 (some #'(lambda (pc)
		   (some #'(lambda (sc)
			     (compatible-unique-elements? pc sc bs :cxn-inventory cxn-inventory))
			 (rest pe)))
	       (rest se)))
	((and (consp se) (eq 'or (first se)))
	 (some #'(lambda (c) 
		   (compatible-unique-elements? c pe bs :cxn-inventory cxn-inventory))
	       (rest se)))
	((and (consp pe) (eq 'or (first pe)))
	 (some #'(lambda (c) 
		   (compatible-unique-elements? se c bs :cxn-inventory cxn-inventory))
	       (rest pe)))
	((consp se) (compatible-unique-elements? (first se) (first pe) bs :cxn-inventory cxn-inventory))))

(defun unify-unique-elements (pe se bsl &key cxn-inventory)
  (when (compatible-unique-elements? pe se +no-bindings+ :cxn-inventory cxn-inventory)
    (unify pe se bsl :cxn-inventory cxn-inventory)))

(defun merge-includes-uniquely (includes-list y bindings &key cutoff (merge-fn #'fcg-merge)
                                              (remove-special-operators t) cxn-inventory)
  (make-subset (rest includes-list) y bindings 
	       :test-fn #'compatible-unique-elements? :cutoff cutoff :merge-fn merge-fn
	       :remove-special-operators remove-special-operators
               :cxn-inventory cxn-inventory))

(add-special-operator (make-instance 'special-operator
				     :representation '==1
				     :unify-fn #'unify-includes-uniquely
				     :merge-fn #'merge-includes-uniquely))

;; ############################################################################
;; The excludes or '==0 extension: (==0 x_1 .. x_n): unifies with
;; every list _not_ containing any of the elements x_1 to x_n.

(export '==0)

(defun unify-excludes (pattern source bindings-list &key cxn-inventory)
  (declare (type list pattern)
	   (optimize (speed 3) (safety 0)))
  (if (some #'(lambda (e)
		(unify (list '== e) source bindings-list :cxn-inventory cxn-inventory))
	    (rest pattern))
    +fail+
    bindings-list))

;;; merging simply returns the source:

(defun merge-excludes (pattern source bindings &key cutoff (merge-fn #'fcg-merge)
		       (remove-special-operators t) cxn-inventory)
  (declare (ignore cutoff) (ignore merge-fn remove-special-operators))
  (setq bindings (unify-simple pattern source bindings :cxn-inventory cxn-inventory))
  (when bindings
    (list (make-merge-result source (list bindings)))))

(defun clean-excludes (excludes bindings)
  (declare (ignore excludes))
  (values nil bindings))

(add-special-operator (make-instance 'special-operator
				     :representation '==0
				     :unify-fn #'unify-excludes
				     :merge-fn #'merge-excludes
				     :clean-fn #'clean-excludes))
(defstruct mss-state
  mr-so-far ;; a merge-result
  remaining ;; lists of ((positions mrs weight) ...) lists
  g-cost
  h-cost
  taken)

(defun bag-to-position-list (bag)
  "Given a bag (a set that might have duplicates) it returns a list
containing each element and its position(s). The duplicates are thus
explicitly represented because they'll have more positions."
  (loop 
   with superset-elts+positions = nil
   for superset-elt in bag 
   for pos from 0 below (length bag) 
   for prev = (find superset-elt superset-elts+positions :key #'car :test #'equal)
   if prev do (push pos (cdr prev)) 
   else do (push (list superset-elt pos) superset-elts+positions)
   finally (return superset-elts+positions)))


(defun initial-mss-state (subset superset bindings test-fn merge-fn cutoff destructive
                                 &key (remove-special-operators t) cxn-inventory)
  (let ((superset-elts+positions (bag-to-position-list superset))
	(remaining nil)
	(continue t))
    (setq remaining
	  (loop 
           for subset-elt in subset 
           while continue 
           collect
           (let ((result-for-subset-elt nil)
                 (candidate-elts+positions (if test-fn
                                             (find-all subset-elt superset-elts+positions 
                                                       :test (make-compare-no-tags-fn test-fn) :key #'car)
                                             superset-elts+positions)))
             (dolist (candidate-elt+positions candidate-elts+positions)
               (let ((mrs (funcall merge-fn subset-elt (car candidate-elt+positions) bindings
                                   :cutoff cutoff
                                   :cxn-inventory cxn-inventory)))
                 (when mrs
                   ;;		       (format t "~%  subset-elt=~A~%  candidate=~A~%  => mrs=~A" subset-elt (car candidate-elt+positions) mrs)
                   (push (list (cdr candidate-elt+positions) mrs (length (mr-added (first mrs))))
                         result-for-subset-elt))))
             (when (and destructive
                        (or (not test-fn) (null candidate-elts+positions))
                        (or (null cutoff) (> cutoff 1)))
               (multiple-value-bind (to-add new-bs)
                   (if remove-special-operators
                     (remove-special-operators subset-elt bindings)
                     (values subset-elt bindings))
                 (when new-bs
                   (push (list '(new) (list (make-merge-result to-add (list new-bs) (list to-add))) 1)
                         result-for-subset-elt))))
             (setq continue result-for-subset-elt)
             (cons subset-elt (sort result-for-subset-elt #'< :key #'third)))))
    ;; so for each subset element, remaining has an entry holding a
    ;; sorted list of (positions mrs weight) lists.
    (when continue
      (make-mss-state 
       :mr-so-far (make-merge-result nil (list bindings) nil)
       :remaining remaining
       :g-cost 0 
       :h-cost (reduce #'+ remaining :key #'(lambda (subset-elt-entries) (third (first (cdr subset-elt-entries)))))
       :taken nil))))

(defun successor-states (mss-state merge-fn cutoff superset &key cxn-inventory)
  (declare (ignore superset cutoff merge-fn))
  (let* ((result nil)
	 (next-candidate (first (cdr (first (mss-state-remaining mss-state)))))
	 (continue t)
	 (next-remaining-1
	  (if (equal '(new) (first next-candidate)) 
            (rest (mss-state-remaining mss-state))
            (loop for subset-entry in (rest (mss-state-remaining mss-state)) while continue collect
                  (let ((new-entry
                         (delete-if #'null
                                    (mapcar #'(lambda (poss+mrs+weight)
                                                (cons
                                                 (remove (first (first next-candidate)) (first poss+mrs+weight))
                                                 (cdr poss+mrs+weight)))
                                            (cdr subset-entry))
                                    :key #'first)))
                    (setq continue new-entry)
                    (cons (car subset-entry) new-entry))))))
    ;;    (format t "~%next-candidate = ~A" next-candidate)
    ;;    (format t "~%next-remaining-1 = ~A" next-remaining-1)
    (when continue
      (dolist (cmr (second next-candidate))
	(let ((next-remaining nil))
	  (setq continue t)
	  (loop for entry in next-remaining-1 while continue do
                (let ((new-entry nil))
                  (loop for poss+mrs+weight in (cdr entry) while continue do
                        (let ((new-mrs nil))
                          (dolist (mr (second poss+mrs+weight))
                            (let ((new-bsl (merge-bindings-lists (mr-bsl cmr)
                                                                 (mr-bsl mr) :cxn-inventory cxn-inventory)))
                              (if new-bsl
				(push (make-merge-result (mr-expr mr) new-bsl (mr-added mr))
				      new-mrs)
				;; (dolist (bs (mr-bsl cmr))
                                ;; 				  (setq new-mrs
                                ;; 					(append new-mrs
                                ;; 						(funcall merge-fn (car entry) 
                                ;; 							 (elt superset (first (first poss+mrs+weight)))
                                ;; 							 bs
                                ;; 							 :cutoff (when cutoff
                                ;; 								   (- cutoff (mss-state-g-cost mss-state)))))))
				)))
                          (when new-mrs
                            (push (list (car poss+mrs+weight) new-mrs (third poss+mrs+weight))
                                  new-entry))))
                  (setq continue new-entry)
                  (when new-entry
                    (push (cons (car entry) 
                                (sort new-entry #'< :key #'third))
                          next-remaining))))
	  (when continue
            ;; 	    (format t "~%next-candidate=~A" next-candidate)
            ;; 	    (format t "~%next-remaining=~A" next-remaining)
	    (push (make-mss-state :mr-so-far (make-merge-result (cons (mr-expr cmr)
								      (mr-expr (mss-state-mr-so-far mss-state)))
								(mr-bsl cmr)
								(append (mr-added cmr)
									(mr-added (mss-state-mr-so-far mss-state))))
				  :remaining next-remaining
				  :g-cost (+ (mss-state-g-cost mss-state)
					     (third next-candidate))
				  :h-cost (reduce #'+ next-remaining :key #'(lambda (subset-elt-entries) 
									      (third (first (cdr subset-elt-entries)))))
				  :taken (cons (car (first next-candidate))
					       (mss-state-taken mss-state)))
		  result)))))
    (when (rest (cdr (first (mss-state-remaining mss-state))))
      (unless (= (third next-candidate)
		 (third (second (cdr (first (mss-state-remaining mss-state))))))
	(incf (mss-state-h-cost mss-state)
	      (- (third (second (cdr (first (mss-state-remaining mss-state)))))
		 (third next-candidate))))
      (setf (cdr (first (mss-state-remaining mss-state)))
	    (cddr (first (mss-state-remaining mss-state))))
      (push mss-state result))
    result))

(defun make-subset (subset superset bindings &key (test-fn nil) (merge-fn #'fcg-merge) cutoff (destructive t)
                           (remove-special-operators t) cxn-inventory)
  "Add elements to superset so that subset is a subset of it."
  ;; This is implemented as a search process.
  ;; Search states holds:
  ;;  - a merge result so far
  ;;  - a description of what still needs to be merged-in
  ;;  - score fields and a field for 'taken'

  ;; The description of what still needs to be merged-in consists of a
  ;; list of entries. Each entry is a list of the form
  ;;   '(subset-elt (positions merge-results weight))
  ;; where positions is a list of positions of superset elements that
  ;; merge with the subset element etc.
  (let ((q (make-instance 'queue))
	(initial-state (initial-mss-state subset superset bindings test-fn merge-fn cutoff destructive
					  :remove-special-operators remove-special-operators :cxn-inventory cxn-inventory))
	(results nil))
    (when (and initial-state
	       (or (null cutoff)
		   (<= (+ (mss-state-g-cost initial-state)
			  (mss-state-h-cost initial-state))
		       cutoff)))
      (enqueue-by-priority q (list initial-state)
			   #'(lambda (ss-state)
			       (+ (mss-state-g-cost ss-state)
				  (mss-state-h-cost ss-state))))
      (loop while (not (empty-queue? q)) do
            (let ((current (remove-front q)))
              ;; 	     (format t "~%+------------------------------------
              ;;    current:~%   ~A~%q: ~A~%results:~%   ~A" current (elements q) results)
              (cond ((and results (> (+ (mss-state-g-cost current)
                                        (mss-state-h-cost current))
                                     (+ (mss-state-g-cost (first results))
                                        (mss-state-h-cost (first results)))))
                     ;;		    (format t "~%case 1")
                     (setf (elements q) nil))
                    ((null (mss-state-remaining current))
                     ;;		    (format t "~%case 2")
                     (push current results))
                    (t 
                     ;;		    (format t "~%case 3")
                     (enqueue-by-priority q 
                                          (successor-states current merge-fn cutoff superset :cxn-inventory cxn-inventory)
                                          #'(lambda (mss-state)
                                              (+ (mss-state-g-cost mss-state)
                                                 (mss-state-h-cost mss-state))))))))
      #+dbg
      (when (and results cutoff)
	(assert (>= cutoff (mss-state-g-cost (first results)))))
      ;;      (format t "~%====> results=~A" results)
      (setq results
	    (mapcar #'(lambda (mss-state)
			(let ((mr (mss-state-mr-so-far mss-state)))
			  (dotimes (i (length superset))
			    (unless (find i (mss-state-taken mss-state))
			      (push (elt superset i) 
				    (mr-expr mr))))
			  mr))
		    results))
      ;;       (format t "~%====> results =~A" results) 
      ;;       (format t "~%+--------------------------------------------~%")
      results)))


(defun compare-no-tags (expr1 expr2 compare-fn)
  (funcall compare-fn 
	   (cond ((tag-p expr1) (third expr1))
		 ((overrides-p expr1) (second expr1))
		 (t expr1))
	   (if (tag-p expr2) (third expr2) expr2)))

(defun equal-no-tags (expr1 expr2)
  (equal (if (tag-p expr1) (third expr1) expr1)
	 (if (tag-p expr2) (third expr2) expr2)))

(defun make-compare-no-tags-fn (fn)
  #'(lambda (expr1 expr2)
      (compare-no-tags expr1 expr2 fn)))


;; ############################################################################
;; VI. Tags

(export '(tag-p TAG TAG-ALL))

(defun tag-p (x)
  (and (consp x)
       (or (eq 'TAG (first x))
	   (eq 'TAG-ALL (first x)))))

(defun unify-TAGs (tags y bindings-list &optional (unify-fn #'unify) &key cxn-inventory)
  (do ((remaining (rest tags) (rest (rest remaining))))
      ((or (null remaining)
	   (fail? bindings-list))
       bindings-list)
    (setq bindings-list
	  (loop for bs in (funcall unify-fn (second remaining) y bindings-list :cxn-inventory cxn-inventory)
                append (unify (first remaining) 
                              (substitute-bindings 
                               bs
                               (remove-special-operators (second remaining) bs))
                              (list bs)
                              :cxn-inventory cxn-inventory)))))

(defun merge-tag (tag-variable tag-expression y bsl merge-fn cutoff &key cxn-inventory)
  (let ((res nil))
    (dolist (bs bsl)
      (let ((mrs (funcall merge-fn tag-expression y bs :cutoff cutoff :cxn-inventory cxn-inventory)))
        ;;	(format t "~%mrs=~A" mrs)
	(setq mrs
	      (loop for mr in mrs append
                    (let ((tmp nil))
                      (dolist (bs (mr-bsl mr))
                        (let* ((var-binding (substitute-bindings bs tag-expression))
                               (prev (find var-binding tmp :key #'car :test #'equal)))
                          (if prev
                            (push bs (cdr prev))
                            (push (list var-binding bs) tmp))))
                      ;;		     (format t "~%tmp=~A" tmp)
                      ;; now tmp is a list of (var-binding . bsl)
                      (mapcar #'(lambda (e)
                                  ;; 				 (format t "~%bsl from~%   (unify '~A '~A '~A)~%   => ~A"
                                  ;; 					 tag-variable (car e) (cdr e)
                                  ;; 					 (unify tag-variable (car e) (cdr e)))
                                  (make-merge-result (mr-expr mr) 
                                                     (unify tag-variable (car e) (cdr e)
                                                            :cxn-inventory cxn-inventory)
                                                     (mr-added mr)))
                              tmp))))
        ;;	(format t "~%mrs now=~A" mrs)
	(dolist (mr mrs)
	  (unless (null (mr-bsl mr))
	    (let ((prev (find (mr-expr mr) res :key #'mr-expr :test #'equal)))
	      (if prev
                (setf (mr-bsl prev) (append (mr-bsl mr) (mr-bsl prev)))
                (push mr res)))))))
    res))

(defun merge-tags (tags y bindings &key cutoff (merge-fn #'fcg-merge)
                        (remove-special-operators t) cxn-inventory)
  (declare (ignorable remove-special-operators))
  (let ((results (list (make-merge-result y (list bindings)))))
    (do ((tag-var-and-expression (rest tags) (rest (rest tag-var-and-expression))))
	((or (null tag-var-and-expression) (null results)))
      (setq results 
	    (loop for r in results append
                  (merge-tag (first tag-var-and-expression)
                             (second tag-var-and-expression)
                             (mr-expr r) 
                             (mr-bsl r)
                             merge-fn
                             (when cutoff
                               (- cutoff (length (mr-added r))))
                             :cxn-inventory cxn-inventory))))
    ;;(format t "~%rs=~A" results)
    results))

(defun clean-tag (tags bindings &key cxn-inventory)
  (let ((result nil))
    (do ((remaining (rest tags) (rest (rest remaining))))
	((or (null remaining) (fail? bindings)))
      ;;      (format t "~%remaining=~A" remaining)
      ;;      (format t "~%bindings=~A" bindings)
      (multiple-value-bind (pr new-bindings)
	  (remove-special-operators (second remaining) bindings)
	(setq bindings (unify-simple (first remaining) pr new-bindings :cxn-inventory cxn-inventory))
        ;; 	(format t "~%(fcg-merge~%   ~A~%   ~A~%   ~A)"
        ;; 		(second remaining) result bindings)
	(let ((mrs (if (consp (second (second remaining)))
                     (fcg-merge-no-vars (second remaining) result bindings :cxn-inventory cxn-inventory)
                     (fcg-merge (second remaining) result bindings :cxn-inventory cxn-inventory))))
		
          ;;	  (format t "~%mrs=~A" mrs)
	  (unless (and (= 1 (length mrs)) (= 1 (length (mr-bsl (first mrs)))))
	    (warn "TAG clean-fn: This case should still be implemented"))
	  (setq result (mr-expr (first mrs))
		bindings (first (mr-bsl (first mrs)))))))
    ;;(format t "~%=> ~A" result)))
    (values result bindings)))

(add-special-operator (make-instance 'special-operator
				     :representation 'TAG
				     :unify-fn #'unify-TAGs
				     :merge-fn #'merge-tags
				     :clean-fn #'clean-tag))



(defun fcg-merge-no-vars (pattern source bindings &key cxn-inventory)
  (multiple-value-bind (ipattern renamings)
      (instantiate-expression pattern)
    (multiple-value-setq (source renamings)
        (instantiate-expression source renamings))
    (let ((mrs (fcg-merge ipattern source bindings :cxn-inventory cxn-inventory)))
      (when mrs
	(dolist (ren renamings) (rotatef (car ren) (cdr ren)))
	(dolist (mr mrs)
	  (setf (mr-expr mr) (sublis renamings (mr-expr mr))
		(mr-bsl mr) (sublis renamings (mr-bsl mr))
		(mr-added mr) (sublis renamings (mr-added mr)))))
      mrs)))

(defun unify-tag-all (tag Y bindings-list &optional (unify-fn #'unify) &key cxn-inventory)
  (loop for bs in (funcall unify-fn (third tag) Y bindings-list :cxn-inventory cxn-inventory) append
        (unify (second tag) 
               (substitute-bindings bs 
                                    (remove-special-operators Y bs))
               (list bs))))

(defun clean-tag-all (tag bindings &key cxn-inventory)
  (multiple-value-bind (r new-bindings)
      (remove-special-operators (third tag) bindings)
    (values r (unify-simple (second tag) r new-bindings :cxn-inventory cxn-inventory))))

(defun merge-tag-all (tag Y bindings &key cutoff (merge-fn #'fcg-merge)
                          (remove-special-operators t) cxn-inventory)
  (declare (ignorable remove-special-operators t))
  (let ((mrs (funcall merge-fn (third tag) Y bindings :cutoff cutoff :cxn-inventory cxn-inventory)))
    (loop for mr in mrs do
          (setf (mr-bsl mr)
                (unify (second tag) (mr-expr mr) (mr-bsl mr) :cxn-inventory cxn-inventory)))
    (delete-if #'null mrs :key #'mr-bsl)))

(add-special-operator (make-instance 'special-operator
				     :representation 'TAG-ALL
				     :unify-fn #'unify-tag-all
				     :merge-fn #'merge-tag-all
				     :clean-fn #'clean-tag-all))


;; ############################################################################
;; '-> special operator (overrides)

(export '->)

(defun overrides-p (x)
  (and (consp x) (eq '-> (car x))))

(defun unify-overrides (pattern source bindings-list &key cxn-inventory)
  ;; pattern is of the form (-> to-unify replacement) 
  (unify (second pattern) source bindings-list :cxn-inventory cxn-inventory))

(defun merge-overrides (pattern source bindings &key cutoff (merge-fn #'fcg-merge) cxn-inventory &allow-other-keys)
  (declare (list pattern) (ignore cutoff) (ignore merge-fn))
  (let ((bsl (unify (second pattern) source (list bindings) :cxn-inventory cxn-inventory)))
    (when bsl
      (list (make-mr :expr (third pattern)
		     :bsl bsl
		     :added (unless (equal source (third pattern))
			      (list (third pattern))))))))

(add-special-operator (make-instance 'special-operator
				     :representation '->
				     :unify-fn #'unify-overrides
				     :merge-fn #'merge-overrides
				     :clean-fn #'(lambda (overrides bindings)
						   (values (third overrides) bindings))))

;; Examples:
;; (unify '(-> ?x (a ?x)) 'b) 
;; => (((?x . b)))
;; (unify '((unit (referent (-> ?x x2))))
;;        '((unit (referent x1))))
;; => (((?x . x1))) ;; now ?x could be used in other parts of the cxn
;; (match-structures '((unit (referent (-> ?x x2))))
;; 		  '((unit (referent x1))))
;; => (((?x . x1)))
;; (unify '(unit (syn-cat (==1 (number (-> singular plural)))))
;;        '(unit (syn-cat ((number singular)))))
;; => (((T . T)))
;; (unify-units '(unit (syn-cat (==1 (number (-> singular plural)))))
;; 	     '(unit (syn-cat ((number singular))))
;; 	     (list +no-bindings+))
;; => (((T . T)))
;; (unify '(unit (syn-cat (==1 (number (-> singular plural)))))
;;        '(unit (syn-cat ((number plural)))))
;; => NIL
;; (unify-units '(unit (syn-cat (==1 (number (-> singular plural)))))
;; 	     '(unit (syn-cat ((number plural))))
;; 	     (list +no-bindings+))
;; => NIL
;;(fcg-merge '(unit (phon-cat (==1 (vowels (== (1 (-> "i"
;;						    ""))))))) ;(-> "i" "")))))))
;;	   '(unit (phon-cat ((vowels ((1 "i") (2 "o"))))))
;;	   +no-bindings+)
;; (fcg-merge '(-> ?x (a ?x)) 'b +no-bindings+)
;; => (#S(MERGE-RESULT :EXPR #1=(A ?X) :BSL (((?X . B))) :ADDED (#1#)))
;; (fcg-merge '((unit (referent (-> ?x x2))))
;; 	   '((unit (referent x1)))
;; 	   +no-bindings+)
;; => (#S(MERGE-RESULT :EXPR ((UNIT (REFERENT X2))) :BSL (((?X . X1))) :ADDED (X2)))
;; (merge-structures '((unit (referent (-> ?x x2))))
;; 		  '((unit (referent x1))))
;; => (#S(MERGE-RESULT
;;     :EXPR ((UNIT (REFERENT X2)))
;;     :BSL (((?X . X1)))
;;     :ADDED ((UNIT (REFERENT (X2))))))
;; (fcg-merge '(unit (syn-cat (==1 (number (-> singular plural)))))
;; 	   '(unit (syn-cat ((number singular))))
;; 	   +no-bindings+)
;; => (#S(MERGE-RESULT
;;     :EXPR (UNIT (SYN-CAT ((NUMBER PLURAL))))
;;     :BSL (((T . T)))
;;     :ADDED (PLURAL)))
;; (merge-units '(unit (syn-cat (==1 (number (-> singular plural)))))
;; 	     '(unit (syn-cat ((number singular))))
;; 	     +no-bindings+)
;; => 
;; (#S(MERGE-RESULT
;;     :EXPR (UNIT (SYN-CAT ((NUMBER PLURAL))))
;;     :BSL (((T . T)))
;;     :ADDED ((UNIT (SYN-CAT (PLURAL))))))
;; (fcg-merge '(unit (syn-cat (==1 (number (-> singular plural)))))
;; 	   '(unit (syn-cat ((number plural))))
;; 	   +no-bindings+)
;; => (#S(MERGE-RESULT
;;     :EXPR (UNIT (SYN-CAT ((NUMBER PLURAL))))
;;     :BSL (((T . T)))
;;     :ADDED NIL))
;; (merge-units '(unit (syn-cat (==1 (number (-> singular plural)))))
;; 	     '(unit (syn-cat ((number plural))))
;; 	     +no-bindings+)
;; => (#S(MERGE-RESULT
;;     :EXPR (UNIT (SYN-CAT ((NUMBER PLURAL))))
;;     :BSL (((T . T)))
;;     :ADDED NIL))
;; (fcg-merge '(-> a b) '() +no-bindings+)
;; => (#S(MERGE-RESULT :EXPR B :BSL (((T . T))) :ADDED (B)))
;; (fcg-merge '((-> a b))
;; 	   '()
;; 	   +no-bindings+)
;; => (#S(MERGE-RESULT :EXPR (B) :BSL (((T . T))) :ADDED (B)))
