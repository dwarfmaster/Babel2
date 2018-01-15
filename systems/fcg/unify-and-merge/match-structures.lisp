(in-package :fcg)

(export '(*update-references* match-structures))

;;; 1. Matching of structures

(defun valid-component-p (p &optional (bs +no-bindings+))
  (or (atom p)
      (not (duplicates? (substitute-bindings bs (rest p)) 
 			:test #'equal))))

(defun valid-component-list-p (p &optional (bs +no-bindings+))
  (setq p (substitute-bindings bs p))
   (and (every #'valid-component-p p)
        (not (duplicates? p :test #'equal))))

(defun unify-components (c1 c2 &optional (bsl (list +no-bindings+)) &key cxn-inventory)
  (delete-if-not #'(lambda (bs)
		     (valid-component-p c1 bs))
		 (unify c1 c2 bsl :cxn-inventory cxn-inventory)))

(defun unify-component-values (v1 v2 &optional (bsl (list +no-bindings+)) &key cxn-inventory)
  (delete-if-not #'(lambda (bs)
		     (valid-component-list-p v1 bs))
		 (unify v1 v2 bsl :cxn-inventory cxn-inventory)))

(defun remove-cons-cell (dotted-list)
  "Turns '(a b . c) into '(a b)"
  (if (symbolp (rest dotted-list))
    (list (first dotted-list))
    (cons (first dotted-list) (remove-cons-cell (rest dotted-list)))))

(defun unify-features (f1 f2 bsl &key cxn-inventory)
  (cond ((eq 'TAG (unit-name f1))
	 (unify-tags f1 f2 bsl #'(lambda (f1 f2 bsl &key cxn-inventory)
				   (unify-features f1 f2 bsl :cxn-inventory cxn-inventory))
                     :cxn-inventory cxn-inventory))
	((eq 'TAG-ALL (unit-name f1))
	 (unify-tag-all f1 f2 bsl #'(lambda (f1 f2 bsl &key cxn-inventory)
				      (unify-features f1 f2 bsl :cxn-inventory cxn-inventory)) :cxn-inventory cxn-inventory))
	((string= (feature-name f1) (feature-name f2))
	 (case (feature-name f1)
	   ;; (meaning (unify-component-values (feature-value f1) (feature-value f2) bsl))
	   ;; (form (unify-component-values (feature-value f1) (feature-value f2) bsl))
	   (otherwise (unify (feature-value f1)
			     (feature-value f2)
			     bsl
                             :cxn-inventory cxn-inventory))))))

(defun unify-unit-features (fs1 fs2 bsl &key cxn-inventory)
  (loop for f1 in fs1 while bsl do
       (let ((f2 (find (if (tag-p f1)
			   (feature-name (third f1))
			   (feature-name f1))
		       fs2
		       :key #'feature-name
                       :test #'string=)))
	 (if f2
           ;; If feature with same feature-name is found in fs2 (transient structure unit)
           ;; then unify f1 with f2
           (setq bsl (unify-features f1 f2 bsl :cxn-inventory cxn-inventory))
           ;; otherwise, unify it with (feature-name nil)
           ;; This case is for example often used for footprints
           (setq bsl (unify-features f1 (make-feature (feature-name f1) nil) bsl :cxn-inventory cxn-inventory)))))
  bsl)

(defun unify-units (u1 u2 bsl &key cxn-inventory)
  (setq bsl (unify (unit-name u1) (unit-name u2) bsl :cxn-inventory cxn-inventory))
  (unless (fail? bsl)
    (unify-unit-features (unit-features u1)
			 (unit-features u2)
			 bsl
                         :cxn-inventory cxn-inventory)))

(defun unify-structures (pattern source bsl &key cxn-inventory)
  (setq pattern (remove-J-units pattern))
  (when (<= (length pattern) (length source))
    (cond ((null pattern) bsl)
	  ((<= (length pattern) (length source))
	   (subset-p pattern source bsl :unify-fn #'(lambda (u1 u2 bsl &key cxn-inventory)
						      (unify-units u1 u2 bsl :cxn-inventory cxn-inventory))
                     :cxn-inventory cxn-inventory))
	  (t +fail+))))

(define-event matching-finished (pattern list) (source list) (bindings-list list))

(defun match-structures (pattern source &optional (bindings +no-bindings+) &key cxn-inventory)
  (let ((result (unify-structures pattern source (list bindings) :cxn-inventory cxn-inventory)))
    (notify matching-finished pattern source result)
    result))

;;; 2. Merging of structures

(defun merge-components (c1 c2 bindings &key cutoff cxn-inventory)
  (declare (ignore cutoff))
  (let ((bsl (unify-components c1 c2 (list bindings) :cxn-inventory cxn-inventory)))
    (when bsl
      (list (make-merge-result c2 bsl)))))


(defun merge-component-values (m1 m2 bindings &key cutoff cxn-inventory)
  (if (and (consp m1) (merge-fn (first m1)))
      (let ((mrs (merge-special m1 m2 bindings cutoff #'merge-components :cxn-inventory cxn-inventory)))
	(dolist (mr mrs)
	  (setf (mr-bsl mr)
		(delete-if-not
		 #'(lambda (bs)
		     (valid-component-list-p (mr-expr mr) bs))
		 (mr-bsl mr))))
	(delete-if #'null mrs :key #'mr-bsl))
      (let ((bsl (unify-component-values m1 m2 (list bindings) :cxn-inventory cxn-inventory)))
	(when bsl (list (make-merge-result m2 bsl))))))

(defun merge-features (f1 f2 bindings &key cutoff cxn-inventory)
  (cond ((eq 'TAG (feature-name f1))
	 (merge-tags f1 f2 bindings :merge-fn #'merge-features :cutoff cutoff :cxn-inventory cxn-inventory))
	((eq 'TAG-ALL (feature-name f1))
	 (merge-tag-all f1 f2 bindings :merge-fn #'merge-features :cutoff cutoff :cxn-inventory cxn-inventory))
	((eq (feature-name f1) (feature-name f2))
	 (let* ((pattern (feature-value f1))
		(value-merges
		 (case (feature-name f1)
		   ;;(referent (merge-referent-values (feature-value f1) (feature-value f2) bindings :cutoff cutoff))
		   (meaning (if (or (variable-p pattern) (variable-p (feature-value f2)))
                              (fcg-merge pattern (feature-value f2) bindings :cutoff cutoff :cxn-inventory cxn-inventory)
                              (merge-component-values (feature-value f1) (feature-value f2) bindings
                                                      :cutoff cutoff :cxn-inventory cxn-inventory)))
		   (form (fcg-merge (feature-value f1) (feature-value f2) bindings :cutoff cutoff :cxn-inventory cxn-inventory))
                   ;; (merge-component-values (feature-value f1) (feature-value f2) bindings :cutoff cutoff))
		   ;;(boundaries (fcg-merge (feature-value f1) (feature-value f2) bindings :cutoff cutoff))
		   (otherwise (fcg-merge pattern
					 (feature-value f2)
					 bindings :cutoff cutoff :cxn-inventory cxn-inventory)))))
;;	   (format t "~%f1=~A,~%f2=~A~%pattern=~A~%bs=~A" f1 f2 pattern bindings)
;;	   (format t "~%vm=~A" value-merges)
	   (let ((result nil))
	     (dolist (mr value-merges)
	       (setf (mr-expr mr) 
		     (make-feature (feature-name f1) (mr-expr mr)))
	       (when (mr-added mr)
		 (setf (mr-added mr)
		       (list (list (feature-name f1) (mr-added mr)))))
	       (if (= 1 (length (mr-bsl mr)))
		   (push mr result)
		   (dolist (bs (mr-bsl mr))
		     (let* ((e (substitute-bindings bs (mr-expr mr)))
			    (prev (find e result :key #'mr-expr :test #'equalp)))
		       (if prev 
			   (push bs (mr-bsl prev))
			   (push (make-merge-result e (list bs))
				 result))))))
	     result)))))

(defun merge-unit-features (fs1 fs2 bindings &key cutoff cxn-inventory)
  (make-subset fs1 fs2 bindings
	       :test-fn #'(lambda (f1 f2) 
			    (eq (if (tag-p f1) (feature-name (third f1)) (feature-name f1))
				(if (tag-p f2) (feature-name (third f2)) (feature-name f2))))
	       :merge-fn #'merge-features
               :cutoff cutoff
               :cxn-inventory cxn-inventory))

(defun merge-units (u1 u2 bindings &key cutoff cxn-inventory)
  (setq bindings (unify-simple (unit-name u1) (unit-name u2) bindings :cxn-inventory cxn-inventory))
  (unless (fail? bindings)
    (let ((mrs (merge-unit-features (unit-features u1)
				    (unit-features u2)
				    bindings
				    :cutoff cutoff
                                    :cxn-inventory cxn-inventory)))
      (dolist (mr mrs)
	(setf (mr-expr mr) (make-unit :name (unit-name u2)
				      :features (mr-expr mr)))
	(when (mr-added mr)
	  (setf (mr-added mr)
		(list (cons (unit-name u2) (mr-added mr))))))
      mrs)))

(defun merge-structures-without-Js (pattern source bindings &key cxn-inventory)
  ;;  (format t "~%bs=~A" bindings)
  (make-subset (substitute-bindings bindings (remove-J-units pattern))
               source 
               bindings
               :test-fn #'(lambda (u1 u2)
                            (unify-simple (unit-name u1) (unit-name u2) bindings :cxn-inventory cxn-inventory))
               :merge-fn #'merge-units
               :cxn-inventory cxn-inventory))

(export '(merge-structures))
;;; Handling of J-units. 
;;; A J-unit looks as follows:
;;; ((J ?name ?parent (?child-1 ... ?child-n))
;;;  new-features
;;;  tags)     
;;; They are handled in the same order as they appear in the construction
;;; Each of them is handled as follows: 

;;; 1) unless the first argument is already bound, a new constant
;;;    binding is created for it. This will correspond to the name of
;;;    the newly created unit
;;; 2) The (new) unit will receive the specified new-features
;;; 3) It will also extract all feature (values) specified by tags
;;;    from their original units
;;; 4) The (new) unit will become a child of the ?parent unit and receives
;;;    the children-units as specified
(defun remove-tag-from-source (tag-variable pattern-unit source-unit source bindings &key cxn-inventory)
  (let ((new-unit nil)
	(processed-feature-names nil))
    #+dbg
    (format t "~%tv~% ~A,~% pu ~A,~% su ~A,~% s ~A~% bs ~A" tag-variable pattern-unit source-unit source bindings)
    ;; first we locate the tag in the pattern
    ;; we construct a new-unit which will later be added to the resulting structure
    (setq new-unit (make-unit :name (unit-name source-unit)))
    ;; for each feature in the pattern unit containing the tag
    (dolist (feature (unit-features pattern-unit))
      ;; if the feature happens to be a tag and contains the variable we want to remove
      #+dbg
      (format t "~%f=~A, tag-p: ~A, find(~A): ~A" feature (tag-p feature) tag-variable (find tag-variable (rest feature)))
      (when (and (tag-p feature)
                 (find tag-variable (rest feature)))
        ;; we add the feature name to the list of processed-feature-names
        ;; (?unit (TAG ?tag (feature-name ...)))
        (push (feature-name (third feature)) processed-feature-names)
        (let ((original-feature (unit-feature source-unit 
                                              (feature-name (third feature)))))
	  #+dbg
	  (format t "~%of=~A,~%tv=~A" original-feature (lookup tag-variable bindings))
          (unless (atom (feature-value original-feature))
            ;; we only keep feature-values that are not part of the binding for
            ;; the tag-variable
            (let ((feature-value nil))
              (dolist (value-element
			(remove-special-operators (feature-value original-feature) bindings))
                (if (and (consp (feature-value (lookup tag-variable bindings)))
			 (find value-element
			       (feature-value (lookup tag-variable bindings))
			       :test #'(lambda (x y)
					 (or (equal x y)
					     (unify x y (list bindings) :cxn-inventory cxn-inventory)))))
		    (setf (feature-value (lookup tag-variable bindings))
			  (remove value-element (feature-value (lookup tag-variable bindings))
				  :test #'(lambda (x y)
					    (or (equal x y)
						(unify x y (list bindings) :cxn-inventory cxn-inventory)))
				  :count 1))
		    (push value-element feature-value))
		#+dbg
		(format t "~%bs after: ~A" bindings))
              (when feature-value
                ;; we add newly constructed feature to new-unit
                (push (make-feature (feature-name original-feature)
                                    feature-value)
                      (unit-features new-unit))))))))
    ;; we loop over all features in source-unit and 'copy' all features
    ;; that are not processed
    (dolist (feature (unit-features source-unit))
      (unless (find (feature-name feature) processed-feature-names)
	(push feature (unit-features new-unit))))
    ;; the new source is reconstructed by incorporating the new unit
    (loop for unit in source collect
	 (if (eq (unit-name unit) (unit-name new-unit))
	     new-unit
	     unit))))

(defun remove-tag-from-added (tag-variable pattern added bindings &key cxn-inventory)
  ;; should be non-destructive; needed for the cases in which a tag is first merged 
  ;; into a normal unit and then this tagged feature/value would be moved by means
  ;; of a tag; without this removal this value it would incorrectly indicate that
  ;; it is merged twice
  (let ((tag-removed-from-unit nil))
    (loop for pattern-unit in (remove-J-units pattern) until tag-removed-from-unit do
	 (when (find tag-variable (variables-in pattern-unit))
	   (setf tag-removed-from-unit
		 (structure-unit added (or (lookup (unit-name pattern-unit)
						   bindings)
					   (unit-name pattern-unit))))))
    (if tag-removed-from-unit
	(let ((tagged-feature (lookup tag-variable bindings)))
	  (loop for unit in added
	     if (eq (unit-name tag-removed-from-unit)
		    (unit-name unit))
	     append (let ((features (loop for feature in (unit-features unit)
				       if (eq (feature-name feature)
					      (feature-name tagged-feature))
				       append (if (variable-p (feature-value tagged-feature))
						  (list (list (feature-name feature)))
						  (let ((values (copy-list (feature-value feature))))
						    (loop for value in (feature-value tagged-feature)
						       do (setf values
								(remove value values
									:test #'(lambda (x y)
										  (or (equal x y)
										      (unify x y (list bindings)
                                                                                             :cxn-inventory cxn-inventory)))
									:count 1)))
						    (when values
						      (list (list (feature-name feature) values)))))
				       else
				       collect feature)))
		      (when features
			(list (cons (unit-name unit) features))))
	     else
	     collect unit))
	added)))


(define-event warn-unable-to-determine-domain (structure t))

(defun make-child (unit-name parent-name source &optional unit-name-var)
  (unless (or (eq unit-name parent-name)
	      (find (structure-unit source parent-name)
		    (subunits (structure-unit source unit-name) source)))	      
    ;; (format t "~%(make-child ~% '~A~% '~A~% '~A~%~%"
    ;; 	    unit-name parent-name source)
    ;; first delete unit from its current parrent:
    (loop for unit in source 
       unless (eq (unit-name unit) unit-name)
       do (let ((subunits-feature (get-subunits-feature unit)))
	    (when (find unit-name (feature-value subunits-feature))
	      (setf (feature-value subunits-feature)
		    (delete unit-name (feature-value subunits-feature))))))
    ;; now add it to the new parent:
    (let* ((parent-unit (structure-unit source parent-name))
	   (subunits-feature (get-subunits-feature parent-unit)))
      ;; (format t "~%~%unit-name=~A, parent-name=~A~%source=~A~%parent-unit=~A~%subunits-feature=~A~%~%"
      ;; 	      unit-name parent-name source parent-unit subunits-feature)
      (unless parent-unit
	(return-from make-child nil))
      (cond (subunits-feature
             (unless (or (find unit-name (feature-value subunits-feature))
                         (find unit-name-var (feature-value subunits-feature)))
               (push unit-name (feature-value subunits-feature))))
            ((or (find-anywhere 'meaning source)
                 (find-anywhere 'referent source)
                 (find-anywhere 'sem-cat source)
                 (find-anywhere 'sem-subunits source))
             (push (make-feature 'sem-subunits (list unit-name))
                   (unit-features parent-unit)))
            ((or (find-anywhere 'form source)
                 (find-anywhere 'syn-cat source)
                 (find-anywhere 'syn-subunits source))
             (push (make-feature 'syn-subunits (list unit-name))
                   (unit-features parent-unit)))
            (t (notify warn-unable-to-determine-domain source)
               (push (make-feature 'subunits (list unit-name))
                     (unit-features parent-unit)))))
    ;; (format t "~%finished")
    source))

(define-event warn-unable-to-merge-tag-value (j-unit list) (tag-variable symbol) (tag-value list) (unit list))
(define-event warn-tag-variable-unbound (j-unit list) (tag-variable symbol))
(define-event warn-hierarchy-in-structure-broken (j-unit list) (pattern list) (bindings list))

(defun get-tag (tag-variable unit)
  (let ((R nil))
    (do* ((tags (mappend #'rest (find-all 'TAG (remove-if-not #'consp (unit-features unit)) :key #'first)) (rest (rest tags)))
	  (tag-var (first tags) (first tags)))
	 ((or R (null tags)) R)
      (when (eq tag-variable tag-var)
	(setq R (second tags))))))

(defun retrieve-special-operator (tag-val bindings)
  (let (special-op)
    (when (consp tag-val)
      (setq special-op (first tag-val))
      (if (eq special-op '++)
          (retrieve-special-operator (fcg-expand (second tag-val) :merge? t :bindings bindings 
                                                 :source nil :value (third tag-val))
                                     bindings)
          special-op))))
  

(defun handle-J-unit (J-unit pattern source bindings tag-variables &optional added &key cxn-inventory)
  "Helper function for merging of structures called from
HANDLE-J-UNITS. Returns a list of MERGE-RESULTs."
  #+dbg
  (format t "~%++++++++++++++ hande-J-unit ++++++++++++++
   J-unit = ~A
   pattern = ~A
   source = ~A
   bindings = ~A
   added = ~A" J-UNIT pattern source bindings added)
  (unless (or (not (variable-p (second (unit-name J-unit))))
	      (and (lookup (second (unit-name J-unit)) bindings)
		   (not (variable-p (lookup (second (unit-name J-unit)) bindings)))))
    (let ((new-unit-name (make-const (second (unit-name J-unit)))))
      (setq bindings (extend-bindings (second (unit-name J-unit))
				      new-unit-name
				      bindings))))
  (let ((new-unit (structure-unit source (or (lookup (second (unit-name J-unit)) bindings)
					     (second (unit-name J-unit)))))
	(tags nil))
    (unless new-unit
      (setq new-unit (make-unit :name (or (lookup (second (unit-name J-unit)) bindings)
					  (second (unit-name J-unit)))))
      (push new-unit source))
    ;; First move the tags. Currently this is only implemented for
    ;; tag-variables bound to an entire feature (i.e. including the
    ;; feature's name.)
    (dolist (e (unit-features J-unit))
      #+dbg
      (format t "~%e=~A" e)
      (when (atom e)	
	(if (find e tag-variables)
	    (let ((tag-value (substitute-bindings bindings (lookup e bindings)))
		  pattern-unit source-unit special-op)
	      (loop for unit in (remove-J-units pattern) until pattern-unit do
		   (when (and (find 'TAG (unit-features unit) :key #'feature-name)
			      (find e (variables-in unit)))
		     (setq pattern-unit unit)
		     (setq source-unit (structure-unit source
						       (or (lookup (unit-name pattern-unit)
								   bindings)
							   (unit-name pattern-unit))))))
	      ;; now pattern-unit is the pattern unit containing the tag and source
	      ;; unit is the corresponding unit in the source
	      #+dbg
	      (format t "~%pu=~A, su=~A" pattern-unit source-unit)
	      #+dbg
	      (assert (and pattern-unit source-unit))
	      (push e tags)
	      ;; bindings need to be copied because the values in it might be
	      ;; changed as side effect, for example while removing tag-values
	      ;; from a unit:
	      (setq source
		(remove-tag-from-source e pattern-unit source-unit source (copy-tree bindings) :cxn-inventory cxn-inventory))
	      (setq added
		(remove-tag-from-added e pattern added bindings :cxn-inventory cxn-inventory))
	      #+dbg
 	      (format t "~%e=~A=>source=~A" e source)
              ;; As far as I understand: at this point the tag
              ;; variable is bound to its value BUT for some reason
              ;; without the initial special operator... Therefore the
              ;; tag is looked up again and the special operator is
              ;; fetched. This however does not work for ++ operator.
	      (let ((tag-val (feature-value (get-tag e pattern-unit))))
                (setq special-op (retrieve-special-operator tag-val bindings)))
	      #+dbg
 	      (format t "~%(values '~A '~A '~A)" 
		      special-op
		      tag-value
 		      (make-feature (feature-name tag-value)
				    (cons special-op (feature-value tag-value)))
 		      )
	      (let* ((to-merge (if special-op
				   (make-feature (feature-name tag-value)
						 (cons special-op (feature-value tag-value)))
				   tag-value))
		     (merges (merge-unit-features
			      (list to-merge)
			      (unit-features new-unit)
			      bindings :cxn-inventory cxn-inventory)))
		#+dbg
		(format t "~% => ~A" merges)
		(unless merges
		  (notify warn-unable-to-merge-tag-value J-unit e tag-value new-unit)
		  (return-from handle-J-unit nil))
		#+dbg
		(assert (and (= (length merges) 1)
			     (= 1 (length (mr-bsl (first merges))))))
		(setf (unit-features new-unit) (mr-expr (first merges)))
		(setq bindings (first (mr-bsl (first merges))))
		(setf added
		      (let ((merge-results (merge-structures
					    (list (make-unit 
						   :name (unit-name new-unit)
						   :features
						   (list (if special-op 
							     (make-feature (feature-name tag-value)
									   (cons special-op (feature-value tag-value)))))))
					    added
					    bindings
                                            nil
                                            :cxn-inventory cxn-inventory)))
			#+dbg
			(assert (= (length merge-results) 1))
			(mr-expr (first merge-results))))))
	    (progn
	      (notify warn-tag-variable-unbound J-unit e)
	      (return-from handle-J-unit nil)))))
    #+dbg
    (format t "~%source=~A" source)
    ;; Next change the hierarchical structure.
    (when (third (unit-name J-unit))
      (let ((parent-name (or (lookup (third (unit-name J-unit)) bindings) (third (unit-name J-unit)))))
;; 	(unless (find parent-name source :key #'unit-name)
;; 	  (when (variable-p parent-name)
;; 	    (let ((new-unit-name (make-const (second (unit-name J-unit)))))
;; 	      (setq bindings (extend-bindings parent-name
;; 					      new-unit-name
;; 					      bindings)
;; 		    parent-name new-unit-name)))
;; 	  (push (make-unit :name parent-name) source))
	(setq source 
	      (make-child (unit-name new-unit) 
			  parent-name
			  source
                          (second (unit-name J-unit)))))
      (unless source
	(notify warn-hierarchy-in-structure-broken J-unit pattern bindings)
        (return-from handle-J-unit nil)))
    #+dbg
    (format t "~%2 source=~A" source)
    (dolist (unit-name (fourth (unit-name J-unit)))
      (setq source 
	    (make-child (or (lookup unit-name bindings) unit-name)
			(unit-name new-unit)
			source
                        unit-name)))
    ;; Finally add all features to the new unit.
    #+dbg
    (format t "~%3 source=~A" source)
    (let ((mrs (merge-unit-features (substitute-bindings 
				     bindings
				     (set-difference 
				      (unit-features J-unit)
				      tags))
				    (substitute-bindings bindings (unit-features new-unit)) ;; Remi: avoids problem of NIL in circular bindings
				    bindings
                                    :cxn-inventory cxn-inventory)))
      (dolist (merge-result mrs)
	(let* ((c-source (copy-tree source))
	       (nu (structure-unit c-source (unit-name new-unit))))
	  (setf (unit-features nu) (mr-expr merge-result))
	  (setf (mr-expr merge-result) c-source)
	  (setf (mr-added merge-result)
		(let ((merge-results 
		       (merge-structures
			(when (mr-added merge-result)
			  (list (cons (unit-name new-unit)
				      (mr-added merge-result))))
			added
			(first (mr-bsl merge-result)))))
		  (when (= (length merge-results) 1)
		    (mr-expr (first merge-results)))))))
      (setf mrs (delete-if #'null mrs :key #'mr-expr))
      #+dbg
      (format t "~%-------------- handle-J-unit ---------------")
      mrs)))

;; new functions for updating references

(defun update-references-upwards (old-values new-value new-source starting-unit)
  (let ((updated-units nil))
    (loop for current-unit = (get-parent-unit starting-unit new-source)
       then (let ((parent-unit (get-parent-unit current-unit new-source)))
              (unless (or (eq parent-unit current-unit)
                          (member parent-unit updated-units))
                parent-unit))
       while current-unit
       do ;; (format t "~% current-unit: ~a old-values: ~a new-value: ~a"
       ;;         (unit-name current-unit) old-values new-value)
       (nsublis (mapcar #'(lambda (x) (cons x new-value))
                        old-values)
                ;; only update ordering constraints
                (loop for form-constraint in (unit-feature-value current-unit 'form)
                   unless (member (first form-constraint) '(string stem))
                   collect form-constraint))
       ;; could check for duplicates in old-values but might be
       ;; more costly than just leaving them in the list
       (push current-unit updated-units)
       (push new-value old-values)
       (setf new-value (unit-name current-unit)))))

(defun collect-form-values (form-constraints)
  ;; collect all special values from form constraints that
  ;; might need to be updated
  (remove-duplicates
   (loop for form-constraint in form-constraints
      when (consp form-constraint)
      append (case (first form-constraint)
	       ((string stem) (list (second form-constraint)))
	       ((meets precedes pattern) (cdr form-constraint))
	       ((or) (collect-form-values (cdr form-constraint)))
	       (otherwise nil)))))

(defun unit-update-references (new-source unit-name)
  (let* ((unit (structure-unit new-source unit-name))
	 (form-constraints (delete-duplicates (remove-if-not #'consp (unit-feature-value unit 'form))
					      :test #'equal)))
    ;; when there are no form-constraints we don't need to update any
    ;; references
    (unless form-constraints
      (return-from unit-update-references new-source))
    ;; otherwise we update the references by first collecting all
    ;; values that need to be changed
    (let ((values-to-be-updated (collect-form-values form-constraints)))
     ;; (format t "~%unit-name: ~a form-constraints: ~a values-to-be-updated: ~a"
     ;;            unit-name form-constraints values-to-be-updated)
      (update-references-upwards values-to-be-updated unit-name new-source unit))))

(defun update-references (new-source j-units bindings)
  (loop for unit-name in  
       (loop for j-unit in j-units
	  for unit-name = (unit-name j-unit)
	  for unit-name-vars = (cons (second unit-name)
				     (fourth unit-name))
	  append (loop for unit-name-var in unit-name-vars
		    collect (cdr (get-binding unit-name-var bindings))))
       do (unit-update-references new-source unit-name))
  new-source)

(defvar *update-references* nil)

(defun tag-get-variables (tag)
  (let ((r nil))
    (do ((remaining (rest tag) (cddr remaining)))
	((null remaining) r)
      (push (first remaining) r))))
(defun unit-get-tag-variables (unit-structure)
  (loop for unit in unit-structure append
       (loop 
	  for feature in (unit-features unit)
	  when (tag-p feature)
	  append (tag-get-variables feature))))

(defun get-subunits (pole)
 (remove nil
         (loop for unit in pole
               for subunits = (or (unit-feature-value unit 'syn-subunits)
                                 (unit-feature-value unit 'sem-subunits)
                                 (unit-feature-value unit 'subunits))
               when subunits
               collect (list (unit-name unit)
                             subunits))))

(defun get-superunit (unit-name subunit-structure-in-pole)
  (loop for (superunit subunits) in subunit-structure-in-pole
        when (find unit-name subunits :test #'string=)
        return superunit))

(defun get-all-subunits (superunit subunit-structure-in-pole)
  (loop for (a-superunit subunits) in subunit-structure-in-pole
        when (eq a-superunit superunit)
        return subunits))

(defun expand-root-form (form-feature-value subunit-structure-in-pole)
  (if subunit-structure-in-pole
    (let* ((expansion-for-form-feature-value
            (loop for feature in form-feature-value
                  for form-type = (first feature)
                  for form-content = (rest feature)
                  if (or (eq form-type 'meets)
                         (eq form-type 'precedes))
                  collect `(,form-type
                            ,@(loop for unit-name in form-content
                                    for superunit
                                    = (get-superunit unit-name subunit-structure-in-pole)
                                    if (and superunit
                                            (null (permutation-of?
                                                   (get-all-subunits
                                                    superunit subunit-structure-in-pole)
                                                   form-content)))
                                    ;;EXPANSION HAPPENS HERE > replace unit name by superunit name
                                    collect superunit 
                                    else collect unit-name))))
           (new-form-feature-value
            (sort
             (remove-duplicates
              (append form-feature-value expansion-for-form-feature-value) :test #'equalp)
             #'string-lessp :key #'first)))
      new-form-feature-value)
    form-feature-value))

(defun update-references-in-root (new-source)
  (when (find 'root new-source :key #'first)
    (let ((root-unit (find 'root new-source :key #'first))
          (subunits-features (get-subunits new-source))
          (new-root-unit nil))
      (setf new-root-unit
             `(root
               ,@(loop for (feature value) in (rest root-unit)
                  if (eq feature 'form)
                  collect (list feature (expand-root-form value subunits-features))
                  else collect (list feature value))))
      (setf new-source (remove root-unit new-source))
      ;;add new root unit
      (setf new-source (append new-source (list new-root-unit)))))
  new-source)

(defun handle-J-units (pattern source bindings &optional added &key cxn-inventory)
  ;; returns a list of MERGE-RESULT cells (i.e. (new-source . new-bindings-list) cells.)
  ;; for every J-unit:
  ;;    - for every binding: handle-J-unit
  ;;      This returns a list of (new-source . bsl) cells
  ;;    - One such cell provides either an output cell 
  ;;      or else the input for the next J-unit with a modified pattern
  ;;    - The modified pattern is always the same
  ;;    - different bindings may return the same new-source
  #+dbg
  (format t "~%++++++++++++++ handle-J-units ++++++++++++++++
   pattern  = ~A
   source   = ~A
   bindings = ~A" pattern source bindings)
;;  (format t "~%hjus ~A" bindings)
  (let ((results (list (make-merge-result source (list bindings) added)))
	(J-units (get-J-units pattern))
	(tag-variables (unit-get-tag-variables pattern)))
    (dolist (J-unit J-units)
;;       (format t "~%J-unit=~A" J-unit)
;;       (format t "~%results=~A" results)
;;       (format t "~%pattern=~A" pattern)
      (let ((new-results nil))
	(dolist (r results)
	  (dolist (bs (mr-bsl r))
;; 	    (format t "~%bs=~A" bs)
;;  	    (format t "~%=>(handle-j-unit~%   '~A~%   '~A~%   '~A~%   '~A)~%=> ~A"
;;  		    J-UNIT pattern (first r) bs
;;  		    (handle-j-unit J-unit pattern (first r) bs))
	    (dolist (j-result (handle-J-unit J-unit pattern (mr-expr r) bs tag-variables (mr-added r) :cxn-inventory cxn-inventory))
	      #+dbg
	      (format t "   j-result=~A" j-result)
	      (let ((prev (find (mr-expr j-result) new-results :key #'mr-expr :test #'equal)))
		(if prev
		    (setf (mr-bsl prev) (append (mr-bsl j-result) (mr-bsl prev)))
		    (push j-result new-results))))))
	(setq results new-results
	      pattern (cons (make-unit :name (second (unit-name J-unit))
				       :features (unit-features J-unit))
			    (remove J-unit pattern)))))
;;    (format t "~%=> results=~A" results)
    (setq results
	  (loop for mr in results
	     append (loop for bs in (mr-bsl mr)
		       ;; we replace the variables in the current expr
		       ;; based on bindings and remove units which
		       ;; have nil as unit-name
		       for expr = (substitute-bindings  
				   bs (delete-if #'null (mr-expr mr) 
						 :key #'unit-name))
		       collect (make-merge-result
                                expr
				(list bs)
				(mr-added mr)))))
    #+dbg
    (format t "~%-------------- handle-J-units ---------------")
    results))

(define-event merging-finished (pattern list) (source list) (new-sources+bindings list))

(defun merge-structures (pattern source &optional (bindings +no-bindings+) unified &key cxn-inventory)
  ;; non-destructive because of copy-tree
  #+dbg
  (format t "~%++++++++++++++ merge-structures +++++++++++++++
  pattern  = ~A
  source   = ~A
  bindings = ~A
  unified  = ~A" pattern source bindings unified)
;;   (format t "~%(merge-structures-without-Js~%  '~A~%  '~A~%  '~A"
;; 	  pattern source bindings)
  (let ((result nil))
    (dolist (mr (if unified 
                  (list (make-merge-result source (list bindings) nil))
                  (merge-structures-without-Js pattern (copy-tree source) bindings :cxn-inventory cxn-inventory)))
      #+dbg
      (format t "~%mr=~A" mr)
      (dolist (bindings (mr-bsl mr))
	;; added copy-tree because some of the underlying functions (eg. make-child) 
	;; are destructive so if there is more than one bindings the handling of
	;; the j-units might interfere with each other (for example producing
	;; when ball is twice in the meaning)
	#+dbg
	(format t "~%bs=~A" bindings)
	(dolist (fmr (handle-J-units pattern (copy-tree (mr-expr mr)) bindings
				     ;; mr-added is a list of pointers so i only 
				     ;; copy the outer list structure
				     (copy-list (mr-added mr)) :cxn-inventory cxn-inventory))
	  #+dbg
	  (format t "~%fmr=~A" fmr)
	  (let ((prev (find-if #'(lambda (rm) (equal (mr-expr rm)
						     (mr-expr fmr)))
			       ;; not checking the mr-added because equal expects
			       ;; symbol within lists to be eq anyway
			       result)))
	    (if prev 
		(push bindings (mr-bsl prev))
		(push fmr result))))))
    #+dbg
    (format t "~%-------------- merge-structures ---------------")
    (notify merging-finished pattern source result)
    result))

(defun merge-intersection (int y bindings &key cxn-inventory)
  (declare (ignore y))
  (let ((i1 (substitute-bindings bindings (second int)))
	(i2 (substitute-bindings bindings (third int))))
;;    (format t "~%int=~A, y=~A, i1=~A, i2=~A" int y i1 i2)
    #+dbg
    (assert (and (null y)))
    (cond ((and (listp i1) (listp i2))
	   (let ((res nil)
		 (bsl (list bindings)))
	     (dolist (e1 i1)
	       (let ((new-bsl (loop for bs in bsl append
				   (unify (list '== e1) i2 (list bs) :cxn-inventory cxn-inventory))))
		 (when new-bsl
		   (setq bsl (delete-duplicates new-bsl
						:test #'(lambda (bs1 bs2)
							  (permutation-of? bs1 bs2 :test #'equal))))
		   (push e1 res))))
;; 	     (setf bsl (delete-if-not #'(lambda (bs)
;; 					  (valid-includes-uniquely-list? res bs))
;; 				      bsl))
	     (when bsl
	       (list (make-merge-result (reverse res) bsl)))))
	  ((equal i1 i2) (list (make-merge-result i1 (list bindings))))
	  (t (list (make-merge-result nil (list bindings)))))))

(defun clean-intersection (int bindings)
  (let ((mrs (merge-intersection int nil bindings)))
    (assert (and (= 1 (length mrs))
		 (= 1 (length (mr-bsl (first mrs))))))
    (values (mr-expr (first mrs) )
	    (first (mr-bsl (first mrs))))))

(defun clean-structure (structure &optional (bindings +no-bindings+))
  (remove-special-operators (remove-J-units structure) bindings))

(add-special-operator (make-instance 'special-operator
				     :representation 'int
				     :merge-fn #'merge-intersection
				     :clean-fn #'clean-intersection))



;;; 3. partial matching


(defun find-container (element tree &key (test #'eq))
  (cond ((funcall test element tree) (values nil t))
	((consp tree)
	 (multiple-value-bind (container found)
	     (find-container element (car tree) :test test)
	   (if found
	       (values (or container tree) t)
	       (progn
		 (multiple-value-setq (container found)
		   (find-container element (cdr tree) :test test))
		 (if found
		     (progn (assert container)
			    (values container t))
		     (values nil nil))))))
	(t (values nil nil))))
	
(defun mark-added (mr)
  (loop for added in (mr-added mr) do
       (multiple-value-bind (container found)
	   (find-container added (mr-expr mr))
	 (assert found)
	 (setf (car container)
	       (cons :added (car container)))))
  mr)
  
(defun partial-match-structures (pat src &optional (bindings +no-bindings+))
  (mapcar #'(lambda (mr)
	      (mr-expr (mark-added mr)))
	  (merge-structures-without-Js pat src bindings)))



(export '(equivalent-coupled-feature-structures equivalent-feature-structures))


;; TODO: The function below takes 60% of all processing time of FCG
;; for large grammars and sentences. ideas for optimization:
;;; 1: first check specific feature-values like meaning, form,
;;; sem-cat, syn-cat. I tried this but has no impact.
;;  2. You do not need to check the full cfs of both nodes but only
;;  those units that changed since their last communal point (their
;;  branching point). This can be done by looking at bindings for
;;  units. Not yet tried.
;;; 3. More tricky but maybe it is possible to ignore the cfs all
;;; together and just look at bindings and potentially whether the
;;; split in the tree was because of different constructions or one
;;; construction that had different hypotheses. The idea would be that
;;; two nodes that have accumulated in some way equivalent bindings
;;; (with the same constructions which is anyway first checked) result
;;; in equivalent cfs's.

(defun equivalent-coupled-feature-structures (cfs-1 cfs-2)
  (and cfs-1 cfs-2
       (length= (left-pole-structure cfs-1)
                (left-pole-structure cfs-2))
       (length= (right-pole-structure cfs-1)
                (right-pole-structure cfs-2))
       ;;            (equivalent-structure-for-feature-x 
       ;;             (left-pole-structure cfs-1) (left-pole-structure cfs-2) 'meaning)
       ;;            (equivalent-structure-for-feature-x 
       ;;             (right-pole-structure cfs-1) (right-pole-structure cfs-2) 'form)
       ;;            (equivalent-structure-for-feature-x 
       ;;             (left-pole-structure cfs-1) (left-pole-structure cfs-2) 'sem-cat)
       ;;            (equivalent-structure-for-feature-x 
       ;;             (right-pole-structure cfs-1) (right-pole-structure cfs-2) 'syn-cat)
       (equivalent-coupled-feature-structures-aux cfs-1 cfs-2)
       (equivalent-coupled-feature-structures-aux cfs-2 cfs-1)))

(defun transform-feature-value (v &optional (feature-directive '==p))
  (if (and (consp v) (not (merge-fn (first v))))
      (cons feature-directive v)
      v))

(defun transform-structure (s &optional (feature-directive '==p))
  (loop for unit in s collect
       (make-unit 
	:name (unit-name unit)
	:features
	(loop for feature in (unit-features unit) collect
	     (if (tag-p feature)
		 feature
		 (make-feature (feature-name feature)
			       (transform-feature-value (feature-value feature) feature-directive)))))))

(defun equivalent-feature-structures (fs-1 fs-2 &key cxn-inventory)
  (multiple-value-bind (instantiated-2-left instantiations)
      (instantiate-expression fs-2)
    (declare (ignore instantiations))
    (let* ((unit-name-renamings
            (when fs-1
              (unless (variable-p (unit-name (first fs-1)))
                (create-vars (mapcar #'unit-name fs-1)))))
           (rcfs (sublis unit-name-renamings fs-1)))
      (unify-structures
       (transform-structure rcfs)
       instantiated-2-left
       (list +no-bindings+)
       :cxn-inventory cxn-inventory))))

(defun equivalent-coupled-feature-structures-aux (cfs-1 cfs-2 &key cxn-inventory)
  (multiple-value-bind (instantiated-2-left instantiations)
      (instantiate-expression (left-pole-structure cfs-2))
    (let* ((instantiated-2-right (instantiate-expression 
                                 (right-pole-structure cfs-2) instantiations))
           (unit-name-renamings
            (when (left-pole-structure cfs-1)
              (unless (variable-p (unit-name (first (left-pole-structure cfs-1))))
                (create-vars
                 (union (mapcar #'unit-name (left-pole-structure cfs-1))
                        (mapcar #'unit-name (right-pole-structure cfs-1)))))))
           (rcfs (sublis unit-name-renamings (left-pole-structure cfs-1)))
           (bsl (progn  
                  (unify-structures
                   (transform-structure rcfs)
                   instantiated-2-left
                   (list +no-bindings+)
                   :cxn-inventory cxn-inventory))))
	(when bsl
	  (unify-structures
	   (transform-structure (sublis unit-name-renamings (right-pole-structure cfs-1)))
	   instantiated-2-right
	   bsl
           :cxn-inventory cxn-inventory)))))

(defun equivalent-structure-for-feature-x (structure-1 structure-2 feature &key cxn-inventory)
  "Compares two cfs's for equality but only for the given
feature (e.g. form, meaning, etc). This is made with both structure-1
and 2 coming from a cfs and not from constructions. For example tags
or syn-syn, sem-sem are not taken into account."
  (let ((feature-in-1 (loop for unit in structure-1
                             append (unit-feature-value unit feature nil)))
        (feature-in-2 (loop for unit in structure-2
                             append (unit-feature-value unit feature nil))))
    (when (and feature-in-1 feature-in-2)
      (let ((instantiated-2 (instantiate-expression feature-in-2)))
        (unify (transform-feature-value feature-in-1)
               instantiated-2 +no-bindings+ :cxn-inventory cxn-inventory)))))
