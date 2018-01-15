(in-package :fcg)

;;;;; Functions for accessing and modifying features, units, feature-structures
;;;;; and coupled-feature-structures.


;; ############################################################################
;; Features
;; ############################################################################

(export '(feature-name tag-feature-name feature-value tag-feature-value))

(defun make-feature (name value)
  (list name value))

(defun feature-name (feature) (when (listp feature) (car feature)))

;(defun feature-name (feature)
;  (declare (type list feature))
;  (if (tag-p feature)
;      (tag-feature name feature)
;      (car feature)))

(defun tag-feature-name (tag)
  "Returns the name of the feature that is tagged by this tag"
  ; Reading only the feature-name of the first tagged feature is
  ; correct behaviour since, while multiple things can be tagged
  ; within one tag, they all have to be values of the same feature
  (declare (type list tag))
  (first (third tag)))

(defun feature-value (feature) (when (listp feature) (second feature)))
(defsetf feature-value (feature) (new) `(setf (second ,feature) ,new))

(defun tag-feature-value (tag)
  (declare (type list tag))
  (rest tag))

;; special feature: subunits

(export '(subunits-feature? subunits sem-subunits syn-subunits))

(defun subunits-feature? (f)
  (find (feature-name f) '(subunits sem-subunits syn-subunits)))

;; utilities

(export '(feature-value-difference))

;; Unreliable function:
(defun feature-value-difference (v1 v2)
  (cond ((and (listp v1) (listp v2))
	 (set-difference v1 v2 :test #'unify-simple))
	((unify-simple v1 v2) nil)
	(t v1)))

;; Unreliable function:
(defun feature-difference (f1 f2)
  (if (equal (feature-name f1) (feature-name f2))
      (let ((new-val (feature-value-difference (feature-value f1)
					       (feature-value f2))))
	(if new-val (make-feature (feature-name f1) new-val)))
      f1))

;; ############################################################################
;; Units
;; ############################################################################

(export '(unit-name unit-body unit-features))

(defun make-unit (&key (name (make-id "UNIT")) features)
  `(,name ,@features))

(defun unit-name (unit &key (maybe-j-unit nil))
  (if (and maybe-j-unit (j-unit-p unit))
    (j-unit-name unit)
    (first unit)))
(defsetf unit-name (unit) (name)
  `(if (j-unit-p ,unit)
     (setf (second (first ,unit)) ,name)
     (setf (first ,unit) ,name)))

(defun unit-body (unit)
  (rest unit))

(defun unit-features (unit) (rest unit))
(defsetf unit-features (unit) (features) `(setf (rest ,unit) ,features))

;; unit feature/value:

(export '(unit-feature unit-tag-feature unit-feature-value))

(defun unit-feature (unit name &optional (ignore-tags t) (return-full-tag nil))
  ;; This function won't work when having multiple tags even when
  ;; ignore-tags = t (like ?required ?remove).

  ; the second parameter only makes sense when ignore-tags is t
;  (if return-full-tag (assert ignore-tags))
  (if ignore-tags
      (let ((found nil))
	(loop for feature in (unit-features unit) 
	   until found do
	     (cond ((eq (feature-name feature) name)
		    (setq found feature))
		   ((and (find (feature-name feature) '(tag tag-all))
			 (eq (feature-name (third feature)) name))
                    (if return-full-tag
                        (setq found feature)
                        (setq found (third feature))))))
	found)
      (find name (unit-features unit) :key #'feature-name)))

;; Unreliable function:
(defun unit-tag-feature (unit feature-name)
  "Returns a tag found in the unit which tags the given feature"
  ; Stops searching when encountering the first tag with that feature
  ; (since FCG doesn't support more than one tag per feature anyway)

  ; it doesn't work together nicely with existing functions because
  ; (unit-feature) cannot find more than one tag
  (find-if #'(lambda (feature)
               (and
                (tag-p feature)
                (eq feature-name (tag-feature-name feature))))
           (unit-features unit)))

(defun unit-feature-value (unit name &optional (ignore-tags nil))
  (feature-value (unit-feature unit name ignore-tags)))

(defun null-1 (x) (null x))
;; with this hack warnings are avoided

(defsetf unit-feature-value (unit name) (value)
  `(if (null-1 ,value)
       (setf (unit-features ,unit)
	     (delete ,name (the list (unit-features ,unit)) :key #'feature-name))
       (let ((prev (unit-feature ,unit ,name)))
	 (if prev
             (setf (feature-value prev) ,value)
	     (setf (unit-features ,unit)
		   (cons `(,,name ,,value) (unit-features ,unit)))))))

;; utilities:

(export '(unit-difference))

;; Unreliable function:
(defun unit-difference (u1 u2)
  (if (equal (unit-name u1)
	     (unit-name u2))
      (let ((new-unit (make-unit :name (unit-name u1))))
	(dolist (f1 (unit-features u1))
	  (setf (unit-feature-value new-unit (feature-name f1))
		(feature-value
		 (feature-difference f1 
				     (unit-feature u2 (feature-name f1))))))
	new-unit)
      u1))

;; subunits:

(export '(get-subunits-feature add-subunit subunits get-parent-unit))

(defun get-subunits-feature (unit)
  ;;(warn "Being designed for OLD OLD FCG (at least before 2015), it still checks subunits, sem-subunits and syn-subunits instead of checking the hierarchy-features of your grammar. Use with care and please adapt!")
  (let ((res (or (unit-feature unit 'subunits)
		 (unit-feature unit 'sem-subunits)
		 (unit-feature unit 'syn-subunits))))
    (when (and res (not (listp (feature-value res))))
      (error "FCG error.  Cannot parse unit~%~%   ~A~%~%REASON: subunits feature must be a list.~%" unit))
    res))

(defun subunits (unit structure)
  (let ((result '())
	(subunits-feature (get-subunits-feature unit)))
    (when (and subunits-feature (not (listp (feature-value subunits-feature))))
      (error "FCG error.  Cannot parse unit~%~%   ~A~%~%REASON: subunits feature must be a list.~%" unit))
    (dolist (unit-name (remove-special-operators (feature-value subunits-feature) +no-bindings+))
      (let ((unit (structure-unit structure unit-name)))
	(unless unit
	  (error "FCG error.  Cannot parse structure~%~% ~A~%~%REASON: non-existing subunit ~A.~%" 
		 structure
		 unit-name))
	(push unit result)))
    (nreverse result)))

(export '(get-parent-unit))

(defun get-parent-unit (unit structure)
  (find (unit-name unit)
        structure
        :test #'(lambda (name candidate-unit)
                  (find name
                        (feature-value
                         (get-subunits-feature candidate-unit))))))

;; meaning:

(export '(extract-meaning meaning))

(defun extract-meaning (unit &optional var (ignore-tags nil))
  "Get the meaning predicates in unit. If var is provided only return those that
   have var as their second element (i.e. as in (predicate var ...)).
   Note: the var options is meaning-format specific, and for instance not
         useful when using IRL meaning."
  (let ((predicates (unit-feature-value unit 'meaning ignore-tags)))
    (if var
	(remove-if-not #'(lambda (pred) (eq var (second pred))) predicates)
	predicates)))

;; sem-cat/syn-cat:

(export '(extract-syn-cat extract-sem-cat sem-cat syn-cat))

(defun extract-sem-cat (unit &optional (ignore-tags nil))
  (feature-value (unit-feature unit 'sem-cat ignore-tags)))

(defun extract-syn-cat (unit &optional (ignore-tags nil))
  (feature-value (unit-feature unit 'syn-cat ignore-tags)))

;; form:

(export '(get-string get-strings get-stem form string extract-string
	  extract-ordering-constraint extract-meets-constraint))

(defun get-strings (unit &optional (ignore-tags nil))
  (loop for el in (feature-value (unit-feature unit 'form ignore-tags))
       when (and (consp el)
		 (eql (first el) 'string))
       collect (third el)))
               ; FIXME this doesn't work for multiple tags per tag feature

(defun get-string (unit &optional (ignore-tags nil))
  (third
   (find 'string 
	 (the list (unit-feature-value unit 'form ignore-tags))
	 :key #'(lambda (e) (if (consp e) (first e) nil)))))

(defun get-stem (unit &optional (ignore-tags nil))
  (third (find 'stem 
	       (the list (unit-feature-value unit 'form ignore-tags))
	       :key #'(lambda (e) (if (consp e) (first e) nil)))))

(defun extract-string (unit &optional (ignore-tags nil))
  (loop for form-value in (unit-feature-value unit 'form ignore-tags)
     when (and (consp form-value)
	       (eq (first form-value) 'string))
     collect form-value))

(defun extract-stem (unit &optional (ignore-tags nil))
  (loop for form-value in (unit-feature-value unit 'form ignore-tags)
     when (and (consp form-value)
	       (eq (first form-value) 'stem))
     collect form-value))

(defun extract-or-string (unit &optional (ignore-tags nil))
  (loop for form-value in (unit-feature-value unit 'form ignore-tags)
     when (and (consp form-value)
	       (eq (first form-value) 'OR))
     append (loop for e in (rest form-value)
	       when (and (consp e) (eq (first e) 'string))
	       collect e)))

(defun extract-or-stem (unit &optional (ignore-tags nil))
  (loop for form-value in (unit-feature-value unit 'form ignore-tags)
     when (and (consp form-value)
	       (eq (first form-value) 'OR))
     append (loop for e in (rest form-value)
	       when (and (consp e) (eq (first e) 'stem))
	       collect e)))

(defun extract-ordering-constraint (unit &optional (ignore-tags nil))
  (let ((form-values (unit-feature-value unit 'form ignore-tags)))
    (loop for form-value in form-values
          unless (eq (first form-value) 'string)
          collect form-value)))

(defun extract-meets-constraint (unit &optional (ignore-tags nil))
  (let ((form-values (unit-feature-value unit 'form ignore-tags)))
    (loop for form-value in form-values
       when (and (consp form-value) (eq (first form-value) 'meets))
       collect form-value)))

(defun extract-precedes-constraint (unit &optional (ignore-tags nil))
  (let ((form-values (unit-feature-value unit 'form ignore-tags)))
    (loop for form-value in form-values
          when (eq (first form-value) 'precedes)
          collect form-value)))

;; context: (context (link ...))

(export '(extract-context get-link context link))

(defun extract-context (unit &optional (ignore-tags nil))
  (feature-value (unit-feature unit 'context ignore-tags)))

(defun get-link (unit &optional (ignore-tags nil))
  (or (cdr (assoc 'link (unit-feature-value unit 'context ignore-tags)))
      (first (unit-feature-value unit 'link))))

;; link: (link (var ...))

(export '(extract-link get-var))

(defun extract-link (unit &optional (ignore-tags nil))
  (feature-value (unit-feature unit 'link ignore-tags)))

(defun get-var (unit &optional (ignore-tags nil))
  (or (first (cdr (assoc 'var (list (unit-feature-value unit 'link ignore-tags)))))
      (first (unit-feature-value unit 'link))))

;; j-units:

(export '(J J-unit-p J-unit-name))

(defun J-unit-p (unit) 
  (and (consp unit)
       (consp (unit-name unit))
       (eq 'J (first (unit-name unit)))))

(defun J-unit-name (j-unit)
  (second (first j-unit)))

;; ############################################################################
;; III. Structures
;; ############################################################################

(export '(feature-structure structure-unit))

(deftype feature-structure ()
  "will be replaced by a proper class or list"
  `(and list (not nil)))

(defun structure-unit (struct name)
  (declare (list struct))
  (or (find name struct :key #'unit-name :test #'equal)
      (find name (get-J-units struct) :key #'(lambda (J-unit) (second (unit-name J-unit))) :test #'equal)))

;; meaning:

(export '(extract-meanings))

(defun extract-meanings (structure &optional var (ignore-tags nil))
  "Get the meaning predicates in structure. If var is provided only return those
   that have var as their second element (i.e. as in (predicate var ...)) "
  (loop for unit in structure append (extract-meaning unit var ignore-tags)))

;; sem-cat/syn-cat:

(export '(extract-sem-cats extract-syn-cats))

(defun extract-sem-cats (structure &optional (ignore-tags nil))
  (loop for unit in structure append (extract-sem-cat unit ignore-tags)))

(defun extract-syn-cats (structure &optional (ignore-tags nil))
  (loop for unit in structure append (extract-syn-cat unit ignore-tags)))

;; form

(export '(extract-forms get-all-strings extract-strings
                        extract-ordering-constraints))

(defun extract-forms (structure &optional (ignore-tags nil))
  (loop for unit in structure 
	  append (feature-value (unit-feature unit 'form ignore-tags))))

(defun get-all-strings (structure &optional (ignore-tags nil))
  (loop for unit in structure
       append (get-strings unit ignore-tags)))

(defun extract-strings (structure &optional (ignore-tags nil))
  (loop for unit in structure
        append (extract-string unit ignore-tags)))

(defun extract-stems (structure &optional (ignore-tags nil))
  (loop for unit in structure
        append (extract-stem unit ignore-tags)))

(defun extract-or-strings (structure &optional (ignore-tags nil))
  (loop for unit in structure
        append (extract-or-string unit ignore-tags)))

(defun extract-or-stems (structure &optional (ignore-tags nil))
  (loop for unit in structure
        append (extract-or-stem unit ignore-tags)))

(defun extract-ordering-constraints (structure &optional (ignore-tags nil))
  (loop for unit in structure
        append (extract-ordering-constraint unit ignore-tags)))

(defun extract-meets-constraints (structure &optional (ignore-tags nil))
  (loop for unit in structure
        append (extract-meets-constraint unit ignore-tags)))

(defun extract-precedes-constraints (structure &optional (ignore-tags nil))
  (loop for unit in structure
        append (extract-precedes-constraint unit ignore-tags)))

;; context: (context (link ...))

(export '(extract-contexts))

(defun extract-contexts (structure &optional (ignore-tags nil))
  (loop for unit in structure append (extract-context unit ignore-tags)))

;; link: (link (var ...))

(export '(extract-links))

(defun extract-links (structure &optional (ignore-tags nil))
  (loop for unit in structure append (extract-link unit ignore-tags)))

;; accessors:

(export '(get-level-0-unit get-root remove-root-unit
	  get-units-downto-level get-level-units check-hierarchy
          get-root))

(defun get-level-0-unit (structure)
  ;; although a very nice definition also not very performant
  (list (find-if-not #'(lambda (unit) 
			 (some #'(lambda (other)
				   (member (unit-name unit)
					   (feature-value
                                            (get-subunits-feature other))
					   :test #'equal))
			       structure))
		     structure)))

(defun get-root (structure)
  "In the root-mode, get the root unit."
  (assoc 'root structure))

(defun root-p (unit)
  (eq (unit-name unit) 'root))

(defun remove-root-unit (structure)
  "Remove the root unit."
  (remove-if #'root-p structure))

(defun fcg-get-sequence (structure)
  "Get the value of the sequence feature inside the form feature"
  (let ((root-unit
         (get-root (if (eql (type-of structure) 'coupled-feature-structure)
                                  (left-pole-structure structure)
                                  structure))))
         
  (cdr (assoc 'sequence (unit-feature-value root-unit 'form)))))

(defun fcg-get-boundaries (structure)
  "Get the value for the feature boundaries."
  (unit-feature-value (get-root (if (eql (type-of structure) 'coupled-feature-structure)
                                  (left-pole-structure structure)
                                  structure))
                      'boundaries))

(defun get-units-without-superunits (structure)
  (let ((non-superunits (loop for unit in structure
                              for subunits = (or (unit-feature-value unit 'syn-subunits)
                                                 (unit-feature-value unit 'sem-subunits)
                                                 (unit-feature-value unit 'subunits))
                              when subunits
                              append subunits)))
    (loop for unit in structure
          unless (find (unit-name unit) non-superunits)
          collect unit)))

(defun get-units-downto-level (level structure)
  (let ((current-level-units (get-level-0-unit structure))
        (current-result nil))        
    (dotimes (n level)
      (setf current-result (append current-level-units current-result))
      (setf current-level-units (mappend (lambda (unit)
                                           (subunits unit structure))
                                         current-level-units)))
    (append current-level-units current-result)))

(defun get-level-units (level structure)
  (cond ((<= level 0) (get-level-0-unit structure))
	(t (mappend #'(lambda (unit)
			(subunits unit structure))
		    (get-level-units (- level 1) structure)))))

;; utilities:

(export '(structure-difference))

(defun structure-difference (s1 s2)
  (let ((new-s1 nil))
    (dolist (u1 s1)
      (let ((new-u1 (unit-difference u1 (structure-unit s2 (unit-name u1)))))
	(if new-u1
	    (push new-u1 new-s1))))
    (reverse new-s1)))

;; j- and r-units

(export '(remove-J-units get-J-units get-J-unit))

(defun remove-J-units (pattern)
  (remove-if #'J-unit-p pattern))

(defun get-J-units (pattern)
  (remove-if-not #'J-unit-p pattern))

(defun get-J-unit (pattern)
  (let ((j-units (get-J-units pattern)))
    (assert (= (length j-units) 1))
    (first j-units)))

;; ############################################################################
;; Structure building
;; ############################################################################

;; originally under j- and r-units

(export '(remove-struct add-struct))

(defun remove-struct (to-remove from)
  ;; used in conflicting-cxns? function in rule-set-application.lisp
  (dolist (unit to-remove)
    (unless (J-unit-p unit)
      (let ((orig-unit (structure-unit from (unit-name unit))))
	(when orig-unit
	  (dolist (feature (unit-features unit))
	    (let ((orig-feature (unit-feature orig-unit (feature-name feature))))
	      (when orig-feature
		(if (consp (feature-value feature))
                  (dolist (part (feature-value feature))
                    ;; TODO: should be unify to be completely safe
                    (setf (feature-value orig-feature)
                          (remove part (feature-value orig-feature)
                                  :count 1
                                  :test #'equalp)))
                  (when (equal (feature-value orig-feature)
                               (feature-value feature))
                    (setf (rest orig-unit)
                          (remove (feature-name feature) (rest orig-unit)
                                  :key #'feature-name)))))))))))
  from)

(defun add-struct (to-add from &optional (add-duplicate-values t))
  ;; not used
  (dolist (unit to-add)
    (unless (J-unit-p unit)
      (let ((orig-unit (structure-unit from (unit-name unit))))
	(if orig-unit
          (dolist (feature (unit-features unit))
            (let ((orig-feature (unit-feature orig-unit (feature-name feature))))
              (if orig-feature
                (if (consp (feature-value feature))
                  (dolist (part (feature-value feature))
                    (setf (feature-value orig-feature)
                          (if add-duplicate-values
                            (push part (feature-value orig-feature))
                            (pushnew part (feature-value orig-feature)
                                     :test #'equalp))))
                  (setf (feature-value orig-feature)
                        (feature-value feature)))
                (setf (unit-features orig-unit)
                      (push feature (unit-features orig-unit))))))
          (setf from (push unit from))))))
  from)

;; structure->pattern (not used at this moment but might still be handy):

(export '(unit->pattern structure->pattern))

(defun predicate-or-constant->pattern (p-or-c &optional renamings introduce-vars)
  "Part of a set of functions to transform a unit into a unit pattern that could
   for example be used as part of the pole of a construction. Probably you only
   need unit->pattern."
  (if introduce-vars
    (values (cond ((atom p-or-c) 
                   (let ((prev (assoc p-or-c renamings)))
                     (unless prev (push (cons p-or-c (make-var p-or-c)) renamings)
                       (setq prev (first renamings)))
                     (cdr prev)))
                  (t (cons (first p-or-c)
                           (let ((rest nil) pat)
                             (dolist (el (rest p-or-c) (reverse rest))
                               (multiple-value-setq (pat renamings)
				   (predicate-or-constant->pattern el renamings
								   introduce-vars))
                               (push pat rest))))))
            renamings)
    (values p-or-c renamings)))

(defun value->pattern (val &optional renamings introduce-vars (includes-operator '==1))
  "Part of a set of functions to transform a unit into a unit pattern that could
   for example be used as part of the pole of a construction. Probably you only
   need unit->pattern."
  (values (cond ((atom val) 
		 (let ((prev (assoc val renamings)))
		   (unless prev (push (cons val (make-var val)) renamings)
                     (setq prev (first renamings)))
		   (cdr prev)))
		(t (cons includes-operator
			 (let ((rest nil) pat)
			   (dolist (el val (reverse rest))
			     (multiple-value-setq (pat renamings)
                                 (predicate-or-constant->pattern el renamings
                                                                 introduce-vars))
			     (push pat rest))))))
	  renamings))

(defun feature->pattern (feature &optional renamings)
  "Part of a set of functions to transform a unit into a unit pattern that could
   for example be used as part of the pole of a construction. Probably you only
   need unit->pattern."
  (multiple-value-bind (new-val new-renamings)
      (value->pattern (feature-value feature) renamings
		      (if (eq 'meaning (feature-name feature)) t nil)
		      (if (eq 'meaning (feature-name feature)) '== '==1))
    (values (list (feature-name feature)
		  new-val)
	    new-renamings)))

(defun unit->pattern (unit &optional renamings)
  "Transforms a unit into a unit pattern that could for example be used as part
   of the pole of a construction."
  (values (let ((prev (assoc (unit-name unit) renamings)))
	    (unless prev
	      (push (cons (unit-name unit) 
			  (make-var (unit-name unit)))
		    renamings)
	      (setq prev (first renamings)))
	    (cons (cdr prev)
		  (let ((rest nil) pat)
		    (dolist (el (unit-features unit) (reverse rest))
		      (multiple-value-setq (pat renamings)
                          (feature->pattern el renamings))
		      (push pat rest)))))
	  renamings))

(defun structure->pattern (structure &optional renamings)
  "Transforms a structure into a into a pattern that could for example be used
   as the pole of a construction."
  (values (reverse (let (result new)
		     (dolist (unit structure result)
		       (multiple-value-setq (new renamings)
                           (unit->pattern unit renamings))
		       (push new result))))
	  renamings))

;; ############################################################################
;;; V. Extract parts of structure

;; based on presence of variables:

(export '(extract-units))

(defun extract-values (variable feature)
  "Part of a set of functions to extract the relevant parts of a structure with
   respect to a variable. Probably you only need extract-units"
  (cond ((atom (feature-value feature))
	 (when (eq variable (feature-value feature))
	   (feature-value feature)))
	(t (remove-duplicates 
	    (loop for el in (remove-if-not #'(lambda (val)
					       (find-anywhere variable val))
					   (feature-value feature))
		  append (cons el
			       (when (and (consp el)
					  (variable-p (second el))
					  (not (eq variable (second el))))
				 (extract-values (second el) feature))))))))

(defun extract-features (variable unit &key names all-values)
  "Part of a set of functions to extract the relevant parts of a structure with
   respect to a variable. Probably you only need extract-units"
  (let ((res nil))
    (when (find-anywhere variable unit)
      (dolist (feature (unit-features unit) res)
	(when (or (not names)
		  (find (feature-name feature) names))
	  (let ((val (if all-values (feature-value feature)
                       (extract-values variable feature))))
	    (when val (push (list (feature-name feature)
				  val)
			    res))))))))

(defun extract-units (variables structure 
                                &key feature-names (all-values nil) (include-parents t))
  ;; used in parsing search experiment
  (let ((res nil))
    (dolist (unit structure res)
      (let ((features 
	     (mappend #'(lambda (var)
			  (extract-features 
			   var unit 
			   :names feature-names :all-values all-values))
		      variables)))
	(when features
	  (push (make-unit :name (unit-name unit))
		res)
	  (dolist (f features)
	    (if (consp (feature-value f))
              (setf (unit-feature-value (first res) (feature-name f))
                    (union (unit-feature-value (first res) (feature-name f))
                           (feature-value f)
                           :test #'equal))
              (setf (unit-feature-value (first res) (feature-name f))
                    (feature-value f)))))))
    (when include-parents ;;only works for one level I think
      (dolist (unit res)
	;;(format t "~%checking unit ~a for parents~%in structure ~a " unit structure)
	(let* ((parent-unit (get-parent-unit unit structure))
	       (res-parent (structure-unit res (unit-name parent-unit)))
	       (subunits-feature-name (feature-name (get-subunits-feature parent-unit))))
	  (cond (res-parent
		 (pushnew (unit-name unit)
			  (unit-feature-value res-parent subunits-feature-name)))
		(parent-unit 
		 (push `(,(unit-name parent-unit)
                         (,subunits-feature-name (,(unit-name unit))))
		       res))))))
    res))

;; ############################################################################
;; Poles and coupled feature-structures
;; ############################################################################

(export '(pole pole-structure pole-domain pole-vars unit-name-vars))

(defclass pole ()
  ((structure :accessor pole-structure
              :initform '((root))
              :initarg :structure
              :initarg :pole-structure
              :type feature-structure)
   (cfs :accessor cfs
        :initform nil
        :initarg :cfs
        :documentation "A reference back to the cfs of which the pole is part")
   (domain :accessor pole-domain
           :initform nil
           :initarg :domain
           :type symbol)
   (vars :accessor pole-vars
         :initform nil
         :type list)))

(defmethod instantiate-variables ((p pole) &optional renamings)
  "Instantiate all variables that occur in p to new unique constants
   and return the result. The renamings used (an alist) is returned as
   second value."
  ;; accessor-like functions
  (multiple-value-bind (new-structure new-renamings)
      (instantiate-variables (pole-structure p) renamings)
    (values (make-instance 'pole 
			   :structure new-structure
			   :domain (pole-domain p))
	    new-renamings)))

(defun tag-and-j-unit-vars-in (x)
  (cond ((tag-p x) (append (list (second x))
                           (tag-and-j-unit-vars-in (third x))))
        ((J-unit-p x) (append (list (second (unit-name x)))
                              (tag-and-j-unit-vars-in (unit-features x))))
        ((consp x) (append (tag-and-j-unit-vars-in (first x))
                           (tag-and-j-unit-vars-in (rest x))))
        (t nil)))

(defun unit-name-vars (structure)
  (loop 
   for unit in structure
   when (variable-p (unit-name unit))
   collect (unit-name unit)))

(defun update-pole-vars (pole)
  (with-slots (vars structure) pole
    (setf vars (remove-duplicates
                (set-difference
                 (set-difference (variables-in structure)
                                 (tag-and-j-unit-vars-in structure))
                 (unit-name-vars structure))))))

;; utility functions

(defmethod copy-object ((pole pole))
  (let ((copy (make-instance 'pole)))
    (copy-object-content pole copy)
    copy))

(defmethod copy-object-content ((source pole)
                                (destination pole))
  (setf (pole-structure destination) (copy-object (pole-structure source)))
  (setf (pole-domain destination) (pole-domain source)))

(defmethod print-object ((pole pole) stream)
  (if *print-pretty*
    (pprint-logical-block (stream nil)
      (format stream "~(~:w~)" (pole-structure pole)))
    (call-next-method)))

;; ############################################################################
;; Coupled-feature-structure
;; ############################################################################

(export '(coupled-feature-structure left-pole right-pole combined-pole sem syn
          make-cfs <--> shorten-variables))

(defclass coupled-feature-structure (blackboard)
  ((left-pole :accessor left-pole
	      :initarg :left-pole
              :initform (make-instance 'pole
                                       :structure '((root)) :domain 'sem)
              :type (or pole cons))
   (right-pole :accessor right-pole
	       :initarg :right-pole
               :initform (make-instance 'pole
                                        :structure '((root)) :domain 'syn)
               :type (or pole cons))
   (combined-pole ;; no accessor (see method combined-pole below)
                  :initarg :combined-pole
                  :initform nil
                  :documentation "This pole is a combination of left
                  and right pole.")))

(defmethod initialize-instance :after ((cfs coupled-feature-structure)
                                       &key
                                       (left-pole-domain 'sem)
                                       (right-pole-domain 'syn)
                                       (compute-combined-pole nil)
                                       &allow-other-keys)
  (with-slots (left-pole right-pole) cfs
    (when (consp left-pole)
      (setf left-pole
            (make-instance 'pole
                           :structure left-pole
                           :domain left-pole-domain
                           :cfs cfs)))
    (when (consp right-pole)
      (setf right-pole
            (make-instance 'pole
                           :structure right-pole
                           :domain right-pole-domain
                           :cfs cfs)))
    (when compute-combined-pole
      (setf (slot-value cfs 'combined-pole)
            (combine-cfs cfs)))))

(defmethod combined-pole ((cfs coupled-feature-structure))
  (or (slot-value cfs 'combined-pole)
      (setf (slot-value cfs 'combined-pole) (combine-cfs cfs))))

(defmethod instantiate-variables ((cfs coupled-feature-structure) &optional renamings)
  "Instantiate all variables that occur in the cfs to new unique
   constants and return the result. The renamings used (an alist) is
   returned as second value."
  (multiple-value-bind (new-left-pole tmp-renamings)
      (instantiate-variables (left-pole cfs) renamings)
    (multiple-value-bind (new-right-pole new-renamings)
	(instantiate-variables (right-pole cfs) tmp-renamings)
      (values (make-instance 'coupled-feature-structure
			     :left-pole new-left-pole
			     :right-pole new-right-pole)
	      new-renamings))))

(defmacro make-cfs (left-pole <--> right-pole 
                              &key (left-pole-domain 'sem) (right-pole-domain 'syn))
  (declare (ignore <-->))
  `(make-instance 'coupled-feature-structure
		  :left-pole (make-instance 'pole
					    :structure ',left-pole 
					    :domain ',left-pole-domain)
		  :right-pole (make-instance 'pole
					     :structure ',right-pole 
					     :domain ',right-pole-domain)))
;; utility functions
(defmethod copy-object ((cfs coupled-feature-structure))
  (let ((copy (make-instance 'coupled-feature-structure)))
    (copy-object-content cfs copy)
    copy))
  
(defmethod copy-object-content ((source coupled-feature-structure) 
				(destination coupled-feature-structure))
  (setf (left-pole destination) (copy-object (left-pole source)))
  (setf (right-pole destination) (copy-object (right-pole source))))

(defmethod print-object ((cfs coupled-feature-structure) stream)
  (if *print-pretty*
    (pprint-logical-block (stream nil)
      (format stream
              "<coupled-feature-structure:~:@_ ~:w~:@_ <-->~:@_ ~:w>"
              (left-pole cfs) (right-pole cfs)))
    (call-next-method)))

(export '(remove-feature-from-unit remove-feature-from-fs))

(defun remove-feature-from-unit (feature unit &key (test #'eql) (key #'identity))
  "Non destructively removes the feature from the unit. The key is
called on every feature in the unit. Features in tags will be completely ignored. As
second value it returns the removed features."
  (loop for feat in (unit-features unit)
        for feat-key = (funcall key feat)
        if (funcall test feature feat-key)
        collect feat into removed-features
        else collect feat into new-features
        finally (return (values (cons (unit-name unit) new-features)
                                removed-features))))

(defun rename-feature-in-unit (old-name new-name unit &key (test #'eql))
  "Features in tags will be completely ignored."
  (cons (unit-name unit)
        (loop for feat in (unit-features unit)
              if (funcall test old-name (feature-name feat))
              collect `(,new-name ,(feature-value feat))
              else collect feat)))

(defun rename-feature-in-fs (old-name new-name fs &key (test #'eql))
  "Features in tags will be completely ignored."
  (loop for unit in fs
        collect (rename-feature-in-unit old-name new-name unit :test test)))

(defun remove-feature-from-fs (feature fs &key (test #'eql) (key #'identity))
  "Non destructively removes the feature from all units in the feature
structure. The key is called on every feature in every
feature-unit. Features in tags will be completely ignored. As second
value it returns a feature structure with only the removed features."
  (loop for unit in fs
        for (new-unit removed-features) = (multiple-value-list 
                                           (remove-feature-from-unit feature unit :test test :key key))
        collect new-unit into new-fs
        collect (cons (unit-name unit) removed-features) into removed-fs
        finally (return (values new-fs removed-fs))))

(defun combine-feature (left-feat right-feat)
  "I assume that these features are actually only footprints or
subunits, which have flat lists as values."
  (assert (equal (feature-name left-feat) (feature-name right-feat)))
  `(,(feature-name left-feat)
    ,(union (listify (feature-value left-feat)) (listify (feature-value right-feat)))))

(defun combine-unit (left-unit right-unit)
  (assert (equal (unit-name left-unit) (unit-name right-unit)))
  (loop for right-feat in (unit-features right-unit)
        for feat-in-left = (and 
                            (consp right-feat) ;; pure symbols always get copied
                            (not (equal (feature-name right-feat) 'TAG)) ;; tags always get copied
                            (find (feature-name right-feat) left-unit :key #'feature-name :test #'equal))
        if feat-in-left
        collect (combine-feature right-feat feat-in-left) into result-unit
        else collect right-feat into result-unit
        finally (return
                 (cons (unit-name left-unit)
                       (append result-unit
                               (loop for left-feat in (unit-features left-unit)
                                     when (or (symbolp left-feat)
                                              (equal (feature-name left-feat) 'TAG)
                                              (not (find (feature-name left-feat) result-unit 
                                                         :key #'feature-name :test #'equal)))
                                     collect left-feat))))))
       
(defun combine-cfs (cfs)
  (loop 
   with left-fs = (rename-feature-in-fs 'sem-subunits 'subunits (left-pole-structure cfs))
   with right-fs = (rename-feature-in-fs 'syn-subunits 'subunits (right-pole-structure cfs))
   for unit in right-fs
   for unit-in-left = (find (unit-name unit) left-fs :key #'unit-name :test #'equal)
   if unit-in-left 
   collect (combine-unit unit-in-left unit) into result-fs
   else collect unit into result-fs
   finally (return 
            (make-instance 'pole
                           :cfs cfs
                           :structure 
                           (append result-fs
                                   (loop for unit in left-fs
                                         unless (find (unit-name unit) result-fs 
                                                      :key #'unit-name :test #'equal)
                                         collect unit))))))

(export '(left-pole-structure right-pole-structure left-pole-domain
                              right-pole-domain match-pole merge-pole production parsing cfs->fs))

(defgeneric left-pole-structure (object))
(defgeneric right-pole-structure (object))
(defgeneric left-pole-domain (object))
(defgeneric right-pole-domain (object))

(defsetf left-pole-structure (object) (new-struct)
  `(setf (pole-structure (left-pole ,object)) ,new-struct))

(defsetf right-pole-structure (object) (new-struct)
  `(setf (pole-structure (right-pole ,object)) ,new-struct))

(defmethod left-pole-structure ((cfs coupled-feature-structure))
  (pole-structure (left-pole cfs)))

(defmethod right-pole-structure ((cfs coupled-feature-structure))
  (pole-structure (right-pole cfs)))

(defmethod left-pole-domain ((cfs coupled-feature-structure))
  (pole-domain (left-pole cfs)))

(defmethod right-pole-domain ((cfs coupled-feature-structure))
  (pole-domain (right-pole cfs)))

(defgeneric match-pole (cfs label-or-direction)
  (:documentation "Returns either the left- or right-pole given a
  label (e.g. 'parsing) or direction symbol (e.g. '->)."))

(defmethod match-pole ((cfs coupled-feature-structure) (label (eql 'production)))
  (left-pole cfs))
(defmethod match-pole ((cfs coupled-feature-structure) (direction (eql '->)))
  (left-pole cfs))
(defmethod match-pole ((cfs coupled-feature-structure) (label (eql 'parsing)))
  (right-pole cfs))
(defmethod match-pole ((cfs coupled-feature-structure) (direction (eql '<-)))
  (right-pole cfs))

(defgeneric merge-pole (cfs label-or-direction)
  (:documentation "Returns either the left- or right-pole given a
  label (e.g. 'parsing) or direction symbol (e.g. '->)."))

(defmethod merge-pole ((cfs coupled-feature-structure) (label (eql 'production)))
  (right-pole cfs))

(defmethod merge-pole ((cfs coupled-feature-structure) (direction (eql '->)))
  (right-pole cfs))

(defmethod merge-pole ((cfs coupled-feature-structure) (label (eql 'parsing)))
  (left-pole cfs))

(defmethod merge-pole ((cfs coupled-feature-structure) (direction (eql '<-)))
  (left-pole cfs))

(defgeneric shorten-variables (structure)
  (:documentation "Shortens all variables so that the numbers tailing
  them are as short as possible."))

(defmethod shorten-variables ((structure symbol))
  (if (variable-p structure)
    (intern (read-until (symbol-name structure) "-" :from-end t))
    structure))

(defmethod shorten-variables ((structure list))
  (loop 
   for el in structure
   if (consp el)
   collect (shorten-variables el)
   else if (variable-p el)
   collect (intern (read-until (symbol-name el) "-" :from-end t))
   else 
   collect el))

(defmethod shorten-variables ((structure pole))
  (setf (pole-structure structure) 
	(shorten-variables (pole-structure structure)))
  structure)

(defmethod shorten-variables ((structure coupled-feature-structure))
  (setf (left-pole-structure structure) 
	(shorten-variables (left-pole-structure structure)))
  (setf (right-pole-structure structure) 
	(shorten-variables (right-pole-structure structure)))
  structure)

(export '(get-unmatched-strings))

(defmethod get-unmatched-strings ((structure coupled-feature-structure))
  "Returns all strings from the root unit of the right-pole-structure"
  (get-strings (get-root (right-pole-structure structure)) t))

;; ############################################################################
;; Utilities added for root grammars
;; ############################################################################

(export '(sem-pole syn-pole))

(proclaim '(inline syn-pole sem-pole root-unit))

(defun syn-pole (construction)
  "Return the right pole of a construction."
  (right-pole-structure construction))

(defun sem-pole (construction)
  "Return the semantic pole of a construction."
  (left-pole-structure construction))