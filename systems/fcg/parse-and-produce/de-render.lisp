
(in-package :fcg)

;; ############################################################################
;; de-render
;; ############################################################################

(export '(footprints meaning sem-cat form syn-cat boundaries))

(defmethod de-render ((utterance t) (mode t) &key &allow-other-keys)
  "Default de-render mode: call de-render-with-scope."
  (de-render utterance :de-render-with-scope))

(defmethod de-render ((utterance string) (mode (eql :de-render-string-meets-precedes)) &key &allow-other-keys)
  "splits utterance by space and calls de-render-string-meets-precedes with this list."
  (de-render (split-sequence:split-sequence #\Space utterance :remove-empty-subseqs t)
             :de-render-string-meets-precedes))

(defmethod de-render ((utterance list) (mode (eql :de-render-string-meets-precedes)) &key &allow-other-keys)
  "De-renders a list of strings into string, meets and precedes constraints."
  (let ((strings nil)
        (sequence nil)
	(constraints nil))
    (dolist (string utterance)
      (let ((unit-name (make-const string nil)))
        (push unit-name sequence)
	(dolist (prev strings)
	  (push `(precedes ,(second prev) ,unit-name) constraints))
	(push `(string ,unit-name ,string) strings)))
    (do ((left strings (rest left)))
	((null (cdr left)))
      (push `(meets ,(second (second left)) ,(second (first left)))
	    constraints))
    (make-instance 'coupled-feature-structure 
		   :left-pole `((root (meaning ())
                                      (sem-cat ())
                                      (form ,(cons (cons 'sequence (reverse sequence))
                                                   (append strings constraints)))
                                      (syn-cat ())))
		   :right-pole '((root)))))

(defmethod de-render ((utterance string) (mode (eql :de-render-string-meets)) &key &allow-other-keys)
  "splits utterance by space and calls de-render-string-meets with this list."
  (de-render (split-sequence:split-sequence #\Space utterance :remove-empty-subseqs t)
             :de-render-string-meets))

(defmethod de-render ((utterance list) (mode (eql :de-render-string-meets)) &key &allow-other-keys)
  "De-renders a list of strings into string and meets."
  (let ((strings nil)
	(constraints nil))
    (dolist (string utterance)
      (let ((new (make-const string nil)))
	(push `(string ,new ,string) strings)))
    (do ((left strings (rest left)))
	((null (cdr left)))
      (push `(meets ,(second (second left)) ,(second (first left)))
	    constraints))
    (make-instance 'coupled-feature-structure 
		   :left-pole `((root (meaning ())
                                      (sem-cat ())
                                      (form ,(append strings constraints))
                                      (syn-cat ())))
		   :right-pole '((root)))))

;;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;;;; De-render-with scope
;;;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defmethod de-render ((utterance string) (mode (eql :de-render-with-scope)) &key cxn-inventory &allow-other-keys)
  "splits utterance by space and calls de-render-with-scope with this list."
  (de-render (split-sequence:split-sequence #\Space utterance :remove-empty-subseqs t) :de-render-with-scope
             :cxn-inventory cxn-inventory))

(defmethod de-render ((utterance list) (mode (eql :de-render-with-scope)) &key cxn-inventory &allow-other-keys)
  "De-renders with scope"
  (let ((sequence nil) ;; Will become an ordered list of unit-names associated with the strings of the sentence
        (word-boundaries nil) ;; E.g. ((unit-1 0 1) (unit-2 1 2))
        (counter 0)
        (form-predicates (get-configuration cxn-inventory :form-predicates)) ;; Fetches the predicates that will be used (by default: MEETS and PRECEDES and FIRST)
        (form-constraints nil))
    
    ;; 1. Collect a STRING-form-constraint and a boundary for each string, and add them to the right feature-value.
    (dolist (string utterance)
      (let ((unit-name (make-const string nil)))
        (push unit-name sequence)
        (push ;; Here we make the boundary, e.g. (unit-1 0 1)
              `(,unit-name ,counter ,(incf counter)) word-boundaries)
        (push ;; Here we make the string-form-constraint
              `(string ,unit-name ,string) form-constraints)))

 ;; 1b. Collect NOUN-CHUNK-form-constraints
    #+odycceus
    (dolist (chunk-as-list (odycceus::get-noun-chunks utterance))
      (let* ((unit-name (make-const "chunk" nil))
             (chunk (utils::list-of-strings->string chunk-as-list))
             (init-chunk-index (position (odycceus::first-word chunk) chunk-as-list :test #'string=)))
        (push `(,unit-name ,init-chunk-index ,(+ init-chunk-index
                                                 (length chunk-as-list))) word-boundaries)
        (push `(noun-chunk ,unit-name ,chunk) form-constraints)))

    
    ;; 2. Now we add all the relevant word ordering constraints to the form-constraints.
    (setf word-boundaries (reverse word-boundaries))
    (setf form-constraints
          (infer-all-constraints-from-boundaries word-boundaries form-predicates form-constraints))
    
    ;; 3. Finally build a transient structure, set its data, and return it.
    (let ((transient-structure
           (make-instance 'coupled-feature-structure
                          :left-pole `((root (meaning ())
                                             (sem-cat ())
                                             (boundaries ,word-boundaries)
                                             (form ,(cons (cons 'sequence (reverse sequence))
                                                          form-constraints))
                                             (syn-cat ())))
                          :right-pole '((root)))))
      (set-data transient-structure :sequence (reverse sequence)) ;; Is this still necessary?
      transient-structure)))

(export '(get-updating-references
          handle-form-predicate-in-de-render infer-before-constraints
          infer-all-constraints-from-boundaries))

;; By default, it uses the form predicates MEETS, PRECEDES, FIELDS and FIRST
;; -------------------------------------------------------------------------
(defun get-updating-references (&optional node-or-cxn-inventory
                                          (default-form-predicates '(meets precedes fields first)))
  "returns form-predicates from node or cxn-inventory"
  (let ((type (type-of node-or-cxn-inventory)))
    (or (case type
          (null default-form-predicates)
          (cip-node (get-configuration (construction-inventory node-or-cxn-inventory) :form-predicates))
          (coupled-feature-structure (ignore-errors ;; Data slot may not exist
                                       (get-data node-or-cxn-inventory :form-predicates)))
          (t (get-configuration node-or-cxn-inventory :form-predicates)))
        default-form-predicates)))

;; -------------------------------------------------------------------
;; METHOD: handle-form-predicate-in-de-render
;;         > Specializes on form predicates (e.g. MEETS)
;;         > Infers constraints based on boundaries
;; -------------------------------------------------------------------

;; Helper functions
(defun infer-before-constraints (list-of-boundaries predicate test-fn)
  "Infer MEETS or PRECEDES constraints from boundaries."
  ;; First ensure that the boundaries are sorted.
  (let ((sorted-boundaries (sort (copy-list list-of-boundaries) #'< :key #'second)))
    ;; A local function that recursively gets the constraints:
    (labels ((get-all-constraints (lst result)
               (if (null lst)
                 (reverse result)
                 (get-all-constraints (rest lst)
                                      (let* ((first-unit (first lst))
                                             (unit-name (first first-unit))
                                             (outer-bd (third first-unit)))
                                        (dolist (unit-and-boundaries (rest lst) result)
                                          (when (funcall test-fn outer-bd (second unit-and-boundaries))
                                            (push `(,predicate ,unit-name ,(first unit-and-boundaries) ,(make-var 'unit))
                                                  result))))))))
      (get-all-constraints sorted-boundaries nil))))

;; Generic function and its methods
(defgeneric handle-form-predicate-in-de-render (list-of-boundaries predicate))

(defmethod handle-form-predicate-in-de-render ((list-of-boundaries list) (predicate t))
  ;; To avoid potential symbol-name problems.
  (assert (symbolp predicate))
  (cond
   ((string= predicate 'meets)
    (handle-form-predicate-in-de-render list-of-boundaries 'meets))
   ((string= predicate 'precedes)
    (handle-form-predicate-in-de-render list-of-boundaries 'precedes))
   ((string= predicate 'first)
    (handle-form-predicate-in-de-render list-of-boundaries 'first))
   ((string= predicate 'last)
    (handle-form-predicate-in-de-render list-of-boundaries 'last))
   (t
    (progn
      (warn "No applicable handle-form-predicate-in-de-render method for the arguments ~a and ~a" list-of-boundaries predicate)
      nil))))

(defmethod handle-form-predicate-in-de-render ((list-of-boundaries list) (predicate (eql 'meets)))
  (infer-before-constraints list-of-boundaries predicate #'=))

(defmethod handle-form-predicate-in-de-render ((list-of-boundaries list) (predicate (eql 'precedes)))
  (infer-before-constraints list-of-boundaries predicate #'<=))

(defmethod handle-form-predicate-in-de-render ((list-of-boundaries list) (predicate (eql 'fields)))
  ;; The FIELDS feature does not provide enough information to make useful
  ;; inferences in comprehension. So we return nothing instead.
  (declare (ignore list-of-boundaries predicate))
  nil)

(defmethod handle-form-predicate-in-de-render ((list-of-boundaries list) (predicate (eql 'adjacent)))
  ;; Provides two predicates for every pair of adjacent units.
  ;; E.g. "the mouse" -> ((adjacent the-unit mouse-unit scope) (adjacent mouse-unit the-unit scope))
  (let ((adjacent-before-constraints (infer-before-constraints list-of-boundaries predicate #'=)))
    (loop for form-constraint in adjacent-before-constraints
          append `(,form-constraint
                   ,(list predicate (third form-constraint) (second form-constraint) (make-var 'unit))))))

(defmethod handle-form-predicate-in-de-render ((list-of-boundaries list) (predicate (eql 'first)))
  "Infer FIRST constraints based on SUBUNITS."
  (let ((units (fcg-get-transient-unit-structure (get-self)))
        (constraints nil))
    (dolist (unit units)
      (let ((subunits (unit-feature-value unit 'subunits)))
        (when subunits
          (let ((first-unit (caar (sort (remove-if #'(lambda(x)
                                                       (not (member (first x) subunits :test #'string=)))
                                                   list-of-boundaries)
                                        #'< :key #'second))))
            (when first-unit
              (push `(,predicate ,first-unit ,(unit-name unit)) constraints))))))
    constraints))

(defmethod handle-form-predicate-in-de-render ((list-of-boundaries list) (predicate (eql 'last)))
  "Get the rightmost subunit."
  (let ((units (fcg-get-transient-unit-structure (get-self)))
        (constraints nil))
    (dolist (unit units)
      (let ((subunits (unit-feature-value unit 'subunits)))
        (when subunits
          (let ((last-unit (caar (sort (remove-if #'(lambda(x)
                                                       (not (member (first x) subunits :test #'string=)))
                                                   list-of-boundaries)
                                        #'> :key #'third))))
            (when last-unit
              (push `(,predicate ,last-unit ,(unit-name unit)) constraints))))))
    constraints))

(defun infer-all-constraints-from-boundaries (boundaries form-predicates &optional result)
  "Given a list of boundaries, infer its form-predicates."
  (dolist (predicate form-predicates)
    (setf result (append result (handle-form-predicate-in-de-render boundaries predicate))))
  result)

;; ################# ;;
;; Helper functions  ;;
;; ################# ;;

(export '(form-constraints-with-meets))

(defun form-constraints-with-meets (utterance &key (variables nil))
  "Takes a simple list of strings and returns its corresponding string/meets-representation"
  ; the second argument determines whether the string-references will
  ; be constants or variables. variables will in any case be
  ; uninstantiated variables! (i.e. without a "-#id" suffix!) because
  ; the result of this function will likely be passed to (make-cxn)
  ; where you want shared variables that are only then instantiated
  (loop
   for string in utterance
   for variable = (if variables
                    (gensym (concatenate 'string "?" (string-upcase (string-replace string " " "-")) "-"))
                    (make-const (string-replace string " " "-") nil))
   when (length> vars 0)

   collect `(meets ,(car (last vars)) ,variable)
   if (typep string 'string)
   collect `(string ,variable ,string)
   and
   collect variable into vars ;; no meets for gestures
   else
   collect `(gesture ,variable ,string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated Methods for backwards compatibility ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod de-render ((utterance t) (mode (eql :de-render-in-one-pole-mode)) &key &allow-other-keys)
  "Deprecated method for backwards compatibility, use de-render-string-meets-precedes now"
  (warn "Please use the :de-render-string-meets-precedes method now. :de-render-in-one-pole-mode is no longer supported.")
  (de-render utterance :de-render-string-meets-precedes))

(defmethod de-render ((utterance t) (mode (eql :de-render-in-one-pole-mode-meets-only))  &key &allow-other-keys)
  "splits utterance by space and calls de render with list instead of string"
  (de-render utterance :de-render-string-meets))
