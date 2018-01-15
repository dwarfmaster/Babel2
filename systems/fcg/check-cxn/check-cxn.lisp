;;; This file contains a range of functions that can help to detect sources
;;; for problems and errors in constructions by checking the structure and
;;; syntax of constructions after they have been created in fcg.
;;; Output monitors are available in the package for buffer-tracing and for
;;; the web-interface.
;;; To enable checking together with the tracing of warnings, enable the
;;; check-cxns monitor: e.g. (activate-monitor check-cxns)
;;; In order to specify the range of applied tests, you can adapt
;;; (set-configuration *constructions* :check-cxn-tests *check-cxn-tests*)
;;; detailing a list of specific tests instead of *check-cxn-tests*

(in-package :fcg)

(export '(check-cxn duplicate-test tag-test feature-test j-unit-test *check-cxn-tests*))

(defparameter *check-cxn-tests* '(duplicate-test feature-test j-unit-test tag-test)
  "The available check-cxn tests.")

(defun count-item-in-struct (item structure &key (test 'eq))
  (length (find-all-anywhere item structure :test test)))

;; The j-unit test
;; ------------------------------------------------------------------------

(defgeneric j-unit-test (structure report))

(defmethod j-unit-test ((structure t) (report t))
  nil)

;; Given a pole from an fcg construction this function performs
;; a range of tests to check if the j-units are defined in a
;; working form. It then returns warnings about
;; possible problems including the source. 
(defmethod j-unit-test ((structure list) (report report))
  ; get the j-unit in the pole
  (let* ((j-unit (get-J-unit structure)) (j-unit-j (first j-unit)))
    (progn
      (when (and j-unit (listp j-unit) (listp j-unit-j))
            ; the second element in the j-unit must be a variable
            (unless (variable-p (second j-unit-j))
              (report (make-report-item (second j-unit-j)
                                        :item-class 'j-unit-test
                                        :message "The j-unit requires a variable at the second position.")
                      report))
            ; the third element in the j-unit must be a variable
            (if (not (variable-p (third j-unit-j)))
                (report (make-report-item (third j-unit-j)
                                          :item-class 'j-unit-test
                                          :message "The j-unit requires a variable at the third position.")
                        report)
                ; the third variable should reappear in the pole as it links the j-unit
                (unless (>= (count-item-in-struct (third j-unit-j) structure) 2)
                  (report (make-report-item (third j-unit-j)
                                            :item-class 'j-unit-test
                                            :message "The third variable in the j-unit should reappear in the same pole.")
                          report)))
            ; if a fourth element is given, it should be a list
            (if (and (fourth j-unit-j) (not (listp (fourth j-unit-j))))
                (report (make-report-item (fourth j-unit-j)
                                          :item-class 'j-unit-test
                                          :message "The fourth item in the j-unit is expected to be a list.")
                        report)
                ; we know that a list was given as a fourth element
                (if (fourth j-unit-j)
                    (let ((non-repeated-j-vars nil))
                      (progn
                        (loop for var-def in (fourth j-unit-j)
                           do (when (< (count-item-in-struct var-def structure) 2)
                              (setf non-repeated-j-vars (append non-repeated-j-vars (list var-def)))))
                        ; if there are variables in the fourth element they should reappear within the same pole
                        (when (> (length non-repeated-j-vars) 0)
                          (report (make-report-item non-repeated-j-vars
                                                    :item-class 'j-unit-test
                                                    :message "Some of the variables defined in the fourth position of the j-unit to not reappear in the rest of the pole.")
                                  report))))))
            ; check whether tag-linking variables in the j-unit are actually defined.
            (loop for elem in j-unit
                 ; if there is a tag-linking var
                 when (and (variable-p elem)
                           ; it should be part of the tag definition in a unit that is not the j-unit
                           (not (loop for unit in structure
                                     when (and (not (J-unit-p unit))
                                               (get-tag elem unit))
                                     return t)))
                 do (report (make-report-item elem
                                              :item-class 'j-unit-test
                                              :message "Linking variable is not defined in tag!")
                               report)))
      ; make sure to always return the report
      report)))

;; The feature test
;; ------------------------------------------------------------------------

(defgeneric feature-test (structure report))

(defmethod feature-test ((structure t) (report t))
  nil)

;; Given a pole from an fcg construction this function performs
;; a range of tests to check if the features are defined only
;; once and have a working form. It then returns warnings about
;; possible problems including the source. 
(defmethod feature-test ((structure list) (report report))
      ; iterate over all units in the pole
      (loop for unit in structure
        do (when (listp unit)
             ; for each unit run through it's features
             (loop for feature in unit
                   if (listp feature)
                   ; record the feature names (e.g. "sem-cat" in (sem-cat ...)
                   append (list (first feature)) into unit-feature-names
                   if (listp unit-feature-names)
                     ; the names of the features are not supposed to be variables
                     do (when (variable-p (first (last unit-feature-names)))
                          (report (make-report-item (last unit-feature-names)
                                                    :item-class 'feature-test
                                                    :message "A variable was given where a feature name was expected!")
                                  report))
                     ; make sure that features are defined only once
                     do (when (>= (count-item-in-struct (first (last unit-feature-names))
                                                        unit-feature-names)
                                  2)
                          (report (make-report-item (first (last unit-feature-names))
                                                    :item-class 'feature-test
                                                    :message "A feature was declared more than once!")
                                  report)))))
    report)

;; The duplicate-test
;; ------------------------------------------------------------------------

(defgeneric duplicate-test (structure report))

(defmethod duplicate-test ((structure t) (report t))
  nil)

(defmethod duplicate-test ((structure list) (report report))
  (loop for item in (recursive-duplicate-test (list structure))
       when item
       do (report (make-report-item item
                                    :item-class 'duplicate-test
                                    :message "This item is a duplicate!")
                  report))
  report)

(defgeneric recursive-duplicate-test (structure))

(defmethod recursive-duplicate-test ((structure t))
  nil)

(defmethod recursive-duplicate-test ((structure list))
  (append (get-duplicates-in structure)
          (loop for elem in structure
             append (recursive-duplicate-test elem))))

(defun get-duplicates-in (list)
  (loop for (e1 e2) in (combinations-of-length list 2)
       when (equal e1 e2)
       collect e1))

;; The tag-test
;; ------------------------------------------------------------------------

(defgeneric tag-test (structure report))

(defmethod tag-test ((structure t) (report t))
  nil)

;; Small helper function to allow for safe
;; recursion calls in lambda expressions.
(defun first-in-list (list)
  (when (listp list)
    (first list)))

;; Given a pole from a construction this function performs
;; a range of tests to check if the tag assignment will
;; likely apply as intended and returns warnings about
;; possible problems including the source. 
(defmethod tag-test ((structure list) (report report))
  ; find all (tag ...) definitions in the structure
  (let ((tags (find-all-anywhere 'TAG structure :key #'first-in-list)))
    (if (or (eq tags nil) (not (listp tags)))
        ; just return report if there are no tags...
        report
        (progn
          ; go through each (tag ...) definition
          (loop for tag in tags
             ; check if the (tag ...) has some content
             when (or (not (listp tag))
                      (eq nil tag)
                      (<= (length tag) 1))
             return (report (make-report-item tag
                                              :item-class 'TAG-test
                                              :message "The tag list is not valid!")
                            report)
             ; collect all variables in (tag var1 val1 [...] varn valn) ...
             do (loop for (tag-var tag-val) on (rest tag) by #'cddr
                   ; check if the variables are at the correct positions
                   if (not (variable-p tag-var))
                   do (report (make-report-item tag-var
                                                :item-class 'TAG-test
                                                :message "A variable is expected in this slot!")
                              report)
                   ; check wether a value is given for the variable
                   else when (eq tag-val nil)
                   do (report (make-report-item tag-var
                                                :item-class 'TAG-test
                                                :message "Variable provided without value!")
                              report)))
          report))))

;; =========================================================================
;; CURRENTLY REPLACED WITH A SINGLE (TRACE) MONITOR ACTIVATION
;; =========================================================================
;; Method around initialize-instance of constructions. Executes first
;; the creation of the construction and then calls (optionally)
;; for a number of fcg-structure-tests. These tests focus mostly
;; on the syntactic correctness of the construction and will produce
;; warnings defining any possible sources of errors
;; To enable all tests pass :check-cxn t
;(defmethod initialize-instance :around ((cxn construction)
;                                        &key (check-cxn nil)
;                                        &allow-other-keys)
;  (call-next-method) ;(make-cxn)
;  (unless (eq nil check-cxn)
;      (when (eq t check-cxn)
;         (setq check-cxn '(duplicate-test feature-test j-unit-test tag-test))) ; make available checks a global var...
;      (notify performing-check-cxn-tests cxn check-cxn)
;      (let ((report (make-instance 'report)))
;      (loop for test in check-cxn
;          when (not (eq test nil))
;          do (report (make-instance 'report-item :item (check-fcg-structure cxn test)) report)
;          finally (print-report report :monitors t)))))


;; A range of overloaded functions to fork the different check-cxn-tests.
;; -------------------------------------------------------------------------

(defgeneric check-cxn (cxn test))

(defmethod check-cxn ((cxn t) (test t))
  (report (make-report-item test
                            :item-class "error"
                            :message "Unkown test passed to check-cxn-test.")
          (make-report :name
                       "check-cxn-error"
                       :description 
                       "There was a problem in check-cxns.")))

; macro creates check-cxn methods specializing on the check-cxn-test types
(defmacro define-check-cxn-defmethod (check-cxn-test)
  `(defmethod check-cxn (cxn (test (eql ',check-cxn-test)))
     (funcall ',check-cxn-test (right-pole-structure cxn)
              (funcall ',check-cxn-test (left-pole-structure cxn)
                       (make-report :name
                                    (mkstr test)
                                    :description
                                    (mkstr "The " test " detected some possible mistakes:"))))))

; function to automatically generate check-cxn methods
(defun create-check-cxn-tests (list)
  (loop for expr in (loop for test in list
                       when test
                       collect `(define-check-cxn-defmethod ,test))
        do (eval expr)))

; create methods for all the tests in *check-cxn-tests*
(create-check-cxn-tests *check-cxn-tests*)