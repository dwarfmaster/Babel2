
(in-package :fcg)

;;;;; Some helper functions.
;;;;; ------------------------------------------------------------------------------
(defun parse-fragment (utterance cxn-set)
  "Ensures that no goal tests are applied except :no-applicable-cxns"
  (set-configuration cxn-set :parse-goal-tests
                     '(:no-applicable-cxns))
  (parse utterance cxn-set)
  (set-configuration cxn-set :parse-goal-tests '(:no-applicable-cxns :arg-applied)))

(defun load-my-file (filename &optional (directory '(:relative "examples" "English-grammar")))
  (load (merge-pathnames (make-pathname :directory directory
                                        :name filename
                                        :type "lisp")
                         (babel-pathname))))

(defun make-cxn-name (id &optional (type 'cxn))
  "Creates a name for a construction."
  (make-symbol (format nil "~a-~a" id type)))

(defun show-cxn (construction-name)
  "Show the construction in the web browser."
  (show (get-cxn construction-name)))

(defun pp-cxn (construction-name)
  "Pretty-print a construction."
  (pp (get-cxn construction-name)))

(defun transient-structure (cip)
  "Returns the transient structure of a construction-inventory-processor."
  (car-resulting-cfs (cipn-car cip)))

(defun root-unit (pole)
  "Return the root-unit of a pole."
  (assoc 'root-unit pole))

(defun replace-feature-value-in-unit (new-value old-value feature-name unit)
  "Replace the value of a feature with a new value within a unit."
  (if (assoc feature-name (unit-body unit))
    (cons (unit-name unit)
          (subst (list feature-name new-value)
                 (list feature-name old-value)
                 (unit-body unit)
                 :test #'equal))
    (cons (unit-name unit)
          (cons (list feature-name new-value) (unit-body unit)))))

(defun replace-unit (new-unit old-unit pole)
  "Replace a unit in a pole."
  (substitute new-unit old-unit pole :test #'equal))