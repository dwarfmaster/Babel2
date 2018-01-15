(in-package :fcg)


(defun subs-lookup (pattern-bindings source-bindings pattern source)
  "returns the binding for a given pattern and source in the bindings-lists"
  (cond
   ;; Case: no pattern-bindings left
   ((null pattern-bindings)
    +fail+)
   ;; Case: see whether pattern and source are first elements of bindings, if so, return binding
   ((and (equal (car (first pattern-bindings)) pattern)
         (equal (car (first source-bindings)) source)
         (equal (cdr (first pattern-bindings)) (cdr (first source-bindings))))
    (cdr (first pattern-bindings)))
   ;; Case: otherwise compare rests
   (t
    (subs-lookup (cdr pattern-bindings) (cdr source-bindings) pattern source))))

(defun substitute-bindings-cxn (bindings processing-cxn)
  (let ((new-cxn (copy-object processing-cxn)))
    (setf (name new-cxn) (make-id (string-append "anti-unified-" (symbol-name  (name processing-cxn)))))
    (setf (pole-structure (left-pole new-cxn)) (substitute-bindings bindings (left-pole-structure processing-cxn)))
    (setf (pole-structure (right-pole new-cxn)) (substitute-bindings bindings (right-pole-structure processing-cxn)))
  new-cxn))

(defun source-substitution-of-pattern-p (pattern source pattern-bindings source-bindings)
  "returns t if pattern is in pattern-bindings, and the corresponding value of source-bindings is NOT equal to source.
   returns nil otherwise"
  (let ((found nil))
    (loop for pb in pattern-bindings
          for sb in source-bindings
          do
          (when (and (equalp (car pb) pattern)
                     (not (equalp (car sb) source)))
            (setf found t)))
    found))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing and displaying ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anti-unify-print (pattern source)
  "prints all solutions (resulting-pattern, bindings for pattern, bindings for source)
   nicely formatted for each solution of the anti-unification fo pattern with source"
  (let ((solutions (anti-unify pattern source :fcg)))
    (loop for solution in solutions
          for s from 1 upto (length solutions) 
          do
          (format t "~%")
          (format t "~%")
          (format t "~%")
          (format t "SOLUTION ~a~%" s)
          (pprint "Resulting Pattern: ")
          (pprint (first solution))
          (format t "~%")
          (pprint "Pattern Bindings: ")
          (pprint (second solution))
          (format t "~%")
          (pprint "Source Bindings: ")
          (pprint (third solution)))))

