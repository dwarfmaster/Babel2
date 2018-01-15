(in-package :planning)

;; The extract-plan methods are used in planning for extracting the
;; plan from a final state

(defgeneric extract-plan (solution mode))

(defmethod extract-plan (solution (mode t))
  (error "Please implement an extract-plan method for ~a" mode))

(defmethod extract-plan (solution (mode (eql :extract-plan-from-actions)))
  "Extract the plan from the action units in the correct order. Loop
through nodes in search branch and collect all actions and
corresponding arguments from the transient structure. By default, we
respect the order in which the actions were applied."
  (let ((plan nil))
    (dolist (node (append (list solution) (all-parents solution)))
      (let ((merged-units (car-first-merge-added (cipn-car node))))
        (dolist (unit merged-units)
          (when (unit-feature unit 'action)
            (push (cons (unit-feature-value unit 'action)
                        (unit-feature-value unit 'args))
                  plan)))))
    plan))

(defmethod extract-plan (solution (mode (eql :extract-hierarchical-plan-from-form)))
  "Extract the plan based on form predicates. For reflecting some hierarchy, the form
   predicates are preceded by the name of the construction by which they were introduced."
  (let ((plan nil))
    (dolist (node (append (list solution) (all-parents solution)))
      (let ((merged-units (car-second-merge-added (cipn-car node))))
        (dolist (unit merged-units)
          (when (unit-feature unit 'form)
            (push (cons (name (car-applied-cxn (cipn-car node))) (unit-feature-value unit 'form))
                  plan)))))
    plan))