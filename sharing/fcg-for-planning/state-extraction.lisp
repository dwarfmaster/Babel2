(in-package :planning)

;; the extract-state-methods specify how a final state from plan recognition
;; should be presented to planning (in plan prediction)

(defgeneric extract-final-state (cfs mode))

(defmethod extract-final-state (cfs (mode (eql :objects-and-fluents)))
  "extract objects and fluents"
  (let ((structure (left-pole-structure cfs))
        (actual-objects nil)
        (actual-fluents nil))
    (dolist (unit structure)
      (cond
       ((find 'object (rest unit) :key 'first :test 'string=)
        (setf actual-objects (append (extract-meaning unit) actual-objects)))
       ((find 'fluent (rest unit) :key 'first :test 'string=)
          (setf actual-fluents (append (extract-meaning unit) actual-fluents)))))
    (values actual-objects actual-fluents)))

(defmethod extract-final-state (cfs (mode (eql :same-cfs)))
  "just return cfs"
  (list cfs))