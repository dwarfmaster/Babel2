(in-package :planning)

;; The extract-goals methods are used to extract the possible goals from a final state
;; in plan recognition

(defgeneric extract-goals (solution goal-templates mode))

(defmethod extract-goals (solution goal-templates (mode (eql :extract-goals-from-fluents)))
  "extract fluents from possible-goals to solution"
  (let* ((resulting-cfs (car-resulting-cfs (cipn-car solution)))
         (left-pole (left-pole-structure resulting-cfs))
         (current-fluents nil)
         (satisfied-goals nil))
    ;; get goals from final state
    (dolist (unit left-pole)
      (when (find 'fluent (rest unit) :test 'equal :key 'first)
        (let ((fluent (caadr (find 'meaning (rest unit) :test 'equalp :key 'first))))
          (push fluent current-fluents))))
    (dolist (pg goal-templates)
          (let ((satisfied-goal (substitute-bindings (first (irl:unify-irl-programs pg current-fluents)) pg)))
            (when satisfied-goal
              (push satisfied-goal satisfied-goals))))
    (reverse satisfied-goals)))

(defmethod extract-goals (solution goal-templates (mode (eql :find-goal-in-meaning)))
  "If the goal is marked as a solution, unify goal-templates with extracted meaning"
  (when  (or (field? (goal-test-data solution) 'goal-reached)
             (not (get-configuration solution :goal-templates)))
    (let ((satisfied-goals nil)
          (meaning (extract-meanings (left-pole-structure
                                      (car-resulting-cfs (cipn-car solution))))))
      (dolist (pg goal-templates)
        (let ((satisfied-goal (substitute-bindings (first (irl:unify-irl-programs pg meaning)) pg)))
          (when satisfied-goal
            (push satisfied-goal satisfied-goals))))
      (reverse satisfied-goals))))

(defmethod extract-goals (solution goal-templates (mode (eql :cxn-names-in-order-of-application)))
  "If no goal-templates or specified or the goal has been marked as reached by a goal test, then
   return the names of the applied constructions (most recent first)"
  (when  (or (field? (goal-test-data solution) 'goal-reached)
             (not (get-configuration solution :goal-templates)))
      (list (mapcar 'name (applied-constructions solution)))))