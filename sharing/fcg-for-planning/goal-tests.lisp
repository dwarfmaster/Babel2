(in-package :planning)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains goal-tests that are useful for planning in FCG ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod cip-goal-test ((node cip-node) (mode (eql :goal-reached)))
  "Checks whether goal that was specified in the configuration of the node
   is compatable (equivalent upto variable renamings) to the meaning extracted
   from the transient structure"
  (let* ((cfs (car-resulting-cfs (cipn-car node)))
	 (structure (left-pole-structure cfs))
         (predicate-list nil))
    ;; collect meanings of transient structure
    (dolist (unit structure)
      (setf predicate-list (append predicate-list (extract-meaning unit))))
    ;; unify with :goal
    (when (irl:unify-irl-programs (get-configuration (construction-inventory node) :goal) predicate-list)
      (set-data (goal-test-data node) 'goal-reached t)
      t)))

(defmethod cip-goal-test ((node cip-node) (mode (eql :one-of-goals-reached)))
  "Checks whether one of the goal-templates specified in the configuration of the node
  "
  (let* ((cfs (car-resulting-cfs (cipn-car node)))
	 (structure (left-pole-structure cfs))
         (predicate-list nil)
         (goal-emulated nil))
    ;; collect meanings of transient structure
    (dolist (unit structure)
      (setf predicate-list (append predicate-list (extract-meaning unit))))
    ;; unify with goal-templates
    (dolist (possible-goal (get-configuration (construction-inventory node) :goal-templates))
      (when (irl:unify-irl-programs possible-goal predicate-list)
        (setf goal-emulated t)))
    (when goal-emulated
      (set-data (goal-test-data node) 'goal-reached t)
      t)))

(defmethod cip-goal-test ((node cip-node) (mode (eql :no-actions-in-root)))
  "The node is a valid solution when there is are no string features
left in the root unit's form predicates (comprehension only)."
  (let ((form-in-root (rest (assoc 'root
                                      (left-pole-structure
                                       (car-resulting-cfs (cipn-car node)))))))
    (set-data (goal-test-data node) 'form-in-root form-in-root)
    (not form-in-root)))