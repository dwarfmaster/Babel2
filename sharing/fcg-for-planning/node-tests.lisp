(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains node-tests that are useful for planning in FCG ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(push (cons '(circular-plan) "#444")  *status-colors*)

(defmethod cip-node-test ((node cip-node) (mode (eql :no-duplicate-actions)))
  "Fails when exactly (even for variables) the same action is already part of the plan."
  (let ((plan (planning::extract-plan node (get-configuration node :plan-extraction-mode))))
    (if (= (length plan) (length (remove-duplicates plan :test 'equal)))
      t
      (and (push 'circular-plan (statuses node)) nil))))
