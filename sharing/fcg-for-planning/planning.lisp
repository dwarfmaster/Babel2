(in-package :planning)

;;##########################################################################;;
;; This file contains the main functions for the planning packaage in FCG   ;;
;;                                                                          ;;
;; Contents:                                                                ;;
;;   1. Invoking the planner                                                ;;
;;   2. Applying constructions                                              ;;
;;   3. Creating an initial transient structure                             ;;
;;   4. Search Strategies                                                   ;;
;;   5. Helper Functions                                                    ;;
;;##########################################################################;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Invoking the planner ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(plan interpret-plan recognise-plan))

(defun plan (&key objects fluents goal (cxn-inventory *fcg-constructions*) (n 3) (silent nil))
  "Takes an initial state and a goal and searches for plans.
   Cf. 'formulate'"
  (unless silent (notify planning-started n objects fluents goal cxn-inventory))
  (set-configuration cxn-inventory :goal goal)
  (find-plan objects fluents cxn-inventory :n n :silent silent))

(defun recognise-plan (&key objects fluents plan goal-templates  (cxn-inventory *fcg-constructions*) (n 3) (silent nil))
  "Takes an initial state and a plan and searches for the goal
   Cf. 'comprehend'"
  (unless silent (notify plan-emulation-started n objects fluents plan goal-templates cxn-inventory))
  (set-configuration cxn-inventory :goal-templates goal-templates)
  (execute-plan objects fluents plan goal-templates cxn-inventory :n n))

(defun predict-plan (&key objects fluents plan goal-templates  (cxn-inventory *fcg-constructions*) (n 1))
  "recognises a plan using a strategy based on re-entrance"
  ;; Get emulation result
  (multiple-value-bind (goals cip solutions)
      (recognise-plan :objects objects
                      :fluents fluents
                      :plan plan
                      :goal-templates goal-templates
                      :cxn-inventory cxn-inventory
                      :n 1)
    (declare (ignore goals))
    ;; Calculate possible goals
    (let ((ranked-plans nil))
      ;; Plan to all goals
      (multiple-value-bind (current-objects current-fluents)
          (extract-final-state (car-resulting-cfs (cipn-car (first solutions)))
                               (get-configuration cxn-inventory :state-extraction-mode))
        (let ((plans nil))
          (dolist (pg goal-templates)
            (multiple-value-bind (plan final-state cip)
                (plan :objects current-objects :fluents current-fluents :goal pg :cxn-inventory cxn-inventory :n n)
              (push (list plan final-state cip) plans)))
          (setf ranked-plans (sort plans '< :key (lambda(x) (length (first x)))))))
      (let ((goals-to-return))
        (loop for i from n downto 1
              for plan in ranked-plans
              do
              (push
               (extract-goals (first (succeeded-nodes (third plan))) goal-templates
                              (get-configuration cxn-inventory :goal-extraction-mode))
               goals-to-return))
        (reverse goals-to-return)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Applying constructions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric find-plan (objects fluents construction-inventory &key silent n)
  (:documentation "apply all constructions from construction-inventory to inital transient structure
   build from objects and fluents"))

(defmethod find-plan ((objects list) (fluents list) (construction-inventory fcg-construction-set)
                         &key silent n)
  "apply constructions in planning (cf. formulation)"
  (let ((processing-cxn-inventory (processing-cxn-inventory construction-inventory))
        (initial-cfs (create-initial-structure (list objects fluents)
                                               (get-configuration construction-inventory :create-initial-structure-mode))))
    (multiple-value-bind (solutions cip)
        (if n
          (fcg-apply-with-n-solutions processing-cxn-inventory initial-cfs '-> n
                                      :notify (not silent))
          (fcg-apply-exhaustively processing-cxn-inventory initial-cfs '->
                                  :notify (not silent)))
      (let* ((succeeded-nodes (reverse (succeeded-nodes cip)))
             (plans (mapcar #'(lambda (solution)
                                (extract-plan solution (get-configuration construction-inventory :plan-extraction-mode)))
                            succeeded-nodes))
             (ranked-plans
              (rank-plans plans solutions))
             (final-states
              (mapcar #'(lambda (solution)
                          (extract-meanings (left-pole-structure (cipn-car solution))))
                      solutions)))
        (unless silent
          (notify produce-all-finished ranked-plans))
        (values ranked-plans final-states cip solutions)))))

(defgeneric execute-plan (objects fluents plan goal-templates construction-inventory &key silent n)
   (:documentation "apply all constructions from construction-inventory to inital transient structure
   build from objects and fluents."))

(defmethod execute-plan ((objects list) (fluents list) (plan list) (goal-templates list) (construction-inventory fcg-construction-set)
                         &key silent n)
  "apply constructions in plan-emulation (cf. comprehension)"
  (let ((processing-cxn-inventory (processing-cxn-inventory construction-inventory))
        (initial-cfs (create-initial-structure (list objects fluents plan)
                                               (get-configuration construction-inventory :create-initial-structure-mode))))
    (multiple-value-bind (solutions cip)
        (if n
          (fcg-apply-with-n-solutions processing-cxn-inventory initial-cfs '<- n
                                      :notify (not silent))
          (fcg-apply-exhaustively processing-cxn-inventory initial-cfs '<-
                                  :notify (not silent)))
      (let* ((goals nil))
        (dolist (solution solutions)
          (let ((extracted-goals (extract-goals solution goal-templates
                                                (get-configuration construction-inventory :goal-extraction-mode))))
                        (when extracted-goals
                          (push extracted-goals goals))))
        (unless silent
          (notify parse-all-finished goals processing-cxn-inventory))
        (values goals cip solutions)))))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Creating an initial transient structure ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod create-initial-structure ((objects-and-fluents list)
                                     (mode (eql :objects-and-fluents)))
  "create an initial transient structure for the objects and fluents,
   should maybe be combined with render-mode and de-render-mode instead
   of doing everything here, if a cfs is as the first of 'objects', it is returned as such"
  (typecase (caar objects-and-fluents)
    (coupled-feature-structure
     (caar objects-and-fluents))
    (t
     (let* ((left-pole nil)
              (objects (first objects-and-fluents))
              (fluents (second objects-and-fluents))
              (object-units (loop for el in objects
                                  collect `(,(make-id (write-to-string (first el)))
                                            (meaning (,el))
                                            (referent ,(second el))
                                            (object ,(first el)))))
              (fluents-units (loop for el in fluents
                                   collect `(,(make-id (write-to-string (first el)))
                                             (meaning (,el))
                                             (args ,(rest el))
                                             (fluent ,(first el))))))
         ;; For plan recognition, plan is given as third argument of objects-and-fluents
         (if (eq 3 (length objects-and-fluents))
           (let* ((plan (third objects-and-fluents))
                  (root `(root (form ,plan))))
             (setf left-pole (append (list root) object-units fluents-units)))
           ;; for planning
           (setf left-pole (append (list '(root)) object-units fluents-units)))
         (make-instance 'coupled-feature-structure
                        :left-pole left-pole
                        :right-pole '((root))
                        :left-pole-domain 'sem
                        :right-pole-domain 'syn)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 4. Search Strategies ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod cip-enqueue ((node cip-node) (cip construction-inventory-processor)
                        (mode (eql :breadth-first)))
  "Implements breadth-firs search" 
  (setf (priority node)  
        (cip-priority node (get-configuration cip :priority-mode)))
  (setf (queue cip) (sorted-insert (queue cip) node :key #'priority :test #'<=)))

(defmethod cip-enqueue ((node cip-node) (cip construction-inventory-processor)
                        (mode (eql :depth-first)))
  (setf (priority node)
        (cip-priority node (get-configuration cip :priority-mode)))
  (setf (queue cip) (sorted-insert (queue cip) node :key #'priority :test #'>)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; 5. Helper Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rank-plans (plans solutions)
  "ranks the plans by length (number of actions)"
  (let ((all-plans-with-path-length
         (loop for i from 0
               for plan in plans
               for solution = (nth i solutions)
               collect (list (priority solution)
                             plan))))
    (mapcar #'second (sort all-plans-with-path-length
                           #'< :key #'first))))



