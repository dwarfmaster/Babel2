(in-package :planning)

;;##########################################################################;;
;; This file contains the monitor functions for the planning package        ;;
;;                                                                          ;;
;; Contents:                                                                ;;
;;   1. Defining the monitor trace-planning                                 ;;
;;   2. Defining planning events                                            ;;
;;   3. Define event-handlers for planning events                           ;;
;;   4. Define event-handlers for construction application                  ;;
;;   5  HTML                                                                ;;
;;##########################################################################;;

(export '(trace-planning))

;;;;;;;;;;;;;;;;;;;;
;; Define Monitor ;;
;;;;;;;;;;;;;;;;;;;;

(define-monitor trace-planning
                :class 'trace-monitor)

;;;;;;;;;;;;;;;;;;;;
;; Define Events  ;;
;;;;;;;;;;;;;;;;;;;;

(define-event planning-started (n t) (objects list) (fluents list) (goal list)
              (construction-inventory construction-inventory))

(define-event plan-emulation-started (n t) (objects list) (fluents list) (plan list) (possible-goals list)
              (construction-inventory construction-inventory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define event-handlers in planning package  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-event-handler (trace-planning planning-started)
  (add-element `((hr)))
  (add-element `((h2) ,(format
                        nil
                        "Planning (~a)&#160;"
                        (if (typep n 'number)
                          (format nil "max ~a solutions" n)
                          "all solutions"))))
  (add-element '((h3) "Goal: "))
  (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                 (predicate-network->svg goal :only-variables nil)
                 (html-pprint goal)))
  (add-element '((h3) "Initial State: "))
  (add-element (typecase (first objects)
                 (coupled-feature-structure (make-html-fcg-light (first objects)
                                                                 :cxn-inventory construction-inventory
                                                                 :feature-types (feature-types construction-inventory)))
                 (t
                  (if (get-configuration construction-inventory :draw-meaning-as-network)
                    (predicate-network->svg (append objects fluents) :only-variables nil)
                    (html-pprint (append objects fluents)))))))

(define-event-handler (trace-planning plan-emulation-started)
  (add-element `((hr)))
  (add-element `((h2) ,(format nil "Recognising (~a)"
                               (if (typep n 'number)
                                 (format nil "max ~a solutions" n)
                                 "all solutions"))))
  (add-element '((h3) "Plan: "))
  (add-element `((p) ,(format nil "~{~a~^, ~}" plan)))
  (add-element '((h3) "Initial State: "))
  (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                 (predicate-network->svg (append objects fluents) :only-variables nil)
                 (html-pprint (append objects fluents))))
  (add-element '((h3) "Goal Templates: "))
  (dolist (pg possible-goals)
    (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                   (predicate-network->svg pg :only-variables nil)
                 (html-pprint pg)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define event-handlers for construction application  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :fcg)

;; Adding and Deleting cxns

(define-event-handler (planning::trace-planning cxn-deleted)
  (add-element '((hr)))
  (add-element 
   `((h4) "Removed&#160;&#160; " 
     ,(make-html construction :expand-initially nil) " from&#160;&#160;"
     ,(make-html construction-inventory))))

(define-event-handler (planning::trace-planning cxn-added)
  (add-element '((hr)))
  (add-element 
   `((h4) "Added&#160;&#160; " 
     ,(make-html construction :expand-initially nil) " to&#160;&#160;"
     ,(make-html construction-inventory))))

;; construction application

(define-event-handler (planning::trace-planning cxn-application-started)
  (add-element `((hr)))
  (add-element 
   `((h3) "Applying "
     ,(make-html (get-original-cxn cxn)
                 :direction direction)
     "in "
     ,(if (eq direction '->) "planning" "plan recognition"))))

(define-event-handler (planning::trace-planning cxn-application-finished)
  (let ((cars (append failed-cars resulting-cars)))
    (cond ((> (length cars) 1)
           (add-element `((p) ((b) ,(length cars) " results:")))
           (loop for car in cars 
                 do (add-element '((hr)))
                 (add-element (make-html-fcg-light car
                                                   :feature-types (feature-types
                                                                   (original-cxn-set (cxn-inventory (car-applied-cxn car))))))))
          (cars (add-element `((div)
                               ,(make-html-fcg-light (first cars)
                                                     :feature-types (feature-types
                                                                     (original-cxn-set (cxn-inventory (car-applied-cxn (first cars)))))))))
          (t (add-element `((div) ((b) "no match")))))))

;; construction set application

(define-event-handler (planning::trace-planning cip-started)
  (add-element `((hr)))
  (add-element 
   `((h3) ,(if (children (top-node cip))
             "Computing next solution for application of "
             "Applying ")
     ; because constantly rendering the full construction inventory
     ; gets very slow with a large number of constructions, turn off
     ; rendering once the inventory gets larger than:
     ,(if (> (size (construction-inventory cip)) (get-configuration (construction-inventory cip) 
                                                                    :max-size-for-html))
        (format nil "a large ~a (~d)"
                (get-construction-inventory-title-string (original-cxn-set (construction-inventory cip)))
                (size (original-cxn-set (construction-inventory cip))))
     (make-html (original-cxn-set (construction-inventory cip))))
     " in "
     ,(if (eq (direction cip) '->) "planning" "plan recognition"))))

(define-event-handler (planning::trace-planning cip-finished)
  (add-element '((hr)))
  (add-element (make-html-fcg-light cip :solutions (when solution (list solution)))))

(define-event-handler (planning::trace-planning cip-restart-requested)
  (add-element '((hr)))
  (add-element `((h4) "Restart requested"))
  (add-element (make-html-fcg-light (cip cipn))))

(define-event-handler (planning::trace-planning fcg-apply-w-n-solutions-started)
  (add-element `((hr)))
  (add-element 
   `((h3) ,(if (typep n 'integer)
             (format nil "Computing max ~a solutions for application of " n)
             "Computing all solutions for application of ")
     ,(make-html (original-cxn-set construction-inventory)) " in "
     ,(if (eq direction '->) "planning" "plan recognition"))))

(define-event-handler (planning::trace-planning fcg-apply-w-n-solutions-finished)
  (add-element '((hr)))
  (add-element (make-html-fcg-light cip :solutions solutions)))

(define-event-handler (planning::trace-planning added-fcg-cxn-set))

;; planning / plan emulation
     
(define-event-handler (planning::trace-planning produce-all-finished)
  (if utterances
    (progn
      (add-element `((h3) ,(format nil "Plans:")))
      (dolist (plan utterances)
        (dolist (plan-part plan)
          (add-element `((h4) ,(string-append (format nil "  ~a: " (first plan-part))
                                              (format nil "~{~a~^, ~}" (instantiate-variables (rest plan-part)))))))))
    (progn
      (add-element `((h3) ,(format nil "Plans:")))
      (add-element '((p) "No plans could be found."))))
  (add-element `((p) " ")))


(define-event-handler (planning::trace-planning parse-all-finished)
  (add-element `((h3 :style "margin-bottom:3px;") "Goals:"))
  (if meanings 
    (loop for meaning in meanings
          for i from 1
          do
          (add-element `((h3) ,(format nil "Solution ~a " i)))
          (add-element `((h4) ,(format nil "Goal: ~a" (caar meaning))))
          (add-element  `((h4) ,(format nil "Meaning: ~{~a~^, ~}" (cdar meaning)))))
    (add-element '((p) "None of the goal templates were satisfied in the final state.")))
    (add-element `((p) " ")))

#|
(define-event-handler (planning::trace-planning parse-all-finished)
  (add-element `((h3 :style "margin-bottom:3px;") "Goals:  "))
  (if meanings 
    (loop for meaning in meanings
          for i from 1
          do
          (add-element `((h4) ,(format nil "Solution ~a " i)))
          (loop for emulated-goal in meaning
                for j from 1
                do
                (when (> (length meaning) 1)
                  (add-element `((h5) ,(format nil "Satisfied goal ~a " j))))
                (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                               (predicate-network->svg emulated-goal :only-variables nil)
                               (html-pprint meaning)))))
    (add-element '((p) "None of the goal templates were satisfied in the final state.")))
    (add-element `((p) " ")))
|#

;;;;;;;;;;;
;; HTML  ;;
;;;;;;;;;;;

(pushnew '((planning::circular-plan) . "#9999ff;") fcg::*status-colors* :test 'equal)


