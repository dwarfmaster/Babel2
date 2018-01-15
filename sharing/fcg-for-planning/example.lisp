
;;##########################################################################;;
;;                                                                          ;;
;; Example file for the planning package                                    ;;
;;                                                                          ;;
;;##########################################################################;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading the package and setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (asdf:operate 'asdf:load-op :planning)

(in-package :planning)

(activate-monitor trace-planning)

;;;;;;;;;;;;;;;;;;;;;;;
;; Loading a grammar ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions planning
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (footprints set))
  :fcg-configurations ((:cxn-supplier . :ordered-by-label-and-score)
                       (:render-mode . :render-final-state)
                       (:queue-mode . :breadth-first)
                       (:max-search-depth . 100)
                       (:node-tests :check-duplicate :restrict-search-depth :no-duplicate-actions)
                       (:production-goal-tests :goal-reached)
                       (:parse-goal-tests :no-applicable-cxns)
                       (:production-order actions)
                       (:parse-order actions)
                       (:create-initial-structure-mode . :objects-and-fluents)
                       (:plan-extraction-mode . :extract-plan-from-actions)
                       (:goal-extraction-mode . :extract-goals-from-fluents)
                       (:state-extraction-mode . :objects-and-fluents))
  :visualization-configurations ((:with-search-debug-data . t))

;; Move action
(def-fcg-cxn walk-act
     ((?walk-unit
       (args (?ag ?l1 ?l2))
       (action walk))
      (?agent-at-unit
       (fluent agent-at)
       (meaning ((agent-at ?ag ?l1) >> (agent-at ?ag ?l2)))
       (args (?ag ?l1) >> (?ag ?l2)))
      (?empty-unit
       (fluent empty)
       (meaning ((empty ?l2) >> (empty ?l1)))
       (args (?l2) >> (?l1)))
      <-
      (?connected-unit
       (args (?l1 ?l2 land))
       (fluent connected)
       --
       (args (?l1 ?l2 land))
       (fluent connected))
      (?agent-at-unit
       (args (?ag ?l1))
       (fluent agent-at)
       --
       (args (?ag ?l1))
       (fluent agent-at))
      (?empty-unit
       (fluent empty)
       (args (?l2))
       --
       (fluent empty)
       (args (?l2)))
      (?walk-unit
       --
       (HASH form ((walk ?ag ?l1 ?l2 )))))
  :cxn-set actions
  :disable-automatic-footprints t)

(def-fcg-cxn swim-act
     ((?swim-unit
       (args (?ag ?l1 ?l2))
       (action swim))
      (?agent-at-unit
       (fluent agent-at)
       (meaning ((agent-at ?ag ?l1) >> (agent-at ?ag ?l2)))
       (args (?ag ?l1) >> (?ag ?l2)))
      (?empty-unit
       (fluent empty)
       (meaning ((empty ?l2) >> (empty ?l1)))
       (args (?l2) >> (?l1)))
      <-
      (?connected-unit
       (args (?l1 ?l2 water))
       (fluent connected)
       --
       (args (?l1 ?l2 water))
       (fluent connected))
      (?agent-at-unit
       (args (?ag ?l1))
       (fluent agent-at)
       --
       (args (?ag ?l1))
       (fluent agent-at))
      (?empty-unit
       (fluent empty)
       (args (?l2))
       --
       (fluent empty)
       (args (?l2)))
      (?swim-unit
       --
       (HASH form ((swim ?ag ?l1 ?l2 )))))
  :cxn-set actions
  :disable-automatic-footprints t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use the grammar in Planning (cf. Formulation) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (clear-page)

;; Find the shortest path to Location 4
(plan
 :objects
 '((location l1) (location l2) (location l3) (location l4) (location l5) (location l6) (location l6)
   (agent age0))
 :fluents
 '((empty l2) (empty l3) (empty l4) (empty l5) (empty l6) (empty l7)
   (sleeping-location l4) (sleeping-location l7)
   (connected l1 l2 land) (connected l2 l1 land) (connected l2 l3 land) (connected l3 l2 land) (connected l3 l4 land) (connected l4 l3 land)
   (connected l5 l6 land) (connected l6 l5 land) (connected l6 l7 land) (connected l7 l6 land)
   (connected l2 l5 water) (connected l5 l2 water)
   (agent-at age0 l1)
   (coordinates l1 0 0) (coordinates l2 0 1) (coordinates l5 0 2) (coordinates l6 0 3)
                        (coordinates l3 1 1)                      (coordinates l7 1 3)
                        (coordinates l4 2 2))
 :goal
 '((agent-at age0 l4)
   (sleeping-location l4)) :n 1)

;; Find the shortest path to location 7
(plan
 :objects
 '((location l1) (location l2) (location l3) (location l4) (location l5) (location l6) (location l7)
   (agent age1))
 :fluents
 '((empty l2) (empty l3) (empty l4) (empty l5) (empty l6) (empty l7)
   (sleeping-location l4) (sleeping-location l7)
   (connected l1 l2 land) (connected l2 l1 land) (connected l2 l3 land) (connected l3 l2 land) (connected l3 l4 land) (connected l4 l3 land)
   (connected l5 l6 land) (connected l6 l5 land) (connected l6 l7 land) (connected l7 l6 land)
   (connected l2 l5 water) (connected l5 l2 water)
   (agent-at age1 l1))
 :goal
 '((agent-at age1 l7)
   (sleeping-location l7)) :n 1)

;; Find all paths to a sleeping location
(plan
 :objects
 '((location l1) (location l2) (location l3) (location l4) (location l5) (location l6) (location l7)
   (agent age1))
 :fluents
 '((empty l2) (empty l3) (empty l4) (empty l5) (empty l6) (empty l7)
   (sleeping-location l4) (sleeping-location l7)
   (connected l1 l2 land) (connected l2 l1 land) (connected l2 l3 land) (connected l3 l2 land) (connected l3 l4 land) (connected l4 l3 land)
   (connected l5 l6 land) (connected l6 l5 land) (connected l6 l7 land) (connected l7 l6 land)
   (connected l2 l5 water) (connected l5 l2 water)
   (agent-at age1 l1))
 :goal
 '((agent-at age1 ?l)
   (sleeping-location ?l)) :n 20)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use the grammar in Plan Interpration (cf. Comprehension) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (clear-page)
(interpret-plan
 :objects
 '((location l1) (location l2) (location l3) (location l4) (location l5) (location l6) (location l7)
   (agent age1))
 :fluents
 '((empty l2) (empty l3) (empty l4) (empty l5) (empty l6) (empty l7)
   (sleeping-location l4) (sleeping-location l7)
   (connected l1 l2 land) (connected l2 l1 land) (connected l2 l3 land) (connected l3 l2 land) (connected l3 l4 land) (connected l4 l3 land)
   (connected l5 l6 land) (connected l6 l5 land) (connected l6 l7 land) (connected l7 l6 land)
   (connected l2 l5 water) (connected l5 l2 water)
   (agent-at age1 l1))
 :plan
 '((walk age1 l1 l2) (swim age1 l2 l5) (walk age1 l5 l6) (walk age1 l6 l7))
 :goal-templates
 '(((agent-at ?ag ?loc)
   (sleeping-location ?loc)))
 :n 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use the grammar in Plan Recognition (cf. use of formulation in comprehension) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (clear-page)
(recognise-plan
 :objects
 '((location l1) (location l2) (location l3) (location l4) (location l5) (location l6) (location l7)
   (agent age1))
 :fluents
 '((empty l2) (empty l3) (empty l4) (empty l5) (empty l6) (empty l7)
   (sleeping-location l4) (sleeping-location l7)
   (connected l1 l2 land) (connected l2 l1 land) (connected l2 l3 land) (connected l3 l2 land) (connected l3 l4 land) (connected l4 l3 land)
   (connected l5 l6 land) (connected l6 l5 land) (connected l6 l7 land) (connected l7 l6 land)
   (connected l2 l5 water) (connected l5 l2 water)
   (agent-at age1 l1))
 :plan ;; this is a partial plan
 '((walk age1 l1 l2))
 :goal-templates
 '(((agent-at ?age1 ?loc-1)
   (sleeping-location ?loc-1)))
 :n 3)



