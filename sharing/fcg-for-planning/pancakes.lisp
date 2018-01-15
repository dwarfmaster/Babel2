
;;##########################################################################;;
;;                                                                          ;;
;; FCG for making pancakes (Paul, Katrien &Luc, January 2017)               ;;
;;                                                                          ;;
;;##########################################################################;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading the package and setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (asdf:operate 'asdf:load-op :planning)

(in-package :planning)

(deactivate-all-monitors)
(activate-monitor trace-planning)

;;;;;;;;;;;;;;;;;;;;;;;
;; Loading a grammar ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions recipe
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (status set))
  :fcg-configurations ((:cxn-supplier . :ordered-by-label-and-score)
                       (:render-mode . :render-final-state)
                       (:queue-mode . :breadth-first)
                       (:max-search-depth . 100)
                       (:node-tests :check-duplicate :restrict-search-depth)
                       (:production-goal-tests :goal-reached)
                       (:parse-goal-tests :no-actions-in-root :one-of-goals-reached)
                       (:production-order actions)
                       (:parse-order actions)
                       (:create-initial-structure-mode . :objects-and-fluents)
                       (:plan-extraction-mode . :extract-hierarchical-plan-from-form)
                       (:goal-extraction-mode . :find-goal-in-meaning)
                       (:state-extraction-mode . :same-cfs))
  :visualization-configurations ((:with-search-debug-data . t))
  :disable-automatic-footprints t



(def-fcg-cxn take-eggs
     ((?eggs
       (status (in-use)))
      <-
      (?eggs
       (referent ?b)
       (object eggs)
       (status (NOT in-use))
       --
       (referent ?b)
       (object eggs)
       (status (NOT in-use))
       (HASH form ((take-eggs ?b)))))
     :cxn-set actions)

(def-fcg-cxn take-flour
     ((?flour
       (status (in-use)))
      <-
      (?flour
       (referent ?b)
       (object flour)
       (status (NOT in-use))
       --
       (referent ?b)
       (object flour)
       (status (NOT in-use))
       (HASH form ((take-flour ?b)))))
     :cxn-set actions)

(def-fcg-cxn take-milk
     ((?milk
       (status (in-use)))
      <-
      (?milk
       (referent ?b)
       (object milk)
       (status (NOT in-use))
       --
       (referent ?b)
       (object milk)
       (status (NOT in-use))
       (HASH form ((take-milk ?b)))))
     :cxn-set actions)

(def-fcg-cxn collect-pancake-ingredients
             ((?pancake-ingredients
               (meaning ((pancake-ingredients ?pi ?e ?f ?m)))
               (args (?pi))
               (subunits (?flour ?eggs ?milk)))
              <-
              (?flour
               (referent ?f)
               (object flour)
               (status (in-use))
               --
               (referent ?f)
               (object flour)
               (status (in-use)))
              (?eggs
               (referent ?e)
               (object eggs)
               (status (in-use))
               --
               (referent ?e)
               (object eggs)
               (status (in-use)))
              (?milk
               (referent ?m)
               (object milk)
               (status (in-use))
               --
               (referent ?m)
               (object milk)
               (status (in-use))))
             :cxn-set actions)

(def-fcg-cxn take-butter
     ((?butter
       (status (in-use)))
      <-
      (?butter
       (referent ?b)
       (object butter)
       (status (NOT in-use))
       --
       (referent ?b)
       (object butter)
       (status (NOT in-use))
       (HASH form ((take-butter ?b)))))
     :cxn-set actions)

(def-fcg-cxn take-pan
     ((?pan
       (status (in-use)))
      <-
      (?pan
       (referent ?b)
       (object pan)
       (status (NOT in-use))
       --
       (referent ?b)
       (object pan)
       (status (NOT in-use))
       (HASH form ((take-pan ?b)))))
     :cxn-set actions)

(def-fcg-cxn prepare-pan
     ((?prepared-pan
       (meaning ((prepared-pan ?pp ?p ?b ?s)))
       (args (?pp))
       (subunits (?pan ?butter ?stove)))
      (?stove
       (meaning ((heating ?s))))
      <-
      (?stove
       (referent ?s)
       (object stove)
       --
       (referent ?s)
       (object stove))
      (?pan
       (referent ?p)
       (object pan)
       (status (in-use))
       --
       (referent ?p)
       (object pan)
       (status (in-use))
       )
      (?butter
       (referent ?b)
       (object butter)
       (status (in-use))
       --
       (referent ?b)
       (object butter)
       (status (in-use)))
      (?prepared-pan
       --
       (HASH form ((put-butter-in-pan ?b ?p)
                   (turn-on-stove ?s)
                   (put-pan-on-stove ?p ?s)))))
  :cxn-set actions)

(def-fcg-cxn take-bowl
     ((?bowl
       (status (in-use)))
      <-
      (?bowl
       (referent ?b)
       (object bowl)
       (status (NOT in-use))
       --
       (referent ?b)
       (object bowl)
       (status (NOT in-use))
       (HASH form ((take-bowl ?b)))))
     :cxn-set actions)

(def-fcg-cxn take-whisk
     ((?whisk
       (status (in-use)))
      <-
      (?whisk
       (referent ?b)
       (object whisk)
       (status (NOT in-use))
       --
       (referent ?b)
       (object whisk)
       (status (NOT in-use))
       (HASH form ((take-whisk ?b)))))
     :cxn-set actions)

(def-fcg-cxn make-pancake-dough
     ((?pancake-dough
       (args (?d))
       (meaning ((pancake-dough ?d ?b ?w ?i)))
       (subunits (?ingredients ?bowl ?whisk)))
      (?ingredients
       (meaning ((stirred ?i))))
      <-
      (?ingredients
       (args (?i))
       (meaning ((pancake-ingredients ?i ?i1 ?i2 ?i3)))
       --
       (args (?i))
       (meaning ((pancake-ingredients ?i ?i1 ?i2 ?i3))))
      (?bowl
       (referent ?b)
       (object bowl)
       (status (in-use))
       --
       (referent ?b)
       (object bowl)
       (status (in-use)))
      (?whisk
       (referent ?w)
       (object whisk)
       (status (in-use))
       --
       (referent ?w)
       (object whisk)
       (status (in-use)))
      (?pancake-dough
       --
       (HASH form ((put-ingredients-in-bowl ?i ?b)
                   (stir ?w ?i)))))
  :cxn-set actions)

(def-fcg-cxn make-pancakes
     ((?pancakes
       (meaning ((pancakes ?pc ?d ?pp ?s)))
       (args ((?pc)))
       (subunits (?pancake-dough ?pan)))
      (?pancake-dough
       (status (baked)))
      <-
      (?pancake-dough
       (args (?d))
       (meaning ((pancake-dough ?d ?bowl ?w ?i)))
       (status (NOT baked))
       --
       (args (?d))
       (meaning ((pancake-dough ?d ?bowl ?w ?i)))
       (status (NOT in-use)))
      (?pan
       (meaning ((prepared-pan ?pp ?p ?b ?s)))
       --
       (meaning ((prepared-pan ?pp ?p ?b ?s))))
      (?pancakes
       --
       (HASH form ((put-dough-in-pan ?d ?pp)
                   (bake-dough-in-pan-on-stove ?d ?pp ?s)))))
  :cxn-set actions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use the grammar in Planning (cf. Formulation) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (clear-page)

;(plan
; :objects
; '((flour f-1) (eggs e-1) (butter b-1) (milk m-1)
;   (pan p-1) (stove s-1) (tabasco t-1) (knife k-1)
;   (bowl b-2) (whisk w-2))
; :goal
; '((pancakes ?pc ?d ?pp ?s)) :n 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use the grammar in Plan Interpration (cf. Comprehension) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (clear-page)

;(recognise-plan
; :objects
; '((flour f-1) (eggs e-1) (butter b-1) (milk m-1)
;   (pan p-1) (stove s-1) (tabasco t-1) (knife k-1)
;   (bowl b-2) (whisk w-1)) 
; :plan
; '((take-eggs e-1) (take-flour f-1) (take-milk m-1)
;   (take-butter b-1) (take-pan p-1) (put-butter-in-pan b-1 p-1)
;   (turn-on-stove s-1) (put-pan-on-stove p-1 s-1)
;   (take-bowl b-2) (take-whisk w-1) (put-ingredients-in-bowl i-1 b-2)
;   (stir w-1 i-1) (put-dough-in-pan d-1 pp-1) (bake-dough-in-pan-on-stove d-1 pp-1 s-1))
;  :goal-templates
; '(((pancakes ?pancakes ?d ?pp ?s))
;   ((cake ?cake ?d ?pp ?s))
;   ((beefsteak-with-fries ?bwf ?b ?w ?f)))
; :n 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Use the grammar in Plan Recognition (cf. use of formulation in comprehension) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (clear-page)

;(predict-plan
; :objects
; '((flour f-1) (eggs e-1) (butter b-1) (milk m-1)
;   (pan p-1) (stove s1) (tabasco t-1) (knife k-1)
;   (bowl b-2) (whisk w-2))
; :plan ;; this is a partial plan
; '((take-eggs) (take-flour) (take-milk) (take-pan)
;   (take-butter) (put-butter-in-pan)
;   (turn-on-stove) (put-pan-on-stove))
; :goal-templates
; '(((pancakes ?pancakes ?d ?pp ?s)))
; :n 3)



