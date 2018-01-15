;(asdf:make :fcg)

(in-package :fcg)

(load (babel-pathname :directory '("grammars" "Russian") :name "motion-verbs" :type "lisp"))

(defparameter *test-sentences-and-meanings* ;;sentences with meanings (csv)
  (babel-pathname :directory '("grammars" "Russian")
                  :name "russian-test-sentences" :type "txt"))

;;Does not completely yield 100% of accuracy because sometimes a
;;sentence can have two interpretations. Currently the evaluation
;;package does not support this (todo: update)
(evaluate-grammar-for-comprehension *test-sentences-and-meanings* ;test set
                                    *fcg-constructions* ;;grammar
                                    :bi-directional? t ;;comprehend and reformulate
                                    :series 1 ;;iterations per sentence (4 by default)
                                    :write-bidirectional-sentences t)

(evaluate-grammar-for-production *test-sentences-and-meanings* ;test set
                                 *fcg-constructions* ;;grammar
                                 :bi-directional? t ;;comprehend and reformulate
                                 :series 1 ;;iterations per sentence (4 by default)
                                 :write-bidirectional-meanings t)

;; Testing the grammar:
;;-----------------------------------------
(activate-monitor trace-fcg)

(clear-page)
(comprehend '("misha" ) :cxn-inventory *russian-motion-grammar*)
;;TEMPORAL (NO TRAJECTORY)
;;---------------------------
(set-configuration (visualization-configuration *russian-motion-grammar*)
                   :show-constructional-dependencies t)
(comprehend '("masha"  "xodit") :cxn-inventory *russian-motion-grammar*)
(comprehend '("masha"  "idti") :cxn-inventory *russian-motion-grammar*)
(comprehend '("masha" "po-" "xodit") :cxn-inventory *russian-motion-grammar*)
(comprehend-all '("masha" "po-" "idti") :cxn-inventory *russian-motion-grammar*)
(comprehend-all '("misha" "za-" "xodit") :cxn-inventory *russian-motion-grammar*);;Misha pops in, Misha starts to walk in multiple directions
(comprehend-all '("misha" "za-" "idti") :cxn-inventory *russian-motion-grammar*) ;;Misha has popped in (completed)


(comprehend-and-formulate '("misha" "s-" "idti") :cxn-inventory *russian-motion-grammar*) ;;Misha descended (from a platform) 
(comprehend-all '("misha" "s-" "idti") :cxn-inventory *russian-motion-grammar*)
(comprehend-all '("masha" "ot-" "xodit") :cxn-inventory *russian-motion-grammar*)
;;saiti soma

;;TEMPORAL
(comprehend '("masha"  "xodit") :cxn-inventory *russian-motion-grammar*)
(comprehend '("masha" "po-" "xodit") :cxn-inventory *russian-motion-grammar*)
(comprehend "masha po- xodit" :cxn-inventory *russian-motion-grammar*) ;;walked for a while
(comprehend-all "masha po- xodit" :cxn-inventory *russian-motion-grammar*)
(comprehend-all '("misha" "za-" "xodit") :cxn-inventory *russian-motion-grammar*) ;;misha started walking (impf)
(comprehend "masha po- idti" :cxn-inventory *russian-motion-grammar*) ;;start to walk in one direction 

(comprehend "misha xodit" :cxn-inventory *russian-motion-grammar*) ;;masha is walking


;;masha idti
(formulate '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
             (event-directionality ev-1 unidirectional)
             (event-aktionsart ev-1 durative)
             (event-bounds ev-1 ongoing)) :cxn-inventory *russian-motion-grammar*)

;;masha xodit
(formulate-all '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
                 (event-directionality ev-1 multidirectional)
                 (event-aktionsart ev-1 durative)
                 (event-bounds ev-1 ongoing)) :cxn-inventory *russian-motion-grammar*)

;;masha po- idti
(formulate '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
             (event-directionality ev-1 unidirectional)
             (event-aktionsart ev-1 ingressive)
             (event-bounds ev-1 complete)) :cxn-inventory *russian-motion-grammar*)


;;masha po- xodit
(formulate '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
             (event-directionality ev-1 multidirectional)
             (event-aktionsart ev-1 delimitative)
             (event-bounds ev-1 complete)) :cxn-inventory *russian-motion-grammar*)

;;masha za- idti
(formulate '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
             (event-directionality ev-1 unidirectional) 
             (event-bounds ev-1 complete) (event-trajectory ev-1 in-out)) :cxn-inventory *russian-motion-grammar*)

;;masha za- xodit (cap)
(formulate '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
             (event-directionality ev-1 multidirectional)
             (event-aktionsart ev-1 ingressive)
             (event-bounds ev-1 complete)) :cxn-inventory *russian-motion-grammar*)

;;masha za- xodit (im)
(formulate '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
             (event-directionality ev-1 multidirectional) 
             (event-bounds ev-1 ongoing)
             (event-trajectory ev-1 in-out)) :cxn-inventory *russian-motion-grammar*)

;;masha vy- idti
(formulate '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
             (event-directionality ev-1 unidirectional)
             (event-bounds ev-1 complete)
             (event-trajectory ev-1 out)) :cxn-inventory *russian-motion-grammar*)

;;masha vy- xodit
(formulate '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
             (event-directionality ev-1 multidirectional)
             (event-bounds ev-1 ongoing) (event-trajectory ev-1 out)) :cxn-inventory *russian-motion-grammar*)

;;masha pri- idti
(formulate '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
             (event-directionality ev-1 unidirectional)
             (event-bounds ev-1 complete) (event-trajectory ev-1 towards)) :cxn-inventory *russian-motion-grammar*)

;;masha pri- xodit
(formulate '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
                 (event-directionality ev-1 multidirectional)
                 (event-bounds ev-1 ongoing) (event-trajectory ev-1 towards)) :cxn-inventory *russian-motion-grammar*)

;;masha u- idti
(formulate-all '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
                 (event-directionality ev-1 unidirectional)
                 (event-trajectory ev-1 from)
                 (event-bounds ev-1 complete)) :cxn-inventory *russian-motion-grammar*)

;;masha u- xodit
(formulate '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
              (event-directionality ev-1 multidirectional)
              (event-bounds ev-1 ongoing)
            (event-trajectory ev-1 from)) :cxn-inventory *russian-motion-grammar*)

;;masha ot- idti !!!!!!
(formulate '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
             (event-directionality ev-1 unidirectional) ;;TERMINATIVE???
             (event-bounds ev-1 complete) (event-trajectory ev-1 away-from)) :cxn-inventory *russian-motion-grammar*)
;; >> masha has moved away from X
(formulate-all '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
                 (event-directionality ev-1 unidirectional) 
                 (event-aktionsart ev-1 terminative) ;;added
                 (event-bounds ev-1 complete) (event-trajectory ev-1 away-from)) :cxn-inventory *russian-motion-grammar*)
;;;masha ot- idti >> masha stopped walking


;;masha ot- xodit
(formulate-all '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
                 (event-directionality ev-1 multidirectional) 
                 (event-bounds ev-1 ongoing) (event-trajectory ev-1 away-from)) :cxn-inventory *russian-motion-grammar*)

;;masha ot- xodit (cap) @yana: check please
(formulate-all '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
                 (event-directionality ev-1 multidirectional)
                 (event-aktionsart ev-1 terminative)
                 (event-bounds ev-1 complete)) :cxn-inventory *russian-motion-grammar*) ;;No trajectory

;masha s- idti
(formulate-all '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
                 (event-directionality ev-1 unidirectional) 
                 (event-bounds ev-1 complete) (event-trajectory ev-1 down)) :cxn-inventory *russian-motion-grammar*)

;;masha s- xodit
(formulate-all '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
                 (event-directionality ev-1 multidirectional) 
                 (event-bounds ev-1 ongoing) (event-trajectory ev-1 down)) :cxn-inventory *russian-motion-grammar*)

;;masha s- xodit (sap)
(formulate-all '((person masha o-1) (motion walk ev-1) (walk-agent ev-1 o-1)
                 (event-directionality ev-1 multidirectional) 
                 (event-bounds ev-1 complete) (event-trajectory ev-1 there-and-back)) :cxn-inventory *russian-motion-grammar*)