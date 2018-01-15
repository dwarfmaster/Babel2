(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File demonstrating the use of anti-unification in FCG ;;
;;                                                       ;;
;; (Paul - 11/02/2016)                                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1. Load FCG

;; (asdf:operate 'asdf:load-op :fcg)
;; (activate-monitor trace-fcg)

;; 2. Evalutate the following grammar

(def-fcg-constructions anti-unification-example-grammar
  :feature-types ((args sequence)
                  (footprints set)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set)
                  (forg set))
  :diagnostics (diagnose-no-match)
  :repairs (anti-unify-pro-unify)
  
  ;; Lexical construction for the word "fille"
  (def-fcg-cxn fille-cxn
               ((?girl-unit
                 (syn-cat (lex-class noun)
                          (agreement (number sg)
                                     (gender f)))
                 (sem-cat (sem-class physical-object)
                          (animate +))
                 (args (?x)))
                <-
                (?girl-unit
                 (HASH meaning ((girl ?x)))
                 --
                 (HASH form ((string ?girl-unit "fille"))))))

  ;; Lexical construction for the word "une"
  (def-fcg-cxn une-cxn
               ((?a-unit
                 (syn-cat (lex-class determiner)
                          (agreement (number sg)
                                     (gender f)))
                 (sem-cat (sem-class identifier)
                          (definite -))
                 (args (?x)))
                <-
                (?a-unit
                 (HASH meaning ((exists ?x)))
                 --
                 (HASH form ((string ?a-unit "une"))))))

  ;; Lexical construction for the word "un"
  (def-fcg-cxn un-cxn
               ((?a-unit
                 (syn-cat (lex-class determiner)
                          (agreement (number sg)
                                     (gender m)))
                 (sem-cat (sem-class identifier)
                          (definite -))
                 (args (?x)))
                <-
                (?a-unit
                 (HASH meaning ((exists ?x)))
                 --
                 (HASH form ((string ?a-unit "un"))))))

  ;; NP -> Det N
  (def-fcg-cxn np-cxn
               ((?np-unit
                 (args (?args))
                 (subunits (?det ?noun))
                 (syn-cat (agreement (number ?number)
                                     (gender ?gender))))
                <-
                (?det
                 (sem-cat (sem-class identifier)
                          (definite ?definite))
                 (args (?args))
                 --
                 (syn-cat (lex-class determiner)
                          (agreement (number ?number)
                                     (gender ?gender))))
                (?noun
                 (sem-cat (sem-class physical-object))
                 (args (?args))
                 --
                 (syn-cat (lex-class noun)
                          (agreement (number ?number)
                                     (gender ?gender))))
                (?np-unit
                 --
                 (HASH form ((meets ?det ?noun)))))))


;; 3. Comprehend these utterances the resulting meanings are all integrated (linked)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comprehend '("une" "fille"))
(comprehend '("un" "fille"))

(comprehend-all '("un" "fille"))