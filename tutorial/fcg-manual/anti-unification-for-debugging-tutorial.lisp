
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Demonstration of the use of anti-unification ;;
;; for debugging in grammar writing             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (asdf:operate 'asdf:load-op :fcg)
(in-package :fcg)
(activate-monitor trace-fcg)

;; 1. Execute the following grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions debugging-example-grammar
  :feature-types ((args sequence)
                  (footprints set)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set))
  
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
  (def-fcg-cxn 1-np-cxn
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
                 (syn-cat (agreement (gender ?gender)))
                 (args (?args))
                 --
                 (syn-cat (lex-class noun)
                          (agreement (number ?number)
                                     (gender ?gender))))
                (?np-unit
                 --
                 (HASH form ((meets ?det ?noun))))))
  
(def-fcg-cxn 2-np-cxn
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
                                     (gender ?gender)
                                     (remove me))))
                (?noun
                 (sem-cat (sem-class physical-object))
                 (syn-cat (agreement (gender ?gender)))
                 (args (?args))
                 --
                 (syn-cat (lex-class noun)
                          (agreement (number ?number)
                                     (gender ?gender))))
                (?np-unit
                 --
                 (HASH form ((meets ?det ?noun))))))
  
  (def-fcg-cxn 3-np-cxn
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
                                     (gender ?gender)
                                     (im (blocking cxn-application)))))
                (?noun
                 (sem-cat (sem-class physical-object))
                 (syn-cat (agreement (gender ?gender)))
                 (args (?args))
                 --
                 (syn-cat (lex-class noun)
                          (agreement (number ?number)
                                     (gender ?gender))))
                (?np-unit
                 --
                 (HASH form ((meets ?det ?noun))))))

  (def-fcg-cxn 4-np-cxn
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
                 ;;(some feature)
                 (syn-cat (lex-class determiner)
                          (agreement (number ?number))))
                (?noun
                 (sem-cat (sem-class physical-object))
                 (syn-cat (agreement (gender ?gender)))
                 (args (?args))
                 --
                 (syn-cat (lex-class noun)
                          (agreement (number ?number)
                                     (gender ?gender)))
                 (sem-cat (NOT (animate +))))
                (?np-unit
                 --
                 (HASH form ((meets ?det ?noun))))))


  (def-fcg-cxn 5-np-cxn
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
                 (some feature)
                 (syn-cat (lex-class determiner)
                          (agreement (number ?number))))
                (?noun
                 (sem-cat (sem-class physical-object))
                 (syn-cat (agreement (gender ?gender)))
                 (args (?args))
                 --
                 (another feature)
                 (syn-cat (lex-class noun)
                          (agreement (number ?number)
                                     (gender ?gender))))
                (?np-unit
                 --
                 (HASH form ((meets ?det ?noun)))))))

;; 2. Comprehend 'un fille'
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comprehend '("un" "fille"))

;; 3. Save the transient structure in the web interface
;; Hover over its top row and click the small T at the left
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;; 5. Now open the FCG Construction Set in the web-interface,
;;    hover over one of the NP-cxns and click the symbol
;;    'apply to *saved-cfs* in comprehension
;;
;;    Alternatively (and more nicely), you can open the
;;    construction set and drag an NP-cxn and drop it
;;    on the last green node in the application process.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 1-np-cxn: Detects gender mismatch (?gender en ?gender in cxn, with m and f in transient structure)

;; 2-np-cxn: Detects gender mismatch + feature in cxn that is not matched in transient structure

;; 3-np-cxn: Detects gender mismatch + complex feature in cxn that is not matched in transient structure

;; 4-np-cxn: Mismatch with negated feature

;; 5-np-cxn: multiple hypotheses (click show entire anti-unification anlysis)



