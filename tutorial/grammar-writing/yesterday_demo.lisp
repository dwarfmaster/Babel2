
;; (ql:quickload :fcg)
(in-package :fcg)
(activate-monitor trace-fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar Fragment for the sentence:            ;;
;; Yesterday , all my troubles seemed so far away ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(def-fcg-constructions yesterday-grammar

  ;; Noun Phrase
  ;;;;;;;;;;;;;;
  
  (def-fcg-cxn all-cxn
                 ((?all-unit
                   (sem-cat (class quantifier))
                   (syn-cat (class pre-determiner))
                   (args (?x)))
                  <-
                  (?all-unit
                   (HASH meaning ((complete-set ?x)))
                   --
                   (HASH form ((string ?all-unit "all"))))))
  
  (def-fcg-cxn my-cxn
                 ((?my-unit
                   (sem-cat (class possessive))
                   (syn-cat (class determiner))
                   (args (?x)))
                  <-
                  (?my-unit
                   (HASH meaning ((speaker-possession ?x)))
                   --
                   (HASH form ((string ?my-unit "my"))))))
  
  (def-fcg-cxn troubles-cxn
                 ((?troubles-unit
                   (sem-cat (class virtual-object))
                   (syn-cat (class noun))
                   (args (?x)))
                  <-
                  (?troubles-unit
                   (HASH meaning ((troubles ?x)))
                   --
                   (HASH form ((string ?troubles-unit "troubles"))))))
  
(def-fcg-cxn np-cxn
                 ((?np-unit
                   (sem-cat (class referring-expression))
                   (syn-cat (class NP))
                   (subunits (?det-unit ?noun-unit))
                   (left-most-subunit ?det-unit)
                   (right-most-subunit ?noun-unit)
                   (args (?x)))
                  <-

                  (?det-unit
                   (sem-cat (class possessive))
                   (args (?x))
                   --
                   (syn-cat (class determiner)))
                  (?noun-unit
                   (sem-cat (class virtual-object))
                   (args (?x))
                   --
                   (syn-cat (class noun)))
                  (?np-unit
                   --
                   (HASH form ((meets ?det-unit ?noun-unit))))))

(def-fcg-cxn pre-determiner-cxn
                 ((?np-unit
                   (sem-cat (class referring-expression))
                   (syn-cat (class NP))
                   (subunits (?pre-det-unit))
                   (args (?x)))
                  <-
                  (?np-unit
                   (sem-cat (class referring-expression))
                   (args (?x))
                   --
                   (syn-cat (class NP))
                   (left-most-subunit ?det-unit)
                   (HASH form ((meets ?pre-det-unit ?det-unit))))
                  (?pre-det-unit
                   (sem-cat (class quantifier))
                   (args (?x))
                   --
                   (syn-cat (class pre-determiner)))))

  ;; Adjectival Phrase
  ;;;;;;;;;;;;;;;;;;;;

  (def-fcg-cxn away-cxn
                 ((?away-unit
                   (sem-cat (class property))
                   (syn-cat (class adjective))
                   (args (?x)))
                  <-
                  (?away-unit
                   (HASH meaning ((distant ?x)))
                   --
                   (HASH form ((string ?away-unit "away"))))))

  (def-fcg-cxn far-cxn
                 ((?far-unit
                   (sem-cat (class property))
                   (syn-cat (class adjective))
                   (args (?x)))
                  <-
                  (?far-unit
                   (HASH meaning ((far ?x)))
                   --
                   (HASH form ((string ?far-unit "far"))))))

  (def-fcg-cxn far-cxn
                 ((?far-unit
                   (sem-cat (class modifier))
                   (syn-cat (class adverb))
                   (args (?x)))
                  <-
                  (?far-unit
                   (HASH meaning ((far ?x)))
                   --
                   (HASH form ((string ?far-unit "far"))))))

  (def-fcg-cxn so-cxn
                 ((?so-unit
                   (sem-cat (class pre-modifier))
                   (syn-cat (class mod-adverb))
                   (args (?x)))
                  <-
                  (?so-unit
                   (HASH meaning ((to-great-extent ?x)))
                   --
                   (HASH form ((string ?so-unit "so"))))))

  (def-fcg-cxn adjp-cxn
                 ((?adjp-unit
                   (sem-cat (class complex-property))
                   (syn-cat (class adjective-phrase))
                   (subunits (?adv-unit ?adj-unit))
                   (left-most-subunit ?adv-unit)
                   (args (?x)))
                  <-

                  (?adv-unit
                   (sem-cat (class modifier))
                   (args (?x))
                   --
                   (syn-cat (class adverb)))
                  (?adj-unit
                   (sem-cat (class property))
                   (args (?x))
                   --
                   (syn-cat (class adjective)))
                  (?adjp-unit
                   --
                   (HASH form ((meets ?adv-unit ?adj-unit))))))

  (def-fcg-cxn adv-adjp-cxn
                 ((?adjp-unit
                   (subunits (?adv-unit)))
                  <-
                  (?adv-unit
                   (sem-cat (class pre-modifier))
                   (args (?x))
                   --
                   (syn-cat (class mod-adverb)))
                  (?adjp-unit
                   (sem-cat (class complex-property))
                   (args (?x))
                   --
                   (syn-cat (class adjective-phrase))
                   (left-most-subunit ?lms)
                   (HASH form ((meets ?adv-unit ?lms))))))

  ;; Verb Phrase
  ;;;;;;;;;;;;;;

  (def-fcg-cxn seemed-cxn
                 ((?seemed-unit
                   (sem-cat (class connective))
                   (syn-cat (class copula))
                   (args (?x ?y ?moment-of-speaking ?ev)))
                  <-
                  (?seemed-unit
                   (HASH meaning ((appear-as ?ev ?x ?y)
                                  (moment-of-speaking ?moment-of-speaking now)
                                  (before ?ev ?moment-of-speaking)))
                   --
                   (HASH form ((string ?seemed-unit "seemed"))))))


  (def-fcg-cxn copula-cxn
                 ((?sentence
                   (sem-cat (class proposition))
                   (syn-cat (class sentence))
                   (left-most-subunit ?np-lms)
                   (subunits (?NP ?copula ?AdjP))
                   (args (?ev ?moment-of-speaking)))
                  <-
                  (?NP
                   (sem-cat (class referring-expression))
                   (args (?x))
                   --
                   (syn-cat (class np))
                   (left-most-subunit ?np-lms)
                   (right-most-subunit ?np-rms)
                   (HASH form ((meets ?np-rms ?copula))))
                  (?copula
                   (sem-cat (class connective))
                   (args (?x ?y ?moment-of-speaking ?ev))
                   --
                   (syn-cat (class copula)))
                  (?AdjP
                   (sem-cat (class complex-property))
                   (args (?y))
                   --
                   (syn-cat (class adjective-phrase))
                   (left-most-subunit ?adj-lms)
                   (HASH form ((precedes ?copula ?adj-lms))))))


  ;; Temporal modifier
  ;;;;;;;;;;;;;;;;;;;;

  (def-fcg-cxn yesterday-cxn
                 ((?yesterday-unit
                   (sem-cat (class temporal-modifier))
                   (syn-cat (class temporal-adverb))
                   (args (?ev ?moment-of-speaking)))
                  <-
                  (?yesterday-unit
                   (HASH meaning ((day-before ?ev ?moment-of-speaking)))
                   --
                   (HASH form ((string ?yesterday-unit "yesterday"))))))


  (def-fcg-cxn sentence-adv-cxn
               ((?sentence
                 (subunits (?time-adverbial ?connection-sign)))
                <-
                (?connection-sign
                 --
                 (HASH form ((string ?connection-sign ","))))
                (?sentence
                 (sem-cat (class proposition))
                 (args (?ev ?moment-of-speaking))
                 --
                 (syn-cat (class sentence))
                 (left-most-subunit ?sentence-lms)
                 (HASH form ((string ?connection-sign ",")
                             (meets ?time-adverbial ?connection-sign)
                             (precedes ?connection-sign ?sentence-lms))))
                (?time-adverbial
                 (sem-cat (class temporal-modifier))
                 (args (?ev ?moment-of-speaking))
                 --
                 (syn-cat (class temporal-adverb)))))
  )


(comprehend '("yesterday" "," "all" "my" "troubles" "seemed" "so" "far" "away"))

(formulate '((troubles o-1) (speaker-possession o-1) (complete-set o-1) (appear-as ev o-1 o-2) (before ev moment-of-speaking) (day-before ev moment-of-speaking) (moment-of-speaking moment-of-speaking now) (distant o-2) (far o-2) (to-great-extent o-2)))
