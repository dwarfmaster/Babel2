(in-package :fcg)

;; November 2015, Katrien Beuls
;; Updated November 2016
;; Second update December 2016

;; This is a small fragment of the Spanish verb phrase grammar for
;; regular verbs in the present tense and some examples of the past perfect/imperfect

;; CANTAR (to sing) & COMER (to eat) & VIVIR (to live)
;; canto            & como           & vivo
;; cantas           & comes          & vives
;; canta            & come           & vive
;; cantamos         & comemos        & vivimos
;; cantais          & comeis         & vivis (viviis)
;; cantan           & comen          & viven

;;The verb phrase grammar has two construction sets: CXN and DEFAULT
;;Default cxns are those that should apply if particular other
;;constructions (morphological) could not apply. They ensure a zero
;;marking.

(def-fcg-constructions spanish-vp-grammar
  :feature-types ((args sequence)
                  (footprints set)
                  (boundaries set-of-predicates)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (sg?-1-2-3 sequence)
                  (pl?-1-2-3 sequence)
                  (verb-class-1-2-3 sequence)
                  (constituents set))
  :fcg-configurations ((:production-order lex cxn morph stress irregular-stem stem default)
                       (:parse-order irregular-stem stem lex morph default cxn)
                       (:parse-goal-tests :no-applicable-cxns :no-strings-in-root :connected-structure)
                       (:shuffle-cxns-before-application . t)
                       (:production-goal-tests
                        :no-applicable-cxns :no-meaning-in-root :connected-structure)
                       (:cxn-supplier-mode . :ordered-by-label-and-score )
                       (:de-render-mode . :de-render-with-scope)
                       (:render-mode . :render-with-scope)
                       (:node-tests  :check-duplicate :restrict-search-depth)) ;:update-references
  :hierarchy-features (constituents)
  :cxn-inventory *spanish-verb-conjugation*

;; Constructions for intransitive verbs in all 6 person/numbers:
;;--------------------------------------------------------------

(def-fcg-cxn stress-cxn-1
    ((?stem-unit
      (phon-cat (primary-stress +)))
     <-
     (?verb-unit
      (constituents (?stem-unit ?suffix-unit))
      (form ((meets ?stem-unit ?suffix-unit ?verb-unit)))
      --)
     (?stem-unit
      (syn-cat (lex-class stem))
      --)
     (?suffix-unit
      (syn-cat (not (lex-class tam-morph)))
      (phon-cat (primary-stress -))
      --))
    :cxn-set stress
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn stress-cxn-2
    ((?stem-unit
      (phon-cat (primary-stress +)))
     <-
     (?verb-unit
      (constituents (?stem-unit ?tam-suffix-unit ?pn-suffix-unit))
      (form ((meets ?stem-unit ?tam-suffix-unit ?verb-unit)
             (precedes ?stem-unit ?pn-suffix-unit ?verb-unit)))
      --
      )
     (?stem-unit
      (syn-cat (lex-class stem))
      --
      )
     (?pn-suffix-unit
      (syn-cat (lex-class pn-morph))
      (phon-cat (contains-vowel -)) ;;we have to know if it is a consonant or vowel
      --
      )
     (?tam-suffix-unit
      (syn-cat (lex-class tam-morph))
      (phon-cat (not (primary-stress +)))
      --
      ))
    :cxn-set stress
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn intransitive-1sg-covert-subject-cxn
    ((?intrans-clause-unit
      (referent ?event)
      (args (?event ?agent))
      (constituents (?unmarked-subject-unit ?verb-unit)) ;;prodrop
      (syn-cat (phrase-type VP)
               (syn-valence (subject ?unmarked-subject-unit))
               (agreement (sg?-1-2-3 (+ + - -))
                          (pl?-1-2-3 (- - - -))))
      (sem-cat (sem-function predicating-expression)
               (sem-roles (agent ?agent))))
     (?unmarked-subject-unit
      (syn-cat (syn-role subject)
               (agreement (sg?-1-2-3 (+ + - -))
                          (pl?-1-2-3 (- - - -)))
               (marked -)))
     <-
     (?verb-unit
      (referent ?event)
      (args (?event ?agent))
      (HASH meaning ((person me ?agent)))
      --
      (syn-cat (lex-class verb)
               (agreement (sg?-1-2-3 (+ + - -))
                          (pl?-1-2-3 (- - - -))))))
    :cxn-set cxn)

(def-fcg-cxn intransitive-2sg-covert-subject-cxn
    ((?intrans-clause-unit
      (referent ?event)
      (args (?event ?agent))
      (constituents (?verb-unit ?unmarked-subject-unit)) ;;prodrop
      (syn-cat (phrase-type VP)
               (syn-valence (subject ?unmarked-subject-unit))
               (agreement (sg?-1-2-3 (+ - + -))
                          (pl?-1-2-3 (- - - -))))
      (sem-cat (sem-function predicating-expression)
               (sem-roles (agent ?agent))))
     (?unmarked-subject-unit
      (syn-cat (syn-role subject)
               (agreement (sg?-1-2-3 (+ - + -))
                          (pl?-1-2-3 (- - - -)))
               (marked -)))
     <-
     (?verb-unit
      (referent ?event)
      (args (?event ?agent))
      (HASH meaning ((person you ?agent)
                     (quantity singleton ?agent)))
      --
      (syn-cat (lex-class verb)
               (agreement (sg?-1-2-3 (+ - + -))
                          (pl?-1-2-3 (- - - -))))))
    :cxn-set cxn)

(def-fcg-cxn intransitive-3sg-covert-subject-cxn
    ((?intrans-clause-unit
      (referent ?event)
      (args (?event ?agent))
      (constituents (?verb-unit ?unmarked-subject-unit)) ;;prodrop
      (syn-cat (phrase-type VP)
               (syn-valence (subject ?unmarked-subject-unit))
               (agreement (sg?-1-2-3 (+ - - +))
                          (pl?-1-2-3 (- - - -))))
      (sem-cat (sem-function predicating-expression)
               (sem-roles (agent ?agent))))
     (?unmarked-subject-unit
      (syn-cat (syn-role subject)
               (agreement (sg?-1-2-3 (+ - - +))
                          (pl?-1-2-3 (- - - -)))
               (marked -)))
     <-
     (?verb-unit
      (referent ?event)
      (args (?event ?agent))
      (HASH meaning (( person he-or-she ?agent)))
      --
      (syn-cat (lex-class verb)
               (agreement (sg?-1-2-3 (+ - - +))
                          (pl?-1-2-3 (- - - -))))))
    :cxn-set cxn)

(def-fcg-cxn intransitive-1pl-covert-subject-cxn
    ((?intrans-clause-unit
      (referent ?event)
      (args (?event ?agent))
      (constituents (?verb-unit ?unmarked-subject-unit)) ;;prodrop
      (syn-cat (phrase-type VP)
               (syn-valence (subject ?unmarked-subject-unit))
               (agreement (sg?-1-2-3 (- - - -))
                          (pl?-1-2-3 (+ + - -))))
      (sem-cat (sem-function predicating-expression)
               (sem-roles (agent ?agent))))
     (?unmarked-subject-unit
      (syn-cat (syn-role subject)
               (agreement (sg?-1-2-3 (- - - -))
                          (pl?-1-2-3 (+ + - -)))
               (marked -)))
     <-
     (?verb-unit
      (referent ?event)
      (args (?event ?agent))
      (HASH meaning ((person we ?agent)))
      --
      (syn-cat (lex-class verb)
               (agreement (sg?-1-2-3 (- - - -))
                          (pl?-1-2-3 (+ + - -))))))
    :cxn-set cxn)

(def-fcg-cxn intransitive-2pl-covert-subject-cxn
    ((?intrans-clause-unit
      (referent ?event)
      (args (?event ?agent))
      (constituents (?verb-unit ?unmarked-subject-unit)) ;;prodrop
      (syn-cat (phrase-type VP)
               (syn-valence (subject ?unmarked-subject-unit))
               (agreement (sg?-1-2-3 (- - - -))
                          (pl?-1-2-3 (+ - + -))))
      (sem-cat (sem-function predicating-expression)
               (sem-roles (agent ?agent))))
     (?unmarked-subject-unit
      (syn-cat (syn-role subject)
               (agreement (sg?-1-2-3 (- - - -))
                          (pl?-1-2-3 (+ - + -)))
               (marked -)))
     <-
     (?verb-unit
      (referent ?event)
      (args (?event ?agent))
      (HASH meaning ((person you ?agent)
                     (quantity group ?agent)))
      --
      (syn-cat (lex-class verb)
               (agreement (sg?-1-2-3 (- - - -))
                          (pl?-1-2-3 (+ - + -))))))
    :cxn-set cxn)

(def-fcg-cxn intransitive-3pl-covert-subject-cxn
    ((?intrans-clause-unit
      (referent ?event)
      (args (?event ?agent))
      (constituents (?unmarked-subject-unit ?verb-unit)) ;;prodrop
      (syn-cat (phrase-type VP) ;;or S?
               (syn-valence (subject ?unmarked-subject-unit))
               (agreement (sg?-1-2-3 (- - - -))
                          (pl?-1-2-3 (+ - - +))))
      (sem-cat (sem-function predicating-expression)
               (sem-roles (agent ?agent))))
     (?unmarked-subject-unit
      (syn-cat (syn-role subject)
               (agreement (sg?-1-2-3 (- - - -))
                          (pl?-1-2-3 (+ - - +)))
               (marked -)))
     <-
     (?verb-unit
      (referent ?event)
      (args (?event ?agent))
      (HASH meaning ((person they ?agent)))
      --
      (syn-cat (lex-class verb)
               (agreement (sg?-1-2-3 (- - - -))
                          (pl?-1-2-3 (+ - - +))))))
    :cxn-set cxn)

;; Constructions for the present and past tense:
;;----------------------------------------------

(def-fcg-cxn present-indicative-cxn
             ((?verb-unit
               (footprints (tense-aspect-mood)))
              <-
              (?verb-unit
               (referent ?event)
             ;  (args (?point ?event))
               (syn-cat (lex-class verb))
               (HASH meaning ((time-point present-point ?point)
                              (simultaneous ?point ?event)))
               (footprints (not tense-aspect-mood))
               --
               (footprints (not tense-aspect-mood))
               (syn-cat (lex-class verb)
                        (tense (present +) (past -) (future -))
                        (aspect (perfective ?pf) (imperfective ?impf))
                        (mood (indicative +) (subjunctive -)))))
             :cxn-set cxn
             :disable-automatic-footprints t)

(def-fcg-cxn present-subjunctive-cxn
             ((?verb-unit
               (footprints (tense-aspect-mood)))
              <-
              (?verb-unit
               (referent ?event)
             ;  (args (?point ?event))
               (syn-cat (lex-class verb))
               (HASH meaning ((time-point present-point ?point)
                              (simultaneous ?point ?event)
                              (event-mood subjunctive ?event)))
               (footprints (not tense-aspect-mood))
               --
               (footprints (not tense-aspect-mood))
               (syn-cat (lex-class verb)
                        (tense (present +) (past -) (future -))
                        (aspect (perfective ?pf) (imperfective ?impf))
                        (mood (indicative -) (subjunctive +)))))
             :cxn-set cxn
             :disable-automatic-footprints t)

(def-fcg-cxn past-imperfective-indicative-cxn
             ((?verb-unit
               (footprints (tense-aspect-mood)))
              <-
              (?verb-unit
               (referent ?event)
             ;  (args (?point ?event))
               (syn-cat (lex-class verb))
               (HASH meaning ((time-point recalled-point ?point)
                              (simultaneous ?point ?event)
                              (event-perspective unbound ?event )))
               (footprints (not tense-aspect-mood))
               --
               (footprints (not tense-aspect-mood))
               (syn-cat (lex-class verb)
                        (tense (present -) (past +) (future -))
                        (aspect (perfective -) (imperfective +))
                        (mood (indicative +) (subjunctive -)))))
             :cxn-set cxn
             :disable-automatic-footprints t)

(def-fcg-cxn past-perfective-indicative-cxn
             ((?verb-unit
               (footprints (tense-aspect-mood)))
              <-
              (?verb-unit
               (referent ?event)
             ;  (args (?point ?event))
               (syn-cat (lex-class verb))
               (HASH meaning ((time-point recalled-point ?point)
                              (simultaneous ?point ?event)
                              (event-perspective bound ?event)))
               (footprints (not tense-aspect-mood))
               --
               (footprints (not tense-aspect-mood))
               (syn-cat (lex-class verb)
                        (tense (present -) (past +) (future -))
                        (aspect (perfective +) (imperfective -))
                        (mood (indicative +) (subjunctive -)))))
             :cxn-set cxn
             :disable-automatic-footprints t)

(def-fcg-cxn future-perfect-indicative-cxn
             ((?verb-unit
               (footprints (tense-aspect-mood)))
              <-
              (?verb-unit
               (referent ?event)
               ;(args (?anchor-point ?event-point ?event))
               (syn-cat (lex-class verb))
               (HASH meaning ((time-point present-point ?anchor-point)
                              (anticipating ?anchor-point ?event-point)
                              (time-point anticipated-point ?event-point)
                              (anterior ?event-point ?event)))
               (footprints (not tense-aspect-mood))
               --
               (footprints (not tense-aspect-mood))
               (syn-cat (lex-class verb)
                        (tense (present -) (past -) (future +))
                        (aspect (perfective +) (imperfective -)) ;;perfect is not the same as perfective
                        (mood (indicative +) (subjunctive -)))))
             :cxn-set cxn
             :disable-automatic-footprints t)

(def-fcg-cxn conditional-perfect-indicative-cxn
             ((?verb-unit
               (footprints (tense-aspect-mood)))
              <-
              (?verb-unit
               (referent ?event)
              ; (args (?anchor-point ?event-point ?event))
               (syn-cat (lex-class verb))
               (HASH meaning ((time-point anticipated-point ?anchor-point)
                              (anticipating ?anchor-point ?event-point)
                              (time-point anticipated-point ?event-point)
                              (anterior ?event-point ?event)))
               (footprints (not tense-aspect-mood))
               --
               (footprints (not tense-aspect-mood))
               (syn-cat (lex-class verb)
                        (tense (present -) (past +) (future -))
                        (aspect (perfective +) (imperfective -)) ;;perfect is not the same as perfective
                        (mood (indicative +) (subjunctive -)))))
             :cxn-set cxn
             :disable-automatic-footprints t)


)


;; TO DO
;; Write segmentation function (based on stems and suffixes in the grammar, this is trivial)
;; >> Write own de-render method
;; Evaluate meaning separately



;;PREVIOUS VERSIONS

;;; (def-fcg-cxn ser-soy-lex
;;;              ((?ser-stem
;;;                (syn-cat (lex-class verb)
;;;                         (verb-class irregular)
;;;                         (agreement (sg?-1-2-3 (+ + - -))
;;;                                    (pl?-1-2-3 (- - - -)))
;;;                         (lemma "ser")
;;;                         (tense (past -) (present +) (future -))
;;;                         (aspect (imperfective +) (perfective -))
;;;                         (mood (indicative +) (subjunctive -)))
;;;                (sem-cat (class state))
;;;                (args (?ev))
;;;                (referent ?ev))
;;;               <-
;;;               (?ser-stem
;;;                (HASH meaning ((be-permanent ?state)
;;;                               (time-point ?state ?ref present-point)
;;;                               (relation ?state ?ref simultaneous)
;;;                               (act-done-by-me ?state ?p)))
;;;                --
;;;                (HASH form ((string ?ser-stem "soy")))))
;;;              :cxn-set cxn)
;;; ;Stem0: viv


;;; ;Stem2: stem1 + "d"
;;; (def-fcg-cxn participle-morph
;;;     ((?participle-unit
;;;       (constituents (?base-unit ?sub-unit ?d-unit)))
;;;      (?d-unit
;;;       (syn-cat (lex-class tam-morph)))
;;;      (?base-unit
;;;       (syn-cat (finite -))
;;;       (phon-cat (theme-vowel ?some-vowel)))
;;;      <-
;;;      (?base-unit
;;;       (phon-cat (theme-vowel ?sub-unit))
;;;       (syn-cat (not (finite ?fin))
;;;                (not (agreement ?agr-matrix)))
;;;       --
;;;       (phon-cat (theme-vowel ?sub-unit))
;;;       (syn-cat (root +)))
;;;      (?d-unit
;;;       --
;;;       (HASH form ((string ?d-unit "d")
;;;                   (meets ?sub-unit ?d-unit)))))
;;;     :cxn-set morph
;;;     :score 0.6)

;;; ;Stem3: stem1 + "r"
;;; (def-fcg-cxn infinitive-morph
;;;     ((?theme-vowel-unit
;;;       (constituents (?r-unit)))
;;;      (?r-unit
;;;       (syn-cat (lex-class tam-morph)))
;;;      (?base-unit
;;;       (syn-cat (finite -))
;;;       )
;;;      <-
;;;      (?base-unit
;;;       (syn-cat (not (finite ?fin))
;;;                (not (agreement ?agr-matrix)))
;;;       (phon-cat (theme-vowel ?sub-unit))
;;;       --
;;;       (phon-cat (theme-vowel ?sub-unit))
;;;       (syn-cat (root +)))
;;;      (?theme-vowel-unit
;;;       (syn-cat (lex-class tam-morph))
;;;       --
;;;       (syn-cat (lex-class tam-morph)))
;;;      (?r-unit
;;;       --
;;;       (HASH form ((string ?r-unit "r")
;;;                   (meets ?sub-unit ?r-unit)))))
;;;     :cxn-set morph
;;;     :score 0.6)



;;; )


