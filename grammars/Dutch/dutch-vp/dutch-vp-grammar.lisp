;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grammar of Dutch VP ;;
;; (Paul)              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :dutch-vp)

(def-fcg-constructions dutch-vp-grammar
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits set))
  :fcg-configurations ((:production-order lex vp modality tense morph)
                        (:parse-order morph lex vp modality tense )
                        (:parse-goal-tests :no-applicable-cxns :connected-structure) 
                        (:production-goal-tests :no-applicable-cxns :connected-structure)
                        (:cxn-supplier-mode . :ordered-by-label-and-score)
                        (:node-tests :check-duplicate :restrict-search-depth :no-morphs-if-unconnected-structure))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;                                           ;;;;
  ;;;; LEXICAL AND MORPHOLOLOGICAL CONSTRUCTIONS ;;;;
  ;;;;                                           ;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Lexical constructions (main verbs)
  ;; ----------------------------------

  (def-fcg-cxn zingen-lex 
               ((?word
                 (sem-cat (class action)
                          (sem-function event))
                 (args (?ev)))
                <-
                (?word
                 (HASH meaning ((event sing ?ev)))
                 --
                 (syn-cat (lex-class verb)
                          (lemma "zingen")
                          (perfect-aux "hebben")
                          (verb-form ?verb-form)
                          (tense ?tense))))
               :cxn-set lex)

  (def-fcg-cxn gaan-lex 
               ((?word
                 (sem-cat (class action)
                           (sem-function event))
                (args (?ev)))
                <-
                (?word
                 (HASH meaning ((event go ?ev)))
                 --
                 (syn-cat (lex-class verb)
                           (lemma "gaan")
                           (perfect-aux "zijn")
                           (verb-form ?verb-form)
                           (tense ?tense))))
               :cxn-set lex)
 
 ;; Morphological constructions (for main verbs)
  ;; ----------------------------------------
         
  (def-fcg-cxn zingt-morph 
               (<-
                (?word
                 (syn-cat (lex-class verb)
                          (lemma "zingen")
                          (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (perfect-aux ?perfect-aux)
                          (tense (present +)
                                 (past -)))
                 --
                 (HASH form ((string ?word "zingt")))))
               :cxn-set morph)
  
  (def-fcg-cxn zong-morph 
               (<-
                (?word
                 (syn-cat (lex-class verb)
                          (lemma "zingen")
                          (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (perfect-aux ?perfect-aux)
                          (tense (present -)
                                 (past +)))
                 --
                 (HASH form ((string ?word "zong")))))
               :cxn-set morph)

  (def-fcg-cxn gezongen-morph 
               (<-
                (?word
                 (syn-cat (lex-class verb)
                          (lemma "zingen")
                          (verb-form (finite -)
                                     (infinitive -)
                                     (perfect-form +))
                (perfect-aux ?perfect-aux)
                (tense ?tense))
                 --
                 (HASH form ((string ?word "gezongen")))))
               :cxn-set morph)

  (def-fcg-cxn zingen-morph 
               (<-
                (?word
                 (syn-cat (lex-class verb)
                          (lemma "zingen")
                          (verb-form (finite -)
                                     (infinitive +)
                                     (perfect-form -))
                          (perfect-aux ?perfect-aux)
                          (tense ?tense))
                 --
                 (HASH form ((string ?word "zingen")))))
               :cxn-set morph)

    (def-fcg-cxn gaat-morph 
               (<-
                (?word
                 (syn-cat (lex-class verb)
                          (lemma "gaan")
                          (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (perfect-aux ?perfect-aux)
                          (tense (present +)
                                 (past -)))
                 --
                 (HASH form ((string ?word "gaat")))))
               :cxn-set morph)
  
  (def-fcg-cxn ging-morph 
               (<-
                (?word
                 (syn-cat (lex-class verb)
                          (lemma "gaan")
                          (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (perfect-aux ?perfect-aux)
                          (tense (present -)
                                 (past +)))
                 --
                 (HASH form ((string ?word "ging")))))
               :cxn-set morph)

  (def-fcg-cxn gegaan-morph 
               (<-
                (?word
                 (syn-cat (lex-class verb)
                          (lemma "gaan")
                          (verb-form (finite -)
                                     (infinitive -)
                                     (perfect-form +))
                (perfect-aux ?perfect-aux)
                (tense ?tense))
                 --
                 (HASH form ((string ?word "gegaan")))))
               :cxn-set morph)

  (def-fcg-cxn gaan-morph 
               (<-
                (?word
                 (syn-cat (lex-class verb)
                          (lemma "gaan")
                          (verb-form (finite -)
                                     (infinitive +)
                                     (perfect-form -))
                          (perfect-aux ?perfect-aux)
                          (tense ?tense))
                 --
                 (HASH form ((string ?word "gaan")))))
               :cxn-set morph)






  ;; Abstract lexical construction (for perfect auxiliaries)
  ;; -------------------------------------------------------

  (def-fcg-cxn perfect-aux-lex 
               ((?word
                 (sem-cat (class perfect-event)
                           (sem-function event)
                           (action-focus ?action-focus))
                (args (?super-event ?event)))
                <-
                (?word
                 (HASH meaning ((aspect perfect ?event ?super-event)
                                (focus action ?action-focus ?super-event)))
                 --
                 (syn-cat (lex-class perfect-aux)
                           (lemma ?lemma)
                           (verb-form ?verb-form)
                           (tense ?tense))))
               :cxn-set lex)
  
  ;; Morphological constructions (for perfect auxiliaries)
  ;; ------------------------------------------------------

  (def-fcg-cxn heeft-aux-morph 
               (<-
                (?word
                 (syn-cat (lex-class perfect-aux)
                          (lemma "hebben")
                          (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (tense (present +)
                                 (past -)))
                 --
                 (HASH form ((string ?word "heeft")))))
               :cxn-set morph)

  (def-fcg-cxn had-aux-morph 
               (<-
                (?word
                 (syn-cat (lex-class perfect-aux)
                          (lemma "hebben")
                          (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (tense (present -)
                                 (past +)))
                 --
                 (HASH form ((string ?word "had")))))
               :cxn-set morph)

  (def-fcg-cxn hebben-aux-morph 
               (<-
                (?word
                 (syn-cat (lex-class perfect-aux)
                          (lemma "hebben")
                          (verb-form (finite -)
                                     (infinitive +)
                                     (perfect-form -))
                          (tense ?tense))
                 --
                 (HASH form ((string ?word "hebben")))))
               :cxn-set morph)

  (def-fcg-cxn is-aux-morph 
               (<-
                (?word
                 (syn-cat (lex-class perfect-aux)
                          (lemma "zijn")
                          (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (tense (present +)
                                 (past -)))
                 --
                 (HASH form ((string ?word "is")))))
               :cxn-set morph)

  (def-fcg-cxn was-aux-morph 
               (<-
                (?word
                 (syn-cat (lex-class perfect-aux)
                          (lemma "zijn")
                          (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (tense (present -)
                                 (past +)))
                 --
                 (HASH form ((string ?word "was")))))
               :cxn-set morph)

  (def-fcg-cxn zijn-aux-morph 
               (<-
                (?word
                 (syn-cat (lex-class perfect-aux)
                          (lemma "zijn")
                          (verb-form (finite -)
                                     (infinitive +)
                                     (perfect-form -))
                          (tense ?tense))
                 --
                 (HASH form ((string ?word "zijn")))))
               :cxn-set morph)

  
  ;; Lexical constructions (for modal auxiliaries)
  ;; ---------------------------------------------

  (def-fcg-cxn moeten-aux-lex 
               ((?word
                 (sem-cat (class modality)
                          (sem-function event))
                (args (?super-event ?event)))
                <-
                (?word
                 (HASH meaning ((modality obligation ?event ?super-event)))
                 --
                 (syn-cat (lex-class modal-aux)
                           (lemma "moeten")
                           (verb-form ?verb-form)
                           (tense ?tense))))
               :cxn-set lex)

  (def-fcg-cxn kunnen-aux-lex 
               ((?word
                 (sem-cat (class modality)
                          (sem-function event))
                (args (?super-event ?event)))
                <-
                (?word
                 (HASH meaning ((modality ability ?event ?super-event)))
                 --
                 (syn-cat (lex-class modal-aux)
                           (lemma "kunnen")
                           (verb-form ?verb-form)
                           (tense ?tense))))
               :cxn-set lex)

  (def-fcg-cxn laten-aux-lex 
               ((?word
                 (sem-cat (class modality)
                          (sem-function event))
                (args (?super-event ?event)))
                <-
                (?word
                 (HASH meaning ((modality causation ?event ?super-event)))
                 --
                 (syn-cat (lex-class modal-aux)
                           (lemma "laten")
                           (verb-form ?verb-form)
                           (tense ?tense))))
               :cxn-set lex)

  (def-fcg-cxn zullen-aux-lex 
               ((?word
                 (sem-cat (class modality)
                          (sem-function event))
                (args (?super-event ?event)))
                <-
                (?word
                 (HASH meaning ((modality hypothesis ?event ?super-event)))
                 --
                 (syn-cat (lex-class modal-aux)
                           (lemma "zullen")
                           (verb-form ?verb-form)
                           (tense ?tense))))
               :cxn-set lex)


  ;; Morphological constructions (for modal auxiliaries)
  ;; ---------------------------------------------------    

  (def-fcg-cxn moet-aux-morph 
               (<-
                (?word
                 (syn-cat (lex-class modal-aux)
                          (lemma "moeten")
                          (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (tense (present +)
                                 (past -)))
                 --
                 (HASH form ((string ?word "moet")))))
               :cxn-set morph)
  
  (def-fcg-cxn moest-aux-morph 
               (<-
                (?word
                 (syn-cat (lex-class modal-aux)
                          (lemma "moeten")
                          (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (tense (present -)
                                 (past +)))
                 --
                 (HASH form ((string ?word "moest")))))
               :cxn-set morph)

 (def-fcg-cxn moeten-aux-morph 
               (<-
                (?word
                 (syn-cat (lex-class modal-aux)
                          (lemma "moeten")
                          (verb-form (finite -)
                                     (infinitive +)
                                     (perfect-form +))
                          (tense ?tense))
                 --
                 (HASH form ((string ?word "moeten")))))
               :cxn-set morph) 

   (def-fcg-cxn kan-aux-morph 
               (<-
                (?word
                 (syn-cat (lex-class modal-aux)
                          (lemma "kunnen")
                          (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (tense (present +)
                                 (past -)))
                 --
                 (HASH form ((string ?word "kan")))))
               :cxn-set morph)
  
  (def-fcg-cxn kon-aux-morph 
               (<-
                (?word
                 (syn-cat (lex-class modal-aux)
                          (lemma "kunnen")
                          (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (tense (present -)
                                 (past +)))
                 --
                 (HASH form ((string ?word "kon")))))
               :cxn-set morph)

 (def-fcg-cxn kunnen-aux-morph 
               (<-
                (?word
                 (syn-cat (lex-class modal-aux)
                          (lemma "kunnen")
                          (verb-form (finite -)
                                     (infinitive +)
                                     (perfect-form +))
                          (tense ?tense))
                 --
                 (HASH form ((string ?word "kunnen")))))
               :cxn-set morph) 

   (def-fcg-cxn laat-aux-morph 
               (<-
                (?word
                 (syn-cat (lex-class modal-aux)
                          (lemma "laten")
                          (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (tense (present +)
                                 (past -)))
                 --
                 (HASH form ((string ?word "laat")))))
               :cxn-set morph)
  
  (def-fcg-cxn liet-aux-morph 
               (<-
                (?word
                 (syn-cat (lex-class modal-aux)
                          (lemma "laten")
                          (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (tense (present -)
                                 (past +)))
                 --
                 (HASH form ((string ?word "liet")))))
               :cxn-set morph)

 (def-fcg-cxn laten-aux-morph 
               (<-
                (?word
                 (syn-cat (lex-class modal-aux)
                          (lemma "laten")
                          (verb-form (finite -)
                                     (infinitive +)
                                     (perfect-form +))
                          (tense ?tense))
                 --
                 (HASH form ((string ?word "laten")))))
               :cxn-set morph) 

  (def-fcg-cxn zal-aux-morph 
               (<-
                (?word
                 (syn-cat (lex-class modal-aux)
                          (lemma "zullen")
                          (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (tense (present +)
                                 (past -)))
                 --
                 (HASH form ((string ?word "zal")))))
               :cxn-set morph)
  
  (def-fcg-cxn zou-aux-morph 
               (<-
                (?word
                 (syn-cat (lex-class modal-aux)
                          (lemma "zullen")
                          (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (tense (present -)
                                 (past +)))
                 --
                 (HASH form ((string ?word "zou")))))
               :cxn-set morph)  


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;                                           ;;;;
  ;;;;         GRAMMATICAL CONSTRUCTIONS         ;;;;
  ;;;;                                           ;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
  ;; Verb Phrase Construction
  ;; ------------------------
  ;; MAIN-VERB --> VERB-PHRASE
  
(def-fcg-cxn vp-cxn 
               ((?vp-unit
                 (sem-cat (sem-function predicating-expression)
                          (origo ?origo)
                          (action-focus ?action-focus))
                 (syn-cat (phrase-type vp)
                           (perfect-aux ?perfect-aux)
                           (tense ?tense)
                           (verb-form ?verb-form)
                           (left-most-subunit ?main-verb-unit))
                 (args (?ev ?origo))
                 (subunits (?main-verb-unit)))
                <-
                (?main-verb-unit
                 (sem-cat (class action))
                 (args (?ev))
                 --
                 (syn-cat (lex-class verb)
                           (perfect-aux ?perfect-aux)
                           (verb-form ?verb-form)
                           (tense ?tense))))
               :cxn-set vp)

  ;; Aspect constructions
  ;; --------------------
  ;; PERFECT-AUX VERB-PHRASE --> VERB-PHRASE
  
  (def-fcg-cxn perfect-cxn 
               ((?new-vp-unit
                 (sem-cat (sem-function predicating-expression)
                          (action-focus -))
                 (syn-cat (phrase-type vp)
                          (perfect-aux ?perfect-aux)
                          (tense ?tense)
                          (verb-form ?verb-form)
                          (left-most-subunit ?aux-unit))
                 (args (?super-event ?origo))
                 (subunits (?aux-unit ?vp-unit)))
                (?vp-unit
                 (sem-cat
                  (part-of-phrase +)))
                <-
                (?aux-unit
                 (sem-cat (class perfect-event)
                          (action-focus -)
                          (NOT (part-of-phrase +)))
                 (args (?super-event ?event))
                 --
                 (syn-cat (lex-class perfect-aux)
                          (lemma ?perfect-aux)
                          (tense ?tense)
                          (verb-form ?verb-form)))
                (?vp-unit
                 (sem-cat (sem-function predicating-expression)
                          (action-focus -)
                          (NOT (part-of-phrase +)))
                 (args (?event ?origo))
                 --
                 (syn-cat (phrase-type vp)
                          (perfect-aux ?perfect-aux)
                          (verb-form (finite -)
                                     (infinitive ?infinitive)
                                     (perfect-form +))
                          (left-most-subunit ?left-most-subunit-of-vp))
                 (HASH form ((meets ?aux-unit ?left-most-subunit-of-vp)))))
               :cxn-set modality)

(def-fcg-cxn inversed-perfect-cxn 
               ((?new-vp-unit
                 (sem-cat (sem-function predicating-expression)
                          (action-focus -))
                 (syn-cat (phrase-type vp)
                          (perfect-aux ?perfect-aux)
                          (tense ?tense)
                          (verb-form ?verb-form)
                          (left-most-subunit ?past-participle))
                 (args (?super-event ?origo))
                 (subunits (?aux-unit ?vp-unit)))
                (?aux-unit
                 (sem-cat
                  (part-of-phrase +)))
                (?vp-unit
                 (sem-cat
                  (part-of-phrase +)))
                <-
                (?aux-unit
                 (sem-cat (class perfect-event)
                          (action-focus -)
                          (NOT (part-of-phrase +)))
                 (args (?super-event ?event))
                 --
                 (syn-cat (lex-class perfect-aux)
                          (lemma ?perfect-aux)
                          (tense ?tense)
                          (verb-form ?verb-form)))
                (?past-participle
                 (sem-cat (class action)
                          (sem-function event))
                 --
                 (syn-cat (verb-form (finite -)
                                     (infinitive -)
                                     (perfect-form +))))
                (?vp-unit
                 (sem-cat (sem-function predicating-expression)
                          (action-focus -)
                          (NOT (part-of-phrase +)))
                 (args (?event ?origo))
                 (subunits (?past-participle))
                 --
                 (syn-cat (phrase-type vp)
                          (perfect-aux ?perfect-aux)
                          (verb-form (finite -)
                                     (infinitive -)
                                     (perfect-form +))
                          (left-most-subunit ?past-participle))
                 (HASH form ((meets ?past-participle ?aux-unit)))))
               :cxn-set modality
               :score 0.4)
  
  (def-fcg-cxn perfect-pp-in-front-cxn 
               ((?new-vp-unit
                 (sem-cat (sem-function predicating-expression)
                          (action-focus +))
                 (syn-cat (phrase-type vp)
                          (perfect-aux ?perfect-aux)
                          (tense ?tense)
                          (verb-form (finite -)
                                     (infinitive +)
                                     (perfect-form -))
                           (left-most-subunit ?aux-unit))
                 (subunits (?aux-unit ?vp))
                 (args (?super-event ?origo)))
                (?vp
                 (sem-cat (part-of-phrase +)))
                <-
                (?vp
                 (sem-cat (sem-function predicating-expression)
                          (action-focus +)
                          (NOT (part-of-phrase +)))
                 (args (?event ?origo))
                 (subunits (?past-participle-unit))
                 --
                 (syn-cat  (phrase-type vp)
                           (perfect-aux ?perfect-aux)
                           (verb-form  (finite -)
                                       (infinitive ?infinitive)
                                       (perfect-form +))
                           (left-most-subunit ?left-most-subunit-of-vp))
                 (subunits (?past-participle-unit)))
                (?past-participle-unit
                 (sem-cat  (class action))
                 --
                 (syn-cat (lex-class verb)
                          (verb-form (finite -)
                                     (infinitive -)
                                     (perfect-form +))))
                (?aux-unit
                 (sem-cat (class perfect-event)
                          (action-focus +))
                 (args (?super-event ?event))
                 --
                 (syn-cat  (lex-class perfect-aux)
                           (lemma ?perfect-aux)
                           (tense ?tense)
                           (verb-form (finite -)
                                           (infinitive +)
                                           (perfect-form -))))
                (?new-vp-unit
                 --
                 (HASH form ((precedes ?left-most-subunit-of-vp ?aux-unit)))))
               :cxn-set modality)

 ;; Modality constructions
  ;; ----------------------
  ;; MODAL-AUX VERB-PHRASE --> NEW-VERB-PHRASE

  (def-fcg-cxn modality-cxn 
               ((?new-vp-unit
                 (sem-cat (sem-function predicating-expression)
                          (action-focus ?action-focus))
                 (syn-cat (phrase-type vp)
                          (perfect-aux ?perfect-aux)
                          (tense ?tense)
                          (verb-form ?verb-form)
                          (left-most-subunit ?aux-unit))
                 (args (?super-event ?origo))
                (subunits (?aux-unit ?vp-unit)))
                (?vp-unit
                 (sem-cat (part-of-phrase +)))
                <-
                (?aux-unit
                 (sem-cat (class modality))
                 (args (?super-event ?event))
                 --
                 (syn-cat (lex-class modal-aux)
                          (tense ?tense)
                          (verb-form ?verb-form)))
                (?vp-unit
                 (sem-cat (sem-function predicating-expression)
                          (action-focus ?action-focus)
                          (NOT (part-of-phrase +)))
                 (args (?event ?origo))
                 --
                 (syn-cat  (phrase-type vp)
                           (perfect-aux ?perfect-aux)
                           (verb-form  (finite -)
                                       (infinitive +)
                                       (perfect-form ?perfect-form))
                           (left-most-subunit ?left-most-subunit-of-vp))
                 (HASH form ((meets ?aux-unit ?left-most-subunit-of-vp)))))
               :cxn-set modality)

  (def-fcg-cxn inversed-perfect-modality-cxn 
               ((?new-vp-unit
                 (sem-cat (sem-function predicating-expression))
                 (syn-cat (phrase-type vp)
                          (perfect-aux ?perfect-aux)
                          (tense ?tense)
                          (verb-form ?verb-form)
                          (left-most-subunit ?aux-unit))
                 (args (?super-event ?origo))
                (subunits (?aux-unit ?vp-unit)))
                (?vp-unit
                 (sem-cat (part-of-phrase +)))
                <-
                (?aux-unit
                 (sem-cat (class modality))
                 (args (?super-event ?event))
                 --
                 (syn-cat (lex-class modal-aux)
                          (tense ?tense)
                          (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))))
                (?past-participle-unit
                 (sem-cat (class action)
                          (sem-function event))
                 --
                 (syn-cat (lex-class verb)
                          (verb-form (finite -)
                                     (infinitive -)
                                     (perfect-form +))))
                (?perfect-aux-unit
                 (sem-cat (class perfect-event)
                          (action-focus +))
                 --
                 (syn-cat (lex-class perfect-aux)))
                (?vp-unit
                 (sem-cat (sem-function predicating-expression)
                          (NOT (part-of-phrase +))
                          (action-focus +))
                 (args (?event ?origo))
                 --
                 (syn-cat  (phrase-type vp)
                           (perfect-aux ?perfect-aux)
                           (verb-form  (finite -)
                                       (infinitive +)
                                       (perfect-form ?perfect-form))
                           (left-most-subunit ?left-most-subunit-of-vp))
                 (HASH form ((meets ?aux-unit ?past-participle-unit)
                             (meets ?past-participle-unit ?left-most-subunit-of-vp)))))
               :cxn-set modality
               :score 0.6)
  
  ;; Tense constructions
  ;; -------------------

  (def-fcg-cxn present-cxn 
               (<-
                (?vp
                 (sem-cat (sem-function predicating-expression)
                          (NOT (part-of-phrase +)))
                 (args (?ev ?origo))
                 (subunits (?finite-verb))
                 (HASH meaning ((time-point deictic ?origo) 
                                (time-relation overlaps ?ev ?origo)))
                 --
                 (syn-cat (phrase-type vp)
                          (left-most-subunit ?finite-verb)))
                (?finite-verb
                 (sem-cat (sem-function event))
                 --
                 (syn-cat (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (tense (present +)
                                 (past -)))))
               :cxn-set tense)

  (def-fcg-cxn past-cxn 
               (<-
                (?vp
                 (sem-cat (sem-function predicating-expression)
                          (NOT (part-of-phrase +)))
                 (args (?ev ?origo))
                 (HASH meaning ((time-point deictic ?origo) 
                                (time-relation before ?ev ?origo)))
                 (subunits (?finite-verb))
                 --
                 (syn-cat (phrase-type vp)
                          (left-most-subunit ?finite-verb)))
                (?finite-verb
                 (sem-cat (sem-function event))
                 --
                 (syn-cat (verb-form (finite +)
                                     (infinitive -)
                                     (perfect-form -))
                          (tense (present -)
                                 (past +)))))
               :cxn-set tense))




;; ##############
;; ## Examples ##
;; ##############

;; (comprehend '("zal" "hebben" "gezongen"))
;; (formulate '((event sing ev) (aspect perfect ev ev-2) (focus action - ev-2) (modality hypothesis ev-2 ev-3) (time-point deictic now) (time-relation overlaps ev-3 now)))

;; (comprehend '("zal" "gezongen" "hebben"))
;; (formulate ' ((event sing ev) (aspect perfect ev ev-2) (focus action + ev-2) (modality hypothesis ev-2 ev-3) (time-point deictic now) (time-relation overlaps ev-3 now)))

;; (comprehend-all '("zal" "moeten" "hebben" "gezongen"))
;; (formulate-all ' ((event sing ev) (aspect perfect ev ev-2) (focus action - ev-2) (modality obligation ev-2 ev-3) (modality hypothesis ev-3 ev-4) (time-point deictic now) (time-relation overlaps ev-4 now)))

;; (comprehend-all '("heeft" "moeten" "zingen"))
;; (formulate-all ' ((event sing ev) (aspect perfect ev-2 ev-3) (focus action - ev-3) (modality obligation ev ev-2) (time-point deictic now) (time-relation overlaps ev-3 now)))

;; (comprehend-all '("heeft" "moeten" "kunnen" "zingen"))
;; (formulate-all ' ((event sing ev) (aspect perfect ev-3 ev-4) (focus action - ev-4) (modality obligation ev-2 ev-3) (modality ability ev ev-2) (time-point deictic now) (time-relation overlaps ev-4 now)))

;; (comprehend-all '("zal" "moeten" "gezongen" "hebben"))
;; (comprehend-all '("zal" "moeten" "hebben" "gezongen"))
;; (comprehend-all '("zal" "gezongen" "moeten" "hebben"))
