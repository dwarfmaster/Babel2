;; Complete refactoring of Russian verbs grammar of Yana Knight for
;; publication in Constructions and Frames
;; Katrien Beuls, December 2016
;;------------------------------------------------------------------

;(asdf:operate 'asdf:load-op :fcg)
(in-package :fcg)

(def-fcg-constructions russian-motion
  :feature-types ((meaning set-of-predicates)
                  (form set-of-predicates)
                  (subunits sequence)
                  (args sequence)
                  (boundaries set-of-predicates)
                  (footprints set))
  :fcg-configurations ((:shuffle-cxns-before-application . t)
                    ;   (:cxn-supplier-mode . :simple-queue)
                       (:parse-order morph unmarked-morph lex cxn)
                       (:production-order lex cxn morph unmarked-morph)
                       (:node-tests :update-references :restrict-search-depth :check-duplicate )
                       (:render-mode . :render-with-scope)
                       (:de-render-mode . :de-render-with-scope)
                       (:parse-goal-tests :connected-structure :no-applicable-cxns :no-strings-in-root)
                       (:production-goal-tests :no-applicable-cxns :no-meaning-in-root))
  :visualization-configurations ((:with-search-debug-data . nil))
  :cxn-inventory *russian-motion-grammar*

(def-fcg-cxn misha-lex
             ((?misha-unit
               (referent ?ref)
               (sem-cat (sem-class person)
                        (sex female))
               (syn-cat (lex-class proper-noun)
                        (gender feminine)
                        (agreement (number singular)
                                   (person 3))))
              <-
              (?misha-unit
               (HASH meaning ((person misha ?ref)))
               --
               (HASH form ((string ?misha-unit "misha")))))
             :cxn-set lex
             :description "A lexical construction for the proper noun Misha.")

(def-fcg-cxn masha-lex
             ((?masha-unit
               (referent ?ref)
               (sem-cat (sem-class person)
                        (sex male))
               (syn-cat (lex-class proper-noun)
                        (gender masculine)
                        (agreement (number singular)
                                   (person 3))))
              <-
              (?masha-unit
               (HASH meaning ((person masha ?ref)))
               --
               (HASH form ((string ?masha-unit "masha")))))
             :cxn-set lex)

  ;;WALK LEX + MORPHS
(def-fcg-cxn walk-lex
             ((?walk-unit
               (args (?ev ?agent))
               (referent ?ev)
               (sem-cat (sem-class event)
                        (verb-class motion)))
              <-
              (?walk-unit
               (HASH meaning ((motion walk ?ev)
                              (walk-agent ?ev ?agent)))
               --
               (syn-cat (lex-id [walk])
                        (lex-class verb))))
             :cxn-set lex)

(def-fcg-cxn xodit-morph
             (<-
              (?xodit-unit
               (syn-cat (lex-id [walk])
                        (lex-class verb)
                        (agreement (number singular)
                                   (person 3)))
               (sem-cat (verb-class motion)
                        (direction (multi +) (uni -)))
               --
               (HASH form ((string ?xodit-unit "xodit")))))
             :cxn-set morph
             :description "This construction maps multidirectional motion verbs with lex-id [walk] into the 'xodit' verb form.")

(def-fcg-cxn idti-morph
             (<-
              (?idti-unit
               (syn-cat (lex-id [walk])
                        (lex-class verb)
                        (agreement (number singular)
                                   (person 3)))
               (sem-cat (verb-class motion)
                        (direction (multi -) (uni +)))
               --
               (HASH form ((string ?idti-unit "idti")))))
             :cxn-set morph)

   ;;RUN LEX + MORPHS
(def-fcg-cxn run-lex
             ((?run-unit
               (args (?ev ?agent))
               (referent ?ev)
               (sem-cat (sem-class event)
                        (verb-class motion)))
              <-
              (?run-unit
               (HASH meaning ((motion run ?ev)
                              (run-agent ?ev ?agent)))
               --
               (syn-cat (lex-id [run])
                        (lex-class verb))))
             :cxn-set lex)

(def-fcg-cxn begat-morph
             (<-
              (?begat-unit
               (syn-cat (lex-id [run])
                        (lex-class verb)
                        (agreement (number singular)
                                   (person 3)))
               (sem-cat (verb-class motion)
                        (direction (multi +) (uni -)))
               --
               (HASH form ((string ?begat-unit "begat")))))
             :cxn-set morph)

(def-fcg-cxn bezat-morph
             (<-
              (?begat-unit
               (syn-cat (lex-id [run])
                        (lex-class verb)
                        (agreement (number singular)
                                   (person 3)))
               (sem-cat (verb-class motion)
                        (direction (multi -) (uni +)))
               --
               (HASH form ((string ?begat-unit "bezat")))))
             :cxn-set morph)

                
(def-fcg-cxn fly-lex
             ((?fly-unit
               (args (?ev ?agent))
               (referent ?ev)
               (sem-cat (sem-class event)
                        (verb-class motion)))
              <-
              (?fly-unit
               (HASH meaning ((motion fly ?ev)
                              (fly-agent ?ev ?agent)))
               --
               (syn-cat (lex-id [fly])
                        (lex-class verb))))
             :cxn-set lex)

(def-fcg-cxn letat-morph
             (<-
              (?letat-unit
               (syn-cat (lex-id [fly])
                        (lex-class verb)
                        (agreement (number singular)
                                   (person 3)))
               (sem-cat (verb-class motion)
                        (direction (multi +) (uni -)))
               --
               (HASH form ((string ?letat-unit "letat")))))
             :cxn-set morph)

(def-fcg-cxn letet-morph
             (<-
              (?letet-unit
               (syn-cat (lex-id [fly])
                        (lex-class verb)
                        (agreement (number singular)
                                   (person 3)))
               (sem-cat (verb-class motion)
                        (direction (multi -) (uni +)))
               --
               (HASH form ((string ?letet-unit "letet")))))
             :cxn-set morph)

(def-fcg-cxn swim-lex
             ((?swim-unit
               (args (?ev ?agent))
               (referent ?ev)
               (sem-cat (sem-class event)
                        (verb-class motion)))
              <-
              (?swim-unit
               (HASH meaning ((motion swim ?ev)
                              (swim-agent ?ev ?agent)))
               --
               (syn-cat (lex-id [swim])
                        (lex-class verb))))
             :cxn-set lex)

(def-fcg-cxn plavat-morph
             (<-
              (?plavat-unit
               (syn-cat (lex-id [swim])
                        (lex-class verb)
                        (agreement (number singular)
                                   (person 3)))
               (sem-cat (verb-class motion)
                        (direction (multi +) (uni -)))
               --
               (HASH form ((string ?plavat-unit "plavat")))))
             :cxn-set morph)

(def-fcg-cxn plit-morph
             (<-
              (?plit-unit
               (syn-cat (lex-id [swim])
                        (lex-class verb)
                        (agreement (number singular)
                                   (person 3)))
               (sem-cat (verb-class motion)
                        (direction (multi -) (uni +)))
               --
               (HASH form ((string ?plit-unit "plit")))))
             :cxn-set morph)

(def-fcg-cxn go-by-car-lex
             ((?go-by-car-unit
               (args (?ev ?agent))
               (referent ?ev)
               (sem-cat (sem-class event)
                        (verb-class motion)))
              <-
              (?go-by-car-unit
               (HASH meaning ((motion drive ?ev)
                              (drive-agent ?ev ?agent)))
               --
               (syn-cat (lex-id [go-by-car])
                        (lex-class verb))))
             :cxn-set lex)

(def-fcg-cxn ezdit-morph
             (<-
              (?ezdit-unit
               (syn-cat (lex-id [go-by-car])
                        (lex-class verb)
                        (agreement (number singular)
                                   (person 3)))
               (sem-cat (verb-class motion)
                        (direction (multi +) (uni -)))
               --
               (HASH form ((string ?ezdit-unit "ezdit")))))
             :cxn-set morph)

(def-fcg-cxn exat-morph
             (<-
              (?exat-unit
               (syn-cat (lex-id [go-by-car])
                        (lex-class verb)
                        (agreement (number singular)
                                   (person 3)))
               (sem-cat (verb-class motion)
                        (direction (multi -) (uni +)))
               --
               (HASH form ((string ?exat-unit "exat")))))
             :cxn-set morph)

(def-fcg-cxn intransitive-phrase-cxn
    ((?intrans-phrase-unit
      (subunits (?verb-unit ?agent-unit))
      (syn-cat (lex-class phrase)))
     (?verb-unit
      (parent ?intrans-phrase-unit))
     (?agent-unit
      (sem-cat (sem-role agent))
      (syn-cat (syn-role subject)))
     <-
     (?verb-unit
      (args (?event ?agent))
      (sem-cat (sem-class event)
               (verb-class motion)
               (direction ?dir))
      --
      (sem-cat (direction ?dir))
      (syn-cat (lex-class verb)
               (agreement ?agr)))
     (?agent-unit
      (referent ?agent)
      --
      (syn-cat (agreement ?agr)))
     (?intrans-phrase-unit
      --
      (HASH form ((precedes ?agent-unit ?verb-unit ?intrans-phrase-unit)))))
    :cxn-set cxn)

;; EVENT DIRECTIONALITY CXNS
;;----------------------------

(def-fcg-cxn unidirectional-event-cxn
    (<-
     (?verb-unit
      (referent ?ev)
      (sem-cat (sem-class event)
               (verb-class motion))
      (HASH meaning ((event-directionality ?ev unidirectional)))
      --
      (syn-cat (lex-class verb))
      (sem-cat (direction (uni +) (multi -)))))
    :cxn-set cxn)

(def-fcg-cxn multidirectional-event-cxn
    (<-
     (?verb-unit
      (referent ?ev)
      (sem-cat (sem-class event)
               (verb-class motion))
      (HASH meaning ((event-directionality ?ev multidirectional)))
      --
      (syn-cat (lex-class verb))
      (sem-cat (direction (uni -) (multi +)))))
    :cxn-set cxn
    :description "Makes an event multi-directional")

;; ASPECTUAL CXNS
;;----------------------------

(def-fcg-cxn perfective-cxn
    (<-
     (?verb-unit
      (referent ?ev)
      (sem-cat (sem-class event)
               (verb-class motion))
      (HASH meaning ((event-bounds ?ev complete)))
      --
      (syn-cat (aspect (imperfective -) (perfective +)))))
    :cxn-set cxn)

(def-fcg-cxn imperfective-cxn
    (<-
     (?verb-unit
      (referent ?ev)
      (sem-cat (sem-class event)
               (verb-class motion))
      (HASH meaning ((event-bounds ?ev ongoing)))
      --
      (syn-cat (aspect (imperfective +) (perfective -)))))
    :cxn-set cxn)

;; AKTIONSARTEN
;;------------------
(def-fcg-cxn ingressive-cxn
    (<-
     (?verb-unit
      (referent ?ev)
      (sem-cat (sem-class event))
      (HASH meaning ((event-aktionsart ?ev ingressive)))
      --
      (sem-cat (aktionsart (delimitative -)
                           (ingressive +)
                           (durative -)
                           (terminative -)))))
    :cxn-set cxn)

(def-fcg-cxn delimitative-cxn
   (<-
    (?verb-unit
     (referent ?ev)
     (sem-cat (sem-class event))
     (HASH meaning ((event-aktionsart ?ev delimitative)))
     --
     (sem-cat (aktionsart (delimitative +)
                           (ingressive -)
                           (durative -)
                           (terminative -))))
    :cxn-set cxn))

(def-fcg-cxn durative-cxn
   (<-
    (?verb-unit
     (referent ?ev)
     (sem-cat (sem-class event))
     (HASH meaning ((event-aktionsart ?ev durative)))
     --
     (sem-cat (aktionsart (delimitative -)
                          (ingressive -)
                          (durative +)
                          (terminative -))))
    :cxn-set cxn))

(def-fcg-cxn terminative-cxn
   (<-
    (?verb-unit
     (referent ?ev)
     (sem-cat (sem-class event))
     (HASH meaning ((event-aktionsart ?ev terminative)))
     --
     (sem-cat (aktionsart (delimitative -)
                           (ingressive -)
                           (durative -)
                           (terminative +))))
    :cxn-set cxn))


;; PREFIXES
;;------------------------

(def-fcg-cxn po-temporal-prefix
             ((?verb-unit
               (subunits (?prefix-unit))
               (syn-cat (cluster-model (complex-act-perfective ?multi)
                                       (single-act-perfective -)
                                       (natural-perfective ?uni)
                                       (specialized-perfective -)))
               (footprints (prefix)))
              <-
              (?verb-unit
               (referent ?ev)
               (syn-cat (aspect (imperfective -) (perfective +)))
               (sem-cat (verb-class motion)
                        (direction (multi ?multi) (uni ?uni))
                        (aktionsart (delimitative ?multi)
                                    (ingressive ?uni)
                                    (durative -)
                                    (terminative -)))
               (footprints (not prefix))
               ;;temporary
               --
               (parent ?verb-phrase)
               (HASH form ((meets ?prefix-unit ?verb-unit ?verb-phrase))))
              (?prefix-unit
               --
               (HASH form ((string ?prefix-unit "po-")))))
             :cxn-set morph
             :disable-automatic-footprints t)

(def-fcg-cxn za-spatial-prefix
             ((?verb-unit
               (subunits (?prefix-unit))
               (syn-cat (cluster-model (complex-act-perfective -)
                                       (single-act-perfective ?imperfective)
                                       (natural-perfective -)
                                       (specialized-perfective ?perfective)))
               (footprints (prefix)))
              <-
              (?verb-unit
               (referent ?ev)
               (syn-cat (aspect (perfective ?perfective)
                                (imperfective ?imperfective)))
               (sem-cat (verb-class motion)
                        (direction (multi ?imperfective) (uni ?perfective)))
               (footprints (not prefix))
               (HASH meaning ((event-trajectory ?ev in-out)))
               --
               (parent ?verb-phrase)
               (HASH form ((meets ?prefix-unit ?verb-unit ?verb-phrase))))
              (?prefix-unit
               --
               (HASH form ((string ?prefix-unit "za-")))))
             :cxn-set morph
             :disable-automatic-footprints t)

(def-fcg-cxn za-temporal-prefix
             ((?verb-unit
               (subunits (?prefix-unit))
               (syn-cat (cluster-model (complex-act-perfective +)
                                       (single-act-perfective -)
                                       (natural-perfective -)
                                       (specialized-perfective -)))
               (footprints (prefix)))
              <-
              (?verb-unit
               (referent ?ev)
               (syn-cat (aspect (perfective +) (imperfective -)))
               (sem-cat (verb-class motion)
                        (direction (multi +) (uni -))
                        (aktionsart (delimitative -)
                                    (ingressive +)
                                    (durative -)
                                    (terminative -)))
               (footprints (not prefix))
               --
               (parent ?verb-phrase)
               (HASH form ((meets ?prefix-unit ?verb-unit ?verb-phrase))))
              (?prefix-unit
               --
               (HASH form ((string ?prefix-unit "za-")))))
             :cxn-set morph
             :disable-automatic-footprints t)

(def-fcg-cxn s-spatial-down-prefix
             ((?verb-unit
               (subunits (?prefix-unit))
               (syn-cat (cluster-model (complex-act-perfective -)
                                       (single-act-perfective ?multi)
                                       (natural-perfective -)
                                       (specialized-perfective ?uni)))
               (footprints (prefix)))
              <-
              (?verb-unit
               (referent ?ev)
               (syn-cat (aspect (perfective ?uni)
                                (imperfective ?multi)))
               (sem-cat (verb-class motion)
                        (direction (multi ?multi) (uni ?uni)))
                       ; (trajectory ?traj))
               (footprints (not prefix))
               (HASH meaning ((event-trajectory ?ev down)))
               --
               (parent ?verb-phrase)
               (HASH form ((meets ?prefix-unit ?verb-unit ?verb-phrase))))
              (?prefix-unit
               --
               (HASH form ((string ?prefix-unit "s-")))))
             :cxn-set morph
             :disable-automatic-footprints t)

(def-fcg-cxn s-spatial-there-and-back-prefix
             ((?verb-unit
               (subunits (?prefix-unit))
               (syn-cat (cluster-model (complex-act-perfective -)
                                       (single-act-perfective +)
                                       (natural-perfective -)
                                       (specialized-perfective -)))
               (footprints (prefix)))
              <-
              (?verb-unit
               (referent ?ev)
               (syn-cat (aspect (perfective +) (imperfective -)))
               (sem-cat (verb-class motion)
                        (direction (multi +) (uni -)))
               (HASH meaning ((event-trajectory ?ev there-and-back)))
               --
               (parent ?verb-phrase)
               (HASH form ((meets ?prefix-unit ?verb-unit ?verb-phrase))))
              (?prefix-unit
               --
               (HASH form ((string ?prefix-unit "s-")))))
             :cxn-set morph
             :disable-automatic-footprints t)

(def-fcg-cxn vy-spatial-prefix
             ((?verb-unit
               (subunits (?prefix-unit))
               (syn-cat (cluster-model (complex-act-perfective -)
                                       (single-act-perfective -)
                                       (natural-perfective -)
                                       (specialized-perfective ?uni)
                                       (imperfective ?multi)))
               (footprints (prefix)))
              <-
              (?verb-unit
               (referent ?ev)
               (syn-cat (aspect (perfective ?uni) (imperfective ?multi)))
               (sem-cat (verb-class motion)
                        (direction (uni ?uni) (multi ?multi)))
               (HASH meaning ((event-trajectory ?ev out)))
               --
               (parent ?verb-phrase)
               (HASH form ((meets ?prefix-unit ?verb-unit ?verb-phrase))))
              (?prefix-unit
               --
               (HASH form ((string ?prefix-unit "vy-")))))
             :cxn-set morph
             :disable-automatic-footprints t)

(def-fcg-cxn pri-spatial-prefix
             ((?verb-unit
               (subunits (?prefix-unit))
               (syn-cat (cluster-model (complex-act-perfective -)
                                       (single-act-perfective -)
                                       (natural-perfective -)
                                       (specialized-perfective ?uni)
                                       (imperfective ?multi)))
               (footprints (prefix)))
              <-
              (?verb-unit
               (referent ?ev)
               (syn-cat (aspect (perfective ?uni) (imperfective ?multi)))
               (sem-cat (verb-class motion)
                        (direction (uni ?uni) (multi ?multi)))
               (HASH meaning ((event-trajectory ?ev towards)))
               --
               (parent ?verb-phrase)
               (HASH form ((meets ?prefix-unit ?verb-unit ?verb-phrase))))
              (?prefix-unit
               --
               (HASH form ((string ?prefix-unit "pri-")))))
             :cxn-set morph
             :disable-automatic-footprints t)

(def-fcg-cxn ot-spatial-prefix
             ((?verb-unit
               (subunits (?prefix-unit))
               (syn-cat (cluster-model (complex-act-perfective -)
                                       (single-act-perfective -)
                                       (natural-perfective -)
                                       (specialized-perfective ?uni)
                                       (imperfective ?multi)))
               (footprints (prefix)))
              <-
              (?verb-unit
               (referent ?ev)
               (syn-cat (aspect (perfective ?uni) (imperfective ?multi)))
               (sem-cat (verb-class motion)
                        (direction (uni ?uni) (multi ?multi)))
               (HASH meaning ((event-trajectory ?ev away-from)))
               --
               (parent ?verb-phrase)
               (HASH form ((meets ?prefix-unit ?verb-unit ?verb-phrase))))
              (?prefix-unit
               --
               (HASH form ((string ?prefix-unit "ot-")))))
             :cxn-set morph
             :disable-automatic-footprints t)

(def-fcg-cxn ot-temporal-prefix
             ((?verb-unit
               (subunits (?prefix-unit))
               (syn-cat (cluster-model (complex-act-perfective +)
                                       (single-act-perfective -)
                                       (natural-perfective -)
                                       (specialized-perfective -)
                                       (imperfective -)))
               (footprints (prefix)))
              <-
              (?verb-unit
               (referent ?ev)
               (syn-cat (aspect (perfective +) (imperfective -)))
               (sem-cat (verb-class motion)
                        (aktionsart (delimitative -)
                                    (ingressive -)
                                    (durative -)
                                    (terminative +))
                        (direction (multi +) (uni -))) 
               --
               (parent ?verb-phrase)
               (HASH form ((meets ?prefix-unit ?verb-unit ?verb-phrase))))
              (?prefix-unit
               --
               (HASH form ((string ?prefix-unit "ot-")))))
             :cxn-set morph
             :disable-automatic-footprints t)

(def-fcg-cxn u-spatial-prefix
             ((?verb-unit
               (subunits (?prefix-unit))
               (syn-cat (cluster-model (complex-act-perfective -)
                                       (single-act-perfective -)
                                       (natural-perfective -)
                                       (specialized-perfective ?uni)
                                       (imperfective ?multi)))
               (footprints (prefix)))
              <-
              (?verb-unit
               (referent ?ev)
               (syn-cat (aspect (perfective ?uni) (imperfective ?multi)))
               (sem-cat (verb-class motion)
                        (direction (uni ?uni) (multi ?multi)))
               (HASH meaning ((event-trajectory ?ev from)))
               --
               (parent ?verb-phrase)
               (HASH form ((meets ?prefix-unit ?verb-unit ?verb-phrase))))
              (?prefix-unit
               --
               (HASH form ((string ?prefix-unit "u-")))))
             :cxn-set morph
             :disable-automatic-footprints t)

(def-fcg-cxn unmarked-prefix
    ((?verb-unit
      (footprints (prefix)))
     <-
     (?verb-unit
      (sem-cat (verb-class motion)
               (sem-class event)
               (aktionsart (delimitative -)
                           (ingressive -)
                           (durative +)
                           (terminative -))
               (direction (uni ?uni) (multi ?multi)))
      (syn-cat (aspect (imperfective +) (perfective -)))
      (footprints (not prefix))
      --
      (syn-cat (lex-class verb))
      (footprints (not prefix))))
    :cxn-set unmarked-morph
    :disable-automatic-footprints t)
 )