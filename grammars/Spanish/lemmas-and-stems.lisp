(in-package :fcg)

;; CENAR
;;----------------------------
(def-fcg-cxn cenar-lex
    ((?cenar-verb 
      (syn-cat (verb-class-1-2-3 (+ - -))
               (tense (past ?past) (present ?pres) (future ?fut))
               (aspect (imperfective ?impf) (perfective ?pf))
               (mood (indicative ?ind) (subjunctive ?subj)))
      (sem-cat (sem-class activity)
               (sem-valence (agent ?agent)))
      (args (?ev ?agent))
      (referent ?ev))
     (?cenar-stem
      (parent ?cenar-verb))
     <-
     (?cenar-verb
      (HASH meaning ((activity eat ?ev)
                     (eater ?ev ?agent)))
      --
      (syn-cat (lex-class verb))
      (constituents (?cenar-stem)))
     (?cenar-stem ;;should be created in production >> we need a unit on the contributing part
                  --
                  (syn-cat (lemma "cenar")
                           (lex-class stem))))
    :cxn-set lex
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn cenar-base-stem
    ((?verb-unit
      (syn-cat (lex-class verb))
      (constituents (?stem-unit)))
     (?stem-unit
      (phon-cat (stem-realized +)))
     <-
     (?stem-unit
      (syn-cat (lex-class stem)
               (lemma "cenar"))
      (phon-cat (not (stem-realized +)))
      (parent ?verb-unit) ;;needed for correct binding in production (otherwise new unit will be created)
      --
      (HASH form  ((string ?stem-unit "cen"))))
     )
    :cxn-set stem
    :cxn-inventory *spanish-verb-conjugation*)

;; LLEGAR
;;----------------------------
(def-fcg-cxn llegar-lex
    ((?llegar-verb 
      (syn-cat (verb-class-1-2-3 (+ - -))
               (tense (past ?past) (present ?pres) (future ?fut))
               (aspect (imperfective ?impf) (perfective ?pf))
               (mood (indicative ?ind) (subjunctive ?subj)))
      (sem-cat (sem-class motion)
               (sem-valence (agent ?agent)))
      (args (?ev ?agent))
      (referent ?ev))
     (?llegar-stem
      (parent ?llegar-verb))
     <-
     (?llegar-verb
      (HASH meaning ((motion arrive ?ev)
                     (arriver ?ev ?agent)))
      --
      (constituents (?llegar-stem))
      (syn-cat (lex-class verb)))
     (?llegar-stem ;;should be created in production >> we need a unit on the contributing part
                   --
                   (syn-cat (lemma "llegar")
                            (lex-class stem))))
    :cxn-set lex
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn llegar-base-stem
    ((?verb-unit
      (syn-cat (lex-class verb))
      (constituents (?stem-unit)))
     (?stem-unit
      (phon-cat (stem-realized +)))
     <-
     (?stem-unit
      (syn-cat (lex-class stem)
               (lemma "llegar"))
      (phon-cat (not (stem-realized +)))
      (parent ?verb-unit) ;;needed for correct binding in production (otherwise new unit will be created)
      --
      (HASH form  ((string ?stem-unit "lleg"))))
     )
    :cxn-set stem
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn llegar-llegu-stem
    ((?verb-unit
      (syn-cat (lex-class verb))
      (constituents (?stem-unit)))
     (?stem-unit
      (phon-cat (stem-realized +)))
     <-
     (?stem-unit
      (syn-cat (lex-class stem)
               (lemma "llegar"))
      (phon-cat (not (stem-realized +)))
      (parent ?verb-unit)
      --
      (HASH form ((string ?stem-unit "llegu"))))
     (?suffix-unit
      (phon-cat (starts-with-front-vowel +))
      (syn-cat (not (lex-class pn-morph))) ;;to avoid llegu-a-is
      (parent ?verb-unit)
      --)
     )
    :cxn-set irregular-stem
    :cxn-inventory *spanish-verb-conjugation*)

;; PENSAR
;;----------------------------
(def-fcg-cxn pensar-lex
    ((?pensar-verb 
      (syn-cat 
               (verb-class-1-2-3 (+ - -))
               (tense (past ?past) (present ?pres) (future ?fut))
               (aspect (imperfective ?impf) (perfective ?pf))
               (mood (indicative ?ind) (subjunctive ?subj)))
      (sem-cat (sem-class cognitive-action)
               (sem-valence (agent ?agent)))
      (args (?ev ?agent))
      (referent ?ev))
     (?pensar-stem
      (parent ?pensar-verb))
     <-
     (?pensar-verb
      (HASH meaning ((cognitive-action think ?ev)
                     (thinker ?ev ?agent)))
      --
      (constituents (?pensar-stem))
      (syn-cat (lex-class verb)))
     (?pensar-stem ;;should be created in production >> we need a unit on the contributing part
                   --
                   (syn-cat (lemma "pensar")
                            (lex-class stem))))
    :cxn-set lex
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn pensar-base-stem
    ((?verb-unit
      (constituents (?stem-unit ))
      (syn-cat (lex-class verb)))
     (?stem-unit
      (phon-cat (stem-realized +)))
     <-
     (?stem-unit
      (syn-cat (lex-class stem)
               (lemma "pensar"))
      (phon-cat (not (stem-realized +)))
      (parent ?verb-unit) ;;needed for correct binding in production (otherwise new unit will be created)
      --
      (HASH form  ((string ?stem-unit "pens")))))
;;;      (?verb-unit
;;;       (constituents (?stem-unit))
;;;       --
;;;       (HASH form ((meets ?stem-unit ?suffix-unit ?verb-unit)))))
    :cxn-set stem
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn pensar-piens-stem
    ((?verb-unit
      (constituents (?stem-unit ))
      (syn-cat (lex-class verb)))
     (?stem-unit
      (phon-cat (stem-realized +)))
     <-
     (?stem-unit
      (syn-cat (lex-class stem)
               (lemma "pensar"))
      (phon-cat (not (stem-realized +))
                (primary-stress +)) ;;only criterion
      (parent ?verb-unit)
      --
      (HASH form  ((string ?stem-unit "piens")))))
     ;(?verb-unit
     ; (constituents (?stem-unit ?suffix-unit))
     ; (syn-cat (lex-class verb))
    ;  (form ((meets ?stem-unit ?suffix-unit ?verb-unit))) ;;meets constraint has to be there in production because ??
    ;  --
   ;   (HASH form ((meets ?stem-unit ?suffix-unit ?verb-unit)))))
    :cxn-set irregular-stem
    :cxn-inventory *spanish-verb-conjugation*)


;; COCER
;;----------------------------
(def-fcg-cxn cocer-lex
    ((?cocer-verb 
      (syn-cat 
               (verb-class-1-2-3 (- + -))
               (tense (past ?past) (present ?pres) (future ?fut))
               (aspect (imperfective ?impf) (perfective ?pf))
               (mood (indicative ?ind) (subjunctive ?subj)))
      (sem-cat (sem-class motion)
               (sem-valence (agent ?agent)))
      (args (?ev ?agent))
      (referent ?ev))
     (?cocer-stem
      (parent ?cocer-verb)
      )
     <-
     (?cocer-verb
      (HASH meaning ((apply-heat cook ?ev)
                     (cook ?ev ?agent)))
      --
      (constituents (?cocer-stem))
      (syn-cat (lex-class verb)))
     (?cocer-stem ;;should be created in production >> we need a unit on the contributing part
                   --
                   (syn-cat (lemma "cocer")
                            (lex-class stem))))
    :cxn-set lex
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn cocer-base-stem
    ((?verb-unit
      (syn-cat (lex-class verb))
      (constituents (?stem-unit)))
     (?stem-unit
      (phon-cat (stem-realized +)))
     <-
     (?stem-unit
      (syn-cat (lex-class stem)
               (lemma "cocer"))
      (phon-cat (not (stem-realized +)))
      (parent ?verb-unit) 
      --
      (HASH form  ((string ?stem-unit "coc"))))
     )
    :cxn-set stem
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn cocer-coz-stem
    ((?verb-unit
      (syn-cat (lex-class verb))
      (constituents (?stem-unit)))
     (?stem-unit
      (phon-cat (stem-realized +)))
     <-
     (?suffix-unit
      (phon-cat (starts-with-back-vowel +))
      (syn-cat (not (lex-class pn-morph)))
      (parent ?verb-unit)
      --)
     (?stem-unit
      (syn-cat (lex-class stem)
               (lemma "cocer"))
      (phon-cat (not (stem-realized +))
                (not (primary-stress +)))
      (parent ?verb-unit)
      --
      (HASH form ((string ?stem-unit "coz"))))
     )
    :cxn-set irregular-stem
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn cocer-cuez-stem
    ((?verb-unit
      (syn-cat (lex-class verb))
      (constituents (?stem-unit)))
     (?stem-unit
      (phon-cat (stem-realized +)))
     <-
     (?suffix-unit
      (phon-cat (starts-with-back-vowel +))
      (syn-cat (not (lex-class pn-morph)))
      (parent ?verb-unit)
      --
      )
     (?stem-unit
      (syn-cat (lex-class stem)
               (lemma "cocer"))
      (phon-cat (not (stem-realized +))
                (primary-stress +))
      (parent ?verb-unit)
      --
      (HASH form  ((string ?stem-unit "cuez"))))
     )
    :cxn-set irregular-stem
    :cxn-inventory *spanish-verb-conjugation*)


(def-fcg-cxn cocer-cuec-stem
    ((?verb-unit
      (syn-cat (lex-class verb))
      (constituents (?stem-unit)))
     (?stem-unit
      (phon-cat (stem-realized +)))
     <-
     (?suffix-unit
      (phon-cat (not (starts-with-back-vowel +)))
      (syn-cat (not (lex-class pn-morph)))
      (parent ?verb-unit)
      --
      )
     (?stem-unit
      (syn-cat (lex-class stem)
               (lemma "cocer"))
      (phon-cat (not (stem-realized +))
                (primary-stress +))
      (parent ?verb-unit)
      --
      (HASH form  ((string ?stem-unit "cuec"))))
     )
    :cxn-set irregular-stem
    :cxn-inventory *spanish-verb-conjugation*)


;; PRESENTAR
;;----------------------------
(def-fcg-cxn presentar-lex
    ((?presentar-verb 
      (syn-cat (verb-class-1-2-3 (+ - -))
               (tense (past ?past) (present ?pres) (future ?fut))
               (aspect (imperfective ?impf) (perfective ?pf))
               (mood (indicative ?ind) (subjunctive ?subj)))
      (sem-cat (sem-class action)
               (sem-valence (agent ?agent)))
      (args (?ev ?agent))
      (referent ?ev))
     (?presentar-stem
      (parent ?presentar-verb)
      )
     <-
     (?presentar-verb
      (HASH meaning ((action present ?ev)
                     (presenter ?ev ?agent)))
      --
      (constituents (?presentar-stem))
      (syn-cat (lex-class verb)))
     (?presentar-stem ;;should be created in production >> we need a unit on the contributing part
                   --
                   (syn-cat (lemma "presentar")
                            (lex-class stem))))
    :cxn-set lex
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn presentar-base-stem
    ((?verb-unit
      (syn-cat (lex-class verb))
      (constituents (?stem-unit)))
     (?stem-unit
      (phon-cat (stem-realized +)))
     <-
     (?stem-unit
      (syn-cat (lex-class stem)
               (lemma "presentar"))
      (phon-cat (not (stem-realized +)))
      (parent ?verb-unit) ;;needed for correct binding in production (otherwise new unit will be created)
      --
      (HASH form  ((string ?stem-unit "present"))))
     )
    :cxn-set stem
    :cxn-inventory *spanish-verb-conjugation*)

;; PRESENTIR
;;----------------------------
(def-fcg-cxn presentir-lex
    ((?presentir-verb 
      (syn-cat 
               (verb-class-1-2-3 (- - +))
               (tense (past ?past) (present ?pres) (future ?fut))
               (aspect (imperfective ?impf) (perfective ?pf))
               (mood (indicative ?ind) (subjunctive ?subj)))
      (sem-cat (sem-class emotion)
               (sem-valence (agent ?agent)))
      (args (?ev ?agent))
      (referent ?ev))
     (?presentir-stem
      (parent ?presentir-verb)
      )
     <-
     (?presentir-verb
      (HASH meaning ((emotion anticipate ?ev)
                     (anticipator ?ev ?agent)))
      --
      (constituents (?presentir-stem))
      (syn-cat (lex-class verb)))
     (?presentir-stem ;;should be created in production >> we need a unit on the contributing part
                   --
                   (syn-cat (lemma "presentir")
                            (lex-class stem))))
    :cxn-set lex
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn presentir-base-stem
    ((?verb-unit
      (syn-cat (lex-class verb))
      (constituents (?stem-unit)))
     (?stem-unit
      (phon-cat (stem-realized +)))
     <-
     (?stem-unit
      (syn-cat (lex-class stem)
               (lemma "presentir"))
      (phon-cat (not (stem-realized +)))
      (parent ?verb-unit) ;;needed for correct binding in production (otherwise new unit will be created)
      --
      (HASH form  ((string ?stem-unit "present"))))
     )
    :cxn-set stem
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn presentir-presient-stem
   ((?verb-unit
      (syn-cat (lex-class verb))
      (constituents (?stem-unit)))
    (?stem-unit
     (phon-cat (stem-realized +)))
    <-
    (?suffix-unit
     (phon-cat (starts-with-back-vowel +))
     (syn-cat (not (lex-class pn-morph)))
     (parent ?verb-unit)
     --)
    (?stem-unit
     (syn-cat (lex-class stem)
              (lemma "presentir"))
     (phon-cat (not (stem-realized +))
               (primary-stress +))
     (parent ?verb-unit)
     --
     (HASH form  ((string ?stem-unit "presient"))))
    )
    :cxn-set irregular-stem
    :cxn-inventory *spanish-verb-conjugation*)