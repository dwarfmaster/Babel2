(in-package :fcg)

;;++++++++++++++++++++++++++++++++++
;;Agreement suffixes
;;++++++++++++++++++++++++++++++++++

(def-fcg-cxn o-1sg-morph
   ((?verb-unit
     (constituents (?stem-unit ?suffix-unit))
     (footprints (person-marking))
     (syn-cat (finite +))
     (form ((meets ?stem-unit ?suffix-unit ?verb-unit))))
    (?suffix-unit
     (parent ?verb-unit)
     (syn-cat (lex-class pn-tam-morph))
     (phon-cat (starts-with-back-vowel +)
               (primary-stress -)))
    <-
    (?verb-unit
     (footprints (not person-marking))
     (constituents (?stem-unit))
     (syn-cat (lex-class verb)
              (agreement (sg?-1-2-3 (+ + - -))
                         (pl?-1-2-3 (- - - -)))
              (verb-class-1-2-3 ?any)
              (tense (present +) (past -) (future -))
              (aspect (imperfective +) (perfective -))
              (mood (indicative +) (subjunctive -)))
     --
     (footprints (not person-marking))
     (constituents (?stem-unit )) ;?suffix-unit
     (syn-cat (lex-class verb)
              (verb-class-1-2-3 ?any)))
    (?suffix-unit
     --
     (HASH form ((string ?suffix-unit "o")))))
   :cxn-set morph
   :disable-automatic-footprints t
   :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn s-2sg-morph
   ((?verb-unit
     (constituents (?stem-unit ?tam-suffix-unit ?suffix-unit))
     (footprints (person-marking))
     (syn-cat (finite +)))
    (?suffix-unit
     (parent ?verb-unit)
     (syn-cat (lex-class pn-morph))
     (phon-cat (primary-stress -)
               (contains-vowel -)))
    <-
    (?verb-unit
     (footprints (not person-marking))
     (constituents (?stem-unit ?tam-suffix-unit))
     (syn-cat (lex-class verb)
              (agreement (sg?-1-2-3 (+ - + -))
                         (pl?-1-2-3 (- - - -)))
              (verb-class-1-2-3 ?any)
              (tense (present ?pres) (past ?past) (future -))
              (aspect (imperfective +) (perfective -))
              (mood (indicative +) (subjunctive -)))
     --
     (footprints (not person-marking))
     (constituents (?stem-unit ?tam-suffix-unit))
     (syn-cat (lex-class verb)
              (verb-class-1-2-3 ?any))
     (HASH form ((precedes ?stem-unit ?suffix-unit ?verb-unit))))
    (?stem-unit
     (syn-cat (lex-class stem))
     (parent ?verb-unit)
     --
     (syn-cat (lex-class stem))
     (parent ?verb-unit))
    (?suffix-unit
     --
     (HASH form ((string ?suffix-unit "s")))))
   :cxn-set morph
   :disable-automatic-footprints t
   :cxn-inventory *spanish-verb-conjugation*)

;; DEFAULT MARKER (NO SUFFIX)
(def-fcg-cxn no-marker-1/3sg-morph
  ((?verb-unit
    (footprints (person-marking))
    (syn-cat (finite +)))
   <-
   (?verb-unit
    (syn-cat (lex-class verb)
             (agreement (sg?-1-2-3 (+ ?1sg - ?3sg))
                        (pl?-1-2-3 (- - - -)))
             (verb-class-1-2-3 ?any)
             (tense (future -)))
    (footprints (not person-marking))
    --
    (footprints (not person-marking))
    (constituents (?stem-unit ?tam-suffix-unit))
    (syn-cat (lex-class verb)))
   (?stem-unit
    (syn-cat (lex-class stem))
    (parent ?verb-unit)
    --
    (syn-cat (lex-class stem))
    (parent ?verb-unit)))
  :cxn-set default ;;DEFAULT CXN SET, only apply if no other person marker applied
  :disable-automatic-footprints t
  :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn mos-1pl-morph
    ((?verb-unit
      (constituents (?stem-unit ?tam-suffix ?suffix-unit))
      (footprints (person-marking))
      (syn-cat (finite +)))
     (?stem-unit
      (phon-cat (primary-stress -)))
     (?suffix-unit
      (parent ?verb-unit)
      (syn-cat (lex-class pn-morph))
      (phon-cat (primary-stress -)))
     <-
     (?verb-unit
      (footprints (not person-marking))
      (constituents (?stem-unit ?tam-suffix))
      (syn-cat (lex-class verb)
               (agreement (sg?-1-2-3 (- - - -))
                          (pl?-1-2-3 (+ + - -)))
               (verb-class-1-2-3 ?any)
               (tense (present ?pres) (past ?past) (future -)))
      --
      (footprints (not person-marking))
      (constituents (?stem-unit ?tam-suffix))
      (syn-cat (lex-class verb)
               (verb-class-1-2-3 ?any))
      (HASH form ((precedes ?stem-unit ?suffix-unit ?verb-unit)))) ;;fields??
     (?stem-unit
      (syn-cat (lex-class stem))
      (parent ?verb-unit)
      --
      (syn-cat (lex-class stem))
      (parent ?verb-unit))
     (?suffix-unit
      --
      (HASH form ((string ?suffix-unit "mos")))))
    :cxn-set morph
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn is-2pl-morph
    ((?verb-unit
      (constituents (?stem-unit ?tam-suffix ?suffix-unit))
      (footprints (person-marking))
      (syn-cat (finite +)))
     (?stem-unit
      (phon-cat (primary-stress -)))
     (?suffix-unit
      (parent ?verb-unit)
      (syn-cat (lex-class pn-morph))
      (phon-cat (starts-with-front-vowel +)
                (primary-stress -)))
     <-
     (?verb-unit
      (footprints (not person-marking))
      (constituents (?stem-unit ?tam-suffix))
      (syn-cat (lex-class verb)
               (agreement (sg?-1-2-3 (- - - -))
                          (pl?-1-2-3 (+ - + -)))
               (verb-class-1-2-3 ?any)
               (tense (present ?pres) (past ?past) (future -)))
      --
      (footprints (not person-marking))
      (constituents (?stem-unit ?tam-suffix))
      (syn-cat (lex-class verb)
               (verb-class-1-2-3 ?any))
      (HASH form ((precedes ?stem-unit ?suffix-unit ?verb-unit)))) ;;fields??
     (?stem-unit
      (syn-cat (lex-class stem))
      (parent ?verb-unit)
      --
      (syn-cat (lex-class stem))
      (parent ?verb-unit))
     (?suffix-unit
      --
      (HASH form ((string ?suffix-unit "is")))))
    :cxn-set morph
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn n-3pl-morph
    ((?verb-unit
      (constituents (?stem-unit ?tam-suffix ?suffix-unit))
      (footprints (person-marking))
      (syn-cat (finite +)))
     (?suffix-unit
      (parent ?verb-unit)
      (syn-cat (lex-class pn-morph))
      (phon-cat (primary-stress -)
                (contains-vowel -)))
     <-
     (?verb-unit
      (footprints (not person-marking))
      (constituents (?stem-unit ?tam-suffix))
      (syn-cat (lex-class verb)
               (agreement (sg?-1-2-3 (- - - -))
                          (pl?-1-2-3 (+ - - +)))
               (verb-class-1-2-3 ?any)
               (tense (present ?pres) (past ?past) (future -)))
      --
      (footprints (not person-marking))
      (constituents (?stem-unit ?tam-suffix))
      (syn-cat (lex-class verb)
               (verb-class-1-2-3 ?any))
      (HASH form ((precedes ?stem-unit ?suffix-unit ?verb-unit)))) ;;fields??
     (?stem-unit
      (syn-cat (lex-class stem))
      (parent ?verb-unit)
      --
      (syn-cat (lex-class stem))
      (parent ?verb-unit))
     (?suffix-unit
      --
      (HASH form ((string ?suffix-unit "n")))))
    :cxn-set morph
    :cxn-inventory *spanish-verb-conjugation*)


;;++++++++++++++++++++++++++++++++++
;; Tam + agreement in one
;;++++++++++++++++++++++++++++++++++

(def-fcg-cxn é-1sg-preterite-morph
   ((?verb-unit
     (constituents (?stem-unit ?suffix-unit))
     (footprints (person-and-tam-marking))
     (syn-cat (finite +)))
    (?suffix-unit
     (parent ?verb-unit)
     (syn-cat (lex-class pn-tam-morph))
     (phon-cat (starts-with-front-vowel +)
               (primary-stress +)))
    <-
    (?verb-unit
     (footprints (not person-and-tam-marking))
     (constituents (?stem-unit))
     (syn-cat (lex-class verb)
              (agreement (sg?-1-2-3 (+ + - -))
                         (pl?-1-2-3 (- - - -)))
              (verb-class-1-2-3 (+ - -))
              (tense (present -) (past +) (future -))
              (aspect (imperfective -) (perfective +))
              (mood (indicative +) (subjunctive -)))
     --
     (footprints (not person-and-tam-marking))
     (constituents (?stem-unit))
     (syn-cat (lex-class verb)
              (verb-class-1-2-3 (+ - -)))
     (HASH form ((meets ?stem-unit ?suffix-unit ?verb-unit))))
    (?suffix-unit
     --
     (HASH form ((string ?suffix-unit "é")))))
   :cxn-set morph
   :disable-automatic-footprints t
   :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn í-1sg-preterite-morph
   ((?verb-unit
     (constituents (?stem-unit ?suffix-unit))
     (footprints (person-and-tam-marking))
     (syn-cat (finite +)))
    (?suffix-unit
     (parent ?verb-unit)
     (syn-cat (lex-class pn-tam-morph))
     (phon-cat (starts-with-front-vowel +)
               (primary-stress +)))
    <-
    (?verb-unit
     (footprints (not person-and-tam-marking))
     (constituents (?stem-unit))
     (syn-cat (lex-class verb)
              (agreement (sg?-1-2-3 (+ + - -))
                         (pl?-1-2-3 (- - - -)))
              (verb-class-1-2-3 (- ?2nd ?3rd))
              (tense (present -) (past +) (future -))
              (aspect (imperfective -) (perfective +))
              (mood (indicative +) (subjunctive -)))
     --
     (footprints (not person-and-tam-marking))
     (constituents (?stem-unit))
     (syn-cat (lex-class verb)
              (verb-class-1-2-3 (- ?2nd ?3rd)))
     (HASH form ((meets ?stem-unit ?suffix-unit ?verb-unit))))
    (?suffix-unit
     --
     (HASH form ((string ?suffix-unit "í")))))
   :cxn-set morph
   :disable-automatic-footprints t
   :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn aste-2sg-preterite-morph
   ((?verb-unit
     (constituents (?suffix-unit))
     (footprints (person-and-tam-marking))
     (syn-cat (finite +)))
    (?suffix-unit
     (parent ?verb-unit)
     (syn-cat (lex-class pn-tam-morph))
     (phon-cat (starts-with-back-vowel +)
               (primary-stress +)))
    <-
    (?verb-unit
     (footprints (not person-and-tam-marking))
     (constituents (?stem-unit))
     (syn-cat (lex-class verb)
              (agreement (sg?-1-2-3 (+ - + -))
                         (pl?-1-2-3 (- - - -)))
              (verb-class-1-2-3 (+ - -))
              (tense (present -) (past +) (future -))
              (aspect (imperfective -) (perfective +))
              (mood (indicative +) (subjunctive -)))
     --
     (footprints (not person-and-tam-marking))
     (constituents (?stem-unit))
     (syn-cat (lex-class verb)
              (verb-class-1-2-3 (+ - -)))
     (HASH form ((meets ?stem-unit ?suffix-unit ?verb-unit))))
    (?suffix-unit
     --
     (HASH form ((string ?suffix-unit "aste")))))
   :cxn-set morph
   :disable-automatic-footprints t
   :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn asteis-2pl-preterite-morph
   ((?verb-unit
     (constituents (?suffix-unit))
     (footprints (person-and-tam-marking))
     (syn-cat (finite +)))
    (?suffix-unit
     (parent ?verb-unit)
     (syn-cat (lex-class pn-tam-morph))
     (phon-cat (starts-with-back-vowel +)
               (primary-stress +)))
    <-
    (?verb-unit
     (footprints (not person-and-tam-marking))
     (constituents (?stem-unit))
     (syn-cat (lex-class verb)
              (agreement (sg?-1-2-3 (- - - -))
                         (pl?-1-2-3 (+ - + -)))
              (verb-class-1-2-3 (+ - -))
              (tense (present -) (past +) (future -))
              (aspect (imperfective -) (perfective +))
              (mood (indicative +) (subjunctive -)))
     --
     (footprints (not person-and-tam-marking))
     (constituents (?stem-unit))
     (syn-cat (lex-class verb)
              (verb-class-1-2-3 (+ - -)))
     (HASH form ((meets ?stem-unit ?suffix-unit ?verb-unit))))
    (?suffix-unit
     --
     (HASH form ((string ?suffix-unit "asteis")))))
   :cxn-set morph
   :disable-automatic-footprints t
   :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn aron-3pl-preterite-morph
   ((?verb-unit
     (constituents (?suffix-unit))
     (footprints (person-and-tam-marking))
     (syn-cat (finite +)))
    (?suffix-unit
     (parent ?verb-unit)
     (syn-cat (lex-class pn-tam-morph))
     (phon-cat (starts-with-back-vowel +)
               (primary-stress +)))
    <-
    (?verb-unit
     (footprints (not person-and-tam-marking))
     (constituents (?stem-unit))
     (syn-cat (lex-class verb)
              (agreement (sg?-1-2-3 (- - - -))
                         (pl?-1-2-3 (+ - - +)))
              (verb-class-1-2-3 (+ - -))
              (tense (present -) (past +) (future -))
              (aspect (imperfective -) (perfective +))
              (mood (indicative +) (subjunctive -)))
     --
     (footprints (not person-and-tam-marking))
     (constituents (?stem-unit))
     (syn-cat (lex-class verb)
              (verb-class-1-2-3 (+ - -)))
     (HASH form ((meets ?stem-unit ?suffix-unit ?verb-unit))))
    (?suffix-unit
     --
     (HASH form ((string ?suffix-unit "aron")))))
   :cxn-set morph
   :disable-automatic-footprints t
   :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn ó-3sg-preterite-morph
   ((?verb-unit
     (constituents (?suffix-unit))
     (footprints (person-and-tam-marking))
     (syn-cat (finite +)))
    (?suffix-unit
     (parent ?verb-unit)
     (syn-cat (lex-class pn-tam-morph))
     (phon-cat (starts-with-back-vowel +)
               (primary-stress +)))
    <-
    (?verb-unit
     (footprints (not person-and-tam-marking))
     (constituents (?stem-unit))
     (syn-cat (lex-class verb)
              (agreement (sg?-1-2-3 (+ - - +))
                         (pl?-1-2-3 (- - - -)))
              (verb-class-1-2-3 (+ - -))
              (tense (present -) (past +) (future -))
              (aspect (imperfective -) (perfective +))
              (mood (indicative +) (subjunctive -)))
     --
     (footprints (not person-and-tam-marking))
     (constituents (?stem-unit))
     (syn-cat (lex-class verb)
              (verb-class-1-2-3 (+ - -)))
     (HASH form ((meets ?stem-unit ?suffix-unit ?verb-unit))))
    (?suffix-unit
     --
     (HASH form ((string ?suffix-unit "ó")))))
   :cxn-set morph
   :disable-automatic-footprints t
   :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn ió-3sg-preterite-morph
   ((?verb-unit
     (constituents (?suffix-unit))
     (footprints (person-and-tam-marking))
     (syn-cat (finite +)))
    (?suffix-unit
     (parent ?verb-unit)
     (syn-cat (lex-class pn-tam-morph))
     (phon-cat (starts-with-back-vowel +)
               (primary-stress +)))
    <-
    (?verb-unit
     (footprints (not person-and-tam-marking))
     (constituents (?stem-unit))
     (syn-cat (lex-class verb)
              (agreement (sg?-1-2-3 (+ - - +))
                         (pl?-1-2-3 (- - - -)))
              (verb-class-1-2-3 (- ?2nd ?3rd))
              (tense (present -) (past +) (future -))
              (aspect (imperfective -) (perfective +))
              (mood (indicative +) (subjunctive -)))
     --
     (footprints (not person-and-tam-marking))
     (constituents (?stem-unit))
     (syn-cat (lex-class verb)
              (verb-class-1-2-3 (- ?2nd ?3rd)))
     (HASH form ((meets ?stem-unit ?suffix-unit ?verb-unit))))
    (?suffix-unit
     --
     (HASH form ((string ?suffix-unit "ió")))))
   :cxn-set morph
   :disable-automatic-footprints t
   :cxn-inventory *spanish-verb-conjugation*)

;;++++++++++++++++++++++++++++++++++
;; Tam suffixes
;;++++++++++++++++++++++++++++++++++

(def-fcg-cxn tam-suffix-a-present-morph
    ((?verb-unit
      (constituents (?stem-unit ?tam-suffix-unit)))
     (?tam-suffix-unit
      (parent ?verb-unit)
      (phon-cat (starts-with-back-vowel +))
      (syn-cat (lex-class tam-morph)))
     <-
     (?verb-unit
      (constituents (?stem-unit))
      (syn-cat (lex-class verb)
               (verb-class-1-2-3 (+ - -))
               (tense (present +) (past -) (future -))
               (aspect (imperfective +) (perfective -))
               (agreement (sg?-1-2-3 (?sg - ?2sg ?3sg))
                          (pl?-1-2-3 ?pl)))
      --
      (constituents (?stem-unit))
      (syn-cat (lex-class verb)
               (verb-class-1-2-3 (+ - -)))
      (HASH form ((meets ?stem-unit ?tam-suffix-unit ?verb-unit))))
     (?tam-suffix-unit
      --
      (HASH form ((string ?tam-suffix-unit "a")))))
    :cxn-set morph
    :score 0.6
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn tam-suffix-a-present-subjunctive-morph
    ((?verb-unit
      (constituents (?stem-unit ?tam-suffix-unit)))
     (?tam-suffix-unit
      (parent ?verb-unit)
      (phon-cat (starts-with-back-vowel +))
      (syn-cat (lex-class tam-morph)))
     <-
     (?verb-unit
      (constituents (?stem-unit))
      (syn-cat (lex-class verb)
               (verb-class-1-2-3 (- ?2nd ?3rd))
               (tense (present +) (past -) (future -))
               (mood (indicative -) (subjunctive +))
               (aspect (imperfective ?impf) (perfective ?pf))
               (agreement (sg?-1-2-3 (?sg - ?2sg ?3sg))
                          (pl?-1-2-3 ?pl)))
      --
      (constituents (?stem-unit))
      (syn-cat (lex-class verb)
               (verb-class-1-2-3 (- ?2nd ?3rd)))
      (HASH form ((meets ?stem-unit ?tam-suffix-unit ?verb-unit))))
     (?tam-suffix-unit
      --
      (HASH form ((string ?tam-suffix-unit "a")))))
    :cxn-set morph
    :score 0.6
    :cxn-inventory *spanish-verb-conjugation*)


(def-fcg-cxn tam-suffix-a-past-morph
    ((?verb-unit
      (constituents (?stem-unit ?tam-suffix-unit)))
     (?tam-suffix-unit
      (parent ?verb-unit)
      (phon-cat (starts-with-back-vowel +))
      (syn-cat (lex-class tam-morph)))
     <-
     (?verb-unit
      (constituents (?stem-unit))
      (syn-cat (lex-class verb)
               (verb-class-1-2-3 (+ - -))
               (tense (present -) (past +) (future -))
               (aspect (imperfective -) (perfective +))
               (agreement (sg?-1-2-3 (- - - -))
                          (pl?-1-2-3 (+ + - -))))
      --
      (constituents (?stem-unit))
      (syn-cat (lex-class verb)
               (verb-class-1-2-3 (+ - -)))
      (HASH form ((meets ?stem-unit ?tam-suffix-unit ?verb-unit))))
     (?tam-suffix-unit
      --
      (HASH form ((string ?tam-suffix-unit "a")))))
    :cxn-set morph
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn tam-suffix-e-2nd-morph
    ((?verb-unit
      (constituents (?stem-unit ?tam-suffix-unit)))
     (?tam-suffix-unit
      (parent ?verb-unit)
      (phon-cat (starts-with-front-vowel +))
      (syn-cat (lex-class tam-morph)))
     <-
     (?verb-unit
      (constituents (?stem-unit))
      (syn-cat (lex-class verb)
               (verb-class-1-2-3 (- + -))
               (tense (present +))
               (mood (indicative +))
               (agreement (sg?-1-2-3 (?sg - ?2sg ?3sg))
                          (pl?-1-2-3 ?pl)))
      --
      (constituents (?stem-unit))
      (syn-cat (lex-class verb)
               (verb-class-1-2-3 (- + -)))
      (HASH form ((meets ?stem-unit ?tam-suffix-unit ?verb-unit))))
     (?tam-suffix-unit
      --
      (HASH form ((string ?tam-suffix-unit "e")))))
    :cxn-set morph
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn tam-suffix-e-3rd-morph
    ((?verb-unit
      (constituents (?stem-unit ?tam-suffix-unit)))
     (?tam-suffix-unit
      (parent ?verb-unit)
      (phon-cat (starts-with-front-vowel +))
      (syn-cat (lex-class tam-morph)))
     <-
     (?verb-unit
      (constituents (?stem-unit))
      (syn-cat (lex-class verb)
               (verb-class-1-2-3 (- - +))
               (tense (present +))
               (agreement (sg?-1-2-3 (?sg - ?2sg ?3sg))
                          (pl?-1-2-3 (?3pl - - ?3pl))))
      --
      (constituents (?stem-unit))
      (syn-cat (lex-class verb)
               (verb-class-1-2-3 (- - +)))
      (HASH form ((meets ?stem-unit ?tam-suffix-unit ?verb-unit))))
     (?tam-suffix-unit
      --
      (HASH form ((string ?tam-suffix-unit "e")))))
    :cxn-set morph
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn tam-suffix-i-morph
    ((?verb-unit
      (constituents (?stem-unit ?tam-suffix-unit)))
     (?tam-suffix-unit
      (parent ?verb-unit)
      (phon-cat (starts-with-front-vowel +))
      (syn-cat (lex-class tam-morph)))
     <-
     (?verb-unit
      (constituents (?stem-unit))
      (syn-cat (lex-class verb)
               (verb-class-1-2-3 (- - +))
               (tense (present +))
               (agreement (sg?-1-2-3 (- - - -))
                          (pl?-1-2-3 (?pl ?1pl ?2pl -))))
      --
      (constituents (?stem-unit))
      (syn-cat (lex-class verb)
               (verb-class-1-2-3 (- - +)))
      (HASH form ((meets ?stem-unit ?tam-suffix-unit ?verb-unit))))
     (?tam-suffix-unit
      --
      (HASH form ((string ?tam-suffix-unit "i")))))
    :cxn-set morph
    :cxn-inventory *spanish-verb-conjugation*)

(def-fcg-cxn tam-suffix-aba-morph
    ((?verb-unit
      (constituents (?stem-unit ?tam-suffix-unit)))
     (?tam-suffix-unit
      (parent ?verb-unit)
      (phon-cat (primary-stress +)
                (starts-with-back-vowel +))
      (syn-cat (lex-class tam-morph)))
     <-
     (?verb-unit
      (constituents (?stem-unit))
      (syn-cat (lex-class verb)
               (verb-class-1-2-3 (+ - -))
               (tense (past +) (present -) (future -))
               (aspect (imperfective +) (perfective -))
               (mood (indicative +) (subjunctive -)))
      --
      (constituents (?stem-unit))
      (syn-cat (lex-class verb)
               (verb-class-1-2-3 (+ - -)))
      (HASH form ((meets ?stem-unit ?tam-suffix-unit ?verb-unit))))
     (?tam-suffix-unit
      --
      (HASH form ((string ?tam-suffix-unit "aba")))))
    :cxn-set morph
    :cxn-inventory *spanish-verb-conjugation*)


