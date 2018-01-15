(in-package :fcg)


;;####################
;; VERBS (lex+morph)
;;####################
(def-fcg-cxn dar-cxn
    ((?dar-unit
      (referent ?x)
      (args (?x ?y ?z ?w))
      (syn-cat (lex-class verb)
               (conj-class irregular)
               (tense ?t)
               (reflexive -)
               (mood ?m)
               (syn-valence 
                (subject ?s)
                (direct-object ?d) 
                (indirect-object ?i)))
      (sem-cat (sem-class event)
               (sem-valence
                (actor ?y)
                (undergoer ?z)
                (receiver ?w))))
     <-
     (?dar-unit
      (HASH meaning ((give ?x)
                     (giver ?x ?y) 
                     (gift ?x ?z)
                     (givee ?x ?w)))
      --
      (syn-cat (lemma "dar"))))
    :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn ver-cxn
    ((?ver-unit
      (referent ?x)
      (args (?x ?y ?z))
      (syn-cat (lex-class verb)
               (conj-class irregular)
               (tense ?t)
               (reflexive -)
               (mood ?m)
               (syn-valence 
                (subject ?s)
                (direct-object ?d) 
                ))
      (sem-cat (sem-class observation)
               (sem-valence
                (actor ?y)
                (undergoer ?z)
                )))
     <-
     (?ver-unit
      (HASH meaning ((see ?x)
                     (seeer ?x ?y) 
                     (seen ?x ?z)))
      --
      (syn-cat (lemma "ver"))))
    :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn ler-cxn
    ((?ler-unit
      (referent ?x)
      (args (?x ?y ?z ))
      (syn-cat (lex-class verb)
               (conj-class irregular)
               (tense ?t)
               (reflexive -)
               (mood ?m)
               (syn-valence 
                (subject ?s)
                (direct-object ?d) 
                ))
      (sem-cat (sem-class event)
               (sem-valence
                (actor ?y)
                (undergoer ?z))))
     <-
     (?ler-unit
      (HASH meaning ((read ?x)
                     (reader ?x ?y) 
                     (read-object ?x ?z)))
      --
      (syn-cat (lemma "ler"))))
    :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn querer-cxn
    ((?querer-unit
      (referent ?x)
      (args (?x ?y ?z))
      (syn-cat (lex-class verb)
               (conj-class irregular)
               (tense ?t)
               (reflexive -)
               (mood ?m)
               (syn-valence 
                (subject ?s)
                (direct-object ?d)))
      (sem-cat (sem-class desire)
               (sem-valence
                (actor ?y)
                (undergoer ?z))))
     <-
     (?querer-unit
      (HASH meaning ((want ?x)
                     (wanter ?x ?y) 
                     (wanted ?x ?z)))
      --
      (syn-cat (lemma "querer"))))
    :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn querer-querias-cxn
    ((?querer-unit
      (sem-cat (sem-valence (actor ?y))
               (sem-function predicating))
      (syn-cat (agreement (person 2)
                          (number singular))
               (lemma "querer")
               (tense imperfect)
               (mood indicative)
               (finite +)))
     <-
     (?querer-unit
      (sem-cat (sem-class desire))
      (syn-cat (conj-class irregular))
      (HASH meaning ((act-would-been-done-by-you ?x ?y)))
      --
      (HASH form  ((string ?querer-unit "querias")))))
    :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn dar-dou-cxn
    ((?dou-unit
      (sem-cat (sem-valence (actor ?y))
               (sem-function predicating))
      (syn-cat (agreement (person 1)
                          (number singular))
               (lemma "dar")
               (tense present)
               (mood indicative)
               (finite +)))
     <-
     (?dou-unit
      (sem-cat (sem-class event))
      (syn-cat (conj-class irregular))
      (HASH meaning ((act-being-done-by-me ?x ?y)))
      --
      (HASH form  ((string ?dou-unit "dou")))))
    :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn dar-dei-cxn
    ((?dei-unit
      (sem-cat (sem-valence (actor ?y))
               (sem-function predicating))
      (syn-cat (agreement (person 1)
                          (number singular))
               (lemma "dar")
               (tense perfect)
               (mood indicative)
               (finite +)))
     <-
     (?dei-unit
      (sem-cat (sem-class event))
      (syn-cat (conj-class irregular))
      (HASH meaning ((act-was-done-by-me ?x ?y)))
      --
      (HASH form  ((string ?dei-unit "dei")))))
    :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn ver-vi-cxn
    ((?vi-unit
      (referent ?x)
      (sem-cat (sem-valence (actor ?y))
               (sem-function predicating))
      (syn-cat (agreement (person 1)
                          (number singular))
               
               (tense past)
               (mood indicative)
               (finite +)))
     <-
     (?vi-unit
      (syn-cat (conj-class irregular)
               (lemma "ver"))
      (HASH meaning ((act-was-done-by-me ?x ?y)))
      --
      (HASH form  ((string ?vi-unit "vi")))))
    :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn dar-deu-cxn
    ((?deu-unit
      (sem-cat (sem-valence (actor ?y))
               (sem-function predicating))
      (syn-cat (agreement (person 3)
                          (number singular))
               (lemma "dar")
               (tense perfect)
               (mood indicative)
               (finite +)))
     <-
     (?deu-unit
      (sem-cat (sem-class event))
      (syn-cat (conj-class irregular)
               (lemma "dar"))
      (HASH meaning ((act-was-done-by-him ?x ?y)))
      --
      (HASH form  ((string ?deu-unit "deu")))))
    :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn dar-deste-cxn
    ((?deste-unit
      (sem-cat (sem-valence (actor ?y))
               (sem-function predicating))
      (syn-cat (agreement (person 2)
                          (number singular))
               (lemma "dar")
               (tense imperfect)
               (mood indicative)
               (finite +)))
     <-
     (?deste-unit
      (sem-cat (sem-class event))
      (referent ?x)
      (syn-cat (conj-class irregular))
      (HASH meaning ((act-was-done-by-you ?x ?y)))
      --
      (HASH form  ((string ?deste-unit "deste")))))
    :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn ler-leio-cxn
    ((?leio-unit
      (sem-cat (sem-valence (actor ?y))
               (sem-function predicating))
      (syn-cat (agreement (person 1)
                          (number singular))
               (lemma "ler")
               (tense present)
               (mood indicative)
               (finite +)))
     <-
     (?leio-unit
      (sem-cat (sem-class event))
      (syn-cat (conj-class irregular))
      (HASH meaning ((act-being-done-by-me ?x ?y)))
      --
      (HASH form  ((string ?leio-unit "leio")))))
    :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn ler-leem-cxn
    ((?leem-unit
      (sem-cat (sem-valence (actor ?y))
               (sem-function predicating))
      (syn-cat (agreement (person 3)
                          (number plural))
               (lemma "ler")
               (tense present)
               (mood indicative)
               (finite +)))
     <-
     (?leem-unit
      (sem-cat (sem-class event))
      (syn-cat (conj-class irregular))
      (HASH meaning ((act-being-done-by-them ?x ?y)))
      --
      (HASH form  ((string ?leem-unit "leem")))))
    :cxn-set lex :cxn-inventory *propor-grammar*)


;;####################
;; NOUNS (lex only)
;;####################

;; (def-fcg-cxn this-book-lex
;;     ((?book-unit
;;       (referent ?x)
;;       (syn-cat (case ?undefined)
;;                (syn-function nominal)
;;                (agreement (number singular)
;;                           (gender masculine)
;;                           (person 3)))
;;       (sem-cat (sem-class (physical-object tangible inanimate))
;;                (sem-function referring-expression)))
;;      <-
;;      (?book-unit
;;       (HASH meaning ((book ?x) (close-to-me ?x)))
;;       --
;;       (HASH form  ((string ?book-unit "este_livro")))))
;;     :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn books-lex
    ((?books-unit
      (referent ?x)
      (syn-cat (lex-class noun)
               (syn-function nominal)
               (agreement (number plural)
                          (gender masculine)
                          (person 3))
               (case ?undefined))
      (sem-cat (sem-class (physical-object tangible inanimate))
               (sem-function referring-expression))
      (boundary (?init ?end)))
     <-
     (?books-unit
      (HASH meaning ((book ?x) (plural ?x)))
      --
      (HASH form  ((string ?books-unit "livros"))))
     (root
      --
      (boundaries ((?books-unit ?init ?end)))))
    :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn book-lex
    ((?book-unit
      (referent ?x)
      (syn-cat (lex-class noun)
               (agreement (number singular)
                          (gender masculine)
                          (person 3))
               (case (?nom ?acc ?dat)))
      (sem-cat (sem-class (physical-object tangible inanimate))
               (sem-function referring-expression))
      (boundary (?init ?end)))
     <-
     (?book-unit
      (HASH meaning ((book ?x) (single ?x)))
      --
      (HASH form  ((string ?book-unit "livro"))))
     (root
      --
      (boundaries ((?book-unit ?init ?end)))))
    :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn pessoas-lex
    ((?people-unit
      (referent ?x)
      (syn-cat (lex-class noun)
               (agreement (person 3)
                          (number plural)
                          (gender feminine))
               (case (?nom ?acc ?dat)))
      (sem-cat (sem-class (physical-object animate))
               (sem-function referring-expression))
      (boundary (?init ?end)))
     <-
     (?people-unit
      (HASH meaning ((people ?x)))
      --
      (HASH form  ((string ?people-unit "pessoas"))))
     (root
      --
      (boundaries ((?people-unit ?init ?end)))))
    :cxn-set lex :cxn-inventory *propor-grammar*)

;;####################
;;CONJUNCTIONS
;;####################

;; causal conjunction: porque
(def-fcg-cxn because-conjunction-cxn
     ((?because-unit
       (referent ?x)
       (sem-cat (sem-class causal))
       (syn-cat (lex-class conjunction)))
      <-
      (?because-unit
       (HASH meaning ((because ?x)))
       --
       (HASH form ((string ?because-unit "porque")))))
  :cxn-set lex :cxn-inventory *propor-grammar*)



;;ADVERBS


(def-fcg-cxn ontem-yesterday-adverb-cxn
     ((?yesterday-unit
       (referent ?x)
       (sem-cat (sem-class temporal))
       (syn-cat (lex-class adverb)
                (negation -)
                (position ?pos)
                (operator-like -))
       (boundary (?init ?end)))
      <-
      (?yesterday-unit
       (HASH meaning ((yesterday ?x)))
       --
       (HASH form ((string ?yesterday-unit "ontem"))))
      (root
      --
      (boundaries ((?yesterday-unit ?init ?end)))))
     :cxn-set lex :cxn-inventory *propor-grammar*)

;; temporal adverb: raramente
(def-fcg-cxn rarely-adverb-cxn
     ((?rarely-unit
       (referent ?x)
       (sem-cat (sem-class temporal))
       (syn-cat (lex-class adverb)
                (negation -)
                (position ?p)
                (operator-like +))
       (boundary (?init ?end)))
      <-
      (?rarely-unit
       (HASH meaning ((rarely ?x)))
       --
       (HASH form ((string ?rarely-unit "raramente"))))
      (root
      --
      (boundaries ((?rarely-unit ?init ?end)))))
     :cxn-set lex :cxn-inventory *propor-grammar*)

;; adverbs of negation
;; adverb of negation: nao (no)
(def-fcg-cxn nao-adverb-cxn
     ((?nao-unit
       (referent ?ev)
       (syn-cat (lex-class adverb)
                (negation +)
                (position pre)
                (operator-like +))
       (boundary (?init ?end)))
      <-
      (?nao-unit
       (HASH meaning ((is-negated ?ev)))
       --
       (HASH form ((string ?nao-unit "nao"))))
      (root
      --
      (boundaries ((?nao-unit ?init ?end)))))
     :cxn-set lex :cxn-inventory *propor-grammar*)

;; adverb of negation: nunca (never)
(def-fcg-cxn nunca-adverb-cxn
     ((?nunca-unit
       (referent ?x)
       (syn-cat (lex-class adverb)
                (negation +)
                (operator-like +)
                (position pre))
       (boundary (?init ?end)))
      <-
      (?nunca-unit
       (HASH meaning ((never ?x)))
       --
       (HASH form ((string ?nunca-unit "nunca"))))
      (root
      --
      (boundaries ((?nunca-unit ?init ?end)))))
     :cxn-set lex  :cxn-inventory *propor-grammar*)


;;####################
;; DETERMINERS
;;####################

;;Quantifiers
;;-----------------

(def-fcg-cxn few-quantifier-cxn
     ((?quantifier-unit
       (referent ?x)
       (boundary (?init ?end)))
      <-
      (?quantifier-unit
       (HASH meaning ((few ?x)))
       --
       (sem-cat (sem-class determiner))
       (syn-cat (lex-class quantifier) ;;what about demonstratives?
                (lemma "pouco")))
      (root
       --
       (boundaries ((?quantifier-unit ?init ?end)))))
     :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn poucas-cxn
     ((?quantifier-unit
       (referent ?x)
       (sem-cat (sem-class determiner))
       (syn-cat (floating +)
                (downward +)))
      <-
      (?quantifier-unit
       (syn-cat (lex-class quantifier)
                (lemma "pouco")
                (agreement (number plural)
                           (gender feminine)
                           (person 3)))
       --
       (HASH form ((string ?quantifier-unit "poucas")))))
     :cxn-set morph :cxn-inventory *propor-grammar*)

(def-fcg-cxn poucos-cxn
     ((?quantifier-unit
       (referent ?x)
       (sem-cat (sem-class determiner))
       (syn-cat (floating +)
                (downward +)))
      <-
      (?quantifier-unit
       (syn-cat (lex-class quantifier)
                (lemma "pouco")
                (agreement (number plural)
                           (gender masculine)
                           (person 3)))
       --
       (HASH form ((string ?quantifier-unit "poucos")))))
     :cxn-set morph :cxn-inventory *propor-grammar*)


(def-fcg-cxn some-quantifier-cxn
     ((?quantifier-unit
       (referent ?x)
       (boundary (?init ?end)))
      <-
      (?quantifier-unit
       (HASH meaning ((some ?x)))
       --
       (sem-cat (sem-class determiner))
       (syn-cat (lex-class quantifier) ;;what about demonstratives?
                (lemma "algumo")))
      (root
       --
       (boundaries ((?quantifier-unit ?init ?end)))))
     :cxn-set lex :cxn-inventory *propor-grammar*)



(def-fcg-cxn algumas-cxn
     ((?quantifier-unit
       (referent ?x)
       (sem-cat (sem-class determiner))
       (syn-cat (floating -)
                (downward -)))
      <-
      (?quantifier-unit
       (syn-cat (lex-class quantifier)
                (lemma "algumo")
                (agreement (number plural)
                           (gender feminine)
                           (person 3)))
       --
       (HASH form ((string ?quantifier-unit "algumas")))))
     :cxn-set morph :cxn-inventory *propor-grammar*)


(def-fcg-cxn algumos-cxn
     ((?quantifier-unit
       (referent ?x)
       (sem-cat (sem-class determiner))
       (syn-cat (floating -)
                (downward -)))
      <-
      (?quantifier-unit
       (syn-cat (lex-class quantifier)
                (lemma "algumo")
                (agreement (number plural)
                           (gender masculine)
                           (person 3)))
       --
       (HASH form ((string ?quantifier-unit "algumos")))))
     :cxn-set morph :cxn-inventory *propor-grammar*)


;;Articles
;;-----------------



(def-fcg-cxn definite-article-cxn
             ((?determiner-unit
               (referent ?x)
               (boundary (?init ?end)))
              <-
              (?determiner-unit
               (HASH meaning ((unique ?x)))
               --
               (sem-cat (sem-class determiner))
               (syn-cat (lex-class article) ;;what about demonstratives?
                        (definiteness +)))
              (root
               --
               (boundaries ((?determiner-unit ?init ?end)))))
             :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn indefinite-article-cxn
             ((?determiner-unit
               (referent ?x)
               (boundary (?init ?end)))
              <-
              (?determiner-unit
               (HASH meaning ((not-unique ?x)))
               --
               (sem-cat (sem-class determiner))
               (syn-cat (lex-class article) ;;what about demonstratives?
                        (definiteness -)))
              (root
               --
               (boundaries ((?determiner-unit ?init ?end)))))
             :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn definite-article-singular-masc-cxn
     ((?determiner-unit
       (referent ?x)
       (sem-cat (sem-class determiner))
       (syn-cat (definiteness +)))
      <-
      (?determiner-unit
       (syn-cat (lex-class article)
                (agreement (number singular)
                           (gender masculine)
                           (person 3)))
       --
       (HASH form ((string ?determiner-unit "o")))))
     :cxn-set morph :cxn-inventory *propor-grammar*)

(def-fcg-cxn definite-article-plural-masc-cxn
     ((?determiner-unit
       (referent ?x)
       (sem-cat (sem-class determiner))
       (syn-cat (definiteness +)))
      <-
      (?determiner-unit
       (syn-cat (lex-class article)
                (agreement (number plural)
                           (gender masculine)
                           (person 3)))
       --
       (HASH form ((string ?determiner-unit "os")))))
     :cxn-set morph :cxn-inventory *propor-grammar*)

(def-fcg-cxn definite-article-singular-fem-cxn
     ((?determiner-unit
       (referent ?x)
       (sem-cat (sem-class determiner))
       (syn-cat (definiteness +)))
      <-
      (?determiner-unit
       (syn-cat (lex-class article)
                (agreement (number singular)
                           (gender feminine)
                           (person 3)))
       --
       (HASH form ((string ?determiner-unit "a")))))
     :cxn-set morph :cxn-inventory *propor-grammar*)

(def-fcg-cxn definite-article-plural-fem-cxn
     ((?determiner-unit
       (referent ?x)
       (sem-cat (sem-class determiner))
       (syn-cat (definiteness +)))
      <-
      (?determiner-unit
       (syn-cat (lex-class article)
                (agreement (number plural)
                           (gender feminine)
                           (person 3)))
       --
       (HASH form ((string ?determiner-unit "as")))))
     :cxn-set morph :cxn-inventory *propor-grammar*)

(def-fcg-cxn indefinite-article-singular-masc-cxn
     ((?determiner-unit
       (referent ?x)
       (sem-cat (sem-class determiner))
       (syn-cat (definiteness -)))
      <-
      (?determiner-unit
       (syn-cat (lex-class article)
                (agreement (number singular)
                           (gender masculine)
                           (person 3)))
       --
       (HASH form ((string ?determiner-unit "um")))))
     :cxn-set morph :cxn-inventory *propor-grammar*)

(def-fcg-cxn indefinite-article-singular-fem-cxn
     ((?determiner-unit
       (referent ?x)
       (sem-cat (sem-class determiner))
       (syn-cat (definiteness -)))
      <-
      (?determiner-unit
       (syn-cat (lex-class article)
                (agreement (number singular)
                           (gender feminine)
                           (person 3)))
       --
       (HASH form ((string ?determiner-unit "uma")))))
     :cxn-set morph :cxn-inventory *propor-grammar*)

(def-fcg-cxn indefinite-article-plural-fem-cxn
     ((?determiner-unit
       (referent ?x)
       (sem-cat (sem-class determiner))
       (syn-cat (definiteness -)))
      <-
      (?determiner-unit
       (syn-cat (lex-class article)
                (agreement (number plural)
                           (gender feminine)
                           (person 3)))
       --
       (HASH form ((string ?determiner-unit "umas")))))
     :cxn-set morph :cxn-inventory *propor-grammar*)

(def-fcg-cxn indefinite-article-plural-masc-cxn
     ((?determiner-unit
       (referent ?x)
       (sem-cat (sem-class determiner))
       (syn-cat (definiteness -)))
      <-
      (?determiner-unit
       (syn-cat (lex-class article)
                (agreement (number plural)
                           (gender masculine)
                           (person 3)))
       --
       (HASH form ((string ?determiner-unit "uns")))))
     :cxn-set morph :cxn-inventory *propor-grammar*)



;;####################
;;PRONOUNS (lex + morph)
;;####################
;; first person singular
;;---------------------------

(def-fcg-cxn single-person-at-origo-cxn
    ((?me-unit
      (referent ?x)
      (sem-cat (sem-class (physical-object animate))
               (sem-function referring-expression))
      (syn-cat (lemma "eu")
               (syn-function nominal)
               (case ?case-undefined))
      (boundary (?init ?end)))
     <-
     (?me-unit
      (HASH meaning ((at-origo ?x) (single ?x)))
      --
      (syn-cat (lex-class pronoun)
               (agreement (person 1)
                          (number singular))))
     (root
    --
    (boundaries ((?me-unit ?init ?end)))))
     :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn eu-pronoun-cxn
     ((?me-unit
       (syn-cat (reflexive -)
                (marked +))
       )
      <-
      (?me-unit
       (syn-cat (lemma "eu")
                (lex-class pronoun)
                (not (marked +))
                (case (+ - -))
                (agreement (person 1)
                           (number singular)))
       --
       (HASH form ((string ?me-unit "eu"))))
      )
     :cxn-set morph :cxn-inventory *propor-grammar*)

(def-fcg-cxn me-pronoun-cxn
     ((?me-unit
       (syn-cat (marked +)
                (reflexive +)
                (clitic +))
       )
      <-
      (?me-unit
       (syn-cat (lemma "eu")
                 (lex-class pronoun)
                (not (marked +))
                (case (- ?acc ?dat))
                (agreement (person 1)
                           (gender ?gen)
                           (number singular)))
       --
       (HASH form ((string ?me-unit "me"))))
      )
     :cxn-set morph :cxn-inventory *propor-grammar*)

;; second person singular
;;---------------------------
(def-fcg-cxn single-person-close-to-origo-cxn
   ((?you-unit
     (referent ?x)
     (sem-cat (sem-class (physical-object animate))
              (sem-function referring-expression))
     (syn-cat (lemma "tu")
              (syn-function nominal)
              (case ?case-undefined))
     (boundary (?init ?end)))
    <-
    (?you-unit
     (HASH meaning ((close-to-origo ?x) (single ?x)))
     --
     (syn-cat (lex-class pronoun)
              (agreement (person 2)
                         (number singular))))
    (root
    --
    (boundaries ((?you-unit ?init ?end)))))
     :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn tu-pronoun-cxn
  ((?you-unit
    (syn-cat (marked +)
             (reflexive -))
    )
   <-
   (?you-unit
    (syn-cat (lemma "tu")
             (lex-class pronoun)
             (not (marked +))
             (case (+ - -))
             (agreement (person 2)
                        (number singular)))
    --
    (HASH form ((string ?you-unit "tu"))))
   )
  :cxn-set morph :cxn-inventory *propor-grammar*)

(def-fcg-cxn te-clitic-cxn
   ((?te-clitic
     (syn-cat (marked +)
              (reflexive +)
              (clitic +))
     )
    <-
    (?te-clitic
     (syn-cat (lemma "tu")
              (lex-class pronoun)
              (not (marked +))
              (case (- ?acc ?dat))
              (agreement (person 2)
                         (number singular)))
     --
     (HASH form ((string ?te-clitic "te"))))
    )
   :cxn-set morph :cxn-inventory *propor-grammar*)

;; third person singular
;;---------------------------
(def-fcg-cxn masculine-referent-single-cxn
    ((?he-unit
      (referent ?z)
      (sem-cat (sem-class (physical-object animate))
               (sem-function referring-expression))
      (syn-cat (lemma "ele")
               (syn-function nominal)
               (agreement (gender masculine))
               (case ?case-undefined))
      (boundary (?init ?end)))
     <-
     (?he-unit
      (HASH meaning ((single ?z) (masculine-referent ?z)))
      --
      (syn-cat (lex-class pronoun)
               (not (relative +))
               (agreement (person 3)
                          (number singular)
                          (gender masculine))))
     (root
      --
      (boundaries ((?he-unit ?init ?end)))))
     :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn feminine-referent-single-cxn
    ((?she-unit
      (referent ?z)
      (sem-cat (sem-class (physical-object animate))
               (sem-function referring-expression))
      (syn-cat (lemma "ela")
              (syn-function nominal)
               (agreement (gender feminine))
               (case ?case-undefined))
      (boundary (?init ?end)))
      
     <-
     (?she-unit
      (HASH meaning ((single ?z) (feminine-referent ?z)))
      --
      (syn-cat (lex-class pronoun)
               (not (relative +))
               (agreement (person 3)
                          (number singular)
                          (gender feminine))))
     (root
      --
      (boundaries ((?she-unit ?init ?end)))))
     :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn undefined-referent-cxn ;;who
    ((?who-unit
      (referent ?z)
      (sem-cat (sem-class (physical-object animate))
               (sem-function referring-expression))
      (syn-cat (lemma "quem")
               (syn-function nominal)
               (case ?case-undefined))
      (boundary (?init ?end)))
     <-
     (?who-unit
      (HASH meaning ((undefined-referent ?z)))
      --
      (syn-cat (lex-class pronoun)
               (relative +)
               (agreement (person 3)
                          (number singular)
                          (gender ?gender))))
     (root
      --
      (boundaries ((?who-unit ?init ?end)))))
     :cxn-set lex :cxn-inventory *propor-grammar*)

;; nominative: he
(def-fcg-cxn ele-pronoun-cxn
     ((?he-unit
       (syn-cat (marked +)
                (reflexive -))
       )
      <-
      (?he-unit
       (syn-cat (lemma "ele")
                (lex-class pronoun)
                (not (marked +))
                (case (+ - -))
                (agreement (person 3)
                          (gender masculine)
                          (number singular)))
       --
       (HASH form ((string ?he-unit "ele"))))
      )
     :cxn-set morph :cxn-inventory *propor-grammar*)

;; accusative: him
(def-fcg-cxn o-pronoun-cxn
    ((?him-unit
      (syn-cat (marked +)
               (reflexive -)
               (clitic +)
               (syn-function nominal))
      )
     <-
     (?him-unit
      (syn-cat (lemma "ele")
               (lex-class pronoun)
               
               (case (- + -))
               (not (marked +))
               (agreement (person 3)
                          (gender masculine)
                          (number singular)))
      --
      (HASH form ((string ?him-unit "o"))))
     )
    :cxn-set morph :cxn-inventory *propor-grammar*)

;; dative: (to) him/her (lhe)
(def-fcg-cxn lhe-pronoun-cxn
    ((?to-him/her-unit
      (syn-cat (marked +)
               (reflexive -)
               (clitic +))
      )
     <-
     (?to-him/her-unit
      (syn-cat (lex-class pronoun)
               (not (marked +))
               (not (relative +))
               (case (- - +))
               (agreement (person 3)
                          (number singular)))
      --
      (HASH form ((string ?to-him/her-unit "lhe"))))
     )
    :cxn-set morph :cxn-inventory *propor-grammar*)



;; relative: (to) whom (quem)
(def-fcg-cxn quem-pronoun-cxn
    ((?quem-unit
      (syn-cat (marked +)
               (reflexive -)
               (relative +))
      )
     <-
     (?quem-unit
      (syn-cat (lemma "quem")
               (lex-class pronoun)
               (relative +)
               (not (marked +))
               (case (?nom ?acc -))
               (agreement (person 3)))
      --
      (HASH form ((string ?quem-unit "quem"))))
     )
    :cxn-set morph :cxn-inventory *propor-grammar*)

(def-fcg-cxn a-quem-pronoun-cxn
    ((?a-quem-unit
      (referent ?x)
      (syn-cat (marked +)
               (reflexive -)
               (relative +)
               (case (- - +)))
      )
     <-
     (?a-quem-unit
      (syn-cat (lex-class pronoun)
               (relative +)
               (not (marked +))
               
               (case (- - +)))
      (HASH meaning ((to-whom ?x)))
      --
      (HASH form ((string ?a-quem-unit "a_quem"))))
     )
    :cxn-set morph :cxn-inventory *propor-grammar*)

;; nominative: she
(def-fcg-cxn ela-pronoun-cxn
    ((?she-unit
      (syn-cat (marked +)
               (reflexive -)))
     <-
     (?she-unit
      (syn-cat (lemma "ela")
               (lex-class pronoun)
               (not (marked +))
               (case (+ - -))
               (agreement (person 3)
                          (gender feminine)
                          (number singular)))
      --
      (HASH form ((string ?she-unit "ela")))))
    :cxn-set morph :cxn-inventory *propor-grammar*)

;; accusative: her
(def-fcg-cxn a-pronoun-cxn
     ((?her-unit
       (syn-cat (marked +)
                (reflexive -)
                (clitic +)
                (syn-function nominal)))
      <-
      (?her-unit
       (syn-cat (lemma "ela")
                (lex-class pronoun)
                (not (marked +))
                (case (- + -))
                (agreement (person 3)
                           (gender feminine)
                           (number singular)))
       --
       (HASH form ((string ?her-unit "a")))))
     :cxn-set morph :cxn-inventory *propor-grammar*)


(def-fcg-cxn closeby-referent-cxn
    ((?this-unit
      (referent ?z)
      (sem-cat (sem-class determiner)
               (sem-function referring-expression))
      (syn-cat (syn-function nominal)
               (lemma "este")
               (agreement (person 3)
                          (gender ?gender-undefined)
                          (number ?number-undefined))
               (case ?case-undefined))
      (boundary (?init ?end)))
      
     <-
     (?this-unit
      (HASH meaning ((referent-nearby ?z)))
      --
      (syn-cat (lex-class demonstrative)))
     (root
      --
      (boundaries ((?this-unit ?init ?end)))))
     :cxn-set lex :cxn-inventory *propor-grammar*)

(def-fcg-cxn este-pronoun-cxn
    ((?this-unit
      (syn-cat (marked +)
               (reflexive -)
               (clitic +)
               (syn-function nominal))
      )
     <-
     (?this-unit
      (syn-cat (lemma "este")
               (lex-class demonstrative)
               
               (case (- + -))
               (not (marked +))
               (agreement (person 3)
                          (gender masculine)
                          (number singular)))
      --
      (HASH form ((string ?this-unit "este"))))
     )
    :cxn-set morph :cxn-inventory *propor-grammar*)
