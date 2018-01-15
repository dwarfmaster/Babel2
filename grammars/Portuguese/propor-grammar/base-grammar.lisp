(in-package :fcg)

(defvar *propor-grammar*)
;;This file contains all grammatical constructions for the Portuguese
;;grammar ;;; Lexical and morphological constructions are automatically
;;created in the create-grammar.lisp file

(def-fcg-constructions proclisis
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (subunits sequence)
                  (suffixes sequence)
                  (case sequence)
                  (sem-class set)
                  (dependents set)
                  (footprints set)
                  (related set)
                  (lex-class set)
                  (boundaries set-of-predicates)
                  (boundary sequence))
  :fcg-configurations ((:parse-goal-tests :no-applicable-cxns :no-strings-in-root :connected-structure ) 
                       (:production-goal-tests :no-applicable-cxns :no-meaning-in-root  :connected-structure)
                       (:de-render-mode . :de-render-with-scope)
                       (:cxn-supplier-mode . :ordered-by-label-and-score)
                       (:production-order
                        lex conjunction argument-linking morph  information-structure
                        triggers cliticization  word-order )
                       (:parse-order
                        lex morph lex conjunction argument-linking information-structure triggers
                        cliticization word-order )
                       (:maximum-depth . 1000)
                       (:node-tests  :check-duplicate :restrict-search-depth ) 
                       (:render-mode . :render-with-scope)
                       (:priority-mode . :depth-first-prefer-local-bindings)
                       (:shuffle-cxns-before-application . nil))
                       ;(:cxn-sets-with-sequential-application cxn lex ))
  :hierarchy-features (dependents)
  :cxn-inventory *propor-grammar*



;;######################
;; CONJUNCTION CXN SET
;;######################


;; causal conjunctions
(def-fcg-cxn causal-conjunction-cxn
             ((?verb-unit1
               (dependents (?causal-unit)))
              (?causal-unit
               (referent ?x)
               (dependents ( ?verb-unit2))
               (sem-cat (sem-valence (cause ?verb-unit1)
                                     (caused ?verb-unit2)))
               (syn-cat (phrasal-cat ADV)))
              (?verb-unit2
               (syn-cat (restricted +)))
              <-
              (?verb-unit1
               (referent ?y)
               (syn-cat (lex-class (verb)))
               --
               (syn-cat (lex-class (verb)))
               (HASH form ((precedes ?verb-unit1 ?causal-unit scope))))
              (?verb-unit2
               (referent ?z)
               (syn-cat (lex-class (verb)))
               --
               (syn-cat (lex-class (verb)))
               (HASH form ((precedes ?causal-unit ?verb-unit2 ?verb-unit2))))
              (?causal-unit
               (sem-cat (sem-class causal))
               (HASH meaning ((causes ?x ?y ?z)))
               --
               (HASH form ((precedes ?verb-unit1 ?verb-unit2 scope)))
               (sem-cat (sem-class causal))
               (syn-cat (lex-class (conjunction)))))
             :cxn-set conjunction
             :cxn-inventory *propor-grammar*)


;;####################
;; TRIGGERS CXN SET
;;####################

(def-fcg-cxn indefinite-pronoun-trigger-cxn
     ((?verb-unit
       (dependents (?object-unit))
       (syn-cat (restricted +)
                (trigger ?indefinite-pronoun-unit)))
     <-
     (?indefinite-pronoun-unit
      (syn-cat (lex-class (indefinite-pronoun)))
      --
      (syn-cat (lex-class (indefinite-pronoun))))
     (?verb-unit
      (syn-cat (not (has-clitic +))
               (not (restricted +))
               (lex-class (verb)))
      --
      (syn-cat (lex-class (verb)))
      (HASH form ((precedes ?indefinite-pronoun-unit ?verb-unit ?verb-unit)))))
    :cxn-set triggers
    :cxn-inventory *propor-grammar*)

(def-fcg-cxn downward-quantification-trigger-cxn
     ((?verb-unit
       (dependents (?object-unit))
       (syn-cat (restricted +)
                (trigger ?object-unit)))
     <-
     (?object-unit ;;is this object the direct object?
      (dependents (?quantifier))
      (sem-cat (sem-function referring-expression))
      --
      (syn-cat (syn-function nominal))
      (dependents (?quantifier)))
     (?quantifier-unit
      (syn-cat (lex-class (quantifier))
               (downward +))
      --
      (syn-cat (lex-class (quantifier))
               (downward +)))
     (?verb-unit
      
      (syn-cat (not (has-clitic +))
               (not (restricted +))
               (lex-class (verb)))
      --
      (syn-cat (lex-class (verb)))
      (HASH form ((precedes ?quantifier-unit ?verb-unit ?verb-unit)))))
    :cxn-set triggers
    :cxn-inventory *propor-grammar*)

(def-fcg-cxn negation-adverb-precedes-verb-cxn
    ((?negation-unit
      (sem-cat (sem-valence (negated ?y))))
     (?verb-unit
      
      (dependents (?negation-unit))
      (syn-cat (restricted +)
               (trigger ?negation-unit)))
     <-
     (?verb-unit
      (referent ?y)
      (syn-cat (lex-class (verb))
               (syn-valence (subject ?subject-unit)))
      --
      (syn-cat (lex-class (verb))))
     (?negation-unit
      (related (?verb-unit))
      (syn-cat (lex-class (adverb))
               (negation +))
      --
      (related (?verb-unit))
       (syn-cat (lex-class (adverb))
               (negation +))
       (HASH form ((precedes ?negation-unit ?verb-unit ?verb-unit)
                   (precedes ?subject-unit ?negation-unit ?verb-unit)))))
    :cxn-set triggers
    :cxn-inventory *propor-grammar*)

(def-fcg-cxn clausal-topic=object-cxn
    ((?verb-unit
      (footprints (topic-cxn))
      (form ((first ?focus-unit ?verb-unit)))
      (dependents (?focus-unit))
      (syn-cat (restricted +)
               (trigger ?focus-unit))) ;;focus-unit
     <-
     
     (?focus-unit
      (related (?verb-unit))
      (referent ?ref)
      (syn-cat (not (clitic +))
               (case (- ?acc ?dat)))
      (HASH meaning ((topic ?ev ?ref)))
      --
      (related (?verb-unit))
      (syn-cat (syn-role ?role)
               (case (- ?acc ?dat)))
      (boundary (?init-focus ?end-focus)))
     (?verb-unit
      (footprints (not topic-cxn))
      (referent ?ev)
      (syn-cat (lex-class (verb))
               (not (restricted +)))
      --
      (footprints (not topic-cxn))
      (syn-cat (lex-class (verb))))
     (root
      --
      (boundaries ((?some-unit-name ?init-focus 1))))
              ; (syn-valence (not (subject ?focus-unit)))))
     )
    :cxn-set information-structure
    :disable-automatic-footprints t
    :cxn-inventory *propor-grammar*)

(def-fcg-cxn wh-fronted-cxn
     ((?verb-unit
       (form ((first  ?wh-unit ?verb-unit)))
       (footprints (wh-pronoun-placement))
       (dependents (?wh-unit))
       (syn-cat (restricted +)
                (trigger ?wh-unit)))
       
      <-
      (?wh-unit
       (referent ?ref)
       (syn-cat (lex-class (pronoun))
                (relative +)
                (not (clitic +)))
       (HASH meaning ((topic ?ev ?ref)))
       --
       (syn-cat (lex-class (pronoun))
                (relative +)))
      (?verb-unit
       (referent ?ev)
       (footprints (not wh-pronoun-placement))
       (syn-cat (not (has-clitic +))
                (not (restricted +))
                (lex-class (verb)))
       --
       (footprints (not wh-pronoun-placement))
       (syn-cat (lex-class (verb))))
      (root
       --
       (boundaries ((?wh-unit 0 1)))))
     :cxn-set information-structure
     :score 0.7
     :disable-automatic-footprints t
     :cxn-inventory *propor-grammar*) ;;triggers overwrite default sequence type
 
(def-fcg-cxn wh-in-situ-cxn
    ((?verb-unit
      (dependents (?wh-unit))
      (footprints ( wh-pronoun-placement))
      (syn-cat (restricted -)))
     <-
     (?wh-unit
      (referent ?ref)
      (syn-cat (lex-class (pronoun))
               (relative +)
               (not (clitic +)))
      --
      (syn-cat (lex-class (pronoun))
               (relative +)))
     (?verb-unit
      (referent ?ev)
      (footprints (not wh-pronoun-placement))
      (syn-cat (not (has-clitic +))
               (not (restricted -))
               (lex-class (verb)))
      (HASH meaning ((predicate-focus ?ev ?ref)))
      --
      (HASH form ((precedes ?verb-unit ?wh-unit ?verb-unit)))
      (footprints (not wh-pronoun-placement))
      (syn-cat (lex-class (verb)))))
    :cxn-set triggers
    :disable-automatic-footprints t :score 0.4
    :cxn-inventory *propor-grammar*)


;;####################
;;ARGUMENT LINKING CXN SET
;;####################

(def-fcg-cxn adverb-linking-cxn
    ;;add default the  adverb if it is not preceding verb
    ((?adverb-unit
      (related (?verb-unit)))
     (?verb-unit
      (dependents (?adverb-unit)))
     <-
     (?adverb-unit
      (referent ?y)
      (syn-cat (lex-class (adverb)))
      --
      (syn-cat (lex-class (adverb))))
     (?verb-unit
      (referent ?y)
      (dependents (not ?adverb-unit))
      (syn-cat (lex-class (verb)))
      --
      (dependents (not ?adverb-unit))
      (syn-cat (lex-class (verb)))))
  :cxn-set argument-linking
  :cxn-inventory *propor-grammar*)

(def-fcg-cxn determination-cxn
    (;(?determiner-unit
     ; (referent ?y))
     (?noun-unit
      (syn-cat (syn-function nominal))
      (dependents (?determiner-unit))
      (boundary (?init-noun ?end-noun) => (?init-det ?end-noun)))
     <-
     (?determiner-unit
      (referent ?y)
      (sem-cat (sem-class determiner))
      --
      (sem-cat (sem-class determiner))
      (syn-cat (agreement (gender ?same-gender)
                          (number ?same-number)
                          (person ?same-person)))
      (boundary (?init-det ?end-det)))
     (?noun-unit
      (referent ?y)
      (sem-cat (sem-function referring-expression))
;;;       (syn-cat (lex-class (noun)) ;;nominal?
;;;                (agreement (gender ?same-gender)
;;;                           (number ?same-number)
;;;                           (person ?same-person)))
      (boundary (?init-noun ?end-noun))
      --
      (syn-cat (lex-class (noun))
               (agreement (gender ?same-gender)
                          (number ?same-number)
                          (person ?same-person)))
      (boundary (?init-noun ?end-noun))
      (HASH form ((precedes ?determiner-unit ?noun-unit ?noun-unit)))))
    :cxn-set argument-linking
    :cxn-inventory *propor-grammar*)


(def-fcg-cxn adjective-noun-cxn
    ((?adjective-unit
      (referent ?y))
     (?noun-unit
      (syn-cat (syn-function nominal))
      (dependents (?adjective-unit)))
   ;   (boundary (?init-noun ?end-noun) => (?init-adj ?end-noun)))
     <-
     (?adjective-unit
      (sem-cat (sem-function modifier))
      --
      (sem-cat (sem-function modifier))
      (syn-cat (agreement (gender ?same-gender)
                          (number ?same-number)))
      (boundary (?init-adj ?end-adj)))
     (?noun-unit
      (referent ?y)
      (syn-cat (lex-class (noun)) ;;nominal?
               (agreement (gender ?same-gender)
                          (number ?same-number)))
      --
      (syn-cat (lex-class (noun))
               (agreement (gender ?same-gender)
                          (number ?same-number)))
      (boundary (?init-noun ?end-noun))
      (HASH form ((meets ?adjective-unit ?noun-unit ?noun-unit)))))
    :cxn-set argument-linking
    :cxn-inventory *propor-grammar*)

(def-fcg-cxn active-transitive-cxn
     ((?verb-unit
       (footprints (argument-structure-cxn)))
      (?subject-unit
       (related (?verb-unit))
       (syn-cat (syn-role subject)))
      (?direct-object-unit
       (related (?verb-unit))
       ;(footprints (argument-structure-cxn))
       (syn-cat (syn-role direct-object)))
      <-
      (?subject-unit
       (referent ?causer)
       (sem-cat (sem-class (animate))
                (sem-function referring-expression))
       (syn-cat (case (+ - -)))
       --
       (referent ?causer)
       (sem-cat (sem-class (animate)))
       (syn-cat (syn-function nominal)
                (agreement (number ?n)
                           (person ?p))
                (case (+ - -))))
      (?direct-object-unit
       (referent ?transferred)
       (sem-cat (sem-function referring-expression)
                (sem-class (physical-object)))
       (syn-cat (case (- + -)))
       --
       (referent ?transferred)
       (sem-cat (sem-class (physical-object)))
       (syn-cat (syn-function nominal)
                (case (- + -))))
      (?verb-unit 
       (referent ?ev)
      ; (args (?ev ?causer ?transferred))
       (footprints (not argument-structure-cxn))
       (sem-cat (sem-function predicating)
                (sem-valence (actor ?causer)
                             (undergoer ?transferred)))
       --
       (referent ?ev)
       ;(args (?ev ?causer ?transferred))
       (footprints (not argument-structure-cxn))
       (syn-cat (lex-class (verb))
               ; (finite +)
                (agreement (number ?n)
                           (person ?p))
                (syn-valence (subject ?subject-unit)
                             (direct-object ?direct-object-unit)))))
  :cxn-set argument-linking
  :cxn-inventory *propor-grammar*
  :score 0.5)

(def-fcg-cxn active-ditransitive-cxn
     ((?verb-unit
       (footprints (argument-structure-cxn)))
      ; (sem-cat (sem-valence (receiver ?receiver)))
      ; (syn-cat (syn-valence (indirect-object ?indirect-object-unit)))
      (?subject-unit
       (related (?verb-unit))
       (syn-cat (syn-role subject)))
      (?direct-object-unit
       (related (?verb-unit))
       (footprints (argument-structure-cxn))
       (syn-cat (syn-role direct-object)
                (case (- + -))))
      (?indirect-object-unit
       (related (?verb-unit))
       (syn-cat (syn-role indirect-object)
                (case (- - +))))
      <-
      (?subject-unit
       (referent ?causer)
       (sem-cat (sem-function referring-expression))
       --
       (referent ?causer)
       (sem-cat (sem-class (animate)))
       (syn-cat (syn-function nominal)
                (agreement (number ?n)
                           (person ?p))
                (case (+ - -))
                ))
      (?direct-object-unit
       (referent ?transferred)
       (sem-cat (sem-function referring-expression)
                (sem-class (physical-object)))
       (syn-cat (case (- ?c-3 ?c-4)))
       --
       (referent ?transferred)
       (footprints (not argument-structure-cxn))
       (sem-cat (sem-class (physical-object)))
       (syn-cat (syn-function nominal)
                (case (- ?c-3 ?c-4))
                ))
      (?indirect-object-unit
       (referent ?receiver)
       (sem-cat (sem-function referring-expression))
       (syn-cat (case (- ?c-3 ?c-4)))
       --
       (referent ?receiver)
       (sem-cat (sem-class (animate)))
       (syn-cat (syn-function nominal)
                (case (- ?c-1 ?c-2))
                ))
      (?verb-unit 
       (referent ?x)
       (footprints (not argument-structure-cxn))
       (sem-cat (sem-function predicating)
                (sem-valence (actor ?causer)
                             (undergoer ?transferred)
                             (receiver ?receiver)))
       --
       (footprints (not argument-structure-cxn))
       (syn-cat (lex-class (verb))
               ; (finite +)
                (agreement (number ?n)
                           (person ?p))
                (syn-valence (subject ?subject-unit)
                             (direct-object ?direct-object-unit)
                             (indirect-object ?indirect-object-unit)))))
  :cxn-set argument-linking
  :score 0.9
  :cxn-inventory *propor-grammar*
  :disable-automatic-footprints t)      



;;####################
;; CLITIZICATION CXN SET
;;####################

(def-fcg-cxn operator-adverb-precedes-verb-cxn
     ((?verb-unit
       (syn-cat (restricted +)
                (trigger ?adv-unit)))
     <-
     (?adv-unit
      (syn-cat (lex-class (adverb))
               (not (negation +))
               (operator-like +))
      --
      (syn-cat (lex-class (adverb))
               (not (negation +))
               (operator-like +)
               (position pre)))
     (?verb-unit
      (dependents (?adv-unit))
      (syn-cat (lex-class (verb))
               (not (has-clitic +)) ;;will be added by cliticization cxns
               (not (restricted +))) ;;should be added by this cxn
      --
      (syn-cat (lex-class (verb)))
      (dependents (?adv-unit))
      (HASH form ((precedes ?adv-unit ?verb-unit ?verb-unit)))))
     :cxn-set cliticization
     :score 0.5
    :cxn-inventory *propor-grammar*)


(def-fcg-cxn non-operator-adverb-precedes-verb-cxn
     (<-
      (?adv-unit
       (related (?verb-unit))
       (syn-cat (lex-class (adverb))
                (not (negation +))
                (operator-like -))
       --
       (related (?verb-unit))
       (syn-cat (lex-class (adverb))
                (operator-like -)
                (position pre)))
      (?verb-unit
       (dependents (?adv-unit))
       (syn-cat (lex-class (verb))
                (not (restricted +)))
       --
       (syn-cat (lex-class (verb)))
       (dependents (?adv-unit))
       (HASH form ((precedes ?adv-unit ?verb-unit ?verb-unit)))))
     :cxn-set cliticization
    :cxn-inventory *propor-grammar*)

(def-fcg-cxn operator-adverb-follows-verb-cxn
     (<-
      (?adv-unit
       (related (?verb-unit))
       (syn-cat (lex-class (adverb))
                (negation -)
                (operator-like +)
                )
       --
       (related (?verb-unit))
       (syn-cat (lex-class (adverb))
                (operator-like +)
                (position post)))
      (?verb-unit
       (dependents (?adv-unit))
       (syn-cat (lex-class (verb))
                (not (restricted +)))
       --
       (syn-cat (lex-class (verb)))
       (dependents (?adv-unit))
       (HASH form ((precedes  ?verb-unit ?adv-unit ?verb-unit)))))
     :cxn-set cliticization
    :cxn-inventory *propor-grammar*)
     
(def-fcg-cxn enclisis-cxn
    ((?clitic-unit
      (footprints (cliticization-cxn)))
     (?verb-unit
      (dependents (?clitic-unit))
      (syn-cat (has-clitic +)))
     <-
      (?clitic-unit
       (footprints (not cliticization-cxn))
       (related (?verb-unit))
       (syn-cat (lex-class (pronoun))
                (clitic +)
                (case (- ?acc ?dat)))
       --
       (footprints (not cliticization-cxn))
       (syn-cat (lex-class (pronoun))
                (clitic +)
                (case (- ?acc ?dat))))
      (?verb-unit
       (syn-cat (lex-class (verb))
                (not (restricted +))
                (not (has-clitic +)))
       --
       (syn-cat (lex-class (verb))
                (not (restricted +)))
       (HASH form ((meets ?verb-unit ?clitic-unit ?clitic-unit)))))
    :cxn-set cliticization
    :disable-automatic-footprints t
    :cxn-inventory *propor-grammar*)

(def-fcg-cxn proclisis-cxn
    ((?clitic-unit
      (footprints (cliticization-cxn)))
     (?verb-unit
      (dependents (?clitic-unit))
      (syn-cat (has-clitic +)))
     <-
     (?clitic-unit
      (footprints (not cliticization-cxn))
      (related (?verb-unit))
      (syn-cat (lex-class (pronoun))
               (clitic +)
               (case (- ?acc ?dat)))
      --
      (footprints (not cliticization-cxn))
      (syn-cat (lex-class (pronoun))
               (clitic +)
               (case (- ?acc ?dat))))
     (?verb-unit
      (syn-cat (lex-class (verb))
               (restricted +)
               (not (has-clitic +)))
      --
      (syn-cat (lex-class (verb)))
      (HASH form ((meets ?clitic-unit ?verb-unit ?verb-unit)))))
    :cxn-set cliticization
    :cxn-inventory *propor-grammar*) 



;;####################
;; WORD ORDER CXN SET
;;####################


(def-fcg-cxn clausal-topic=subject-cxn
    ((?verb-unit
      (footprints ( topic-cxn))
      (dependents (?focus-unit))
      (form ((first ?focus-unit ?verb-unit )))) 
     <-
     (?focus-unit
      (referent ?ref)
      (syn-cat (not (clitic +))
               (case (+ - -)))
      (HASH meaning ((topic ?ev ?ref)))
      --
      (syn-cat (syn-role ?role)
               (case (+ - -)))
      (boundary (?init-focus ?end-focus)))
     (?verb-unit
      (footprints (not topic-cxn))
      (referent ?ev)
      (syn-cat (lex-class (verb))
               (syn-valence (subject ?focus-unit)))
      --
      (footprints (not topic-cxn))
      (syn-cat (lex-class (verb))
               (finite +)
               (syn-valence (subject ?focus-unit))))
     (root
      --
      (boundaries ((?some-unit ?init-focus 1))))) ;;?some-unit => can be ?focus-unit or child
    :cxn-set information-structure
    :disable-automatic-footprints t
    :cxn-inventory *propor-grammar*)

 
(def-fcg-cxn predicate-focus=direct-object-cxn ;;PREDICATE FOCUS = object
     ((?verb-unit
       (dependents (?object-unit ))
       (footprints (predicate-focus-cxn)))
      <-
      (?verb-unit
       (referent ?ev)
       (footprints (not predicate-focus-cxn))
       (sem-cat (sem-function predicating)
                (not (sem-class copular)))
       (syn-cat (syn-valence (subject ?subject-unit)
                             (direct-object ?object-unit)))
       (HASH meaning ((predicate-focus ?ev ?y)))
       --
       (footprints (not predicate-focus-cxn))
       (syn-cat (lex-class (verb))
                (syn-valence (subject ?subject-unit)
                             (direct-object ?object-unit)))
       (HASH form ((precedes ?verb-unit ?object-unit ?verb-unit))))
      (?object-unit
       (referent ?y)
       (sem-cat (sem-function referring-expression))
       (syn-cat (syn-function nominal)
                (case (- + -)))
       --
       (syn-cat (syn-function nominal)
                ;(not (clitic +))
                (case (- + -)))))
     :cxn-set word-order
     :disable-automatic-footprints t
     :cxn-inventory *propor-grammar*)

(def-fcg-cxn predicate-focus=indirect-object-cxn ;;PREDICATE FOCUS = indir-object
     ((?verb-unit
       (dependents (?object-unit ))
       (footprints (predicate-focus-cxn)))
      <-
      (?verb-unit
       (referent ?ev)
       (sem-cat (sem-function predicating)
                (not (sem-class copular)))
       (HASH meaning ((predicate-focus ?ev ?y)))
       (footprints (not predicate-focus-cxn))
       --
       (footprints (not predicate-focus-cxn))
       (syn-cat (lex-class (verb))
                (syn-valence (subject ?subject-unit)
                             (direct-object ?other-unit)
                             (indirect-object ?object-unit)))
       (HASH form ((precedes ?verb-unit ?object-unit ?verb-unit))))
      (?object-unit
       (referent ?y)
       (syn-cat (syn-function nominal)
                (case (- - +)))
       (sem-cat (sem-function referring-expression))
       --
       (syn-cat (syn-function nominal)
                (not (clitic +))
                (case (- - +))))) 
             :cxn-set word-order
             :disable-automatic-footprints t
             :cxn-inventory *propor-grammar*)

(def-fcg-cxn predicate-focus=subject-cxn ;;PREDICATE FOCUS = subject
     ((?verb-unit
       (footprints ( subject-verb-cxn))
       (dependents (?subject-unit )))
      <-
      (?verb-unit
       (footprints (not subject-verb-cxn))
       (referent ?ev)
       (sem-cat (sem-function predicating)
                (not (sem-class copular))) ;;not a set???
       (HASH meaning ((predicate-focus ?ev ?y)))
       --
       (footprints (not subject-verb-cxn))
       (syn-cat (lex-class (verb))
                (syn-valence (subject ?subject-unit)))
       (HASH form ((precedes ?verb-unit ?subject-unit ?verb-unit))))
      (?subject-unit
       (referent ?y)
       (syn-cat (syn-function nominal)
                (case (+ - -)))
       (sem-cat (sem-function referring-expression))
       --
       (syn-cat (syn-function nominal)
                (case (+ - -)))))
     :cxn-set word-order :disable-automatic-footprints t
     :cxn-inventory *propor-grammar*)

(def-fcg-cxn subject-verb-cxn ;;only used in subclauses now because
                              ;;topic construction only works for main
                              ;;clause (absolute boundary)
             ((?verb-unit
               (dependents (?subject-unit))
               (footprints (subject-verb-cxn)))
              <-
              (?subject-unit
               (referent ?x)
               (sem-cat (sem-function referring-expression))
               (syn-cat (case (+ - -)))
               --
               (syn-cat (syn-function nominal)
                        (case (+ - -))))
              (?verb-unit
               (footprints (not subject-verb-cxn))
               (referent ?ev)
               (syn-cat (syn-valence (subject ?subject-unit)))
               --
               (footprints (not subject-verb-cxn))
               (syn-cat (lex-class (verb))
                       ; (finite +)
                        (syn-valence (subject ?subject-unit)))
               (HASH form ((precedes ?subject-unit ?verb-unit ?verb-unit)))))
       
             :cxn-set word-order :score 0.2
             :disable-automatic-footprints t
             :cxn-inventory *propor-grammar*)


)
