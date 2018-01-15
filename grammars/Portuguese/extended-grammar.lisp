;; Katrien Beuls (21 April 2016)
;; Revised completely September 2016
;; This file contains additional constructions needed for the evaluation of the Portuguese grammar on actual sentences.

(in-package :fcg)


(defparameter *portuguese-grammar* nil)
;;This file contains all grammatical constructions for the Portuguese
;;grammar ;;; Lexical and morphological constructions are automatically
;;created in the create-grammar.lisp file

(def-fcg-constructions proclisis-extended
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
  :fcg-configurations ((:form-predicates . (meets precedes first))
                       (:parse-goal-tests :no-applicable-cxns :no-strings-in-root :connected-structure)
                       (:production-goal-tests :no-applicable-cxns :no-meaning-in-root :connected-structure)
                       (:de-render-mode . :de-render-with-scope)
                       (:cxn-supplier-mode . :ordered-by-label-and-score)
                       (:production-order
                        lex conjunction argument-linking morph information-structure
                        triggers cliticization word-order)
                       (:parse-order
                        lex morph lex conjunction argument-linking
                        information-structure triggers
                        cliticization word-order )
                       ;(:max-search-depth . 100)
                       ;(:max-nr-of-nodes . 250)
                       (:node-tests :check-duplicate :update-references-dependency-grammar
                        :restrict-search-depth :restrict-nr-of-nodes) 
                       (:render-mode . :render-with-scope)
                       (:priority-mode . :depth-first-prefer-local-bindings)
                       (:shuffle-cxns-before-application . nil))
  :hierarchy-features (dependents)
  :cxn-inventory *portuguese-grammar*

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
               (sem-cat (sem-class (causal)))
               (HASH meaning ((causes ?x ?y ?z)))
               --
               (HASH form ((precedes ?verb-unit1 ?verb-unit2 scope)))
               (sem-cat (sem-class (causal)))
               (syn-cat (lex-class (conjunction)))))
             :cxn-set conjunction
             :cxn-inventory *portuguese-grammar*)


;;####################
;; TRIGGERS CXN SET
;;####################

(def-fcg-cxn indefinite-pronoun-trigger-cxn
     ((?verb-unit
       (dependents (?indefinite-pronoun-unit))
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
     :cxn-set information-structure
    :cxn-inventory *portuguese-grammar*)

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
    :cxn-inventory *portuguese-grammar*)


(def-fcg-cxn wh-fronted-cxn
     ((?verb-unit
       (form ((first  ?wh-unit ?verb-unit)))
       (footprints (wh-pronoun-placement))
       (dependents (?wh-unit))
       (syn-cat (restricted +)
                (trigger ?wh-unit)))
      <-
      (root
       --
       (boundaries (?wh-unit 0 ?end)))
      (?wh-unit
       (referent ?ref)
       (syn-cat (lex-class (pronoun))
                (relative +)
                (not (clitic +)))
       (HASH meaning ((topic ?ev ?ref)))
       --
       (HASH form ((precedes ?wh-unit ?verb-unit ?verb-unit)))
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
      )
     :cxn-set information-structure
     :score 0.7
     :disable-automatic-footprints t
     :cxn-inventory *portuguese-grammar*) ;;triggers overwrite default sequence type
 
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
     ; (HASH meaning ((predicate-focus ?ev ?ref)))
      --
      (HASH form ((precedes ?verb-unit ?wh-unit ?verb-unit)))
      (footprints (not wh-pronoun-placement))
      (syn-cat (lex-class (verb)))))
    :cxn-set triggers
    :disable-automatic-footprints t :score 0.4
    :cxn-inventory *portuguese-grammar*)


;;####################
;;ARGUMENT LINKING CXN SET
;;####################

;;; (def-fcg-cxn adverb-linking-cxn
;;;     ;;add default the  adverb if it is not preceding verb
;;;     ((?adverb-unit
;;;       (related (?verb-unit)))
;;;      (?verb-unit
;;;       (dependents (?adverb-unit)))
;;;      <-
;;;      (?adverb-unit
;;;       (referent ?y)
;;;       (syn-cat (lex-class (adverb)))
;;;       --
;;;       (syn-cat (lex-class (adverb))))
;;;      (?verb-unit
;;;       (referent ?y)
;;;       (dependents (not ?adverb-unit))
;;;       (syn-cat (lex-class (verb)))
;;;       --
;;;       (dependents (not ?adverb-unit))
;;;       (syn-cat (lex-class (verb)))))
;;;   :cxn-set argument-linking
;;;   :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn negation-cxn ;;scope?
             ((?negation-unit
               (sem-cat (sem-valence (negated ?ref)))
               )
              (?predication-unit
               (dependents (?negation-unit))
               (syn-cat (restricted +)
               (trigger ?negation-unit)))
              <-
              (?negation-unit
               (referent ?ref)
               (sem-cat (sem-class (negation)))
               --
               (syn-cat (negation +)
                        (lex-class (adverb))
                        (position pre))
               (HASH form ((precedes ?negation-unit ?predication-unit ?predication-unit))))
              (?predication-unit
               (referent ?ref)
               (sem-cat (sem-function predicating))
               --
               (syn-cat (lex-class (verb))
                        ))
              )
             :cxn-set argument-linking
             :score 0.8
             :cxn-inventory *portuguese-grammar*
             )

;;; (def-fcg-cxn negation-adverb-precedes-verb-cxn
;;;     ((?negation-unit
;;;       (sem-cat (sem-valence (negated ?y))))
;;;      (?verb-unit
;;;       
;;;       (dependents (?negation-unit))
;;;       (syn-cat (restricted +)
;;;                (trigger ?negation-unit)))
;;;      <-
;;;      (?verb-unit
;;;       (referent ?y)
;;;       (syn-cat (lex-class (verb))
;;;                (syn-valence (subject ?subject-unit)))
;;;       --
;;;       (syn-cat (lex-class (verb))))
;;;      (?negation-unit
;;;       (related (?verb-unit))
;;;       (syn-cat (lex-class (adverb))
;;;                (negation +))
;;;       --
;;;       (related (?verb-unit))
;;;        (syn-cat (lex-class (adverb))
;;;                (negation +))
;;;        (HASH form ((precedes ?negation-unit ?verb-unit ?verb-unit)
;;;                    (precedes ?subject-unit ?negation-unit ?verb-unit)))))
;;;     :cxn-set triggers
;;;     :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn determination-cxn
    ((?noun-unit
      (syn-cat (syn-function nominal))
      (dependents (?determiner-unit)))
     <-
     (root
      --
      (boundaries ((?determiner-unit ?init-det ?end-det)
                   (?noun-unit ?end-det ?end-noun))))
     (?determiner-unit
      (referent ?y)
      (sem-cat (sem-class (determiner)))
      --
      (sem-cat (sem-class (determiner)))
      (syn-cat (agreement (gender ?same-gender)
                          (number ?same-number)
                          (person ?same-person))))
     (?noun-unit
      (referent ?y)
      (sem-cat (sem-function referring-expression))
      --
      (syn-cat (lex-class (noun))
               
               (agreement (gender ?same-gender)
                          (number ?same-number)
                          (person ?same-person)))
      (HASH form ((precedes ?determiner-unit ?noun-unit ?noun-unit)))))
    :cxn-set argument-linking
    :cxn-inventory *portuguese-grammar*)


(def-fcg-cxn adjective-noun-cxn
    ((?adjective-unit
      (referent ?y))
     (?noun-unit
      (syn-cat (syn-function nominal))
      (dependents (?adjective-unit)))
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
    :cxn-inventory *portuguese-grammar*)

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
                (case (- + -))
                (not (syn-role direct-object))))
      (?verb-unit 
       (referent ?ev)
       (footprints (not argument-structure-cxn))
       (sem-cat (sem-function predicating)
                (sem-valence (actor ?causer)
                             (undergoer ?transferred)))
       --
       (referent ?ev)
       ;(dependents (not ?any-dependent))
       (footprints (not argument-structure-cxn))
       (syn-cat (lex-class (verb))
               ; (finite +)
                (agreement (number ?n)
                           (person ?p))
                (syn-valence (subject ?subject-unit)
                             (direct-object ?direct-object-unit)))))
  :cxn-set argument-linking
  :cxn-inventory *portuguese-grammar*
  :score 0.5)

(def-fcg-cxn active-ditransitive-cxn
     ((?verb-unit
       (footprints (argument-structure-cxn)))
      (?subject-unit
       (related (?verb-unit))
       (syn-cat (syn-role subject)))
      (?direct-object-unit
       (related (?verb-unit))
     ;  (footprints (argument-structure-cxn))
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
                (case (+ - -))))
      (?direct-object-unit
       (referent ?transferred)
       (sem-cat (sem-function referring-expression)
                (sem-class (physical-object)))
       (syn-cat (case (- ?c-3 ?c-4)))
       --
       (referent ?transferred)
       (sem-cat (sem-class (physical-object)))
       (syn-cat (syn-function nominal)
                (case (- ?c-3 ?c-4))
                ))
      (?indirect-object-unit
       (referent ?receiver)
       (sem-cat (sem-function referring-expression))
       (syn-cat (case (- ?c-1 ?c-2)))
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
       (sem-cat (sem-valence (actor ?causer)
                             (undergoer ?transferred)
                             (receiver ?receiver)))
       (syn-cat (lex-class (verb))
                (agreement (number ?n)
                           (person ?p))
                (syn-valence (subject ?subject-unit)
                             (direct-object ?direct-object-unit)
                             (indirect-object ?indirect-object-unit)))))
  :cxn-set argument-linking
  :score 0.9
  :cxn-inventory *portuguese-grammar*
  :disable-automatic-footprints t)      



;;####################
;; CLITIZICATION CXN SET
;;####################

(def-fcg-cxn operator-adverb-follows-verb-cxn
     ((?verb-unit
       (dependents (?adv-unit))
       )
      <-
      (?adv-unit
      ; (related (?verb-unit))
       (syn-cat (lex-class (adverb))
                (negation -)
                (operator-like +)
                )
       --
     ;  (related (?verb-unit))
       (syn-cat (lex-class (adverb))
                (operator-like +)
                (position post)))
      (?verb-unit
      ; (dependents (?adv-unit))
       (syn-cat (lex-class (verb))
                (not (restricted +)))
       --
       (syn-cat (lex-class (verb)))
       ;(dependents (?adv-unit))
       (HASH form ((precedes  ?verb-unit ?adv-unit ?verb-unit)))))
     :cxn-set cliticization
    :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn operator-adverb-precedes-verb-cxn
     ((?verb-unit
       (dependents (?adv-unit))
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
      ;(dependents (?adv-unit))
      (syn-cat (lex-class (verb))
               (not (has-clitic +)) ;;will be added by cliticization cxns
               (not (restricted +))) ;;should be added by this cxn
      --
      (syn-cat (lex-class (verb)))
      ;(dependents (?adv-unit))
      (HASH form ((precedes ?adv-unit ?verb-unit ?verb-unit)))))
     :cxn-set cliticization
     :score 0.5
    :cxn-inventory *portuguese-grammar*)


(def-fcg-cxn non-operator-adverb-precedes-verb-cxn
     ((?verb-unit
       (dependents (?adv-unit)))
      <-
      (?adv-unit
       (referent ?ref)
       ;(related (?verb-unit))
       (syn-cat (lex-class (adverb))
                (not (negation +))
                (operator-like -))
       --
       ;(related (?verb-unit))
       (syn-cat (lex-class (adverb))
                (operator-like -)
                (position pre)))
      (?verb-unit
       (referent ?ref)
       ;(dependents (?adv-unit))
       (syn-cat (lex-class (verb))
                (not (restricted +)))
       --
       (syn-cat (lex-class (verb)))
       
       (HASH form ((meets ?adv-unit ?verb-unit ?verb-unit)))))
     :cxn-set cliticization
    :cxn-inventory *portuguese-grammar*)



;;####################
;; WORD ORDER CXN SET
;;####################



(def-fcg-cxn clausal-topic=predicate-cxn
    ((root
      (footprints (topic-cxn)))
     (?focus-unit
      (form ((first ?focus-unit ?focus-unit))))
     <-
     (root
      (footprints (not topic-cxn))
      --
      (footprints (not topic-cxn))
      (boundaries ((?focus-unit 0 ?end))))
     (?focus-unit
      (referent ?ref)
      (syn-cat (lex-class (verb))
               (finite +))
      (HASH meaning ((topic ?ev ?ref)))
      --
   ;   (HASH form ((precedes ?focus-unit ?verb-unit ?verb-unit)))
      (syn-cat (lex-class (verb))
               (finite +))))
             :cxn-set information-structure
             :disable-automatic-footprints t
             :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn clausal-topic=subject-cxn
    ((root
      (footprints (topic-cxn)))
     (?verb-unit
      (footprints (topic-cxn))
      (dependents (?focus-unit))
      (form ((first ?focus-unit ?verb-unit)))) 
     <-
     (root
      (footprints (not topic-cxn))
      --
      (footprints (not topic-cxn))
      (boundaries ((?focus-unit 0 ?end))))
     (?focus-unit
      (referent ?ref)
      (syn-cat (not (clitic +))
               (case (+ - -)))
      (HASH meaning ((topic ?ev ?ref)))
      --
  ;    (HASH form ((precedes ?focus-unit ?verb-unit ?verb-unit)))
      (syn-cat (syn-role subject)
               (case (+ - -)))
      )
     (?verb-unit
      (footprints (not topic-cxn))
      (referent ?ev)
      (syn-cat (lex-class (verb))
               (syn-valence (subject ?focus-unit)))
      --
      (footprints (not topic-cxn))
      (syn-cat (lex-class (verb))
               (finite +)
               (syn-valence (subject ?focus-unit))))) 
    :cxn-set information-structure
    :disable-automatic-footprints t
    :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn clausal-topic=object-cxn
    ((root
      (footprints (topic-cxn)))
     (?verb-unit
      (footprints (topic-cxn))
      (form ((first ?focus-unit ?verb-unit)))
      (dependents (?focus-unit))
      (syn-cat (restricted +)
               (trigger ?focus-unit))) ;;focus-unit
     <-
     (root
      (footprints (not topic-cxn))
      --
      (footprints (not topic-cxn))
      (boundaries ((?focus-unit 0 ?end))))
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
      (HASH form ((precedes ?focus-unit ?verb-unit ?verb-unit))))
     (?verb-unit
      (footprints (not topic-cxn))
      (referent ?ev)
      (syn-cat (lex-class (verb))
               (not (restricted +)))
      --
      (footprints (not topic-cxn))
      (syn-cat (lex-class (verb)))))
    :cxn-set information-structure
    :disable-automatic-footprints t
    :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn clausal-topic=prep-object-cxn
    ((root
      (footprints (topic-cxn)))
     (?verb-unit
      (form ((first ?focus-unit ?verb-unit)))
      (dependents (?focus-unit))
      (syn-cat (restricted +)
               (trigger ?focus-unit))) ;;focus-unit
     <-
     (root
      (footprints (not topic-cxn))
      --
      (footprints (not topic-cxn))
      (boundaries ((?focus-unit 0 ?end))))
     (?focus-unit
      (referent ?ref)
      (dependents (?arg-unit))
      (syn-cat (case (prepositional-object)))
      (HASH meaning ((topic ?ev ?ref)))
      --
      (HASH form ((precedes ?focus-unit ?verb-unit ?verb-unit)))
      (referent ?ref)
      (syn-cat (case (prepositional-object)))
      )
     (?verb-unit
      (referent ?ev)
      (syn-cat (lex-class (verb))
               (not (restricted +)))
      --
      (syn-cat (lex-class (verb)))))
    :cxn-set information-structure
    :disable-automatic-footprints t
    :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn predicate-focus=direct-object-cxn ;;PREDICATE FOCUS = object
     ((?object-unit
       (footprints (predicate-focus)))
      (?verb-unit
       (dependents (?object-unit))
       (footprints (predicate-focus)))
      <-
      (?verb-unit
       (referent ?ev)
       (footprints (not predicate-focus))
       (sem-cat (sem-function predicating)
                (not (sem-class (copular))))
       (syn-cat (syn-valence (subject ?subject-unit)
                             (direct-object ?object-unit)))
       (HASH meaning ((predicate-focus ?ev ?y)))
       --
       (footprints (not predicate-focus))
       (syn-cat (lex-class (verb))
                (syn-valence (subject ?subject-unit)
                             (direct-object ?object-unit)))
       (HASH form ((meets ?verb-unit ?object-unit ?verb-unit))))
      (?object-unit
       (referent ?y)
       (related (?verb-unit))
       (footprints (not predicate-focus))
       (sem-cat (sem-function referring-expression))
       (syn-cat (syn-function nominal)
                (case (- + -)))
       --
       (footprints (not predicate-focus))
       (syn-cat (syn-function nominal)
                (case (- + -)))))
     :cxn-set word-order
     :disable-automatic-footprints t
     :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn predicate-focus=indirect-object-cxn ;;PREDICATE FOCUS = indir-object
     ((?verb-unit
       (dependents (?object-unit ))
       (footprints (predicate-focus)))
      <-
      (?verb-unit
       (referent ?ev)
       (sem-cat (sem-function predicating)
                (not (sem-class (copular))))
       (HASH meaning ((predicate-focus ?ev ?y)))
       (footprints (not predicate-focus))
       --
       (footprints (not predicate-focus))
       (syn-cat (lex-class (verb))
                (syn-valence (subject ?subject-unit)
                             (direct-object ?other-unit)
                             (indirect-object ?object-unit)))
       (HASH form ((meets ?verb-unit ?object-unit ?verb-unit))))
      (?object-unit
       (referent ?y)
       (related (?verb-unit))
       (syn-cat (syn-function nominal)
                (case (- - +)))
       (sem-cat (sem-function referring-expression))
       --
       (related (?verb-unit))
       (syn-cat (syn-function nominal)
             ;   (not (clitic +))
                (case (- - +))))) 
     :cxn-set word-order
     :disable-automatic-footprints t
     :score 0.6
     :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn predicate-focus=subject-cxn ;;PREDICATE FOCUS = subject
     ((?verb-unit
       (footprints ( subject-verb-cxn))
       (dependents (?subject-unit )))
      <-
      (?verb-unit
       (footprints (not subject-verb-cxn))
       (referent ?ev)
       (sem-cat (sem-function predicating)
                (not (sem-class (copular)))) 
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
     :cxn-inventory *portuguese-grammar*)

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
             :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn active-ditransitive-without-overt-subject
             ((?verb-unit
               (footprints (argument-structure-cxn)))
              (?direct-object-unit
               (related (?verb-unit))
               (footprints (argument-structure-cxn))
               (syn-cat (syn-role direct-object)))
              (?indirect-object-unit
               (related (?verb-unit))
               (footprints (argument-structure-cxn))
               (syn-cat (syn-role indirect-object)))
              <-
              (?indirect-object-unit
               (referent ?receiver)
               (sem-cat (sem-function referring-expression)
                        (sem-class (physical-object)))
               (syn-cat (case (- - +)))
               --
               (referent ?receiver)
               (footprints (not argument-structure-cxn))
               (sem-cat (sem-class (physical-object)))
               (syn-cat (syn-function nominal)
                        (case (- - +))))
              (?direct-object-unit
               (referent ?transferred)
               (sem-cat (sem-function referring-expression)
                        (sem-class (physical-object)))
               (syn-cat (case (- + -)))
               --
               (referent ?transferred)
               (footprints (not argument-structure-cxn))
               (sem-cat (sem-class (physical-object)))
               (syn-cat (syn-function nominal)
                        (case (- + -))))
              (?verb-unit 
               (referent ?ev)
               (footprints (not argument-structure-cxn))
               (sem-cat (sem-function predicating)
                        (sem-valence (actor ?causer)
                                     (undergoer ?transferred)
                                     (receiver ?receiver)))
               --
               (referent ?ev)
               (footprints (not argument-structure-cxn))
               (syn-cat (lex-class (verb))
                        (agreement (number ?n)
                                   (person ?p))
                        (syn-valence (subject ?subject-unit)
                                     (direct-object ?direct-object-unit)
                                     (indirect-object ?indirect-object-unit)))))
             :cxn-set argument-linking
             :cxn-inventory *portuguese-grammar*
             :score 0.7)

(def-fcg-cxn active-inherent-dative-without-overt-subject-cxn
             ((?verb-unit
               (footprints (argument-structure-cxn)))
              
              (?indirect-object-unit
               (related (?verb-unit))
               (footprints (argument-structure-cxn))
               (syn-cat (syn-role indirect-object)))
              <-
              
              (?indirect-object-unit
               (referent ?receiver)
               (sem-cat (sem-function referring-expression)
                        (sem-class (physical-object)))
               (syn-cat (case (- - +)))
               --
               (referent ?receiver)
               (footprints (not argument-structure-cxn))
               (sem-cat (sem-class (physical-object)))
               (syn-cat (syn-function nominal)
                        (case (- - +))))
              (?verb-unit 
               (referent ?ev)
               (footprints (not argument-structure-cxn))
               (sem-cat (sem-function predicating)
                        (sem-class (transfer-included))
                        (sem-valence (actor ?causer)
                                     (receiver ?receiver)))
               --
               (referent ?ev)
               (footprints (not argument-structure-cxn))
               (sem-cat (sem-class (transfer-included)))
               (syn-cat (lex-class (verb))
                        (agreement (number ?n)
                                   (person ?p))
                        (syn-valence (subject ?subject-unit)
                                     (indirect-object ?indirect-object-unit)))))
             :cxn-set argument-linking
             :cxn-inventory *portuguese-grammar*
             :score 0.6)

(def-fcg-cxn active-inherent-dative-cxn
             ((?verb-unit
               (footprints (argument-structure-cxn)))
              (?subject-unit
               (related (?verb-unit))
               (footprints (argument-structure-cxn))
               (syn-cat (syn-role subject)))
              (?indirect-object-unit
               (related (?verb-unit))
               (footprints (argument-structure-cxn))
               (syn-cat (syn-role indirect-object)))
              <-
              (?subject-unit
               (referent ?causer)
               (sem-cat (sem-function referring-expression)
                        (sem-class (physical-object)))
               (syn-cat (case (+ - -)))
               --
               (referent ?causer)
               (footprints (not argument-structure-cxn))
               (sem-cat (sem-class (physical-object)))
               (syn-cat (syn-function nominal)
                        (agreement (number ?n)
                                   (person ?p))
                        (case (+ - -))))
              (?indirect-object-unit
               (referent ?receiver)
               (sem-cat (sem-function referring-expression)
                        (sem-class (physical-object)))
               (syn-cat (case (- - +)))
               --
               (referent ?receiver)
               (footprints (not argument-structure-cxn))
               (sem-cat (sem-class (physical-object)))
               (syn-cat (syn-function nominal)
                        (case (- - +))))
              (?verb-unit 
               (referent ?ev)
               (footprints (not argument-structure-cxn))
               (sem-cat (sem-function predicating)
                        (sem-class (transfer-included))
                        (sem-valence (actor ?causer)
                                     (receiver ?receiver)))
               --
               (referent ?ev)
               (footprints (not argument-structure-cxn))
               (sem-cat (sem-class (transfer-included)))
               (syn-cat (lex-class (verb))
                        (agreement (number ?n)
                                   (person ?p))
                        (syn-valence (subject ?subject-unit)
                                     (indirect-object ?indirect-object-unit)))))
             :cxn-set argument-linking
             :cxn-inventory *portuguese-grammar*
             :score 0.6)
  
(def-fcg-cxn active-transitive-without-overt-subject
             ((?verb-unit
               (footprints (argument-structure-cxn)))              
              (?direct-object-unit
               (related (?verb-unit))
               (footprints (argument-structure-cxn))
               (syn-cat (syn-role direct-object)))
              <-
              
              (?direct-object-unit
               (referent ?transferred)
               (sem-cat (sem-function referring-expression)
                        (sem-class (physical-object)))
               --
               (referent ?transferred)
               (footprints (not argument-structure-cxn))
               (sem-cat (sem-class (physical-object)))
               (syn-cat (syn-function nominal)
                        (case (- + -))))
              (?verb-unit 
               (referent ?ev)
               (footprints (not argument-structure-cxn))
               (sem-cat (sem-function predicating)
                        (sem-valence (actor ?causer)
                                     (undergoer ?transferred)))
               --
               (referent ?ev)
               (footprints (not argument-structure-cxn))
               (syn-cat (lex-class (verb))
                        (agreement (number ?n)
                                   (person ?p))
                        (syn-valence (subject ?subject-unit)
                                     (direct-object ?direct-object-unit)))))
             :cxn-set argument-linking
             :cxn-inventory *portuguese-grammar*
             :score 0.3)

(def-fcg-cxn comparative-conjunction-cxn
             ((?verb-unit1
               (dependents (?conjunction-unit)))
              (?conjunction-unit
               (referent ?x)
               (dependents (?verb-unit2))
               (syn-cat (phrasal-cat ADV)))
              <-
              (?verb-unit1
               (referent ?y)
               (syn-cat (lex-class (verb)))
               --
               (syn-cat (lex-class (verb)))
               (HASH form ((precedes ?verb-unit1 ?conjunction-unit ?scope))))
              (?verb-unit2
               (referent ?z)
               (syn-cat (lex-class (verb)))
               --
               (syn-cat (lex-class (verb)))
               (HASH form ((precedes ?conjunction-unit ?verb-unit2 ?verb-unit2))))
              (?conjunction-unit
               (syn-cat (lex-class (conjunction)))
               (sem-cat (sem-class (comparative)))
               (HASH meaning ((subordination ?x ?y ?z)))
               --
               (HASH form ((precedes ?verb-unit1 ?verb-unit2 ?scope)))
               (sem-cat (sem-class (comparative)))
               (syn-cat (lex-class (conjunction))
                        (syn-function subordinative))))
             :cxn-set conjunction
             :cxn-inventory *portuguese-grammar*)
 
(def-fcg-cxn subordinating-conjunction-cxn
                    ((?verb-unit1
                      (dependents (?conjunction-unit)))
                     (?conjunction-unit
                      (referent ?x)
                      (dependents (?verb-unit2))
                      (syn-cat (phrasal-cat ADV)))
                     (?verb-unit2
                      (syn-cat (restricted +)))
                     <-
                     (?verb-unit1
                      (referent ?y)
                      (syn-cat (lex-class (verb)))
                      --
                      (syn-cat (lex-class (verb)))
                      (HASH form ((precedes ?verb-unit1 ?conjunction-unit scope))))
                     (?verb-unit2
                      (referent ?z)
                      (syn-cat (lex-class (verb)))
                      --
                      (syn-cat (lex-class (verb)))
                      (HASH form ((precedes ?conjunction-unit ?verb-unit2 ?verb-unit2))))
                     (?conjunction-unit
                      (syn-cat (lex-class (conjunction)))
                      (sem-cat (sem-class (subordinating)))
                      (HASH meaning ((subordination ?x ?y ?z)))
                      --
                      (HASH form ((precedes ?verb-unit1 ?verb-unit2 scope)))
                      (syn-cat (lex-class (conjunction))
                               (syn-function subordinative))))
                    :cxn-set conjunction
                    :cxn-inventory *portuguese-grammar*)
 
(def-fcg-cxn coordinating-conjunction-cxn
                    ((?verb-unit1
                      (dependents (?conjunction-unit)))
                     (?conjunction-unit
                      (referent ?x)
                      (dependents (?verb-unit2))
                      (syn-cat (phrasal-cat ADV)))
                     (?verb-unit2
                      (syn-cat (restricted +)))
                     <-
                     (?verb-unit1
                      (referent ?y)
                      (syn-cat (lex-class (verb)))
                      --
                      (syn-cat (lex-class (verb)))
                      (HASH form ((precedes ?verb-unit1 ?conjunction-unit scope))))
                     (?verb-unit2
                      (referent ?z)
                      (syn-cat (lex-class (verb)))
                      --
                      (syn-cat (lex-class (verb)))
                      (HASH form ((precedes ?conjunction-unit ?verb-unit2 ?verb-unit2))))
                     (?conjunction-unit
                      (syn-cat (lex-class (conjunction)))
                      (HASH meaning ((coordination ?x ?y ?z)))
                      --
                      (HASH form ((precedes ?verb-unit1 ?verb-unit2 scope)))
                      (syn-cat (lex-class (conjunction))
                               (syn-function coordinative))))
                    :cxn-set conjunction
                    :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn subject-predicate-cxn ;;right member of that
                                          ;;construction stands in the
                                          ;;predication relation to
                                          ;;the left member
                    ((?subject-unit
                      (related (?predication-unit))
                      (syn-cat (syn-function nominal)
                               (syn-role subject)))
                     <-
                     (?predication-unit
                      (referent ?ev)
                      (sem-cat (sem-class (copular))
                               (sem-valence (actor ?x)))
                      (HASH meaning ((predication ?ev ?x)))
                      --
                      (sem-cat (sem-class (copular)))
                      (syn-cat (lex-class (verb))
                               (syn-valence (subject ?subject-unit))))
                     (?subject-unit
                      (referent ?x)
                      (sem-cat (sem-class (physical-object)))
                      (syn-cat (not (clitic +))
                               (case (+ - -)))
                      --
                      (syn-cat (not (clitic +))
                               (case (+ - -)))))
                    :cxn-set argument-linking
                    :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn predicate-focus=prep-object-cxn ;;PREDICATE FOCUS = complement 
                    ((?verb-unit
                      (footprints ( predicate-focus))
                      (dependents (?complement-unit )))
                     <-
                     (?verb-unit
                      (referent ?ev)
                      (footprints (not predicate-focus))
                      (sem-cat (sem-function predicating))
                      (syn-cat (syn-valence (subject ?subject-unit)))
                      (HASH meaning ((predicate-focus ?ev ?y)))
                      --
                      (footprints (not predicate-focus))
                      (syn-cat (syn-valence (subject ?subject-unit)))
                      (HASH form ((precedes ?verb-unit ?object-unit ?verb-unit))))
                     (?object-unit
                      (referent ?y)
                      (sem-cat (sem-function referring-expression))
                      (syn-cat (syn-function nominal)
                               (case (prepositional-object)))
                      --
                      (syn-cat (syn-function nominal)
                               (syn-role ?syn-role)
                               (case (prepositional-object)))))
                    :cxn-set word-order
                    :disable-automatic-footprints t
                    :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn predicate-focus=complement-cxn ;;PREDICATE FOCUS = complement 
                    ((?verb-unit
                      (footprints ( predicate-focus))
                      (dependents (?complement-unit )))
                     <-
                     (?subject-unit
                      (referent ?y)
                      (syn-cat (syn-role subject)
                               (agreement (number ?nb)
                                          (gender ?gen)
                                          (person ?pers)))
                      --
                      (related (?verb-unit))
                      (syn-cat (syn-role subject)))
                     (?verb-unit
                      (referent ?ev)
                      (footprints (not predicate-focus))
                      (sem-cat (sem-function predicating)
                               (sem-class (copular)))
                      (syn-cat (syn-valence (subject ?subject-unit)))
                      (HASH meaning ((predicate-focus ?ev ?y)))
                      --
                      (footprints (not predicate-focus))
                      (sem-cat (sem-class (copular)))
                      (syn-cat (syn-valence (subject ?subject-unit)))
                      (HASH form ((precedes ?verb-unit ?complement-unit ?verb-unit))))
                     (?complement-unit
                      (referent ?y)
                      (sem-cat (sem-function referring-expression))
                      (syn-cat (syn-function nominal)
                               (case (+ - -))
                               (agreement (number ?nb)
                                          (gender ?gen)
                                          (person ?pers)))
                      --
                      (syn-cat (syn-function nominal)
                               (not (clitic +))
                               (case (+ - -)))))
                    :cxn-set word-order
                     :disable-automatic-footprints t
                    :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn relative-clause-cxn ;;right member of that
                                          ;;construction stands in the
                                          ;;predication relation to
                                          ;;the left member
                    ((?verb-unit
                      (syn-cat (restricted +)))
                     (?nominal-unit
                      (dependents (?verb-unit ?conjunction-unit)))
                     <-
                     (?nominal-unit
                      (sem-cat (sem-function referring-expression))
                      (referent ?x)
                      (related (?verb-unit))
                      --
                      (HASH form ((meets ?nominal-unit ?conjunction-unit ?nominal-unit)
                                  (precedes ?conjunction-unit ?verb-unit ?nominal-unit))))
                     (?conjunction-unit
                      (sem-cat (sem-class (subordinating)))
                      (referent ?x)
                      (HASH meaning ((elaboration ?x)))
                      --
                      (syn-cat (lex-class (conjunction)))
                      )
                     (?verb-unit
                      (sem-cat (sem-function predicating))
                      (referent ?ev)
                      (dependents (?nominal-unit) >> ())
                      --
                      (dependents (?nominal-unit) >> ())
                      (syn-cat (lex-class (verb)))))
                    :cxn-set triggers
                    :cxn-inventory *portuguese-grammar*)


(def-fcg-cxn active-intransitive-cxn
                    ((?verb-unit
                      (footprints (argument-structure-cxn)))
                     (?subject-unit
                      (related (?verb-unit))
                      (syn-cat (syn-role subject)))
      
                     <-
                     (?subject-unit
                      (referent ?causer)
                      (sem-cat (sem-class (animate))
                               (sem-function referring-expression))
                      --
                      (referent ?causer)
                      (sem-cat (sem-class (animate)))
                      (syn-cat (syn-function nominal)
                               (agreement (number ?n)
                                          (person ?p))
                               (case (+ - -))))
                     (?verb-unit 
                      (referent ?ev)
                      (footprints (not argument-structure-cxn))
                      (sem-cat (sem-function predicating)
                               (sem-valence (actor ?causer)))
                      --
                      (referent ?ev)
                      (sem-cat (not (sem-class (copular))))
                      (footprints (not argument-structure-cxn))
                      (syn-cat (lex-class (verb))
                               (agreement (number ?n)
                                          (person ?p))
                               (syn-valence (subject ?subject-unit)))))
                    :cxn-set argument-linking
                    :cxn-inventory *portuguese-grammar*
                    :score 0.4)

(def-fcg-cxn morph-contraction-cxn
             ((?direct-object
               (footprints (cliticization-cxn))
               (related (?same-verb)))
              (?indirect-object
               (dependents (?direct-object))
               (related (?same-verb)))
              
              <-
              (?indirect-object
               (referent ?io-ref)
               (syn-cat (clitic +)
                        (lex-class (pronoun))
                        (case (- - +)))
               --
               (HASH form ((meets ?indirect-object ?direct-object ?indirect-object)))
               (syn-cat (clitic +)
                        (lex-class (pronoun))
                        (case (- - +))))
              (?direct-object
               (footprints (not cliticization-cxn))
               (referent ?do-ref)
               (syn-cat (clitic +)
                        (lex-class (pronoun))
                        (case (- + -)))
               --
               (footprints (not cliticization-cxn))
               (syn-cat (clitic +)
                        (lex-class (pronoun))
                        (case (- + -)))))
             :cxn-set morph
             :disable-automatic-footprints t
             :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn enclisis-cxn
             ((?clitic-unit
               (footprints (cliticization-cxn)))
              (?verb-unit
               (dependents (?clitic-unit))
               (syn-cat (has-clitic +)))
               ;(form ((meets ?verb-unit ?clitic-unit ?verb-unit))))
              <-
              (?clitic-unit
               (footprints (not cliticization-cxn))
               (related (?verb-unit))
               (syn-cat (lex-class (pronoun))
                        (clitic +)
                        (case (- ?acc ?dat)))
               --
               (related (?verb-unit))
               (footprints (not cliticization-cxn))
               (syn-cat (lex-class (pronoun))
                        (clitic +)
                        (case (- ?acc ?dat))))
              (?verb-unit
               (sem-cat (not (sem-class (copular))))
               (syn-cat (lex-class (verb))
                        (not (restricted +))
                        ) ;(not (has-clitic +))
               --
               (HASH form ((meets ?verb-unit ?clitic-unit ?verb-unit)))
               (syn-cat (lex-class (verb))
                        (not (restricted +)))))
             :cxn-set cliticization
             :disable-automatic-footprints t
             :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn proclisis-cxn
         ((?verb-unit
           (dependents (?clitic-unit))
           (syn-cat (has-clitic +)))
          (?clitic-unit
           (footprints (cliticization-cxn)))
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
           (HASH form ((meets ?clitic-unit ?verb-unit ?verb-unit)))
           (syn-cat (restricted +)
                    (lex-class (verb)))))
         
    :cxn-set cliticization
    :disable-automatic-footprints t
    :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn prepositional-cxn ;;optional first arg??
                    ((?prep-unit
                      (footprints ( prepositional-cxn))
                      (dependents (?arg-2-unit)))
                     (?arg-1-unit
                      (dependents (?prep-unit)))
                     (?arg-2-unit
                      (syn-cat (case (prepositional-object))))
                     <- 
                     (?arg-1-unit
                      (referent ?arg-1)
                      (related (?verb-unit))
                      (sem-cat (sem-function referring-expression))
                      --
                      (referent ?arg-1)
                      
                      (syn-cat (syn-function nominal)
                               (not (reflexive +))))
                     (?prep-unit
                      (args (?arg-1 ?arg-2))
                      (syn-cat (lex-class (preposition)))
                      (footprints (not prepositional-cxn))
                      --
                      (footprints (not prepositional-cxn))
                      (HASH form ((precedes ?prep-unit ?arg-2-unit ?verb-unit)
                                  (meets ?arg-1-unit ?prep-unit ?verb-unit)))
                      (syn-cat (lex-class (preposition))))
                     (?arg-2-unit
                      (referent ?arg-2)
                      (sem-cat (sem-function referring-expression))
                      --
                      (referent ?arg-2)
                      (syn-cat (syn-function nominal)
                               (not (reflexive +)))))
                    :cxn-set argument-linking
                    :disable-automatic-footprints t
                    :score 0.6
                    :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn prepositional-cxn-2 ;;optional first arg??
                    ((?prep-unit
                      (footprints ( prepositional-cxn))
                      (referent ?arg-2)
                      (dependents (?arg-2-unit))
                      (syn-cat (case (prepositional-object))
                               (syn-role ?syn-role))
                      )
                     
                     (?arg-2-unit
                      (syn-cat (case (prepositional-object))))
                     <- 
                     (?prep-unit
                      (args ( ?arg-1 ?arg-2))
                      (syn-cat (lex-class (preposition)))
                      (footprints (not prepositional-cxn))
                      --
                      (footprints (not prepositional-cxn))
                      (HASH form ((precedes ?prep-unit ?arg-2-unit ?arg-2-unit))) ;better with boundaries?
                      (boundary (?init-prep ?end-prep))
                      (syn-cat (lex-class (preposition))))
                     (?arg-2-unit
                      (referent ?arg-2)
                      (sem-cat (sem-function referring-expression))
                      --
                      (referent ?arg-2)
                      (boundary (?end-prep ?end-arg-2))
                      (syn-cat (syn-function nominal)
                               )))
                    :cxn-set argument-linking
                    :disable-automatic-footprints t
                    :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn universal-quantification-cxn
                    ((?noun-unit
                      (footprints ( determination-cxn))
                      (syn-cat (syn-function nominal))
                      (dependents (?determiner-unit))
                      )
                     <-
                     (?determiner-unit
                      (referent ?y)
                      (syn-cat (lex-class (universal-quantifier)))
                      (sem-cat (sem-class (predeterminer)))
                      --
                      (sem-cat (sem-class (predeterminer)))
                      (syn-cat (lex-class (universal-quantifier))
                               (agreement (gender ?same-gender)
                                          (number ?same-number)
                                          (person ?same-person)))
                      (boundary (?init-det ?end-det)))
                     (?noun-unit
                      (footprints (not determination-cxn))
                      (referent ?y)
                      (sem-cat (sem-function referring-expression))
                      (boundary (?init-noun ?end-noun))
                      --
                      (footprints (not determination-cxn))
                      (syn-cat (lex-class (noun))
                               (agreement (gender ?same-gender)
                                          (number ?same-number)
                                          (person ?same-person)))
                      (boundary (?init-noun ?end-noun))
                      (HASH form ((precedes ?determiner-unit ?noun-unit ?noun-unit)))))
                    :cxn-set argument-linking
                    :disable-automatic-footprints t
                    :score 0.7
                    :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn infinitive-clause-w-direct-object-cxn
             ((?direct-object
               (related (?infinitive))
               (syn-cat (syn-role direct-object)))
              <-
              (?infinitive
               (referent ?event)
               (sem-cat (sem-valence (undergoer ?undergoer)))
               --
               (syn-cat (finite -)
                        (syn-valence (direct-object ?direct-object))))
              (?direct-object
               (referent ?undergoer)
               --
               (syn-cat (case (- + -)))
               ))
             :cxn-set argument-linking :score 0.6
             :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn object-complement-cxn
          ((?auxiliary
            (dependents (?complement)))
          ; (?main-verb
          ;  (syn-cat ));;infinitiefzin
           <-
           (?auxiliary
            (referent ?aux-event)
            (syn-cat (finite +))
            (sem-cat (sem-valence
                      (actor ?actor)
                      (undergoer ?main-event)))
            --
            (HASH form ((precedes ?auxiliary ?complement ?auxiliary)))
            (syn-cat (finite +)
                     (agreement (number ?n)
                                (person ?p))
                     (syn-valence
                      (subject ?subject-unit)
                      (direct-object ?main-verb))))
           (?complement
            (syn-cat (finite -))
            (referent ?main-event)
            (sem-cat (sem-valence
                      (actor ?actor)))
                      
            --
            (syn-cat (finite -)
                     (syn-valence
                      (subject ?subject-unit)))))
          :cxn-set argument-linking
          :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn cleft-cxn
          ((?aux-to-be
            (dependents (?predicate)))
           <-
           (?aux-to-be
            (referent ?referent)
            (sem-cat (sem-class (copular)))
            --
            (syn-cat (agreement
                      (person 3)
                      (number singular))
                     (finite +)
                     (lemma "ser")))
           (?predicate
            (referent ?referent)
            --
            (HASH form ((meets ?aux-to-be ?predicate ?aux-to-be)))))
          :cxn-set argument-linking
          :cxn-inventory *portuguese-grammar*)



(def-fcg-cxn medio-passive-intransitive-cxn
                    ((?verb
                      (syn-cat (syn-valence
                                (subject ?reflexive-pronoun)))
                      (footprints (argument-structure-cxn)))
                     (?reflexive-pronoun
                      (related (?verb))
                      (syn-cat (syn-role subject)))
                     <-
                     (?reflexive-pronoun ;;
                      (referent ?causer)
                      (syn-cat (reflexive +)
                               (lex-class (pronoun)))
                      --
                      (syn-cat (reflexive +)
                               (lex-class (pronoun))))
                     (?verb
                      (referent ?referent)
                      (footprints (not argument-structure-cxn))
                      (sem-cat (sem-valence (actor ?causer)))
                      (syn-cat (reflexive +)
                               (lex-class (verb)))
                      (HASH meaning ((medio-passive ?referent)))
                      --
                      (footprints (not argument-structure-cxn))
                      (HASH form ((meets ?reflexive-pronoun ?verb ?verb)))
                      (syn-cat (reflexive +)
                               (lex-class (verb)))))
                    :cxn-set argument-linking
                    :disable-automatic-footprints t
                    :score 0.6
                    :cxn-inventory *portuguese-grammar*)

(def-fcg-cxn reflexive-cxn
                    ((?verb
                      (syn-cat (syn-valence
                                (subject ?reflexive-pronoun)))
                      (footprints (argument-structure-cxn))
                      (dependents (?reflexive-pronoun)))
                     (?reflexive-pronoun
                      (related (?verb))
                      (syn-cat (syn-role subject)))
                     <-
                     (?reflexive-pronoun ;;
                      (referent ?causer)
                      (syn-cat (reflexive +)
                               (lex-class (pronoun)))
                      --
                      (syn-cat (reflexive +)
                               (lex-class (pronoun))))
                     (?verb
                      (referent ?referent)
                      (footprints (not argument-structure-cxn))
                      (sem-cat (sem-valence (actor ?causer)))
                      (syn-cat (reflexive +)
                               (lex-class (verb)))
                      (HASH meaning ((medio-passive ?referent)))
                      --
                      (footprints (not argument-structure-cxn))
                      (HASH form ((meets ?reflexive-pronoun ?verb ?verb)))
                      (syn-cat (reflexive +)
                               (lex-class (verb)))))
                    :cxn-set argument-linking
                    :disable-automatic-footprints t
                    :score 0.6
                    :cxn-inventory *portuguese-grammar*))


