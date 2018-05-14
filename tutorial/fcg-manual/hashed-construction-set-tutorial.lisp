;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;; How to use hashed construction sets                                ;;
;; April 2018 - Katrien Beuls                                         ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This tutorial explains how to use hashed construction sets instead
;; of regular list-like construction sets that store their
;; constructions in the 'constructions' slot. Instead, hashed
;; construction sets store their constructions in a hash table, with
;; keys that are typically the string and the meaning of the
;; constructions. The NIL bucket (key) hosts all constructions that do
;; not have a meaning/string predicate, typically grammatical
;; constructions.

(ql:quickload :fcg)
(in-package :fcg)

;;The def-fcg-constructions macro has a keyword that allows you to
;;created a hashed construction set: ':hashed'. By default, its value
;;is set to NIL. You can set this to T if you want to start hashing
;;your constructions.

;;The grammar fragment below is based on the FCG demo grammar but only
;;has three constructions:
(def-fcg-constructions hashed-construction-set-tutorial
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (constituents sequence)
                  (dependents sequence))
  :hierarchy-features (constituents dependents)
  :hashed t
  
  ;; Lexical constructions
  (def-fcg-cxn the-cxn
               ((?the-word
                 (args (?x))
                 (sem-cat (sem-class referent))
                 (syn-cat (lex-class article)))
                <-
                (?the-word
                 (HASH meaning ((unique ?x)))                     
                 --
                 (HASH form ((string ?the-word  "the")))))
                :attributes (:string "the" ;;add string and meaning to the attributes to use as hashkeys
                             :meaning unique))

  (def-fcg-cxn mouse-cxn
               ((?mouse-word
                 (args (?x))
                 (sem-cat (sem-class physical-entity))
                 (syn-cat (lex-class noun)))
                <-
                (?mouse-word
                 (HASH meaning ((mouse ?x)))                     
                 --
                 (HASH form ((string ?mouse-word  "mouse")))))
                :attributes (:string "mouse"
                             :meaning mouse))
  
  ;;Grammatical Construction
  ;; NP -> ART NOUN
  (def-fcg-cxn noun-phrase-cxn
               ((?noun-phrase
                 (args (?x))
                 (sem-cat (sem-class referring-expression))
                 (syn-cat (lex-class noun-phrase))
                 (constituents (?article ?noun)))
                (?noun
                 (dependents (?article)))
                <-
                (?article
                 (args (?x))
                 (sem-cat (sem-class referent))
                 --
                 (syn-cat (lex-class article)))
                (?noun
                 (args (?x))
                 (sem-cat (sem-class physical-entity))                   
                 --
                 (syn-cat (lex-class noun)))
                (?noun-phrase
                 --
                 (HASH form ((meets ?article ?noun))))));;if no attributes are specified, the cxn will be put in the NIL bucket
  ) 



(activate-monitor trace-fcg)
(comprehend-and-formulate '("the" "mouse"))

;;number of hash keys:
(size *fcg-constructions*)
;;the constructions list stays empty:
(constructions *fcg-constructions*)

;;delete a construction:
(delete-cxn (find-cxn 'the-cxn *fcg-constructions* :hash-key "the") *fcg-constructions*)
(delete-cxn (find-cxn 'noun-phrase-cxn *fcg-constructions*) *fcg-constructions*)

;;check size again:
(size *fcg-constructions*)