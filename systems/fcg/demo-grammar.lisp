(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toy Grammar for Demos Purposes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;; This toy grammar serves in many demos.
;; It is used by FCG's (load-demo-grammar) function which can be used
;; to automatically load this grammar for tutorial purposes.

(def-fcg-constructions demo-grammar
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (constituents sequence)
                  (dependents sequence))
  :hierarchy-features (constituents dependents)
  
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
                 (HASH form ((string ?the-word  "the"))))))

  (def-fcg-cxn mouse-cxn
               ((?mouse-word
                 (args (?x))
                 (sem-cat (sem-class physical-entity))
                 (syn-cat (lex-class noun)))
                <-
                (?mouse-word
                 (HASH meaning ((mouse ?x)))                     
                 --
                 (HASH form ((string ?mouse-word  "mouse"))))))
  
  (def-fcg-cxn likes-cxn
               ((?likes-word
                 (args (?x ?y))
                 (sem-cat (sem-class relation))
                 (syn-cat (lex-class verb)
                          (type transitive)))
                <-
                (?likes-word
                 (HASH meaning ((deep-affection ?x ?y)))                     
                 --
                 (HASH form ((string ?likes-word  "likes"))))))
  
  (def-fcg-cxn linguist-cxn
               ((?linguist-word
                 (args (?x))
                 (sem-cat (sem-class physical-entity))
                 (syn-cat (lex-class noun)))
                <-
                (?linguist-word
                 (HASH meaning ((linguist ?x)))                     
                 --
                 (HASH form ((string ?linguist-word  "linguist"))))))
  
  ;;Grammatical Constructions
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
                 (HASH form ((meets ?article ?noun))))))
  
  ;; VP -> V
  (def-fcg-cxn verb-phrase-cxn
               ((?verb-phrase
                 (args (?x ?y))
                 (sem-cat (sem-class relational-expression))
                 (syn-cat (lex-class verb-phrase)
                          (type transitive))
                 (constituents (?verb)))
                <-
                (?verb
                 (args (?x ?y))
                 (sem-cat (sem-class relation))
                 --
                 (syn-cat (lex-class verb)
                          (type transitive)))))
  
  ;; Transitive-clause -> NP VP NP
  (def-fcg-cxn transitive-clause-cxn
               ((?transitive-clause
                 (args (?x ?y))
                 (sem-cat (sem-class predicating-expression))
                 (syn-cat (lex-class transitive-clause))
                 (constituents (?subject-noun-phrase ?verb-phrase ?object-noun-phrase)))
                (?verb
                 (dependents (?subject-noun ?object-noun)))
                <-
                (?subject-noun-phrase
                 (args (?x))
                 (sem-cat (sem-class referring-expression))
                 (constituents (?subject-article ?subject-noun))
                 --
                 (syn-cat (lex-class noun-phrase))
                 (constituents (?subject-article ?subject-noun)))
                (?verb-phrase
                 (args (?x ?y))
                 (sem-cat (sem-class relational-expression))
                 (constituents (?verb))
                 --
                 (syn-cat (lex-class verb-phrase)
                          (type transitive))
                 (constituents (?verb)))
                (?object-noun-phrase
                 (args (?y))
                 (sem-cat (sem-class referring-expression))
                 (constituents (?object-article ?object-noun))
                 --
                 (syn-cat (lex-class noun-phrase))
                 (constituents (?object-article ?object-noun)))
                (?transitive-clause
                 --
                 (HASH form ((meets ?subject-noun ?verb)
                             (meets ?verb ?object-article)))))))
