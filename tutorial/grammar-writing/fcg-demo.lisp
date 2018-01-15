
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEMONSTRATION OF FLUID CONSTRUCTION GRAMMAR ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; 1. Load and set up FCG (including FCG 2)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:operate 'asdf:load-op :fcg)
(in-package :fcg)
(activate-monitor trace-fcg)

;;;; 2. Load the following FCG Light Example Grammar
;;;;    by executing def-fcg-light-constructions
;;;;    and def-fcg-light-cxn definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions simple-english-grammar
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

;;;; 3.  Start comprehending and formulating!
;;;;    Enjoy following up every step in the process in the
;;;;    brand new FCG Light visualisation (open a web browser
;;;;    at http://localhost:8000)!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(comprehend '("the" "mouse"))

(comprehend '("the" "mouse" "likes" "the" "linguist"))
(comprehend '("the" "linguist" "likes" "the" "mouse"))

;; The mouse likes the linguist
(formulate '((unique x) (mouse x) (unique y) (linguist y) (deep-affection x y)))
;; The linguist likes the mouse
(formulate '((unique x) (mouse x) (unique y) (linguist y) (deep-affection y x)))

;; This grammar exploits the 'multiple perspectives' feature of FCG and supports
;; a visualisation of both constituent and dependency structure. Switch between
;; the perspectives in the web-interface by hoovering over the final transient
;; structure and click on the h1 and h2 buttons that appear.

;;;; 4. Print the FCG Light constructions to LaTeX
;;;;    (see output browser)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fcg-light-construction-set->latex *fcg-constructions*)
;; -> copy the resulting LaTeX code into your paper file... Compile!
;; OR
;; click in the webinterface on the little L appearing when you hover with your mouse over a construction

;;;; 6. Common errors in grammar writing trigger warnings
;;;;    or, if necessary, explicative errors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -> Warning for unit-name which is not a variable (for root, no warning is given)
(def-fcg-cxn verb-phrase-cxn
             ((?verb-phrase
               (args (?x ?y))
               (sem-cat (sem-class relational-expression))
               (syn-cat (lex-class verb-phrase)
                        (type transitive))
               (constituents (?verb)))
              ->
              (verb
               (args (?x ?y))
               (sem-cat (sem-class relation))
               --
               (syn-cat (lex-class verb)
                        (type transitive)))))

;; -> Warning for the use of two times the same unit name in the conditional or contributing part
(def-fcg-cxn transitive-clause-cxn
             ((?transitive-clause
               (args (?x ?y))
               (sem-cat (sem-class predicating-expression))
               (syn-cat (lex-class transitive-clause))
               (constituents (?subject-noun-phrase ?verb-phrase ?object-noun-phrase)))
              ->
              (?noun-phrase
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
              (?noun-phrase
               (args (?y))
               (sem-cat (sem-class referring-expression))
               (constituents (?object-article ?object-noun))
               --
               (syn-cat (lex-class noun-phrase))
               (constituents (?object-article ?object-noun)))
              (?transitive-clause
               --
               (HASH form ((meets ?subject-noun ?verb)
                           (meets ?verb ?object-article))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; You can choose to which cxn-inventory to add ;;
;; your constructions !                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You can specify the cxn-inventory-name inside the def-fcg-constructions macro. A new
;; fcg-construction-set will be created. def-fcg-cxn macros inside the def-fcg-constructions macro
;; will automatically be added to this inventory.
;; For def-fcg-cxn macros outside the def-fcg-constructions macro, you will have to specify the cxn-inventory
;; with the keyword :cxn-inventory

(def-fcg-constructions simple-english-grammar
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (constituents sequence))
  :cxn-inventory *my-cxn-inv*
  
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
               (HASH form ((string ?mouse-word  "mouse")))))))

;;Grammatical Constructions
;; NP -> ART NOUN
;; Outside def-fcg-constructions macro -> cxn-inventory keyword needed
(def-fcg-cxn noun-phrase-cxn
             ((?noun-phrase
               (args (?x))
               (sem-cat (sem-class referring-expression))
               (syn-cat (lex-class noun-phrase))
               (constituents (?article ?noun)))
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
               (HASH form ((meets ?article ?noun)))))
             :cxn-inventory *my-cxn-inv*)

(comprehend '("the" "mouse") :cxn-inventory *my-cxn-inv*)
(formulate '((mouse x) (unique x)) :cxn-inventory *my-cxn-inv*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; You can add additional attributes to your cxn;;                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-fcg-constructions dutch-gender
  (def-fcg-cxn hond-cxn
               ((?hond-unit
                 (syn-cat (lex-class noun)
                          (gender m))
                 (referent ?x)
                 (sem-cat (tangible +)
                          (animate +)
                          (countable +)))
                <-
                (?hond-unit
                 (HASH meaning ((dog ?x)))
                 --
                 (HASH form ((string ?hond-unit "hond")))))
               :attributes (:match 1 :mismatch 0 :gender m :label lex))

  (def-fcg-cxn cat-cxn
               ((?kat-unit
                 (syn-cat (lex-class noun)
                          (gender f))
                 (referent ?x)
                 (sem-cat (tangible +)
                          (animate +)
                          (countable +)))
                <-
                (?kat-unit
                 (HASH meaning ((cat ?x)))
                 --
                 (HASH form ((string ?kat-unit "kat")))))
               :cxn-set lex
               :score 1.0
               :attributes (:match 3 :mismatch 0 :gender f)))

(comprehend '("hond"))
(comprehend '("kat"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; You can also use a construction like this:   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-cxn (make-instance 'fcg-construction
                        :name 'noun-phrase-cxn
                        :contributing-part (list (make-instance 'contributing-unit
                                                                :name '?noun-phrase
                                                                :unit-structure '((args (?x))
                                                                                  (sem-cat (sem-class referring-expression))
                                                                                  (syn-cat (lex-class noun-phrase))
                                                                                  (constituents (?article ?noun)))))
                        :conditional-part (list (make-instance 'conditional-unit
                                                               :name '?article
                                                               :formulation-lock '((args (?x))
                                                                                   (sem-cat (sem-class referent)))
                                                               :comprehension-lock '((syn-cat (lex-class article))))
                                                (make-instance 'conditional-unit
                                                               :name '?noun
                                                               :formulation-lock '((args (?x))
                                                                                   (sem-cat (sem-class physical-entity)))      
                                                               :comprehension-lock '((syn-cat (lex-class noun))))
                                                (make-instance 'conditional-unit
                                                               :name '?noun-phrase
                                                               :formulation-lock nil
                                                               :comprehension-lock '((HASH form ((meets ?article ?noun))))))
                        :attributes '((:label . cxn) (score . 0.5))
                        :cxn-inventory *fcg-constructions*)
         *fcg-constructions*)

(delete-cxn (find-cxn 'noun-phrase-cxn *fcg-constructions*  :key #'name) *fcg-constructions*)

