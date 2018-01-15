
(in-package :fcg)

(setf *fcg-constructions* (def-fcg-constructions init-cxn-inventory))

;;;;; Testing FCG (new notation)
;;;;; --------------------------------------------------------------------------------------

(deftest test-fcg-constructions ()
         ;; tests the main classes and methods
         (let* ((fcg-construction-set (make-instance 'fcg-construction-set
                                                     :feature-types '((form set-of-predicates)
                                                                      (meaning set-of-predicates)
                                                                      (subunits set)
                                                                      (args set))))
                (fcg-construction (make-instance 'fcg-construction
                                                 :name 'noun-phrase-cxn
                                                 :contributing-part (list (make-instance 'contributing-unit :name '?noun-phrase
                                                                                         :unit-structure '((args (?x))
                                                                                                           (sem-cat (sem-function referring-expression))
                                                                                                           (syn-cat (lex-class noun-phrase))
                                                                                                           (subunits (?noun)))))
                                                 :conditional-part (list (make-instance 'conditional-unit
                                                                                        :name '?noun
                                                                                        :formulation-lock '((HASH meaning ((success ?x))))      
                                                                                        :comprehension-lock '((HASH form ((string ?noun "success!"))))))
                                                 :attributes '((:label . cxn) (:score . 0.5))
                                                 :cxn-inventory fcg-construction-set)))
           (setf (processing-cxn-inventory fcg-construction-set)
                 (make-instance 'construction-set))
           (setf (configuration fcg-construction-set) (configuration (processing-cxn-inventory fcg-construction-set)))
           (test-assert (eq 'fcg-construction (type-of fcg-construction)))
           (test-assert (eql'fcg-construction-set (type-of fcg-construction-set)))
           (test-assert (add-cxn fcg-construction fcg-construction-set))
           (test-assert (= 1 (length (constructions fcg-construction-set))))
           (test-assert (find-cxn fcg-construction fcg-construction-set))
           (test-assert (find-cxn fcg-construction (constructions fcg-construction-set)))
           (test-assert (eq (configuration fcg-construction-set) (configuration (processing-cxn-inventory fcg-construction-set))))
           (test-assert (= 1 (length (constructions (processing-cxn-inventory fcg-construction-set)))))
           (test-assert (unify '((success x)) (comprehend '("success!") :cxn-inventory fcg-construction-set :silent t)))
           (test-assert (equal '("success!") (formulate '((success x)) :cxn-inventory fcg-construction-set :silent t)))
           (test-assert (delete-cxn fcg-construction fcg-construction-set))
           (test-assert (null (constructions fcg-construction-set)))
           (test-assert (null (constructions (processing-cxn-inventory fcg-construction-set))))))

;; (test-fcg-constructions)

(eval-when (:execute)
  (deftest test-fcg-macros ()
    ;; tests the correct working of the macros
    (let* ((fcg-construction-set nil)
           (fcg-construction-set (def-fcg-constructions (make-id 'some-test-set)
                                   :cxn-inventory fcg-construction-set
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
                                   (def-fcg-cxn noun-phrase-cxn
                                                ((?noun-phrase
                                                  (args (?x))
                                                  (sem-cat (sem-class referring-expression))
                                                  (syn-cat (lex-class noun-phrase))
                                                  (subunits (?article ?noun))
                                                  (left-most-subunit ?article)
                                                  (right-most-subunit ?noun))
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
                                   (def-fcg-cxn verb-phrase-cxn
                                                ((?verb-phrase
                                                  (args (?x ?y))
                                                  (sem-cat (sem-class relational-expression))
                                                  (syn-cat (lex-class verb-phrase)
                                                           (type transitive))
                                                  (subunits (?verb)))
                                                 <-
                                                 (?verb
                                                  (args (?x ?y))
                                                  (sem-cat (sem-class relation))
                                                  --
                                                  (syn-cat (lex-class verb)
                                                           (type transitive)))))
                                   (def-fcg-cxn transitive-clause-cxn
                                                ((?transitive-clause
                                                  (args (?x ?y))
                                                  (sem-cat (sem-class predicating-expression))
                                                  (syn-cat (lex-class transitive-clause))
                                                  (subunits (?subject-noun-phrase ?verb-phrase ?object-noun-phrase)))
                                                 <-
                                                 (?subject-noun-phrase
                                                  (args (?x))
                                                  (sem-cat (sem-class referring-expression))
                                                  --
                                                  (syn-cat (lex-class noun-phrase))
                                                  (right-most-subunit ?noun))
                                                 (?verb-phrase
                                                  (args (?x ?y))
                                                  (sem-cat (sem-class relational-expression))
                                                  --
                                                  (syn-cat (lex-class verb-phrase)
                                                           (type transitive))
                                                  (subunits (?verb)))
                                                 (?object-noun-phrase
                                                  (args (?y))
                                                  (sem-cat (sem-class referring-expression))
                                                  --
                                                  (syn-cat (lex-class noun-phrase))
                                                  (left-most-subunit ?article))
                                                 (?transitive-clause
                                                  --
                                                  (HASH form ((meets ?noun ?verb)
                                                              (meets ?verb ?article)))))))))
      (test-assert (eq 7 (length (constructions fcg-construction-set))))
      (test-assert (eq 7 (length (constructions (processing-cxn-inventory fcg-construction-set)))))
      (test-assert (equal (feature-types fcg-construction-set)
                          '((form set-of-predicates) (meaning set-of-predicates) (subunits set) (args sequence)
                            (boundaries set-of-predicates) (footprints set))))
      (test-assert (eq (type-of fcg-construction-set) 'fcg-construction-set))
      ;;test added as an alternative to use irl:unify-irl-programs
      (test-assert (equivalent-meaning? (comprehend '("the" "mouse" "likes" "the" "linguist") :cxn-inventory fcg-construction-set :silent t)
                                        '((unique x) (mouse x) (unique y) (linguist y) (deep-affection x y))
                                        :unify))
      (test-assert (equal '("the" "mouse" "likes" "the" "linguist")
                          (formulate '((mouse o) (unique o) (linguist p) (unique p) (deep-affection o p))  :cxn-inventory fcg-construction-set))))))

;; (test-fcg-macros)

(eval-when (:execute)
  (deftest test-expansion-operator ()
    ;; tests the correct translation of the expansion operator into FCG 2
    (let* ((fcg-construction-set nil)
           (fcg-construction-set (def-fcg-constructions (make-id 'another-test-set)
                                   :cxn-inventory fcg-construction-set
                                   (def-fcg-cxn pensar-lex
                                                ((?pensar-stem
                                                  (stressed (?boolean)))
                                                 <-
                                                 (?pensar-stem
                                                  (HASH meaning ((think ?ev) (thinker ?ev ?p)))
                                                  --
                                                  (syn-cat (lemma "pensar"))))
                                                :feature-types ((stressed sequence :stressed-stem?))))))
      ;;test added as an alternative to use irl:unify-irl-programs
      (test-assert (equal '((?BOOLEAN))
                          (set-difference '(++ :STRESSED-STEM? (?BOOLEAN))
                                          (second (third (second (left-pole-structure
                                                                  (get-processing-cxn (first (constructions fcg-construction-set)))))))
                                          :test 'equal))))))

;; (test-expansion-operator)
