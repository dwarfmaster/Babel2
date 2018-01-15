;(asdf:operate 'asdf:load-op 'portuguese-grammar)

(in-package :fcg)

;;####################
;; PREPOSITIONS
;;####################

(defun create-prepositional-cxn (lemma &key meaning sem-class
                                       (cxn-inventory *propor-grammar*))
  (let* ((cxn-name (make-symbol (mkstr (upcase lemma) '-prep-cxn)))
         (unit-name (make-var (mkstr (upcase lemma) '-unit)))
         (referent-rel (make-var 'rel)))

    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          ;(referent ,referent-rel)
                          (args (?arg-1 ?arg-2))
                          (sem-cat (sem-class (,@sem-class)))
                          (syn-cat (lex-class (preposition)))
                          (boundary (?init ?end)))
                         <-
                         (,unit-name
                          (HASH meaning ((,meaning  ?arg-1 ?arg-2)))
                          --
                          (HASH form ((string ,unit-name ,lemma))))
                         (root
                          --
                          (boundaries ((,unit-name ?init ?end)))))
                        :cxn-set lex
                        :cxn-inventory ,cxn-inventory))))

;;####################
;; DETERMINERS
;;####################

(defun create-determiner-lemma-cxn (lemma &key meaning lex-class definiteness
                                          (cxn-inventory *propor-grammar*))
  "Automatically create a lemma for determiners based on their lemma,
meaning and lex-class (quantifier, )"
  (let* ((cxn-name (make-symbol (mkstr (upcase lemma) '-determiner-cxn)))
         (unit-name (make-var (mkstr (upcase lemma) '-unit)))
         (referent (make-var 'o))
         (meaning-predicates (loop for pred in meaning
                                   collect (list pred referent))))
    (if (eq lex-class 'universal-quantifier)
      (eval `(def-fcg-cxn ,cxn-name
               ((,unit-name
                 (referent ,referent)
                 (boundary (?init ?end)))
                <-
                (,unit-name
                 (HASH meaning ,meaning-predicates)
                 --
                 (sem-cat (sem-class (predeterminer)))
                 (syn-cat (lex-class (,lex-class)) ;;what about demonstratives?
                          ,@(when definiteness `((definiteness ,definiteness)))
                          (lemma ,lemma)))
                (root
                 --
                 (boundaries ((,unit-name ?init ?end)))))
                          :cxn-set lex :cxn-inventory ,cxn-inventory))
      (eval `(def-fcg-cxn ,cxn-name
                          ((,unit-name
                            (referent ,referent)
                            (boundary (?init ?end)))
                           <-
                           (,unit-name
                            (HASH meaning ,meaning-predicates)
                            --
                            (sem-cat (sem-class (determiner)))
                            (syn-cat (lex-class (,lex-class)) ;;what about demonstratives?
                                     ,@(when definiteness `((definiteness ,definiteness)))
                                     (lemma ,lemma)))
                           (root
                            --
                            (boundaries ((,unit-name ?init ?end)))))
                          :cxn-set lex :cxn-inventory ,cxn-inventory)))))

(deftest test-create-determiner-lemma-cxn ()
  (let* ((init-grammar (make-proclisis-extended-cxns))
         (init-length (length (constructions init-grammar))))
    (multiple-value-bind (new-grammar new-cxn)
        (create-determiner-lemma-cxn "pouco" :meaning '(few) :lex-class 'quantifier
                                     :cxn-inventory init-grammar)
      (test-assert (= (length (constructions new-grammar)) (+ init-length 1)))
      (test-assert new-cxn)
      (let ((test-transient-structure
             (create-initial-structure '((few o-1)) :one-pole-mode)))
        (test-assert (fcg-apply (get-processing-cxn new-cxn) 
                                test-transient-structure '->))))))

(test-create-determiner-lemma-cxn)

(defun create-determiner-morph-cxn (lemma morph &key lex-class number gender
                                          floating downward definiteness
                                          (cxn-inventory *propor-grammar*))
  "Automatically create a morph cxn for determiners based on their lemma, morph
and a number of syntactic features"
  (let ((cxn-name (make-symbol (mkstr (upcase lemma) '- (upcase morph) '-det-cxn)))
        (unit-name (make-var (mkstr (upcase morph) '-unit))))
    (if (eq lex-class 'universal-quantifier)
      (eval `(def-fcg-cxn ,cxn-name
                         ((,unit-name
                           (sem-cat (sem-class (predeterminer)))
                           (syn-cat ,@(when floating `((floating ,floating)))
                                    ,@(when downward `((downward ,downward)))
                                    ,@(when definiteness `((definiteness ,definiteness)))))
                          <-
                          (?noun-unit
                           (referent ?ref)
                           (syn-cat (syn-function nominal)
                                    (lex-class (noun)))
                           (sem-cat (sem-function referring-expression))
                           --
                           (syn-cat (syn-function nominal)
                                    (lex-class (noun))))
                          (,unit-name
                           (referent ?ref)
                           (syn-cat (lex-class (,lex-class))
                                    (lemma ,lemma)
                                    (agreement (number ,number)
                                               (gender ,gender)
                                               (person 3)))
                           --
                           (HASH form ((string ,unit-name ,morph))))
                          (root
                           --
                           (form ((precedes ,unit-name ?noun-unit ?scope)))))
               
                         :cxn-set morph :cxn-inventory ,cxn-inventory :score 0.6))
     (eval `(def-fcg-cxn ,cxn-name
                         ((,unit-name
                           (sem-cat (sem-class (determiner)))
                           (syn-cat ,@(when floating `((floating ,floating)))
                                    ,@(when downward `((downward ,downward)))
                                    ,@(when definiteness `((definiteness ,definiteness)))))
                          <-
                          (?noun-unit
                           (referent ?ref)
                           (syn-cat (syn-function nominal)
                                    (lex-class (noun)))
                           (sem-cat (sem-function referring-expression))
                           --
                           (syn-cat (syn-function nominal)
                                    (lex-class (noun))))
                          (,unit-name
                           (referent ?ref)
                           (syn-cat (lex-class (,lex-class))
                                    (lemma ,lemma)
                                    (agreement (number ,number)
                                               (gender ,gender)
                                               (person 3)))
                           --
                           (HASH form ((string ,unit-name ,morph))))
                          (root
                           --
                           (form ((precedes ,unit-name ?noun-unit ?scope)))))
               
                         :cxn-set morph :cxn-inventory ,cxn-inventory :score 0.6)))))

(deftest test-create-determiner-morph-cxn ()
  (let* ((init-grammar (make-proclisis-extended-cxns))
         (init-length (length (constructions init-grammar))))
    (multiple-value-bind (new-grammar new-cxn)
        (create-determiner-morph-cxn "pouco" "poucas":number 'plural :gender 'feminine
                                     :floating '+ :downward '+ :lex-class 'quantifier
                                     :cxn-inventory init-grammar)
      (multiple-value-bind (new-grammar new-cxn-2)
          (create-noun-cxn "palavras" :number 'plural
                           :sem-class '(physical-object tangible inanimate)
                           :meaning '(book plural) :gender 'feminine
                           :cxn-inventory new-grammar)
        (test-assert (= (length (constructions new-grammar)) (+ init-length 2)))
        (test-assert new-cxn)
        (let* ((test-transient-structure
                (de-render '("poucas" "palavras") :de-render-with-scope))
               (noun-unit-name (second (fcg-get-sequence test-transient-structure)))
               (noun-unit
                `(,noun-unit-name (syn-cat ((syn-function nominal) (lex-class (noun))))
                                 (sem-cat nil)
                                 (referent nil)
                                 (footprints nil))))
          (setf (left-pole-structure test-transient-structure)
                (push noun-unit (left-pole-structure test-transient-structure)))
          (test-assert (fcg-apply (get-processing-cxn new-cxn)
                                  test-transient-structure '<-)))))))

(test-create-determiner-morph-cxn)


;;####################
;; ADVERBS
;;####################
(defun create-adverb-cxn (adverb &key sem-class meaning operator-like negation 
                                 (position (make-var 'pos)) (cxn-inventory *propor-grammar*)) 
  (let* ((cxn-name (make-symbol (mkstr (upcase adverb) '-cxn)))
         (unit-name (make-var (mkstr (upcase adverb) '-unit)))
         (referent-var (make-var "REF")))
    (eval `(def-fcg-cxn ,cxn-name
               ((,unit-name
                 (referent ,referent-var)
                 (sem-cat (sem-class (,@sem-class)))
                 (syn-cat (lex-class (adverb))
                          (negation ,negation)
                          (position ,position)
                          (operator-like ,operator-like))
                 (boundary (?init ?end)))
                <-
                (,unit-name
                 (HASH meaning ((,meaning ,referent-var)))
                 --
                 (HASH form ((string ,unit-name ,adverb))))
                (root
                 --
                 (boundaries ((,unit-name ?init ?end)))))
               :cxn-set lex :cxn-inventory ,cxn-inventory))))

(deftest test-create-adverb-cxn ()
  (let* ((init-grammar (make-proclisis-extended-cxns))
         (init-length (length (constructions init-grammar))))
    (multiple-value-bind (new-grammar new-cxn)
        (create-adverb-cxn "raramente" :sem-class '(temporal) :meaning 'rarely :operator-like '+
                           :negation '- :cxn-inventory init-grammar)
      (test-assert (= (length (constructions new-grammar)) (+ init-length 1)))
      (test-assert new-cxn)
      (let ((test-transient-structure<-
             (de-render '("raramente") :de-render-with-scope))
            (test-transient-structure->
             (create-initial-structure '((rarely x-1)) :one-pole-mode)))
        (test-assert (fcg-apply (get-processing-cxn new-cxn) 
                                test-transient-structure<- '<-))
        (test-assert (fcg-apply (get-processing-cxn new-cxn) 
                                test-transient-structure-> '->))))))

(test-create-adverb-cxn)

;;####################
;; VERBS
;;####################

(defparameter *possible-args* '(x y z w a b c))

(defun create-verb-lemma-cxn (lemma &key meaning sem-class
                                         (conj-class 'irregular) reflexive?
                                         (cxn-inventory *propor-grammar*))
  ;;e.g. meaning = (give giver gift givee)
 "Automatically creates a lexical construction for the given verb
lemma (string) based on a number of criteria such as the valency
(number of arguments), meaning (list of event meaning + event
participants), sem-class, conj-class (regular, ar, er, ir) and
reflexive? (+, -)"
 
 (let* ((cxn-name (make-symbol (mkstr (upcase lemma) '-cxn)))
        (unit-name (make-var (mkstr (upcase lemma) '-unit)))
        (reflexive? (if reflexive? '+ '-))
        (nr-of-args (- (length meaning) 1))
        (arguments (loop for i from 0 to nr-of-args
                         collect (make-var (nth i *possible-args*))))
        (event-arg (first arguments))
        
        (meaning-predicates
         `(,(list (first meaning) event-arg)
           ,@(loop for pred in (rest meaning)
                     for i from 1
                     collect (list pred event-arg (nth i arguments)))))
        (syn-valence
         (loop with syn-val = (if (equalp '(transfer-included) sem-class) ;;DO in verb included
                                '(subject indirect-object)
                                '(subject direct-object indirect-object))
               for i from 0 to (- (length arguments) 2) ;;args includes event itself
               collect (list (nth i syn-val) (make-var (nth i syn-val)))))
        (sem-valence
         (loop with sem-val = (if (equalp '(transfer-included) sem-class) ;;undergoer in verb included
                                '(actor receiver)
                                '(actor undergoer receiver))
               for i from 0 to (- (length arguments) 2)
               collect (list (nth i sem-val) (nth (+ i 1) arguments)))))
        
                               
   (eval `(def-fcg-cxn ,cxn-name
            ((,unit-name
              (referent ,event-arg)
              (args ,arguments)
              (syn-cat (lex-class (verb))
                       (conj-class ,conj-class)
                       (tense ?t)
                       (reflexive ,reflexive?)
                       (mood ?m)
                       (syn-valence ,@syn-valence))
              (sem-cat (sem-class (,@sem-class))
                       (sem-valence ,@sem-valence)))
             <-
             (,unit-name
              (HASH meaning ,meaning-predicates)
              --
              (syn-cat (lemma ,lemma))))
            :cxn-set lex
            :cxn-inventory ,cxn-inventory))))

(deftest test-create-verb-lemma-cxn ()
  (let* ((init-grammar (make-proclisis-extended-cxns))
         (init-length (length (constructions init-grammar))))
    (multiple-value-bind (new-grammar new-cxn)
        (create-verb-lemma-cxn "dar" :meaning '(give giver gift givee) :sem-class '(transfer-included give)
                                 :cxn-inventory init-grammar)
      (test-assert (= (length (constructions new-grammar)) (+ init-length 1)))
      (test-assert new-cxn)
      (let ((test-transient-structure
             (create-initial-structure '((give ev-1) (giver ev-1 p-1) (gift ev-1 o-1)
                                         (givee ev-1 p-2)) :one-pole-mode)))
        (test-assert (fcg-apply (get-processing-cxn new-cxn) 
                                test-transient-structure '->))))))


(test-create-verb-lemma-cxn)

(defparameter *tense-meanings*
  '((present overlaps)
    (past before)
    (future after)))

(defparameter *person-number-meanings*
  '(act-done-by-me act-done-by-you act-done-by-him/her
    act-done-by-us act-done-by-you-pl act-done-by-them))


(defun create-verb-morph-cxn (lemma verb-form
                               &key person number
                               tense (mood (make-var "MOOD"))
                               sem-class (conj-class 'irregular)
                               (cxn-inventory *propor-grammar*))
  
  (assert (or (variable-p number) (member number '(singular plural))))
  (assert (or (variable-p person) (numberp person)))
  (assert (or (variable-p tense) (member tense '(past present future conditional))))
  (assert (or (variable-p conj-class) (member conj-class '(ar er ir irregular))))
  
  (let* ((cxn-name (make-symbol (mkstr (upcase lemma) '- (upcase verb-form)
                                       '- (upcase person) '- (upcase number) '-cxn)))
         (unit-name (make-var (mkstr (upcase verb-form) '-unit)))
         (event-var (make-var "EVENT"))
         (time-point-var (make-var "ORIGO"))
         (actor-var (make-var "ACTOR"))
         (person/number-meaning
          (unless (and (variable-p number)
                       (variable-p person))
            (nth (case number ;;person/number-meaning
                   (fcg::singular (- person 1))
                   (fcg::plural (- (+ person 3) 1))) *person-number-meanings*)))
         (meaning-predicates
          (when person/number-meaning
          `(,(list person/number-meaning event-var actor-var)
            ,@(list `(deictic-time-point ,time-point-var)
                     `(,@(assqv tense *tense-meanings*) ,event-var ,time-point-var))))))
    (when meaning-predicates ;;currently no cxn added if there is no meaning
      ;;TO DO use finite feature!
  (eval `(def-fcg-cxn ,cxn-name
                      ((,unit-name
                        (footprints (morph))
                        (sem-cat (sem-valence (actor ,actor-var))
                                 (sem-function predicating))
                        (syn-cat (agreement (person ,person)
                                            (number ,number))
                               ;  (lemma ,lemma)
                                 (tense ,tense)
                                 (mood ,mood)
                                 (finite +)))
                       <-
                       (,unit-name
                        ;(sem-cat (sem-class (,@sem-class)))
                        (syn-cat (conj-class ,conj-class)
                                 (lex-class (verb))
                                 (lemma ,lemma))
                        (HASH meaning ,meaning-predicates)
                        (footprints (not morph))
                        --
                        (HASH form  ((string ,unit-name ,verb-form)))))
                      :cxn-set lex
                      :disable-automatic-footprints t
                      :cxn-inventory ,cxn-inventory)))))

(deftest test-create-verb-morph-cxn ()
  (let* ((init-grammar (make-proclisis-extended-cxns))
        (init-length (length (constructions init-grammar))))
    (multiple-value-bind (new-grammar new-cxn)
        (create-verb-morph-cxn "dar" "dou" :person 1 :number 'singular :sem-class '(transfer)
                               :tense 'present :conj-class 'irregular
                               :cxn-inventory init-grammar)
      (test-assert (= (length (constructions new-grammar)) (+ init-length 1)))
      (test-assert new-cxn)
      (let ((test-transient-structure
             (de-render '("dou") :de-render-with-scope)))
        (test-assert (fcg-apply (get-processing-cxn new-cxn) 
                                test-transient-structure '<-))))))

(test-create-verb-morph-cxn)


(defun create-infinitive-morph-cxn (lemma &key sem-class
                                         (conj-class 'irregular) (reflexive? '-)
                                         (cxn-inventory *propor-grammar*))
  ;;e.g. meaning = (give giver gift givee)
 "Automatically creates a lexical construction for the given verb
lemma (string) based on a number of criteria such as the valency
(number of arguments), meaning (list of event meaning + event
participants), sem-class, conj-class (regular, ar, er, ir) and
reflexive? (+, -)"
 
 (let* ((cxn-name (make-symbol (mkstr (upcase lemma) '- (upcase lemma) '-cxn)))
        (unit-name (make-var (mkstr (upcase lemma) '-unit)))  )
                               
   (eval `(def-fcg-cxn ,cxn-name
            ((,unit-name
              (sem-cat (sem-function predicating))
              (footprints (morph))
              (syn-cat (finite -)))
             <-
             (,unit-name
              (footprints (not morph))
              (syn-cat (conj-class ,conj-class)
                       (lex-class (verb))
                       (lemma ,lemma))
          ;    (HASH meaning ()
              --
              (HASH form  ((string ,unit-name ,lemma)))))
            :cxn-set lex
            :score 0.2
            :disable-automatic-footprints t
            :cxn-inventory ,cxn-inventory))))



(deftest test-interaction-verb-lemma-and-morph ()
;;to do
  )

;;####################
;; NOUNS
;;####################

(defun create-noun-cxn (noun &key (person 3) number gender sem-class meaning lex-class
                             (cxn-inventory *propor-grammar*)) ;;(book single)
  (assert (or (variable-p number) (member number '(singular plural))))
  (assert (or (variable-p person) (member person '(1 2 3))))
  (assert (or (variable-p gender) (member gender '(masculine feminine))))
  (assert (listp sem-class))
  (let* ((cxn-name (make-symbol (mkstr (upcase noun) '-cxn)))
         (unit-name (make-var (mkstr (upcase noun) '-unit)))
         (referent-var (make-var "REF"))
         (meaning-predicates (loop for pred in meaning
                                   collect (list pred referent-var))))
    (eval 
     `(def-fcg-cxn ,cxn-name
                   ((,unit-name
                     (referent ,referent-var)
                     (syn-cat (lex-class (noun ,lex-class)) ;;what with proper/common noun?
                              (syn-function nominal)
                              (agreement (number ,number)
                                         (gender ,gender)
                                         (person ,person))
                      (case ?undefined))
                     (sem-cat (sem-class (,@sem-class)) ;;list
                              (sem-function referring-expression))
                     (boundary (?init ?end)))
                    <-
                    (,unit-name
                     (HASH meaning ,meaning-predicates)
                     --
                     (HASH form  ((string ,unit-name ,noun))))
                    (root
                     --
                     (boundaries ((,unit-name ?init ?end)))))
                   :cxn-set lex
                  ; :feature-types ((lex-class set))
                   :cxn-inventory ,cxn-inventory))))

(deftest test-create-noun-cxn ()
  (let* ((init-grammar (make-proclisis-extended-cxns))
         (init-length (length (constructions init-grammar))))
    (multiple-value-bind (new-grammar new-cxn)
        (create-noun-cxn "livros" :number 'plural
                         :sem-class '(physical-object tangible inanimate)
                         :meaning '(book plural) :gender 'masculine
                         :cxn-inventory init-grammar)
      (test-assert (= (length (constructions new-grammar)) (+ init-length 1)))
      (test-assert new-cxn)
      (let ((test-transient-structure<-
             (de-render '("livros") :de-render-with-scope))
            (test-transient-structure->
             (create-initial-structure '((book o-1) (plural o-1)) :one-pole-mode)))
      ;  (activate-monitor trace-fcg-light)
      ;  (fcg-apply (get-processing-cxn new-cxn) test-transient-structure<- '<-)
      ;  (fcg-apply (get-processing-cxn new-cxn) test-transient-structure-> '->)
        (test-assert (fcg-apply (get-processing-cxn new-cxn) 
                                test-transient-structure<- '<-))
        (test-assert (fcg-apply (get-processing-cxn new-cxn) 
                                test-transient-structure-> '->))))))
(test-create-noun-cxn)

;;####################
;; PRONOUNS
;;####################
;;eu, me (dir, indir)
(defun create-pronoun-lex-cxn (lemma &key meaning number person gender (reflexive '-)
                                                (relative '-) (demonstrative '-) (cxn-set 'morph)
                                                (cxn-inventory *propor-grammar*))
  (let* ((cxn-name (make-symbol (mkstr (upcase lemma) '-cxn)))
         (unit-name (make-var (mkstr (upcase lemma) '-unit)))
         (referent-var (make-var "REF"))
         (meaning-predicates (loop for pred in meaning
                                   collect (list pred referent-var))))
    (eval `(def-fcg-cxn ,cxn-name
                    ((,unit-name
                      (referent ,referent-var)
                      (sem-cat (sem-class (physical-object animate))
                               (sem-function referring-expression))
                      (syn-cat (syn-function nominal)
                               (lex-class (pronoun))
                            ;   (clitic +) ;;needed for nos
                               (relative ,relative)
                               (demonstrative ,demonstrative)
                               (reflexive ,reflexive)
                               (agreement ,@(when person `((person ,person)))
                                          ,@(when number `((number ,number)))
                                          ,@(when gender `((gender ,gender))))
                               (case ?case-undefined))
                      (boundary (?init ?end)))
                     <-
                     (,unit-name
                      (HASH meaning ,meaning-predicates)
                      --
                      (HASH form ((string ,unit-name ,lemma))))
                     (root
                      --
                      (boundaries ((,unit-name ?init ?end)))))
                    :cxn-set ,cxn-set
                    :cxn-inventory ,cxn-inventory))))


(defun create-personal-pronoun-lemma-cxn (lemma &key meaning number person gender
                                                (reflexive '-)
                                                (relative '-) (demonstrative '-)
                                                (cxn-inventory *propor-grammar*))
 ; (assert (or (variable-p number) (member number '(singular plural))))
 ; (assert (or (variable-p person) (member person '(1 2 3))))
  (let* ((cxn-name (make-symbol (mkstr (upcase lemma) '-cxn)))
         (unit-name (make-var (mkstr (upcase lemma) '-unit)))
         (referent-var (make-var "REF"))
         (meaning-predicates (loop for pred in meaning
                                   collect (list pred referent-var))))
    (eval `(def-fcg-cxn ,cxn-name
                    ((,unit-name
                      (footprints ( lex))
                      (referent ,referent-var)
                      (sem-cat (sem-class (physical-object animate))
                               (sem-function referring-expression))
                      (syn-cat (lemma ,lemma)
                               (syn-function nominal)
                               (case ?case-undefined))
                      (boundary (?init ?end)))
                     <-
                     (,unit-name
                      ;(footprints (not lex))
                      (HASH meaning ,meaning-predicates)
                      --
                      (footprints (not lex))
                      (syn-cat (lex-class (pronoun))
                               (relative ,relative)
                               (reflexive ,reflexive)
                               (demonstrative ,demonstrative)
                               (agreement ,@(when person `((person ,person)))
                                          ,@(when number `((number ,number)))
                                          ,@(when gender `((gender ,gender))))))
                     (root
                      --
                      (boundaries ((,unit-name ?init ?end)))))
                    :cxn-set lex
                    :disable-automatic-footprints t
                    :cxn-inventory ,cxn-inventory))))

(deftest test-create-personal-pronoun-lemma-cxn ()
  (let* ((init-grammar (make-proclisis-extended-cxns))
         (init-length (length (constructions init-grammar))))
    
    (multiple-value-bind (new-grammar new-cxn)
        (create-personal-pronoun-lemma-cxn "eu" :number 'singular :person 1
                                           :meaning '(me person) :gender (make-var 'gender)
                                           :cxn-inventory init-grammar)
      (test-assert (= (length (constructions new-grammar)) (+ init-length 1)))
      (test-assert new-cxn)
      (let ((test-transient-structure->
             (create-initial-structure '((person  p-1) (me p-1)) :one-pole-mode)))
        
        (test-assert (fcg-apply (get-processing-cxn new-cxn) 
                                test-transient-structure-> '->))))

    (multiple-value-bind (new-grammar new-cxn)
        (create-personal-pronoun-lemma-cxn "quem" :meaning '(undefined-referent) :number 'singular
                                  :relative '+ :cxn-inventory init-grammar)
      (test-assert (= (length (constructions new-grammar)) (+ init-length 2)))
      (test-assert new-cxn)
      (let ((test-transient-structure->
             (create-initial-structure '((undefined-referent o-1)) :one-pole-mode)))
        
        (test-assert (fcg-apply (get-processing-cxn new-cxn) 
                                test-transient-structure-> '->))))))

(test-create-personal-pronoun-lemma-cxn)

(defun create-prepositional-object-morph-cxn (lemma morph &key number person  lex-class
                                                (gender '?gender) (relative '-) (cxn-set 'morph)
                                                (reflexive '-) (demonstrative '-)
                                                (cxn-inventory *propor-grammar*))
  (assert (or (variable-p number) (member number '(singular plural))))
  (assert (or (variable-p person) (member person '(1 2 3))))
  (let* ((cxn-name (make-symbol (mkstr (upcase lemma) '- (upcase morph) '-prep-object-cxn)))
         (unit-name (make-var (mkstr (upcase morph) '-unit)))
         (clitic? (when (string= lex-class 'clitic) t)))
    ;(assert (null (equalp case-matrix '(?nom ?acc ?dat))))
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (syn-cat (marked +)
                                   (syn-function nominal)
                                    ,@(when clitic? '((clitic +))))) 
                         <-
                         (,unit-name
                          (syn-cat (lemma ,lemma)
                                   (lex-class (pronoun))
                                   (not (marked +))
                                   (case (prepositional-object))
                                   (relative ,relative)
                                   (reflexive ,reflexive)
                                  
                                   (demonstrative ,demonstrative)
                                   (agreement ,@(when person `((person ,person)))
                                              ,@(when number `((number ,number)))
                                              ,@(when gender `((gender ,gender)))))
                          --
                          (HASH form ((string ,unit-name ,morph)))))
                        :cxn-set ,cxn-set
                        :cxn-inventory ,cxn-inventory))))


(defun create-personal-pronoun-morph-cxn (lemma morph &key number person case lex-class
                                                (gender '?gender) (relative '-) (cxn-set 'morph)
                                                (reflexive '-) (demonstrative '-)
                                                (cxn-inventory *propor-grammar*))
  (assert (or (variable-p number) (member number '(singular plural))))
  (assert (or (variable-p person) (member person '(1 2 3))))
  (let* ((cxn-name (make-symbol (mkstr (upcase lemma) '- (upcase morph) '-pronoun-cxn)))
         (unit-name (make-var (mkstr (upcase morph) '-unit)))
         (clitic? (when (string= lex-class 'clitic) t))
         (negative-matrix '(- - -))
         (case-matrix
          (if (> (length case) 1)
            (loop with result = (copy-list negative-matrix)
                  for c in case
                  do 
                  (cond ((string= c 'nominative)
                         (setf (nth 0 result) '?nom))
                        ((string= c 'accusative)
                         (setf (nth 1 result) '?acc))
                        ((string= c 'dative)
                         (setf (nth 2 result) '?dat)))
                  finally (return result))
            (let ((result (copy-list negative-matrix)))
              (cond ((string= (first case) 'nominative)
                     (setf (nth 0 result) '+))
                    ((string= (first case) 'accusative)
                     (setf (nth 1 result) '+))
                    ((string= (first case) 'dative)
                     (setf (nth 2 result) '+)))
              result))))
    (assert (null (equalp case-matrix '(+ + +))))
    ;(assert (null (equalp case-matrix '(?nom ?acc ?dat))))
    (eval `(def-fcg-cxn ,cxn-name
                        ((,unit-name
                          (syn-cat (marked +)
                                   (syn-function nominal)
                                    ,@(when clitic? '((clitic +))))) 
                         <-
                         (,unit-name
                          (syn-cat (lemma ,lemma)
                                   (lex-class (pronoun))
                                   (not (marked +))
                                   (case ,case-matrix)
                                   (relative ,relative)
                                   (reflexive ,reflexive)
                                  
                                   (demonstrative ,demonstrative)
                                   (agreement ,@(when person `((person ,person)))
                                              ,@(when number `((number ,number)))
                                              ,@(when gender `((gender ,gender)))))
                          --
                          (HASH form ((string ,unit-name ,morph)))))
                        :cxn-set ,cxn-set
                        :cxn-inventory ,cxn-inventory))))

(deftest test-create-personal-pronoun-morph-cxn ()
  (let* ((init-grammar (make-proclisis-extended-cxns))
        (init-length (length (constructions init-grammar))))
    (multiple-value-bind (new-grammar new-cxn)
        (create-personal-pronoun-morph-cxn "eu" "eu" :person 1 :number 'singular :case '(nominative)
                                           :cxn-inventory init-grammar)
      (test-assert (= (length (constructions new-grammar)) (+ init-length 1)))
      (test-assert new-cxn)
      (let ((test-transient-structure<-
             (de-render (make-instance 'utterance
                                       :utterance '("eu")) :de-render-with-stress-pattern)))
        
        (test-assert (fcg-apply (get-processing-cxn new-cxn) 
                                test-transient-structure<- '<-))))
    (multiple-value-bind (new-grammar new-cxn)
        (create-personal-pronoun-morph-cxn "eu" "me" :person 1 :number 'singular :case '(accusative dative)
                                           :cxn-inventory init-grammar)
      (test-assert (= (length (constructions new-grammar)) (+ init-length 2)))
      (test-assert new-cxn)
      (let ((test-transient-structure<-
             (de-render (make-instance 'utterance
                                       :utterance '("me")) :de-render-with-stress-pattern)))
        
        (test-assert (fcg-apply (get-processing-cxn new-cxn) 
                                test-transient-structure<- '<-))))

    (multiple-value-bind (new-grammar new-cxn)
        (create-personal-pronoun-morph-cxn "tu" "te" :person 2 :number 'singular :case '(accusative dative)
                                           :cxn-inventory init-grammar)
      (test-assert (= (length (constructions new-grammar)) (+ init-length 3)))
      (test-assert new-cxn)
      (let ((test-transient-structure<-
             (de-render (make-instance 'utterance
                                       :utterance '("te")) :de-render-with-stress-pattern)))
        
        (test-assert (fcg-apply (get-processing-cxn new-cxn) 
                                test-transient-structure<- '<-))))))

(test-create-personal-pronoun-morph-cxn)


(deftest test-pronoun-cxn-interaction ()
  (let ((init-grammar (make-proclisis-extended-cxns)))

    (create-personal-pronoun-lemma-cxn "eu" :number 'singular :person 1 :meaning '(person me)
                                           :cxn-inventory init-grammar)
    (create-personal-pronoun-morph-cxn "eu" "eu" :person 1 :number 'singular :case '(nominative)
                                       :cxn-inventory init-grammar)
    (create-personal-pronoun-morph-cxn "eu" "me" :person 1 :number 'singular :case '(accusative dative)
                                       :cxn-inventory init-grammar)
    (let ((test-transient-structure-eu<-
           (de-render '("eu") :de-render-with-scope))
          (test-transient-structure-me<-
             (de-render '("me") :de-render-with-scope))
          (test-transient-structure-me->
           (create-initial-structure '((person  p-1) (me p-1)) :one-pole-mode)))
      
      (test-assert (fcg-apply (processing-cxn-inventory init-grammar)
                              test-transient-structure-eu<- '<-))
      (test-assert (fcg-apply (processing-cxn-inventory init-grammar)
                              test-transient-structure-me<- '<-))
      (test-assert (fcg-apply (processing-cxn-inventory init-grammar)
                              test-transient-structure-me-> '->)))
    ))

(test-pronoun-cxn-interaction)


;;####################
;; CONJUNCTIONS
;;####################

(defun create-conjunction-cxn (form &key sem-class meaning syn-function
                                    (cxn-inventory *propor-grammar*)) ;;(book single)
  (let* ((cxn-name (make-symbol (mkstr (upcase form) '-cxn)))
         (unit-name (make-var (mkstr (upcase form) '-unit)))
         (referent-var (make-var "REF"))
         (meaning-predicates (loop for pred in meaning
                                   collect (list pred referent-var))))
    (eval 
     `(def-fcg-cxn ,cxn-name
                   ((,unit-name
                     (referent ,referent-var)
                     (sem-cat (sem-class (,@sem-class)))
                     (syn-cat (lex-class (conjunction))
                              (syn-function ,syn-function)))
                    <-
                    (,unit-name
                     (HASH meaning ,meaning-predicates)
                     --
                     (HASH form ((string ,unit-name ,form)))))
                   :cxn-set lex :cxn-inventory ,cxn-inventory))))



;;####################
;; ADJECTIVES
;;####################

(defun create-adjective-cxn (form &key sem-class meaning gender number
                                    (cxn-inventory *propor-grammar*)) ;;(book single)
  (let* ((cxn-name (make-symbol (mkstr (upcase form) '-cxn)))
         (unit-name (make-var (mkstr (upcase form) '-unit)))
         (referent-var (make-var "REF"))
         (meaning-predicates (loop for pred in meaning
                                   collect (list pred referent-var))))
    (eval 
     `(def-fcg-cxn ,cxn-name
                   ((,unit-name
                     (referent ,referent-var)
                     (sem-cat (sem-class (,@sem-class))
                              (sem-function modifier))
                     (syn-cat (lex-class (adjective))
                              (agreement (gender ,gender)
                                         (number ,number)))
                     (boundary (?init ?end)))
                    <-
                    (,unit-name
                     (HASH meaning ,meaning-predicates)
                     --
                     (HASH form ((string ,unit-name ,form))))
                    (root
                     --
                     (boundaries ((,unit-name ?init ?end)))))
                   :cxn-set lex :cxn-inventory ,cxn-inventory))))

