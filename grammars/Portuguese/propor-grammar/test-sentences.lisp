;(asdf:operate 'asdf:load-op 'propor-grammar)

(in-package :fcg)

(deftest test-fronted-focus ()
  (deactivate-all-monitors)
  (let ((test-grammar *propor-grammar*)
        (default-focus '("eu" "dei" "te" "este" "livro")) ;;default enclisis
        (fronted-focus '("este" "livro" "te" "dei" "eu")) ;;fronted focus
        (sentence-1c-ungram '("este" "livro" "dei" "te" "eu"))) ;;ungrammatical
    
    (test-assert (equalp (comprehend-and-formulate default-focus :cxn-inventory test-grammar)
                         default-focus )) 
    
    (test-assert (equalp (comprehend-and-formulate fronted-focus :cxn-inventory test-grammar)
                         fronted-focus ))
    (test-assert (null
                  (equalp (comprehend-and-formulate sentence-1c-ungram :cxn-inventory test-grammar)
                          sentence-1c-ungram)))))

;(test-fronted-focus)

(deftest test-wh-questions ()
  (deactivate-all-monitors)
  (let ((test-grammar *propor-grammar*)
        (wh-fronted '("a_quem" "o" "deste" "tu" )) ;;proclisis
        (wh-in-situ '("tu" "deste" "o" "a_quem")) ;;enclisis
        (wh-fronted-subject '("quem" "te" "deu" "este" "livro" ))
        (sentence-1c-ungram '("a_quem"  "deste" "o" "tu" )) ;;ungrammatical
        (sentence-1d-ungram '("tu" "o"  "deste" "a_quem" )) ;;ungrammatical
        )
    
    
    (test-assert (equalp (comprehend-and-formulate wh-fronted :cxn-inventory test-grammar)
                         wh-fronted ))
    (test-assert (equalp (comprehend-and-formulate wh-fronted-subject :cxn-inventory test-grammar)
                         wh-fronted-subject ))
    
    (test-assert (member (comprehend-and-formulate wh-in-situ :cxn-inventory test-grammar)
                         (list wh-fronted wh-in-situ) :test #'equalp ))

    
    (test-assert (null (equalp
                        (comprehend-and-formulate sentence-1c-ungram :cxn-inventory test-grammar)
                        sentence-1c-ungram)))
    (test-assert (null (equalp
                        (comprehend-and-formulate sentence-1d-ungram :cxn-inventory test-grammar)
                        sentence-1d-ungram)))))

;(test-wh-questions)

(deftest test-adverb-position ()
  (deactivate-all-monitors)
  (let ((test-grammar *propor-grammar*)
        (raramente-enclisis '("eu" "leio" "o"  "raramente")) ;;default enclisis with operator-like modifier
        (raramente-proclisis '("eu" "raramente" "o" "leio" )) ;;default proclisis with operator-like modifier
;	(raramente-ungram-1 '("eu" "raramente" "leio" "o")) ;;ungrammatical
	(raramente-ungram-2 '("eu" "o" "leio" "raramente")) ;;ungrammatical
        (nao-proclisis '("ele" "nao" "te" "deu" "este" "livro")) ;;negation marker of proclisis
	(nao-ungram-1 '("ele" "te" "deu" "nao" "este" "livro")) ;;ungrammatical
;	(nao-ungram-2 '("ele" "deu" "te" "nao" "este" "livro")) ;;ungrammatical
        (nunca-proclisis '("ele" "nunca" "te" "deu" "este" "livro")) ;;negation with never triggers proclisis
	(nunca-ungram-1 '("ele" "nunca" "deu" "te" "este" "livro")) ;;ungrammatical
;	(nunca-ungram-2 '("ele" "deu" "te" "nunca"  "este" "livro")) ;;ungrammatical
        (ontem-enclisis '("eu" "ontem" "vi" "te")) ;; enclisis with non-operator like modifier
	(ontem-ungram-1 '("eu" "ontem" "te" "vi")) ;;ungrammatical
	)
    (test-assert (= (length (remove-duplicates (comprehend-and-formulate-all
                              raramente-enclisis :cxn-inventory test-grammar):test #'equalp))
                    2))
    (test-assert (member (comprehend-and-formulate
                          raramente-enclisis :cxn-inventory test-grammar)
                         (list raramente-enclisis raramente-proclisis) :test #'equalp ))
    
    (test-assert (member (comprehend-and-formulate raramente-proclisis :cxn-inventory test-grammar)
                         (list raramente-enclisis raramente-proclisis) :test #'equalp ))
    
    (test-assert (equalp (comprehend-and-formulate nao-proclisis :cxn-inventory test-grammar)
                          nao-proclisis ))
    
    (test-assert (equalp (comprehend-and-formulate nunca-proclisis :cxn-inventory test-grammar)
                         nunca-proclisis ))
    
    

;;;     (test-assert (null (comprehend-and-formulate
;;;                         (make-instance 'utterance
;;;                                        :utterance raramente-ungram-1) :cxn-inventory test-grammar)))
    (test-assert (null (equalp
                        (comprehend-and-formulate raramente-ungram-2 :cxn-inventory test-grammar)
                        raramente-ungram-2)))
    (test-assert (null (equalp
                        (comprehend-and-formulate nao-ungram-1 :cxn-inventory test-grammar)
                       nao-ungram-1)))
;;;     (test-assert (null (comprehend-and-formulate
;;;                         (make-instance 'utterance
;;;                                        :utterance nao-ungram-2) :cxn-inventory test-grammar)))
    (test-assert (null (equalp
                        (comprehend-and-formulate nunca-ungram-1 :cxn-inventory test-grammar)
                        nunca-ungram-1)))
;;;     (test-assert (null (comprehend-and-formulate
;;;                         (make-instance 'utterance
;;;                                        :utterance nunca-ungram-2) :cxn-inventory test-grammar)))
    (test-assert (null (equalp
                        (comprehend-and-formulate ontem-ungram-1 :cxn-inventory test-grammar)
                        ontem-ungram-1)))
    ))

;;(test-adverb-position)

(deftest test-quantified-subjects ()
  (deactivate-all-monitors)
  (let ((test-grammar *propor-grammar*)
        (algumas-enclisis '("algumas" "pessoas" "leem" "o"));; non-downard entailing quantifier
        (algumas-ungram '("algumas" "pessoas" "o" "leem"));; ungrammatical
	(poucas-proclisis '("poucas" "pessoas" "o" "leem"));; downward entailing quantifier
	(poucas-ungram '("poucas" "pessoas" "leem" "o"));; ungrammatical
	)
    (test-assert (= (length (remove-duplicates
                             (comprehend-and-formulate-all
                              algumas-enclisis :cxn-inventory test-grammar) :test #'equalp))
                    1))
    (test-assert (equalp (comprehend-and-formulate algumas-enclisis :cxn-inventory test-grammar)
                         algumas-enclisis))
    (test-assert (= (length (remove-duplicates
                             (comprehend-and-formulate-all
                              poucas-proclisis :cxn-inventory test-grammar) :test #'equalp))
                    1))
    (test-assert (equalp (comprehend-and-formulate
                          poucas-proclisis :cxn-inventory test-grammar)
                         poucas-proclisis))
    ))

;(test-quantified-subjects)


(deftest test-complementarizers ()
  (deactivate-all-monitors)
  (let ((test-grammar *propor-grammar*)
        (porque-gram '("eu" "dei" "te" "este" "livro" "porque" "tu" "o" "querias"));; complementarize in head of the second sentence triggers proclisis
	(porque-ungram '("eu" "dei" "te" "este" "livro" "porque" "tu" "querias" "o")) ;;ungrammatical
	)
    (test-assert (equalp (comprehend-and-formulate porque-gram :cxn-inventory test-grammar)
                         porque-gram))
    (test-assert (null (equalp (comprehend-and-formulate
                         porque-ungram :cxn-inventory test-grammar)
                         porque-ungram)))
    

    ))

;(test-complementarizers)



(test-fronted-focus)
(test-wh-questions)
(test-adverb-position)
(test-quantified-subjects)
;(test-complementarizers)
