
(in-package :irl)

(defparameter *test-chunks* nil)
(defmacro def-chunk (id chunk)
  `(progn (push (cons ',id ,chunk) *test-chunks*) ,chunk))

(defparameter *test-meanings* nil)
(defmacro def-meaning (id meaning)
  `(progn (push (cons ',id ,meaning) *test-meanings*) ,meaning))

(defmacro match- (chunk-id meaning-id)
  `(match-chunk (assqv ',chunk-id *test-chunks*) 
                 (assqv ',meaning-id *test-meanings*)))

(deftest test-match-chunk ()
;;;;; 1. simple cases
  (def-chunk chunk-1
      (make-instance 
       'chunk 
       :irl-program '((foo ?var-0 ?var-1 ?var-2 ?var-3)
                      (bar ?var-1 ?x)
                      (bar ?var-2 ?y)
                      (baz ?var-3 ?z))
       :target-var '(?target . string)
       :open-vars '((?x . float) (?y . float) (?z . integer))))

  ;; complete meaning -> one result
  (def-meaning meaning-1a '((foo ?a ?b ?c ?d)
                            (bar ?b ?e) (bar ?c ?f) (baz ?d ?g)
                            (bind float ?e v-1) (bind float ?f v-2)
                            (bind integer ?g v-3)))
  (test-assert (= 1 (length (match- chunk-1 meaning-1a))))
  
  ;; only bind statements -> two results
  (def-meaning meaning-1b '((bind float ?e v-1) (bind float ?f v-2)
                            (bind integer ?g v-3)))
  (test-assert (= 2 (length (match- chunk-1 meaning-1b))))

  ;; one float bind statement -> two results
  (def-meaning meaning-1c '((bind float ?e v-1)))
  (test-assert (= 2 (length (match- chunk-1 meaning-1c))))

  ;; wrong bind type -> no results
  (def-meaning meaning-1d '((bind string ?1 value-1) (bind float ?2 value-2)
                            (bind integer ?3 value-3)))
  (test-assert (not (match- chunk-1 meaning-1d)))

  ;; all primitives -> one result
  (def-meaning meaning-1e '((foo ?a ?b ?c ?d)
                            (bar ?b ?e) (bar ?c ?f) (baz ?d ?g)))
  (test-assert (= 1 (length (match- chunk-1 meaning-1e))))

  ;; only bar primitive -> two results
  (def-meaning meaning-1f '((bar ?b ?e)))
  (test-assert (= 2 (length (match- chunk-1 meaning-1f))))

  ;; incompatible program -> no results
  (def-meaning meaning-1g '((foo ?a ?b ?c ?d)
                            (bar ?b ?e) (baz ?c ?f) (baz ?d ?g)))
  (test-assert (not (match- chunk-1 meaning-1g)))


;;;;; 2. additional equalities in meaning
  (def-chunk chunk-2
      (make-instance 'chunk 
                     :irl-program '((foo ?a ?b ?c) (bar ?b) (baz ?c ?b ?d))
                     :target-var '(?a . string)
                     :open-vars '((?d . number))))
  
  ;; only primitives -> one result
  (def-meaning meaning-2a '((foo ?x ?y ?z) (baz ?z ?y2 ?d)))
  (test-assert (= 1 (length (match- chunk-2 meaning-2a))))

  ;; with bind statements -> one result
  (def-meaning meaning-2b '((foo ?x ?y ?z) (baz ?z ?y2 ?d)
                            (bind float ?d str)))
  (test-assert (= 1 (length (match- chunk-2 meaning-2b))))

    
;;;;; 3. additional equalities in chunk
  (def-chunk chunk-3
      (make-instance 'chunk 
                     :irl-program '((foo ?var-0 ?var-1)
                                    (bar ?var-1 ?var-2)
                                    (bar ?var-2 ?var-3))
                     :target-var '(?var-0 . string)
                     :open-vars '((?var-3 . float))))

  ;; additional variable equality in chunk -> no result
  (def-meaning meaning-3a '((foo ?a ?b) (bar ?b ?c) (bar ?c ?b)))
  (test-assert (not (match- chunk-3 meaning-3a)))
  
  ;; fine -> one result
  (def-meaning meaning-3b '((foo ?a ?b) (bar ?b ?c) (bar ?c ?d)))
  (test-assert (= 1 (length (match- chunk-3 meaning-3b))))


;;;;; 4. argument order
  (def-chunk chunk-4 
      (make-instance 'chunk 
                     :irl-program '((foo ?a ?b) (baz ?a ?v1) (baz ?b ?v2))
                     :target-var '(?a . string)
                     :open-vars '((?v1 . float) (?v2 . float))))
  
  ;; two results that are equivalent
  (def-meaning meaning-4a '((baz ?x ?x-1) (baz ?y ?y-1)))
  (test-assert (= 2 (length (match- chunk-4 meaning-4a))))

  ;; six results, of which some are not equivalent
  (def-meaning meaning-4b '((baz ?x ?x-1) (bind float ?x-1 foo)
                            (baz ?y ?y-1) (bind float ?y-1 bar)))
  (test-assert (= 2 (length (match- chunk-4 meaning-4b))))


;;;;; 5. variable-equalities
  (def-chunk chunk-5
      (make-instance 'chunk
                     :irl-program '((foo ?x ?y) (bar ?x))
                     :target-var '(?x . string)
                     :open-vars '((?y . string))))

  ;; This program is already complete
  (def-meaning meaning-5a '((foo ?a ?b) (bar ?a) (bind string ?b i-am-a-hippy)))
  (test-assert (= 1 (length (match- chunk-5 meaning-5a))))

  ;; Here a variable quality between primitives is added
  (def-meaning meaning-5b '((foo ?a ?b) (bar ?c) (bind string ?b i-am-a-hippy)))
  (test-assert (= 1 (length (match- chunk-5 meaning-5b))))
  
  ;; Here a variable quality between a primitive and a bind statement
  ;; is added
  (def-meaning meaning-5c '((foo ?a ?b) (bar ?a) 
                            (bind string ?d i-would-like-to-be-a-hippy)))
  (test-assert (= 1 (length (match- chunk-5 meaning-5c)))))



;; to see the result of matching, activate this monitor
;; (toggle-monitor trace-irl-in-web-browser)

;; (test-match-chunk)

;;(time (dotimes (n 100) (test-match-chunk)))
