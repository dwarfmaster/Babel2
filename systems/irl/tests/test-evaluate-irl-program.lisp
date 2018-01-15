
(in-package :irl)

(deftest test-evaluate-bind-statements ()
         (test-assert (evaluate-bind-statements
                       '((bind quantity ?amount ten))
                       *test-ontology*)))
;; (test-evaluate-bind-statements)

(deftest test-evaluate-primitive-in-program ()
         ;; both given consistent
         (test-assert (= 1 (length (evaluate-primitive-in-program
                                    '(pick-apples ?var-1 ?var-2)
                                    (list (make-instance 'binding
                                                         :var '?var-1
                                                         :score 0.5
                                                         :value (make-apples-set 20))
                                          (make-instance
                                           'binding
                                           :var '?var-2
                                           :score 0.5
                                           :value (make-instance 'quantity :n 20)))
                                    *test-ontology*))))

         ;; both given inconsistent
         (test-assert (eq 'inconsistent (evaluate-primitive-in-program
                                    '(pick-apples ?var-1 ?var-2)
                                    (list (make-instance 'binding
                                                         :var '?var-1
                                                         :score 0.5
                                                         :value (make-apples-set 20))
                                          (make-instance
                                           'binding
                                           :var '?var-2
                                           :score 0.5
                                           :value (make-instance 'quantity :n 19)))
                                    *test-ontology*))))
;; (test-evaluate-primitive-in-program)


;; #############################################
;; check program 
;; #############################################
(deftest test-check-irl-program ()
         (test-assert (check-irl-program '()))
         (test-error (check-irl-program '(())))
         (test-assert (check-irl-program
                  `((bind entity ?var ,(make-instance 'entity :id 'entity)))))
         
         (test-error
          (check-irl-program `((bind apples-set ?var ,(make-instance 'entity :id 'entity)))))
         (test-error
          (check-irl-program `((bind apples-set ?var test))))
         (test-error
          (check-irl-program `((bind apples-set ?var entity-2)) *test-ontology*))
         (test-error
          (check-irl-program `((bind entity ?var- entity-1)
                               (bind entity ?var entity-2))
                             *test-ontology*))
         (test-assert (check-irl-program '((pick-apples ?var-1 ?var-2))))
         (test-error (check-irl-program `((test))))
         (test-error (check-irl-program `((pick-apples))))
         (test-error (check-irl-program `((pick-apples ?dfdf addas))))
         (test-error (check-irl-program `((pick-apples ?dfdf ?dfdf))))
         
         (test-assert (check-irl-program `((pick-apples ?var-1 ?var-2)
                                      (bind apples-set ?var-1 ,(make-apples-set 12)))))
         (test-error
          (check-irl-program `((pick-apples ?var-2 ?var-1)
                               (bind apples-set ?var-1 ,(make-apples-set 12)))))
         )

;; (test-check-irl-program)

(defun evaluate-irl-program-test (expected-number-of-solutions program)
  (let ((solutions (evaluate-irl-program program *test-ontology*)))
    (test-assert 
     (= expected-number-of-solutions (length solutions))
     "~%    program: ~:w~%    returned ~a solutions, expected ~a"
     program (length solutions) expected-number-of-solutions)))

;; test evaluate irl program
(deftest test-evaluate-irl-program ()
         ;; pick-apples: all bound
         (evaluate-irl-program-test 1 `((bind apples-set ?apples ,(make-apples-set 7))
                                        (pick-apples ?apples ?amount)
                                        (bind quantity ?amount seven)))
                  
         ;; pick-apples: set -> amount
         (evaluate-irl-program-test 1 `((bind apples-set ?apples ,(make-apples-set 7))
                                        (pick-apples ?apples ?amount)))
  
         ;; pick-apples: amount -> set
         (evaluate-irl-program-test 1 `((pick-apples ?apples ?amount)
                                        (bind quantity ?amount seven)))



         ;; add-apples: all-bound
         (evaluate-irl-program-test 1 `((bind apples-set ?apples ,(make-apples-set 17))
                                        (add-apples ?apples ?amount ?apples-2)
                                        (bind apples-set ?apples-2 ,(make-apples-set 7))
                                        (bind quantity ?amount ten)))

         ;; add-apples: set, amount -> sum
         (evaluate-irl-program-test 1 `((add-apples ?apples ?amount ?apples-2)
                                        (bind apples-set ?apples-2 ,(make-apples-set 7))
                                        (bind quantity ?amount ten)))

         ;; add-apples: sum -> amount, set
         (evaluate-irl-program-test 1 `((bind apples-set ?apples ,(make-apples-set 17))
                                        (add-apples ?apples ?amount ?apples-2)))



         ;; multiply-apples: all bound
         (evaluate-irl-program-test 1 `((bind apples-set ?apples ,(make-apples-set 32))
                                        (multiply-apples ?apples ?factor ?amount ?apples-2)
                                        (bind apples-set ?apples-2 ,(make-apples-set 2))
                                        (bind quantity ?factor three)
                                        (bind quantity ?amount ten)))


         ;; multiply-apples: factor, amount, set -> sum
         (evaluate-irl-program-test 1 `((multiply-apples ?apples ?factor ?amount ?apples-2)
                                        (bind apples-set ?apples-2 ,(make-apples-set 2))
                                        (bind quantity ?factor three)
                                        (bind quantity ?amount ten)))


         ;; sum -> factor, amount-1, amount-2
         (evaluate-irl-program-test 1 `((bind apples-set ?apples ,(make-apples-set 32))
                                        (multiply-apples ?apples ?factor ?amount ?apples-2)))
  
  
         ;; interpret "97 apples"
         (evaluate-irl-program-test 1 `((multiply-apples ?apples ?factor ?amount-1 ?apples-2)
                                        (bind quantity ?factor four)
                                        (bind quantity ?amount-1 twenty)
                                        (add-apples ?apples-2 ?amount-2 ?apples-3)
                                        (bind quantity ?amount-2 ten)
                                        (pick-apples ?apples-3 ?amount-3)
                                        (bind quantity ?amount-3 seven)))

         ;; produce "97 apples"
         (evaluate-irl-program-test 1 `((bind apples-set ?apples ,(make-apples-set 97))
                                        (multiply-apples ?apples ?factor ?amount-1 ?apples-2)
                                        (add-apples ?apples-2 ?amount-2 ?apples-3)
                                        (pick-apples ?apples-3 ?amount-3))))


;;(test-evaluate-irl-program)

