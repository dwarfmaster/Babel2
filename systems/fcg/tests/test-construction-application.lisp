(in-package :fcg)

;; ############################################################################
;; I. Checking whether bindings during second merge phase are also
;;    substituted in first merge

(deftest test-bindings-introduced-in-second-merge ()
  (let* ((cxn
          (make-instance 'construction
                         :name 'bindings-in-merges
                         :left-pole '((root
                                       (sem-subunits (?sub-unit)))
                                      ((J ?sub-unit)
                                       (feat-2 (== (val ?x)))))
                         :right-pole '((root
                                        (feat-1 (== (val ?x)))))))
         (struct
          (make-instance 'coupled-feature-structure
                         :left-pole '((root
                                       (sem-subunits (sub-unit))))
                         :right-pole '((root (feat-1 ((val a)))))))
         (cars (fcg-apply cxn struct '->)))
    (test-assert (= (length cars) 1))
    (let ((feat-2-value (cadar (unit-feature-value
                                (structure-unit
                                 (left-pole-structure (first cars))
                                 'sub-unit)
                                'feat-2))))
      (test-assert (eq feat-2-value 'a)))))

;;(test-bindings-introduced-in-second-merge)

;; ############################################################################
;; II. When ?new-unit-name and ?name-unit-name are equal, the application
;;     of the J-operator is illegal.

;; This typically occurs when applying a default lex-stem construction
;; twice if you are not using a hat. After the first application the
;; same construction matches with root bound to the unit created in this first
;; application.  Because of the ?new variable in the string, ?new and
;; root will be identical.

(deftest test-j-operator-when-new-and-parent-are-equal ()
  (let* ((test-cxn
          (make-instance 
	   'construction
	   :name 'default-lex-stem
	   :left-pole '((root
			 (TAG ?meaning (meaning (== (ball ?x)))))
			((J ?new)
			 ?meaning))
	   :right-pole '((root
			  (TAG ?form (form (== (string ?new "ball")))))
			 ((J ?new)
			  ?form))))
	 (intermediate-cars (fcg-apply test-cxn
                                       (handler-bind ((warning #'(lambda (c)
                                                                   (declare (ignore c))
                                                                   (muffle-warning))))
                                         (de-render '("ball") :de-render-in-root-mode))
                                       '<-)))
    (test-assert (= (length intermediate-cars) 1))
    (let ((intermediate-cfs (car-resulting-cfs (first intermediate-cars))))
      (test-assert (null (fcg-apply test-cxn intermediate-cfs '<-))))))

;; (test-j-operator-when-new-and-parent-are-equal)

;; ############################################################################
;; III. Checking whether a new variable is introduced in both poles by
;; a construction, this variable is equal (something that wasn't the
;; case in the fcg-core).  In this example this would be the variable
;; ?cat.

(deftest fixed-bug-in-introduction-of-variable-in-both-poles ()
  (let* ((test-cxn
          (make-instance
           'construction
           :name 'variable-introduction
           :left-pole '((root
                         (TAG ?meaning 
                              (meaning (== (ball ?x)))))
                        ((J ?new)
                         ?meaning
			 (sem-cat (==1 (sem-cat ?cat)))))
           :right-pole '((root
                          (TAG ?form
                               (form (== (string ?new "ball")))))            
                         ((J ?new)
                          ?form
                          (syn-cat (==1 (lex-cat ?cat)))))))
	 (init-struct-production
          (make-instance 'coupled-feature-structure
			 :left-pole `((root (meaning ((ball x)))))
			 :right-pole `((root))))
	 (production-cars (fcg-apply test-cxn init-struct-production '->))
	 (interpretation-cars (fcg-apply test-cxn
                                         (handler-bind ((warning #'(lambda (c)
                                                                     (declare (ignore c))
                                                                     (muffle-warning))))
                                           (de-render '("ball") :de-render-in-root-mode)) '<-)))
    (test-assert (= (length production-cars) 1))
    (test-assert (= (length interpretation-cars) 1))
    (let* ((sem-cat-production
            (extract-sem-cats 
             (left-pole-structure (first production-cars))))
           (sem-cat-interpretation
            (extract-sem-cats 
             (left-pole-structure (first interpretation-cars))))
           (syn-cat-production
            (extract-syn-cats 
             (right-pole-structure (first production-cars))))
           (syn-cat-interpretation
            (extract-syn-cats 
             (right-pole-structure (first interpretation-cars)))))
      (test-assert (eq (cadar sem-cat-production)
                       (cadar syn-cat-production)))
      (test-assert (eq (cadar sem-cat-interpretation)
                       (cadar syn-cat-interpretation))))))

;; (fixed-bug-in-introduction-of-variable-in-both-poles)

;; ############################################################################
;; IV. Testing whether moving of feature values using tag now works 
;;     for basic lexical constructions in production.

(deftest fixed-bug-in-tag-for-lexical-con-cxn ()
  ;; fixed in commit 4284 by Joachim, adapted to root by Miquel
  (let* ((test-cxn
          (make-instance
           'construction
           :name 'ballon-entry
           :left-pole '((root
                         (TAG ?meaning 
                              (meaning (== (ball ?x)))))
                        ((J ?ball-unit)
                         ?meaning
			 (referent ?x)))
           :right-pole '((root
                          (TAG ?form
                               (form (== (string ?ball-unit "ballon")))))            
                         ((J ?ball-unit)
                          ?form
                          (syn-cat (==1 (pos noun)))))))
	 (cars (fcg-apply test-cxn
                          (make-instance 'coupled-feature-structure
                                         :left-pole `((root (meaning ((ball x)))))
                                         :right-pole `((root)))
			  '->)))
    (test-assert (= (length cars) 1))
    (let* ((resulting-cfs (car-resulting-cfs (first cars)))
	   (form-in-root-unit (unit-feature-value
                               (get-root
                                (right-pole-structure resulting-cfs))
                               'form)))
      (test-assert (null form-in-root-unit)))))

;; (fixed-bug-in-tag-for-lexical-con-cxn)