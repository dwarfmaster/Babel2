
(in-package :fcg)

(deftest test-render ()
  (test-assert
   (loop for i from 1 to 10 always 
         (equal (render (make-instance 'coupled-feature-structure
                                       :left-pole '((root
                                                     (form ((meets u1 u2) (meets u2 u3))))
                                                    (u1 (form ((string u1 "s1"))))
                                                    (u2 (form ((string u2 "s2"))))
                                                    (u3 (form ((string u3 "s3"))))))
                        :render-string-meets-precedes)
                '("s1" "s2" "s3"))))
   (test-assert
    (loop for i from 1 to 10 always 
         (equal (render (make-instance 'coupled-feature-structure
                                       :left-pole '((root
                                                     (form ((meets u1 u2) (meets u2 u3))))
                                                    (u1 (form ((string u1 "s1"))))
                                                    (u2 (form ((string u2 "s2"))))
                                                    (u3 (form ((string u3 "s3"))))))
                        :render-string-meets)
                '("s1" "s2" "s3"))))
   (test-assert
    (loop for i from 1 to 10 always 
         (equal (render (make-instance 'coupled-feature-structure
                                       :left-pole '((root
                                                     (form ((meets u1 u2 u4) (meets u2 u3 u4))))
                                                    (u1 (form ((string u1 "s1"))))
                                                    (u2 (form ((string u2 "s2"))))
                                                    (u3 (form ((string u3 "s3"))))))
                        :render-with-scope)
                '("s1" "s2" "s3")))))

;; (test-render)

(deftest test-render-with-scope ()
  ;; Needs to be expanded with additional tests!
  (test-assert (equal '((NP ((det) (adj)))) (order-units-locally '(meets det adj NP) nil 'meets)))
  (test-assert (equal '((NP ((det) (adj) (N)))) (order-units-locally '(meets adj N NP) '((NP ((det) (adj)))) 'meets)))
  (test-assert (equal '((NP ((what) (det) (adj) (N)))) (order-units-locally '(meets what det NP) '((NP ((det) (adj) (N)))) 'meets)))
  (test-assert (equal '((NP (det) (adj))) (order-units-locally '(precedes det adj NP) nil 'precedes)))
  (test-assert (equal '((NP (det) (adj) (n))) (order-units-locally '(precedes det N NP) '((NP (det) (adj))) 'precedes)))
  (test-assert (equal '((NP (det) (adj n))) (order-units-locally '(precedes det n NP) '((NP (adj n))) 'precedes)))
  (test-assert (equal '((np (art))) (order-units-locally '(first art NP) nil 'first)))
  (test-assert (equal '((np (art adj n))) (order-units-locally '(first art NP)  '((NP (adj art N))) 'first)))
  (test-assert (equal '((NP (the) (very) (good) (book))) (order-units-locally '(fields the (very good) book NP) nil 'fields)))
  (test-assert (equal '((NP (the) (very) (good) (book))) (order-units-locally '(fields the (very good) book NP) '((NP (the) (book))) 'fields))))

;; (test-render-with-scope)

(deftest test-de-render-with-scope ()
  (let* ((transient-structure (de-render '("a" "good" "book") :de-render-with-scope :cxn-inventory *fcg-constructions*))
         (boundaries (fcg-get-boundaries transient-structure)))
    (test-assert (equal '(meets precedes fields first) (get-updating-references)))
    (test-assert (unify '((meets ?a ?b ?var1)
                          (meets ?b ?c ?var2))
                        (handle-form-predicate-in-de-render boundaries (first (get-updating-references)))))
    (test-assert (unify '(==p (precedes ?a ?b ?var1)
                              (precedes ?a ?c ?var2)
                              (precedes ?b ?c ?var3))
                        (handle-form-predicate-in-de-render boundaries (second (get-updating-references)))))
    (test-assert (unify '(==p (string ?a "a")
                              (string ?good "good")
                              (string ?book "book"))
                        (fcg-extract-selected-form-constraints transient-structure '(string))))
    (test-assert (unify '(== (meets ?a ?b ?x) (meets ?b ?c ?y)
                             (precedes ?a ?b ?n) (precedes ?a ?c ?m) (precedes ?b ?c ?k))
                        (fcg-extract-selected-form-constraints transient-structure (get-updating-references))))
    (test-assert (unify '(boundaries ((?a 0 1) (?good 1 2) (?book 2 3)))
                        (assoc 'boundaries (unit-body (get-root (fcg-get-transient-unit-structure transient-structure))))))))

;; (test-de-render-with-scope)

;; Tests some functions that are associated with the node-test :update-references, which is used in tandem with
;; the de-render method :de-render-with-scope
(deftest test-update-references ()
  ;(set-hierarchy-feature 'subunits)
  (let* ((boundaries '((a 0 1)
                       (good 1 2)
                       (book 2 3)))
         (new-ts-1 '((root
                      (boundaries ((a 0 1) (good 1 2) (book 2 3))))
                     (good
                      (form ((string good "good"))))
                     (book
                      (form ((string book "book"))))
                     (np
                      (subunits (good book)))))
         (node-1 (make-instance 'cip-node
                               :construction-inventory *fcg-constructions*
                               :statuses (list 'initial)
                               :car (make-cxn-application-result
                                     :resulting-cfs (make-instance 'coupled-feature-structure
                                                                   :left-pole new-ts-1)
                                     :direction '<-)))
          (new-ts-2 '((root
                       (boundaries ((a 0 1) (good 1 2) (book 2 3))))
                      (a
                       (form ((string a "a"))))
                      (good
                       (form ((string good "good"))))
                      (book
                       (form ((string book "book"))))
                      (np
                       (subunits (a good book)))))
          (node-2 (make-instance 'cip-node
                               :construction-inventory *fcg-constructions*
                               :statuses (list 'initial)
                               :car (make-cxn-application-result
                                     :resulting-cfs (make-instance 'coupled-feature-structure
                                                                   :left-pole new-ts-2)
                                     :direction '<-)))
          (new-ts-3 '((root
                       (boundaries ((a 0 1) (good 1 2) (book 2 3))))
                      (a
                       (form ((string a "a"))))
                      (good
                       (form ((string good "good"))))
                      (book
                       (form ((string book "book"))))
                      (adjn
                       (subunits (good book)))
                      (np
                       (subunits (a adjn)))))
          (node-3 (make-instance 'cip-node
                               :construction-inventory *fcg-constructions*
                               :statuses (list 'initial)
                               :car (make-cxn-application-result
                                     :resulting-cfs (make-instance 'coupled-feature-structure
                                                                   :left-pole new-ts-3)
                                     :direction '<-))))
    (test-assert (unify '(==p (a 0 1) (NP 1 3) (good 1 2) (book 2 3)) (update-list-of-boundaries boundaries node-1)))
    (test-assert (unify '(==p (a 0 1) (NP 0 3) (good 1 2) (book 2 3)) (update-list-of-boundaries boundaries node-2)))
    (test-assert (unify '(==p (a 0 1) (good 1 2) (book 2 3) (adjn 1 3) (np 0 3)) (update-list-of-boundaries boundaries node-3)))))

;; (test-update-references)

(deftest test-scope ()
  (test-assert (eql 'NP (get-scope-from-form-constraint '(first art NP))))
  (test-assert (eql 'NP (get-scope-from-form-constraint '(meets art N NP))))
  (test-assert (eql 'NP (get-scope-from-form-constraint '(precedes art N NP)))))

;; (test-scope)
