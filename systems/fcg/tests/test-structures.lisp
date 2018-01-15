(in-package :fcg)

(deftest test-features ()
  (let* ((feature (make-feature 'feature 'value))
         (tag-feature (list 'tag (make-var "tag") feature))
         (unit (make-unit :name 'unit-name
                          :features (list feature))))
    (test-assert (equal '(feature value) (make-feature 'feature 'value)))
    (test-assert (eql (feature-name feature) 'feature))
    (test-assert (eql (tag-feature-name tag-feature) 'feature))
    (test-assert (eql (feature-value feature) 'value))

    (setf (feature-value feature) 'value2)
    (test-assert (eql (feature-value feature) 'value2))
    (test-assert (subunits-feature? '(subunits (a b c))))
    (test-assert (subunits-feature? '(sem-subunits (a b c))))
    (test-assert (subunits-feature? '(syn-subunits (a b c))))

    (test-assert (eql 'unit-name (unit-name unit)))
    (test-assert (equal (unit-features unit) (unit-body unit)))
    (test-assert (equal (unit-features unit) '((feature value2))))
    (test-assert (unit-feature unit 'feature))

    (setf unit (make-unit :name 'unit
                          :features (list tag-feature)))
    (test-assert (eql 'feature (feature-name (unit-feature unit 'feature))))))
  
;; (test-features)

(deftest test-transient-structure-helper-functions ()
  ;; Tests some helper functions concerning the transient structure.
  (let* ((transient-structure (make-instance 'coupled-feature-structure
                                            :left-pole '((root
                                                          (form ((string the-1 "the")
                                                                 (string word-1 "word")
                                                                 (meets the-1 word-1)
                                                                 (precedes the-1 word-1)
                                                                 (meets word-1 rhymes-1)
                                                                 (precedes word-1 rhymes-1)
                                                                 (precedes the-1 rhymes-1))))
                                                         (rhymes-1
                                                          (form ((string rhymes-1 "rhymes")))))
                                            :right-pole '((root))))
         (cxn-application-result (make-cxn-application-result :resulting-cfs transient-structure))
         (cipn-node (make-instance 'cip-node
                                   :car cxn-application-result
                                   :construction-inventory (make-instance 'construction-set))))

    ;; fcg-get-transient-structure
    (test-assert (fcg-get-transient-structure cipn-node))
    (test-assert (fcg-get-transient-structure cxn-application-result))
    (test-assert (fcg-get-transient-structure transient-structure))

    ;; fcg-get-transient-unit-structure
    (test-assert (fcg-get-transient-unit-structure cipn-node))
    (test-assert (fcg-get-transient-unit-structure cxn-application-result))
    (test-assert (fcg-get-transient-unit-structure transient-structure))
    (test-assert (fcg-get-transient-unit-structure (left-pole-structure transient-structure)))

    ;; fcg-extract-selected-form-constraints
    (test-assert (= 5 (length (fcg-extract-selected-form-constraints transient-structure))))
    (test-assert (= 3 (length (fcg-extract-selected-form-constraints transient-structure '(string)))))))

;; (test-transient-structure-helper-functions)
