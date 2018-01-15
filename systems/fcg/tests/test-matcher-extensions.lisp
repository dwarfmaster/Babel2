(in-package :fcg)

#|
(deftest test-or ()
  (test-unify (==1 (OR (person 2nd) (number plural)))
	      ((person ?p) (number ?n))
	      (((?N . PLURAL)) ((?P . 2ND))))
  (test-unify (==1 (OR (a 1) (OR (b 2) (c 3))))
	      ((a ?a) (b ?b) (c ?c))
	      (((?C . 3)) ((?B . 2)) ((?A . 1)))))
|#
;; (test-or)

(deftest test-==p ()
  ;; matching
  (test-assert (unify '(==p a) '(a)))
  (test-assert (unify '(==p a b) '(a b)))
  (test-assert (unify '(==p a b) '(b a)))
  (test-assert (not (unify '(==p a) '(b))))
  (test-assert (not (unify '(==p a b) '(a b c))))
  (test-assert (not (unify '(==p a b) '(a))))
  ;; merging
  (test-assert (equal '(a)
                      (mr-expr (first (fcg-merge '(==p a)
                                                 '(a)
                                                 +no-bindings+)))))
  (test-assert (member (mr-expr (first (fcg-merge '(==p a b)
                                                  '(b a)
                                                  +no-bindings+)))
                       '((a b) (b a))
                       :test #'equal))
  (test-assert (not (fcg-merge '(==p a b) '(a b c) +no-bindings+)))
  ;; construction-application
  (let* ((cfs
          (make-instance 'coupled-feature-structure
                         :left-pole
                         '((np-unit-44
                            (sem-subunits (dog-unit-33 det-unit-163))
                            (referent dog)
                            (footprints (det-noun-cxn))
                            (sem-cat ((type referent-expression)
                                      (sem-role ?sem-role-232))))
                           (fall-unit-13
                            (meaning ((fall-agent ev-1 dog) (fall ev-1)))
                            (footprints (lex-entry-fall))
                            (referent ev-1)
                            (sem-cat ((sem-val ((agent ev-1 dog))))))
                           (dog-unit-33
                            (meaning ((dog dog)))
                            (footprints (lex-entry-dog))
                            (referent dog)
                            (sem-cat ((sem-role ?sem-role-232))))
                           (det-unit-163
                            (meaning ((determine dog)))
                            (referent dog)
                            (footprints (lex-entry-determiner)))
                           (root
                            (sem-frame nil)
                            (sem-cat nil)
                            (referent)
                            (footprints nil)))
                         :right-pole
                         '((np-unit-44
                            (form ((meets det-unit-163 dog-unit-33)))
                            (syn-subunits (dog-unit-33 det-unit-163))
                            (syn-cat
                             ((type nominal)
                              (case ((nom ?n ?n-m - -)
                                     (acc ?a ?a-m - -)
                                     (dat ?d ?d-m - -)))))
                            (footprints (det-noun-cxn)))
                           (fall-unit-13
                            (form ((string fall-unit-13 "fiel")))
                            (footprints (lex-entry-fall))
                            (syn-cat ((pos verb) (type verbal)
                                      (syn-val ((nom ?nom-unit-30))))))
                           (dog-unit-33
                            (footprints (lex-entry-dog))
                            (form ((string dog-unit-33 "hund")))
                            (syn-cat
                             ((type noun)
                              (case ((nom ?n ?n-m - -)
                                     (acc ?a ?a-m - -)
                                     (dat ?d ?d-m - -)))
                              (pos noun))))
                           (det-unit-163
                            (footprints (lex-entry-determiner))
                            (syn-cat
                             ((case ((nom ?n-239 ?n-m-124 - -)
                                     (acc ?a-231 ?a-m-116 - -)
                                     (dat ?d-235 ?d-m-120 - -)))
                              (pos det))))
                           (root
                            (footprints nil)
                            (syn-frame nil)
                            (syn-cat nil)))))
         (cxn-with-==p
          (make-instance 'construction
                         :name 'intransitive-cxn-1
                         :score 1.0
                         :left-pole `((root
                                       (footprints (==0 intransitive-cxn)))
                                      (?agent-unit
                                       (referent ?agent)
                                       (sem-cat (==1 (type referent-expression)
                                                     (sem-role agent))))
                                      (?event-unit
                                       (referent ?event)
                                       (sem-cat
                                        (==1 (sem-val 
                                              (==1 (agent ?event ?agent))))))
                                      ((J ?intransitive-cxn-unit)
                                       (subunits (?agent-unit ?event-unit))
                                       (footprints (==1 intransitive-cxn))))
                         :right-pole `((root
                                        (footprints (==0 intransitive-cxn)))
                                       (?agent-unit
                                        (syn-cat
                                         (==1 (type nominal)
                                              (case (==1 (nom + ?n-masc ?n-fem ?n-neut)
                                                         (acc - - - -)
                                                         (dat - - - -))))))
                                       (?event-unit
                                        (syn-cat (==1 (type verbal)
                                                      (syn-val
                                                       (==1 (nom ?agent-unit))))))
                                       ((J ?intransitive-cxn-unit)
                                        (subunits (?agent-unit ?event-unit))
                                        (footprints (==1 intransitive-cxn)))))) 
         (cxn-without-==p
          (make-instance 'construction
                         :name 'intransitive-cxn-2
                         :score 1.0
                         :left-pole `((root
                                       (footprints (==0 intransitive-cxn)))
                                      (?agent-unit
                                       (referent ?agent)
                                       (sem-cat (==1 (type referent-expression)
                                                     (sem-role agent))))
                                      (?event-unit
                                       (referent ?event)
                                       (sem-cat
                                        (==1 (sem-val 
                                              (==1 (agent ?event ?agent))))))
                                      ((J ?intransitive-cxn-unit)
                                       (subunits (?agent-unit ?event-unit))
                                       (footprints (==1 intransitive-cxn))))
                         :right-pole `((root
                                        (footprints (==0 intransitive-cxn)))
                                       (?agent-unit
                                        (syn-cat
                                         (==1 (type nominal)
                                              (case (==1 (nom + ?n-masc ?n-fem ?n-neut)
                                                         (acc - - - -)
                                                         (dat - - - -))))))
                                       (?event-unit
                                        (syn-cat (==1 (type verbal)
                                                      (syn-val
                                                       (==1 (nom ?agent-unit))))))
                                       ((J ?intransitive-cxn-unit)
                                        (subunits (?agent-unit ?event-unit))
                                        (footprints (==1 intransitive-cxn))))))
         (cars-with-==p (fcg-apply cxn-with-==p cfs '->))
         (cars-without-==p (fcg-apply cxn-without-==p cfs '->))
         (car-with-==p (and (listp cars-with-==p) (first cars-with-==p)))
         (car-without-==p (and (listp cars-without-==p) (first cars-without-==p))))
    (test-assert (= (length cars-with-==p) 1))
    (test-assert (= (length cars-without-==p) 1))
    (test-assert (eq (car-status car-with-==p) 'cxn-applied))
    (test-assert (eq (car-status car-without-==p) 'cxn-applied))
    (test-assert (equivalent-coupled-feature-structures (car-resulting-cfs car-with-==p)
                                                        (car-resulting-cfs car-without-==p)))))

;; (test-==p)