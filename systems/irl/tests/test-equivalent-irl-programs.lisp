
(in-package :irl)


(deftest test-equivalent-irl-programs? ()
  (test-assert (equivalent-irl-programs? '((p1 ?a) (p2 ?a ?b) (p3 ?b))
                                         '((p1 ?x) (p2 ?x ?y) (p3 ?y))))

  (test-assert (equivalent-irl-programs? '((p1 ?a) (p2 ?a ?b) (p3 ?b))
                                         '((p1 ?a) (p2 ?a ?b) (p3 ?b))))

  (test-assert (equivalent-irl-programs? '((p1 ?a) (p2 ?a ?b) (p3 ?b))
                                         '((p1 ?b) (p2 ?b ?a) (p3 ?a))))
  
  (test-assert (not (equivalent-irl-programs? '((p1 ?a) (p2 ?a ?b) (p3 ?b))
                                              '((p1 ?a) (p2 ?b ?a) (p3 ?b)))))
  
  (test-assert (equivalent-irl-programs? '((p1 ?a) (p2 ?b))
                                         '((p1 ?x) (p2 ?y))))
  
  (test-assert (not (equivalent-irl-programs? '((p1 ?a) (p2 ?b))
                                              '((p1 ?x) (p2 ?x)))))
  
  (test-assert (not (equivalent-irl-programs?  '((foo ?a ?b) (bar ?a)) 
                                               '((foo ?a ?b) (bar ?b)))))

  (test-assert (not (equivalent-irl-programs?
                     '((prim-1 ?bar ?foo) (prim-1 ?foo ?baz) (prim-2 ?baz))
                     '((prim-1 ?bar ?foo) (prim-1 ?foo ?baz) (prim-2 ?foo)))))
  
  (test-assert (not (equivalent-irl-programs?
                     '((a ?a) (p ?a ?b) (b ?b) (c ?c) (p ?c ?d) (d ?d))
                     '((a ?a) (p ?a ?d) (d ?d) (c ?c) (p ?c ?b) (b ?b))))))


;;(test-equivalent-irl-programs?)