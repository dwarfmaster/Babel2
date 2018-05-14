;;;;
;;;; File: test-utils.lisp
;;;; 
;;;; In this file I will try to test the most crucial parts in the utils package.

(in-package :utils)

;; ############################################################################
;; testing of lists-and-sets.lisp
;; ----------------------------------------------------------------------------

(deftest test-lists-and-sets ()
  ;; This test is all but complete (I tried to test only those that
  ;; are non-trivial)
  (let ((test-list (list 1 2 3 4 5)))
    (pushnew 2 test-list)
    (pushnew 0 test-list)
    (test-assert (equal '(0 1 2 3 4 5) test-list))
    (pushend 6 test-list)
    (test-assert (equal '(0 1 2 3 4 5 6) test-list))
    (setf test-list (butlast test-list))
    (test-assert (equal '(0 1 2 3 4 5) test-list))
    (test-assert (equal (remove-if-not #'(lambda (item) (< item 3)) test-list)
			'(0 1 2)))
    ;; not tested: toggle, mappend, mapunion, mapcar! remove-1
    (test-assert (equal (flatten '((1 2 3) (4) 5 ((((6))))))
			'(1 2 3 4 5 6)))
    (test-assert (equal (utils::listXlist '(1 2) '(3 4))
			'((1 3) (1 4) (2 3) (2 4))))
    ;; not tested: combinations, combinations-of-length, shuffle, permutate-list
    (test-assert (and (permutation-of? '(1 2 3) '(3 1 2))
		      (not (permutation-of? '(1 2 3) '(1 2 3 4)))))
    (test-assert (and (not (duplicates? '(1 2 3 4)))
		      (duplicates? '(1 2 3 1 4))))
    (test-assert (find (random-elt test-list) test-list))
    ;; not tested: random-elts, random-elt-if, permutations-of-length
    ;; not tested: all extremes and search functions
    
    (test-assert (length= '(a b c) 3))
    (test-assert (length= '() 0))
    (test-assert (not (length= '(a b c) 2)))
    (test-assert (not (length= '(a b c) 5)))
    (test-assert (not (length= '(a b c) -2)))
    (test-assert (not (length= '() 1)))
    (test-assert (not (length= '() 5)))
    (test-assert (not (length= '() -1)))
    (test-assert (not (length= '() -4)))
    (test-assert (length= '(a b c) '(d e f)))
    (test-assert (length= '() '()))
    (test-assert (not (length= '(a b c) '(d e))))
    (test-assert (not (length= '() '(d e))))
    (test-assert (not (length= '(a b c) '())))
    
    (test-assert (length= 3 '(a b c)))
    (test-assert (length= 0 '()))
    (test-assert (not (length= 2 '(a b c))))
    (test-assert (not (length= 5 '(a b c))))
    (test-assert (not (length= -2 '(a b c))))
    (test-assert (not (length= 1 '())))
    (test-assert (not (length= 5 '())))
    (test-assert (not (length= -1 '())))
    (test-assert (not (length= -4 '())))
    (test-assert (length= '(d e f) '(a b c)))
    (test-assert (length= '() '()))
    (test-assert (not (length= '(d e) '(a b c))))
    (test-assert (not (length= '(d e) '())))
    (test-assert (not (length= '() '(a b c))))
    
    (test-assert (length> '(a b c) '(d e)))
    (test-assert (length> '(a b c) '()))
    (test-assert (not (length> '(a b c) '(d e f))))
    (test-assert (not (length> '() '())))
    (test-assert (length> '(a b c) 2))
    (test-assert (length> '(a b c) 1))
    (test-assert (length> '(a b c) 0))
    (test-assert (length> '(a b c) -1))
    (test-assert (length> '(a b c) -2))
    (test-assert (length> 4 '(a b c)))
    (test-assert (length> 4 '()))
    (test-assert (length> 1 '()))

    (test-assert (length< '(d e) '(a b c)))
    (test-assert (length< '() '(a b c)))
    (test-assert (not (length< '(d e f) '(a b c))))
    (test-assert (not (length< '() '())))
    (test-assert (length< 2 '(a b c)))
    (test-assert (length< 1 '(a b c)))
    (test-assert (length< 0 '(a b c)))
    (test-assert (length< -1 '(a b c)))
    (test-assert (length< -2 '(a b c)))
    (test-assert (length< '(a b c) 4))
    (test-assert (length< '() 4))
    (test-assert (length< '() 1))

    (test-assert (length>= '(a b c) '(d e)))
    (test-assert (length>= '(a b c) '(d e f)))
    (test-assert (length>= '() '()))
    (test-assert (length>= '(a b c) '()))
    (test-assert (not (length>= '(a b c) '(d e f g))))
    (test-assert (not (length>= '() '(d e))))
    (test-assert (not (length>= '() '(d))))
    (test-assert (length>= '(a b c) 2))
    (test-assert (length>= '(a b c) 3))
    (test-assert (length>= '() 0))
    (test-assert (length>= '(a b c) 0))
    (test-assert (not (length>= '(a b c) 4)))
    (test-assert (not (length>= '() 2)))
    (test-assert (not (length>= '() 1)))
    (test-assert (length>= 3 '(d e)))
    (test-assert (length>= 3 '(d e f)))
    (test-assert (length>= 0 '()))
    (test-assert (length>= 3 '()))
    (test-assert (not (length>= 3 '(d e f g))))
    (test-assert (not (length>= 0 '(d e))))
    (test-assert (not (length>= 0 '(d))))

    (test-assert (length<= '(d e) '(a b c)))
    (test-assert (length<= '(d e f) '(a b c)))
    (test-assert (length<= '() '()))
    (test-assert (length<= '() '(a b c)))
    (test-assert (not (length<= '(d e f g) '(a b c))))
    (test-assert (not (length<= '(d e) '())))
    (test-assert (not (length<= '(d) '())))
    (test-assert (length<= 2 '(a b c)))
    (test-assert (length<= 3 '(a b c)))
    (test-assert (length<= 0 '()))
    (test-assert (length<= 0 '(a b c)))
    (test-assert (not (length<= 4 '(a b c))))
    (test-assert (not (length<= 2 '())))
    (test-assert (not (length<= 1 '())))
    (test-assert (length<= '(d e) 3))
    (test-assert (length<= '(d e f) 3))
    (test-assert (length<= '() 0))
    (test-assert (length<= '() 3))
    (test-assert (not (length<= '(d e f g) 3)))
    (test-assert (not (length<= '(d e) 0)))
    (test-assert (not (length<= '(d) 0)))
    ))


;; (test-lists-and-sets)


;; ############################################################################
;; testing of tree.lisp
;; ----------------------------------------------------------------------------

(deftest test-tree ()
  (let ((node (make-instance 'tree-node))
	(child (make-instance 'tree-node))
        (new-node (make-instance 'tree-node))
	(tree (make-instance 'tree)))
    (test-assert (null (top tree)))
    (test-assert (and (not (children node))
		      (not (parent node))
		      (not (nodes tree))
		      (empty? tree)
		      (leaf? node)))
    (test-ok (traverse tree #'identity))
    (test-assert (top? node))
    (test-ok (add-node tree node))
    (test-assert (and (top? node)
		      (eq node (top tree))
		      (typep (top tree) 'tree-node)
		      (find node (nodes tree))))
    (add-node tree child :parent node)
    (test-assert (and (eq (first (children node)) child)
		      (eq (parent child) node)
		      (= (length (nodes tree)) 2)))
    (test-error (add-node tree (make-instance 'tree-node)))
    (test-error (add-node tree (make-instance 'tree-node) :parent (make-instance 'tree-node)))
    (test-error (add-node tree child :parent node))
    
    ;; replace-node
    (test-error (replace-node tree child node))
    (test-ok (replace-node tree child new-node))
    (test-assert (and (eq (top tree) node)
                      (eq (first (children (top tree))) new-node)
                      (= (length (children (top tree))) 1)
                      (eq (parent new-node) node)
                      (not (eq (first (children (top tree))) child))
                      (= (length (nodes tree)) 2)))

    (test-ok (replace-node tree node child))
    (test-assert (and (eq (top tree) child)
                      (eq (first (children child)) new-node)
                      (= (length (children (top tree))) 1)
                      (eq (parent new-node) child)))
    ;; depth
    (test-assert (and (= (depth (top tree)) 0)
                      (= (depth new-node) 1)))
    
    ;; parents
    (test-assert (and (= (length (parents (top tree))) 0)
                      (eq (first (parents new-node)) (top tree))
                      (= (length (parents new-node)) 1)))
    (test-error (replace-node tree node node))))


;; (test-tree)

(defun make-test-tree ()
  (let ((tree (make-instance 'tree))
        (n1 (make-instance 'tree-node :id 1))
        (n2 (make-instance 'tree-node :id 2))
        (n3 (make-instance 'tree-node :id 3))
        (n4 (make-instance 'tree-node :id 4))
        (n5 (make-instance 'tree-node :id 5))
        (n6 (make-instance 'tree-node :id 6)))
    (add-node tree n1)
    (add-node tree n2 :parent n1)
    (add-node tree n3 :parent n1)
    (add-node tree n4 :parent n3)
    (add-node tree n6 :parent n3)
    (add-node tree n5 :parent n4)
    tree))

;; Test tree:
;;
;;         1
;;       /   \
;;      3     2
;;    /   \
;;   6     4
;;         |
;;         5

(deftest test-tree-traversal ()
  (let ((tree (make-test-tree))
        (dft-visits nil)
        (bft-visits nil))
    (traverse tree (lambda (node) (push (id node) dft-visits)))
    (traverse-bft tree (lambda (node) (push (id node) bft-visits)))
    (setf dft-visits (reverse dft-visits))
    (setf bft-visits (reverse bft-visits))
    (test-assert (equal dft-visits '(1 3 6 4 5 2)))
    (test-assert (equal bft-visits '(1 3 2 6 4 5)))))

;(test-tree-traversal)

;; ############################################################################
;; testing of misc-utils.lisp
;; ----------------------------------------------------------------------------

;; Not a complete set of tests yet.

(deftest test-misc-utils ()
  (test-assert (equal '((number sg) (gender n)) (substitute-or-cons '(number sg) '((gender n)))))
  (test-assert (equal '((number pl) (gender n)) (substitute-or-cons '(number pl) '((number sg) (gender n)))))
  (test-assert (equal '(D E F G) (remove-if-member '(a b c) '(a b c d e f g))))
  (test-assert (equal '((E F)) (remove-if-member '((a b) (c d)) '((a b) (c d) (e f)) :test #'equal)))
  (test-assert (equal '((C D)) (remove-if-member '(a b) '((a b) (c d) (a c)) :key #'first)))
  (test-assert (deep-member '(a) '(a b (c (d)) (((((((((d (a))))))))))) :test #'equal))
  (test-assert (null (deep-member '(a) '(a b (c (d)) (((((((((d (a b))))))))))) :test #'equal)))
  (test-assert (deep-member 'a '(a b c)))
  (test-assert (null (deep-member 'a 'a)))
 ; (test-assert (none nil nil nil))
 ; (test-assert (not (none 'a)))
 ; (test-assert (none (= 1 2) (= 1 2)))
 ; (test-assert (not (none (= 1 2) (< 1 2))))
  )

;; (test-misc-utils)