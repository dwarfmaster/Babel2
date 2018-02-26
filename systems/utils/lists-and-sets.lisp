(in-package :utils)

(export '(mapconcat list-of-strings->string))

(defun mapconcat (function sequence separator)
  "mapconcat applies function to each element of sequence;
   the results, which must be sequences of characters (strings, vectors, or lists),
   are concatenated into a single string return value.
   Between each pair of result sequences, mapconcat inserts the characters
   from separator, which also must be a string, or a vector or list of characters.
   The argument function must be a function that can take one argument and
   returns a sequence of characters: a string, a vector, or a list.
   The argument sequence can be any kind of sequence except a char-table;
   that is, a list, a vector, a bool-vector, or a string."
  (format nil (format nil "~~{~~a~~^~a~~}" separator)
          (mapcar function sequence)))

(defun list-of-strings->string (list &key (separator " "))
  "Turn a list of strings into a single string. The indidual strings are separated by sep"
  ;; check the input
  (assert (every #'identity (mapcar #'stringp list)))
  (assert (stringp separator))
  ;; concatenate the strings
  (mapconcat #'identity list separator))


(export '(listify))

(defun listify (element)
  "Creates (list element) only when element is not (1) a list or (2)
nil."
  (if (listp element) element (list element)))

;; ############################################################################
;; list formatting:
;; ----------------------------------------------------------------------------

(export '(*english-list* *english-list2* write-list write-list2))

;; from Seibel, Practical Common Lisp.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *english-list*
    "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}")
  (defparameter *english-list2*
    "~{~#[~;~(~a~)~;~(~a~) and ~(~a~)~:;~@{~(~a~)~#[~;, and ~:;, ~]~}~]~}"))

;; Examples:
;;  (format nil *english-list* '())        ==> ""
;;  (format nil *english-list* '(1))       ==> "1"
;;  (format nil *english-list* '(1 2))     ==> "1 and 2"
;;  (format nil *english-list* '(1 2 3))   ==> "1, 2, and 3"
;;  (format nil *english-list* '(1 2 3 4)) ==> "1, 2, 3, and 4"

(defun write-list (elements &optional (stream t))
  (format stream #.*english-list* elements))

(defun write-list2 (elements &optional (stream t))
  (format stream #.*english-list2* elements))


;; ############################################################################
;; associative list utilities:
;; ----------------------------------------------------------------------------

(export '(assq assqv))

(declaim (inline assq))

(defun assq (obj list &key (test #'eq))
  "Calls assoc with eq as test predicate."
  (declare (type list list))
  (assoc obj list :test test))

(declaim (inline v))

(defun assqv (obj list &key (test #'eq))
  "Calls assoc with eq as test predicate, and returns cdr of found value or nil."
  (declare (type list list))
  (cdr (assoc obj list :test test)))

;; ############################################################################
;; list length utilities:
;; ----------------------------------------------------------------------------

(export '(length=
          length>
          length<
          length>=
          length<=))

(defun list-length=n (list length)
  (declare (list list) (integer length))
  (unless (< length 0)
    (loop (cond ((null list) (return (zerop length)))
                ((zerop length) (return nil)))
          (pop list) (decf length))))

(defun list-length=l (list1 list2)
  (declare (list list1 list2))
  (loop (cond ((null list1) (return (null list2)))
              ((null list2) (return nil)))
        (pop list1) (pop list2)))

(defun list-length>n (list length)
  (declare (list list) (integer length))
  (loop (cond ((null list) (return (< length 0)))
              ((<= length 0) (return t)))
        (pop list) (decf length)))

(defun list-length>l (list1 list2)
  (declare (list list1 list2))
  (loop (cond ((null list1) (return nil))
              ((null list2) (return t)))
        (pop list1) (pop list2)))

(defun list-length<n (list length)
  (declare (list list) (integer length))
  (loop (cond ((null list) (return (> length 0)))
              ((<= length 1) (return nil)))
        (pop list) (decf length)))

(defun list-length<l (list1 list2)
  (declare (list list1 list2))
  (loop (cond ((null list2) (return nil))
              ((null list1) (return t)))
        (pop list1) (pop list2)))

(defun list-length>=n (list length)
  (declare (list list) (integer length))
  (loop (cond ((null list) (return (<= length 0)))
              ((<= length 1) (return t)))
        (pop list) (decf length)))

(defun list-length>=l (list1 list2)
  (declare (list list1 list2))
  (loop (cond ((null list1) (return (null list2)))
              ((null list2) (return t)))
        (pop list1) (pop list2)))

(defun list-length<=n (list length)
  (declare (list list) (integer length))
  (loop (cond ((null list) (return (>= length 0)))
              ((<= length 0) (return nil)))
        (pop list) (decf length)))

(defun list-length<=l (list1 list2)
  (declare (list list1 list2))
  (loop (cond ((null list2) (return (null list1)))
              ((null list1) (return t)))
        (pop list1) (pop list2)))

(defun length= (arg1 arg2)
  (etypecase arg1
    (integer (etypecase arg2
               (integer (warn "Length= used to compare two integers ~S and ~S." arg1 arg2)
                        (= arg1 arg2))
               (vector (= arg1 (length arg2)))
               (list (list-length=n arg2 arg1))))
    (vector (etypecase arg2
              (integer (= (length arg1) arg2))
              (vector (= (length arg1) (length arg2)))
              (list (list-length=n arg2 (length arg1)))))
    (list (etypecase arg2
            (integer (list-length=n arg1 arg2))
            (vector (list-length=n arg1 (length arg2)))
            (list (list-length=l arg1 arg2))))))

(defun length> (arg1 arg2)
  (etypecase arg1
    (integer (etypecase arg2
               (integer (warn "Length> used to compare two integers ~S and ~S." arg1 arg2)
                        (> arg1 arg2))
               (vector (> arg1 (length arg2)))
               (list (list-length<n arg2 arg1))))
    (vector (etypecase arg2
              (integer (> (length arg1) arg2))
              (vector (> (length arg1) (length arg2)))
              (list (list-length>n arg2 (length arg1)))))
    (list (etypecase arg2
            (integer (list-length>n arg1 arg2))
            (vector (list-length>n arg1 (length arg2)))
            (list (list-length>l arg1 arg2))))))

(defun length< (arg1 arg2)
  (etypecase arg1
    (integer (etypecase arg2
               (integer (warn "Length< used to compare two integers ~S and ~S." arg1 arg2)
                        (< arg1 arg2))
               (vector (< arg1 (length arg2)))
               (list (list-length>n arg2 arg1))))
    (vector (etypecase arg2
              (integer (< (length arg1) arg2))
              (vector (< (length arg1) (length arg2)))
              (list (list-length>n arg2 (length arg1)))))
    (list (etypecase arg2
            (integer (list-length<n arg1 arg2))
            (vector (list-length<n arg1 (length arg2)))
            (list (list-length<l arg1 arg2))))))

(defun length>= (arg1 arg2)
  (etypecase arg1
    (integer (etypecase arg2
               (integer (warn "Length>= used to compare two integers ~S and ~S." arg1 arg2)
                        (>= arg1 arg2))
               (vector (>= arg1 (length arg2)))
               (list (list-length<=n arg2 arg1))))
    (vector (etypecase arg2
              (integer (>= (length arg1) arg2))
              (vector (>= (length arg1) (length arg2)))
              (list (list-length<=n arg2 (length arg1)))))
    (list (etypecase arg2
            (integer (list-length>=n arg1 arg2))
            (vector (list-length>=n arg1 (length arg2)))
            (list (list-length>=l arg1 arg2))))))

(defun length<= (arg1 arg2)
  (etypecase arg1
    (integer (etypecase arg2
               (integer (warn "Length<= used to compare two integers ~S and ~S." arg1 arg2)
                        (<= arg1 arg2))
               (vector (<= arg1 (length arg2)))
               (list (list-length>=n arg2 arg1))))
    (vector (etypecase arg2
              (integer (<= (length arg1) arg2))
              (vector (<= (length arg1) (length arg2)))
              (list (list-length>=n arg2 (length arg1)))))
    (list (etypecase arg2
            (integer (list-length<=n arg1 arg2))
            (vector (list-length<=n arg1 (length arg2)))
            (list (list-length<=l arg1 arg2))))))


;; ############################################################################
;; list utilities:
;; ----------------------------------------------------------------------------

(export '(member-equal
	  pushend
          filter ;; this export is only useful for the class 'filter elsewhere, not for the function filter deprecated here
	  toggle
	  mappend
	  mapunion
	  listXlist
	  flatten
	  combinations
	  combinations-of-length
	  selections-of-length
          set-partitions
          all-subsets
          random-subset
	  decompose-list
	  decompose-list-into
	  sorted-insert
          binary-search
	  sorted-p
          collect-ignore-nils
          remove-first
          remove-nth
          cartesian-product))

(defun remove-first (item sequence &key (test 'equal) test-not)
  (cond
   ((null sequence)
    nil)
   ((or (and test-not
             (not (funcall test-not item (first sequence))))
        (and test (funcall test item (first sequence))))
    (rest sequence))
   (t (let ((new-rest (remove-first item (rest sequence))))
        (if (eq new-rest (rest sequence))
          sequence
          (cons (first sequence) new-rest))))))

(defun remove-nth (nth sequence &key)
  (assert (> nth -1))
  (cond
   ((null sequence)
    nil)
   ((= nth 0)
    (rest sequence))
   (t (let ((new-rest (remove-nth (- nth 1) (rest sequence))))
        (if (eq new-rest (rest sequence))
          sequence
        (cons (first sequence)
              new-rest))))))

(defmacro pushend (item location)
  "Append the item to the list in the given location, and returns the list."
  ;; #-sbcl (warn "The use of pushend is very expensive. Try to reformulate the algorithm to not use pushend. - Pascal")
  `(setf ,location (append ,location (list ,item))))
  ;;`(if (null ,location)
  ;;     (setf ,location (list ,item))
  ;;     (setf (cdr (last ,location)) (list ,item))))

(defmacro toggle (var &rest vars)
  (cons 'progn (loop for v in (cons var vars)
		  collect `(setq ,v (not ,v)))))

(defun mappend (fn &rest lists)
  "Append the results of calling fn on each element of list"
  (declare (dynamic-extent lists))
  (loop for result in (apply #'mapcar fn lists) append result))

(define-compiler-macro mappend (fn &rest lists)
  (let ((result (gensym)))
    `(loop for ,result in (mapcar ,fn ,@lists) append ,result)))

(defun mapunion (fn &rest lists)
  (declare (dynamic-extent lists))
  (delete-duplicates
   (loop for list in lists
      nconc (loop for elm in list
	       collect (funcall fn elm)))))

(defun flatten (list)
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0)))
  (labels ((rec (obj acc)
             (cond ((null obj) acc)
                   ((atom obj) (cons obj acc))
                   (t (rec (car obj) (rec (cdr obj) acc))))))
    (rec list '())))

(defun listXlist (l1 l2 &key (combiner #'list))
  (declare (list l1) (list l2) (function combiner))
  (loop for el1 in l1 nconc
        (loop for el2 in l2 collect
               (funcall combiner el1 el2))))

(defun combinations (&rest lists)
  (reduce #'(lambda (ac new) 
              (listXlist new ac :combiner #'cons))
          (push '(()) lists)))

(defun combinations-of-length (elements length)
  (declare (list elements) (fixnum length))
  (cond ((= 0 length) '(()))
        ((null elements) nil)        
        (t (nconc
            (mapcar #'(lambda (c)
                        (cons (first elements) c))
                    (combinations-of-length (rest elements) (- length 1)))
            (combinations-of-length (rest elements) length)))))

(defun set-partitions (list &key (min-length 1) max-length)
  "computes a list of all partitions of a set:
   (set-partitions '(a b c))
   -> (((a b c)) ((a b) (c)) ((a c) (b)) ((a) (b c)) ((a) (b) (c)))
   algorithm from Michael Orlov (2002). Efficient Generation of Set Partitions."
  (loop with n = (length list)
     with k = (make-array (list n) :initial-element 0)
     with M = (make-array (list n) :initial-element 0)
     for partition = (loop for i from 0 to (- n 1)
                        for subset = (loop for j from 0 to (- n 1)
                                        when (= i (aref k j)) collect (nth j list))
                        when subset collect subset)
     for length = (length partition)
     when (and (>= length min-length) (or (not max-length) (<= length max-length)))
     collect partition into partitions
     while (loop for i from (- n 1) downto 1
              when (<= (aref k i) (aref M (- i 1)))
              do (incf (aref k i))
                (setf (aref M i) (max (aref k i) (aref M i)))
                (loop for j from (+ i 1) to (- n 1)
                   do (setf (aref k j) (aref k 0))
                     (setf (aref M j) (aref M i)))
                (return t))
     finally (return partitions)))

(defun all-subsets (list &key (min-length 1))
  "a slow hack for getting all subsets of a list. needs to be properly written"
  (reverse 
   (loop for x in (remove-duplicates 
                   (apply #'append (set-partitions list)) :test #'equal)
      when (>= (length x) min-length)
      collect x)))

(defun random-subset (list &key (include-empty-set? t))
  "Returns a random subset both in the amount of elements and in which
elements it picks."
  (let ((result (random-elts list (random (1+ (length list))))))
    (if (and (not include-empty-set?)
             (null result))
        (random-subset list :include-empty-set? include-empty-set?)
        result)))

(defun simple-cartesian-product (set list)
   (loop for elm in set
         nconc (loop for set in list
                     collect (cons elm set))))

(defun cartesian-product (&rest list-of-sets)
  "Cartesian product of sets '(a b) (c d) => '((a c) (b d) (a d) (b c))"
   (reduce #'simple-cartesian-product list-of-sets
           :from-end t
           :initial-value '(())))

(defun true (&rest args)
  (declare (ignore args))
  t)

(defun sorted-insert (list to-insert &key (key #'identity) (test #'<))
  "Assumes a list sorted on func. Will insert the given el in the
list based on test. Example: (sorted-insert '(1 2 4 5) 3) --> (1 2 3 4 5)"
  (loop with prefix
        for postfix on list
        for elm = (first postfix)
        if (funcall test (funcall key elm) (funcall key to-insert))
        do (push elm prefix)
        else do (loop-finish)
        finally (return (nreconc (cons to-insert prefix) postfix))))

(defun %fast-sorted-insert-default-test (list to-insert key)
  (loop with prefix
        for postfix on list
        for elm = (first postfix)
        if (< (funcall key elm) (funcall key to-insert))
        do (push elm prefix)
        else do (loop-finish)
        finally (return (nreconc (cons to-insert prefix) postfix))))

(defun %fast-sorted-insert-default-key (list to-insert test)
  (loop with prefix
        for postfix on list
        for elm = (first postfix)
        if (funcall test elm to-insert)
        do (push elm prefix)
        else do (loop-finish)
        finally (return (nreconc (cons to-insert prefix) postfix))))

(defun %fast-sorted-insert-default-key-and-test (list to-insert)
  (loop with prefix
        for postfix on list
        for elm = (first postfix)
        if (< elm to-insert)
        do (push elm prefix)
        else do (loop-finish)
        finally (return (nreconc (cons to-insert prefix) postfix))))

(define-compiler-macro sorted-insert (&whole w list to-insert &key (key nil keyp) (test nil testp))
  (let ((default-key (or (not keyp)
                         (equal key '(function identity))
                         (equal key '(quote identity))))
        (default-test (or (not testp)
                          (equal test '(function <))
                          (equal test '(quote <)))))
    (if default-key
      (if default-test
        `(%fast-sorted-insert-default-key-and-test ,list ,to-insert)
        `(%fast-sorted-insert-default-key ,list ,to-insert ,test))
      (if default-test
        `(%fast-sorted-insert-default-test ,list ,to-insert ,key)
        w))))


;; sorted lists/vectors are amenable to a binary search to speed up
;; finding elements which have a specific weight
;; 
;; on lists sequential search via (find) will be faster because random
;; access is so expensive, but for vectors binary search will result
;; in a speedup from average Theta(N/2) to exactly Theta(log2(N))!
(defun binary-search (sorted-vector value &key (key #'identity) (test #'<=))
  "Performs a binary search on the given vector and returns the first
element for which the sought value satisfies the test"
  ; i.e. (binary-search <.1 .4 .9> .3) will return .4 because
  ; (< .3 .4) => T but (< .3 .1) => NIL
  (declare (type vector sorted-vector))
  ; could probably be made even more efficient by actually only using
  ; powers of two instead of doing normal division+truncate
  (loop
     with low = 0
     with high = (1- (length sorted-vector))
     for mid = (+ low (truncate (/ (- high low) 2)))
     do
       (if (funcall test value (funcall key (aref sorted-vector mid)))
           (setf high mid)
           (setf low (1+ mid)))
     while (< low high)
     finally
       (if (funcall test value (funcall key (aref sorted-vector low)))
           (return (aref sorted-vector low)))))


(defun collect-ignore-nils (&rest lists)
  "Like (list) except that it does not put nil elements into the list"
  (loop for element in lists
     when element
     collect it))

(export '(list->array array->list))

(defun list->array (list)
  "Returns the given list as an array"
  (let ((list-length (length list)))
    (loop 
       with result = (make-array list-length :fill-pointer 0)
       for el in list
       do (vector-push el result)
       finally (return (values result list-length)))))

(defun array->list (array)
  "Returns the given array as a list"
  (loop for el across array
       collect el))

;; ############################################################################
;; list creation utilities:
;; ----------------------------------------------------------------------------

(export '(full-list
          zeros-list
          ones-list
          nil-list
          randint))

(defun _full-list (&key (dimensions nil) fill-value)
  "Returns a list in the given dimensions, filled with fill-value.
   fill-value needs to be a lambda, since it is evaluated every time.
   This is useful when generating random numbers."
  (if (null dimensions)
    (funcall fill-value)
    (loop as i below (first dimensions)
          collect (_full-list :dimensions (rest dimensions) :fill-value fill-value))))

(defun full-list (&key (dimensions nil) (fill-value nil))
  "Returns a list in given dimensions, filled with fill-value."
  (_full-list :dimensions dimensions :fill-value (lambda () fill-value)))

(defun zeros-list (&key (dimensions nil))
  "Returns a list in given dimensions, filled with 0."
  (full-list :dimensions dimensions :fill-value 0))

(defun ones-list (&key (dimensions nil))
  "Returns a list in given dimensions, filled with 1."
  (full-list :dimensions dimensions :fill-value 1))

(defun nil-list (&key (dimensions nil))
  "Returns a list in given dimensions, filled with nil."
  (full-list :dimensions dimensions :fill-value nil))

(defun randint (&key (start 0) (end (1+ start)) (dimensions nil))
  "Returns a list in given dimensions, filled with random integers in [start, end];
   start and end are inclusive. When called without arguments, returns either 0 or 1."
  (_full-list :dimensions dimensions :fill-value (lambda () (+ start (random (+ 1 (- end start)))))))


   
;; ############################################################################
;; list randomize utilities:
;; ----------------------------------------------------------------------------

(export '(simple-shuffle
	  shuffle
	  permutate-list
	  permutation-of?
	  duplicates?
	  random-elt
	  random-elts
	  random-other
	  random-elt-if
          random-shuffle
	  permutations-of-length))

(defun simple-shuffle (list)
  "A very naive non destructive shuffle implementation."
  (loop 
     with copy-list = (copy-list list)
     for i from 1 to (length list)
     for new-el = (random-elt copy-list)
     do (setf copy-list (remove new-el copy-list :count 1))
     collect new-el))

(defun shuffle (l)
  (let ((vector (coerce l 'simple-vector)))
    (loop for i of-type fixnum from (length vector) downto 2
          do (rotatef (svref vector (1- i)) (svref vector (random i))))
    (coerce vector 'list)))

(defun permutate-list (list)
  "Permutates a list containing the permuted elements of the given list."
  (shuffle list))

(defun permutation-of? (l1 l2 &key (key #'identity) (test #'eql))
  #-lispworks
  (declare (list l1) (list l2) (function key) (function test))
  (and (= (length l1) (length l2))
       (let ((fail nil)
	     (cl2 (copy-list l2)))
	 (loop for el1 in l1 until fail do
	       (let ((m (member (funcall key el1) cl2 :key key :test test)))
		 (if m 
		     (if (cdr m)
			 (setf (car m) (cadr m)
			       (cdr m) (cddr m))
		       (setf cl2 (reverse (rest (reverse cl2)))))
		   (setq fail t))))
	 (not fail))))

(defun duplicates? (list &key (test #'equal) (key #'identity))
  "Checks whether an element occurs twice in the same list. Only use on short lists."
  (declare (function test) (function key))
  (when (and list (listp list)) ;; Check whether it is a list with more than one element.
    (labels ((check-for-duplicate (first-element rest-of-list original-list) ;; Define a local recursive function.
               (cond ((null rest-of-list) nil)
                     ((funcall test
                               (funcall key first-element)
                               (funcall key (if (symbolp rest-of-list) ;; For handling cons-lists.
                                              rest-of-list
                                              (first rest-of-list))))
                      original-list)
                     ((symbolp rest-of-list) nil)
                     (t
                      (check-for-duplicate first-element (rest rest-of-list) original-list))))) ;; Tail recursion.
      (or (check-for-duplicate (first list) (rest list) list) ;; If the first element occurs more than once, return the list
          (duplicates? (rest list) :test test :key key))))) ;; Else recursively check the remaining elements.
;; (duplicates? '(a b . c)) --> NIL
;; (duplicates? '(a b . a)) --> (A B . A)
;; (duplicates? '(a b . b)) --> (B . B)
;; (duplicates? '(a b c)) --> NIL
;; (duplicates? '(a b a)) --> (A B A)
;; (duplicates? '(a b b)) --> (B B)

;;#+ccl(make-random-state t) ;<-- This is *very* slow, but it's often necessary, so why comment it out (paul asks)?
(defun random-elt (l)
  (declare (list l))
  (unless (null l)
    (elt l (random (length l) #+ccl(make-random-state t)))))

(defun random-elts (seq n)
  (declare (list seq) (fixnum n))
  (subseq (shuffle seq) 0 n))

(defun random-other (elts seq)
  (random-elt (set-difference seq elts)))

(defun random-elt-if (predicate list)
  "Returns a random element that satisfies the predicate"
  (random-elt (remove-if-not predicate list)))

(defun random-shuffle (sequence)
  "Randomly orderes the elements in sequence"
  (declare (optimize speed))
  (map-into sequence #'car
            (sort (map 'vector (lambda (x)
                                 (cons x (random 1.0 #+ccl(make-random-state t))))
                       sequence)
                  #'< :key #'cdr)))

(defun permutations-of-length (elements length &optional (copy-fn #'identity))
  (declare (list elements) (fixnum length) (function copy-fn))
  (cond ((= 0 length) nil)
        ((= 1 length) (mapcar #'(lambda (e) (list (funcall copy-fn e))) elements))
        (t (let ((result nil))
             (do* ((prev nil (cons e prev))
                   (e (first elements) (first rest))
                   (rest (rest elements) (rest rest)))
		  ((null e) result)
               (setf result
		     (append (mapcar #'(lambda (p)
					 (cons (funcall copy-fn e) p))
				     (permutations-of-length (append prev rest)
							     (- length 1)))
			     result)))))))


;; ############################################################################
;; list extremes utilities:
;; ----------------------------------------------------------------------------

(export '(the-highest
	  the-x-highest
	  simple-sort
	  the-biggest
	  all-biggest
	  the-smallest
	  all-smallest))

(defun the-highest (lst func)
  "Loops over the given list and evaluates function on every
   item. It returns the item for which the function returned the
   highest value. If there are multiple it gives the first
   one."
  (declare (type list lst)
	   (type function func))
  (when lst
      (loop 
	 for item  in (rest lst)
	 with result = (first lst)
	 with current-highest = (funcall func result)
	 for current-eval = (funcall func item)
	 when (> current-eval current-highest)
	 do
	   (setf result item)
	   (setf current-highest current-eval)
	 finally (return result))))

(defun the-x-highest (lst nr-of-highest &key (key #'identity))
  "Loops over the given list and evaluates function on every
   item. It returns the item for which the function returned the
   highest value. If there are multiple it gives the first
   one. You must supply a third parameter which allows you to get
   the x highest elements. It will even return these x elements
   ordered so from high to low."
  (declare (type list lst)
	   (type function key))
  ;;(format t "~%The ~a highest of:~%~a" nr-of-highest lst)
  (when lst
    (let* ((result (list (first lst))))
      (loop for item  in (rest lst)
	 when (or (> (funcall key item) (funcall key (first result)))
		  (< (length result) nr-of-highest))
	 do
	   (setf result (sorted-insert result item :test #'< :key key))
	   (when (> (length result) nr-of-highest)
	     (pop result)))
      (reverse result))))

(defun simple-sort (lst &key (test #'<) (key #'identity))
  (declare (type list lst)
	   (type function test)
	   (type function key))
  (loop for item  in (rest lst)
     with result = (list (first lst))
     do (setf result (sorted-insert result item :test test :key key))
     finally (return result)))

(defun the-biggest (fn l &optional cut-of)
  (declare (list l) (function fn))
  (when l
    (let ((biggest (list (first l)))
	  (best-val (funcall fn (first l))))
      (loop for x in (rest l) 
	 until (and cut-of (>= best-val cut-of))
	 do (let ((val (funcall fn x)))
	      (cond ((= val best-val)
		     (push x biggest))
		    ((> val best-val)
		     (setf (car biggest) x)
		     (setf (cdr biggest) nil)
		     (setq best-val val)))))
      (values (random-elt biggest) best-val))))

(defun all-biggest (fn l &key (key #'identity))
  (declare (list l) (function fn))
  (when l
    (let ((biggest (list (first l)))
	  (best-val (funcall fn (funcall key (first l)))))
      (dolist (x (rest l))
	(let ((val (funcall fn (funcall key x))))
	  (cond ((= val best-val)
		 (push x biggest))
		((> val best-val)
		 (setf (car biggest) x)
		 (setf (cdr biggest) nil)
		 (setq best-val val)))))
      (values biggest best-val))))

(defun the-smallest (fn l)
  (declare (function fn) (list l))
  (the-biggest (compose #'- fn) l))

(defun all-smallest (fn l)
  (declare (function fn) (list l))
  (all-biggest (compose #'- fn) l))


;; ############################################################################
;; list/tree search utilities:
;; ----------------------------------------------------------------------------

(export '(get-duplicate-elements
          find-of-type
          find-all
	  find-all-if
	  find-all-if-not
          find-all-of-type
	  find-anywhere
	  find-all-anywhere
          find-all-anywhere-of-type
          find-all-anywhere-if
          member-of-tree
          replace-by-variable
          deep-reverse))

(defun get-duplicate-elements (lst)
  (cond ((null lst) '())
        ((member (car lst) (cdr lst))
         (cons (car lst) (get-duplicate-elements (cdr lst))))
        (t (get-duplicate-elements (cdr lst)))))

(defun find-of-type (type lst &key (key #'identity))
  (declare (function key))
  (loop for el in lst when (typep (funcall key el) type) do (return el)))

(defun find-all (what lst &key (test #'eq) (key #'identity))
  (declare (function test) (function key))
  (loop for el in lst when (funcall test what (funcall key el)) collect el))

(defun find-all-of-type (type lst &key (key #'identity))
  (declare (function key))
  (loop for el in lst when (typep (funcall key el) type) collect el))

(defun find-all-if (test lst &key (key #'identity))
  (declare (function test) (function key))
  (loop for el in lst when (funcall test (funcall key el)) collect el))

(defun find-all-if-not (test lst &key (key #'identity))
  (declare (function test) (function key))
  (loop for el in lst when (not (funcall test (funcall key el))) collect el))

(defun find-anywhere (item tree &key (test #'eq) (key #'identity))
  (declare (function test))
  (cond ((funcall test item (funcall key tree)) tree)
	((consp tree)
	 (or (find-anywhere item (car tree) :test test :key key)
	     (find-anywhere item (cdr tree) :test test :key key)))))

(defun find-all-anywhere (item tree &key (test #'eq) (key #'identity))
  (declare (function test))
  (cond ((funcall test item (funcall key tree)) (list tree))
	((consp tree)
	 (append (find-all-anywhere item (car tree) :test test :key key)
		 (find-all-anywhere item (cdr tree) :test test :key key)))))

(defun find-all-anywhere-of-type (type tree &key (key #'identity))
  (cond ((typep (funcall key tree) type) (list tree))
	((consp tree)
	 (append (find-all-anywhere-of-type type (car tree) :key key)
		 (find-all-anywhere-of-type type (cdr tree) :key key)))))

(defun find-all-anywhere-if (test tree &key (key #'identity))
  (declare (function test))
  (cond ((funcall test (funcall key tree)) (list tree))
        ((consp tree)
         (append (find-all-anywhere-if test (car tree) :key key)
                 (find-all-anywhere-if test (cdr tree) :key key)))))

(defun member-of-tree (item tree)
  "looks whether feature-value or symbol is member of tree (any depth)"
  (cond ((atom tree)
         (cond ((and (atom item) (stringp tree) (string= tree item))
                t)
               ((and (atom item) (string= item (write-to-string tree))) ;; for when leaves are numbers
                t)
               (t nil)))
        ((equal item (car tree))
         t)
        (t
         (or (member-of-tree item (car tree))
             (member-of-tree item (cdr tree))))))

(defun replace-by-variable (item tree)
  "replaces feature-value or symbol in tree with new variable (any depth)"
  (cond ((atom tree)
         tree)
        ((equal item (car tree))
         (replace-by-variable item (cons (make-var 'neg) (cdr tree))))
        (t
         (cons (replace-by-variable item (car tree))
               (replace-by-variable item (cdr tree))))))

(defun deep-reverse (tree)
  "Reverses a tree recursively (deep)"
  (if (atom tree)
    tree
    (mapcar #'deep-reverse (reverse tree))))

;; ############################################################################
;; tree utilities:
;; ----------------------------------------------------------------------------

(export '(tree-difference _))

(defgeneric tree-difference (a b)
  (:documentation "returns a list of things that are in b but not in a"))

(defmethod tree-difference ((a list) (b list))
  (loop with result = nil 
     for y in b 
     do (cond 
	  ;; (foo .. .. .. ...)
	  ((and (consp y) (atom (first y)))
	   (let ((x (find (first y) a :key #'first :test #'equal)))
	     (if x 
		 (progn
		   (setf a (remove x a)) 
		   (push (tree-difference x y) result))
		 (push y result))))
	  ;; ((foo ... ...) ....)
	  ((consp y)
	   (push (tree-difference (pop a) y) result))
	  ;; foo or "foo"
	  ((atom y)
	   (let ((x (find y a :test #'equal)))
	     (if x 
		 (progn
		   (setf a (remove x a))
		   (push '_ result))
		 (push y result))))
	  (t (error "this should not happen")))
     finally (return (reverse result))))


;; ############################################################################
;; set utilities:
;; ----------------------------------------------------------------------------

(export '(union+
	  is-set
	  is-subset
	  equal-sets
	  eq-sets
	  eql-sets))

(defun union+ (&rest sets)
  "Returns the union of any number of sets."
  (if (cdr sets)
      (union (car sets) (union+ (cdr sets)))
      (car sets)))

(defun is-set (list &key (test #'eq) key)
  "Return true if the given list does not contain the same element twice."
  (declare (type list list)
	   (type function test)
	   (type (or null function) key))
  (loop
   for (el . rest) on list
   never (member el rest :key key :test test)))

(defun is-subset (sub-set super-set &key (test #'eq) key)
  "Return true if each member of the given sub-set is a member of the
   given super-set. This function assumes that the given list are sets!"
  (declare (type list sub-set super-set)
	   (type function test))
  (and (< (length sub-set) (length super-set))
       (loop for el in sub-set
	     always (member el super-set :test test
                            :key key))))

(defun equal-sets (set-1 set-2 &key (test #'equal) key)
  "Return true if the given sets contain the same elements."
  (declare (type list set-1 set-2)
	   (type function test))
  (and (= (length set-1) (length set-2))
       (loop for e in set-1
	     always (member e set-2 :test test :key key))))

(defun eq-sets (set-1 set-2)
  "Return true if the given sets contain the same elements."
  (declare (type list set-1 set-2))
  (equal-sets set-1 set-2 :test #'eq))

(defun eql-sets (set-1 set-2)
  "Return true if the given sets contain the same elements."
  (declare (type list set-1 set-2))
  (equal-sets set-1 set-2 :test #'eql))

;; ############################################################################
;; boolean utilities:
;; ----------------------------------------------------------------------------

(export '(always))

(defun always (&rest elements)
  "Returns true if all elements evaluate to true."
  (loop for element in elements
        always element))

;; ############################################################################
;; hash-table utilities:
;; ----------------------------------------------------------------------------

(export '(copy-hash-table add-hash-table))

(defun copy-hash-table (source)
  (declare (type hash-table source))
  (let ((copy (make-hash-table
	       :test (hash-table-test source)
	       :size (hash-table-size source)
	       :rehash-size (hash-table-rehash-size source)
	       :rehash-threshold (hash-table-rehash-threshold source))))
    (maphash #'(lambda (key val)
		 (setf (gethash key copy) val))
	     source)
    copy))

(defun add-hash-table (table1 table2 &key overwrite)
  "copies table2 into table1"
  (maphash #'(lambda (key val)
               (when (or overwrite
                         (null (nth-value 1 (gethash key table1))))
                 (setf (gethash key table1) val)))
           table2))

;; ############################################################################


(defun sorted-p (list predicate &key (key #'identity))
  (cond ((or (null list) 
	     (= 1 (length list))) t)
	(t (let ((ok t))
	     (do ((remaining list (rest remaining)))
		 ((or (null (rest remaining))
		      (not ok)) ok)
	       (setq ok (funcall predicate 
				 (funcall key (first remaining))
				 (funcall key (second remaining)))))))))


; functions to find the extremum (and/or the extremum's position) in a list
(export '(extremum extremum-position))

(defun extremum (list &key (key #'identity) (test #'>))
  "Returns the element of the list which maximises/minimises the given
key. This is the element which would come first if you sorted the list
with the same key + test."
  (loop for item in list
     for value = (funcall key item)
     ; store the current maximum in a (value item) pair so we don't
     ; constantly have to re-calculate the value of the key
     as max-item = (cons value item)
     then (if (funcall test value (car max-item))
              (cons value item)
              max-item)
     finally (return (cdr max-item))))

(defun extremum-position (list &key (key #'identity) (test #'>))
  "Returns the position of the element in the list which
maximises/minimises the given key. This is the position of the element
which would come first if you sorted the list with the same key +
test."
  (loop for item in list
     for value = (funcall key item)
     and position from 0
     ; store the current maximum in a (value item) pair so we don't
     ; constantly have to re-calculate the value of the key
     as max-item = (cons value position)
     then (if (funcall test value (car max-item))
              (cons value position)
              max-item)
     finally (return (cdr max-item))))

; tools to merge multiple sorted lists together
(export '(sorted-list-merger next-item nmerge))

(defclass sorted-list-merger ()
  ((sorted-lists :type list :initarg :lists :accessor lists
  :documentation "A list of lists, each of which is sorted according
  to some attribute")
   (key :type function :initarg :key :accessor key :initform #'identity
        :documentation "The key after which the lists are sorted")
   (test :type function :initarg :test :accessor test :initform #'>
         :documentation "The test after which the keys are sorted")))

(defmethod next-item ((sorted-list-merger sorted-list-merger))
  (let ((index (extremum-position
                (lists sorted-list-merger)
                :key (lambda (rest-list) (funcall
                                          (key sorted-list-merger) (first rest-list))))))
    (when index ; could be nil if we've finished up all lists
      (if (cdr (nth index (lists sorted-list-merger)))
          (pop (nth index (lists sorted-list-merger)))
          ; if the would-be popped element is the last one of this
          ; list, remove the list from the remaining sorted lists
          (let ((element (first (nth index (lists sorted-list-merger)))))
            (setf (lists sorted-list-merger) (delete-if (lambda (x)
                                                          (declare (ignore x)) t)
                                                        (lists sorted-list-merger)
                                                        :start index :count 1))
            element)))))

(defun nmerge (lists &key (key #'identity) (test #'>))
  "Non-destructively merges the ordered lists contained in the passed
list together into one ordered list. The list content itself is not
copied, but the resulting list is made up of new conses so as not to
destroy the original lists"
  (loop
     with iterator = (make-instance 'sorted-list-merger :lists lists :key key :test test)
     for item = (next-item iterator)
     while item
     collect item))


