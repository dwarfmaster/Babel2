;;; Some additional math functions

(in-package :utils)

(export '(sum
          multf
	  compose
	  average
          median
	  stdev
	  correlation
	  combination
          permutations
          rad-to-deg
          deg-to-rad
          euclidean
          iota))

(declaim (inline sum))
(defun sum (values)
  (reduce #'+ values))

#| not used
(defun random-between (a b)
  "a random number in the interval [a b]"
  (declare (real a) (real b))
  (if (= a b) a
    (+ a (random (- b a)))))
|#

(define-modify-macro multf (&rest args)
  * "multiplies x with a number")

(defun compose (f g)
  (declare (function f) (function g))
  "Return a function h such that (h x) = (f (g x))."
  #'(lambda (x) (funcall f (funcall g x))))

(defun average (list &key (key #'identity))
  "the average of a list"
  (declare (list list) (function key))
  (loop for elm in list
        count t into length
        sum (funcall key elm) into sum
        finally (return (if (= length 0)
                          0 (coerce (/ sum length) 'float)))))

(defun %fast-average (list)
  (declare (list list))
  (loop for elm in list
        count t into length
        sum elm into sum
        finally (return (if (= length 0)
                          0 (coerce (/ sum length) 'float)))))

(define-compiler-macro average (&whole w list &key (key nil keyp))
  (if (or (not keyp)
          (equal key '(function identity))
          (equal key '(quote identity)))
    `(%fast-average ,list)
    w))

(defun median (list &key (already-sorted nil) (key #'identity))
  "the median of a list"
  (unless already-sorted 
    (setf list (sort (copy-list list) #'< :key key)))
  (let* ((length (length list))
         (half-length (/ length 2)))
    (if (evenp length)
        (/ (+ (nth (1- half-length) list)
              (nth half-length list)) 2)
        (nth (floor half-length) list))))

;; (defun stdev (l &key average (key #'identity))
;;   "the standard deviation of a list"
;;   (declare (list l) (function key))
;;   (let ((avg (or average (average l :key key))))
;;     (expt
;;      (/ (reduce #'+ (mapcar #'(lambda (e) 
;;                                 (expt (- (funcall key e) avg) 2))
;;                             l))
;;         (length l)
;; 	1.0)
;;      0.5)))

(defun stdev (list &key average (key #'identity))
  (declare (list list) (function key))
  (if list
      (let ((avg (or average (average list :key key))))
        (loop for elm in list
           count t into length
           sum (expt (- (funcall key elm) avg) 2) into sum
           finally (return (expt (coerce (/ sum length) 'float) 0.5))))
      0))

(defun %fast-stdev (list average)
  (declare (list list))
  (if list 
      (let ((avg (or average (average list))))
        (loop for elm in list
           count t into length
           sum (expt (- elm avg) 2) into sum
           finally (return (expt (coerce (/ sum length) 'float) 0.5))))
      0))

(define-compiler-macro stdev (&whole w list &key average (key nil keyp))
  (if (or (not keyp)
          (equal key '(function identity))
          (equal key '(quote identity)))
    `(%fast-stdev ,list ,average)
    w))

(defun correlation (datapoints)
  "Datapoints should be a list of (x . y) pairs. Returns values a,b,r**2 for
which y=a+bx is a least square fitting with the square of the correlation
coefficient equal to r**2."
  (labels ((ss (key1 key2 avg1 avg2)
	     (loop for p in datapoints sum
		   (* (- (funcall key1 p) avg1)
		      (- (funcall key2 p) avg2)))))
    (let* ((avgx (average datapoints :key #'car))
	   (avgy (average datapoints :key #'cdr))
	   (ssxx (ss #'car #'car avgx avgx))
	   (ssxy (ss #'car #'cdr avgx avgy))
	   (ssyy (ss #'cdr #'cdr avgy avgy))
	   (b (if (> ssxx 0) (/ ssxy ssxx) 0.0)))
      (values (- avgy (* b avgx))
	      b
	      (if (> ssyy 0) (/ (* b ssxy) ssyy) 0)))))

(defun fac! (nr &optional (start 1))
  (loop for i from start to nr
        for product = i then (* product i)
        finally (return product)))

(defun permutations (a &optional (b a))
  "The number of permutations of length b out of a"
  (fac! a (1+ (- a b))))

(defun combination (a b)
  "The combination for b out of a"
  (/ (permutations a b) (fac! b)))

(defun count-subsets (n)
  (1+ (loop for i from 1 to n
	 sum (combination n i))))


(defun vector-magnitude (vector)
  (let ((magnitude 0))
    (dolist (el vector)
          (setf magnitude (+ magnitude (* el el))))
    (sqrt magnitude)))

(defun dot-product (vector1 vector2 &rest more-vectors)
  (loop with result = 0
     for i from 0
     for el1 in vector1
     for el2 in vector2
     for more-els = (mapcar #'(lambda (vec)
                                (nth i vec))
                            more-vectors)
     do
       (setf result (+ result
                       (if (car more-vectors)
                           (eval `(* ,el1 ,el2 ,@more-els))
                           (* el1 el2))))
     finally (return result)))

(export '(cosine-similarity multiply-list-of-vectors))

(defun cosine-similarity (vector1 vector2 &rest more-vectors)
  (let ((magnitudes nil))
    (setf magnitudes
          (loop for vector in `(,vector1 ,vector2 ,@more-vectors)
                collect (vector-magnitude vector)))
    (if (find 0 magnitudes :test #'equalp)
      0
      (min 1 (/ (dot-product vector1 vector2 more-vectors)
                (eval `(funcall #'* ,@magnitudes)))))))

(defun multiply-list-of-vectors (list-of-vectors)
  "Cross product of a list of vectors."
  (reduce #'multiply-two-vectors list-of-vectors))

(defun multiply-two-vectors (vector-1 vector-2)
  "Cross product of two vectors."
  (mapcar #'* vector-1 vector-2))

(defun rad-to-deg (rad)
  "Radians to degrees"
  (* 180.0 (/ rad pi)))

(defun deg-to-rad (deg)
  "Degrees to radians"
  (* pi (/ deg 180.0)))

(defun euclidean (a b)
  "Compute Euclidean distance between a and b"
  (let ((diff (mapcar #'- a b)))
    (float (sqrt (reduce #'+ (mapcar #'* diff diff))))))

(defun iota (max &key (min 0) (step 1))
  (loop for n from min below max by step
        collect n))


;;;
;;; stuff not used

;; (defun iota (n &optional (start-at 0))
;;   "Return a list of n consecutive integers, by default starting at 0."
;;   (if (<= n 0) nil (cons start-at (iota (- n 1) (+ start-at 1)))))
