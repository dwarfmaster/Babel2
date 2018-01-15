#|
Copyright (C) 2007  David Owen <dsowen@fugue88.ws>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
|#

(in-package #:dso-util)



(defun positive (x) (> x 0))

(defun negative (x) (< x 0))

(defun range (n)
  (labels ((iter (i l)
             (if (>= i 0)
                 (iter (1- i) (cons i l))
                 l)))
    (iter (1- n) nil)))

(defun subvect (v start &optional (end 0))
  (when (< start 0) (setf start (+ (length v) start)))
  (when (<= end 0) (setf end (+ (length v) end)))
  (make-array (- end start) :displaced-to v :displaced-index-offset start))

(defun zip-plist (l1 l2)
  "Zips the two lists together as a plist.  The result is truncated to
match the shorter list.  If either list is NIL, evaluates to NIL."
  (mapcan (lambda (x y) (list x y)) l1 l2))

(defun zip-alist (l1 l2)
  "Zips the two lists together as a alist.  The result is truncated to
match the shorter list.  If either list is NIL, evaluates to NIL."
  (mapcar (lambda (x y) (cons x y)) l1 l2))

(defun zip-glist (&rest args)
  "Zips lists together as a grouped list."
  (apply #'mapcar #'list args))

(defmacro with-gensyms ((&rest symbols) &body body)
  `(let (,@(mapcar (lambda (sym) `(,sym (gensym ,(symbol-name sym)))) symbols))
    ,@body))



(defun compose (f g)
  (lambda (&rest args)
    (funcall f (apply g args))))

(defun peval (fn x)
  "Partially evaluates function FN over X."
  (lambda (&rest args)
    (apply fn x args)))

(defun inflist (obj)
  "Returns an infinite list containing OBJ."
  (let ((c (cons obj nil)))
    (setf (cdr c) c)
    c))

(defun bound (x a b)
  "Bounds X to within the interval [A, B].  If A > B, returns NIL."
  (unless (> a b)
    (min (max a x) b)))

(defmacro boundf (x a b)
  (with-gensyms (y)
    `(let ((,y (bound ,x ,a ,b)))
       (setf ,x ,y))))

(defun nothing (&rest args)
  (declare (ignore args)))

(defun true-fn (fn)
  (or (funcall fn) t))

(defun nil-fn (fn)
  (and (funcall fn) nil))



(defun string+ (&rest args)
  (let ((acc (make-string-output-stream)))
    (dolist (s args (get-output-stream-string acc))
      (write-string s acc))))

(defun string-initcase (str)
  (if (string= str "")
      str
      (string-upcase str :end 1)))



(defmacro only-if (form fn)
  (with-gensyms (x)
    `(let ((,x ,form))
       (when ,x (,fn ,x)))))
