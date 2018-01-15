;;;; Miscellaneous utilities:

(in-package :utils)

(export '(babel-pathname))

;; ############################################################################
;; various:
;; ----------------------------------------------------------------------------

(export '(id let-values safe-setf))

;;; Provided here to be share in both in the FCG & CIS package package
;;; hierarchies.
;;;
(defgeneric id (obj)
  (:documentation "Return the id of the given object."))

(defmacro let-values (vars expr &body body)
  "Easier to remember/write replacement for multiple-value-bind."
  `(multiple-value-bind ,vars ,expr . ,body))

(defmacro safe-setf (variable value)
	"Only setfs the variable when it was nil."
  `(unless ,variable (setf ,variable ,value)))

;; ############################################################################
;; Various useful functions:
;; ----------------------------------------------------------------------------

(export '(bigger-list? first-atom last-atom cons-if alist-value last-elt >1
                       substitute-or-cons remove-if-member deep-member none))

(defun deep-member (elt lst &key (test #'eql))
  "Recursively check whether an element can be found in a list. Returns T or NIL."
  (cond ((atom lst) nil)
        ((funcall test elt (first lst)) t)
        (t
         (or (deep-member elt (first lst) :test test)
             (deep-member elt (rest lst) :test test)))))

;; None checks whether all of its arguments are NIL.
(defmacro none (&rest args)
  `(not (or ,@args)))

(defun bigger-list? (lst1 lst2)
  "More efficient function than comparing lengths of lists, capable of handling cons-lists."
  (cond
   ((null lst1) nil)
   ((null lst2) t)
   (t
    (bigger-list? (listify (rest lst1)) (listify (rest lst2))))))

(defun first-atom (lst)
  "Recursively fetch the first atom of a list."
  (let ((x (first lst)))
    (if (atom x)
      x
      (first-atom x))))

(defun last-atom (lst)
  "recursively fetch the last atom in a list."
  (let ((x (last lst)))
    (cond ((rest x) (last-atom x))
          ((atom (first x)) (first x))
          (t
           (last-atom (first x))))))

(defun cons-if (x lst)
  "If x, then cons x in lst."
  (if x (cons x lst) lst))

(defun alist-value (elt alist &key (key #'second))
  "Returns the second element of an alist if found."
  (funcall key (assoc elt alist)))

(defun last-elt (lst)
  "Fetch the last element of a list."
  (first (last lst)))

(defun >1 (lst)
  "Checks whether a list is bigger than 1."
  (rest lst))

(defun substitute-or-cons (new list &key (test #'equal) (key #'first))
  "Substitute an alist-element if one already exists, cons it if it does not."
  (let ((old (find (funcall key new) list :test test :key key)))
    (if old
      (substitute new old list :test test)
      (cons new list))))
;; (substitute-or-cons '(number sg) '((gender n)))
;; >> ((number sg) (gender n))
;; (substitute-or-cons '(number pl) '((number sg) (gender n)))
;; >> ((number pl) (gender n))

(defun remove-if-member (items sequence &key (test #'eql) (key #'identity))
  "Remove all elements that are member of the items."
  (loop for elt in sequence
        unless (member (funcall key elt) items :test test)
        collect elt))

;; ############################################################################
;; unify-equal definition:
;; ----------------------------------------------------------------------------

;; TODO: need at this low level because some functionality in CIS depends on it

(export 'unify-equal)

(defgeneric unify-equal (x y)
  (:documentation "Return t if x and y are equal when matched in unification."))

;;; Default implementation returns true if x and y are eq.
(defmethod unify-equal (x y)
  (eq x y))


;; ############################################################################
;; boolean utilities:
;; ----------------------------------------------------------------------------

(export '(xor))

(defun xor (&rest els)
  (= 1 (loop for el in els counting el)))


;; ############################################################################
;; pathname utilities:
;; ----------------------------------------------------------------------------

#-ecl
(export '(directory-pathname-p))

(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

#-ecl
(defun directory-pathname-p (pathspec)
  (and 
    (not (component-present-p (pathname-name pathspec)))
    (not (component-present-p (pathname-type pathspec)))
    pathspec))


;; ############################################################################
;; pretty-printing:
;; ----------------------------------------------------------------------------

(export '(pp pp-nil ppfc print-values class-string print-expansion))

(defgeneric class-string (object)
  (:documentation "Returns the class-name of the object as a string in
  lower case"))

(defmethod class-string ((object t))
  (string-downcase (symbol-name (class-name (class-of object)))))

(defmacro pp (&body body)
  "Name-value pretty-printer shorthand."
  (cond
    ((= 1 (length body))
     (let ((val (gensym)))
       (if (stringp (first body))
	   `(let ((,val ,(first body)))
	      (format t "~&- ~(~a~)~%" ,val)
	      ,val)
	   `(let ((,val ,(first body)))
	      (format t "~&- ~(~a~):~:[~;~% ~] ~:w~%"
                      ',(first body)
		      ,(> (length (format nil "~a" (first body))) 10) 
		      ,val)
	      ,val))))
    ((= 2 (length body))
     (if (eq t (first body))
	 `(format t "~&- ~(~a~)~%" ,(second body))
	 `(format t "~&- ~(~a~):~:[~;~% ~] ~:w~%"
                  ,(first body)
                  ,(> (length (format nil "~a" (first body))) 10)
                  ,(second body))))
    (t `(format t "~&- ~(~a~):~:[~;~% ~] ~:w~%"
                ,(first body)
                ,(> (length (format nil "~a" (first body))) 10)
                (list ,@(cdr body))))))

(defmacro pp-nil (&body body)
  "The same as PP but returns NIL."
  `(progn (pp ,@body)
     nil))

(defmacro ppfc (&body body)
  "Function call pretty-printer shorthand."
  `(progn
     ,(cond
       ((= 1 (length body))
	`(format t "~&>> ~(~a~)~%" ,(first body)))
       ((= 2 (length body))
	`(format t "~&>> ~(~a~): ~:w~%" ,(first body) ,(second body)))
       (t `(format t "~&>> ~(~a~): ~:w~%" ,(first body) (list ,@(cdr body)))))))

(defmacro print-values (&rest symbols)
  "for each symbol passed it prints a 'name: value' line."
  (cons 'progn (loop for s in symbols
                  collect `(format t "~%~a: ~a" (symbol-name ',s) ,s))))

(defmacro print-expansion (macro-call)
  "Pretty print the macro-expansion of a macro-call."
  `(pprint (macroexpand-1 ,macro-call)))


;; ############################################################################
;; compiled evaluation
;; ----------------------------------------------------------------------------

(export '(compiled-eval))

(defun compiled-eval (expression)
  "Like eval, but ensures that the expression is compiled before evaluation.
   Can be used in systems where eval performs interpretation instead of compilation.
   Warning: Compilation is not always better, especially not for one-shot evaluations.
   The reason is that execution speed is compilation + execution, not just execution.
   Only if compilation + execution < interpretation, it is a good idea to use compiled-eval."
  (funcall (compile nil `(lambda () ,expression))))

;; ############################################################################
;; quit the currently running lisp
;; ----------------------------------------------------------------------------

(export '(quit-lisp))

(defun quit-lisp ()
  #+:sbcl
  (sb-ext:quit)

  #+:ccl
  (ccl:quit)
  
  #+lispworks
  (cl-user::quit))

;; ############################################################################
;; Handy macros
;; ----------------------------------------------------------------------------

(export '(when-let when-let* while last-elt cond-let with-package))

(defmacro when-let* (vars-and-tests then-clause &optional else-clause)
  "If a value can be found for all vars, then execute the then-clause."
  `(let* ,vars-and-tests
     (if ,(cons 'and (mapcar #'first vars-and-tests))
       ,then-clause
       ,else-clause)))

(defmacro cond-let (&body body)
  (if (null body)
    nil
    `(let ,(list (caar body))
       (if ,(caaar body)
         (progn ,@(cdar body))
         (cond-let ,@(rest body))))))

(defmacro when-let (vars-and-tests then-clause &optional else-clause)
  "If a value can be found for all vars, then execute the then-clause."
  `(let ,vars-and-tests
     (if ,(cons 'and (mapcar #'first vars-and-tests))
       ,then-clause
       ,else-clause)))

(defmacro while (test &body body)
  "Easier to read than do macro."
  `(do ()
       ((not ,test))
     ,@body))

(defmacro with-package (package &body body)
  "evaluates an expression in a given package"
  `(let ((current-package (package-name ,*package*)))
     (eval `(in-package ,,package))
     (unwind-protect (progn ,@body)
       (eval `(in-package ,current-package)))))

; (defmacro with-working-directory (path &body body)
;   "evaluates the body from within a given directory"
;   `(progn
;      (let ((current-working-directory #+lispworks ,(hcl:get-working-directory)
;                                       #+clozure ,(ccl:current-directory)
;                                       #+sbcl ,(sb-unix:posix-getcwd)))
;        #+lispworks (hcl:change-directory ,path)
;        #+sbcl (sb-unix:chdir ,path)
;        (unwind-protect (progn ,@body)
;          #+lispworks (hcl:change-directory current-working-directory)
;          #+sbcl (sb-unix:chdir ,path)))))

