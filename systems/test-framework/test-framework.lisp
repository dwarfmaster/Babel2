
(in-package :test-framework)

(export '(deftest 
          test-error test-condition test-ok test-assert test-equal test-equalp
          run-tests *raise-errors*))

(define-condition meta-error (error)
  ()
  (:documentation "This error is actually to check whether an
  other error triggered or not. So it's an error about errors."))


(defvar *deftest-running* nil)
(defvar *run-tests-running* nil)

(defvar *deftest-error-messages* nil)
(defvar *run-tests-error-messages* nil)

(defvar *raise-errors* nil
  "when t, then all errors in test functions are raised")

(defmacro error-occurred (expr message &rest msg-args)
  `(progn
     (princ "x")
     (let* ((*print-right-margin* 80)
            (*print-case* :downcase)
            (*print-pretty* t)
            (error-message (format nil "~%~%> ~a~%> ~a"
                                   ',expr (format nil ,message ,@msg-args))))
       (if *deftest-running*
           (push error-message *deftest-error-messages*)
           (princ error-message)))
     (force-output t)))

(defun no-error-occurred ()
  (princ ".")
  (force-output t))


(defmacro test-error (expression)
  "Use test-error for testing that indeed an error is thrown. When
   expression does not throw an error I throw an error and signal that
   no error was thrown from expression."
  `(handler-case (progn ,expression
			(error 'meta-error))
     (meta-error ()
       (error-occurred (test-error ,expression) "did not throw an error."))
     (error () 
       (no-error-occurred))))

(defmacro test-condition (expression)
  "Use test-condition for testing that indeed a condition is
   thrown. When expression does not throw a condition I throw an error
   and signal that no warning was thrown from expression."
  `(handler-case (progn ,expression
			(error 'meta-error))
     (meta-error ()
       (error-occurred (test-condition ,expression) "did not throw a condition."))
     (condition () 
       (no-error-occurred))))

(defmacro test-ok (expression)
  "Use test-ok to evaluate an expression that should not signal
   an error. If it does I catch it and report the error."
  `(restart-case 
       (handler-bind  ((error 
                        #'(lambda (x)
                            (unless *raise-errors*
                              (invoke-restart 'report-error x))))
                       (condition 
                        #'(lambda (x)
                            (unless *raise-errors*
                              (invoke-restart 'report-condition x)))))
         (progn ,expression
                (no-error-occurred)))
     (report-error (x)
       (error-occurred (test-ok ,expression) "threw an error:~%~(~:w~)" x))
     (report-condition (x)
       (error-occurred (test-ok ,expression) "threw a condition:~%~(~:w~)" x))))


(defmacro test-assert-aux (expression test-expression &optional msg &rest msg-args)
  `(restart-case 
       (handler-bind  ((error 
                        #'(lambda (x)
                            (unless *raise-errors*
                              (invoke-restart 'report-error x))))
                       (condition 
                        #'(lambda (x)
                            (unless *raise-errors*
                              (invoke-restart 'report-error x)))))
         (progn 
           (assert ,expression () ,(or msg "assertion failed") ,@msg-args)
           (no-error-occurred)))
     (report-error (x)
       (error-occurred ,test-expression "~a" x))))



(defmacro test-assert (expression &optional msg &rest msg-args)
  "Just asserts the expression but when the assert is false the
   error is caught and just reported."
  `(test-assert-aux ,expression (test-assert ,expression) ,msg ,@msg-args))


(defmacro test-equal (e1 e2 &optional msg &rest msg-args)
  `(test-assert-aux (equal ,e1 ,e2) (test-equal ,e1 ,e2) ,msg ,msg-args))

(defmacro test-equalp (e1 e2 &optional msg &rest msg-args)
  `(test-assert (equalp ,e1 ,e2) ,msg ,msg-args))

(defmacro test-permutation-p (e1 e2 &optional msg &rest msg-args)
  `(test-assert (permutation-of? ,e1 ,e2 :test #'equalp) 
		,msg ,msg-args))

(defvar *all-tests* nil)

(defmacro deftest (func-name parameters &body body)
  "Use this when creating a test instead of a regular defun. It
   is needed for correct output and will also catch any error that
   slips through and report it. For the user it just behaves as a
   defun."
  `(progn (pushnew ',func-name *all-tests*)
	  (defun ,func-name ,parameters
            (setf *deftest-error-messages* nil)
            (let ((*deftest-running* t))
              (format t "~%(~a::~(~a~)): "
                      (package-name (symbol-package ',func-name)) ',func-name)
              (restart-case 
                  (handler-bind ((error 
                                  #'(lambda (x)
                                      (unless *raise-errors*
                                        (invoke-restart 'report-error x))))
                                 (condition 
                                  #'(lambda (x)
                                      (unless *raise-errors*
                                        (invoke-restart 'report-condition x)))))
                    (progn ,@body))
                (report-error (x)
                  (error-occurred
                   (deftest ,func-name ,@parameters)
                   "threw an unexpected error:~%~(~:w~)" x))
                (report-condition (x)
                  (error-occurred
                   (deftest ,func-name ,@parameters)
                   "threw an unexpected condition:~%~(~:w~)" x))))
            (when *deftest-error-messages*
              (if *run-tests-running*
                  (push (cons ',func-name (reverse *deftest-error-messages*))
                        *run-tests-error-messages*)
                  (mapcar #'princ (reverse *deftest-error-messages*))))
            (not *deftest-error-messages*))))


(defgeneric run-tests (&optional package-name)
  (:documentation "Runs all tests in the specified package or all tests"))

(defmethod run-tests (&optional package-name)
  "Runs all defined tests"
  (setf *run-tests-error-messages* nil)
  (loop with *run-tests-running* = t
     for test in (reverse *all-tests*)
     when (or (not package-name)
              (eq (symbol-package test) (find-package package-name)))
     do (funcall test))
  (when *run-tests-error-messages*
    (format t "~%~%!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    (format t "~%!!!! ~a TESTS FAILED !" (loop for m in *run-tests-error-messages*
                                            sum (length (cdr m))))
    (format t "~%!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    (loop for m in *run-tests-error-messages*
       do (format t "~%~%!!!!! in ~a::~(~a~):" 
                  (package-name (symbol-package (car m))) (car m))
         (mapcar #'princ (cdr m)))
    (format t "~%~%!!!!! setting *raise-errors* to t might be helpful~%"))
  (not *run-tests-error-messages*))

(defvar *dont-run-tests-when-loading-asdf-systems* nil)

#+ecl
(require 'asdf)

(defmethod asdf:perform :after ((operation asdf:load-op) (system asdf:system))
  "after an asdf system is loaded automatically runs all tests that
   are in the package with the name of the asdf system"
  (unless *dont-run-tests-when-loading-asdf-systems*
    (run-tests (string-upcase (asdf:component-name system)))))
           


;; examples

;; (defun func-that-throws-an-error ()
;;   (error "foo bar"))
;; (defun func-that-throws-no-error ()
;;   t)

;; (deftest test-1 ()
;;   ;; these are good tests
;;   (test-assert (and (equal 2 (+ 1 1))
;; 		    (eql (* 2 5) 10)))
;;   (test-ok (find 1 '(1 2 3)))
;;   (test-error (func-that-throws-an-error))
;;   ;; the next ones go wrong, but indeed the code keeps running
;;   (test-assert (equal 1 2))
;;   (test-ok (func-that-throws-an-error))
;;   (test-error (func-that-throws-no-error))
;;   ;; and again some good tests
;;   (test-assert 1))

;; (deftest test-2 ()
;;   ;; even when an error is thrown outside a (test-... ) call we do not
;;   ;; crash but we cannot however simply continue.
;;   (test-ok (+ 1 1))
;;   (func-that-throws-an-error)
;;   (test-ok (+ 2 2)))
  

;; (progn
;;   (test-1)
;;   (test-2))

;; (run-tests :test-framework)
