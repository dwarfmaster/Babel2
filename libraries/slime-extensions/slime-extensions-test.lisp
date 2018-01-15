;;; -*-babel-*-
;;; Small tests to see if the candidates and documentation retrieval works...
;;; The lookups of uncompiled lisp symbols are limited to the loaded packages

(defparameter *testparam* "testparam" 
      "The test parameter")

(setf *testparam* "test")

(defun hello ()
  "A small test function with a little documentation."
 (print "hello again!"))

(defun test-fun (name)
  (print name))

(defmacro test-macro (name &key word)
"Macros can be documented..."
  (let
      ((unit-name (make-symbol (format nil "?test-~a" name))))
      `(progn
        (test-fun ,name)
        (print ,word))))

(test-macro "the-test"
            :word "this is the test!")

(defvar thetestvar "BLUBB"
"this is a small test for uncompiled lisp")