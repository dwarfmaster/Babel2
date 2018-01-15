
(in-package :common-lisp)

(defpackage :experiment-framework
  (:use :common-lisp :cl-user
   :test-framework
   :utils 
   :monitors
   #+:hunchentoot-available-on-this-platform :web-interface)
  (:documentation "experiments and agent"))
