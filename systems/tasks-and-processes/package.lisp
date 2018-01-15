
(in-package :common-lisp)

(defpackage :tasks-and-processes
  (:nicknames :tap2)
  (:use  :common-lisp :utils :monitors :test-framework
   #+:hunchentoot-available-on-this-platform :web-interface
   :meta-layer-learning)
  (:documentation "tasks and processes"))


