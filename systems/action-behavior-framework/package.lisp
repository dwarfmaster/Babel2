
(in-package :common-lisp)

(defpackage :action-behavior-framework
  (:use :common-lisp :test-framework :utils 
   :monitors :tasks-and-processes :meta-layer-learning
   #+:hunchentoot-available-on-this-platform :web-interface
   :experiment-framework)
  (:documentation "experiments and agent"))
