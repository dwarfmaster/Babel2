
(in-package :cl-user)

(defpackage :fcg
  (:use :common-lisp :test-framework :utils :monitors :meta-layer-learning
   :experiment-framework ;;needed for data monitors
   #+:hunchentoot-available-on-this-platform :web-interface
   :network)
  (:shadow :equivalent-meaning? :prototype)
  (:documentation  "A package for the FCG system"))

(pushnew :fcg *features*)
