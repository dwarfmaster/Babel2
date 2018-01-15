
(in-package :common-lisp-user)

(defpackage :meta-layer-learning
  (:nicknames :learning)
  (:use :common-lisp 
        :test-framework 
        :utils
        :monitors
        #+:hunchentoot-available-on-this-platform :web-interface)
  (:documentation "Support for meta-layer learning such as diagnostics
  and repairs."))