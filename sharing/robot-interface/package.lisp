
(in-package #:common-lisp-user)

(defpackage :robot-interface
  (:use :common-lisp :test-framework :utils :cl-json :cl-json-shell :nao-interface)
  (:shadow "PROTOTYPE")
  (:documentation "Interface between Babel2 and physical robots."))
