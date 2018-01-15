
(in-package :common-lisp)

(defpackage :nlp-tools
  (:use :common-lisp :cl-user :utils :cl-json :cl-json-shell)
  (:shadow "PROTOTYPE")
  (:documentation "This package collects the NLP tools included in Babel2. It mainly consists of an interface to tools running on the fcg-net.org server."))
