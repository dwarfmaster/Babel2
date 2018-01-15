
(in-package #:common-lisp-user)

(defpackage :nao-interface
  (:use :common-lisp
        :test-framework
        :utils
        :cl-json
        :cl-json-shell)
  (:shadow "PROTOTYPE")
  (:export :start-nao-server :stop-nao-server :test-server-connection)
  (:documentation "Interface between Babel2 and the Nao Robot"))
