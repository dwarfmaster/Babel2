;;; A web interface to the rule sets of an agent

(in-package :asdf)

(defsystem :server-interface
  :description "A server interface to Lisp."
  :depends-on (:utils :bordeaux-threads :cl-ppcre :usocket :cl-json)
  :components 
  ((:file "package")
   (:file "http-request")
   (:file "server-interface")
   (:file "server-lw")
   (:file "server-ccl")))



