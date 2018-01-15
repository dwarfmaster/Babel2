
(in-package :asdf)

(defsystem :cl-json-shell
  :description "Functions extending :cl-json for use with shell"
  :depends-on (:utils :cl-json)
  :components 
  ((:file "package")
   (:file "cl-json-shell")))
