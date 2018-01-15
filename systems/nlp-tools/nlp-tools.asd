
(in-package :asdf)

(defsystem :nlp-tools
  :description "A system loading the NLP tools for Babel2"
  :depends-on (:utils :cl-json :cl-json-shell :dexador)
  :components 
  ((:file "package")
   (:file "penelope-interface")
   (:file "utils")))
