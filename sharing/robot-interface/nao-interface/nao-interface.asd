(in-package :asdf)

(defsystem :nao-interface
  :description "A system implementing an interface between Babel2 and the Nao Robot."
  :depends-on (:test-framework
               :utils
               :cl-json
               :drakma)
  :serial t
  :components 
  ((:file "package")
   (:file "json-curl")
   (:file "nao")
   (:file "nao-movement")
   (:file "nao-headtouch")
   (:file "nao-speak")
   (:file "nao-vision")))
