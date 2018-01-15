(in-package :asdf)

(defsystem :robot-interface
  :description "A system implementing an interface between Babel2 and physical robots."
  :depends-on (:test-framework :utils :cl-json :cl-json-shell :nao-interface)
  :serial t
  :components 
  ((:file "package")
   (:file "setup")
   (:file "robot-connection")
   (:file "movement")
   (:file "speech")
   (:file "vision")
   (:file "headtouch")))
