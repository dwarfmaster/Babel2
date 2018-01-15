;;; A web interface to the rule sets of an agent

(in-package :asdf)

(defsystem :web-interface
  :description "A web interface to the rule sets of an agent."
  :depends-on (:utils :hunchentoot :ht-simple-ajax :monitors :bordeaux-threads)
  :components 
  ((:file "package")
   (:file "render-xml" :depends-on ("package"))
   (:file "web-interface" :depends-on ("render-xml"))
   (:file "html-utils" :depends-on ("web-interface"))
   (:file "dot" :depends-on ("web-interface"))
   (:file "draw-predicate-network" :depends-on ("dot"))
   (:file "static-flash-network" :depends-on ("web-interface"))
   (:file "presentation" :depends-on ("static-flash-network"))
   (:file "3d-svg" :depends-on ("presentation"))
   (:file "class-diagram" :depends-on ("3d-svg"))))