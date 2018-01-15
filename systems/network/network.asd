(in-package :asdf)

(defsystem :network
  :description "Basic support for the datastructure network"
  :depends-on (:s-dot :test-framework :utils  :monitors
               #+:hunchentoot-available-on-this-platform :web-interface)
  :components 
  ((:file "package")
   (:file "network" :depends-on ("package"))
   (:file "drawing" :depends-on ("network"))
   #+:hunchentoot-available-on-this-platform 
   (:file "html" :depends-on ("network" "drawing"))
   
   ;; (:file "rule-network" :depends-on ("drawing" "network"))
   ;;(:file "rule-network-application" :depends-on '("rule-network"))
   ))


