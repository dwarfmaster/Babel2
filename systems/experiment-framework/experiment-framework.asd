
(in-package :asdf)

(defsystem :experiment-framework
  :description "Basic definitions of agents and experiments"
  :depends-on (:test-framework :utils :monitors
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t 
  :components 
  ((:file "package")
   (:file "experiment")
   (:file "agent")
   (:file "monitors")
   #+hunchentoot-available-on-this-platform
   (:file "web-monitors")
   #-ecl
   (:file "parallel-batch")
   (:file "tests")))


