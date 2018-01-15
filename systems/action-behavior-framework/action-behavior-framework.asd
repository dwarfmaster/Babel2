
(in-package :asdf)

(defsystem :action-behavior-framework
  :description "Basic definitions of agents and experiments"
  :depends-on (:test-framework
               :utils
               :monitors
               :meta-layer-learning
               :tasks-and-processes
               :experiment-framework
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t 
  :components 
  ((:file "package")
   (:file "interaction")
   (:file "action")
   (:file "world")
   (:file "agent")
   (:file "experiment")
   #+:hunchentoot-available-on-this-platform
   (:file "web-monitors")
   (:file "tests")))


