
(in-package #:asdf)

(defsystem :planning
  :description "All files for the planning package."
  :depends-on (:experiment-framework 
               :utils
               :monitors
               :fcg
               :irl
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components ((:file "package")
               (:file "monitors")
               (:file "planning")
               (:file "goal-extraction")
               (:file "plan-extraction")
               (:file "state-extraction")
               (:file "node-tests")
               (:file "goal-tests")))
