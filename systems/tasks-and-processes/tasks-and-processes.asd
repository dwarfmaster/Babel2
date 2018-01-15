
(in-package :asdf)

(defsystem :tasks-and-processes
  :description "tasks and processes"
  :depends-on (:utils :monitors
                      #+:hunchentoot-available-on-this-platform :web-interface
                      :meta-layer-learning)
  :serial t
  :components 
  ((:file "package")
   (:file "task")
   (:file "learning")
   (:file "object-w-tasks")
   #+:hunchentoot-available-on-this-platform
   (:file "html")
   (:file "tests")
   (:file "tests-learning")))

