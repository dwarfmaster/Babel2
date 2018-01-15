(in-package :asdf)

(defsystem :meta-layer-learning
  :description "Compiles all files needed to use meta-layer learning
  such as diagnostics and repairs"
  :depends-on (:test-framework :utils :monitors
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components 
  ((:file "package")
   (:file "notify-learning")
   (:file "diagnostic-problem-repair-fix")
   (:file "object-w-learning")
   (:file "events")
   (:file "monitors")
   #+:hunchentoot-available-on-this-platform
   (:file "html")
   (:file "test-meta-layer-learning")))


