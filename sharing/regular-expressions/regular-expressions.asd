
(in-package #:asdf)

(defsystem :regular-expressions
  :depends-on (:experiment-framework 
               :utils
               :monitors
               :plot-raw-data
               :fcg
               :irl
               :tasks-and-processes
               :meta-layer-learning
               :network
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components
  ((:file "web-interface")
   (:file "utilities")
   (:file "overwrite-functions")))