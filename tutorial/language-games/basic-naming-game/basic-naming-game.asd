
(in-package #:asdf)

(defsystem :basic-naming-game
  :depends-on (:experiment-framework 
               :utils
               :monitors
               :plot-raw-data
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components 
   ((:file "naming-game")
    (:file "monitors")))
