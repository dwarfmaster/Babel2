
(in-package :asdf)

(defsystem :guessing-game
  :depends-on (:utils
               :monitors
               :experiment-framework
               :tasks-and-processes
               :meta-layer-learning
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components 
  ((:file "guessing-game")
   (:file "monitors")
   (:file "tests")))