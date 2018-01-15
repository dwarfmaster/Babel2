;;; 
;;; File: monitors.asd
;;;
;;; General debugging and monitoring mechanisms for the babel framework
;;;

(in-package :asdf)

(defsystem :monitors
  :description "General debugging and monitoring mechanisms for the babel framework."
  :depends-on (:utils)
  :components 
  ((:file "package")
   (:file "base" :depends-on ("package"))
   (:file "monitors" :depends-on ("base"))
   (:file "trace-monitor" :depends-on ("monitors"))
   (:file "data-monitors" :depends-on ("monitors"))
   (:file "plot-monitors" :depends-on ("data-monitors"))
   (:file "alist-monitors" :depends-on ("plot-monitors"))
   (:file "3d-monitors" :depends-on ("alist-monitors"))
   ))

