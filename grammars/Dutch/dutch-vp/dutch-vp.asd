
(in-package :asdf)

(defsystem :dutch-vp
  :depends-on (:experiment-framework :utils :monitors :fcg :network #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components 
  ((:file "package")
   (:file "robust-parsing")
   (:file "dutch-vp-grammar")))

