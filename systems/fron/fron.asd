(in-package :asdf)
; specifies what needs to be compiled 

(defsystem :fron
  :depends-on (:utils :web-interface)
  :serial t
  :components ((:file "package")
               (:file "fron")))
