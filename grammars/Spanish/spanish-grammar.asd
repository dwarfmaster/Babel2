(in-package :asdf)

(defsystem :spanish-grammar
  :depends-on (:utils :fcg :monitors :xmls
               #+:hunchentoot-available-on-this-platform :web-interface)
  
  :serial t
  :components 
  (;(:file "package")
   (:file "constructions")
   (:file "lemmas-and-stems")
   (:file "morphology")
 
   ))


