;;;;; (c) Tania Marques and Katrien Beuls
(in-package #:asdf)

(defsystem :portuguese-grammar
  :depends-on (:utils :monitors :fcg
               #+:hunchentoot-available-on-this-platform :web-interface)

  :serial t
  :components (;(:file "utils")
               (:file "extended-grammar")
               (:file "construction-templates")
               (:file "create-grammar")
             ;  (:file "unit-tests")
               ))  
            
