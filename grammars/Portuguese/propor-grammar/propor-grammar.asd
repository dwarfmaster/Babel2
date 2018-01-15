;;;;; (c) Tania Marques and Katrien Beuls
(in-package #:asdf)

(defsystem :propor-grammar
  :depends-on (:utils :monitors :fcg :portuguese-grammar
               #+:hunchentoot-available-on-this-platform :web-interface)

  :serial t
  :components ((:file "base-grammar")
               (:file "lexicon")
               (:file "test-sentences")))
            
