(in-package :asdf)

(pushnew :fcg *features*)

(defsystem :fcg
  :description "All files part of the implementation of Fluid
  Construction Grammar"
  :depends-on (:experiment-framework :test-framework :utils :monitors :meta-layer-learning :cl-store
               #+:hunchentoot-available-on-this-platform :web-interface
               :network
               :s-dot)
  :serial t
  :components 
  ((:file "package")
   (:file "utils")
   (:module unify-and-merge
    :serial t
    :components ((:file "matcher")
                 (:file "matcher-extensions")
                 (:file "expansion-operator")
                 (:file "structures")
                 (:file "match-structures")))
   (:file "construction")
   (:file "construction-application")
   (:module construction-inventories
    :serial t
    :components ((:file "construction-inventory")
                 (:file "construction-set")
                 (:file "construction-network")
                 (:file "hashed-construction-set")
                 (:file "construction-inventory-collection")
                 ))
   (:module parse-and-produce 
    :serial t
    :components ((:file "parse-and-produce")
                 (:file "create-initial-structure")
                 (:file "render")
                 (:file "de-render")))
   (:file "legacy-functions-fcg-2")
   (:module construction-inventory-processor
    :serial t
    :components ((:file "construction-inventory-processor")
                 (:file "cxn-suppliers")
                 (:file "node-tests")
                 (:file "goal-tests")))
   (:module monitoring
    :serial t
    :components ((:file "monitors")
                 #+:hunchentoot-available-on-this-platform 
                 (:file "html")
                 #+:hunchentoot-available-on-this-platform
                 (:file "web-monitors")
                   #+:hunchentoot-available-on-this-platform
                 (:file "visualisation-helpers")))
   (:module tests
    :serial t
    :components ((:file "helpers")
                 (:file "test-matcher-extensions")
                 (:file "test-render")
                 (:file "test-construction-application")
                 (:file "test-construction-inventory")
                 (:file "test-cip")
                 (:file "test-structures")
                 (:file "test-anti-unification")))
   (:module check-cxn
    :serial t
    :components ((:file "report")
                 (:file "check-cxn")
                 (:module monitoring
                  :serial t
                  :components ((:file "monitors")
                               #+:hunchentoot-available-on-this-platform
                               (:file "web-monitors")))))
   (:module fcg-light
    :serial t
    :components ((:file "utilities")
                 (:file "fcg-light-construction")
                 #+:hunchentoot-available-on-this-platform
                 (:file "html-fcg-light")
                 (:file "monitors-fcg-light")
                 (:file "fcg-light-to-fcg2")
                 (:file "fcg-light-to-latex")
		 (:file "processing-cxn-to-fcg-cxn")
                 (:file "tests")))
   (:module extras
    :serial t
    :components ((:file "cxn-graph-supplier")))
   (:module meta-layer
    :serial t
    :components ((:file "fcg-meta-layer-lib")))
   (:module anti-unification
    :serial t
    :components ((:file "utils")
                 (:module algorithms
                  :serial t
                  :components ((:file "basic-algorithm")
                               (:file "anti-unify-fcg")
                               (:file "anti-unify-fcg-specialise")
                               (:file "pro-unification")))
                 (:file "calculate-source-patterns")
                 (:file "anti-unification-cost")
                 (:file "robust-matching")))
   (:module evaluation
    :serial t
    :components ((:file "measures")
                 (:file "evaluate-grammar")
                 (:file "monitors")))
   (:module constructional-dependencies
    :serial t
    :components ((:file "application-dependencies")
                 (:file "data")
                 (:file "html")))))
