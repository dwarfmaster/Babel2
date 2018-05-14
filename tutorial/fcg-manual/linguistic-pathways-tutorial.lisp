;; ####################################
;; # VISUALIZING LINGUISTIC PATHWAYS  #
;; ####################################
;; May 2017, started by Sebastien Hoorens
;; October 2017, updated by Katrien Beuls                         

;; This tutorial demonstrates the new FCG visualization that shows the
;; constructional dependencies that exist between constructions that
;; applied to process a single utterance/meaning. It is by default
;; activated in the FCG web interface and shown under the regular
;; search tree.

;; -------------------
;; # 1. Introduction #
;; -------------------
;; Evaluate the following line to load the FCG package and to start
;; the web-interface. Then, open a web browser (preferrably Safari or
;; Firefox) at http://localhost:8000.
(ql:quickload :fcg)

;; Now evaluate the following two lines to set the package right and
;; activate the trace-fcg monitor
(in-package :fcg)
(activate-monitor trace-fcg)

;; Then you can load the FCG demo grammar ("the linguist likes the mouse")
(load-demo-grammar)

;; By default the new visualization is shown in the web browser.
;; Try comprehending the following sentence:
(comprehend "the linguist likes the mouse")

;; Or formulating it:
(formulate '((FCG::UNIQUE #:X-5) (FCG::LINGUIST #:X-5) (FCG::UNIQUE #:Y-1) (FCG::MOUSE #:Y-1) (FCG::DEEP-AFFECTION #:X-5 #:Y-1)))

;; -------------------------------------------------------
;; # 2. Customizing the constructional dependencies view #
;; -------------------------------------------------------

;; To disable the constructional dependency view, evaluate the
;; following s-expression:
(set-configuration (visualization-configuration *fcg-constructions*)
                   :show-constructional-dependencies nil)


;; By default, the paths between the constructional units are
;; unlabeled. You can show the features that established the
;; dependency between the two constructions by setting :labeled-paths
;; to 'no-bindings or 'full:

(set-configuration (visualization-configuration *fcg-constructions*)
                   :labeled-paths 'no-bindings) ;;only features

;; Recomprehend the example utterance:
(comprehend "the linguist likes the mouse")

(set-configuration (visualization-configuration *fcg-constructions*)
                   :labeled-paths 'full) ;;features + bindings

;; Reproduce the example meaning:
(formulate '((FCG::UNIQUE #:X-5) (FCG::LINGUIST #:X-5) (FCG::UNIQUE #:Y-1) (FCG::MOUSE #:Y-1) (FCG::DEEP-AFFECTION #:X-5 #:Y-1)))

;; Paths between units can either be colored or grey:
(set-configuration (visualization-configuration *fcg-constructions*)
                   :colored-paths t)

;; One last time:
(formulate '((FCG::UNIQUE #:X-5) (FCG::LINGUIST #:X-5) (FCG::UNIQUE #:Y-1) (FCG::MOUSE #:Y-1) (FCG::DEEP-AFFECTION #:X-5 #:Y-1)))

;; -------------------------------------------------------
;; # 3. Directly visualizing the result as PDF file      #
;; -------------------------------------------------------

;; For comprehension:
(get-constructional-dependencies "the mouse"
                                 :cxn-inventory *fcg-constructions*)

;; For formulation:
(get-constructional-dependencies '((FCG::MOUSE #:X-4) (FCG::UNIQUE #:X-4))
                                 :cxn-inventory *fcg-constructions*)

;; You can also specify two keyword arguments to
;; configure the visualization
(get-constructional-dependencies "the mouse"
                                 :cxn-inventory *fcg-constructions*
                                 :labeled-paths 'full
                                 :colored-paths t)