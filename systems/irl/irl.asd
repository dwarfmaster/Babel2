
(in-package :asdf)

(defsystem :irl
  :description "Incremental Recruitment Language"
  :depends-on (:test-framework :utils :monitors
               #+:hunchentoot-available-on-this-platform :web-interface)
  :serial t
  :components 
  ((:file "package")
   (:file "entity")
   (:file "primitive")
   (:file "binding")
   (:file "evaluate-primitive")
   (:file "irl-programs")
   (:module chunk-composer
            :serial t
            :components 
            ((:file "chunk")
             (:file "match-chunk")
             (:file "evaluate-chunk")
             (:file "composer")
             (:file "single-topic-composer")
             (:file "multi-topic-composer")
             (:file "learning")))
   (:module monitoring
            :serial t
            :components 
            (#+:hunchentoot-available-on-this-platform (:file "draw-irl-program")
             #+:hunchentoot-available-on-this-platform (:file "html")
             #+:hunchentoot-available-on-this-platform (:file "web-monitors")))
   (:module tests
            :serial t
            :components 
            ((:file "apple-counting-example")
             (:file "test-evaluate-primitive")
             (:file "test-evaluate-irl-program")
             (:file "test-equivalent-irl-programs")
             (:file "test-irl-program-connected")
             (:file "test-expand-chunk")
             (:file "test-match-chunk")
             (:file "test-binding-helpers")))))




;;;;; This cleans the irl directory after it was compiled. Because asdf
;;;;; doesn't recompile files in module b when module a changed, this
;;;;; can help during development

;; (defmethod perform :after ((op load-op) (c (eql (asdf:find-system :irl))))
;;   (let ((*verbose-out* nil))
;;     (run-shell-command (format nil "cd ~a && ../../clean.sh" 
;;                                (component-pathname (asdf:find-system :irl))))))

