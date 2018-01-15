;;;; Definition of the fcg-utils asdf system

(in-package :asdf)

(defsystem :utils
  :description "Basic utilities"
  :depends-on (:split-sequence :closer-mop :s-dot :cl-who :test-framework)
  :components 
  ((:file "package")
   (:file "pop" :depends-on ("package"))
   (:file "common-babel-symbols" :depends-on ("package"))
   (:file "owner" :depends-on ("common-babel-symbols"))
   (:file "copy-object" :depends-on ("owner"))
   (:file "math" :depends-on ("package"))
   (:file "lists-and-sets" :depends-on ("math" "package"))
   (:file "blackboard" :depends-on ("lists-and-sets" "copy-object" "package"))
   (:file "relation" :depends-on ("package" "copy-object" "lists-and-sets"
                                            "blackboard"))
   (:file "configuration" :depends-on ("package"))
   (:file "symbols-and-strings" :depends-on ("package"))
   (:file "make-new-word" :depends-on ("package"))
   (:file "tree" :depends-on ("package"))
   (:file "queue" :depends-on ("package"))
   (:file "misc-utils" :depends-on ("lists-and-sets" "symbols-and-strings"))
   (:file "shell" :depends-on ("package"))
   (:file "slist" :depends-on ("copy-object"))
   (:file "test-distribution" :depends-on ("package"))
   (:file "timer" :depends-on ("package"))
   (:file "types" :depends-on ("package"))
   (:file "write-to-latex" :depends-on ("package"))
   (:file "event-dispatcher")
   (:file "ssh-scp")
   (:module tests
    :depends-on ("lists-and-sets" "configuration" "tree" "relation")
    :components 
    ((:file "test-utils")
     (:file "test-relations")
     (:file "test-configuration")))))


