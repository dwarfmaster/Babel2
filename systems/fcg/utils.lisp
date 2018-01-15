(in-package :fcg)

(export '(load-demo-grammar))

(defun load-demo-grammar ()
  "This function loads the demo grammar (the linguist likes the mouse)
from the tutorial directory and returns the fcg construction set."
  (load (babel-pathname :directory '("systems" "fcg")
                        :name "demo-grammar"
                        :type "lisp"))
  (make-demo-grammar-cxns))
