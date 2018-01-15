;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package :cludg)

;; Special cludg handler

(cludg::define-descriptor-handler DEFINE-STRING-PURGER (form)
  "string purger"
  (setf (car form) 'cludg::define-string-purger)
  (values nil :restart (list (let ((*print-case* :upcase))
			       (macroexpand-1 form)))))

(cludg::define-descriptor-handler DEFINE-LAMBDA-LIST-PURGER (form)
  "lambda purger"
  (setf (car form) 'cludg::define-lambda-list-purger)
  (values nil :restart (list (let ((*print-case* :upcase))
			       (macroexpand-1 form)))))

;; Extract doc.

(cludg:extract-documentation 'cludg:html "../docu"
  (asdf:find-system :cldoc)
  ;;:filter #'cludg::default-filter
  :table-of-contents-title "Common Lisp Universal Documentation Generator")
