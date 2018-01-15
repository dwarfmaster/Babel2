(in-package :fcg)

;;
;; This file contains functions that are normally NOT used anymore
;; and to which all references should be removed and adapted
;; (december 2016)
;; 
;;

;; This method is still used in a lot of tests, feel free to adapt the tests!!!

(defmethod de-render ((utterance list) (mode (eql :de-render-in-root-mode)) &key &allow-other-keys)
  "De-renders with a root-node and full form constraints."
  (warn "Deprecated Function: two-pole mode (as in FCG before 2015) is not supported anymore")
  (let ((strings nil)
        (sequence nil)
	(constraints nil))
    (dolist (string utterance)
      (let ((new (make-const string nil)))
        (push new sequence)
	(dolist (prev strings)
	  (push `(precedes ,(second prev) ,new) constraints))
	(push `(string ,new ,string) strings)))
    (do ((left strings (rest left)))
	((null (cdr left)))
      (push `(meets ,(second (second left)) ,(second (first left)))
	    constraints))
    (make-instance 'coupled-feature-structure 
		   :left-pole '((root (meaning ()) (sem-cat ())))
		   :right-pole `((root (form ,(cons (cons 'sequence (reverse sequence))
                                                    (append strings constraints)))
                                       (syn-cat ()))))))

(defun make-default-cxn-set ()
  "Build a default construction-set."
  (warn "The function make-default-cxn-set is deprecated and should not be used anymore! Moreover, it is still tailored towards the root-mode from before 2015. Instead, just make an instance of a construction-set and set its configurations")
  (let ((cxn-set (make-instance 'construction-set )))
    ;;; Make sure that the root unit is used instead of the top:
    (set-configuration cxn-set :create-initial-structure-mode :root-mode) 
    (set-configuration cxn-set :de-render-mode :de-render-in-root-mode) 
    (set-configuration cxn-set :render-mode :render-in-root-mode) 
    ;;; Use construction sets:
    (set-configuration cxn-set :cxn-supplier-mode :ordered-by-label) 
    cxn-set))
