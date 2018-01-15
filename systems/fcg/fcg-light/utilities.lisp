
(in-package :fcg)

(defmacro test-match (pattern source &key bindings type (fn 'unify))
  "Helper macro for manually testing matching."
  `(let ((the-bindings ,(cond ((and bindings (string= (symbol-name fn) "UNIFY"))
                               `(list ',bindings))
                              (bindings `(first (list ',bindings)))
                              (t `(list +no-bindings+)))))
     (cond
      ((or (variable-p ',pattern) (variable-p ',source))
       (,fn ',pattern ',source the-bindings))
      ((member ',type '(atom sequence) :test #'string= :key #'symbol-name)
       (,fn ',pattern ',source the-bindings))
      ((member ',type '(set set-of-predicates) :test #'string= :key #'symbol-name)
       (,fn (cons '== ',pattern) ',source the-bindings))
      (t
       (,fn (cons '==1 ',pattern) ',source the-bindings)))))

(defmacro test-merge (pattern source &key bindings type)
  "Helper macro for manually testing merging."
  `(test-match ,pattern ,source
               :bindings ,(or bindings +no-bindings+)
               :type ,type
               :fn fcg-merge))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions imported from root templates ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-initial-cxn-set (&key configurations init-cxns original-cxn-set)
  "For making an initial construction-set."
  (let ((cxn-set (if init-cxns
                   (if (listp init-cxns)
                     (loop with merged-cxn-set = (make-instance 'construction-set)
                           for cxns in init-cxns
                           do (setf (constructions merged-cxn-set)
                                    (append (constructions merged-cxn-set)
                                            (constructions
                                             (funcall (internal-symb 'make- cxns '-cxns)))))
                           finally (return merged-cxn-set))
                     (funcall (internal-symb 'make- init-cxns '-cxns)))
                   (make-default-cxn-set))))
    (set-configurations cxn-set configurations)
    (when original-cxn-set
      (setf (original-cxn-set cxn-set) original-cxn-set))
    cxn-set))

;;function used in the html-fcg-light.lisp
(defun get-properly-binding-name (binding)
  (let ((str-binding (replace-all (replace-all (write-to-string binding) "FCG::" "") "#:" "")))
  (subseq str-binding
          (if (search "?" str-binding)
            (search "?" str-binding)
            0)
          (search "-" str-binding :from-end t))))