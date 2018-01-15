(in-package :fcg)

(export '(construction-set trash original-cxn-set))
  
(defclass construction-set (construction-inventory)
  ((constructions 
    :type (or list null) :initform nil :initarg :constructions
    :documentation "A list of the constructions in the set")
   (trash
    :type (or list null) :initform nil :initarg :trash :accessor trash
    :documentation "A list of the constructions in the trash (containing non-active constructions)"))
  (:documentation "An organization of fcg constructions in a simple list"))

(define-configuration-default-value :shuffle-cxns-before-application nil)
(define-configuration-default-value :cxn-supplier-mode :simple-queue)

(defmethod clear ((construction-set construction-set) &key (include-trash t) &allow-other-keys)
  (setf (constructions construction-set) nil)
  (when include-trash
    (setf (trash construction-set) nil)))

(defmethod constructions ((construction-set construction-set) &key &allow-other-keys)
  (slot-value construction-set 'constructions))

(defmethod (setf constructions) ((construction-list list)
                                 (construction-set construction-set))
  (setf (slot-value construction-set 'constructions) construction-list))

(defmethod find-cxn ((construction construction) (constructions list) 
                     &key (key #'name) (test #'eql))
  (find (funcall key construction) constructions
        :key key :test test))

(defmethod find-cxn ((construction construction) (construction-set construction-set) 
                     &key (key #'name) (test #'eql) (search-trash nil))
  (find-cxn construction (append (constructions construction-set)
                                 (when search-trash (trash construction-set)))
            :key key :test test))

(defmethod find-cxn ((construction t) (construction-set construction-set) 
                     &key (key #'name) (test #'eql) (search-trash nil))
  (find construction (append (constructions construction-set)
                             (when search-trash (trash construction-set))) 
        :key key :test test))

(defmethod add-cxn ((construction construction) (construction-set construction-set)
                    &key (recover-from-trash nil) (equivalent-key #'name) 
                    (equivalent-test #'eql) &allow-other-keys)
  (if recover-from-trash
      (let ((trashed-cxn (find (funcall equivalent-key construction) (trash construction-set) 
                               :key equivalent-key
                               :test equivalent-test)))
        (push trashed-cxn (constructions construction-set))
        (setf (trash construction-set) (remove trashed-cxn (trash construction-set))))
      (push construction (constructions construction-set)))
  (values construction-set construction))

;; remark that the key is also called on the construction, which is
;; non-standard lisp behaviour.
(defmethod delete-cxn ((construction construction) 
                       (construction-set construction-set)
                       &key (key #'identity) (test #'eql) (move-to-trash nil))
  (let ((to-delete (find-cxn construction construction-set :test test :key key)))
    (when to-delete
      (setf (constructions construction-set)
            (remove to-delete (constructions construction-set)))
      (when move-to-trash
        (push to-delete (trash construction-set)))
      to-delete)))


(defmethod copy-object-content ((source construction-set)
                                (destination construction-set))
  (setf (constructions destination) (copy-list (constructions source)))
  (setf (trash destination) (copy-list (trash source)))) ;;no deep copy





