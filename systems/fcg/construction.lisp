(in-package :fcg)

(export '(construction make-cxn name domain match-source attributes
          persistent-cxn cxn-with-original-variables renamings sem-syn
          sem syn full top-only static-hat dynamic-hat attr attr-val root cxn-inventory original-cxn-set))

(defclass construction (coupled-feature-structure)
  ((name 
    :type symbol :initarg :name :initform (make-id 'CXN) :accessor name 
    :documentation "A symbol for identifying a construction. Doesn't
                    need to be unique")
   (domain ;; can't we get this from the poles?
    :type symbol :initarg :domain :accessor domain
    :initform 'sem-syn
    :documentation "Whether to operate on both poles ('sem-syn) or only on the
                    'sem or 'syn side")
   (match-source
    :type symbol :accessor match-source :initform 'full :initarg :match-source
    :documentation "To which part of a transient structure to apply
                    to. Other possible values: 'top-only, 'static-hat,
                    'dynamic-hat")
   (attributes 
    :type list :initarg :attributes :initform nil :accessor attributes
    :documentation "A key . value alist for customizing the behavior
                    of the construction in processing")
   (persistent-cxn ;; use this for learning etc
    :type (or construction null) :initform nil :initarg :persistent-cxn 
    :accessor persistent-cxn
    :documentation "Points to the persistent version of the
    construction. This is either itself or the version it was copied
    from during application.")
   (renamings ;; private
    :type list :initform nil :initarg :renamings  :accessor renamings
    :documentation "The bindings used for renaming a construction.")
   (cxn-inventory
    :type (or construction-inventory null) :initform nil :initarg :cxn-inventory  :accessor cxn-inventory
    :documentation "A pointer back to the construction-inventory to which the cxn belongs"))
  (:documentation "Represents a FCG construction"))

;;; Public interface

(defmethod initialize-instance :after ((cxn construction) &key (rename-variables t)
                                       &allow-other-keys)
  (with-slots (domain) cxn
    (ecase domain
      (sem-syn t)
      (syn (setf (pole-domain (left-pole cxn)) 'syn))
      (sem (setf (pole-domain (right-pole cxn)) 'sem))))

  (when rename-variables
    ;; Rename all variables in order to ensure that no unintended
    ;; variable equalities between different constructions are
    ;; introduced, for example because two rules use the same
    ;; variable.
    (rename-variables cxn))
  cxn)

(defmethod initialize-instance :around ((cxn construction) &key
                                        &allow-other-keys)
  (let ((created-cxn (call-next-method)))
    (unless (persistent-cxn created-cxn)
      ;; we make sure the persistent-cxn slot is pointing to itself in
      ;; case it was not deliberatly set.
      (setf (persistent-cxn created-cxn) created-cxn))
    created-cxn))

(defun cxn-with-original-variables (cxn)
  "Uses the renamings slot to reconstruct the cxn as it was
defined. This is thus not the persistent-cxn and is more
expensive. Use this for visualization."
  (let ((cxn (persistent-cxn cxn))
        (reversed-renamings (loop for (first . rest) in (renamings cxn)
                               collect (cons rest first))))
    (make-instance 'construction
                   :name (name cxn)
                   :attributes (attributes cxn)
                   :left-pole (sublis reversed-renamings (left-pole-structure cxn))
                   :right-pole (sublis reversed-renamings (right-pole-structure cxn))
                   :rename-variables nil)))
#|
(defmacro make-cxn (name attributes left-pole <--> right-pole 
                    &key (domain 'sem-syn) (match-source 'full))
  ;; I think this is a backward compatibel convenience macro
  (declare (ignore <-->))
  `(make-instance 'construction
                  :name ',name
		  :left-pole ',left-pole 
		  :right-pole ',right-pole
		  :attributes ',(parse-attributes attributes) 
                  :domain ',domain
		  :match-source ',match-source))
|#

(defmethod copy-object ((construction construction))
  (let ((copy (make-instance 'construction
			     :name (name construction)
			     :domain (domain construction)
			     :match-source (match-source construction)
			     :left-pole (left-pole-structure construction)
			     :attributes (copy-list (attributes construction))
			     :right-pole (right-pole-structure construction)
                             :cxn-inventory (cxn-inventory construction)
			     )))
    (unless (eq (persistent-cxn construction) construction)
      (setf (persistent-cxn copy) (persistent-cxn construction)))
    copy))

(defmethod print-object ((cxn construction) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
        (format stream "<cxn ~(~a~):~:_ attrs: ~a,~:_ " (name cxn) (attributes cxn))
        (call-next-method)
        (format stream ">"))
      (format stream "<cxn: ~a>" (name cxn))))

(defun attr (cxn name)
  (assoc name (attributes cxn)))
(defun attr-val (cxn name)
  (cdr (attr cxn name)))
(defsetf attr-val (cxn name) (new-val)
  `(let ((prev (attr ,cxn ,name)))
     (if prev 
	 (setf (cdr prev) ,new-val)
	 (push (cons ,name ,new-val)
	       (attributes ,cxn)))
     ,new-val))



;; Private 

(defmethod rename-variables ((cxn construction) &optional renamings)
  (multiple-value-bind (new-left-pole-structure renamings)
      (rename-variables (pole-structure (left-pole cxn)) renamings)
    (setf (pole-structure (left-pole cxn)) new-left-pole-structure)
    (multiple-value-bind (new-right-pole-structure renamings)
	(rename-variables (pole-structure (right-pole cxn)) renamings)
      (setf (pole-structure (right-pole cxn)) new-right-pole-structure)
      (setf (renamings cxn) renamings)
      (values cxn renamings))))

(defun safe-cxn (cxn previous-cxns)
  "We need te rename variables in a construction when it has been
applied before."
  (if (find cxn previous-cxns) ;; if applied before -> rename variables
      ;; (let ((cxn (persistent-cxn cxn)))
;; 	(make-instance 'construction
;; 		       :name (name cxn)
;;                        :attributes (attributes cxn)
;; 		       :match-source (match-source cxn)
;; 		       :left-pole (copy-object (left-pole cxn))
;; 		       :right-pole (copy-object (right-pole cxn))
;; 		       :persistent-cxn cxn))
      (let ((safe (copy-object cxn)))
        (setf (persistent-cxn safe) (persistent-cxn cxn))
        safe)
      cxn))

(defun parse-attributes (attributes &optional parsed-attributes)
  "Converts attributes in a macro definition in the acceptable form."
  (unless (or (null attributes) (keywordp (first attributes)))
    (error "The attributes must start with a keyword.~%Required: '(:attr-1 0.5 :attr-2 5 ..)~%passed: ~:w" attributes))
  (if (null attributes)
    (reverse parsed-attributes)
    (let ((keyword (first attributes))
          (value nil)
          (new-attribute nil))
      ;; We now determine the value.
      (dolist (attr (rest attributes))
        (if (keywordp attr)
          (return (setf value (reverse value)))
          (push attr value)))
      ;; Now we know whether the value of the attribute is a single symbol or a list.
      (setf new-attribute (if (rest value) (cons keyword  (reverse value)) (cons keyword (first value))))
      ;; Recursively call the function.
      (parse-attributes (member-if #'keywordp (rest attributes))
                        (cons new-attribute parsed-attributes)))))
