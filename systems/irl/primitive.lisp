
(in-package :irl)

;; ############################################################################
;; 'slot-spec' definition:
;; ----------------------------------------------------------------------------

(export '(slot-spec slot-spec-type))

;;; Parameters:
;;; - name: The name of the slot, which should be unique in the scope of
;;;         the primitive to which the slot belongs.
;;; - type: The type of the slot, a lisp typespec.
;;;
(defstruct slot-spec
  (name nil :type symbol :read-only t)
  (type nil :type t :read-only t))


(defmethod print-object ((slot-spec slot-spec) stream)
  (format stream "<slot-spec ~(~a~):~(~a~)>"
          (slot-spec-name slot-spec) (slot-spec-type slot-spec)))

;; ----------------------------------------------------------------------------

(defun check-slot-spec-defs (slot-spec-defs)
  "definition format checking"
  (assert (listp slot-spec-defs) ()
          "The slot-specs should be a list, got:~%  ~a" slot-spec-defs)
  (assert slot-spec-defs ()
          "The slot-specs should contain at least one element")
  (loop for slot-spec-def in slot-spec-defs
        do (assert (and (listp slot-spec-def) (= 2 (length slot-spec-def))
                        (symbolp (first slot-spec-def))
                        (symbolp (second slot-spec-def)))
                   () "Each slot-spec definition should be of form:~
                    ~%  (slot-name type),~%got:~%  ~a"
                   slot-spec-def)))


(defmacro make-slot-specs (slot-spec-defs)
  "Creates slot specs from a list of slot-spec definitions of the form:
   ((slot-name slot-type)*)"
  (check-slot-spec-defs slot-spec-defs)
  `(list . ,(loop
             for slot-spec-def in slot-spec-defs
             collect
              `(make-slot-spec :name ',(first slot-spec-def)
                               :type ',(second slot-spec-def)))))




;; ############################################################################
;; evaluation-spec definition:
;; ----------------------------------------------------------------------------

(defstruct (evaluation-spec)
  (pattern nil :type list) ;; the original pattern as defined in the primitive
  (bound-slot-names nil :type list) ;; a list of slot names for bound slots
  (unbound-slot-names nil :type list) ;; a list of slot names for unbound slots
  (function nil :type function)       ;; the body of the evaluation spec
  ;; List of booleans, one for each slot,  t if the corresponding slot is bound.
  (bound-slots-pattern nil :type list))




;; ----------------------------------------------------------------------------
;; definition format checking:

(defun check-bindings (bindings unbound-slot-names primitive pattern)
  (assert (listp bindings) ()
          "The bindings should be a list, got:~%  ~a" bindings)
  (loop for binding in bindings
        do (assert (and (listp binding) (= 3 (length binding))) ()
                   "Primitive ~a. Pattern ~a.~
                 ~%A binding should be a list of three elements, as in ~
                 (slot-name 0.8 binding-spec) got:~%  ~a"
                   primitive pattern binding)
        do (assert (member (car binding) unbound-slot-names) ()
                   "Primitive ~a. Pattern ~a.~
                 ~%The first element of the binding spec~%  ~a~
                 ~%should be the name of an unbound slot, i.e. one of~%  ~a."
                   primitive pattern binding unbound-slot-names))
  (let ((names (mapcar #'car bindings)))
    (assert (is-set names) ()
            "Primitive ~a. Pattern ~a.~
             ~%There seem to be two or more bindings for the same slot in~%  ~a."
            primitive pattern bindings)
    (assert (equal-sets names unbound-slot-names) ()
            "Primitive ~a. Pattern ~a.
             ~%The set of new bindings in~%  ~a~
             ~%does not seem to match the set of unbound slots, i.e.~%  ~a"
            primitive pattern bindings
            unbound-slot-names)))

(defun expand-evaluation-spec (primitive evaluation-spec-def slot-spec-defs)
  (destructuring-bind (pattern &body body) evaluation-spec-def
    (let (;; (weight (gensym))
          (slot-names (mapcar #'car slot-spec-defs))
          (slot-types (mapcar #'second slot-spec-defs))
          (bound-slot-names nil)
          (unbound-slot-names nil))
      (loop with bound-slot? = t
         for x in pattern
         do (cond 
              ((eq x '=>) (setf bound-slot? nil))
              (bound-slot? (push x bound-slot-names))
              (t (push x unbound-slot-names))))
      `(make-evaluation-spec
        :pattern ',pattern
        :bound-slot-names ',(reverse bound-slot-names)
        :unbound-slot-names ',(reverse unbound-slot-names)
        :bound-slots-pattern ',(loop for x in slot-spec-defs
                                  collect (if (find (car x) bound-slot-names) t nil))
        :function
        ,(if (= 0 (length unbound-slot-names))
           ;; if no unbound slots -> return what body returns
           `(lambda (ontology . ,slot-names)
              (declare (ignorable ontology . ,slot-names))
              ,@body)
           ;; if bound slots -> return weighted-value-set,
           ;; which is nil if bind has not been called
           (let ((weighted-value-sets (gensym)))
             `(macrolet ((bind (&rest bindings)
                           ;; push a list with an entry for each slot, holding
                           ;; the value-specs for the unbound one, and nil for
                           ;; the others on the value-sets list that is returned
                           (check-bindings bindings ',unbound-slot-names
                                           ',primitive ',pattern)
                           `(push
                             (list .
                                   ,(loop for slot-name in ',slot-names
                                          for binding = (assq slot-name bindings)
                                          collect
                                          `(list ,(second binding) ,(third binding))))
                             ,',weighted-value-sets)))
                ;; Here also all slot-names are given as formal parameters.
                ;; The concrete parameters for bound slots are the bindings
                ;; under consideration, whereas the concrete parameters for
                ;; unbound slots are the vars linked to those slots. These
                ;; could be used to inspect the new bindings resulting from
                ;; previously considered binding combinations.
                (lambda (ontology . ,slot-names)
                  (declare (ignorable ontology . ,slot-names))
                  (let ((,weighted-value-sets '()))
                    ,@body
                    ;; check the bindings
                    (loop for value-set in ,weighted-value-sets
                          do
                          (loop for (score value) in value-set
                                for type in ',slot-types
                                for name in ',slot-names
                                when (not (or (numberp score) (null score)))
                                do
                                (error "Primitive ~a. Pattern ~a.~
                                       ~%The provided score ~a for slot ~a is not of ~
                                         expected type number."
                                       ',primitive ',pattern
                                       score name)
                                when (and (find name ',unbound-slot-names)
                                          (not (typep value type)))
                                do (error "Primitive ~a. Pattern ~a.~
                                          ~%The bound value ~a for slot ~a is not of ~
                                          expected type ~a."
                                          ',primitive ',pattern
                                          value name type)))
                    ;; return the weighted-value-sets
                    ,weighted-value-sets)))))))))

(defun check-evaluation-spec-defs (evaluation-spec-defs slot-specs)
  (assert (listp evaluation-spec-defs) ()
          "The evaluation-spec-defs should be a list, got:~%  ~a" evaluation-spec-defs)
  (loop for evaluation-spec-def in evaluation-spec-defs
        for pattern = (progn 
                        (assert (listp evaluation-spec-def) ()
                                "A evaluation-spec-def should be a list, got:~%  ~a" evaluation-spec-def)
                        (car evaluation-spec-def))
     do (assert (listp pattern) ()
                "The pattern of a evaluation-spec-def should be a ~
                 list, got:~% ~a" (car evaluation-spec-def))
       (assert (find '=> pattern) ()
               "Couldn't find the '=> in pattern~% ~:w" pattern)
       (loop for x in pattern
          do (assert (symbolp x) ()
                     "Expected a slot name or '=> ,~%got ~:w" x)
            (assert (or (eq x '=>)
                        (find x slot-specs :key #'car)) ()
                        "Slot name ~a in pattern ~a~% should be one of ~a"
                        x pattern (mapcar #'car slot-specs))
            (assert (= 1 (length (find-all x pattern))) ()
                    "~a in pattern ~a occurs more than once" x pattern))
     do (loop for s in (mapcar #'car slot-specs)
           do (assert (find s pattern) ()
                      "Could not find slot ~a in pattern ~a" s pattern))))


;; ----------------------------------------------------------------------------
;; define-primitive macro expanders:

(defun expand-evaluation-specs (primitive evaluation-spec-defs slot-spec-defs)
  (check-evaluation-spec-defs evaluation-spec-defs slot-spec-defs)
  `(list . ,(loop for evaluation-spec-def in evaluation-spec-defs
                  collect (expand-evaluation-spec primitive
                                                  evaluation-spec-def
                                                  slot-spec-defs))))

;; ############################################################################
;; primitive definition:
;; ----------------------------------------------------------------------------
(export '(slot-specs))

(defclass primitive ()
  ((id
    :documentation "The identifier."
    :type symbol
    :initarg :id
    :initform (error "primitive requires :id")
    :reader id)
   (slot-specs
    :documentation "The list of <slot-spec> specified for the primitive.
                    Each of these specifies the details of one of the slots of
                    the primitive."
    :type list
    :initarg :slot-specs
    :initform (error "primitive requires :slot-specs")
    :reader slot-specs)
   (evaluation-specs
    :documentation "The evaluation-spec instances associated with this type."
    :type list
    :initarg :evaluation-specs
    :initform (error ":evaluation-specs are required")
    :reader evaluation-specs))
  (:documentation "Represents the 'type' of a primitive,
                   i.e. all the information that is passed to
                   defprimitive"))


(defmethod print-object ((type primitive) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
	(format stream "(~~primitive~~~:_ id: ~(~a~))" (id type)))
      (call-next-method)))

(defun primitive-p (obj)
  (typep obj 'primitive))


(defun slot-count (primitive)
  "Return the number of slots of the primitive."
  (declare (type primitive primitive))
  (length (slot-specs primitive)))





;; ############################################################################
;; defprimitive macro:
;; ----------------------------------------------------------------------------

(export '(defprimitive => bind primitive get-primitive ontology))

(defmacro defprimitive (id slot-spec-defs &body evaluation-spec-defs)
  "Creates a parameter for the primitive, which is instantiated as
   concrete primitives in irl programs."
  `(defparameter ,id
     (make-instance
      'primitive
      :id ',id
      :slot-specs (make-slot-specs ,slot-spec-defs)
      :evaluation-specs ,(expand-evaluation-specs id
                                                  evaluation-spec-defs
                                                  slot-spec-defs))))


(defun get-primitive (id &key surpress-errors)
  "Return the primitive bound to the given id. An
   error is thrown if this given symbol is unbound or if the bound
   value is not a primitive."
  (declare (type symbol id))
  (if (boundp id)
      (let ((obj (symbol-value id)))
	(if (primitive-p obj)
	    obj
            (if surpress-errors
              nil
              (error "The value of ~a is not a primitive." id))))
      (if surpress-errors
        nil
        (error "The symbol ~a is unbound." id))))

