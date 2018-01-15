
(in-package :irl)

;; ############################################################################
;; entity
;; ----------------------------------------------------------------------------

(export '(entity id equal-entity))

(defclass entity ()
  ((id :type symbol :initarg :id :accessor id
       :documentation "A symbol that should be unique within an ontology."))
  (:documentation "Everything that can be bound to a slot of a
                   primitive is an entity"))

(defmethod initialize-instance :around ((entity entity) &rest initargs &key id)
  "when :id was not passed, make a new one"
  (apply #'call-next-method entity :id (or id (make-id (type-of entity))) initargs))

(defmethod print-object ((entity entity) stream)
  (format stream "<~(~a~) ~(~a~)>" (type-of entity) (id entity)))

(defun make-entity (id)
  (make-instance 'entity :id id))

(defgeneric equal-entity (value-1 value-2)
  (:documentation "Return true if the given entities are equal. This function is
      called while revising primitives in order to detect isomorphic values
      for re-use, which leads to increased efficiency."))

(defmethod equal-entity (entity-1 entity-2)
  (declare (ignorable entity-1 entity-2))
  (eq entity-1 entity-2))

(defmethod equal-entity ((entity-1 entity) (entity-2 entity))
  (eq (id entity-1) (id entity-2)))

  

;; ############################################################################
;; find-entity-by-id
;; ----------------------------------------------------------------------------

(export 'find-entity-by-id)


(defgeneric find-entity-by-id (thing id)
  (:documentation "Finds an entity in thing by its id"))

(defmethod find-entity-by-id ((thing t) (id symbol))
  nil)

(defmethod find-entity-by-id ((entity entity) (id symbol))
  (when (eq (id entity) id)
    entity))

(defmethod find-entity-by-id ((blackboard blackboard) (id symbol))
  (loop for field in (data-fields blackboard)
     thereis (find-entity-by-id (cdr field) id)))

(defmethod find-entity-by-id ((cons cons) (id symbol))
  (if (and (typep (car cons) 'entity) (eq (id (car cons)) id))
    (car cons)
    (or (find-entity-by-id (car cons) id)
        (find-entity-by-id (cdr cons) id))))
