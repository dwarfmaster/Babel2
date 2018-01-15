(in-package :utils)

;; ############################################################################
;; role type
;; ----------------------------------------------------------------------------

(export '(role eq-role))

;;; Modify this type if new datatypes are acceptable as roles. Also consider
;;; providing an specialized implementation of eq-role in this case.
(deftype role () 'symbol)

(defgeneric eq-role (role-1 role-2)
  (:documentation "Role equality function, can be specialized for special roles."))

(defmethod eq-role (role-1 role-2)
  (equal role-1 role-2))

;; ############################################################################
;; relatable ADT
;; ----------------------------------------------------------------------------
;; There is both a base relatable class and a base relatable struct definition
;; for respectively classes to inherit from and structs to include.

(export '(relatable
          relatable-class
          relatable-struct
          relaters
          relatablep))

(defclass relatable-class ()
  ((relaters
    :documentation "The associative list ((role . relation*)*) that contains
      the relations that relate this relatable, organized by the respective
      roles the relatables assume in the relations."
    :initform '()
    :reader relaters)
   (copy
    :documentation "When a relatable is copied using copy-object, then the copy
     is temporarily assigned to this slot. This copy can then be used to
     re-link the copy of the relations in the second pass of the copying."
    :type (or null relatable)
    :initform nil))
  (:documentation "Mixin super-class for classes of which instances can
     participate in relations."))

(defstruct relatable-struct
  (relaters nil :type list)
  (copy nil :type (or null relatable-struct)))

(deftype relatable ()
  '(or relatable-class relatable-struct))

;; ----------------------------------------------------------------------------

(defun relatablep (obj) (typep obj 'relatable))

(defmethod print-object ((relatable relatable-class) stream)
  (print-relatable relatable stream))

(defmethod print-object ((relatable relatable-struct) stream)
  (print-relatable relatable stream))

(defun print-relatable (relatable stream)
  (declare (type relatable relatable)
           (type stream stream)
           (ignore relatable))
  (format stream "<relatable>"))

(defmethod relaters ((relatable relatable-struct))
  (relatable-struct-relaters relatable))


;; ############################################################################
;; relation ADT
;; ----------------------------------------------------------------------------
;; There is both a base relation class and a base relation struct definition
;; for respectively classes to inherit from and structs to include.
;; The make-relation utility creates structs as these are more efficiently
;; handled.

(export '(relation
          relation-class
          relation-struct
          relatees
          relationp
          make-relation))

(defclass relation-class (relatable-class)
  ((relatees
    :documentation "The associative list ((role . relatable)*) that contains
      the relatees related by this relation, organized by their roles."
    :type list
    :initarg :relatees
    :initform '()
    :reader relatees))
  (:documentation "A relation relates one or more relatables, each one under
     a unique role.
     Example instantiation:
     (make-instance 'relation
                    :relatees `((parent ,a-parent)
                                (child ,a-child))"))

;;; Example use:
;;; - (make-relation `((role-1 . ,obj-1) (role-2 . ,obj-2)))
;;;
(defstruct (relation-struct (:include relatable-struct)
                            (:constructor make-relation (relatees)))
  (relatees nil :type list))

(deftype relation ()
  '(or relation-class relation-struct))

;; ----------------------------------------------------------------------------

(defmethod initialize-instance :after ((relation relation-class)
				       &key relatees &allow-other-keys)
  (loop for (role . relatable) in relatees
	do (assert (relatablep relatable) () "~a is not a relatable" relatable)
        (add-relater relatable role relation)))

(defmethod print-object ((relation relation-class) stream)
  (print-relation relation stream))

(defmethod print-object ((relation relation-struct) stream)
  (print-relation relation stream))

(defun print-relation (relation stream)
  (declare (type relation relation)
           (type stream stream))
  (with-slots (relatees) relation
    (pprint-logical-block (stream nil)
      (format stream "<relation")
      (loop for (role . relatee) in relatees
            do (format stream "~:_ ~(~a~): ~a" role relatee))
      (format stream ">"))))

(defmethod relatees ((relation relation-struct))
  (relation-struct-relatees relation))


;; ############################################################################
;; public interface:

;; ============================================================================
;; relatee accessors:

(export '(find-relatee get-relatee))

(defun find-relatee (relation role)
  "Returns the relatee for the given role."
  (declare (type relation relation)
           (type role role))
  (with-slots (relatees) relation
    (let ((entry (assoc role relatees :test #'eq-role)))
      (if entry (cdr entry)))))

(defun get-relatee (relation role)
  (declare (type relation relation)
           (type role role))
  (or (find-relatee relation role)
      (error "Could not find a relatee for the given role:~
              ~%- relation: ~:w~
              ~%- role: ~:w"
	     relation role)))

;; ----------------------------------------------------------------------------

(defun add-relatee (relation role relatable)
  "Add a relatable under the given role."
  (declare (type relation relation)
           (type relatable relatable))
  (with-slots (relatees) relation
    (assert (not (assoc role relatees :test #'eq-role)))
    (add-relater relatable role relation)
    (pushend (cons role relatable) relatees)))

;; ----------------------------------------------------------------------------

(defun remove-relatee (relation relatable)
  "Remove the given relatable from the given relation."
  (declare (type relation relation)
           (type relatable relatable))
  (with-slots (relatees) relation
    (let ((entry (find relatable relatees :key #'cdr)))
      (when entry
        (remove-relater relatable relation :role (car entry))
        (setf relatees (remove entry relatees))))))

(defun remove-relatee-by-role (relation role)
  "Remove a relatee from the given relation, where the to be removed
   relatable participates in the relation under the given role."
  (declare (type relation relation)
           (type role role))
  (with-slots (relatees) relation
    (let ((entry (assoc role relatees :test #'eq-role)))
      (when entry
        (remove-relater (cdr entry) relation :role role)
        (setf relatees (remove entry relatees))))))
  

;; ============================================================================
;; unrelating a relation or relatable:

(export 'unrelate)

(defgeneric unrelate (obj))

(defun unrelate-relation (relation)
  (declare (type relation relation))
  (with-slots (relatees) relation
    (loop for entry in relatees
          do (remove-relater (cdr entry) relation))))

(defmethod unrelate ((relation relation-class))
  (unrelate-relation relation))

(defmethod unrelate ((relation relation-struct))
  (unrelate-relation relation))

(defun unrelate-relatable (relatable)
  (declare (type relatable relatable))
  (with-slots (relaters) relatable
    (loop for (role . relations) in relaters
          do (loop for relation in relations
                do (remove-relatee-by-role relation role)))
    (assert (null relaters))))

(defmethod unrelate ((relatable relatable-class))
  (unrelate-relatable relatable))

(defmethod unrelate ((relatable relatable-struct))
  (unrelate-relatable relatable))

;; ============================================================================
;; relater accessors:

(export '(get-relaters))

(defun get-relaters (relatable role &optional typespec)
  "Return the relations that relate the given relatable. If a role is given
   then only those relations that relate this relatable under the given role
   are returned. If a typespec is given than only those relations that are of
   that given type are returned. A role and a typespec can be combined."
  (declare (type relatable relatable)
           (type (or null role) role)
	   (type (or null class symbol) typespec))
  (with-slots (relaters) relatable
    (let ((relations (if role
                         (loop for (rrole . relations) in relaters
                               when (eq-role rrole role)
                               append relations)
                         (mapcan #'cdr relaters))))
      (if typespec
          (loop for relation in relations
                when (typep relation typespec)
                collect relation)
          relations))))


;; ############################################################################
;; system functionality:

;; ----------------------------------------------------------------------------
;; Adding and removing relations to/from relatables. These should not be called
;; directly but can be extended with custom functionality.

(export '(add-relater remove-relater))

(defun add-relater (relatable role relation)
  "Add the given relation to the collection of relations that
  relate this relatable. Warning: this method should typically not be called
  by users. Users should typically call add-relatee, which calls add-relater."
  (declare (type relatable relatable)
           (type role role)
           (type relation relation))
  (with-slots (relaters) relatable
    (let ((entry (assoc role relaters :test #'eq-role)))
      (if entry
          (pushnew relation (cdr entry))
          (push (list role relation) relaters)))))

(defun remove-relater (relatable relation &key role)
  "Removes the given relation from the collection of relations
   that relate this relatable. A role can be given when the relatable is
   related multiple times by the same relation under different roles."
  (declare (type relatable relatable)
           (type relation relation)
           (type (or null role) role))
  (with-slots (relaters) relatable
    (labels ((rec-remove (rest)
               (cond ((null rest) '())
                     ((member relation (cdar rest))
                      (if (and role (not (eq-role role (caar rest))))
                          (cons (car rest) (rec-remove (cdr rest)))
                          (let ((relations (remove relation (cdar rest))))
                            (if relations
                                (cons (cons (caar rest) relations) (cdr rest))
                                (cdr rest)))))
                     (t (cons (car rest) (rec-remove (cdr rest)))))))
      (setf relaters (rec-remove relaters)))))


;; ############################################################################
;; three-pass relation copying functionality:
;; ----------------------------------------------------------------------------
;;
;; About copy-object and relations:
;;
;; Originally the copying simply copied all objects in one pass. Before the
;; arrival of relations, objects were organized in a hierarchical manner.
;; The copying could thus be simply performed by starting at the root object
;; and recursively copying the child objects.
;; With the arrival of relations, the situation becomes a bit more complicated.
;; When related objects are copied, then the relations should also be copied,
;; but before a relations can be properly copied, including having it point
;; to the copies of the relatees, these relatee copies should be available
;; before copying the relation. Given that the model effectively becomes a
;; network with the introduction of relations, it is no longer trivial to
;; determine the order in which the entities (both relatees and relations) in
;; the model should be copied. In case of a cyclic relations network, it is
;; even impossible.
;; Therefore a three-pass copying was introduced.
;; 1) The first pass copies all objects and relations.
;;    The relations are however not yet re-linked to the copies
;;    of their relatees. Furthermore all relatables that are effectively
;;    related keep a pointer to their copy.
;; 2) The second pass is performed after all objects have been copied. In this
;;    second pass the relations are relinked to the then avaible copies of all
;;    concerned relatees.
;; 3) The third pass then removes the pointers to the copies.
;;
;; The three-pass copying is implemented by adding partially copied relations
;; to the *partially-copied-relations* parameter. After all objects have been
;; copied, all relations in this list are treated with the second and third
;; pass.
;;
;; The final issue to deal with is to discern when all objects have been copied,
;; or in other words, when it the appropriate time to start the second pass.
;; In anticipation of a better solution, for now several functions in which
;; the copying process is 'started', the full-copy-object function
;; is called. So if you implement a new point when copying is started, you
;; should call this function.

;;; The list of relations that need to be further treated when all relatables
;;; have been copied.
(defparameter *partially-copied-relations* '())

;; ============================================================================
;; copy-object-content implementations:

(defun copy-relatable-content (source target)
  (declare (type relatable source target))
  ;;(ppfc 'copy-relatable-content source)
  (with-slots ((source-relaters relaters)) source
    (with-slots ((target-relaters relaters)) target
      ;; copy both the roles and the relations that relate this :
      (setf target-relaters
            (loop for (role . relations) in source-relaters
                  collect (cons (copy-object role)
                                (loop for relation in relations
                                      ;; copy the relation or take the already
                                      ;; existing copy:
                                      collect (or (slot-value relation 'copy)
                                                  (copy-object relation))))))
      (setf (slot-value source 'copy) target))))

(defmethod copy-object-content ((source relatable-class) (target relatable-class))
  (copy-relatable-content source target))

(defmethod copy-object-content ((source relatable-struct) (target relatable-struct))
  (copy-relatable-content source target))

;; ----------------------------------------------------------------------------

(defun copy-relation-content (source target)
  (declare (type relation source target))
  ;;(ppfc 'copy-relation-content source)
  (unless (slot-value source 'copy)
    (with-slots ((source-relatees relatees)) source
      (with-slots ((target-relatees relatees)) target
        (setf target-relatees
              (loop for (role . relatee) in source-relatees
                    ;; copy the role, but defer the copying of the relatee till
                    ;; the second copy-pass:
                    do (assert relatee)
                    collect (cons (copy-object role) relatee)))))
    (setf (slot-value source 'copy) target)
    (push source *partially-copied-relations*)))

(defmethod copy-object-content ((source relation-class) (target relation-class))
  (copy-relation-content source target))

(defmethod copy-object-content ((source relation-struct) (target relation-struct))
  (copy-relation-content source target))

;; ============================================================================
;; the helper functions function:

(export '(full-copy-object))

(defun full-copy-object (obj)
  "Initiates three-pass copy from obj down, and returns the copy."
  (let (copy)
    ;; first pass:
    (setq copy (copy-object obj))
    ;; second pass:
    (loop for relation in *partially-copied-relations*
          do (setf (slot-value (slot-value relation 'copy) 'relatees)
                   (loop for (role . relatee) in
                        (slot-value (slot-value relation 'copy) 'relatees)
                        do (assert relatee)
                        (assert (slot-value relatee 'copy))
                        collect (cons role (slot-value relatee 'copy)))))
    ;; third pass:
    (loop for relation in *partially-copied-relations*
          do (loop for entry in (slot-value relation 'relatees)
                  do (setf (slot-value (cdr entry) 'copy) nil))
          do (setf (slot-value relation 'copy) nil))
    (setf *partially-copied-relations* nil)
    ;; return the copy:
    copy))


;; ############################################################################
