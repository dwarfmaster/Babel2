
(in-package :meta-layer-learning)

;; ###########################################################################
;; object-w-learning
;; ---------------------------------------------------------------------------

(export '(object-w-learning diagnostics problems repairs
                            open-problem? notify-learning restart-object))

(defclass object-w-learning ()
  ((diagnostics
    :documentation "A list of all diagnostics."
    :type list
    :accessor diagnostics
    :initarg :diagnostics
    :initform nil)
   (problems
    :documentation "All the problems that were reported by diagnostics."
    :type list 
    :initform nil 
    :initarg :problems 
    :accessor problems)
   (repairs
    :documentation "A list of repairs."
    :type list
    :accessor repairs
    :initarg :repairs
    :initform nil))
  (:documentation "A class that provided with
  diagnostics, repairs and problems"))

;; ---------------------------------------------------------------------------

(defmethod get-diagnostics ((object object-w-learning) &key &allow-other-keys)
  (diagnostics object))

;; ---------------------------------------------------------------------------

(defmethod get-repairs ((object object-w-learning) &key &allow-other-keys)
  (repairs object))

;; ---------------------------------------------------------------------------

(defmethod add-problem ((object object-w-learning) 
                        problem
                        &key &allow-other-keys)
  (push problem (problems object)))

;; ---------------------------------------------------------------------------

(defmethod get-problems ((object object-w-learning) &key &allow-other-keys)
  (problems object))

;; ###########################################################################
;; some more methods for add/get/delete repairs
;; ---------------------------------------------------------------------------

(export '(add-diagnostic delete-diagnostic add-repair delete-repair))

;; ---------------------------------------------------------------------------

(defgeneric add-diagnostic (object diagnostic &key)
  (:documentation "Add diagnostics to an object."))
  
(defmethod add-diagnostic ((object object-w-learning)
			   (diagnostic diagnostic)
                           &key (key #'identity) (test #'eq))
  (unless (member diagnostic (diagnostics object)
                  :key key :test test)
    (push diagnostic (diagnostics object))))

(defmethod add-diagnostic ((object object-w-learning)
			   (diagnostic-class symbol)
                           &key (key #'identity) (test #'eq))
  (add-diagnostic object (make-instance diagnostic-class)
                  :key key :test test))

;; ---------------------------------------------------------------------------

(defgeneric delete-diagnostic (object diagnostic &key)
  (:documentation "Deletes a diagnostic from an object"))

(defmethod delete-diagnostic ((object object-w-learning)
			      diagnostic
                              &key (key #'identity) (test #'eq))
  (setf (diagnostics object) (remove diagnostic (diagnostics object)
                                     :key key :test test)))


;; ---------------------------------------------------------------------------

(defgeneric add-repair (object repair &key)
  (:documentation "Adds a repair strategy to an object."))

(defmethod add-repair ((object object-w-learning) 
                       (repair repair)
                       &key (key #'identity) (test #'eq))
  (unless (member repair (repairs object) :key key :test test)
    (push repair (repairs object))))

(defmethod add-repair ((object object-w-learning) 
                       (repair-class symbol)
                       &key (key #'identity) (test #'eq))
  (add-repair object (make-instance repair-class)
              :key key :test test))

;; ---------------------------------------------------------------------------

(defgeneric delete-repair (object repair &key)
  (:documentation "Deletes a repair strategy from an object"))

(defmethod delete-repair ((object object-w-learning) 
                          repair
                          &key (key #'identity) (test #'eq))
  (setf (repairs object) (remove repair (repairs object)
                                 :key key :test test)))

;; ---------------------------------------------------------------------------

(defmethod copy-object-content ((source object-w-learning)
                                (destination object-w-learning))
  
  (setf (diagnostics destination) (copy-list (diagnostics source)))
  (setf (problems destination) (problems source))
  (setf (repairs destination) (repairs source)))

