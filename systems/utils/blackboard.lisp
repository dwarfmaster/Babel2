;;;; Definition of blackboard

(in-package :utils)

;; ############################################################################
;; blackboard ADT
;; ----------------------------------------------------------------------------

(export '(blackboard make-blackboard data-fields fields data
	  field?  add-data-field find-data get-data
          set-data append-data push-data remove-data
	  merge-data-fields with-data))

(defclass blackboard ()
  ((data :initarg :data-fields
         :initarg :data
         :initarg :blackboard
         :accessor data-fields
         :accessor data
         :accessor blackboard
         :initform nil
         :type list)))

(defun make-blackboard (&key data-fields)
  (make-instance 'blackboard :data-fields data-fields))

;; ----------------------------------------------------------------------------

(defgeneric fields (blackboard)
  (:documentation "Returns a list of all fields of a blackboard"))

(defmethod fields ((data-fields list))
  (mapcar #'car data-fields))

(defmethod fields ((blackboard blackboard))
  (fields (data-fields blackboard)))

;; ----------------------------------------------------------------------------

(defgeneric field? (blackboard label)
  (:documentation "Returns two values. The first one returns true
    only when there is a field with the given label in the
    blackboard. The second one returns true if the first one is
    true and there is a non-nil value attached to the field."))

(defmethod field? ((blackboard blackboard) (label symbol))
  (let ((entry (assq label (data-fields blackboard))))
    (if entry 
      (values t (cdr entry))
      (values nil nil))))

(defmethod field? ((data-fields list) (label symbol))
  (let ((entry (assq label data-fields)))
    (if entry 
      (values t (cdr entry))
      (values nil nil))))

;; ----------------------------------------------------------------------------
;; add-data-field

(defgeneric add-data-field (blackboard label &optional initial-value)
  (:documentation "Adds a data field to the blackboard"))

(defmethod add-data-field ((data-fields list) (label symbol)
			   &optional (initial-value nil))
  (if (assoc label data-fields)
    (error "Data field ~a already exists in this blackboard" label))
  (push (cons label initial-value) data-fields))

(defmethod add-data-field ((blackboard blackboard) (label symbol)
			   &optional (initial-value nil))
  (push (cons label initial-value) (data-fields blackboard))) 

;; ----------------------------------------------------------------------------
;; find-data

(defgeneric find-data (blackboard label &key)
  (:documentation "When the field exists, returns the value and t,
      otherwise nil and nil."))

(defmethod find-data ((data-fields list) (label symbol) &key &allow-other-keys)
  (let ((entry (assq label data-fields)))
    (if entry
      (values (cdr entry) t)
      (values nil nil))))

(defmethod find-data ((blackboard blackboard) (label symbol)
                      &rest rest
                      &key omit-owner)
  (multiple-value-bind (value found)
      (apply 'find-data (data-fields blackboard) label rest)
    (cond
     (found (values value found))
     ((and (not omit-owner) (slot-exists-p blackboard 'owner))
      (apply 'find-data (data-fields blackboard) rest))
     (t (values nil nil)))))

;; ----------------------------------------------------------------------------
;; get-data

(defgeneric get-data (blackboard label &key)
  (:documentation "Retrieves the data for the given label from the
  blackboard. Returns an error if the data field does not exist."))

(defmethod get-data (blackboard (label symbol) &rest rest)
  (multiple-value-bind
      (value found) (apply 'find-data blackboard label rest)
    (if found
      value
      (error "Data field ~a does not exist." label))))

;; ----------------------------------------------------------------------------
;; append-data

(defgeneric append-data (blackboard label data)
  (:documentation "Appends data to a data field of a blackboard. When
  the field does not exist, then it is added to the blackboard. When
  the data-field already exists it assumes the value to be a list."))

(defmethod append-data ((blackboard blackboard) (label symbol) data)
  (let ((entry (assq label (data-fields blackboard))))
    (if entry
      (setf (cdr entry) (append (cdr entry) (listify data)))
      (push (cons label (listify data)) (data-fields blackboard)))))

(defmethod append-data ((data-fields list) (label symbol) data)
  (let ((entry (assq label data-fields)))
    (if entry
      (setf (cdr entry) (append (cdr entry) (listify data)))
      (push (cons label (listify data)) data-fields)))
  data-fields)

;; ----------------------------------------------------------------------------
;; push-data

(defgeneric push-data (blackboard label data)
  (:documentation "Pushes data to a data field of a blackboard. When
  the field does not exist, it is added to the blackboard. When
  the data-field already exists it assumes the value to be a list."))

(defmethod push-data ((blackboard blackboard) (label symbol) data)
  (let ((entry (assq label (data-fields blackboard))))
    (if entry
      (setf (cdr entry) (cons data (cdr entry)))
      (push (cons label (list data)) (data-fields blackboard)))))

(defmethod push-data ((data-fields list) (label symbol) data)
  (let ((entry (assq label data-fields)))
    (if entry
      (setf (cdr entry) (cons data (cdr entry)))
      (push (cons label (list data)) data-fields)))
  data-fields)

;; ----------------------------------------------------------------------------
;; set-data

(defgeneric set-data (blackboard label data)
  (:documentation "Writes data to a data field of a
  blackboard. When the field does not exist, then it is added to
  the blackboard"))

(defmethod set-data ((data-fields list) (label symbol) data)
  (let ((entry (assq label data-fields)))
    (if entry
      (setf (cdr entry) data)
      (push (cons label data) data-fields)))
  data-fields)

(defmethod set-data ((blackboard blackboard) (label symbol) data)
  (let ((entry (assq label (data-fields blackboard))))
    (if entry
      (setf (cdr entry) data)
      (push (cons label data) (data-fields blackboard)))))

;; ----------------------------------------------------------------------------
;; remove-data

(defgeneric remove-data (blackboard label)
  (:documentation "Removes the complete entry for the given
  label. This also removes the label itself, not only it's
  data."))

(defmethod remove-data ((blackboard blackboard) (label symbol))
  (setf (data-fields blackboard)
	(remove label (data-fields blackboard) :key #'first)))

(defmethod remove-data ((data-fields list) (label symbol))
  (setf data-fields
	(remove label data-fields :key #'first)))

;; ----------------------------------------------------------------------------
;; with-data

(defmacro with-data ((&rest labels) blackboard &body body)
  "Shortcut for binding a set of variables to values from a blackboard.
   The given labels are used both as the name of the variable as well as
   the label of the blackboard field whose data has to be bound to that
   variable."
  `(let (,@(loop for label in labels
             collect `(,label (get-data ,blackboard ',label))))
     (declare (ignorable ,@labels))
     (macrolet ((setd (setd-label setd-value)
		  `(set-data ,',blackboard ,setd-label ,setd-value)))
       ,@body)))

;; ----------------------------------------------------------------------------

(defmethod copy-object ((blackboard blackboard))
  (let ((copy (make-instance 'blackboard)))
    (copy-object-content blackboard copy)
    copy))

(defmethod copy-object-content ((source blackboard) (destination blackboard))
  (setf (data-fields destination) nil)
  (loop for field in (data-fields source)
        do (push (cons (first field) (copy-object (rest field))) 
                 (data-fields destination))))

(defgeneric merge-data-fields (source destination))

(defmethod merge-data-fields ((source blackboard) (destination blackboard))
  (loop for field in (data-fields source)
        do (set-data destination (first field) (copy-object (rest field)))))

(defmethod merge-data-fields ((source list) (destination blackboard))
  (loop for field in source
        do (set-data destination (first field) (rest field))))

(defmethod print-object ((blackboard blackboard) stream)
  (if *print-pretty*
    (pprint-logical-block (stream nil)
      (format stream "<blackboard:~:_ ~{~(~a~):~:_ ~:a~^,~:_ ~}>"
              (loop for (label . value) in (data blackboard)
                    append (list label value))))
    (call-next-method)))

;; ############################################################################
