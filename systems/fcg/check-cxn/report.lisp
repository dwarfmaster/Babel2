;;; Rather general reporting classes for collecting messages about items.

(in-package :fcg)

(defclass report-item ()
    ((item
      :initarg :item
      :initform (error "Must supply an item to report on.")
      :reader item
      :documentation "The item that is reported on.")
     (item-class
      :initarg :item-class
      :initform nil
      :accessor item-class
      :documentation "The class of the report-item.")
     (message
      :initarg :message
      :initform nil
      :accessor message
      :documentation "The message string for the report about the item.")
     (attachments
      :initarg :attachments
      :initform nil
      :accessor attachments
      :documentation "Any comments and other attachments to the item.")))

(defmethod print-object ((object report-item) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (item item-class message attachments) object
      (format stream "~s :ITEM-CLASS ~d :MESSAGE ~d :ATTACHMENTS ~d" item item-class message attachments))))

(defun make-report-item (item &key item-class message attachments)
  (make-instance 'report-item :item item :item-class item-class :message message :attachments attachments))

(defclass report ()
  ((name
    :initarg :name
    :initform (format nil "report-~a" (gensym))
    :accessor name
    :documentation "The name of the report.")
   (items
    :initarg :items
    :initform nil
    :accessor items
    :documentation "A list of report-items.")
   (description
    :initarg :description
    :initform nil
    :accessor description
    :documentation "A description of what the report is about.")))

(defmethod print-object ((object report) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name items description) object
      (progn
        (format stream "~s :DESCRIPTION ~d" name description)
        (loop for item in items
             do (format stream "~& ~s" item))))))

(defun make-report (&key name items description)
  (make-instance 'report :name name :items items :description description))

;; for now just to be able to specify in gen. funs
;(defclass check-cxn-report (report)())

(defgeneric report (item report))

(defmethod report (item (report report))
  (error "Function report was called with an unknown item-type: ~a" item))

(defmethod report ((item report-item) report)
  (error "Function report was called with an unknown report-type: ~a" report))

(defmethod report ((item report-item) (report report))
  (with-accessors ((report-items items)) report
    (setf report-items (append report-items (list item))))
  report)

(defun get-report-items (report &key (item-class nil))
  (if item-class
      (loop for item in (items report)
           when (eql (item-class item) item-class)
           collect item)
      (items report)))

;(defun print-report (report &key (monitors nil))
  ;(unless (eq nil monitors)
      ;(when (eq t monitors)
         ;(setq monitors '(web))) ; make available checks a global var...
      ;(loop for monitor in monitors
          ;when (not (eq monitor nil))
          ;do (compose-report report monitor))))

;(defgeneric compose-report (report monitor)
  ;(:documentation "A set of functions specializing on the type of output monitor for printing the report in a readable fashion"))

;(defmethod compose-report :before ((report t) (monitor t))
  ;(format t "~&Report::: ~a" report))

;(defmethod compose-report ((report report) monitor))

;(defmethod compose-report ((report report) (monitor (eql 'web))))