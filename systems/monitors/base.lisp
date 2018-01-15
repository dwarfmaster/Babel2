;;;;
;;;; file: base.lisp
;;;; 
;;;; base classes and mechanisms of the monitoring system.
;;;;

(in-package :monitors)

;; ############################################################################
;; monitor
;; ----------------------------------------------------------------------------

(export 'monitor)

;; classes for monitor and events
;;
;; Monitors can handle a number of events (slot 'event-ids').
;; When they become 'activated', 
;; they add themselves  to the subscriber list (slot 'active-monitors') of their events. 

(defclass monitor ()
  ((id :documentation "A unique monitor id"
       :type symbol :initarg :id :reader id)
   (event-ids :documentation "The ids of the events the monitor is listening to"
	      :type list :initform nil :accessor event-ids)
   (active :documentation "If true, then this monitor is notified on its events"
	   :type boolean :initform nil :accessor active) 
   (documentation :documentation "To help the user to guess the purpose of the monitor"
		  :initarg :documentation :initform "no documentation provided"
		  :type string :reader monitor-documentation)
   (source-file :documentation "The file in that the monitor was defined"
		:type (or null pathname) :initform nil :reader source-file
		:initarg :source-file)
   (init-arguments :documentation "The keword parameter list with that the monitor was defined"
		   :type list :reader init-arguments :initarg :init-arguments :initform nil)
   (error-occured-during-initialization
    :documentation "When t, an error occured during the initialization"
    :type boolean :accessor error-occured-during-initialization :initform t))
  (:documentation "Listens to a list of events. Is notified when only when it is active"))


;; ############################################################################
;; event
;; ----------------------------------------------------------------------------

(defclass event () 
  ((id :documentation "An unique event id"
       :type symbol :initarg :id :reader id)
   (active-monitors :documentation "The list of monitors that are notified"
		    :type list :initform nil :accessor active-monitors)
   (source-file :documentation "The file in that the event was defined"
		:type (or null pathname) :initform nil :reader source-file
		:initarg :source-file)
   (parameters :type list :initarg :parameters :reader parameters
	       :documentation "A list of (name type) parameter definitions,
                               used for method generation"))
  (:documentation "Represents an event with its parameters and which monitors
                   are listening to the event."))

(defvar *monitors* (make-hash-table )  "All registered monitors.")
(defvar *events* (make-hash-table) "All registered events")

(defun get-monitor (id) "returns the monitor for 'id'" (gethash id *monitors*))
(defun get-event (id) "returns the monitor for 'id'" (gethash id *events*))

(defmethod initialize-instance :around ((monitor monitor)
                                        &key id documentation source-file)
  "Creates a monitor and automatically adds it to the monitor list."
  (setf (error-occured-during-initialization monitor) t)
  (check-type id (and (not null) symbol)
              "a symbol (in initialisation of class monitor).")
  (check-type source-file (or null pathname))
  (when documentation (check-type documentation (string)))
  (setf (error-occured-during-initialization monitor) nil)
  (let ((previous-monitor (get-monitor id)))
    (call-next-method)
    (setf (gethash id *monitors*) monitor)
    (when previous-monitor
      (setf (active monitor) (active previous-monitor))
      (setf (event-ids monitor) (event-ids previous-monitor)))))

(defun make-monitor-unless-already-defined (id class &optional init-arguments)
  "Makes a new monitor (only when there is no previously defined monitor
   with these paramters)."
  (let ((previous-monitor (get-monitor id)))
    ;; only make an instance when
   ;; - that monitor was not defined
    ;; - when errors occured during the last initialization of the monitor
    ;; - when parameters changed.
    (when (or (not previous-monitor) 
	      (error-occured-during-initialization previous-monitor)
	      (not (equal (init-arguments previous-monitor) init-arguments)))
      (eval `(make-instance ,class :id ',id 
			    :init-arguments ',init-arguments ,@init-arguments
			    :source-file *load-pathname*))))
  (get-monitor id))


(defun subscribe-to-event (monitor-id event-id)
  (unless (get-event event-id)
    (error "event ~a was not defined yet." event-id))
  (unless (get-monitor monitor-id)
    (error "monitor ~a was not defined yet." monitor-id))
  (pushnew event-id (event-ids (get-monitor monitor-id)) :test #'equal)
  (when (active (get-monitor monitor-id))
    (pushnew monitor-id (active-monitors (get-event event-id)) :test #'equal)))


(defmethod initialize-instance :around ((event event) &key id parameters source-file)
  "Creates an event and automatically adds it to the list."
  (check-type id (and (not null) symbol) 
	      "a symbol (in initialisation of class event).")
  (check-type source-file (or null pathname))
  (dolist (parameter parameters)
    (unless (and (listp parameter) (= (length parameter) 2))
      (error "Parameter definition ~a is not of the form (name class)." parameter))
    (unless (and (first parameter) (symbolp (first parameter)))
      (error "Name ~a of parameter definition ~a is not a valid parameter name."
	     (first parameter) parameter)))
  (call-next-method)
  ;; when there was already an event with that id, copy its list of active monitors
  (let ((previous-event (get-event id)))
    (when previous-event
      (setf (active-monitors event) (active-monitors previous-event))))
  (setf (gethash id *events*) event))


(defmacro intern-in-package-of (variable name)
  "interns a new symbol 'name' in the package of 'variable'"
  `(intern ,name (if (listp ,variable)
		     (symbol-package (first ,variable))
		     (symbol-package ,variable))))

(defvar *monitor-notifications-disabled* nil
  "when t, monitor notifications generated by the notify macro below won't do anything")

(defun make-event-unless-already-defined (id parameters)
  "Makes an event (only when it is not defined yet or when parameters changed)."
  (let ((method-name (intern-in-package-of id (format nil "HANDLE-~a-EVENT" id))))
    (let* ((previous-event (get-event id))
	   (parameters-changed 
	    (and previous-event (not (equal (parameters previous-event) parameters)))))
      (when parameters-changed
	;; the event is defined with different parameters as before
	;; -> we need to unbind all existing handling methods
	(fmakunbound method-name)
	(format t "~%Warning: new parameter list for event ~a. ~
                   All handlers for this event were unbound!" id))
      (when (or (not previous-event) parameters-changed)
	(make-instance 'event :id id :parameters parameters :source-file *load-pathname*)
	;; define a generic method for the handling of this event
        (compiled-eval
         `(defgeneric ,method-name  
              (monitor monitor-id event ,@(mapcar #'first parameters))))
	;; a very unspecific implementation of that method that is only called when
	;; other parameter types than those specified in the event parameter lists were 
	;; passed to a notify macro
	(compiled-eval
         `(defmethod ,method-name 
                     ((monitor monitor) (monitor-id t) (event t)
                      ,@(mapcar #'(lambda (p) (list (first p) t)) parameters))
            (declare (ignorable monitor monitor-id event))
            ,@(loop for parameter in parameters  for number from 2
                    collect
                    `(unless
                         (subtypep (class-name (class-of ,(first parameter))) 
                                   ',(second parameter))
                       (error "Parameter ~a (~a) should be of type ~a. ~
                                   Instead, ~a (~a) was passed."
                              ,number ',(first parameter) ',(second parameter) 
                              ,(first parameter) 
                              (class-name (class-of ,(first parameter))))))))))))

;; ############################################################################
;; event-handlers:
  

(defun make-event-handler (monitor-id event-id body &key method-qualifier)
  "Makes an event handler."
  (let ((monitor-ids (if (listp monitor-id) monitor-id (list monitor-id))))
    (loop for monitor-id in monitor-ids
	 do (subscribe-to-event monitor-id event-id)
	 (compiled-eval 
	  `(defmethod 
                      ,@(remove nil (list (intern-in-package-of 
                                           event-id (format nil "HANDLE-~a-EVENT" event-id))
                                          method-qualifier))
                      ((,(intern-in-package-of monitor-id "MONITOR") 
                        ,(class-name (class-of (get-monitor monitor-id))))
                       (,(intern-in-package-of monitor-id "MONITOR-ID") (eql ',monitor-id))
                       (,(intern-in-package-of monitor-id "EVENT-ID") (eql ',event-id))
                       ,@(parameters (get-event event-id)))
	     ,@(if (subtypep (class-of (get-monitor monitor-id)) 'trace-monitor)
                 `((let ((*print-pretty* nil)) ,@body))
                 body))))))


;; ############################################################################
;;; Activation and deactivation of monitors:

(defgeneric activate-monitor-method (monitor &optional active)
  (:documentation "Changes the activation of a monitor"))

(defmethod activate-monitor-method ((monitor monitor) &optional (active t))
  "activates/deactivates an instance of a monitor"
  (if active
      (if (active monitor)
	  (format t "~%warning: monitor ~a already active" (id monitor))
	  (progn
	    (dolist (event-id (event-ids monitor))
	      (pushnew (id monitor) 
		       (active-monitors (gethash event-id *events*)) :test #'equal))
	    (setf (active monitor) t)))
      (if (active monitor)
	  (progn
	    (dolist (event-id (event-ids monitor))
	      (let ((event (gethash event-id *events*)))
		(setf (active-monitors event)
		      (delete (id monitor) (active-monitors event)))))
	    (setf (active monitor) nil))
	  (format t "~%warning: monitor ~a not active" (id monitor)))))

(defmethod activate-monitor-method ((id symbol) &optional (active t))
  "Activates a monitor specified by an id"
  (let ((monitor (get-monitor id)))
    (if monitor 
	(activate-monitor-method monitor active)
	(error "monitor ~a is not defined." id))))

;; when t, monitors can not be enabled/disabled. use with care
(defvar *disable-activation-of-monitors* nil)

(defmethod activate-monitor-method :around (monitor &optional active)
  (declare (ignore monitor active))
  (unless *disable-activation-of-monitors*
    (call-next-method)))

      
(defgeneric toggle-monitor-method (monitor))

(defmethod toggle-monitor-method ((id symbol))
  "Toggles the activation of a monitor"
  (let ((monitor (get-monitor id)))
    (if monitor 
	(toggle-monitor-method monitor)
	(error "monitor ~a is not defined" id))))

(defmethod toggle-monitor-method ((monitor monitor))
  "Toggles the activation of a monitor"
  (activate-monitor-method monitor (not (active monitor))))



(defmethod print-object ((monitor monitor) stream)
  "prints a monitor on one line"
  (format stream "<~(~a~) ~a>" (class-name (class-of monitor)) (id monitor)))

(defmethod print-object ((event event) stream)
  "prints an event in a readable way" 
  (format stream "<event ~a>" (id event)))



(defun make-file-name-with-experiment-class (file-name experiment-class)
  "A helper function for making file-names that contains the experiment class"
  (if (pathname-name file-name)
      (format nil "~a~a-~a.~a" 
              (make-pathname :directory (pathname-directory file-name))
              (string-downcase experiment-class)
              (pathname-name file-name)
              (pathname-type file-name))
      (format nil "~a~a.~a"
              (make-pathname :directory (pathname-directory file-name))
              (string-downcase experiment-class)
              (pathname-type file-name))))

(defun make-file-name-with-time-and-experiment-class (file-name experiment-class)
  "A helper function for making file-names that contain the time and the experiment class"
  (mkstr (make-pathname :directory (pathname-directory file-name))
	 (multiple-value-bind (sec min hour day month year)
	     (decode-universal-time (get-universal-time))
	   (format nil "~d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-"
                   year month day hour min sec))
	 (string-downcase experiment-class)
	 "-" (pathname-name file-name) "." (pathname-type file-name)))
#+sbcl
(defun make-file-name-with-job-and-task-id (file-name experiment-class)
  "A helper function for making file-names that contain the time and the experiment class"
  (mkstr (make-pathname :directory (pathname-directory file-name))
 	 (format nil "~a-~a-" (sb-unix::posix-getenv "SLURM_ARRAY_JOB_ID")
                 (sb-unix::posix-getenv "SLURM_ARRAY_TASK_ID"))
	 (string-downcase experiment-class)
 	 "-" (pathname-name file-name) "." (pathname-type file-name)))

#+(or lispworks ccl)
(defun make-file-name-with-job-and-task-id (file-name experiment-class)
  "A helper function for making file-names that contain the time and the experiment class"
  (mkstr (make-pathname :directory (pathname-directory file-name))
 	 (format nil "~a-~a-" (ccl::getenv "SLURM_ARRAY_JOB_ID")
                (ccl::getenv "SLURM_ARRAY_TASK_ID"))
	 (string-downcase experiment-class)
         "-" (pathname-name file-name) "." (pathname-type file-name)))

(defun make-file-name-with-time (file-name)
  "A helper function for making file-names that contain the time"
  (mkstr (make-pathname :directory (pathname-directory file-name))
	 (multiple-value-bind (sec min hour day month year)
	     (decode-universal-time (get-universal-time))
	   (format nil "~d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-"
                   year month day hour min sec))
	 (pathname-name file-name) "." (pathname-type file-name)))

;; #+sbcl
;; (defun make-file-name-with-job-and-task-id (file-name experiment-class)
;;   "A helper function for making file-names that contain the time and the experiment class"
;;   (mkstr (make-pathname :directory (pathname-directory file-name))
;; 	 (format nil "~a-~a-" (sb-unix::posix-getenv "SLURM_ARRAY_JOB_ID")
;;                  (sb-unix::posix-getenv "SLURM_ARRAY_TASK_ID"))
;; 	 (string-downcase experiment-class)
;; 	 "-" (pathname-name file-name) "." (pathname-type file-name)))
