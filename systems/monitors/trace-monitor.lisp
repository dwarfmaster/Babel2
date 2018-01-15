;;;;
;;;; File: trace-monitor.lisp
;;;;
;;;; A simple monitor class for print debugging
;;;;
;;;;

(in-package :monitors)

(export '(trace-monitor monitor-stream
	  activate-buffering-of-trace-monitors  
	  clear-trace-monitors-buffer
	  deactivate-buffering-of-trace-monitors
	  print-buffered-messages-of-trace-monitors
	  print-with-overline))


(defclass trace-monitor (monitor)
  ()
  (:documentation 
   "Prints string messages on a screen or keeps them in a shared buffer for later retrieval"))

(defparameter *buffering-active* nil)
(defparameter *buffer-stream* t)
(defparameter *message-buffer* nil)

(defgeneric monitor-stream (monitor)
  (:documentation "Returns a stream for a monitor. Depending on
      whether buffering is active, this returns t or a string
      stream"))

(defmethod monitor-stream ((monitor trace-monitor))
  (when *buffering-active*
    (let ((last-message (get-output-stream-string *buffer-stream*)))
      (unless (equal last-message "")
	(push last-message *message-buffer*))))
  *buffer-stream*)

(defmethod initialize-instance :around ((monitor trace-monitor) &key id &allow-other-keys)
  "Automatically adds an event with name 'trace-monitor to the event list
   and defines a handler for that event"
  (call-next-method)
  (setf (error-occured-during-initialization monitor) t)
  (make-event-unless-already-defined id '((message string)))
  #-:lispworks (let ((previous-monitor (get-monitor id)))
                 (when (or (not previous-monitor)
                           (error-occured-during-initialization previous-monitor)
                           (not (find id (event-ids monitor))))
                   (make-event-handler 
                    id id `((unless (equal message "")
                              (format (monitor-stream monitor) "~a" message))))))
  (setf (error-occured-during-initialization monitor) nil))

(defmethod print-object ((monitor trace-monitor) stream)
  "prints a monitor on one line"
  (format stream "<trace-monitor ~a>" (id monitor)))

;; ----------------------------------------------------------------------------

(defun activate-buffering-of-trace-monitors ()
  "Switches on buffering. Messages for trace monitors are not printed immediately anymore.
   Instead, they are kept in a buffer."
  (setf *buffering-active* t)
  (setf *message-buffer* nil)
  (setf *buffer-stream* (make-string-output-stream)))

(defun print-buffered-messages-of-trace-monitors ()
  "Prints the buffered messages of the trace monitors onto the screen" 
  (if *buffering-active*
      (progn
	(let ((last-message (get-output-stream-string *buffer-stream*)))
	  (unless (equal last-message "")
	    (push last-message *message-buffer*)))
	(loop for message in (reverse *message-buffer*)
	   do (format t "~a" message))
	t)
      (format t "Warning: buffering not active")))

(defun clear-trace-monitors-buffer ()
  "Clears the buffer for the trace monitors. Does not change activation state"
  (setf *message-buffer* nil))

(defun deactivate-buffering-of-trace-monitors ()
  "Deactivates buffering"
  (setf *buffering-active* nil)
  (setf *message-buffer* nil)
  (setf *buffer-stream* t))



;; ----------------------------------------------------------------------------

(defgeneric print-with-overline (monitor character message)
  (:documentation "TODO"))

(defmethod print-with-overline ((monitor trace-monitor)
				(character character)
				(message string))
  (format (monitor-stream monitor) "~%~a~%~a"
	  (make-string (length message) :initial-element character) message))

;; ----------------------------------------------------------------------------
;; trace-monitor utilities:

(export '(format-monitor))

(defmacro format-monitor (format-pattern &rest format-args)
  "Convenient shorthand for (format (monitor-stream monitor) pattern args)."
  `(format (monitor-stream monitor) ,format-pattern ,@format-args))


;; ############################################################################
