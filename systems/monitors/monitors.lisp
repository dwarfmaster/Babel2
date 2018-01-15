;;;;
;;;; monitors.lisp
;;;;
;;;; The main interface of the monitoring system.
;;;;

(in-package :monitors)

;; ############################################################################

(export '(define-monitor define-event define-event-handler
	  activate-monitor with-disabled-activation-of-monitors
	  deactivate-monitor deactivate-all-monitors toggle-monitor
	  notify with-disabled-monitor-notifications
          with-disabled-monitors
          with-activated-monitor
          with-activated-monitors
	  print-all-monitors print-all-events
	  interaction-started interaction-number
	  interaction-finished
          repairing-started
          repairing-finished
          repaired
          diagnostic-started
          diagnostic-returned-problems
          diagnosed-problems
          series-finished experiment series-number interaction
          run-series-finished
          batch-finished
	  reset-monitors))

(defmacro define-event (id &body parameters)
  "Defines an event. This needs to be done before monitors using that event can be
   registered. 'parameters' is pairs of parameter names and their classes as used in method
   declarations."
  ;; make an instance at macro expansion time so that the event is already defined
  ;; for the use in the expansion of the define-event-handler macro
  (make-event-unless-already-defined id parameters)
  ;; make an instance at load time 
  ;; (e.g. when this code is loaded from a precompiled file, then there is no macro
  ;; expansion)
  `(make-event-unless-already-defined ',id ',parameters))


(defmacro define-monitor (id
                          &rest init-arguments
                          &key (class ''monitor) documentation
                          &allow-other-keys) 
  "Defines a monitor. Note that all monitors have to be defined before they can
   be used, e.g in defining event handlers. Parameter 'id' is used to identify
   the monitor. 
   Documentation should contain a short string that helps other users to guess
   what the monitor does. If you want to use another base class than 'monitor',
   then pass this to 'class'.
   Note that you can pass arbitrary keyword arguments for the creation of the
   monitor."
  (declare (ignore documentation))
  ;; make an instance at macro expansion time so that the monitor is already defined
  ;; for the use in the expansion of define-event-handler
  (make-monitor-unless-already-defined id class init-arguments) 
  ;; make an instance at load time 
  ;; (e.g. when this code is loaded from a precompiled file,
  ;;  then there is no macro expansion)
  `(defvar ,id (make-monitor-unless-already-defined ',id ',class ',init-arguments)))


(defmacro define-event-handler ((monitor-id event-id) &body body)
  "Defines a handling method for an event a (list of) specific monitor. 
   Pass the event id you want to handle to 'event'.
   The id of the monitor (or a list of many monitor ids) is passed to 'monitor-id'. 
   The parameters for the method are automatically copied from the event
   definition of 'event'.
   The monitor instance is accessible with the parameter 'monitor'."
  `(make-event-handler ',monitor-id ',event-id ',body))
 
(defmacro with-disabled-monitor-notifications (&body body)
  "use this to run some code without monitor notifications"
  `(let ((*monitor-notifications-disabled* t))
     ,@body))


(defmacro notify (event-id &rest data)
  "Notifies all active monitors for 'event-id' on data."
  (unless (get-event event-id)
    (error "~%Event ~a was not registered (in notify)." event-id))
  (unless (= (length data) (length (parameters (get-event event-id))))
    (error "Wrong number of parameters in notify for event ~a.~%Wanted: ~{~a~^, ~}." 
	    event-id (parameters (get-event event-id))))
  `(and (not *monitor-notifications-disabled*)
	(dolist (monitor-id (active-monitors (get-event ',event-id)))
	  (let ((monitor (get-monitor monitor-id)))
	    (,(intern-in-package-of event-id (format nil "HANDLE-~a-EVENT" event-id))
	      monitor monitor-id ',event-id ,@data)))))

(defmacro with-disabled-activation-of-monitors (&body body)
  "use this to run some code without monitor activations"
  `(progn
     (setf *disable-activation-of-monitors* t)
     (unwind-protect (progn ,@body) (setf *disable-activation-of-monitors* nil))))

(defmacro with-activated-monitor (monitor &body body)
  "use this to run some code with a monitor activated
   when exiting active initial active or non-active status of monitors is preserved"  
  `(progn
     (let ((non-active-monitor? (unless (active ,monitor)
                                  ,monitor)))
       (when non-active-monitor? (activate-monitor-method non-active-monitor? t))
       (unwind-protect (progn ,@body)
         (when non-active-monitor? (activate-monitor-method non-active-monitor? nil))))))

(defmacro with-activated-monitors (list-of-monitors &body body)
  "use this to run some code with a list of monitors activated
   when exiting active initial active or non-active status of monitors is preserved"
  `(progn
     (let ((non-active-monitors (loop for monitor in ,list-of-monitors
                                   unless (active (eval monitor))
                                   collect monitor)))
     (loop for monitor in non-active-monitors
           do (activate-monitor-method monitor t))
     (unwind-protect (progn ,@body)
       (loop for monitor in non-active-monitors
             do (activate-monitor-method monitor nil))))))

(defmacro activate-monitor (id &optional (active t))
  "Activates a monitor"
  `(activate-monitor-method ',id ',active))

(defmacro deactivate-monitor (id)
  "Deactivates a monitor"
  `(activate-monitor-method ',id nil))

(defun deactivate-all-monitors ()
  "Deactivates all active monitors"
  (maphash #'(lambda (key m) (declare (ignore key))
		     (when (active m) (activate-monitor-method m nil)))
	   *monitors*))

(defmacro toggle-monitor (id)
  "Toggles the activation of a monitor"
  `(toggle-monitor-method ',id))

(defmacro toggle-monitors (&rest ids)
  "Toggles the activation of monitors"
  `(dolist (id ',@ids)
    (toggle-monitor-method id)))

(defun print-all-monitors ()
  "Prints all registered monitors. You can use this function to easily see
   which monitors are active."
  (let ((monitors nil))
    (maphash #'(lambda (key m) (declare (ignore key)) (push m monitors)) *monitors*)
    (loop for monitor in (sort monitors #'string-lessp :key #'id)
       do (format t "~%~a: ~:[inactive~;active~]~%   doc:    ~a~@[~%   source: ~a~]~%" 
		  monitor (active monitor) (monitor-documentation monitor)
		  (source-file monitor)))))

(defun print-all-events ()
  "Prints all events"
  (let ((events nil))
    (maphash #'(lambda (key e) (declare (ignore key)) (push e events)) *events*)
    (loop for event in (sort events #'string-lessp :key #'id)
       do (format t "~%~(~a~)~@[~%   parameters: ~(~{(~a ~a)~^ ~}~)~]~
                     ~@[~%   source:     ~a~]~%" 
		  (string-downcase (symbol-name (id event)))
		  (loop for p in (parameters event)
		     append (list (symbol-name (first p)) (symbol-name (second p))))
		  (source-file event)))))



(defmacro with-disabled-monitors (&body body)
  "use this to run some code without monitor activations and
   notifications"
  `(with-disabled-monitor-notifications
     (with-disabled-activation-of-monitors
       ,@body)))

(defmethod test-framework::run-tests :around (&optional package-name)
  "disables monitor notifications in tests"
  (declare (ignore package-name))
  (with-disabled-monitors
    (call-next-method)))

;; ============================================================================
;; These events are used by several monitors to flush data or to trigger
;;  other actions.
;; Please include the notifications for them in your code. Terminology: 
;; - an interaction is the basic unit for that values are recorded.
;;   This could be a language game.
;; - a series is a set of subsequent interactions (games)
;; - a batch is a set of series (repetitions of subsequent interactions) 

;; on the start (reset) of an interaction (game)
(define-event interaction-started (experiment t)(interaction t)
              (interaction-number number))

;; when a game finished
(define-event interaction-finished (experiment t) (interaction t)
              (interaction-number number))

;; when a series of interactions finished (called in run-batch or client-processes)
(define-event series-finished (series-number number))

;; when a series of interactions finished (really called :after run-series and gets the experiment)
(define-event run-series-finished (experiment t))

;; when a batch of series of interactions finished
(define-event batch-finished (experiment-class string))

;; use this event to reset all data recorders
(define-event reset-monitors)

;; ############################################################################
