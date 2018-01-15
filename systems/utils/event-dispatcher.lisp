(in-package :utils)

;; ############################################################################
;; d-event struct:
;; ----------------------------------------------------------------------------

(export '(d-event
          d-event-target
          d-event-type))

(defstruct d-event
  (target nil)
  (type 'd-event :type symbol))

;;(defmethod target ((d-event d-event)) (d-event-target d-event))

(defmethod print-object ((d-event d-event) stream)
  (format stream "(~~d-event~~ type: ~(~a~) target: ~a)"
          (d-event-type d-event)
          (d-event-target d-event)))


;; ############################################################################
;; d-event-dispatcher class:
;; ----------------------------------------------------------------------------

(export '(d-event-dispatcher target))

(defclass d-event-dispatcher ()
  ((target
    :documentation "The target of this dispatcher, if not given than the dispatcher
      is the target."
    :initform nil
    :accessor target)
   (listeners
    :documentation "alist: ((d-event-type . listener*)*)"
    :type list
    :initform nil
    :accessor listeners)))

(defmethod initialize-instance :after ((dispatcher d-event-dispatcher)
                                       &key target)
  (unless target (setf (target dispatcher) dispatcher)))

(defmethod print-object ((d-event-dispatcher d-event-dispatcher) stream)
  (declare (ignorable d-event-dispatcher))
  (format stream "(~~d-event-dispatcher~~)"))

;; ----------------------------------------------------------------------------
;; interface:

(export '(add-event-listener
          remove-event-listener
          dispatch-event))

;; args in flash: type:String, listener:Function, useCapture:Boolean = false,
;;                priority:int = 0, useWeakReference:Boolean = false)
(defgeneric add-event-listener (target type listener)
  (:documentation "Registers an d-event listener object with a dispatcher
     object so that the listener receives notification of an d-event dispatched
     by the dispatcher.
     To be implemented by the d-event-dispatcher and types that cannot inherit
     from d-event-dispatcher."))

(defgeneric remove-event-listener (target type listener)
  (:documentation "Removes a listener from the d-event-dispatcher object."))

;;; Parameters:
;;; - d-event:d-event — The d-event object that is dispatched into the event flow.
;;;
(defgeneric dispatch-event (target d-event)
  (:documentation "Dispatches an d-event into the event flow. The event target is
     the d-event-dispatcher object upon which the dispatch-event method is called."))

;; ----------------------------------------------------------------------------
;; dispatcher implementation:

(defmethod add-event-listener ((dispatcher d-event-dispatcher)
                               (event-type symbol)
                               (listener function))
  (let ((entry (assq event-type (listeners dispatcher))))
    (if entry
      (pushnew listener entry)
      (push (list event-type listener) (listeners dispatcher)))))

(defmethod remove-event-listener ((dispatcher d-event-dispatcher)
                                  (event-type symbol)
                                  (listener function))
  (let ((entry (assq event-type (listeners dispatcher))))
    (when entry
      (setf (cdr entry) (remove (listeners dispatcher) (cdr entry))))))

(defmethod dispatch-event ((dispatcher d-event-dispatcher)
                           (d-event d-event))
  (setf (d-event-target d-event) (target dispatcher))
  (loop for listener in (assqv (d-event-type d-event) (listeners dispatcher))
        do (funcall listener d-event)))


;; ############################################################################
