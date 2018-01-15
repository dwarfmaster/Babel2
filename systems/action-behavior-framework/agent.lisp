
(in-package :action-behavior-framework)

;; ============================================================================
;; agent
;; ============================================================================

(export '(action-agent))

(defclass action-agent (agent)
  ((rerun
    :documentation "After an agent-level repair-strategy has been
    successfull it can fill this slot with data it wants to be
    available in the next run."
    :type t :initform nil :initarg :rerun :accessor rerun)))

;; ----------------------------------------------------------------------------

(defmethod restart-object ((agent action-agent) (restart-data t)
                           &key &allow-other-keys)
  "rerunning just requires to set rerun agent to t"
  (setf (rerun agent) t))

;; ----------------------------------------------------------------------------

(export '(consolidate-agent consolidation-started consolidation-ended))

(define-event consolidation-started (agent t))

(define-event consolidation-ended (agent t))

(defgeneric consolidate-agent (agent world)
  (:documentation "Is called for each of the interacting agents ath the end of
      an interaction. It is intended for commiting learned things or update
      scores of inventories."))

(defmethod consolidate-agent (agent
                              world)
  "empty default implementation"
  (declare (ignore agent world))
  nil)

;; ----------------------------------------------------------------------------

(export 'act)

(defgeneric act (agent world action)
  (:documentation "Called from run-agent to act on some action and
     should return a new action or nil. Function might be called
     multiple times on the same action when agent repairs repaired
     something. Should return an action object."))

(defmethod act (agent world action)
  "default implementation returns no-action"
  (declare (ignore agent world action))
  (make-instance 'no-action))

;; ----------------------------------------------------------------------------

(export 'finalize-action)

(defgeneric finalize-action (agent world action)
  (:documentation "Called from run-agent after nothing was repaired anymore with
                   the last action computed in act"))

(defmethod finalize-action (agent world action)
  "default does nothing but return the action already computed"
  (declare (ignore agent world))
  action)

;; ----------------------------------------------------------------------------
;; events and helpers for running agents

(export '(run-agent run-agent-started run-agent-finished))

(define-event run-agent-started
  (agent agent) (world t)(interaction interaction)(experiment experiment))

(define-event run-agent-finished
  (agent agent) (world t)(interaction interaction)(experiment experiment)
  (action action))

(defgeneric run-agent (agent world interaction experiment &key &allow-other-keys)
  (:documentation "overloadable method for running an agent"))

(defmethod run-agent ((agent action-agent)
                      (world action-world)
                      (interaction interaction)
                      experiment &key max-rerun &allow-other-keys)
  (declare (ignorable experiment))
  (loop
   initially (setf (rerun agent) nil)
   with last-action = (first (actions world))
   with next-action = nil
   for r from 0
   for new-action
   = (progn
       ;; reset rerun agent data
       (setf (rerun agent) nil)
       ;; act
       (act agent world last-action))
   do
   (setf next-action new-action) 
   ;; run again when rerun agent is t
   while (and (rerun agent)
              (or (null max-rerun)
                  (< r max-rerun)))
   finally
   ;; finalize the action
   (setf next-action
         (finalize-action agent world next-action))
   (return next-action)))