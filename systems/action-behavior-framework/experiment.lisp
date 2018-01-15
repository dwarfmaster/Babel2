
(in-package :action-behavior-framework)

;; ============================================================================
;; experiment
;; ============================================================================

(export '(action-experiment))

(defclass action-experiment (experiment)
  ((world :initform (make-instance 'action-world))))

;; ----------------------------------------------------------------------------

(defmethod begin-interaction ((interaction interaction)
                              &rest parameters
                              &key)
  "experiment begin-interaction calls begin-interaction on world
   and interacting agents"
  (loop for agent in (interacting-agents interaction)
        do (apply 'begin-interaction agent parameters)))

(defmethod begin-interaction ((experiment experiment)
                              &rest parameters
                              &key)
  "experiment begin-interaction calls begin-interaction on world
   and interacting agents"
  (apply 'begin-interaction (world experiment) parameters)
  (apply 'begin-interaction (car (interactions experiment))
         parameters))

;; ----------------------------------------------------------------------------

(defmethod finish-interaction ((interaction interaction)
                               &rest parameters
                               &key)
  "experiment finish-interaction calls finish-interaction on world
   and interacting agents"
  (loop for agent in (interacting-agents interaction)
        do (apply 'finish-interaction agent parameters)))

(defmethod finish-interaction ((experiment experiment)
                               
                               &rest parameters
                               &key)
  "experiment finish-interaction calls finish-interaction on world
   and interacting agents"
  (apply 'finish-interaction (world experiment) parameters)
  (apply 'finish-interaction (car (interactions experiment)) parameters))

;; ----------------------------------------------------------------------------

(defmethod interact ((experiment action-experiment)
                     (interaction interaction) &key &allow-other-keys)
  (begin-interaction experiment)
  (loop with at-least-one-agent-returned-an-action = nil
        do
        (setf at-least-one-agent-returned-an-action nil)
        (loop for agent in (interacting-agents interaction)
              do
              (notify run-agent-started agent (world experiment)
                      interaction experiment)
              (let ((action (or (run-agent agent (world experiment)
                                           interaction experiment)
                                (make-instance 'no-action))))
                (update-world (world experiment) action)
                (unless (typep action 'no-action)
                  (setf at-least-one-agent-returned-an-action t))
                (notify run-agent-finished agent (world experiment)
                        interaction experiment
                        action)))
        while at-least-one-agent-returned-an-action)
  (loop for agent in (interacting-agents interaction)
        do
        (notify consolidation-started agent)
        (consolidate-agent agent (world experiment))
        (notify consolidation-ended agent))
  (finish-interaction experiment))
