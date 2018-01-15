
(in-package :experiment-framework)

;; ############################################################################
;; agent
;; ----------------------------------------------------------------------------

(export '(agent
          discourse-role utterance
          communicated-successfully))

(defclass agent (configuration blackboard)
  ((experiment :initarg :experiment :accessor experiment :initform nil
               :documentation "The experiment this agent is part of")
   (id
    :documentation "The unique identifier of this agent."
    :type (or symbol fixnum) :initarg :id :initform (make-id "AGENT") :reader id)    
   (discourse-role 
    :documentation "A symbol designating the role the agent plays in
    the game. e.g. unknown, speaker, hearer. You have to set this slot
    yourself in method determine-interacting-agents."
    :type symbol :initform 'not-set :accessor discourse-role)
   (utterance 
    :documentation "The utterance that either a speaker produced or a
    hearer received. You have to set this slot yourself."
    :type t :initarg :utterance :initform nil :accessor utterance)
   (communicated-successfully 
    :documentation "Whether the agent experienced success."
    :initform nil :accessor communicated-successfully))
  (:documentation "Base class for all agents"))

;; ----------------------------------------------------------------------------
;; get-configuration

(defmethod get-configuration ((agent agent) key &key &allow-other-keys)
  "get-configuration for a agent tries the experiment if no configuration
   is found in the agent"
  (multiple-value-bind (entry found)
      (call-next-method)
    (if found
      (values entry found)
      (get-configuration (experiment agent) key))))

;; ----------------------------------------------------------------------------
;; print-object

(defmethod print-object ((agent agent) stream)
  (if *print-pretty*
    (pprint-logical-block (stream nil)
      (format stream "<~(~:w~):~:_ id: ~a,~:_ configuration: ~a,~:_" 
              (type-of agent) (id agent) (configuration agent))
      (call-next-method)
      (format stream "~:_ data: ~a>" (data agent)))
    (format stream "<~(~:w~) ~a>" (type-of agent) (id agent))))


