
(in-package :experiment-framework)

;; ############################################################################
;; experiment
;; ----------------------------------------------------------------------------

(export '(experiment
          agents
          population
          world
          interactions))

(defclass experiment (blackboard configuration)
  ((id :initarg id :accessor id :initform (make-id 'experiment)
       :documentation "id of the experiment")
   (agents
    :documentation "A list of agents to be used in the experiment."
    :type list
    :initarg :agents
    :initarg :population
    :accessor agents
    :accessor population
    :initform nil)
   (series-number
    :documentation "The series counter."
    :type number
    :initarg :series-number
    :initform 0
    :accessor series-number)
   (world
    :initarg :world
    :accessor world
    :initform nil)
   (interactions :initarg :interactions
                 :accessor interactions :initform nil
                 :documentation "a list of interactions"))
  (:documentation "class for experiments"))

;; ----------------------------------------------------------------------------
;; interaction

(export '(interaction interaction-number interacting-agents current-interaction))

(defclass interaction (blackboard)
  ((experiment :initarg :experiment :accessor experiment :accessor owner)
   (interaction-number
    :documentation "A interaction counter that is increased with every interaction"
    :type number
    :initform 0
    :initarg :interaction-number
    :accessor interaction-number)
   (interacting-agents
    :documentation "The agents that are involved in the current interaction"
    :type list
    :initarg :interacting-agents
    :initform nil
    :accessor interacting-agents)
   (communicated-successfully 
    :documentation "Whether the interaction was a success."
    :initform nil :accessor communicated-successfully)))

;; ----------------------------------------------------------------------------
;; current-interaction

(defgeneric current-interaction (thing &key))

(defmethod current-interaction ((experiment experiment) &key &allow-other-keys)
  "fetches the current interaction from interactions in experiment"
  (car (interactions experiment)))

;; ----------------------------------------------------------------------------
;; interacting-agents

(defgeneric interacting-agents (thing))

(defmethod interacting-agents ((experiment experiment))
  (interacting-agents
   (current-interaction experiment)))


;; ----------------------------------------------------------------------------
;; determine-interacting-agents

(export '(determine-interacting-agents interacting-agents-determined
          speaker hearer))

(defgeneric determine-interacting-agents (experiment interaction mode &key)
  (:documentation "Determines which agents are part
    of the next interaction."))

(define-event interacting-agents-determined (experiment experiment)
  (interaction interaction))

(defmethod determine-interacting-agents (experiment
                                         (interaction interaction)
                                         mode
                                         &key &allow-other-keys)
  "This default implementation randomly chooses two interacting agents
   and adds the discourse roles speaker and hearer to them"
  (declare (ignore mode))
  (let ((agents (agents experiment)))
    (setf (interacting-agents interaction)
          (if (> (length agents) 1)
            (random-elts agents 2)
            agents))
    (loop for a in (interacting-agents interaction)
          for d in '(speaker hearer)
          do (setf (discourse-role a) d)
          (setf (utterance a) nil)
          (setf (communicated-successfully a) nil))
    (notify interacting-agents-determined experiment interaction)))

(defgeneric speaker (object &key &allow-other-keys)
  (:documentation "Gets the speaker, probably from an experiment"))

(defgeneric hearer (object &key &allow-other-keys)
  (:documentation "Gets the hearer, probably from an experiment"))

(defmethod speaker ((interaction interaction) &key (discourse-role 'speaker))
  (find discourse-role (interacting-agents interaction) :key #'discourse-role))

(defmethod speaker ((experiment experiment) &key (discourse-role 'speaker))
  (speaker (current-interaction experiment)
           :discourse-role discourse-role))

(defmethod hearer ((interaction interaction) &key (discourse-role 'hearer))
  (find discourse-role (interacting-agents interaction) :key #'discourse-role))

(defmethod hearer ((experiment experiment) &key (discourse-role 'hearer))
  (hearer (current-interaction experiment)
          :discourse-role discourse-role))

;; ----------------------------------------------------------------------------
;; interact

(export '(interact))

(defgeneric interact (experiment interaction &key)
  (:documentation "method called by run-interaction - has to be overloaded"))

;; ----------------------------------------------------------------------------
;; run-interaction

(export '(run-interaction))

(defgeneric run-interaction (experiment &key))

(defmethod run-interaction ((experiment experiment)
                            &key &allow-other-keys)
  "runs an interaction by increasing the interaction number"
  (let ((interaction (make-instance
                      'interaction
                      :experiment experiment
                      :interaction-number (if (interactions experiment)
                                            (+ 1 (interaction-number
                                                  (car (interactions experiment))))
                                            1))))
    (push interaction (interactions experiment))
  
    (determine-interacting-agents experiment interaction
                                  (get-configuration experiment
                                                     :determine-interacting-agents-mode))
    (notify interaction-started experiment interaction (interaction-number interaction))
    (interact experiment interaction)

    (setf (communicated-successfully interaction)
          (loop for agent in (interacting-agents interaction)
             always (communicated-successfully agent)))
    (if (get-configuration experiment :record-every-x-interactions)
	(when 
	    (or 
	       (= (mod (interaction-number interaction) (get-configuration experiment :record-every-x-interactions)) 0) ;; If we set a configuration called record-every-x-interactions, notify will only be fired every x interactions AND notify it the very first interaction
	       (= (interaction-number interaction) 1))
	  (notify interaction-finished experiment interaction (interaction-number interaction))) ;;; if you do not set such a configuration, notify will always be notified
	(notify interaction-finished experiment interaction (interaction-number interaction)))
    (values interaction experiment)))

;; ----------------------------------------------------------------------------
;; run-series

(export '(run-series))

(defgeneric run-series (experiment number-of-interactions
                                   &key &allow-other-keys)
  (:documentation "runs a series of interactions"))

(defmethod run-series ((experiment experiment) (number-of-interactions number) &key)
  (loop for interaction from 1 to number-of-interactions 
        do (run-interaction experiment))
  (notify run-series-finished experiment))

;; ----------------------------------------------------------------------------
;; run-batch

(export '(run-batch run-batch-for-different-configurations))

(defgeneric run-batch (experiment number-of-interactions number-of-series
				  &key &allow-other-keys)
  (:documentation "runs a batch (multiple series of interactions)"))

(defmethod run-batch ((experiment-class symbol) 
		      (number-of-interactions number)
                      (number-of-series number)
                      &rest experiment-parameters)
  "Runs a series of experiments. Each time a new instance of experiment
   is created"
  (notify reset-monitors)
  (loop for series from 1 to number-of-series
        for experiment = (apply 'make-instance
                                experiment-class
                                :series-number series                                
                                experiment-parameters)
        do (run-series experiment (+ 1 number-of-interactions))
        (notify series-finished series))
  (notify batch-finished (symbol-name experiment-class)))

(defun run-batch-for-different-configurations
       (&key experiment-class number-of-interactions number-of-series              
             monitors shared-configuration configurations 
             (output-dir (error "Please supply an :output-dir for monitoring")))
  "Runs multiple batches of series. Every batch takes a
different (named) configuration. Each configuration in configurations
should be a pair like (name . configuration-list). You can use
shared-configuration to set configuration values that are shared among
all batches. Values in configurations have precedence over values in
shared-configration, should there be a conflict. output-dir can be set
to the directory of (or subdir in) your experiment. If it is set then
all data-outputting monitors will be overridden to output there. For
each named configuration a subdir will be made there with the given
name."
  (loop for configuration in configurations
        unless (and (listp configuration) 
                    (symbolp (first configuration)) 
                    (listp (second configuration)))
        do (error "Configurations should be a list of pairs like (name configuration-list)")
        do 
        ;; merge shared-configuration and current configuration
        (setf (second configuration)
              (loop with local-config = (make-configuration :entries (second configuration)) 
                    for (key . value) in shared-configuration
                    do (set-configuration local-config key value :replace nil)
                    finally (return (entries local-config))))
       
        ;; adapt file-writing monitors so they output in the correct output-dir
       (monitors::deactivate-all-monitors)
        (loop for monitor-string in monitors
              for monitor = (monitors::get-monitor (read-from-string monitor-string))
              when (slot-exists-p monitor 'file-name)
              do (setf (slot-value monitor 'file-name)
                       (ensure-directories-exist
                        (merge-pathnames (make-pathname :directory 
                                                        `(:relative ,(string-downcase (symbol-name (first configuration))))
                                                        :name (pathname-name (file-name monitor)) 
                                                        :type (pathname-type (file-name monitor)))
                                         output-dir)))
             (monitors::activate-monitor-method (read-from-string monitor-string)))
       
        ;; run the actual batch for the current configuration
         (run-batch experiment-class number-of-interactions number-of-series
                    :configuration (make-configuration :entries (second configuration)))))


;; ----------------------------------------------------------------------------
;; print-object

(defmethod print-object ((experiment experiment) stream)
  (if *print-pretty*
    (pprint-logical-block (stream nil)
      (format stream "<~(~:w~):~:_ interaction-number: ~a,~:_ ~
interacting-agents: ~a,~:_ population: ~a,~:_ configuration: ~a,~:_ world: ~a,~:_ "
              (type-of experiment)
              (when (current-interaction experiment)
                (interaction-number (current-interaction experiment)))
              (when (current-interaction experiment)
                (let ((*print-pretty* nil))
                  (loop for agent in (interacting-agents
                                      (current-interaction experiment))
                        collect (format nil "~a" agent))))
              (let ((*print-pretty* nil))
                (loop for agent in (population experiment)
                      collect (format nil "~a" agent)))
              (configuration experiment)
              (world experiment))
      (call-next-method) (format stream ">"))
    (format stream "<~(~:w~): interaction-number: ~a>"
            (type-of experiment) (when (current-interaction experiment)
                                   (interaction-number (current-interaction experiment))))))
