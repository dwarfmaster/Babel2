
(in-package :action-behavior-framework)

;; ############################################################################

(defclass test-experiment (action-experiment)
  ())

(defmethod determine-interacting-agents ((experiment test-experiment)
                                         (interaction interaction)
                                         mode
                                         &key &allow-other-keys)
  (declare (ignore mode))
  (setf (interacting-agents interaction)
        (subseq (agents experiment) 0 2)))

;; ----------------------------------------------------------------------------

(defclass action-test-agent (action-agent)
  ((consolidated :accessor consolidated :initform 0)
   (actions :accessor actions :initform nil)))

(defmethod act ((agent action-test-agent)(world t)(action (eql nil)))
  (push (make-instance 'speak-action) (actions agent))
  (first (actions agent)))

(defmethod act ((agent action-test-agent)(world t)(action speak-action))
  (push (make-instance 'point-action) (actions agent))
  (first (actions agent)))

(defmethod act ((agent action-test-agent)(world t)(action point-action))
  (push (make-instance 'no-action) (actions agent))
  (first (actions agent)))

(defmethod act ((agent action-test-agent) world action)
  "default implementation returns no-action"
  (declare (ignore world action))
  (push (make-instance 'no-action) (actions agent))
  (first (actions agent)))

(defmethod consolidate-agent ((agent action-test-agent)
                              world)
  (declare (ignore world))
  (incf (consolidated agent)))

;; ----------------------------------------------------------------------------
 
(deftest test-action-behavior-framework ()
  (let* ((agent-1 (make-instance 'action-test-agent))
         (agent-2 (make-instance 'action-test-agent))
         (agent-3 (make-instance 'action-test-agent))
         (experiment (make-instance 'test-experiment
                                    :agents (list agent-1 agent-2
                                                  agent-3)))
         (interaction (run-interaction experiment)))

    (test-equal interaction (current-interaction experiment))
    
    ;; expect speak and then no-action
    (loop for action in (actions (first (interacting-agents 
                                         interaction)))
          for expected-type in '(no-action speak-action)
          do (test-assert (typep action expected-type)))
    ;; expect point and then no-action
    (loop for action in (actions (second (interacting-agents 
                                          (current-interaction experiment))))
          for expected-type in '(no-action point-action)
          do (test-assert (typep action expected-type)))
    ;; expect speak, point, no and then no-action 
    (loop for action in (actions (world experiment))
          for expected-type in '(no-action no-action point-action speak-action)
          do (test-assert (typep action expected-type)))
    ;; expect no action in agent-3
    (test-assert (length= 0 (actions agent-3)))
    ;; expect all interacting agents to be consolidated once
    (test-assert
     (loop for a in (interacting-agents 
                     (current-interaction experiment))
           always (= (consolidated a) 1)))
    ;; everybody else no
    (test-assert (= 0 (consolidated agent-3)))))

;; (test-action-behavior-framework)

;; ############################################################################

(defclass test-diagnostic (diagnostic)
  ((trigger :initform 'test)))

(defmethod diagnose ((diagnostic test-diagnostic)
                     obj &key &allow-other-keys)
  (unless (problems obj)
    (make-instance 'problem)))

(defclass test-repair (repair)
  ((trigger :initform 'test)))

(defmethod repair ((repair test-repair)
                   (problem problem)
                   obj &key &allow-other-keys)
  (declare (ignore problem obj))
  (make-instance 'fix :restart-data t))

(defclass action-test-agent-learning (action-test-agent object-w-learning)
  ((action-nil-runs :accessor action-nil-runs :initform 0)))

(defmethod act ((agent action-test-agent-learning)
                (world t)
                (action (eql nil)))
  (incf (action-nil-runs agent))
  (let ((action (call-next-method)))
    (notify-learning agent :trigger 'test)
    action))

(deftest test-action-behavior-framework-learning ()
  (let* ((agent-1 (make-instance 'action-test-agent-learning))
         (agent-2 (make-instance 'action-test-agent-learning))
         (agent-3 (make-instance 'action-test-agent-learning))
         (experiment (make-instance 'test-experiment
                                    :agents (list agent-1 agent-2
                                                  agent-3))))

    (run-interaction experiment)
    (test-assert (loop for a in  (agents experiment)
                       for r in '(1 0 0)
                       always (= r (action-nil-runs a)))))

  (let* ((agent-1 (make-instance 'action-test-agent-learning
                                 :diagnostics (list (make-instance 'test-diagnostic))
                                 :repairs (list (make-instance 'test-repair))))
         (agent-2 (make-instance 'action-test-agent-learning))
         (agent-3 (make-instance 'action-test-agent-learning))
         (experiment (make-instance 'test-experiment
                                    :agents (list agent-1 agent-2
                                                  agent-3))))

    (run-interaction experiment)
    (test-assert (loop for a in  (agents experiment)
                       for r in '(2 0 0)
                       always (= r (action-nil-runs a))))))

;; (test-action-behavior-framework-learning)
