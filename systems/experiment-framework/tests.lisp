
(in-package :experiment-framework)

;; ----------------------------------------------------------------------------

(defclass test-agent (agent)
  ((initialize-run
    :initform 0
    :accessor initialize-run)
   (run-agent-run
    :initform 0
    :accessor run-agent-run)))

;;;; ;; ----------------------------------------------------------------------------
(defclass test-experiment (experiment)
  ((initialize-run
    :initform 0
    :accessor initialize-run)
   (determine-interacting-agents-run
    :initform 0
    :accessor determine-interacting-agents-run)
   (interact-run
    :initform 0
    :accessor interact-run)))

(defmethod initialize-instance :after ((experiment test-experiment)
                                       &key &allow-other-keys)
  (setf (agents experiment)
        (loop for i from 1 to (get-configuration experiment 'number-of-agents)
              collect (make-instance 'test-agent))))
 
(defmethod determine-interacting-agents
           ((experiment test-experiment)
            interaction
            mode &key &allow-other-keys)
  (declare (ignore mode))
  (setf (interacting-agents interaction)
        (list (first (agents experiment))
              (second (agents experiment))))
  (incf (determine-interacting-agents-run experiment)))

(defmethod interact
           ((experiment test-experiment)
            (interaction interaction) &key &allow-other-keys)
  (incf (interact-run experiment))
  (loop for a in (interacting-agents interaction)
        do (incf (run-agent-run a))))

;;;; ;; ----------------------------------------------------------------------------

(deftest test-experiment-framework ()
  (let* ((experiment (make-instance
                      'test-experiment
                      :configuration '((number-of-agents . 12)))))
    (with-slots (agents configuration) experiment
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; test initialization
      ;; 1 configuration
      (test-assert (length= 1 configuration))
      ;; 12 agents
      (test-assert (length= 12 agents))
      (test-assert (loop for a in agents
                         always (typep a 'test-agent)))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; test run-interaction
      ;; we set interacting-agents to the first two
      (test-assert (length= 0 (interactions experiment)))

      (multiple-value-bind (i e)
          (run-interaction experiment)
        (test-assert (typep i 'interaction))
        (test-equal (current-interaction experiment)
                    i
                    (test-assert (eq e experiment))))
      
      (test-assert (length= 1 (interactions experiment)))

      ;; test what was run on experiment
      (test-assert (= 1 (determine-interacting-agents-run experiment)
                      (interact-run experiment)))

      ;; test what was run
      (loop
       with interaction = (current-interaction experiment)
       for a in agents
       if (member a (interacting-agents interaction))
       do (test-assert (and (= 1  (run-agent-run a))
                            (= 0 (initialize-run a))))
       else
       do (test-assert (= 0 (run-agent-run a)
                          (initialize-run a))))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; test run-series (+ 1 run-interaction)

      (test-ok (run-series experiment 100))

      (test-assert (= 101 (interaction-number
                           (current-interaction experiment))))

      ;; test what was run on experiment
      (test-assert (= 101
                      (interact-run experiment)
                      (determine-interacting-agents-run experiment)))

      ;; test what was run
      (test-ok
       (loop
        for a in agents
        if (member a (interacting-agents (current-interaction
                                          experiment)))
        do (test-assert (= 101 (run-agent-run a)))
        else
        do (test-assert (= 0 (run-agent-run a)))))

      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; test run-series continue

      (test-ok (run-series experiment 100))

      (test-assert (= 201 (interaction-number
                           (current-interaction experiment))))

      ;; test what was run on experiment
      (test-assert (= 201
                      (determine-interacting-agents-run experiment)
                      (interact-run experiment)))

      ;; test what was run
      (test-ok
       (loop
        with interacting-agents = (interacting-agents
                                   (current-interaction experiment))
        for a in agents
        if (member a interacting-agents)
        do (test-assert (= 201 (run-agent-run a)))
        else
        do (test-assert (= 0 (run-agent-run a))))))))

;; (test-experiment-framework)

;; ----------------------------------------------------------------------------

(deftest test-determine-interacting-agents-default ()
  (let ((experiment (make-instance
                     'experiment
                     :agents (loop for i from 1 to 10
                                   collect (make-instance 'agent))
                     :interactions (list (make-instance 'interaction)))))
    (test-assert (length= 0 (interacting-agents (car (interactions experiment)))))
    (test-ok 
     (determine-interacting-agents experiment (car (interactions experiment))
                                   t))
    (test-assert (length= 2 (interacting-agents (car (interactions experiment)))))
    (test-assert (loop for a in (interacting-agents (car (interactions experiment)))
                       for d in '(speaker hearer)
                       always (eq (discourse-role a) d)))))

;; (test-determine-interacting-agents-default)