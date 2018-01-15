
(in-package :guessing-game)

(defmethod determine-interacting-agents ((experiment experiment)
                                         interaction
                                         (mode (eql 'fixed))
                                         &key &allow-other-keys)
  "This default implementation randomly chooses two interacting agents
   and adds the discourse roles speaker and hearer to them"
  (with-slots (interacting-agents agents) experiment
    (assert (length= 2 agents))
    (setf (interacting-agents interaction) agents)
    (loop for a in (interacting-agents interaction)
          for d in '(speaker hearer)
          do (setf (discourse-role a) d))
    (notify interacting-agents-determined experiment interaction)))

(deftest production/interpretation ()
  (let ((experiment (make-instance 'minimal-gg-experiment
                                   :configuration '((:total-nr-of-objects . 10)
                                                    (:context-size . 10)
                                                    (:population-size . 2)))))
    (loop
     with lexicon = (loop for o in (objects (world experiment))
                          collect (list (symbol-name o) 1.0 o))
     for a in (population experiment)
     do (setf (lexicon a)  lexicon))
    (loop repeat 100
          do
          (run-interaction experiment)
          (test-assert (communicated-successfully (speaker experiment)))
          (test-assert (communicated-successfully (hearer experiment)))
          (setf (communicated-successfully (speaker experiment)) nil)
          (setf (communicated-successfully (hearer experiment)) nil))
    experiment))

;; (production/interpretation)

(deftest learning ()
  (loop
   repeat 10
   do
   (let ((experiment (make-instance
                      'minimal-gg-experiment
                      :configuration '((:total-nr-of-objects . 1)
                                       (:context-size . 1)
                                       (:population-size . 2)
                                       (:determine-interacting-agents-mode . fixed)
                                       (:diagnostics . ())
                                       (:repairs . ())))))
     (test-equal (length (agents experiment)) 2)
     ;; first agent is tutor
     (setf (lexicon (first (agents experiment)))
           (loop for o in (objects (world experiment))
                 collect (list (symbol-name o) 1.0 o)))
     ;; second agent is learning
     (add-diagnostic (second (agents experiment))
                     'interpretation-diagnostic)
     (add-repair (second (agents experiment))
                 'adopt-word)

     ;; learn
     (run-interaction experiment)
     (test-assert
      (and 
       (eq (communicated-successfully (speaker experiment))
           (communicated-successfully (hearer experiment)))
       (null (communicated-successfully (speaker experiment)))))
     (test-assert (= 1 (length (lexicon (speaker experiment)))
                     (length (lexicon (hearer experiment)))))

     ;; test 1
     (run-interaction experiment)
     (test-assert
      (and 
       (communicated-successfully (speaker experiment))
       (communicated-successfully (hearer experiment))))
     (test-assert (= 1 (length (lexicon (speaker experiment)))
                     (length (lexicon (hearer experiment)))))
     experiment)))

;; (learning)

(deftest evolution ()
  (loop
   repeat 10
   do
   (let ((experiment (make-instance
                      'minimal-gg-experiment
                      :configuration '((:total-nr-of-objects . 1)
                                       (:context-size . 1)
                                       (:population-size . 2)
                                       (:determine-interacting-agents-mode . fixed)))))
     (test-equal (length (agents experiment)) 2)
     ;; first agent is inventor
     (add-repair (first (agents experiment))
                 'invent-word)
     ;; second agent is learning
     (add-diagnostic (second (agents experiment))
                     'interpretation-diagnostic)
     (add-repair (second (agents experiment))
                 'adopt-word)
     (run-interaction experiment)
     (test-assert
      (and 
       (eq (communicated-successfully (speaker experiment))
           (communicated-successfully (hearer experiment)))
       (null (communicated-successfully (speaker experiment)))))
     
     (run-interaction experiment)
     (test-assert
      (and 
       (communicated-successfully (speaker experiment))
       (communicated-successfully (hearer experiment))))
     experiment)))

;; (evolution)
