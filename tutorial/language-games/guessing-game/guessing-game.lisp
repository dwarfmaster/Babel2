
(defpackage :guessing-game
  (:use :common-lisp
   :test-framework
   :utils
   :monitors
   :web-interface
   :tasks-and-processes
   :meta-layer-learning
   :experiment-framework))

(in-package :guessing-game)

;; ------------------------------
;; Main definitions and interface
;; ------------------------------

(defclass minimal-gg-experiment (experiment)
  ()
  (:documentation "The experiment class for a Minimal Guessing
  Game. This game introduces uncertainty about the intended referent
  object (a naming game without pointing and multi-object
  context). Meanings are atomic and only a single word is uttered per
  game. It relies on action-experiment which is part of the
  action-behavior-framework."))

(defclass minimal-gg-agent (agent object-w-tasks object-w-learning)
  ((lexicon 
    :documentation "The word meaning mappings of the agent. Can
    contain both synonyms and homonyms. Has the form (word score meaning)."
    :accessor lexicon
    :initform nil))
  (:documentation "A very basic guessing-game agent"))

(defclass minimal-gg-world ()
  ((context
    :documentation "The context is a subset of world which contains
    some objects."
    :type list :initform nil :accessor context)
   (objects :initarg :objects :initform nil :accessor objects))
  (:documentation "An action-world is for the
  action-behavior-framework."))

(define-configuration-default-value :total-nr-of-objects 6)

(define-configuration-default-value :context-size 3)

(define-configuration-default-value :population-size 10)

(define-configuration-default-value :diagnostics '(interpretation-diagnostic))

(define-configuration-default-value :repairs '(adopt-word invent-word))

(defmethod initialize-instance :after ((experiment minimal-gg-experiment) &key)
  (setf (world experiment)
        (make-instance 'minimal-gg-world 
                       :objects (loop for i from 1 to (get-configuration experiment :total-nr-of-objects)
                                      collect (make-id "obj"))))
  (setf (population experiment)
	(loop for i from 1 to (get-configuration experiment
                                                 :population-size)
              for agent = (make-instance 'minimal-gg-agent :id i
                                         :experiment experiment
                                         :world (world experiment))
              do
              (loop for diagnostic in (get-configuration experiment
                                                         :diagnostics)
                    do (add-diagnostic agent diagnostic))
              (loop for repair in (get-configuration experiment
                                                     :repairs)
                    do (add-repair agent repair))
              collect agent)))


;; -----------------------------
;; produce/parse
;; -----------------------------

(define-configuration-default-value :strategy 'minimal)

(defgeneric gg-produce (topic agent strategy))

(defmethod gg-produce (topic agent (strategy (eql 'minimal)))
  (let ((utterances (find-all topic
                              (lexicon agent)
                              :key #'third)))
    (first (random-elt utterances))))

(defgeneric gg-parse (utterance agent strategy))
  
(defmethod gg-parse (utterance agent (strategy (eql 'minimal)))
  (let ((topics (find-all utterance
                          (lexicon agent)
                          :key #'first
                          :key #'string-equal)))
    (assert (length< topics 2))
    (first topics)))

(defun parse/produce (item agent)
  (if (stringp item)
    (third
     (first (sort (find-all item
                            (lexicon agent)
                            :key #'first)
                  #'>
                  :key #'second)))
    (first
     (first (sort (find-all item
                            (lexicon agent)
                            :key #'third)
                  #'>
                  :key #'second)))))

;; -----------------------------
;; speaker
;; -----------------------------

(defclass production-task (task object-w-learning)
  ((processes :initform '(pick-topic produce))))

(defmethod restart-object ((task production-task) (restart-data process) &key)
  (restart-process task restart-data 'produce))

(defmethod initialize-instance :after ((task production-task) &key)
  (setf (diagnostics task) (diagnostics (owner task)))
  (setf (repairs task) (repairs (owner task))))

(defmethod run-process (process (process-label (eql 'pick-topic)) task agent)
  (declare (ignore task))
  (make-instance 'process-result 
                 :process process
                 :score 1.0
                 :data `((topic . ,(random-elt
                                    (context (world (experiment agent))))))))

(defmethod run-process (process (process-label (eql 'produce)) task agent)
  (let* ((utterance (parse/produce (get-data process 'topic)
                                   agent)))
    (if utterance
      (make-instance 'process-result 
                     :process process
                     :score 1.0
                     :data `((utterance . ,utterance)))
      (notify-learning task :trigger 'production-failed
                       :omit-diagnostics t
                       :process process))))

(define-configuration-default-value :initial-score 1.0)

(defclass invent-word (repair)
  ((trigger :initform 'production-failed)))

(defmethod repair ((repair invent-word) (problem (eql nil))
                   (task production-task) &key trigger process)
  (assert (eq trigger 'production-failed))
  (let ((topic (get-data process 'topic))
        (agent (owner task)))
    (setf (communicated-successfully agent) nil)
    (push (list (make-new-word)
                (get-configuration agent :initial-score)
                topic)
          (lexicon agent))
    (make-instance 'fix
                   :restart-data process)))

;; -------------------------
;; hearer
;; -------------------------

(defclass interpretation-task (task object-w-learning)
  ((processes :initform '(parse compare-to-context))))

(defmethod run-process (process (process-label (eql 'parse)) task agent)
  (declare (ignore task))
  (loop for entry in (sort (find-all (utterance agent) (lexicon agent) :key #'first) #'> :key #'second)
        for score = (second entry)
        for topic = (third entry)
        collect (make-instance 'process-result 
                               :process process
                               :score score
                               :data `((topic . ,topic)))))

(defmethod run-process (process (process-label (eql 'compare-to-context))
                                task agent)
  (declare (ignore task))
  (when (find (get-data (input process) 'topic)
              (context (world (experiment agent))) :test #'equal)
    (make-instance 'process-result 
                   :process process
                   :score (score (input process)))))

(defclass interpretation-diagnostic (diagnostic)
  ((trigger :initform 'after-parsing))
  (:documentation "Checks the interpretations for mismatches."))

(defmethod diagnose ((diagnostic interpretation-diagnostic) (agent minimal-gg-agent) &key trigger task)
  ;; create a problem when either 1. there were no parses at all or
  ;; 2. none of the parses was successful for interpretation in the
  ;; context
  (assert (equal trigger 'after-parsing))
  (when (not (results task))
    (make-instance 'interpretation-failed)))

(defclass interpretation-failed (problem)
  ())

(defclass adopt-word (repair)
  ((trigger :initform 'after-parsing)))

(defgeneric adopt-word (context agent strategy))

(defmethod adopt-word (word agent (strategy (eql 'minimal)))
  ;; remove existing associations
  (loop for assoc in (find-all word (lexicon agent)
                               :key #'first :test #'equalp)
        do (setf (lexicon agent) (remove assoc (lexicon agent))))
  (push (list word
              (get-configuration agent :initial-score)
              (random-elt (context (world (experiment agent)))))
        (lexicon agent))
  t)

(defmethod repair ((repair adopt-word) (problem interpretation-failed)
                   (agent minimal-gg-agent) &key trigger)
  (assert (equal trigger 'after-parsing))
  (setf (communicated-successfully agent) nil)
  (when (adopt-word (utterance agent) agent
                    (get-configuration agent :strategy))
    (make-instance 'fix)))

;; -------------------------
;; interact
;; -------------------------

(defmethod interact ((experiment minimal-gg-experiment)
                     interaction &key &allow-other-keys)
  (declare (ignore interaction))
  (setf (context (world experiment))
        (random-elts (copy-list (objects (world experiment)))
                     (get-configuration experiment :context-size)))
  (let ((speaker (speaker experiment))
        (hearer (hearer experiment))
        (utterance nil))

    (loop for agent in (list speaker hearer)
          do (setf (communicated-successfully agent) nil))
        
    ;; speaker
    (let* ((results (object-run-task speaker 'production-task))
           (final-result (first results)))
      (when final-result
        (setf utterance
              (get-data (data final-result) 'utterance))
        (setf (utterance speaker) utterance)))
    
    ;; hearer
    (when utterance
      (setf (utterance hearer) utterance)
      (let ((results (object-run-task hearer 'interpretation-task)))
        (notify-learning hearer :trigger 'after-parsing :task (first (tasks hearer)))
        (when results
          (loop for a in (list speaker hearer)
                do (setf (communicated-successfully a) t))))))) 
