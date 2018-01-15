(defpackage :naming-game
  (:use :common-lisp :utils :monitors :web-interface :plot-raw-data
        :experiment-framework))

(in-package :naming-game)

;; ------------------------------
;; Main definitions and interface
;; ------------------------------

(defclass ng-experiment (experiment)
  ()
  (:documentation "The experiment class for a basic Naming Game. It
  only relies on experiment and does NOT use tasks-and-processes,
  meta-layer-learning or the action-behaviour-framework."))

(defclass ng-agent (agent)
  ((lexicon 
    :documentation "The word meaning mappings of the agent"
    :type list :initform nil :accessor lexicon)
   (context
    :documentation "The context is a subset of world which contains
    some objects."
    :type list :initform nil :accessor context)
   (topic
    :documentation "An element of the context that was either selected
      for production or that was interpreted."
    :initform nil :accessor topic)
   (applied-lex 
    :documentation "The name that was used. This is reset at the
    beginning of each game."
    :type (or null ng-lex) :initform nil :accessor applied-lex))
  (:documentation "A very basic naming-game agent"))

(define-configuration-default-value :total-nr-of-objects 5)

(define-configuration-default-value :population-size 50)

(define-configuration-default-value :alignment-strategy :frequency) 
; can take values:
;; :no-alignment, :frequency, :minimal-NG, :lateral-inhibition, :lateral-inhibition-2, :imitation

(define-configuration-default-value :who-aligns? :both) 
; can take values:
;; :both, :speaker or :hearer

(defmethod initialize-instance :after ((experiment ng-experiment) &key)
  (setf (world experiment) 
	(loop for i from 1 to (get-configuration experiment :total-nr-of-objects)
              collect (make-id "obj")))

  (setf (population experiment)
	(loop for i from 1 to (get-configuration experiment :population-size)
              for agent = (make-instance 'ng-agent :id i
                                         :experiment experiment
                                         :world (world experiment))
              collect agent)))

;; -----------------------------------------------
;; production and parsing + invention and adoption
;; -----------------------------------------------

(defclass ng-lex ()
  ((meaning 
    :initform nil :initarg :meaning :accessor meaning :type symbol)
   (form 
    :initform "" :initarg :form :accessor form :type string)
   (score 
    :initform 0.5 :accessor score :initarg :score :type number)))

(defmethod print-object ((lex ng-lex) stream)
  (pprint-logical-block (stream nil)
    (format stream "~%~a <-(~a)-> ~a" 
            (meaning lex) (score lex) (form lex))))

(define-event lex-added (lex ng-lex))

(defun add-lex (lex agent)
  (push lex (lexicon agent))
  (notify lex-added lex)
  lex)

(defun produce (agent)
  (setf (applied-lex agent)
        (the-biggest #'score (find-all (topic agent) (lexicon agent) :key #'meaning)))
  (setf (utterance agent) (when (applied-lex agent) (form (applied-lex agent))))
  (utterance agent))

(define-configuration-default-value :initial-score 0.5)

(defun invent-name (agent)
  (add-lex (make-instance 'ng-lex :form (make-new-word) :meaning (topic agent)
                          :score (get-configuration agent :initial-score))
           agent))

(defmethod parse ((agent ng-agent))
  (setf (applied-lex agent) (find (utterance agent) (lexicon agent) :key #'form))
  (when (applied-lex agent)
    (meaning (applied-lex agent))))

(defun adopt-name (agent)
  (let ((lex (make-instance 'ng-lex :form (utterance agent) :meaning (topic agent)
                            :score (get-configuration agent :initial-score))))
    (add-lex lex agent)
    (setf (applied-lex agent) lex)))

(defmethod initialize-agent ((agent ng-agent) &key context)
  (setf (applied-lex agent) nil)
  (setf (communicated-successfully agent) t)
  (setf (context agent) context)
  (setf (topic agent) (first context)))

(defmethod interact ((experiment ng-experiment) interaction &key)
  (declare (ignore interaction))
  (let ((speaker (speaker experiment))
        (hearer (hearer experiment))
        (context (random-subset (world experiment) :include-empty-set? nil)))
    (initialize-agent speaker :context context)
    (initialize-agent hearer :context context)
    (unless (produce speaker)
      (invent-name speaker) 
      (produce speaker))
    (setf (utterance hearer) (utterance speaker))
    (unless (parse hearer)
      (adopt-name hearer)
      (setf (communicated-successfully hearer) nil)
      (setf (communicated-successfully speaker) nil)))
  (finish-interaction experiment))


;; ---------------------------------------------------
;;                      ALIGNMENT
;; ---------------------------------------------------

;; convenience methods for alignment

(defun inc-score (lex &optional (delta 0.1) (upper-bound 1.0))
  (incf (score lex) delta)
  (when (> (score lex) upper-bound)
    (setf (score lex) upper-bound))
  lex)

(defun dec-score (lex agent &key (delta 0.1) (lower-bound 0.0) 
                              (remove-on-lower-bound t))
  (decf (score lex) delta)
  (when (<= (score lex) lower-bound)
    (if remove-on-lower-bound
        (setf (lexicon agent) (remove lex (lexicon agent)))
        (setf (score lex) lower-bound)))
  (lexicon agent))

(defun get-form-competitors (agent lex)
  "Retrieves all the lexical constructions that have the same form as
the given lex. (Does not include the given lex itself)"
  (remove lex (find-all (meaning lex) (lexicon agent) :key #'meaning)))

(defun get-meaning-competitors (agent lex)
  "Retrieves all the lexical constructions that have the same form as
the given lex. (Does not include the given lex itself)"
  (remove lex (find-all (form lex) (lexicon agent) :key #'form)))

(defmethod finish-interaction ((experiment ng-experiment) &key)
  (loop for agent in (interacting-agents experiment)
     do (align-agent agent (get-configuration agent :alignment-strategy))))

(defgeneric align-agent (agent strategy)
  (:documentation "Can implement different alignment strategies
  depending on the mode. Is called in finish-interaction for every
  participating agent."))

(defmethod align-agent :around (agent strategy)
  (declare (ignore strategy))
  (case (get-configuration agent :who-aligns?)
    (:both (call-next-method))
    (:speaker (when (equal (discourse-role agent) 'speaker) (call-next-method)))
    (:hearer (when (equal (discourse-role agent) 'hearer) (call-next-method)))))

(defmethod align-agent ((agent ng-agent) (strategy (eql :no-alignment)))
  ;; do nothing
  )

(defmethod align-agent ((agent ng-agent) (strategy (eql :imitation)))
  (loop for competitor in (get-form-competitors agent (applied-lex agent))
     do (setf (lexicon agent) (remove competitor (lexicon agent)))))

(defmethod align-agent ((agent ng-agent) (strategy (eql :minimal-NG)))
  (when (communicated-successfully agent)
    (loop for competitor in (get-form-competitors agent (applied-lex agent))
       do (setf (lexicon agent) (remove competitor (lexicon agent))))))

(defmethod align-agent ((agent ng-agent) (strategy (eql :frequency)))
  (when (communicated-successfully agent)
    (incf (score (applied-lex agent)))))
   
(define-configuration-default-value :li-incf-score 0.1)
(define-configuration-default-value :li-decf-score 0.2)
(define-configuration-default-value :li-inh-score 0.2)

(defmethod align-agent ((agent ng-agent)  (strategy (eql :lateral-inhibition)))
  (if (communicated-successfully agent)
    (progn (inc-score (applied-lex agent) (get-configuration agent :li-incf-score))
	   (loop for competitor in (get-form-competitors agent (applied-lex agent))
	      do (dec-score competitor agent 
                            :delta (get-configuration agent :li-decf-score))))
    (when (equal (discourse-role agent) 'speaker)
      (dec-score (applied-lex agent) agent :delta (get-configuration agent :li-decf-score)))))

(defmethod align-agent ((agent ng-agent) (strategy (eql :lateral-inhibition-2)))
  (if (communicated-successfully agent)
      (progn
	(setf (score (applied-lex agent)) (+ (* (score (applied-lex agent)) 
						(- 1 (get-configuration agent :li-incf-score)))
                                             (get-configuration agent :li-incf-score)))
	(loop for competitor in (get-form-competitors agent (applied-lex agent))
	   do (dec-score competitor agent 
                         :delta (* (score competitor) 
                                   (- 1 (get-configuration agent :li-decf-score))))))
      (when (equal (discourse-role agent) 'speaker)
        (dec-score (applied-lex agent) agent 
                   :delta (* (score (applied-lex agent)) 
                             (- 1 (get-configuration agent :li-decf-score)))))))

(defmethod align-agent ((agent ng-agent) (strategy (eql :th-lateral-inhibition)))
  (if (communicated-successfully agent)
      (progn
	(setf (score (applied-lex agent)) (+ (* (score (applied-lex agent)) 
						(- 1 (get-configuration agent :li-incf-score)))
                                             (get-configuration agent :li-incf-score)))
        (if (equal (discourse-role agent) 'speaker)
          (loop for competitor in (get-form-competitors agent (applied-lex agent))
                do (dec-score competitor agent 
                              :delta (* (score competitor) 
                                        (- 1 (get-configuration agent :li-decf-score)))))

          (loop for competitor in (get-meaning-competitors agent (applied-lex agent))
                do (dec-score competitor agent 
                              :delta (* (score competitor) 
                                        (- 1 (get-configuration agent :li-decf-score)))))))
      (when (equal (discourse-role agent) 'speaker)
        (dec-score (applied-lex agent) agent 
                   :delta (* (score (applied-lex agent)) 
                             (- 1 (get-configuration agent :li-decf-score)))))))
