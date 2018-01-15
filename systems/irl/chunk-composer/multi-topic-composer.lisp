(in-package :irl)

(export '(multi-topic-composer multi-topic-composer-node))

(defun check-dnf-topic (evaluation-result ontology topic)
  (with-disabled-monitors 
    (let* ((target-var-id (car (target-var (chunk evaluation-result))))
           ;; get a clean irl-program (i.e. the original program w/o the target bind-statement)
           (irl-program (remove target-var-id
                                (append
                                 (irl-program (chunk evaluation-result))
                                 (bind-statements evaluation-result)) :key #'third))
           ;; evaluate
           (solutions (evaluate-irl-program irl-program ontology))
           ;; get the target entities for the solutions
           (solution-targets
             (loop for solution in solutions
                   for target-binding = (find target-var-id solution :key #'var)
                   if target-binding
                   collect (value target-binding))))
      ;; see if they precisely match the intended topic
      (permutation-of? solution-targets topic :test #'equal-entity))))

(defclass multi-topic-composer (chunk-composer)
  ((topics :initarg :topics :accessor topics :initform nil)))

(defclass multi-topic-composer-node (chunk-composer-node)
  ((results-before-goal-test :initarg :results-before-goal-test :accessor results-before-goal-test :initform nil)))


(defmethod handle-node ((node multi-topic-composer-node)
                        (status (eql 'evaluate))
                        (composer multi-topic-composer))
  (setf (next-handler node) 'expand)
  (setf (results-before-goal-test node)
        (loop for chunk in (get-chunks-for-evaluation node)
              append (evaluate-chunk
                      chunk
                      (ontology composer)
                      :notify nil)))
  (let ((good-results
         (loop for result in (results-before-goal-test node)
               if (check-dnf-topic result (ontology composer) (topics composer))
               collect result)))
    (values good-results nil)))

(defmethod copy-object ((node multi-topic-composer-node))
  node)


