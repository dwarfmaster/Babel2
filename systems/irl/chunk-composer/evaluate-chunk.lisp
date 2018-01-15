
(in-package :irl)

;; ############################################################################
;; evaluate-chunk
;; ----------------------------------------------------------------------------

(export '(target-entity bind-statements bindings evaluate-chunk
          simple-evaluation-result-scoring chunk-evaluation-result))


(defclass chunk-evaluation-result ()
  ((score 
    :type float :initarg :score :initform 0.0 :accessor score
    :documentation "A score that is used to rank results (higher == better)")
   (chunk 
    :type chunk :initarg :chunk :accessor chunk
    :documentation "The evaluated chunk.")
   (evaluation-tree 
    :type irl-program-evaluation-node :initarg :evaluation-tree
    :accessor evaluation-tree
    :documentation "The top node of the evaluation search tree")
   (target-entity
    :initarg :target-entity :accessor target-entity
    :documentation "The value of the binding that
                     corresponds to the target var")
   (bind-statements 
    :type list :initarg :bind-statements :accessor bind-statements
    :documentation "IRL representations of those bindings
                     that are target or source variables of the chunk
                     and that are not given in the wrapped chunk")
   (bindings 
    :type list :initarg :bindings :accessor bindings
    :documentation "A list of bindings (one solution of the
                    irl program)"))
  (:documentation "Represents the one result of a chunk evaluation"))

(defmacro sorted-list-insert (item list &key (predicate '<) (key 'identity))
  "inserts item into list ordered by predicate"
  `(if ,list
       (loop for x on ,list
          do (cond ((,predicate (,key ,item) (,key (car x)))
                    (setf (cdr x) (cons (car x) (cdr x)))
                    (setf (car x) ,item)
                    (return ,list))
                   ((not (cdr x))
                    (setf (cdr x) (cons ,item nil))
                    (return ,list))))
       (setf ,list (list ,item))))

(defmacro insert-evaluation-result (result list)
  "inserts an evaluation result into a list, sorted by score"
  `(sorted-list-insert ,result ,list :predicate > :key score))

(defun evaluate-chunk (chunk
                       ontology
                       &key
                       configuration
                       (evaluation-result-scoring-fn
                        #'simple-evaluation-result-scoring)
                       (chunk-evaluation-result-class
                        'chunk-evaluation-result)
                       (notify t))
  (declare (type chunk chunk))
  (let* ((monitors::*monitor-notifications-disabled* (not notify))
         (irl-program (expand-chunks (irl-program chunk) ontology))
         (chunk-evaluation-results nil))
    (multiple-value-bind (evaluation-results evaluation-tree)
        (evaluate-irl-program irl-program ontology
                              :configuration configuration)
      (loop
       with bound-variable-ids-in-irl-program 
       = (mapcar #'third (find-all 'bind (irl-program chunk) :key #'car))
       with chunk-variable-ids 
       = (mapcar #'car (cons (target-var chunk) (open-vars chunk)))
       with newly-bound-variable-ids
       = (set-difference chunk-variable-ids bound-variable-ids-in-irl-program)
       for evaluation-result in evaluation-results
       for bind-statements = (loop for b in evaluation-result
                                   when (and
                                         (find (var b) newly-bound-variable-ids)
                                         (not (eq (var b) (car (target-var chunk)))))
                                   collect (list 'bind (type-of (value b))
                                                 (var b) 
                                                 (id (value b))))
       for target-entity = (value (find (car (target-var chunk)) evaluation-result 
                                        :key #'var))
       for chunk-evaluation-result = (make-instance chunk-evaluation-result-class
                                                    :chunk chunk
                                                    :evaluation-tree evaluation-tree
                                                    :target-entity target-entity
                                                    :bind-statements bind-statements
                                                    :bindings evaluation-result)
       do (setf (score chunk-evaluation-result)
                (funcall evaluation-result-scoring-fn
                         chunk-evaluation-result
                         ontology))
       (insert-evaluation-result chunk-evaluation-result chunk-evaluation-results))
      (values chunk-evaluation-results
              evaluation-tree ;; the evaluation-tree (there is only one anyway for the moment)
              ))))
  
(defun simple-evaluation-result-scoring (evaluation-result ontology)
  "A default for evaluation-result-scoring-fn"
  (declare (ignorable ontology))
  (average
   (list
    ;; score of the chunk
    (score (chunk evaluation-result))
    ;; multiply all scores, the higher the better
    (apply #'* (mapcar #'score (bindings evaluation-result)))
    (let ((primitive-ids (mapcar #'car (irl-program (chunk evaluation-result)))))
      ;; when there are two times the same primitive in the program,
      ;; use 0.5, three times 0.33, etc.
      (/ 1 (1+ (- (length primitive-ids) 
                  (length (remove-duplicates primitive-ids)))))))))

(defmethod copy-object ((e chunk-evaluation-result))
  e)

(defmethod print-object ((result chunk-evaluation-result) stream)
  (format stream "<~{~(~a~)~^, ~} (~,2f)>" 
          (mapcar #'fourth (bind-statements result)) (score result)))




