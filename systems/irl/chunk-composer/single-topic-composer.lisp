(in-package :irl)

(export '(single-topic-composer-node node))

(defclass single-topic-composer-node (chunk-composer-node)
  ((status 
    :type symbol :initarg :status :initform 'initial :accessor status
    :documentation "The current status the node, used to drive the search.")
   (status-history 
    :type list :initarg :status-history :initform nil :accessor status-history
    :documentation "A list of statuses the node was previously in")
   (wrapped-chunks 
    :type list :initarg :wrapped-chunks :initform nil 
    :accessor wrapped-chunks
    :documentation "The chunks created by the chunk-wrapper-fn")
   (bad-evaluation-results
    :type list :initarg :bad-evaluation-results :initform nil 
    :accessor bad-evaluation-results
    :documentation "A list of results of program revision that were
                     rejected by check-evaluation-result-fn")
   (check-evaluation-result-data 
    :type list :initarg :check-evaluation-result-data :initform nil 
    :accessor check-evaluation-result-data
    :documentation "Whatever was passed as a second value by
                     check-evaluation-result-fn, used for
                     visualization.")
   (rating 
    :type float :initarg :rating :initform 1.0 :accessor rating
    :documentation "For enqueuing the node (lower == better")
   (equivalent-node 
    :type (or null chunk-composer-node) :initarg :equivalent-node :initform nil
    :accessor equivalent-node 
    :documentation "When node is a duplicate of another node, then
                     this is the other node")))

(defun change-status (node new-status)
  "Changes status to new-status and keeps the old status in status-history"
  (push (status node) (status-history node))
  (setf (status node) new-status))

   
;; ############################################################################
;; chunk composer
;; ----------------------------------------------------------------------------
(export '(single-topic-composer expand-chunk-fns
                                topic
                                check-chunk-fns
                                node-rating-fn
                                initial-chunk-score-fn
                                chunk-wrapper-fn
                                check-evaluation-result-fn
                                evaluation-result-scoring-fn))

(defclass single-topic-composer (chunk-composer)
 ((node-class :initform 'single-topic-composer-node)
  (expand-chunk-fns
    :type list :initarg :expand-chunk-fns :reader expand-chunk-fns
    :initform (list #'expand-chunk-combine-program)
    :documentation "A list of functions that are called on the chunk
                    of each search node to create new chunks. Takes a
                    node and the list of all chunks. Returns a list
                    of new nodes")
  (check-chunk-fns
    :type list :initarg :check-chunk-fns :reader check-chunk-fns
    :initform (list #'identity)
    :documentation "A list of functions that is called whenever a new
                     chunk is created to filter out bad chunks. Each
                     function takes a chunk and returns nil when the
                     chunk should be excluded from further
                     processing.")
   (node-rating-fn 
    :type (or symbol function) :initarg :node-rating-fn :reader node-rating-fn
    :initform #'simple-node-rating
    :documentation "A function that is called whenever a new node is
                     created to rate the node. Returns a float (lower
                     == better). Takes a node and the depth of the node
                     in the tree")
   (initial-chunk-score-fn
    :type (or symbol function) :initarg
    :initial-chunk-score-fn :reader initial-chunk-score-fn
    :initform #'simple-initial-chunk-score-fn
    :documentation "A function that is called whenever a new chunk is
                     created to compute the score of the chunk.
                     Takes the chunk, the node of the chunk and
                     the depth of the node.
                     Returns a float between 0.0 and 1.0 (very good)")
   (chunk-wrapper-fn
    :type (or symbol function) :initarg :chunk-wrapper-fn :reader chunk-wrapper-fn
    :initform #'identity
    :documentation "A function that is called before chunk evaluation
                     to add things to the irl program. Gets the chunk
                     and returns a new chunk or nil when the wrapping
                     failed.")
   (check-evaluation-result-fn
    :type (or symbol function) :initarg :check-evaluation-result-fn 
    :reader check-evaluation-result-fn :initform #'list
    :documentation "A function that is called after chunk
                     evaluation. Returns t when the result is a good
                     result and as a second value some debug data
                     that will be stored in the node.
                     Takes an evaluation result, the ontology and the composer.")
   (evaluation-result-scoring-fn 
    :type (or symbol function) :initarg :evaluation-result-scoring-fn 
    :reader evaluation-result-scoring-fn :initform #'simple-evaluation-result-scoring
    :documentation "A function that is called after chunk evaluation
                     to compute a score for a result (higher == better).
                     Gets evaluation-result and ontology")
   (topic :initarg :topic :accessor topic :initform nil)
   (hashed-nodes 
    :type hash-table :accessor hashed-nodes :initform (make-hash-table :size 128)
    :documentation "All nodes are also stored in this hash-table for
                     faster duplicate detection. Hash keys are computed
                     from the irl programs of the chunks with #'chunk->hash-key")))

(defmethod initialize-instance 
           :around ((composer single-topic-composer) &key)
  (call-next-method)
  (when (top-node composer)
    (setf (next-handler (top-node composer)) 'expand)))

;; ############################################################################
;; duplicate detection
;; ----------------------------------------------------------------------------

(defgeneric chunk->hash-key (chunk-or-node)
  (:documentation "Computes an isomorphism invariant hash, so that all
                    chunks that are equivalent have the same hash
                    key. But not all chunks with the same hash are
                    equivalent."))

(defmethod chunk->hash-key ((chunk chunk))
  (loop for pred in (irl-program chunk)
        sum (sxhash (first pred))))

(defmethod chunk->hash-key ((node single-topic-composer-node))
  (chunk->hash-key (chunk node)))

(defmethod equivalent-irl-programs? ((chunk-1 chunk) (chunk-2 chunk))
  (equivalent-irl-programs? (irl-program chunk-1) (irl-program chunk-2)))

(defun find-node-with-equivalent-chunk (chunk composer)
  (declare (type chunk chunk)
           (type chunk-composer composer))
  (loop for node in (gethash (chunk->hash-key chunk) (hashed-nodes composer))
        when (equivalent-irl-programs? chunk (chunk node))
        do (return node)))

;; ############################################################################
;; functions for expanding chunk composer nodes
;; ----------------------------------------------------------------------------

;; functions that can go into the :expand-chunk-fns slot of the
;; composer. Each of these functions takes a chunk composer node and a
;; list of other chunks and returns a list of new chunk composer nodes
;; or nil

(export '(expand-chunk-combine-program
          expand-chunk-combine-call-pattern
          expand-chunk-recombine-open-variables
          expand-chunk-link-open-variables))

(defun expand-chunk-combine-program (chunk composer)
  "Adds the network of another chunk when the target variable of that
   chunk is compatble with an open variable"
  (loop for other-chunk in (chunks composer)
        append (loop for new-chunk in (combine-chunk-program chunk other-chunk)
                     collect (cons new-chunk (list other-chunk)))))

(defun expand-chunk-combine-call-pattern (chunk composer)
  "Adds the call pattern of another chunk when the target variable of
   that chunk is compatble with an open variable"
  (loop for other-chunk in (chunks composer)
        append (loop for new-chunk in (combine-chunk-call-pattern chunk other-chunk)
                     collect (cons new-chunk (list other-chunk)))))

(defun expand-chunk-recombine-open-variables (chunk composer)
  "Tries to account for open variables of 'chunk' by using primitives
   that are already in the irl program. Returns all possible
   solutions."
  (declare (ignore composer))
  (loop for new-chunk in (recombine-open-variables chunk)
        collect (cons new-chunk nil)))
       
(defun expand-chunk-link-open-variables (chunk composer)
  "Tries to make two open variables of compatible type equal and when
   it does, reduces the number of open variables of the chunk by one."
  (declare (ignore composer))
  (loop for new-chunk in (link-open-variables chunk)
        collect (cons new-chunk nil)))

;; ############################################################################
;; create-meaning
;; ----------------------------------------------------------------------------

(export 'create-meaning)

(defgeneric create-meaning (thing &key &allow-other-keys))

(defmethod create-meaning ((chunk-evaluation-result chunk-evaluation-result)
                           &key all &allow-other-keys)
  "Appends the irl-program of the chunk and the bind statements"
  (declare (type chunk-evaluation-result chunk-evaluation-result))
  (append
   ;; get irl program and replace all entities with symbols
   (loop
    with irl-program = (if all
                         (irl-program (chunk chunk-evaluation-result))
                         (irl-program (chunk (node chunk-evaluation-result))))
    for s in irl-program
    if (and (eq (first s) 'bind)
            (typep (fourth s) 'entity))
    collect `(bind ,(second s) ,(third s), (id (fourth s)))
    else
    collect s)
    ;; add bind statements from evaluation
    (append 
     (bind-statements chunk-evaluation-result))))

;; ############################################################################
;; defaults and examples of node-rating-fn, check-chunk-fns and
;; check-evaluation-result-fn
;; ----------------------------------------------------------------------------
(export '(simple-node-rating simple-initial-chunk-score-fn no-primitive-occurs-more-than-once
                             no-primitive-occurs-more-than-twice-p
                             contains-no-open-vars))

(defun simple-node-rating (node node-depth)
  "simple default function for rating nodes in the search tree"
  (let ((chunk (chunk node)))
    (/ (+ node-depth                   ;; the less depth the better
          (length (open-vars chunk))   ;; less open vars are better
          (length (irl-program chunk)) ;; less primitives are better
          ;; less duplicate primitives are better (bind statements are not considered)
          (* 5 (-
                (length (remove 'bind (irl-program chunk) :key #'car)) 
                (length (remove-duplicates
                         (remove 'bind
                                 (mapcar #'car (irl-program chunk))))))))
       ;; the higher the score the better
       (score chunk))))

(defun simple-initial-chunk-score-fn (chunk node node-depth)
   "simple default function for computing the score of a new chunk"
   (declare (ignorable chunk) (ignorable node) (ignorable node-depth))
   (average (mapcar #'score (source-chunks node))))

(export '(no-primitive-occurs-more-than-twice-p no-primitive-occurs-more-than-once))

(defun no-primitive-occurs-more-than-twice-p (chunk)
  "simple default function for filtering out chunks with more than two
    times the same primitive"
  (loop with primitive-ids = (mapcar #'car (irl-program chunk))
     for id in primitive-ids
     never (> (length (find-all id primitive-ids)) 2)))

(defun no-primitive-occurs-more-than-once (chunk)
  "simple default function for filtering out chunks with more than two
    times the same primitive"
  (loop with primitive-ids = (mapcar #'car (irl-program chunk))
     for id in primitive-ids
     never (> (length (find-all id primitive-ids)) 1)))


;; check-evaluation-result-fns
(defun contains-no-open-vars (evaluation-result &rest rest)
  "checks whether there are open vars in the chunk of an evaluation result"
  (declare (ignore rest))
  (not (open-vars (chunk evaluation-result))))

;; ############################################################################
;; actual composer functions
;; ----------------------------------------------------------------------------

;; node solution scoring
(defmethod score-solution ((solution chunk-composer-node-solution)
                           (composer single-topic-composer))
  (setf (score solution)
        (funcall (evaluation-result-scoring-fn composer)
                 solution (ontology composer))))

;; enqueue
(defmethod enqueue-node ((node single-topic-composer-node)
                         (composer single-topic-composer))
  "inserts the node in the queue of the composer depending on its rating"
  (when (next-handler node)
    (sorted-list-insert node (queue composer) :predicate < :key rating)))

;; match
(defmethod handle-node ((node single-topic-composer-node)
                        (status (eql 'match))
                        (composer single-topic-composer))
  (call-next-method)
  (when (and (meaning composer) (not (matched-chunks node)))
    (change-status node 'match-chunk-failed))
  nil)

;; evaluate
(defmethod get-chunks-for-evaluation ((node single-topic-composer-node))
  (wrapped-chunks node))

(defmethod handle-node ((node single-topic-composer-node)
                        (status (eql 'evaluate))
                        (composer single-topic-composer))
  (setf (next-handler node) 'expand)
  (setf (wrapped-chunks node)
        (mapcar (chunk-wrapper-fn composer)
                (if (matched-chunks node)
                  (matched-chunks node)
                  (list (chunk node)))))
  (loop with results = (call-next-method)
        for result in results
        for (good-result test-data) 
        = (multiple-value-list 
           (funcall (check-evaluation-result-fn composer)
                    result (ontology composer) composer))
        do (when test-data
             (push test-data (check-evaluation-result-data node)))
        if (and good-result
                (or (null (topic composer))
                    (equal-entity (target-entity result)
                                  (topic composer))))
        collect result into solutions
        else  do (insert-evaluation-result result (bad-evaluation-results node))
        finally (cond 
                 ;; we succeeded
                 (solutions (change-status node 'evaluation-succeeded))
                 ;; there are only bad evaluation-results
                 ((bad-evaluation-results node) (change-status node 'bad-evaluation-results))
                 ;; no evaluation-results
                 (t (change-status node 'no-evaluation-results)))
        (return (values solutions nil))))

;; expand
(defmethod expand-chunk ((chunk chunk)
                         (composer single-topic-composer))
  (loop for fn in (expand-chunk-fns composer)
        append (funcall fn chunk composer)))

(define-event chunk-composer-node-changed (node chunk-composer-node))

(defmethod handle-node ((node single-topic-composer-node)
                        (status (eql 'expand))
                        (composer single-topic-composer))
  (loop
   with children = (multiple-value-bind (solutions children) (call-next-method)
                     (declare (ignorable solutions))
                     children)
   for child in children
   do (setf (status child) 'combined-program)
   if ;; bad chunk
   (not (loop for fn in (check-chunk-fns composer)
              always (funcall fn (chunk child))))
   do (progn
        (change-status child 'bad-chunk)
        (notify chunk-composer-node-changed node)
        (setf (next-handler child) nil))
   else 
   do (let ((equivalent-node 
             (find-node-with-equivalent-chunk
              (chunk child) composer)))
        (if equivalent-node 
          ;; duplicate
          (progn 
            (change-status child 'duplicate-chunk)
            (setf (equivalent-node child) 
                  (or (equivalent-node equivalent-node)
                      equivalent-node))
            (notify chunk-composer-node-changed node)
            (setf (next-handler child) nil))
          ;; good chunk, rate, add to hashed nodes and queue
          (progn
            (setf (rating child)
                  (funcall 
                   (node-rating-fn composer)
                   child (1+ (depth node))))
            (push child (gethash (chunk->hash-key child) (hashed-nodes composer)))
            (setf (score (chunk child))
                  (funcall (initial-chunk-score-fn composer)
                           (chunk child) child (1+ (depth node)))))))
   finally
   (return (values nil children))))
      


;; ############################################################################
;; printing, copying, etc
;; ----------------------------------------------------------------------------

(defmethod print-object ((node single-topic-composer-node) stream)
  (format stream "<node ~a(~,2f): ~a>" (sequence-number node) (rating node)
          (irl-program->title (irl-program (chunk node)) :for-html nil)))

(defmethod copy-object ((node single-topic-composer-node))
  (let ((copied-node (call-next-method)))
    (setf (status copied-node) (status node))
    (setf (status-history copied-node) (status-history node))
    (setf (rating copied-node) (rating node))
    (setf (wrapped-chunks copied-node) (wrapped-chunks node))
    (setf (check-evaluation-result-data copied-node) (check-evaluation-result-data node))
    (setf (bad-evaluation-results copied-node) (bad-evaluation-results node))
    (setf (equivalent-node copied-node) (equivalent-node node))
    copied-node))

(defmethod copy-object ((composer single-topic-composer))
  composer)

