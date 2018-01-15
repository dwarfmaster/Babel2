(in-package :irl)

;; #########################################################
;;; defmethods used by the composer
;; #########################################################

(export '(handle-node enqueue-node))

(defgeneric handle-node (node handler composer))

(defgeneric enqueue-node (node composer))

;; #########################################################
;; node and composer
;; #########################################################

(export '(chunks
          chunk-composer-node
          chunk-composer-node-solution
          chunk-composer-node-solution
          chunk-composer set-max-search-depth
          solutions
          depth
          sequence-number
          ontology
          children
          matched-chunks
          source-chunks
          node))

(defclass chunk-composer-node-solution (chunk-evaluation-result)
  ((node
    :type chunk-composer-node :initarg :node :accessor node
    :documentation "Node that was evaluated to yield evaluation result."))
  (:documentation "A chunk evaluation result (a solution), augmented by the node."))

(defclass chunk-composer-node ()
  ((next-handler :initarg :next-handler :accessor next-handler :initform nil)
   (handler-history :initarg :handler-history :accessor handler-history :initform nil)
   (chunk :initarg :chunk :accessor chunk :initform (make-instance 'chunk))
   (source-chunks :initarg :source-chunks :accessor source-chunks :initform nil)
   (children 
    :type list :initarg :children :initform nil :accessor children)
   (sequence-number
    :type integer :initarg :sequence-number :initform 0 :accessor sequence-number
    :documentation "Each node that is added to the tree gets a consecutive number")
   (depth 
    :type integer :initarg :depth :initform 0 :accessor depth
    :documentation "How deep the node is in the tree")
   (solutions
    :type list :initarg :solutions :initform nil 
    :accessor solutions
    :documentation "A list of composer-node-results")
   (evaluation-tree
    :initarg :evaluation-tree :initform nil
    :accessor evaluation-tree
    :documentation "When evaluated, this contains the tree")
   (matched-chunks :initarg :matched-chunks :accessor matched-chunks :initform nil)))

(defclass chunk-composer ()
  ((node-class :initarg :node-class :accessor node-class
               :initform 'chunk-composer-node)
   (ontology :initarg :ontology :reader ontology
             :documentation "The ontology to be used for the composer")
   (configuration :initarg :configuration :reader configuration
                  :initform (make-configuration)
                  :documentation "A configuration used in the composer")
   (chunks :initarg :chunks :accessor chunks :initform nil)
   (nodes :initarg :nodes :accessor nodes :initform nil)
   (top-node
    :initform nil
    :type (or null chunk-composer-node) :accessor top-node
    :documentation "the top node of the search tree (== the initial node)")
   (solutions :initarg :solutions :accessor solutions :initform nil)
   (queue :initarg :queue :accessor queue :initform nil)
   (meaning :initarg :meaning :accessor meaning :initform nil)
   (max-search-depth :type integer :initarg :max-search-depth :initform 10
                     :reader max-search-depth 
                     :documentation "the maximum number of chunk extensions")))

(defmethod initialize-instance 
           :after ((composer chunk-composer) 
                   &key
                   (initial-node nil)
                   (initial-chunk nil))
  (when (and initial-node initial-chunk)
    (error "Please only provide either an initial chunk or an initial node."))
  (unless (chunks composer) 
    (setf (chunks composer) (get-data (ontology composer) 'chunks)))
  (let ((initial-node (if initial-node
                        initial-node
                        (make-instance (node-class composer)
                                       :next-handler 'match
                                       :chunk (if initial-chunk
                                                initial-chunk
                                                (make-instance 'chunk))))))
    (setf (source-chunks initial-node) (list (chunk initial-node)))
    (setf (top-node composer) initial-node)
    (setf (nodes composer) (list initial-node))
    (enqueue-node initial-node composer)))

(define-event chunk-composer-increased-search-depth
  (queued-nodes list) (composer chunk-composer))

(defun increase-max-search-depth (composer max-search-depth)
  (loop
   with queue-nodes = (find-all-if #'(lambda(n)
                                         (= (depth n)
                                            (max-search-depth composer)))
                                     (nodes composer))
   for n in queue-nodes
   unless (next-handler n)
   do (setf (next-handler n) 'expand)
   do (enqueue-node n composer)
   finally (notify chunk-composer-increased-search-depth
                   queue-nodes composer))
  (setf (slot-value composer 'max-search-depth) max-search-depth))

;; #########################################################
;;; get solutions defuns
;; #########################################################

(export '(get-next-solutions get-all-solutions score-solution))

(defgeneric score-solution (solution composer))

(defmethod score-solution ((solution chunk-composer-node-solution)
                           (composer chunk-composer))
  (setf (score solution)
        (simple-evaluation-result-scoring solution (ontology composer))))

;; events relevant for composition
(define-event chunk-composer-get-next-solutions-started (composer chunk-composer))
(define-event chunk-composer-get-all-solutions-started (composer chunk-composer))
(define-event chunk-composer-next-node (node chunk-composer-node))
(define-event chunk-composer-node-handled (node chunk-composer-node))
(define-event chunk-composer-new-nodes (successors list))
(define-event chunk-composer-finished (solutions list) (composer chunk-composer))

(defun get-next-solutions (composer &key (notify t))
  (when notify (notify chunk-composer-get-next-solutions-started composer))
  (when (queue composer)
    (loop
       for node = (pop (queue composer))
       for handler = (next-handler node)
       for (solutions new-nodes) = (multiple-value-list (handle-node node handler composer))
       ;; handle node
       do (progn
            (notify chunk-composer-node-handled node)
            (setf (handler-history node)
                  (cons handler (handler-history node)))
            (enqueue-node node composer))
       ;; handle new nodes
       when new-nodes
       do (progn
            (notify chunk-composer-new-nodes new-nodes)
            (setf (children node) new-nodes)
            (loop for new-node in new-nodes
               do (progn
                    (setf (sequence-number new-node) (length (nodes composer)))
                    (setf (nodes composer) (cons new-node (nodes composer)))
                    (setf (depth new-node) (1+ (depth node)))
                    (enqueue-node new-node composer))))
       ;; handle solutions
       when solutions
       do (progn
            (setf solutions
                  (sort 
                   (loop for solution in solutions
                      do (setf (score solution) (score-solution solution composer))
                      collect solution)
                   #'> :key #'score))
            (setf (solutions node) solutions)
            (setf (solutions composer)
                  (sort (append solutions (solutions composer))
                        #'> :key #'score)))
       ;; notify for next node
       when (> (length (queue composer)) 0)
       do (notify chunk-composer-next-node (first (queue composer)))
       while (queue composer)
       until solutions
       finally
       (when notify (notify chunk-composer-finished 
                            (and node (solutions node)) composer))
       (return solutions))))

(export '(get-solutions get-all-solutions))

(defun get-all-solutions (composer &key (notify t))
  (when notify
    (notify chunk-composer-get-all-solutions-started composer))
  (loop while (queue composer)
        do (get-next-solutions composer :notify notify))
  (when notify
    (notify chunk-composer-finished (solutions composer) composer))
  (solutions composer))
    

(defun get-solutions (composer &key (stop-criteria #'identity))
  (sort
   (loop while (and (queue composer)
                    (not (funcall stop-criteria composer)))
         append (get-next-solutions composer))
   #'> :key #'score))

;; #########################################################
;; enqueue
;; #########################################################

(defmethod enqueue-node ((node chunk-composer-node)
                         (composer chunk-composer))
  (when (not (null (next-handler node)))
    (setf (queue composer) (cons node (queue composer)))))

;; #########################################################
;;; match
;; #########################################################

(export '(match))

(defmethod handle-node ((node chunk-composer-node)
                        (handler (eql 'match))
                        (composer chunk-composer))
  (if (meaning composer)
    (let ((matched-chunks
           (match-chunk (chunk node) (meaning composer)
                        :notify nil)))
      (if matched-chunks
        (progn
          (setf (matched-chunks node) matched-chunks)
          (setf (next-handler node) 'evaluate))
        (setf (next-handler node) 'expand)))
    (setf (next-handler node) 'evaluate))
  (values nil nil))

;; #########################################################
;;; evaluate
;; #########################################################

(export '(get-chunks-for-evaluation evaluate))

(defgeneric get-chunks-for-evaluation (node))

(defmethod get-chunks-for-evaluation ((node chunk-composer-node))
  (if (matched-chunks node)
    (matched-chunks node)
    (list (chunk node))))

(defmethod handle-node ((node chunk-composer-node)
                        (handler (eql 'evaluate))
                        (composer chunk-composer))
  (setf (next-handler node) 'expand)
  (values (loop for chunk in (get-chunks-for-evaluation node)
                append (multiple-value-bind (solutions evaluation-tree)
                             (evaluate-chunk
                              chunk (ontology composer)
                              :chunk-evaluation-result-class
                              'chunk-composer-node-solution
                              :configuration (configuration composer)
                              :notify nil)
                           (setf (evaluation-tree node) evaluation-tree)
                           (loop
                            for solution in solutions
                            do (setf (node solution) node)
                            collect solution)))
          nil))

;; #########################################################
;;; expand
;; #########################################################

(export '(expand-chunk expand))

(defgeneric expand-chunk (chunk composer))

;; should return a cons of a list of chunks and a list of source-chunks
;; well somehow it has to return the source chunks, as only it knows
(defmethod expand-chunk ((chunk chunk)
                         (composer chunk-composer))
  (declare (ignorable composer))
  (loop for other-chunk in (chunks composer)
        append
        (loop for combined-chunk in (combine-chunk-program chunk other-chunk)
              collect (cons combined-chunk (list other-chunk)))))

(defmethod handle-node ((node chunk-composer-node)
                        (handler (eql 'expand))
                        (composer chunk-composer))
  (setf (next-handler node) nil)
  (values nil
          (when (> (max-search-depth composer) (depth node))
            (loop
             for (new-chunk . source-chunks) in (expand-chunk (chunk node) composer)
             for new-node = (make-instance 
                             (type-of node)
                             :next-handler 'match
                             :source-chunks (append source-chunks (source-chunks node))
                             :chunk new-chunk)
             collect new-node))))

;; ############################################################################
;; printing, copying, etc
;; ----------------------------------------------------------------------------

(defmethod print-object ((node chunk-composer-node) stream)
  (format stream "<node ~a: ~a>" (sequence-number node)
          (irl-program->title (irl-program (chunk node)) :for-html nil)))

(defmethod copy-object ((node chunk-composer-node))
  (make-instance (type-of node)
                 :next-handler (next-handler node) 
                 :handler-history (handler-history node)
                 :source-chunks (source-chunks node)
                 :chunk (chunk node)
                 :depth (depth node) 
                 :children (copy-list (children node))
                 :matched-chunks (matched-chunks node)
                 :solutions (solutions node)
                 :sequence-number (sequence-number node)
                 :evaluation-tree (evaluation-tree node)))