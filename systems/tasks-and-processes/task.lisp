
(in-package :tasks-and-processes)

;; ##################################################################
;; process-result
;; ##################################################################

(export '(process-result make-process-result))

(defclass process-result (blackboard)
  ((score :initarg :score :accessor score)
   (process :initarg :process :accessor process :accessor owner
            :initform nil
            :documentation "pointer to the process that produced the result")))

(defun make-process-result (score data
                                  &key (type 'process-result-w-learning)
                                  process)
  "makes a process result the default type is with learning"
  (make-instance type
                 :process process
                 :score score
                 :data data))

(defmethod find-data ((process-result process-result)(label t) &rest rest &key omit-owner)
  "go through the data of the process and then the data of parent
   processes (through input)"
  (loop for data in (list (data process-result)
                          (unless omit-owner
                            (owner process-result)))
        for (entry found) = (multiple-value-list
                             (apply 'find-data data label
                                    rest))
        when found
        do (return-from find-data (values entry found)))
  (values nil nil))

;; ##################################################################
;; process
;; ##################################################################

(export '(process run-process continue-process
                  data input
                  results))

(defclass process (tree-node blackboard)
  ((task :initarg :task :accessor task :accessor owner)
   (label :initarg :label :accessor label)
   (status :initarg :status :accessor status :initform '(create)
           :documentation "track of processing of this node
                           '(continue run create)")
   (input :initarg :input :accessor input :initform nil
          :documentation "the input result from the previous process")
   (results :initarg :results :accessor results :initform nil
            :documentation "a list of all results of run/continue the process")))

(defgeneric run-process (process process-label task owner)
  (:documentation "called for executing a process,
                   returns a list of process-result
                   (should be ordered by score)"))

(defgeneric continue-process (process process-label task owner)
  (:documentation "continues a process, same as run except for that
                   run has already been called once on this process"))

;; ##################################################################

(defmethod find-data ((process process)(label t) &rest rest &key omit-owner)
  "go through the data of the process and then the data of parent
   processes (through input)"
  (loop for data in (list (data process)
                          (input process)
                          (unless omit-owner
                            (owner process)))
        for (entry found) = (multiple-value-list
                             (apply 'find-data data label
                                    rest))
        when found
        do (return-from find-data (values entry found)))
  (values nil nil))

;; ##################################################################

(export '(find-process))

(defgeneric find-process (thing label)
  (:documentation "retrieves the process from a process result or process
                   and parents"))

(defmethod find-process ((process process)(label symbol))
  (if (eq (label process) label)
    process
    (find-process (parent process) label)))

(defmethod find-process ((result process-result)(label symbol))
  (find-process (process result) label))

;; ##################################################################
;; task
;; ##################################################################

(export '(task processes unfinished-processes results owner run-task
               run-task-started run-task-finished run-next-process
               restart-process continue-task))

(defclass task (tree blackboard configuration)
  ((label :initarg :label :accessor label :initform (make-id 'task)
          :documentation "something like production, interpretation etc")
   (processes :initarg :processes :accessor processes :initform nil
              :documentation "linear order of processes to run e.g.
                              '(conceptualize produce ...)")
   (unfinished-processes :initarg :unfinished-processes
                         :accessor unfinished-processes
                         :initform nil
                         :documentation "queue of unfinished processes")
   (owner :initarg :owner :accessor owner :initform nil
               :documentation "he who owns the task, normally some agent")
   (results :initarg :results :accessor results
            :initform nil
            :documentation "all process-results of final processes
                            ever obtained in this task")))

;; ##################################################################

(defgeneric run-next-process (task)
  (:documentation "takes the next process from the unfinished-processes
                   queue and runs it
                   returns the results and the process run"))

(defmethod run-next-process ((task task))
  "takes the next process from the unfinished-processes queue and runs it
   returns the results and the process run"
  ;; get next process
  (let ((process (pop (unfinished-processes task))))
    (when process
      ;; run process
      (let ((results 
             (listify (if (member 'run (status process))
                        ;; if process already run -> continue
                        (progn
                          (push 'continue (status process))
                          (continue-process process (label process) task
                                            (owner task)))
                        ;; run otherwise
                        (progn 
                          (push 'run (status process))
                          (run-process process (label process)
                                       task (owner task)))))))
        ;; set the process pointer in the result (unless already done)
        (loop for res in results
              unless (process res)
              do (setf (process res) process))
        ;; add results to results of the process
        (setf (results process) (append (results process) results))
        (values results process)))))

;; ##################################################################

(define-event run-task-started (task task))

(define-event run-task-finished (task task))

(define-configuration-default-value :run-task-process-class 'process)

(defun run-task (task &key n-results)
  "high level function for running a task by running/continuing unfinished processes
   returns the results of finished processes and the processes themselves
   finished processes are those last in (processes task) which returned results.
   n-results -- only get the first n results (can return more results, but stops as
                                              soon as n-results is reached)"
  (notify run-task-started task)
  (loop
   initially
   (when (null (top task))
     ;; create first process
     (let ((first-process
            (make-instance (get-configuration task :run-task-process-class)
                           :label  (first (processes task))
                           :task task)))
       ;; add it to unfinished processes
       (push first-process
             (unfinished-processes task))
       ;; add it as top-node
       (add-node task first-process)))
   
   
   ;; run process
   for (results process) = (multiple-value-list (run-next-process task))
   ;; get next process label that should be run after this
   for next-process-label = (nth (+ 1 (length (parents process :key #'label)))
                                 (processes task))
   if next-process-label
   do ;; create next processes based on results
   (loop for result in (reverse results)
         for new-process = (make-instance (get-configuration task :run-task-process-class)
                                          :label next-process-label
                                          :task task
                                          :input result)
         do
         (push new-process (unfinished-processes task))
         (add-node task new-process :parent process))
   else if results
   append results into finished-processes-results
   and collect process into finished-processes
   ;; as long as there are still processes to run
   while (and (unfinished-processes task)
              (or (null n-results) (length< finished-processes-results n-results)))
   finally
   (setf (results task)
         (sort (copy-list (append (results task) finished-processes-results))
               #'> :key #'score))
   (notify run-task-finished task)
   (return (values (sort finished-processes-results #'> :key #'score)
                   finished-processes))))

;; ##################################################################

(defun continue-task (task &key process-label top-only)
  "fetches relevant processes, adds them to (unfinished-processes task)
   calls run-task
   1) all processes (and (null process-label) (null top-only))
   2) just the top (and (null process-label) top-only )
   3) just processes with process-label (and process-label top-only)
   4) all processes with process-label and their children (and process-label (null top-only))
   Notice1: if process-label is specified really all processes with that label are continued
   Notice2: if you want to continue one specific process it is probably easier you
            fetch it and add it to unfinished-processes yourself"
  (let ((processes-to-continue
         ;; compute processes to be continued
         (cond
          ((and (null process-label)(null top-only))
           (nodes task))
          ((and (null process-label) top-only)
           (list (top task)))
          ((and process-label top-only)
           (find-all process-label (nodes task) :key #'label))
          ((and process-label (null top-only)) ;; careful! no duplicate checking
           ;; if you use the same process-label for different levels of processes
           ;; you are screwed
           (loop for node in (find-all process-label (nodes task)
                                       :key #'label)
                 collect node
                 append (all-children node))))))
    ;; add processes to end of unfinished processes 
    (setf (unfinished-processes task) (append (unfinished-processes task)
                                              processes-to-continue))
    ;; run the task
    (run-task task)))

;; ##################################################################

(defun restart-process (task process process-label &key data)
  "restart is not really a restart. All that happens is that
   the relevant process-result is fetched from the parent process (of process) with
   process-label and then this is used to create a new process branch
   which is appended to unfinished-processes
   returns the new process
   Note: for technical reasons you CANNOT restart the first process, just create
   a dummy process to restart your first process (you can use the one below)"
  (let* ((process-to-restart
          (find process-label (cons process (parents process))
                :key #'label))
         (new-process (when (and process-to-restart
                                 (parent process-to-restart))
                        (make-instance (get-configuration task :run-task-process-class)
                                       :label process-label
                                       :input (input process-to-restart)
                                       :task task
                                       :data data))))
    (if new-process
      (progn
        ;; add process to unfinished processes and add to tree
        (push 'restarted (status process))
        (push 'restart (status new-process))
        (push new-process (unfinished-processes task))
        (add-node task new-process :parent (parent process-to-restart))
        new-process)
      (cond
       ((null process-to-restart)
        (error "error trying to restart process with label ~a. process ~
              does not exist." process-label))
       ((null (parent process-to-restart))
        (error "error the process that needs to be restarted CANNOT be the ~
                root process, i.e. it has to have parents."))
       ((null new-process)
        (error "error the process for restart could not be created."))))))

;; ##################################################################
;; dummy inital-process
;; ##################################################################

(defmethod run-process (process (process-label (eql 'initial-process))
                                task owner)
  "dummy process for restart situations"
  (declare (ignorable process task owner process-label))
  (list (make-process-result 1.0 nil)))
