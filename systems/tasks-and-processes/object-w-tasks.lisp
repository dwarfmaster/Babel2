
(in-package :tasks-and-processes)

(export '(object-w-tasks tasks))

(defclass object-w-tasks ()
  ((tasks
    :initarg :tasks :accessor tasks :initform nil
    :documentation "This is used to keep tasks the object was 
                    running")))

;; ----------------------------------------------------------------------------
;; object-run-task (helper for running tasks with this object)

(export '(object-run-task object-run-task-started object-run-task-finished))

(define-event object-run-task-started 
  (object t) (task task))

(define-event object-run-task-finished
  (object t) (task task))

(defgeneric object-run-task (object task &key &allow-other-keys)
  (:documentation "Runs the task for the given object. 
     agent -- the agent the task is run with
     task -- symbol (task type) or task object
     returns: the results of run-task results, processes and the task itself"))

(defmethod object-run-task ((object object-w-tasks)
                            (task task) &key n-results &allow-other-keys)
  "given a task (actual object) runs the task and returns the result
   adds the task to list of tasks in object
   notifies object-run-task-started and object-run-task-finished events"
  ;; run task, collect results and return
  (notify object-run-task-started object task)
  (unless (member task (tasks object))
    (push task (tasks object)))
  (multiple-value-bind (results processes)
      (run-task task :n-results n-results)
    (notify object-run-task-finished object task)
    (values results processes task)))

(defmethod object-run-task ((object object-w-tasks)
                            (task-class symbol)
                            &rest rest)
  "given the symbol creates the task and runs it
   passes all parameters to class initialization and next method object-run-task
   if owner is not provided, sets owner to object in task initialization"
  (apply 'object-run-task  object
         (apply 'make-instance task-class
                (if (member :owner rest)
                  rest
                  (cons :owner (cons object rest))))
         rest))
