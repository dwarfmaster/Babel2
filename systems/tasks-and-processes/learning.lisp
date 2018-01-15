
(in-package :tasks-and-processes)

(export '(task-w-learning
          process-w-learning
          process-result-w-learning))

;; ############################################################################
;; process-result-w-learning
;; ----------------------------------------------------------------------------

(defclass process-result-w-learning (process-result object-w-learning) ())

(defmethod add-problem ((result process-result-w-learning)
                        problem &key &allow-other-keys)
  (call-next-method)
  (add-problem (process result) problem))

(defmethod get-diagnostics ((result process-result-w-learning)
                            &key (recursive t) &allow-other-keys)
  (append (diagnostics result)
          (when recursive (get-diagnostics (process result)))))

(defmethod get-repairs ((result process-result-w-learning)
                        &key (recursive t) &allow-other-keys)
  (append (call-next-method)
          (when recursive (get-repairs (process result)))))

(defmethod restart-object ((result process-result-w-learning)
                           (restart-data t)
                           &key process-label &allow-other-keys)
  (restart-object (process result) restart-data :process-label process-label))

;; ############################################################################
;; process-w-learning
;; ----------------------------------------------------------------------------

(defclass process-w-learning (process object-w-learning)
  ())

(defmethod add-problem ((process process-w-learning)
                        problem &key &allow-other-keys)
  (call-next-method)
  (add-problem (task process) problem))

(defmethod get-diagnostics ((process process-w-learning)
                            &key (recursive t) &allow-other-keys)
  (append (diagnostics process)
          (when recursive (get-diagnostics (task process)))))

(defmethod get-repairs ((process process-w-learning)
                        &key (recursive t) &allow-other-keys)
  (append (call-next-method)
          (when recursive (get-repairs (task process)))))

(defmethod restart-object ((process process-w-learning)
                           restart-data
                           &key process-label &allow-other-keys)
  (declare (ignore restart-data))
  (restart-process (task process) process
                   (or process-label (label process))))

;; ############################################################################
;; task-w-learning
;; ----------------------------------------------------------------------------

(defclass task-w-learning (task object-w-learning)
  ((configuration :initform '((:run-task-process-class . process-w-learning)))))

