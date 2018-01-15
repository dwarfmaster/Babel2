
(in-package :tasks-and-processes)

(defclass test-problem (problem) ())

;; -----------------------------------------------------------------
;; test process result diagnosis
;; -----------------------------------------------------------------

(defclass process-result-diagnostic (diagnostic)
  ((trigger :initform 'process-2)))

(defmethod diagnose ((diagnostic process-result-diagnostic)
                     object-w-learning &key trigger &allow-other-keys)
  (declare (ignore object-w-learning))
  (assert (eq trigger 'process-2))
  (make-instance 'test-problem))

(defmethod run-process (process (process-label (eql 'test-process-1-results-diagnose))
                                task owner)
  (declare (ignorable process task owner))
  ;; run process
  (loop for res from 1 to 5
        for r = (make-process-result (/ 1.0 res)
                                     `((result-process-1 . ,res))
                                     :process process)
        do (notify-learning r :trigger 'process-1)
        collect r))

(defmethod run-process (process (process-label (eql 'test-process-2-results-diagnose))
                                task owner)
  (declare (ignorable process task owner))
  ;; run process
  (loop for res from 1 to 5
        for r = (make-process-result (/ 1.0 res)
                                     `((result-process-1 . ,res))
                                     :process process)
        do (notify-learning r :trigger 'process-2)
        collect r))

(deftest test-diagnose-process-results ()
  (let ((task (make-instance 'task-w-learning
                             :processes '(test-process-1-results-diagnose
                                          test-process-2-results-diagnose)
                             :diagnostics (list (make-instance 'process-result-diagnostic)))))
    (run-task task)
    (loop for r in (results task)
          do (test-assert (length= 1 (problems r))))
    (loop for n in (nodes task)
          if (eq (label n) 'test-process-1-results-diagnose)
          do (test-assert (length= 0 (problems n)))
          else
          do (test-assert (length= 5 (problems n))))
    (test-assert (length= 25 (problems task)))))

;; (test-diagnose-process-results)

;; -----------------------------------------------------------------
;; test process result repair
;; -----------------------------------------------------------------

(defclass process-result-repair (repair)
  ((trigger :initform 'process-2)
   (counter :accessor counter :initform 0)))

(defmethod repair ((repair process-result-repair)
                   (problem test-problem)
                   (object process-result)
                   &key trigger &allow-other-keys)
  (declare (ignore problem))
  (test-assert (eq trigger 'process-2))
  (incf (counter repair))
  (when (= (mod (counter repair) 2) 0)
    (make-instance 'fix)))

(deftest test-repair-process-results ()
  (let ((task (make-instance 'task-w-learning
                             :processes '(test-process-1-results-diagnose
                                          test-process-2-results-diagnose)
                             :diagnostics (list (make-instance 'process-result-diagnostic))
                             :repairs (list (make-instance 'process-result-repair)))))
    (run-task task)
    (test-assert (length= 13 (find-all-if #'open-problem? (problems task))))
    (test-assert (length= 12 (find-all-if #'(lambda (x)
                                              (not (open-problem? x)))
                                          (problems task))))))

;; (test-repair-process-results)

;; -----------------------------------------------------------------
;; test process diagnostic
;; -----------------------------------------------------------------

(defclass process-diagnostic (diagnostic)
  ((trigger :initform 'test-process-2-diagnose)))

(defmethod diagnose ((diagnostic process-diagnostic)
                     (object-w-learning process) &key trigger &allow-other-keys)
  (assert (eq trigger 'test-process-2-diagnose))
  (make-instance 'test-problem))

(defmethod run-process (process
                        (process-label (eql 'test-process-1-diagnose))
                        task owner)
  (declare (ignorable process task owner))
  ;; run process
  (loop for res from 1 to 5
        for r = (make-process-result (/ 1.0 res)
                                     `((result-process-1 . ,res))
                                     :process process)
        collect r))

(defmethod run-process (process
                        (process-label (eql 'test-process-2-diagnose))
                        task owner)
  (declare (ignorable process task owner))
  ;; run process
  (loop for res from 1 to 5
        for r = (make-process-result (/ 1.0 res)
                                     `((result-process-1 . ,res))
                                     :process process)
        collect r))

(defclass test-task (task-w-learning) ())

(defmethod run-next-process :around ((task test-task))
  (multiple-value-bind (results process)
      (call-next-method)
    (notify-learning process :trigger (label process))
    (values results process)))

(deftest test-diagnose-process ()
  (let ((task (make-instance 'test-task
                             :processes '(test-process-1-diagnose
                                          test-process-2-diagnose)
                             :diagnostics (list (make-instance 'process-diagnostic)))))
    (run-task task)
    (loop for r in (results task)
          do (test-assert (length= 0 (problems r))))
    (loop for n in (nodes task)
          if (eq (label n) 'test-process-1-diagnose)
          do (test-assert (length= 0 (problems n)))
          else
          do (test-assert (length= 1 (problems n))))
    (test-assert (length= 5 (problems task)))))

;; (test-diagnose-process)

;; -----------------------------------------------------------------
;; test process repair
;; -----------------------------------------------------------------

(defclass process-repair (repair)
  ((trigger :initform 'test-process-2-diagnose)
   (counter :accessor counter :initform 0)))

(defmethod repair ((repair process-repair)
                   (problem test-problem)
                   (process process)
                   &key trigger &allow-other-keys)
  (declare (ignore problem))
  (test-assert (eq trigger 'test-process-2-diagnose))
  (incf (counter repair))
  (when (= (mod (counter repair) 2) 0)
    (make-instance 'fix)))

(deftest test-repair-process ()
  (let ((task (make-instance 'test-task
                             :processes '(test-process-1-diagnose
                                          test-process-2-diagnose)
                             :diagnostics (list (make-instance 'process-diagnostic))
                             :repairs (list (make-instance 'process-repair)))))
    (run-task task)
    (test-assert (length= 3 (find-all-if #'open-problem? (problems task))))
    (test-assert (length= 2 (find-all-if #'(lambda (x)
                                             (not (open-problem? x)))
                                         (problems task))))))

;; (test-repair-process)

;; -----------------------------------------------------------------
;; test process repair + restart
;; -----------------------------------------------------------------

(defclass process-repair+restart (repair)
  ((trigger :initform 'test-process-2-diagnose)
   (counter :accessor counter :initform 0)))

(defmethod repair ((repair process-repair+restart)
                   (problem test-problem)
                   (process process)
                           &key trigger &allow-other-keys)
  (declare (ignore problem))
  (test-assert (eq trigger 'test-process-2-diagnose))
  (incf (counter repair))
  (when (not (member 'restart (status process)))
    (make-instance 'fix :restart-data t)))

(deftest test-repair-process+restart ()
  (let ((task (make-instance 'test-task
                             :processes '(test-process-1-diagnose
                                          test-process-2-diagnose)
                             :diagnostics (list (make-instance 'process-diagnostic))
                             :repairs (list (make-instance 'process-repair+restart)))))
    (run-task task)
    ;; 5 processes + 5 restarted
    (test-assert (length= 10 (find-all 'test-process-2-diagnose (nodes task)
                                       :key #'label)))
    ;; 5 problems repaired
    (test-assert (length= 5 (find-all-if #'open-problem? (problems task))))
    ;; 5 restart problems diagnosed not repaired
    (test-assert (length= 5 (find-all-if #'(lambda (x)
                                             (not (open-problem? x)))
                                         (problems task))))))

;; (test-repair-process+restart)
