
(in-package :tasks-and-processes)

(defmethod run-process (process (process-label (eql 'test-process-1))
                                task owner)
  (declare (ignorable process task owner))
  ;; run process
  (loop for res from 1 to 5
        collect (make-process-result (/ 1.0 res)
                                     `((result-process-1 . ,res)))))

(defmethod run-process (process (process-label (eql 'test-process-2))
                                task owner)
  (declare (ignorable process task owner))
  ;; run process
  (loop for res from 1 to 10
        collect (make-process-result 1.0 `((result-process-2 . ,res)))))

(defmethod continue-process (process (process-label (eql 'test-process-2))
                                     task owner)
  (declare (ignorable process task owner))
  ;; run process
  (loop for res from 1 to 2
        collect (make-process-result 1.0 `((result-process-2 . ,res)))))

(defmethod run-process (process (process-label (eql 'test-process-3))
                                task owner)
  (declare (ignorable process task owner))
  ;; run process
  (loop for res from 1 to 5
        collect (make-process-result 1.0 `((result-process-3 . ,res)))))

(defmethod continue-process (process (process-label (eql 'test-process-3))
                                     task owner)
  (declare (ignorable process task owner))
  ;; run process
  (loop for res from 1 to 1
        collect (make-process-result 1.0 `((result-process-3 . ,res)))))

(deftest test-run-task ()
  ;; run-task, single process
  (let ((task
         (make-instance 'task :processes '(test-process-1))))
    (multiple-value-bind (results processes)
        (run-task task)
      (test-assert (length= results 5))
      (test-assert (length= processes 1))
      (test-assert (loop for res in results
                         always (typep res 'process-result)))
      (test-assert (loop for p in processes
                         always (typep p 'process)))))

  ;; run-task, two processes
  (let ((task
         (make-instance 'task :processes '(test-process-1 test-process-2))))
    (multiple-value-bind (results processes)
        (run-task task)
      (test-assert (length= results (* 5 10)))
      (test-assert (length= (results task) (* 5 10)))
      (test-assert (length= processes 5))
      (test-assert (loop for res in results
                         always (typep res 'process-result)))
      (test-assert (loop for p in processes
                         always (typep p 'process)))
      (test-assert (loop for p in (find-all 'test-process-1
                                            (nodes task) :key #'label)
                         always (length= (results p) 5)))
      (test-assert (loop for p in (find-all 'test-process-2
                                            (nodes task) :key #'label)
                         always (length= (results p) 10)))))

  ;; task continue - error
  (let ((task
         (make-instance 'task :processes '(test-process-1))))
    (run-task task)
    (test-error (continue-task task :top-only t)))
  
  ;; task continue - single process
  (let ((task
         (make-instance 'task :processes '(test-process-2))))
    (run-task task)
    (multiple-value-bind (results processes)
        (continue-task task :top-only t)
      (test-assert (length= results 2))
      (test-assert (length= (results task) (+ 2 10)))
      (test-assert (length= processes 1))))
  
  ;; task continue - double process, last processes (test-process-3)
  (let ((task
         (make-instance 'task :processes '(test-process-2 test-process-3))))
    (multiple-value-bind (results processes)
        (run-task task)
      (test-assert (length= results (* 5 10)))
      (test-assert (length= (results task) (* 5 10)))
      (test-assert (length= processes 10))
      (test-assert (loop for p in (find-all 'test-process-2
                                            (nodes task) :key #'label)
                         always (length= (results p) 10)))
      (test-assert (loop for p in (find-all 'test-process-3
                                            (nodes task) :key #'label)
                         always (length= (results p) 5)))
      (multiple-value-bind (results processes)
          (continue-task task :process-label 'test-process-3)
        (test-assert (length= results 10))
        (test-assert (length= (results task) 60))
        (test-assert (loop for p in (find-all 'test-process-2
                                              (nodes task) :key #'label)
                           always (length= (results p) 10)))
        (test-assert (loop for p in (find-all 'test-process-3
                                              (nodes task) :key #'label)
                           always (length= (results p) 6)))
        (test-assert (length= processes 10)))))

  ;; task continue - double process, last processes (test-process-3)
  (let ((task
         (make-instance 'task :processes '(test-process-2 test-process-3))))
    (multiple-value-bind (results processes)
        (run-task task)
      (declare (ignore results processes))
      (multiple-value-bind (results processes)
          (continue-task task :process-label 'test-process-3 :top-only t)
        (test-assert (length= results 10))
        (test-assert (length= (results task) 60))
        (test-assert (loop for p in (find-all 'test-process-2
                                              (nodes task) :key #'label)
                           always (length= (results p) 10)))
        (test-assert (loop for p in (find-all 'test-process-3
                                              (nodes task) :key #'label)
                           always (length= (results p) 6)))
        (test-assert (length= processes 10)))))
 
  ;; task continue - double process, top-only
  ;; continue test-process-2 -> 2 new results
  ;;  * 5 new results for test-process-3 (run)
  (let ((task
         (make-instance 'task :processes '(test-process-2 test-process-3))))
    (multiple-value-bind (results processes)
        (run-task task)
      (declare (ignore results processes))
      (multiple-value-bind (results processes)
          (continue-task task :top-only t)
        (test-assert (length= results 10))
        (test-assert (length= (results task) 60))
        (test-assert (loop for p in (find-all 'test-process-2
                                              (nodes task) :key #'label)
                           always (length= (results p) 12)))
        (test-assert (loop for p in (find-all 'test-process-3
                                              (nodes task) :key #'label)
                           always (length= (results p) 5)))
        (test-assert (length= processes 2)))))
  
  ;; task continue - double process, all
  (let ((task
         (make-instance 'task :processes '(test-process-2 test-process-3))))
    (multiple-value-bind (results processes)
        (run-task task)
      (declare (ignore results processes))
      (multiple-value-bind (results processes)
          (continue-task task)
        (test-assert (length= results 20))
        (test-assert (length= (results task) 70))
        (test-assert (loop for p in (find-all 'test-process-2
                                              (nodes task) :key #'label)
                           always (length= (results p) 12)))
        (test-assert (loop for p in (find-all 'test-process-3
                                              (nodes task) :key #'label)
                           always (or (length= (results p) 6)
                                      (length= (results p) 5))))
        (test-assert (length= processes 12)))))

  ;; restart
  (let ((task
         (make-instance
          'task :processes '(initial-process test-process-1))))
    (run-task task)
    (test-assert
     (length= 1 (find-all 'test-process-1 (nodes task) :key #'label)))
    (test-assert
     (length= 5 (results task)))
    (restart-process task (find 'test-process-1 (nodes task) :key #'label)
                     'test-process-1)
    (test-assert
     (length= 5 (results task)))
    (test-assert
     (length= 2 (find-all 'test-process-1 (nodes task) :key #'label)))
    (run-task task)
    (test-assert
     (length= 10 (results task)))))

;; (test-run-task)



