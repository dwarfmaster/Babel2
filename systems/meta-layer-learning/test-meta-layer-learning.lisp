
(in-package :meta-layer-learning)

(deftest test-instance-creation ()
  (test-ok (make-instance 'diagnostic))
  (test-ok (make-instance 'diagnostic :trigger nil))
  (test-ok (make-instance 'diagnostic :trigger 'foo))
  (test-ok (make-instance 'diagnostic :trigger '(foo bar)))
  (let ((diagnostic (make-instance 'diagnostic :trigger '(FCG ?any))))
    (test-assert (equal '(FCG ?any) (trigger diagnostic)))
    (test-ok (setf (trigger diagnostic) 'foo)))

  (test-ok (make-instance 'problem))
  (test-ok (make-instance 'problem :issued-by 'foo))
  (test-ok (make-instance 'problem 
                          :issued-by 'foo 
                          :fixes (make-instance 'repair)))
  (test-ok (make-instance 'problem 
                          :fixes (make-instance 'repair)))
  (let* ((repair (make-instance 'repair))
         (problem (make-instance 'problem 
                                 :issued-by 'diagnostic-1
                                 :fixes repair)))
    (test-assert (equal 'diagnostic-1 (issued-by problem)))
    (test-assert (equal (fixes problem) repair))))

;; (test-instance-creation)


;; -----------------------------------------------------------------
;; Definition of diagnostics and repairs for testing notify-learning
;; -----------------------------------------------------------------

(defclass test-diagnostic-1 (diagnostic)
  ((trigger :initform 'test-diagnostic-1)))

(defmethod diagnose ((d test-diagnostic-1) object-w-learning &key trigger)
  (declare (ignore object-w-learning))
  (let ((problem (make-instance 'problem)))
    (set-data problem 'test-field 'test-diagnostic-1)
    (set-data problem 'trigger trigger)
    (list problem)))

(defclass test-diagnostic-2 (diagnostic)
  ())
(defmethod initialize-instance :after ((diagnostic test-diagnostic-2) &key)
  (setf (trigger diagnostic) 'test-diagnostic-2))

(defmethod diagnose ((d test-diagnostic-2) object-w-learning &key test-data &allow-other-keys)
  (let ((problem (make-instance 'problem)))
    (set-data problem 'test-field 'test-diagnostic-2)
    (set-data problem 'test-data test-data)
    (set-data problem 'obj-w-learning object-w-learning)
    problem))

(defclass test-diagnostic-3 (diagnostic)
  ())

(defmethod diagnose ((d test-diagnostic-3) object-w-learning &key &allow-other-keys)
  (declare (ignore object-w-learning))
  (let ((problem (make-instance 'problem)))
    (set-data problem 'test-field 'test-diagnostic-3)
    problem))

(defclass test-diagnostic-4 (diagnostic)
  ((trigger :initform 'test-diagnostic-4)))

(defmethod diagnose ((d test-diagnostic-4) object-w-learning &key &allow-other-keys)
  (declare (ignore object-w-learning))
  "string-problem")

(defclass test-diagnostic-5 (diagnostic)
  ((trigger :initform 'test-diagnostic-5)))


(deftest test-diagnostics ()
  (let* ((diagnostic-1 (make-instance 'test-diagnostic-1))
         (diagnostic-2 (make-instance 'test-diagnostic-2))
         (diagnostic-3 (make-instance 'test-diagnostic-3))
         (diagnostic-4 (make-instance 'test-diagnostic-4))
         (diagnostic-5 (make-instance 'test-diagnostic-5))
         (obj-w-learning (make-instance 'object-w-learning
                                        :diagnostics (list diagnostic-1 diagnostic-2))))
    ;; test whether obj-w-learning has correctly initialized
    (test-assert (= (length (diagnostics obj-w-learning)) 2))
    ;; test addition of diagnostics and repairs
    (test-ok (add-diagnostic obj-w-learning diagnostic-3))
    
    ;; 1. notify without :trigger 
    ;;; (a) should trigger diagnostic-3 (and repair-3) because they
    ;;; did not specify a trigger
    (notify-learning obj-w-learning)
    (test-assert (= 1 (length (problems obj-w-learning)))) ;; created by diagnostic-3
    (test-assert (equal (get-data (first (problems obj-w-learning)) 'test-field) 'test-diagnostic-3))
    (test-assert (equal (issued-by (first (problems obj-w-learning))) diagnostic-3))
    (setf (problems obj-w-learning) nil)

    ;; 2. notify with unknown :trigger 
    ;;; (a) should trigger nothing
    (test-ok (notify-learning obj-w-learning :trigger 'unknown))
    (test-assert (null (problems obj-w-learning)))

    ;; 3. notify with :trigger test-diagnostic-1 
    ;;; (a) should trigger diagnostic-1
    ;; (b) testing whether setting :trigger through new :initform works
    ;;; (c) testing whether trigger is passed correctly
    ;; (d) testing whether issued-by slot is set correctly
    (test-ok (notify-learning obj-w-learning :trigger 'test-diagnostic-1))
    (test-assert (= 1 (length (problems obj-w-learning))))
    (test-assert (equal (get-data (first (problems obj-w-learning)) 'test-field) 'test-diagnostic-1))
    (test-assert (equal (get-data (first (problems obj-w-learning)) 'trigger)
                        'test-diagnostic-1))
    (test-assert (equal (issued-by (first (problems obj-w-learning))) diagnostic-1))
    ; not setting problems back to nil (to test appending)

    ;; 4. notify with test-diagnostic-2 
    ;;; (a) should trigger diagnostic-2
    ;; (b) mainly testing whether passing extra keywords works
    (test-ok (notify-learning obj-w-learning
                              :trigger 'test-diagnostic-2
                              :test-data 'foo))
    (test-assert (length= (problems obj-w-learning) 2))
    (let ((problem (find 'test-diagnostic-2 (problems obj-w-learning)
                         :key #'(lambda (problem) (get-data problem 'test-field)))))
      (test-assert (equal (get-data problem 'test-field) 'test-diagnostic-2))
      (test-assert (equal (get-data problem 'test-data) 'foo)) ;; this is the important one
      (test-assert (equal (issued-by problem) diagnostic-2)))

    ;; 5. notify with test-diagnostic-4 (should trigger diagnostic-4)
    (add-diagnostic obj-w-learning diagnostic-4)
    (test-ok (notify-learning obj-w-learning
                              :trigger 'test-diagnostic-4
                              :unknown-key 'should-be-ignored))
    ;; (a) the trigger should trigger diagnose method for diagnostic-4
    ;;; (b) and it can ignore unkown keyword parameters
    ;; (c) furthermore this diagnostic will return a string
    ;; "string-problem" instead of a problem instance
    (test-assert (= 3 (length (problems obj-w-learning))))
    (test-assert (find "string-problem" (problems obj-w-learning) :test #'equal))
    
    ;; 6. notify with test-diagnostic-5
    ;;; (a) does not have a diagnose method should thus call default and warn
    (add-diagnostic obj-w-learning diagnostic-5)
    (test-condition (notify-learning obj-w-learning :trigger 'test-diagnostic-5))
    (test-assert (= 3 (length (problems obj-w-learning))))))

;; (test-diagnostics)

(defclass real-problem (problem)
  ())

(defclass test-repair (repair)
  ((ran :initarg :ran :accessor ran :initform nil)))

(defclass empty-repair (test-repair)
  ((ran :accessor ran :initform nil)))

(defclass universal-repair (test-repair)
  ((trigger :initform 'universal)))

(defmethod repair ((r universal-repair)
                   (problem (eql nil))
                   object-w-learning &key &allow-other-keys)
  (declare (ignore object-w-learning))
  (setf (ran r) (if (ran r) (incf (ran r)) 1))
  t)

(defclass repair-no-trigger-yes-problem (test-repair)
  ())

(defmethod repair ((r repair-no-trigger-yes-problem)
                   (problem string)
                   object-w-learning &key &allow-other-keys)
  (declare (ignore problem object-w-learning))
  (setf (ran r) (if (ran r) (incf (ran r)) 1))
  t)

(defmethod repair ((r repair-no-trigger-yes-problem)
                   (problem symbol)
                   object-w-learning &key &allow-other-keys)
  (declare (ignore object-w-learning))
  (unless (null problem)
    (setf (ran r) (if (ran r) (incf (ran r)) 1))
    t))

(defmethod repair ((r repair-no-trigger-yes-problem)
                   (problem real-problem)
                   object-w-learning &key &allow-other-keys)
  (declare (ignore problem object-w-learning))
  (setf (ran r) (if (ran r) (incf (ran r)) 1))
  t)

(defclass regular-repair (test-repair)
  ((trigger :initform 'regular-trigger)))

(defmethod repair ((r regular-repair)
                   (problem real-problem)
                   object-w-learning 
                   &key  extra-data-1 extra-data-2 &allow-other-keys)
  (declare (ignore object-w-learning))
  (set-data problem 'extra-data-1 extra-data-1)
  (set-data problem 'extra-data-2 extra-data-2)
  (setf (ran r) (if (ran r) (incf (ran r)) 1))
  t)

(defun reset-test-repairs (object-w-learning)
  (loop for r in (repairs object-w-learning)
        do (setf (ran r) nil)))

(deftest test-repairs ()
  ;; in order to test repairs we have to also create problems.
  (let* ((diagnostic (make-instance 'diagnostic))
         (empty-repair (make-instance 'empty-repair))
         (universal-repair (make-instance 'universal-repair))
         (repair-no-trigger-yes-problem (make-instance 'repair-no-trigger-yes-problem))
         (regular-repair (make-instance 'regular-repair))
         (string-problem "string-problem")
         (symbol-problem 'symbol-problem)
         (real-problem (make-instance 'real-problem :issued-by diagnostic))
         (object-w-learning (make-instance 'object-w-learning
                                           :repairs (list empty-repair))))

    ;; 1. testing a repair that has no trigger, nor trigger-problems
    ;; (a) with a trigger it should not call any repair
    (test-ok (notify-learning object-w-learning :trigger 'foo))
    (test-equal (find-all-if #'ran (repairs object-w-learning))
                nil)
    ;; there should be no difference if there are problems
    (setf (problems object-w-learning) (list string-problem symbol-problem real-problem))
    (test-ok (notify-learning object-w-learning :trigger 'foo))
    (test-equal (find-all-if #'ran (repairs object-w-learning))
                nil)
    (setf (problems object-w-learning) nil)

    ;; 2. test repair that has a trigger but no trigger-problems (universal repair)
    (test-ok (delete-repair object-w-learning 'empty-repair))
    (test-ok (add-repair object-w-learning universal-repair))
    ;; (a) calling without correct trigger should do nothing
    (test-ok (notify-learning object-w-learning))
    (test-equalp (find-all-if #'ran (repairs object-w-learning))
                 nil)
    ;; (b) calling it with :trigger 'universal should
    (notify-learning object-w-learning :trigger 'universal)
    (test-assert (ran universal-repair))
    (test-equalp (find-all-if #'ran (repairs object-w-learning))
                 (list universal-repair))
    (test-assert (= 1 (ran universal-repair)))
    (reset-test-repairs object-w-learning)

    (setf (problems object-w-learning) nil)
    (notify-learning object-w-learning :trigger 'unknown)
    (test-equal (find-all-if #'ran (repairs object-w-learning))
                nil)


    ;; 3. no trigger but there is a problem
    (reset-test-repairs object-w-learning)
    (test-assert (null (problems object-w-learning)))
    (test-ok (add-repair object-w-learning repair-no-trigger-yes-problem))
    ;; (a) without any problem nothing should happen
    (test-ok (notify-learning object-w-learning))
    (test-equal (find-all-if #'ran (repairs object-w-learning))
                nil)
    ;; (b) testing different types of individual problems
    (reset-test-repairs object-w-learning)
    (push string-problem (problems object-w-learning))
    (test-ok (notify-learning object-w-learning))
    (test-equalp  (find-all-if #'ran (repairs object-w-learning))
                  (list repair-no-trigger-yes-problem))
    (test-assert (= 1 (ran repair-no-trigger-yes-problem)))
    (reset-test-repairs object-w-learning)
    (push symbol-problem (problems object-w-learning))
    (test-ok (notify-learning object-w-learning))
    (test-equalp  (find-all-if #'ran (repairs object-w-learning))
                  (list repair-no-trigger-yes-problem))
    (test-assert (= 2 (ran repair-no-trigger-yes-problem)))
    (reset-test-repairs object-w-learning)
    (setf (problems object-w-learning) nil)
    (push symbol-problem (problems object-w-learning))
    (test-ok (notify-learning object-w-learning))
    (test-equalp  (find-all-if #'ran (repairs object-w-learning))
                  (list repair-no-trigger-yes-problem))
    (test-assert (= 1 (ran repair-no-trigger-yes-problem)))
    (reset-test-repairs object-w-learning)
    (push real-problem (problems object-w-learning))
    (test-ok (notify-learning object-w-learning))
    (test-equalp (find-all-if #'ran (repairs object-w-learning))
                 (list repair-no-trigger-yes-problem))
    (test-assert (= 2 (ran repair-no-trigger-yes-problem)))
    
    ;; 4. regular repair with extra data
    (reset-test-repairs object-w-learning)
    (setf (problems object-w-learning)
          (list real-problem))
    (setf (fixes real-problem) nil)
    (test-ok (add-repair object-w-learning regular-repair))
    (test-ok (notify-learning object-w-learning
                              :trigger 'regular-trigger
                              :extra-data-1 'foo 
                              :extra-data-2 'bar))
    (test-equalp (find-all-if #'ran (repairs object-w-learning))
                 (list regular-repair))
    (test-assert (= 1 (ran regular-repair)))
    (test-assert (eq (get-data real-problem 'extra-data-1) 'foo))
    (test-assert (eq (get-data real-problem 'extra-data-2) 'bar))))

;; (test-repairs)

(defun run-meta-layer-tests ()
  (test-instance-creation)
  (test-diagnostics)
  (test-repairs))

;; (run-meta-layer-tests)