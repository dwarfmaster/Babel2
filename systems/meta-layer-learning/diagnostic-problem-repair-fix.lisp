
(in-package :meta-layer-learning)

;; ###########################################################################
;; problem 
;; ---------------------------------------------------------------------------

(export '(problem issued-by fixes))

(defclass problem (blackboard)
  ((issued-by
    :documentation "The object that created this problem, i.e. the diagnostic."
    :initarg :issued-by
    :initform nil
    :accessor issued-by)
   (fixes
    :documentation "fixes of this problem"
    :initarg :fixes
    :initform nil
    :accessor fixes))
  (:documentation "A problem is created by diagnostics to signal a
      failure or some inefficiency. Exceptionally it can also be
      created by repairs that do not trigger on problems. A problem is
      also a blackboard so that it can store extra data. The issued-by
      slot is automatically set by method handle-diagnosed-problems.
      The fixes slot is automatically set to the return value of repair-object
      and repair-problem"))

;; ---------------------------------------------------------------------------

(defmethod open-problem? ((problem problem))
  (null (fixes problem)))

;; ---------------------------------------------------------------------------

(defmethod handle-fix (fix repair (problem problem) object-w-learning
                           &key &allow-other-keys)
  (declare (ignore repair object-w-learning))
  (push fix (fixes problem)))

;; ###########################################################################
;; diagnostic
;; ---------------------------------------------------------------------------

(export '(diagnostic trigger))

(defclass diagnostic (blackboard)
  ((trigger
    :documentation "A trigger specifies to which notification the
         diagnostic should respond. It can be a single symbol or a
         (nested) list of symbols. It is also possible to use a
         variable (a symbol starting with ?) which can match to any
         symbol or list. A trigger should respond to at least one
         notification somewhere, otherwise the diagnostic will never
         run"
    :initform nil
    :initarg :trigger
    :accessor trigger))
  (:documentation "Base class for diagnostics."))

(defmethod trigger? ((diagnostic diagnostic) trigger &key &allow-other-keys)
  (equalp (trigger diagnostic) trigger))

;; ###########################################################################
;; repair
;; ---------------------------------------------------------------------------

(export '(repair score trigger))

(defclass repair (blackboard)
  ((trigger
    :documentation "A trigger specifies to which notification the
         repair responds. It can be a single symbol or a (nested) list
         of symbols. It is also possible to use a variable (a symbol
         starting with ?) which can match to any symbol or
         list. Normally a trigger should respond to at least one
         notification somewhere."
    :initform nil
    :initarg :trigger
    :accessor trigger)
   (score :accessor score
          :initform 1.0
          :initarg :score))
  (:documentation "Base class for a repair."))

;; ---------------------------------------------------------------------------

(defmethod trigger? ((repair repair) trigger &key &allow-other-keys)
  (equalp (trigger repair) trigger))

;; ###########################################################################
;; fix
;; ---------------------------------------------------------------------------

(export '(fix restart-data))

(defclass fix (blackboard)
  ((issued-by :initarg :repair
              :initarg :issued-by
              :accessor issued-by
              :initform nil
              :documentation "pointer to the repair")
   (problem :initarg :problem :accessor problem :initform nil
            :documentation "pointer to the repaired problem")
   (restart-data :initarg :restart-data :accessor restart-data :initform nil
                 :documentation "restart and with what data (can be t also)"))
  (:documentation "Fixes are created by repairs"))

;; ---------------------------------------------------------------------------

(defgeneric restart-object (object data &key &allow-other-keys)
  (:documentation "overload for allowing to restart an object"))

(defmethod restart-object (object data &key &allow-other-keys)
  "default is empty"
  (declare (ignore object data))
  nil)

;; ---------------------------------------------------------------------------

(defmethod handle-fix ((fix fix) repair problem object-w-learning
                       &key &allow-other-keys)
  (call-next-method)
  (setf (issued-by fix) (or (issued-by fix) repair))
  (setf (problem fix) (or (problem fix) problem))
  (when (restart-data fix)
    (restart-object object-w-learning (restart-data fix) :fix fix)))

;; ###########################################################################
;; print-object
;; ---------------------------------------------------------------------------

(defmethod print-object ((diagnostic diagnostic) stream)
  (if *print-pretty* 
    (pprint-logical-block (stream nil)
      (format stream "<~(~:w~):~:_ trigger: ~:w>"
              (type-of diagnostic)
              (trigger diagnostic)))
    (call-next-method)))

(defmethod print-object ((repair repair) stream)
  (if *print-pretty* 
    (pprint-logical-block (stream nil)
      (format stream "<~(~:w~):~:_ trigger: ~:w>"
              (type-of repair)
              (trigger repair)))
    (call-next-method)))

(defmethod print-object ((problem problem) stream)
  (if *print-pretty* 
    (pprint-logical-block (stream nil)
      (format stream "<~(~:w~):~:_ issued-by: ~:w, ~:_fixes: ~:w>"
              (type-of problem)
              (issued-by problem)
              (fixes problem)))
    (call-next-method)))

(defmethod print-object ((fix fix) stream)
  (if *print-pretty* 
    (pprint-logical-block (stream nil)
      (format stream "<~(~:w~):~:_ issued-by: ~:w, ~:_restart-data: ~:w>"
              (type-of fix)
              (issued-by fix)
              ;(problem fix)
              (restart-data fix)))
    (call-next-method)))
