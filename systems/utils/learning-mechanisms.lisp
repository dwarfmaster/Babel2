;;;
;;; File: learning-mechanisms.lisp
;;;
;;; Definition of base classes for diagnostics, problems and repair strategies.
;;;

(in-package :utils)

;; ############################################################################
;; diagnostic
;; ----------------------------------------------------------------------------

(export '(diagnostic learning-situations success repaired-thing))

(defclass diagnostic ()
  ((learning-situations
      :documentation "A situation narrows down the point of execution of a diagnostic. 
         The kinds of learning-situations depends on the level the diagnostic
         is operating on. It is a list of symbols."
      :type list
      :initform nil
      :initarg :learning-situations
      :reader learning-situations))
  (:documentation "Base class for diagnostics."))

;; ############################################################################
;; problem
;; ----------------------------------------------------------------------------

(export '(problem repaired-by issued-after data))

(defclass problem ()
  ((issued-after
    :documentation "This is a symbol or something else representing when this
    problem was signaled. When created by a process-diagnostic
    this is the name of the process or the process itself."
    ;; :type symbol
    :initarg :issued-after
    :initform nil
    :accessor issued-after)
   (repaired-by
    :documentation "This slot is automatically set to the repair-strategy
         that repaired it. When it's nil it means it's still unrepaired."
    :type t
    :initarg :repaired-by
    :initform nil
    :accessor repaired-by)
   (data :initarg data :accessor data
         :initform (make-blackboard)
         :documentation "learning data for keeping track how you repaired this"))
  (:documentation "A problem is created by diagnostics to signal
      a failure or some inefficiency."))

(defmethod get-data ((p problem) label &key &allow-other-keys)
  (get-data (data p) label))

(defmethod find-data ((p problem) label &key &allow-other-keys)
  (find-data (data p) label))

(defmethod push-data ((p problem) label data)
  (push-data (data p) label data))

;; ############################################################################
;; repair-strategy
;; ----------------------------------------------------------------------------

(export '(repair-strategy
	  triggered-by-problems
	  repaired repaired-thing
	  success-score))

(defclass repair-strategy ()
  ((triggered-by-problems
      :documentation "A list of problems (class-names of
      problems) this repair-strategy might be able to fix."
      :type list
      :reader triggered-by-problems
      :initform nil
      :initarg :triggered-by-problems)
   (learning-situations
      :documentation "A situation narrows down the point of
         execution of a repair-strategy. The kinds of
         learning-situations depends on the level the
         repair-strategy is operating on. It is a list of
         symbols."
      :type list
      :reader learning-situations
      :initform nil
      :initarg :learning-situations)
   (success-score
    :documentation "Resembles how successful the repair-strategy
    is. The higher the better. This will be used to sort when
    multiple repair-strategies can be triggered at the same
    time."
    :type number
    :accessor success-score
    :initform 1.0
    :initarg :success-score))
  (:documentation "Base class for a repair-strategy. Repair
  strategies are able to deal with a set of problems."))


;; ############################################################################
;; object-with-learning-mechanisms
;; ----------------------------------------------------------------------------

(export '(object-with-learning-mechanisms
	  diagnostics
          problems
	  repair-strategies))

(defclass object-with-learning-mechanisms ()
  ((diagnostics
      :documentation "A list of all diagnostics."
      :type list
      :accessor diagnostics
      :initarg :diagnostics
      :initform nil)
   (problems
      :documentation "All the problems that were reported by diagnostics."
      :type list 
      :initform nil 
      :initarg :problems 
      :accessor problems)
   (repair-strategies
      :documentation "A list of repair-strategies."
      :type list
      :accessor repair-strategies
      :initarg :repair-strategies
      :initform nil))
  (:documentation "A helper class that provides derived classes
  with diagnostics and repair strategies"))

;; ----------------------------------------------------------------------------

(export '(add-diagnostic
	  delete-diagnostic
	  add-repair-strategy
	  delete-repair-strategy
	  unsolved-problems))

(defgeneric add-diagnostic (object diagnostic)
  (:documentation "Adds a diagnostic to an object."))

(defgeneric delete-diagnostic (object diagnostic)
  (:documentation "Deletes a diagnostic from an object"))

(defgeneric add-repair-strategy (object repair-strategy)
  (:documentation "Adds a repair strategy to an object."))

(defgeneric delete-repair-strategy (object repair-strategy)
  (:documentation "Deletes a repair strategy from an object"))

(defgeneric unsolved-problems (object)
  (:documentation "Returns all the problems of object that are not yet
  repaired."))

(defmethod unsolved-problems ((object object-with-learning-mechanisms))
  (loop for problem in (problems object)
     unless (repaired-by problem)
     collect problem))

;; ----------------------------------------------------------------------------

(defmethod add-diagnostic ((object object-with-learning-mechanisms)
			   (diagnostic diagnostic))
  (let ((previous-diagnostic (find (class-of diagnostic) (diagnostics object)
				   :key #'class-of)))
    (when previous-diagnostic
      (warn "Replacing diagnostic ~a in ~a." diagnostic object)
      (delete-diagnostic object previous-diagnostic))
    (push diagnostic (diagnostics object))))

(defmethod delete-diagnostic ((object object-with-learning-mechanisms)
			      (diagnostic diagnostic))
  (setf (diagnostics object) (delete diagnostic (diagnostics object))))

(defmethod delete-diagnostic ((object object-with-learning-mechanisms)
			      (class-name symbol))
  (setf (diagnostics object)
	(delete-if #'(lambda (diagnostic)
		       (eq (class-name (class-of diagnostic))
			   class-name))
		   (diagnostics object))))

(defmethod add-repair-strategy ((object object-with-learning-mechanisms) 
				(repair-strategy repair-strategy))
  (let ((previous-repair-strategy (find (class-of repair-strategy)
					(repair-strategies object) 
					:key #'class-of)))
    (when previous-repair-strategy
      (warn "Replacing repair-strategy ~a in ~a." repair-strategy object)
      (delete-repair-strategy object previous-repair-strategy))
    (push repair-strategy (repair-strategies object))))

(defmethod delete-repair-strategy ((object object-with-learning-mechanisms) 
				   (repair-strategy repair-strategy))
  (setf (repair-strategies object) (delete repair-strategy (repair-strategies object))))

;; ----------------------------------------------------------------------------

(defmethod copy-object-content ((source diagnostic) (destination diagnostic))
  (setf (slot-value destination 'learning-situations) (learning-situations source)))

(defmethod copy-object-content ((source problem) (destination problem))
  (setf (slot-value destination 'repaired-by) (repaired-by source))
  (setf (slot-value destination 'issued-after) (issued-after source)))

(defmethod copy-object-content ((source repair-strategy) (destination repair-strategy))
  (setf (slot-value destination 'learning-situations) (learning-situations source))
  (setf (slot-value destination 'triggered-by-problems) (triggered-by-problems source))
  (setf (slot-value destination 'success-score) (success-score source)))

(defmethod copy-object-content ((source object-with-learning-mechanisms)
				(destination object-with-learning-mechanisms))
  (setf (slot-value destination 'diagnostics)
	(copy-object (diagnostics source)))
  (setf (problems destination) 
        (copy-object (problems source)))
  (setf (slot-value destination 'repair-strategies)
	(copy-object (repair-strategies source))))

;; ----------------------------------------------------------------------------

(defmethod print-object ((diagnostic diagnostic) stream)
  (if *print-pretty* 
      (pprint-logical-block (stream nil)
	(format stream "<diagnostic:~:_ learning-situations: ~:w>"
		(learning-situations diagnostic)))
      (call-next-method)))

(defmethod print-object ((problem problem) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
	(format stream "<problem:~:_ repaired-by: ~:w,~:_ issued-after: ~:w>" (repaired-by problem) (issued-after problem)))
      (call-next-method)))

(defmethod print-object ((repair-strategy repair-strategy) stream)
  (if *print-pretty* 
      (pprint-logical-block (stream nil)
	(format stream "<repair-strategy:~:_ triggered-by-problems: ~:w,~:_ learning-situations: ~:w,~:_ success-score: ~:w>"
		(triggered-by-problems repair-strategy) (learning-situations repair-strategy) (success-score repair-strategy)))
      (call-next-method)))

(defmethod print-object ((object object-with-learning-mechanisms) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
	(format stream "<object-with-learning-mechanisms:~:_ diagnostics: ~:w,~:_ repair-strategies: ~:w,~:_ problems: ~:w>"
                (diagnostics object) (repair-strategies object) (problems object)))
      (call-next-method)))

;; ############################################################################
