
(in-package :meta-layer-learning)

;; ---------------------------------------------------------------------------

(export '(open-problem?))

(defgeneric open-problem? (problem))

(defmethod open-problem? (problem)
  (declare (ignore problem))
  t)

;; ---------------------------------------------------------------------------

(export '(trigger?))

(defgeneric trigger? (thing trigger &key &allow-other-keys)
  (:documentation "Whether the diagnostic is triggered in a certain
  situation."))

;; ---------------------------------------------------------------------------

(export '(diagnose))

(defgeneric diagnose (diagnostic object-w-learning &key trigger &allow-other-keys)
  (:documentation "Each diagnostic should implement method diagnose
  which is called when the contents of its :trigger slot
  matches (through trigger-diagnostic?) the trigger of a
  notification. Return: nil, a single element or a list of
  elements. Elements are not required to be of class problem."))

;; ---------------------------------------------------------------------------

(export '(repair))

(defgeneric repair (repair problem object-w-learning &key trigger &allow-other-keys)
  (:documentation "Every repair should implement this method. It is
  called whenever a notification is thrown which matches the trigger
  of the repair and when one of its trigger-problems is found
  in (problems obj-w-learning). If triggered-by-problems = nil it will
  only take the trigger into account. Should return a one value which
  we call a repair-result. This is not required to be of type
  repair-result."))

(defmethod repair (repair problem object-w-learning &key trigger &allow-other-keys)
  (declare (ignore repair problem object-w-learning trigger))
  nil)

;; ---------------------------------------------------------------------------

(export '(handle-fix))

(defgeneric handle-fix (fix repair problem object-w-learning 
                            &key &allow-other-keys))

(defmethod handle-fix (fix repair problem object-w-learning
                           &key &allow-other-keys)
  (declare (ignore fix repair problem object-w-learning))
  t)

;; ---------------------------------------------------------------------------

(export '(get-diagnostics))

(defgeneric get-diagnostics (object &key &allow-other-keys)
  (:documentation "gets the diagnostics from an object"))

;; ---------------------------------------------------------------------------

(export '(get-repairs))

(defgeneric get-repairs (object &key &allow-other-keys)
  (:documentation "gets the repairs from an object"))

;; ---------------------------------------------------------------------------

(export '(get-problems))

(defgeneric get-problems (object &key &allow-other-keys)
  (:documentation "gets the problems from an object"))

;; ---------------------------------------------------------------------------

(export '(add-problem))

(defgeneric add-problem (object problem &key &allow-other-keys)
  (:documentation "adds a problem to object"))

;; ---------------------------------------------------------------------------

(export '(notify-learning notify-learning-finished notify-learning-started
                          diagnose-finished repair-finished))

(define-event notify-learning-started (object t) (trigger t))

(define-event notify-learning-finished (object t) (trigger t)
              (problems t) (fixes t))

(define-event diagnose-finished (diagnostic diagnostic) (object t)
              (problems t))

(define-event repair-finished (repair repair) (object t)
              (fixes t))

(defgeneric notify-learning (object &key trigger))

(defmethod notify-learning (notified-object
                            &rest parameters
                            &key
                            trigger
                            omit-diagnostics omit-repairs
                            &allow-other-keys)
  "Checks all diagnostics and repairs in notified-object for the
   given :trigger and if a match is found calls the associated
   diagnose or repair method. For repairs also the problems are checked."
  (notify notify-learning-started notified-object trigger)
  (let ((old-problems (find-all-if #'open-problem?
                                   (get-problems notified-object)))
        new-problems
        new-fixes)
    ;; 1. run diagnostics and handle diagnosed problems
    (unless omit-diagnostics
      (loop for diagnostic in (get-diagnostics notified-object)
            when (trigger?
                  diagnostic trigger :notified-object notified-object)
            do (loop
                  with problems = (apply 'diagnose diagnostic notified-object
                                         :trigger trigger
                                         parameters)
                  for problem in (listify problems)
                  when (and (slot-exists-p problem 'issued-by)
                            (null (issued-by problem)))
                  do (setf (issued-by problem) diagnostic)
                  do
                    (add-problem notified-object problem)
                    (push problem new-problems)
                  finally 
                    (notify diagnose-finished diagnostic notified-object problems))))
    ;; 2. run repairs
    (unless omit-repairs
      (loop
       for problem in (cons nil (append new-problems old-problems))
       for continue = t
       do (loop
           for repair in (get-repairs notified-object)
           for (fixes continue?)
           = (multiple-value-list
              (when (apply 'trigger? repair trigger
                           :problem problem
                           :notified-object notified-object
                           parameters)
                (apply 'repair repair problem notified-object
                       :trigger trigger parameters)))
           when fixes
           do (loop for fix in (listify fixes)
                    do
                    (handle-fix fix repair problem notified-object)
                    (push fix new-fixes)
                 finally 
                   (notify repair-finished repair notified-object fixes))
           and do (setf continue continue?)
           while continue)))
    (notify notify-learning-finished notified-object trigger new-problems new-fixes)
    (values new-problems new-fixes)))