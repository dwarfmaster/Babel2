(in-package :utils)

(export '(start-timer reset-timer pause-timer read-timer seconds-to-hours-minutes-seconds))
(defparameter *timers* nil)

(defun reset-timer (timer)
  (let ((timer-cons (assq timer *timers*)))
    (if timer-cons 
      (and (setf (cdr timer-cons) (cons 0 0)) t)
      (and (push (cons timer (cons 0 0)) *timers*) t))))

(defun start-timer (timer)
  (let ((timer-cons (assq timer *timers*)))
    (if timer-cons
      (setf (cdr (cdr timer-cons)) (get-internal-run-time))
      (error "Timer ~a does not exist" timer))))

(defun pause-timer (timer)
  (let* ((time (get-internal-run-time))
         (timer-cons (assq timer *timers*)))
    (if timer-cons
      (setf (car (cdr timer-cons)) (+ (car (cdr timer-cons))
                                      (- time (cdr (cdr timer-cons)))))
      (error "Timer ~a does not exist" timer))))

(defun read-timer (timer)
  (let ((timer-cons (assq timer *timers*)))
    (if timer-cons
      (car (cdr timer-cons))
      (error "Timer ~a does not exist" timer))))



(defun seconds-to-hours-minutes-seconds (n)
  "This function converts seconds into hours, minutes and seconds."
  (let* ((hours (floor (/ n 3600)))
         (hour-rest (mod n 3600))
         (minutes (floor (/ hour-rest 60)))
         (seconds (mod hour-rest 60)))
    (values hours minutes seconds)))