;; ************** Test Distribution **************
;; ** Simon Pauw
;; ***********************************************
;; (test-distribution zero-place-function      => the function that generates a distribution
;;                    number-of-trials         => the amount of trials
;;                    :test #'equality-test)   => the equality test

;; ******************* Example: ******************
;; (test-distribution #'(lambda () (random 5)) 1000 :test #'eq)
;; 0 => 215
;; 1 => 195
;; 2 => 168
;; 3 => 210
;; 4 => 207
;; ***********************************************


(in-package :utils)

(export '(test-distribution))


(defun test-distribution (fun trials &key (test #'eq))
  (let ((hash (make-hash-table :test test)))
    (dotimes (i trials)
      (inc-hash-value (funcall fun) hash))
    (maphash #'(lambda (x y) (format *standard-output* "~a => ~a~%" x y))
             hash)
    hash))

(defun inc-hash-value (item hash-table)
  (if (gethash item hash-table)
    (incf (gethash item hash-table))
    (setf (gethash item hash-table) 0)))
