
(in-package :irl)


(defparameter *test-ontology* nil)

(defclass quantity (entity)
  ((n :type integer :initarg :n :accessor n)))

(defmethod equal-entity ((q-1 quantity)(q-2 quantity))
  (= (n q-1) (n q-2)))

#+:hunchentoot-available-on-this-platform
(defmethod make-html-for-entity-details ((quantity quantity) &key &allow-other-keys)
  `(((div :class "entity-detail") "n = " ,(n quantity))))

(defun make-quantity (n)
  (make-instance 'quantity :id (intern (format nil "~:@(~r~)" n)) :n n))


(defun add-quantities-to-ontology (blackboard)
  (set-data blackboard 'quantities 
            (append 
             (loop for i from 1 to 10 collect (make-quantity i))
             (list (make-quantity 20)))))



(defclass apples-set (entity)
  ((apples :type list :initarg :apples :accessor apples)))

(defun make-apples-set (number-of-apples)
  (make-instance 'apples-set :id (make-id (mkstr number-of-apples '-apples))
                 :apples (loop for n from 1 to number-of-apples collect 'apple)))

(defmethod equal-entity ((set-1 apples-set)(set-2 apples-set))
  (= (length (apples set-1)) (length (apples set-2))))

#+:hunchentoot-available-on-this-platform
(defmethod make-html-for-entity-details ((set apples-set) &key &allow-other-keys)
  `(((div :class "entity-detail")
     ,@(loop for i from 1 to (length (apples set))
          append `("&#x274d; " ,(if (= (mod i 10) 0) '((br)) ""))))
    ((div :class "entity-detail") "number of apples: " ,(length (apples set)))))



(defprimitive pick-apples ((set apples-set) (amount quantity))

  ((set amount =>)
   (= (length (apples set)) (n amount)))

  ((set => amount)
   (let ((amount (find (length (apples set))
                       (get-data ontology 'quantities)
                       :key #'n :test #'=)))
     (when amount (bind (amount 1.0 amount)))))

  ((amount => set)
   (bind (set 1.0 (make-apples-set (n amount))))))


(defprimitive add-apples ((sum apples-set) (amount quantity) (set apples-set))
  ((sum amount set =>)
   (= (length (apples sum)) (+ (n amount) (length (apples set)))))
  
  ((set amount => sum)
   (bind (sum 1.0 (make-apples-set (+ (n amount) (length (apples set)))))))
  
  ((sum => amount set)
   (loop for amount 
      in (reverse (get-data ontology 'quantities))
      for rest = (- (length (apples sum)) (n amount))
      when (> rest 0)
      do (bind (amount 1.0 amount) (set 1.0 (make-apples-set rest)))
      (return))))


(defprimitive multiply-apples ((sum apples-set) (factor quantity) 
                               (amount quantity) (set apples-set))
  ((sum factor amount set =>)
   (= (length (apples sum)) (+ (* (n factor) (n amount))
                               (length (apples set)))))

  ((set factor amount => sum)
   (bind (sum 1.0 (make-apples-set (+ (* (n factor) (n amount))
                                      (length (apples set)))))))

  ((sum => factor amount set)
   (loop with quantities
        = (reverse (get-data ontology 'quantities))
        for amount in quantities
        thereis (loop for factor in quantities
                   for rest = (- (length (apples sum)) (* (n factor) (n amount)))
                   when (> rest 1)
                   do
                   (bind (factor 1.0 factor) (amount 1.0 amount)
                            (set 1.0 (make-apples-set rest)))
                   (return t)))))


(progn
  (setf *test-ontology* (make-blackboard))
  (add-quantities-to-ontology *test-ontology*))




;;(activate-monitor trace-irl-in-web-browser-verbose)

;; produce "97 apples"
(evaluate-irl-program `((bind apples-set ?apples ,(make-apples-set 97))
                        (multiply-apples ?apples ?factor ?amount-1 ?apples-2)
                        (add-apples ?apples-2 ?amount-2 ?apples-3)
                        (pick-apples ?apples-3 ?amount-3))
                      *test-ontology*)

;;(deactivate-all-monitors)


