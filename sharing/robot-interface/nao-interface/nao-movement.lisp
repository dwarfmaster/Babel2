
(in-package :nao-interface)

(export '(go-to-posture get-posture set-head-joint head-say raise-left-arm raise-right-arm raise-both-arms
                        raise-left-and-speak raise-right-and-speak head-say-and-speak))

(defgeneric go-to-posture (nao posture &key)
  (:documentation "Make the Nao go to a certain posture"))
(defgeneric get-posture (nao &key)
  (:documentation "Get the current posture of the Nao"))
(defgeneric set-head-joint (nao joint value &key)
  (:documentation "Sets the joints in the head to the given value. Also, checks if the value is within a safe range for the Nao!"))
(defgeneric head-say (nao yesno &key)
  (:documentation "Say yes or no using Nao's head movement"))
(defgeneric raise-left-arm (nao &key)
  (:documentation "Raises the left arm"))
(defgeneric raise-right-arm (nao &key)
  (:documentation "Raises the right arm"))
(defgeneric raise-both-arms (nao &key)
  (:documentation "Raises both arms"))
(defgeneric raise-left-and-speak (nao speech &key)
  (:documentation "Point and speak simultaneously"))
(defgeneric raise-right-and-speak (nao speech &key)
  (:documentation "Point and speak simultaneously"))

(defmethod go-to-posture ((nao nao) posture &key (speed 0.3))
  (assert (and (<= speed 1.0)
               (>= speed 0.0)))
  (let* ((json (make-json 'posture :data `((action . "set") (posture . ,posture) (speed . ,speed)))))
    (send-and-test-response-key json :response 1
                                :host (server-host nao) :port (server-port nao)
                                :test #'=)))

(defmethod get-posture ((nao nao) &key)
  (let* ((json (make-json 'posture :data '((action . "get")))))
    (send-and-get-response-key json :posture
                               :host (server-host nao) :port (server-port nao))))

(defun set-joint (nao joint value &key (speed 0.3))
  "Move a joint. Not to be used directly, since this function does not check if the value for the joint is within the proper range"
  (let* ((json (make-json 'set-joint :data `((joint . ,joint) (value . ,value) (speed . ,speed)))))
    (send-and-test-response-key json :response 1
                                :host (server-host nao) :port (server-port nao)
                                :test #'=)))

(defmethod set-head-joint ((nao nao) joint value &key (speed 0.3))
  (assert (and (<= speed 1.0)
               (>= speed 0.0)))
  (cond ((string= joint "HeadPitch")
         (if (and (< value 0.51)
                  (> value -0.67))
           (set-joint nao joint value :speed speed)
           (error (format nil "The value ~a is out of range for the joint ~a" value joint))))
        ((string= joint "HeadYaw")
         (if (and (< value 2.08)
                  (> value -2.08))
           (set-joint nao joint value :speed speed)
           (error (format nil "The value ~a is out of range for the joint ~a" value joint))))))

(defmethod head-say ((nao nao) yesno &key)
  (let* ((json (make-json 'move-head :data `((yesno . ,yesno)))))
    (send-and-test-response-key json :response 1
                                :host (server-host nao) :port (server-port nao)
                                :test #'=)))

(defmethod raise-left-arm ((nao nao) &key)
  (let* ((json (make-json 'raise-arm :data '((arm . "LArm")))))
    (send-and-test-response-key json :response 1
                                :host (server-host nao) :port (server-port nao)
                                :test #'=)))

(defmethod raise-right-arm ((nao nao) &key)
  (let* ((json (make-json 'raise-arm :data '((arm . "RArm")))))
    (send-and-test-response-key json :response 1
                                :host (server-host nao) :port (server-port nao)
                                :test #'=)))

(defmethod raise-both-arms ((nao nao) &key)
  (let* ((json (make-json 'raise-arm :data '((arm . "Both")))))
    (send-and-test-response-key json :response 1
                                :host (server-host nao) :port (server-port nao)
                                :test #'=)))

;; These methods are hacked together
(defmethod raise-left-and-speak ((nao nao) speech &key)
  (let* ((json (make-json 'point-speak :data `((arm . "LArm") (speech . ,speech))))
         (response (curl json :host (server-host nao) :port (server-port nao))))
    (when (assoc :speech response)
      (when (string= (rest (assoc :speech response)) speech)
        t))))

(defmethod raise-right-and-speak ((nao nao) speech &key)
  (let* ((json (make-json 'point-speak :data `((arm . "RArm") (speech . ,speech))))
         (response (curl json :host (server-host nao) :port (server-port nao))))
    (when (assoc :speech response)
      (when (string= (rest (assoc :speech response)) speech)
        t))))

(defmethod head-say-and-speak ((nao nao) yesno speech &key)
  (let* ((json (make-json 'head-speak :data `((yesno . ,yesno) (speech . ,speech))))
         (response (curl json :host (server-host nao) :port (server-port nao))))
    (when (assoc :speech response)
      (when (string= (rest (assoc :speech response)) speech)
        t))))

