
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

;; TO DO: add speed keyword argument
(defmethod go-to-posture ((nao nao) posture &key)
  (let* ((json (make-json 'posture :data `((action . "set") (posture . ,posture))))
         (response (curl json :host (server-host nao) :port (server-port nao))))
    (when (assoc :response response)
      (when (= (rest (assoc :response response)) 1)
        t))))

(defmethod get-posture ((nao nao) &key)
  (let* ((json (make-json 'posture :data '((action . "get"))))
         (response (curl json :host (server-host nao) :port (server-port nao))))
    (when (assoc :posture response)
      (rest (assoc :posture response)))))

;; TO DO: add speed keyword argument
(defun set-joint (nao joint value)
  "Move a joint. Not to be used directly, since this function does not check if the value for the joint is within the proper range"
  (let* ((json (make-json 'set-joint :data `((joint . ,joint) (value . ,value))))
         (response (curl json :host (server-host nao) :port (server-port nao))))
    (when (assoc :response response)
      (when (= (rest (assoc :response response)) 1)
        t))))

(defmethod set-head-joint ((nao nao) joint value &key)
  (cond ((string= joint "HeadPitch")
         (if (and (< value 0.51)
                  (> value -0.67))
           (set-joint nao joint value)
           (error (format nil "The value ~a is out of range for the joint ~a" value joint))))
        ((string= joint "HeadYaw")
         (if (and (< value 2.08)
                  (> value -2.08))
           (set-joint nao joint value)
           (error (format nil "The value ~a is out of range for the joint ~a" value joint))))))

(defmethod head-say ((nao nao) yesno &key)
  (let* ((json (make-json 'move-head :data `((yesno . ,yesno))))
         (response (curl json :host (server-host nao) :port (server-port nao))))
    (when (assoc :response response)
      (when (= (rest (assoc :response response)) 1)
        t))))

;; TO DO: add speed keyword argument
(defmethod raise-left-arm ((nao nao) &key)
  (let* ((json (make-json 'raise-arm :data '((arm . "LArm"))))
         (response (curl json :host (server-host nao) :port (server-port nao))))
    (when (assoc :response response)
      (when (= (rest (assoc :response response)) 1)
        t))))

;; TO DO: add speed keyword argument
(defmethod raise-right-arm ((nao nao) &key)
  (let* ((json (make-json 'raise-arm :data '((arm . "RArm"))))
         (response (curl json :host (server-host nao) :port (server-port nao))))
    (when (assoc :response response)
      (when (= (rest (assoc :response response)) 1)
        t))))

;; TO DO: add speed keyword argument
(defmethod raise-both-arms ((nao nao) &key)
  (let* ((json (make-json 'raise-arm :data '((arm . "Both"))))
         (response (curl json :host (server-host nao) :port (server-port nao))))
    (when (assoc :response response)
      (when (= (rest (assoc :response response)) 1)
        t))))

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

