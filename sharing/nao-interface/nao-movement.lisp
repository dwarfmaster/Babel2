
(in-package :nao-interface)

(export '(nao-go-posture nao-get-posture nao-set-joint nao-head-say nao-raise-arm
                         raise-left-and-speak raise-right-and-speak head-say-and-speak))

;; + nao go posture +

(defgeneric nao-go-posture (nao posture &key speed)
  (:documentation "Make the Nao go to a certain posture"))

(defmethod nao-go-posture :before ((nao nao) posture &key (speed 0.3))
  (unless (and (<= speed 1.0)
               (>= speed 0.0))
    (error (format nil "Specified an invalid speed ~a"
                   speed))))

(defmethod nao-go-posture ((nao nao) (posture (eql :sit)) &key (speed 0.3))
  (let* ((json (make-json 'posture :data `((action . "set")
                                           (posture . "Sit")
                                           (speed . ,speed))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :response :value 1 :test #'=)))

(defmethod nao-go-posture ((nao nao) (posture (eql :stand)) &key (speed 0.3))
  (let* ((json (make-json 'posture :data `((action . "set")
                                           (posture . "Stand")
                                           (speed . ,speed))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :response :value 1 :test #'=)))

(defmethod nao-go-posture ((nao nao) (posture (eql :stand-init)) &key (speed 0.3))
  (let* ((json (make-json 'posture :data `((action . "set")
                                           (posture . "StandInit")
                                           (speed . ,speed))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :response :value 1 :test #'=)))

(defmethod nao-go-posture ((nao nao) (posture (eql :stand-zero)) &key (speed 0.3))
  (let* ((json (make-json 'posture :data `((action . "set")
                                           (posture . "StandZero")
                                           (speed . ,speed))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :response :value 1 :test #'=)))

(defmethod nao-go-posture ((nao nao) (posture (eql :crouch)) &key (speed 0.3))
  (let* ((json (make-json 'posture :data `((action . "set")
                                           (posture . "Crouch")
                                           (speed . ,speed))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :response :value 1 :test #'=)))

;; + nao get posture +

(defgeneric nao-get-posture (nao)
  (:documentation "Get the current posture of the Nao"))

(defmethod nao-get-posture ((nao nao))
  (let* ((json (make-json 'posture :data '((action . "get"))))
         (response (nao-send-http nao json)))
    (get-response-key response :key :posture)))

;; + nao set joint +

(defgeneric nao-set-joint (nao region joint &key value speed)
  (:documentation "Sets the joints in the head to the given value.
                   Also, checks if the value is within a safe range for the Nao!"))

(defmethod nao-set-joint :before ((nao nao) region joint &key value (speed 0.3))
  (declare (ignorable value))
  (unless (and (<= speed 1.0)
               (>= speed 0.0))
    (error (format nil "Specified an invalid speed ~a"
                   speed))))

(defmethod nao-set-joint ((nao nao) (region (eql :head)) (joint (eql :head-pitch)) &key value (speed 0.3))
  (if (and (< value 0.51) (> value -0.67))
    (let* ((json (make-json 'set-joint :data `((joint . "HeadPitch")
                                               (value . ,value)
                                               (speed . ,speed))))
           (response (nao-send-http nao json)))
      (test-response-key response :key :response :value 1 :test #'=))
    (error (format nil "The value ~a is out of range for the joint ~a"
                   value
                   joint))))

(defmethod nao-set-joint ((nao nao) (region (eql :head)) (joint (eql :head-yaw)) &key value (speed 0.3))
  (if (and (< value 2.08) (> value -2.08))
    (let* ((json (make-json 'set-joint :data `((joint . "HeadYaw")
                                               (value . ,value)
                                               (speed . ,speed))))
           (response (nao-send-http nao json)))
      (test-response-key response :key :response :value 1 :test #'=))
    (error (format nil "The value ~a is out of range for the joint ~a"
                   value
                   joint))))


;; + nao head say +

(defgeneric nao-head-say (nao yesno)
  (:documentation "Say yes or no using Nao's head movement"))

(defmethod nao-head-say ((nao nao) (yesno (eql :yes)))
  (let* ((json (make-json 'move-head :data `((yesno . "yes"))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :response :value 1 :test #'=)))

(defmethod nao-head-say ((nao nao) (yesno (eql :no)))
  (let* ((json (make-json 'move-head :data `((yesno . "no"))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :response :value 1 :test #'=)))

;; + nao raise arm +

(defgeneric nao-raise-arm (nao arm)
  (:documentation "Makes the Nao point using left, right or both arms"))

(defmethod nao-raise-arm ((nao nao) (arm (eql :left)))
  (let* ((json (make-json 'raise-arm :data '((arm . "LArm"))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :response :value 1 :test #'=)))

(defmethod nao-raise-arm ((nao nao) (arm (eql :right)))
  (let* ((json (make-json 'raise-arm :data '((arm . "RArm"))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :response :value 1 :test #'=)))

(defmethod nao-raise-arm ((nao nao) (arm (eql :both)))
  (let* ((json (make-json 'raise-arm :data '((arm . "Both"))))
          (response (nao-send-http nao json)))
    (test-response-key response :key :response :value 1 :test #'=)))

;; These methods are hacked together
(defgeneric raise-left-and-speak (nao speech)
  (:documentation "Point and speak simultaneously"))
(defgeneric raise-right-and-speak (nao speech)
  (:documentation "Point and speak simultaneously"))

(defmethod raise-left-and-speak ((nao nao) speech)
  (let* ((json (make-json 'point-speak :data `((arm . "LArm") (speech . ,speech))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :speech :value speech :test #'string=)))

(defmethod raise-right-and-speak ((nao nao) speech)
  (let* ((json (make-json 'point-speak :data `((arm . "RArm") (speech . ,speech))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :speech :value speech :test #'string=)))

(defmethod head-say-and-speak ((nao nao) yesno speech &key)
  (let* ((json (make-json 'head-speak :data `((yesno . ,yesno) (speech . ,speech))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :speech :value speech :test #'string=)))

