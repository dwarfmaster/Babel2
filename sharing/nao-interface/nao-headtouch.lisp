
(in-package :nao-interface)

(export '(nao-detect-touch nao-head-yes-no))

;; + nao detect touch +

(defgeneric nao-detect-touch (nao region sensor)
  (:documentation "Detect touch on one of the sensors of the Nao.
                   Region must be from '(:head :chest :hand :feet)
                   Sensor depends on Region"))

(defmethod nao-detect-touch ((nao nao) (region (eql :head)) (sensor (eql :front)))
  (let* ((json (make-json 'head-touch :data `((action . "detect") (region . "Front"))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :touch :value 1 :test #'=)))

(defmethod nao-detect-touch ((nao nao) (region (eql :head)) (sensor (eql :middle)))
  (let* ((json (make-json 'head-touch :data `((action . "detect") (region . "Middle"))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :touch :value 1 :test #'=)))

(defmethod nao-detect-touch ((nao nao) (region (eql :head)) (sensor (eql :rear)))
  (let* ((json (make-json 'head-touch :data `((action . "detect") (region . "Rear"))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :touch :value 1 :test #'=)))

;; + head yes or no +

(defgeneric nao-head-yes-no (nao)
  (:documentation "Detect if the Nao's head is touched in the front or in the back"))

(defmethod nao-head-yes-no ((nao nao))
  (let* ((json (make-json 'head-touch :data '((action . "front-back"))))
         (response (nao-send-http nao json)))
    (test-response-key response :key :detected :value 1 :test #'=)))