
(in-package :nao-interface)

(export '(detect-head-touch head-yes-or-no))

(defgeneric detect-head-touch (nao region &key)
  (:documentation "Detect if the Nao's head is touched. Regian can be 'Front', 'Middle' or 'Rear'"))
(defgeneric head-yes-or-no (nao &key)
  (:documentation "Detect if the Nao's head is touched in the front or in the back"))

(defmethod detect-head-touch ((nao nao) region &key)
  (let* ((json (make-json 'head-touch :data `((action . "detect") (region . ,region))))
         (response (curl json :host (server-host nao) :port (server-port nao))))
    (when (assoc :touch response)
      (when (= (rest (assoc :touch response)) 1)
        t))))

(defmethod head-yes-or-no ((nao nao) &key)
  (let* ((json (make-json 'head-touch :data '((action . "front-back"))))
         (response (curl json :host (server-host nao) :port (server-port nao))))
    (when (assoc :detected response)
      (= (rest (assoc :detected response)) 1))))