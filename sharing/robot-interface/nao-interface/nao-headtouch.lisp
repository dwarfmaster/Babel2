
(in-package :nao-interface)

(export '(detect-head-touch head-yes-or-no))

(defgeneric detect-head-touch (nao region &key)
  (:documentation "Detect if the Nao's head is touched. Regian can be 'Front', 'Middle' or 'Rear'"))

(defmethod detect-head-touch ((nao nao) region &key)
  (let* ((json (make-json 'head-touch :data `((action . "detect") (region . ,region)))))
    (send-and-test-response-key json :touch 1
                                :host (server-host nao) :port (server-port nao)
                                :test #'=)))

(defgeneric head-yes-or-no (nao &key)
  (:documentation "Detect if the Nao's head is touched in the front or in the back"))

(defmethod head-yes-or-no ((nao nao) &key)
  (let* ((json (make-json 'head-touch :data '((action . "front-back")))))
    (send-and-test-response json :detected 1
                            :host (server-host nao) :port (server-port nao)
                            :test #'=)))