(in-package :nao-interface)

(export '(nao-speak start-speech-recognition stop-speech-recognition))

(defgeneric nao-speak (nao speech &key)
  (:documentation "Make the Nao say something"))
(defgeneric start-speech-recognition (nao vocabulary &key)
  (:documentation "Start the speech recognition process, given a certain vocabulary of words"))
(defgeneric stop-speech-recognition (nao subscriber &key)
  (:documentation "Stop the speech recognition process and return the detected word(s)"))

;; TO DO: make the robot speak slower. Insert \\rspd=x\\ into the string and let x be a keyword argument
(defmethod nao-speak ((nao nao) speech &key)
  ;; This is the same method as robot-interface:speak
  (let ((json (make-json 'speak :data `((speech . ,speech)))))
    (curl json :host (server-host nao) :port (server-port nao))))

(defmethod start-speech-recognition ((nao nao) vocabulary &key)
  (let* ((json (make-json 'speech-recognition :data `((action . "start") (vocabulary . ,vocabulary))))
         (response (curl json :host (server-host nao) :port (server-port nao))))
    (when (assoc :subscriber response)
      (rest (assoc :subscriber response)))))

(defmethod stop-speech-recognition ((nao nao) subscriber &key)
  (let* ((json (make-json 'speech-recognition :data `((action . "stop") (subscriber . ,subscriber))))
         (response (curl json :host (server-host nao) :port (server-port nao))))
    (when (assoc :detected response)
      (rest (assoc :detected response)))))