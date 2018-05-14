(in-package :nao-interface)

(export '(nao-speak nao-start-speech-rec nao-stop-speech-rec))

;; + nao speak +

(defgeneric nao-speak (nao speech &key speed)
  (:documentation "Make the Nao say something"))

(defmethod nao-speak ((nao nao) speech &key (speed 100))
  (let* ((speech (if (not (= speed 100))
                   (string-append (format nil "\\rspd=~a\\" speed) speech)
                   speech))
         (json (make-json 'speak :data `((speech . ,speech)))))
    (nao-send-http nao json)))

;; + nao speech recognition +

(defgeneric nao-start-speech-rec (nao vocabulary)
  (:documentation "Start the speech recognition process, given a certain vocabulary of words"))
(defgeneric nao-stop-speech-rec (nao subscriber)
  (:documentation "Stop the speech recognition process and return the detected word(s)"))

(defmethod nao-start-speech-rec ((nao nao) vocabulary)
  (let* ((json (make-json 'speech-recognition :data `((action . "start")
                                                      (vocabulary . ,vocabulary))))
         (response (nao-send-http nao json)))
    (get-response-key response :key :subscriber)))

(defmethod nao-stop-speech-rec ((nao nao) subscriber)
  (let* ((json (make-json 'speech-recognition :data `((action . "stop")
                                                      (subscriber . ,subscriber))))
         (response (nao-send-http nao json)))
    (get-response-key response :key :detected)))