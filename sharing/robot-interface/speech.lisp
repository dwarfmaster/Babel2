(in-package :robot-interface)

(export '(speak recognise-words))

(defmethod speak ((robot robot) speech &key)
  "Make the robot say something"
  (nao-speak robot speech))

(defmethod recognise-words ((robot robot) words &key)
  "Recognise the given list of words. To stop the speech recognition, touch the front of Nao's head"
  (let ((subscriber (start-speech-recognition robot words)))
    ; (sleep 10)
    (when (detect-head-touch robot "Middle")
      (stop-speech-recognition robot subscriber))))
