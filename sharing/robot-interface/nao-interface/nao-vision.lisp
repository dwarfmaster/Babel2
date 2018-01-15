
(in-package :nao-interface)

(export '(take-picture-nao nao-analyze-scene))

(defgeneric take-picture-nao (nao &key)
  (:documentation "Takes a picture on the Nao"))
(defgeneric nao-analyze-scene (nao filename &key)
  (:documentation "Analyze the scene using opencv"))

(defmethod take-picture-nao ((nao nao) &key)
  (break "Please change the world and carry on.")
  (curl (make-json 'vision :data '((action . "capture")))
        :host (server-host nao)
        :port (server-port nao)))

(defmethod nao-analyze-scene ((nao nao) filename &key)
    (curl (make-json 'vision :data `((action . "analyze") (filename . ,filename)))
          :host (server-host nao)
          :port (server-port nao)))

