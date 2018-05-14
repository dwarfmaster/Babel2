
(in-package :nao-interface)

(export '(nao
          start-nao-server stop-nao-server stop-all-nao-servers
          test-server-connection nao-scp-get
          clear-nao-recordings clear-local-recordings
          nao-clear-all))

;;;;;;;;;;;;;;;;;
;; Nao Servers ;;
;;;;;;;;;;;;;;;;;

;; This will keep track of the running nao containers and their ports and container names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *nao-servers* nil
  "A list of the form ((ip port container-name) (ip port container-name) ...)")

(defun nao-servers ()
  *nao-servers*)

(defun port-occupied? (server-port)
  "Check if a port is already occupied"
  (find server-port (nao-servers) :key #'second :test #'equalp))

(defun ip-occupied? (nao-ip)
  "Check if an IP address is already occupied"
  (find nao-ip (nao-servers) :key #'first :test #'string=))

(defun push-nao-server (nao-ip server-port container-name)
  "Pushes a new nao-container to the list of running containers"
  (push (list nao-ip server-port container-name) *nao-servers*))

(defun pop-nao-server (entry)
  "Pops a nao-container from the list of running containers"
  ;; Entry = (ip port container-name)
  (setf *nao-servers*
        (remove-if (lambda (elem)
                     (reduce (lambda (x y) (and x y))
                             (mapcar #'equalp elem entry)))
                   (nao-servers))))

;;;;;;;;;;;;;;;;;;;;;
;; Nao Robot Class ;;
;;;;;;;;;;;;;;;;;;;;;

(defclass nao ()
  ((ip :initarg :ip :type string :accessor ip :initform ""
       :documentation "IP address of the nao")
   (port :initarg :port :type string :accessor port :initform "9559"
         :documentation "Port number of the nao")
   (username :initarg :username :type string :accessor username :initform "nao"
             :documentation "Username of the nao")
   (password :initarg :password :type string :accessor password :initform "nao"
             :documentation "Password of the nao")
   (server-host :initarg :server-host :type string :accessor server-host :initform "localhost"
                :documentation "Host of the nao server")
   (server-port :initarg :server-port :type string :accessor server-port :initform ""
                :documentation "Port to which the nao server should listen")
   (container-name :initarg :container-name :initform nil :accessor container-name
                   :documentation "Name of the Docker container of this Nao"))
  (:documentation "Nao robot class"))

(defmethod initialize-instance :after ((nao nao) &key (connect-automatically t))
  (when connect-automatically
    (let ((container-name (format nil "nao-~a-~a" (ip nao) (server-port nao))))
      (setf (container-name nao) container-name)
      (start-nao-server nao))))

(defun make-nao (&key ip server-port
                      (port "9559")
                      (username "nao")
                      (password "nao")
                      (server-host "localhost")
                      (connect-automatically t))
  (make-instance 'nao
                 :ip ip
                 :port port
                 :username username
                 :password password
                 :server-host server-host
                 :server-port server-port
                 :connect-automatically connect-automatically))


;; Starting and stopping nao servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric start-nao-server (nao &key test-connection)
  (:documentation "Make connection to this nao"))
(defgeneric stop-nao-server (nao &key)
  (:documentation "Stop the connection to the nao"))

(defmethod start-nao-server ((nao nao)
                             &key (test-connection t))
  (let ((exists? (not (string= (first (exec-and-return "docker" "container" "inspect" (container-name nao))) "[]"))))
    (cond ((ip-occupied? (ip nao))
           (error (format nil "The IP address ~a is already in use" (ip nao))))
          ((port-occupied? (server-port nao))
           (error (format nil "The port number ~a is already in use" (server-port nao))))
          (t
            (if exists?
                ;; Container does exist, make it run again
                (run-prog "docker" :args `("start" ,(container-name nao)))
                ;; Container does not yet exist, create one
                (run-prog "docker" :args `("run" "-it" "-d"
                                                 "-p" ,(format nil "~a:80" (server-port nao))
                                                 "-v" ,(format nil "~a:/naoqi/src" (babel-pathname :directory '("sharing" "nao-interface" "python-server")))
                                                 "-v" ,(format nil "~a:/naoqi/src/img" (babel-pathname :directory '(".tmp" "nao-img")))
                                                 "--name" ,(container-name nao)
                                                 "naoqi-python")))
            ;; Push to the running containers
            (push-nao-server (ip nao) (server-port nao) (container-name nao))
            ;; Give some time to start the container
            (sleep 1)
            ;; Start the nao server inside the docker container
            (run-prog "docker" :args `("exec" "-d"
                                              ,(container-name nao)
                                              "/usr/bin/python" "/naoqi/src/nao_server.py"
                                              "--robot-ip" ,(ip nao)
                                              "--robot-port" ,(port nao)))
            (when test-connection
              (sleep 1)
              (test-server-connection nao))))))

(defmethod stop-nao-server ((nao nao) &key)
  "Stops the python server associated to the given nao instance. Updates *nao-servers*"
  (run-prog "docker" :args `("stop" ,(container-name nao)))
  (pop-nao-server (list (ip nao) (server-port nao) (container-name nao)))
  *nao-servers*)

(defun stop-all-nao-servers ()
  "Stops all known nao-servers."
  (when *nao-servers*
    (loop for entry in *nao-servers*
          do (run-prog "docker" :args `("stop" ,(third entry)))
          do (pop-nao-server entry)))
  *nao-servers*)
           

;; Testing the connection with nao
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric test-server-connection (nao &key silent)
  (:documentation "Returns t if communication with nao-server succeeded, nil if it failed. Use silent = t for quick checks."))

(defmethod test-server-connection ((nao nao)
                                   &key (silent nil))
  (let* ((message (make-json 'test-connection :data '((test-message . "test-server-connection"))))
         (response (nao-send-http nao message)))
    (unless silent
      (nao-send-http nao
                     (make-json 'speak :data `((speech . ,(format nil "Connected to Babel 2 at port ~a"
                                                                  (server-port nao))))))
      (warn "Did Nao speak? If not, check whether you are connected to the same WiFi network (CiscoNao) and that Docker is running!"))
    (test-response-key response :key :test-message
                       :value "test-server-connection"
                       :test #'string=)))

;; HTTP Request to Nao
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric nao-send-http (nao json)
  (:documentation "Send an http request to this nao"))

(defmethod nao-send-http ((nao nao) json)
  (let ((uri (format nil "http://~a:~a"
                     (server-host nao)
                     (server-port nao))))
    (send-http uri json)))

;; Getting files from nao
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric nao-scp-get (nao remote-file local-file)
  (:documentation "Gets remote-file from the Nao and stores it in local-file"))

(defmethod nao-scp-get ((nao nao)
                        remote-file
                        local-file)
  (utils::scp-get (ip nao)
                  (username nao)
                  (password nao)
                  remote-file
                  local-file))

;; Clear images from nao
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric clear-nao-recordings (nao &key dir)
  (:documentation "Remove all images from the Nao's hard drive"))

(defmethod clear-nao-recordings ((nao nao)
                                 &key (dir "/var/persistent/home/nao/recordings/cameras"))
  (utils::ssh-clear-dir (ip nao)
                        (username nao)
                        (password nao)
                        dir))

;; Clear images from nao-interface/.tmp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clear-local-recordings ()
  (exec-and-return "rm" (format nil "~a*" (babel-pathname :directory '("sharing" "robot-interface" "nao-interface" ".tmp")))))

;; General clear function
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun nao-clear-all ()
  (warn "Clearing all local recordings")
  (clear-local-recordings)
  (warn "Clearing all running nao servers")
  (setf *nao-servers* nil))
