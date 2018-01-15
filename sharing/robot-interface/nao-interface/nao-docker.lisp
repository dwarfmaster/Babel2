(in-package :nao-interface)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is not used anymore. Now, Nao is a class of its own         ;;
;; This allows us to use several robots. See nao.lisp!                   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file implements and test the connection between Babel2 and the Nao Robot ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(*nao-robot-ip* start-nao-server stop-nao-server stop-all-nao-servers scp-get-nao clear-nao-recordings))


;; Setting a few parameters that can be customized in init-babel.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *nao-robot-ip*
  (if (boundp 'cl-user::*nao-robot-ip*)
      (eval 'cl-user::*nao-robot-ip*)
      "192.168.1.3")
  "IP address of the nao robot.")

(defparameter *nao-robot-username*
  (if (boundp 'cl-user::*nao-robot-username*)
      (eval 'cl-user::*nao-robot-username*)
      "nao")
  "The user name of the nao.")

(defparameter *nao-robot-password*
  (if (boundp 'cl-user::*nao-robot-password*)
      (eval 'cl-user::*nao-robot-password*)
      "nao")
  "The password for the user on the nao.")

(defparameter *nao-robot-port*
  (if (boundp 'cl-user::*nao-robot-port*)
      (eval 'cl-user::*nao-robot-port*)
      "9559")
  "Port on which the nao robot listens.")

(defparameter *nao-server-host*
  (if (boundp 'cl-user::*nao-server-host*)
      (eval 'cl-user::*nao-server-host*)
      "localhost")
  "Host on which the nao server runs/should run.")

(defparameter *nao-server-port*
  (if (boundp 'cl-user::*nao-server-port*)
      (eval 'cl-user::*nao-server-port*)
      "7850")
  "Port on which the nao server listens/should listen to http requests.")

;; This will keep track of the running nao containers and their ports and container names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *nao-servers* nil
  "An a-list of nao servers in the form ((port-1 . container-name-1) (port-2 . container-name-2))")

(defun nao-servers ()
  "Returns the list of nao-servers as ((port-1 . container-name-1) (port-2 . container-name-2))."
  *nao-servers*)

(defun nao-server (port)
  "Returns (port . container-name) if a nao-container is listening to port, nil otherwise"
  (assoc (parse-integer port) *nao-servers*))

(defun push-nao-server (port container-name)
  "Pushes a new nao-container to the list of running containers"
  (push (cons (parse-integer port) container-name) *nao-servers*))

(defun pop-nao-server (port)
  "Pops a nao-container from the list of running containers"
  (setf *nao-servers* (remove (assoc (parse-integer port) *nao-servers*) *nao-servers*)))

;; Starting and stopping nao servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-nao-server (&key (robot-ip *nao-robot-ip*)
                              (robot-port *nao-robot-port*)
                              (server-port *nao-server-port*)
                              (test-connection t))
  (let ((running? (nao-server server-port)))
    (when (null running?)
      (let* ((container-name (format nil "~a~a" "nao-container-" server-port))
             (exists? (exec-and-return "docker" "container" "inspect" (format nil "~a" container-name))))
        ;; Start up the docker container
        (if (string= (first exists?) "[]")
            ;; Container does not yet exist, create one
            (run-prog "docker" :args (list "run" "-it" "-d"
                                           "-p" (format nil "~a:80" server-port)
                                           "-v" (format nil "~a:/naoqi/src" (babel-pathname :directory '("sharing" "robot-interface" "nao-interface")))
                                           "--name" (format nil "~a" container-name)
                                           "naoqi-python"))
            ;; Container does exist, make it run again
            (run-prog "docker" :args (list "start" (format nil "~a" container-name))))
        ;; Push the (port . container-name) to the list of running containers
        (push-nao-server server-port container-name)
        ;; Give some time to start the docker container
        (sleep 1)
        ;; Start up the nao server inside the docker container
        (run-prog "docker" :args (list "exec" "-d"
                                       (format nil "~a" container-name)
                                       "/usr/bin/python" "/naoqi/src/nao_server.py"
                                       "--robot-ip" (format nil "~a" robot-ip)
                                       "--robot-port" (format nil "~a" robot-port)))))
    (when test-connection
      (sleep 1)
      (test-nao-server-connection))))

(defun stop-nao-server (&optional (port *nao-server-port*))
  "Stops the python server listening to the specified port. Updates *nao-servers*"
  (let ((container-name (rest (nao-server port))))
    (run-prog "docker" :args (list "stop" (format nil "~a" container-name)))
    (pop-nao-server port))
  *nao-servers*)

(defun stop-all-nao-servers ()
  "Stops all known nao-servers."
  (when *nao-servers*
    (loop for (port . container-name) in *nao-servers*
          do (stop-nao-server port)))
  *nao-servers*)

;; Getting files from nao
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scp-get-nao (remote-file local-file
                    &key (host *nao-robot-ip*)
                         (username *nao-robot-username*)
                         (password *nao-robot-password*))
  "Gets remote-file from Nao and stores it in local-file."
  (utils::scp-get host username password remote-file local-file)
  local-file)

(defun clear-nao-recordings (&key (host *nao-robot-ip*)
                                  (username *nao-robot-username*)
                                  (password *nao-robot-password*)
                                  (dir "/var/persistent/home/nao/recordings/cameras"))
  (utils::ssh-clear-dir host username password dir))

;; Testing the connection with nao
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-nao-server-connection (&key (host *nao-server-host*) (port *nao-server-port*) (silent nil))
  "Returns t if communication with nao-server succeeded, nil if it failed. Use silent t for quick checks."
  (let* ((message (encode-json-to-string-for-shell '((action . test-connection)
                                                     (data (test-message . "test-nao-server-connection")))))
         (response (exec-and-return "curl" (format nil "~a:~a" host port) "-s" "-d" message))
         (response-message (when response (decode-json-from-string (first response)))))
    (unless silent
      (exec-and-return "curl" (format nil "~a:~a" host port) "-s" "-d"
                       (encode-json-to-string-for-shell `((action . speak) (data (speech . ,(format nil "Connected to Babel 2 at port ~a" port))))))
      (warn "Did Nao speak? If not, check whether you are connected to the same WiFi network, and that docker is running.!"))
    (when (assoc :test-message response-message)
      (when (string= (rest (assoc :test-message response-message)) "test-nao-server-connection")
        t))))

;(test-nao-server-connection :silent t)

