(in-package :nao-interface)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file is not used anymore. We connect now through a docker image. ;;
;; This can be found in the nao-docker.lisp file                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file implements and test the connection between Babel2 and the Nao Robot ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(*nao-robot-ip* start-nao-server stop-nao-server scp-get-nao))


;; Setting a few parameters that can be customized in init-babel.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *nao-server-file*
  (if (boundp 'cl-user::*nao-server-file*)
      (eval 'cl-user::*nao-server-file*)
      (babel-pathname :directory '("sharing" "robot-interface" "nao-interface") :name "nao_server" :type "py"))
  "Path pointing to the nao_server.py file.")

(defparameter *pynaoqi*
  (if (boundp 'cl-user::*pynaoqi*)
      (eval 'cl-user::*pynaoqi*)
      (error "*pynaoqi* was unbound. Please set *pynaoqi* in your init-babel file to the pynaoqi-path on your machine"))
  "Path pointing to pynaoqi.")

(defparameter *nao-robot-ip*
  (if (boundp 'cl-user::*nao-robot-ip*)
      (eval 'cl-user::*nao-robot-ip*)
      "192.168.1.2")
  "IP address of the nao robot.")

(defparameter *nao-robot-username*
  (if (boundp 'cl-user::*nao-robot-ip*)
      (eval 'cl-user::*nao-robot-ip*)
      "nao")
  "The user name of the nao.")

(defparameter *nao-robot-password*
  (if (boundp 'cl-user::*nao-robot-ip*)
      (eval 'cl-user::*nao-robot-ip*)
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

;; This will keep track of the running nao servers and their ports and PIDs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *nao-servers* nil
  "An a-list of nao servers in the form ((port-1 . PID-1) (port-2 . PID-2))")

(defun nao-servers ()
  "Returns the list of nao-servers as ((port-1 . PID-1) (port-2 . PID-2))."
  *nao-servers*)

(defun nao-server (port)
  "Returns (port . pid) if a nao-server is listening to port, nil otherwise"
  (assoc port *nao-servers*))

;; Starting and stopping nao servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun start-nao-server (&key (pynaoqi *pynaoqi*)
                           (robot-ip *nao-robot-ip*)
                           (robot-port *nao-robot-port*)
                           (server-file *nao-server-file*)
                           (server-host *nao-server-host*)
                           (server-port *nao-server-port*)
                           (test-connection t))
  "Starts a python server for the Nao Robot, updates *nao-servers*. Uses :test-connection nil for quicker startup."
  ;; If the server is already running, restart it.
  (when (assoc server-port *nao-servers*)
    (stop-nao-server server-port))
  ;; start the server and store the pid in a file
  (let* ((pid-filepath (babel-pathname :directory '(".tmp") :name (format nil "~a" (gensym "NAO-PID-")) :type "txt"))
         (arg (format nil "/usr/bin/python ~a --pynaoqi ~a --robot-ip ~a --robot-port ~a --server-host ~a --server-port ~a & echo $!>~a"
                     server-file pynaoqi robot-ip robot-port server-host server-port pid-filepath)))
    (run-prog "/bin/sh" :args (list "-c" arg))
    ;; we need to wait until the server has started up :-(
    (loop until (probe-file pid-filepath) do (sleep 0.05))
    ;; now we open the file and update *nao-servers*, then we delete the file
    (with-open-file (stream pid-filepath)
      (let ((pid (read-line stream)))
        (push (cons *nao-server-port* pid) *nao-servers*)))
    (run-prog "rm" :args (list (format nil "~a" pid-filepath))))
  (when test-connection (sleep 1) (test-nao-server-connection))
  *nao-servers*)

(defun stop-nao-server (&optional (port *nao-server-port*))
  "Stops the python server listening to the specified port. Updates *nao-server-port"
  (let ((pid (rest (assoc port *nao-servers*))))
    (run-prog "kill" :args (list pid))
    (setf *nao-servers* (remove (assoc port *nao-servers*) *nao-servers*)))
  *nao-servers*)

(defun stop-all-nao-servers ()
  "Stops all known nao-servers."
  (when *nao-servers*
    (loop for (port . pid) in *nao-servers*
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
      (warn "Did Nao speak? If not, check whether you are connected to the same WiFi network!"))
    (when (assoc :test-message response-message)
      (when (string= (rest (assoc :test-message response-message)) "test-nao-server-connection")
        t))))

;(test-nao-server-connection :silent t)

