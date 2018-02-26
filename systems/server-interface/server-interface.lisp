(in-package :server-interface)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server Interface                                                                              ;;
;;                                                                                               ;;
;; This package sets up a LISP server. The server listens to a specified port                    ;;
;; (8920 by default), expecting http POST requests with json data. When a request comes in, it   ;;
;; creates a new thread for handling the request, parses the request into a LISP data            ;;
;; structure, and invokes the handle-http-request method, specialised on the request-type        ;;
;; (GET or POST) and the method-handler specified in the request.                                ;;
;;                                                                                               ;;
;; For using the server, it suffices to write a handle-http-request method 'my-handler'          ;;
;; specialised on :GET, :POST or T and the handler-method ':my-hanlder. Make sure of             ;;
;; course that the http request sends in its data 'handler-method=my-handler'.                   ;;
;;                                                                                               ;;
;; (start-server)                                                                                ;;
;; curl 'http://localhost:8920' -d '{"handler-method": "my-handler"}'                            ;;
;; (stop-server)                                                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(start-server
          stop-server
          handle-http-request))

;;;;;;;;;;;;;;;;;;;;
;; Default Address ;;
;;;;;;;;;;;;;;;;;;;;

;; By default, the web-server is running at http://localhost:8920
;; Add *web-server-host-address* and *ext-web-server-port* to your init-file
;; you want to customize this.

(defparameter *host* (if (boundp 'cl-user::*web-server-host-address*)
                       (eval 'cl-user::*web-server-host-address*)
                       "127.0.0.1"))

(defparameter *port* (if (boundp 'cl-user::*ext-web-server-port*)
                       (eval 'cl-user::*ext-web-server-port*)
                       8920))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handling the request  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun talk-on-stream (stream)
  "reads input on stream, parses the http request, distributes its to the
   right handler-method and writes back the output to the stream."
  (when (listen stream)
    (let* ((request (parse-http-request stream))
           (response (distribute-http-request request)))
      (http-send-line stream "HTTP/1.1 200 OK Content-Type: application/json")
      (http-send-line stream (format nil "Allow: POST"))
      (http-send-line stream (format nil "Accept: application/json"))
      (http-send-line stream (format nil "Access-Control-Allow-Headers: Content-Type"))
      ;(http-send-line stream (format nil "Cache-Control: no-cache"))
      (http-send-line stream (format nil "Access-Control-Allow-Origin:~a" "*"))
      ;(http-send-line stream (format nil "Access-Control-Allow-Origin: ~a" *host*))
      ;(http-send-line stream (format nil "Vary: Origin"))
      (http-send-line stream "")
      (format stream "~a" response)
      (http-send-line stream ""))
    (force-output stream)
    (close stream)))

(defun distribute-http-request (http-request)
  "Invokes the right handle-http-request method."
  (let* ((request-type (cdr (assoc :request-type http-request)))
         (data (cdr (assoc :data http-request)))
         (handler-method (cdr (assoc :handler-method data))))
    (if handler-method
      (handle-http-request http-request (make-kw request-type) (make-kw handler-method))
      (format nil "The http request should include a handler-method (handler-method=some-handler-method)."))))

(defgeneric handle-http-request (http-request request-type handler-method))
(defmethod handle-http-request (http-request (request-type t) (handler-method t))
  "Default handle-http-request method, write a specialised one for doing useful stuff"
  (let ((message (format nil "Please implement a handle-http-request method for request-type ~a and handler-method ~a." request-type handler-method)))
    (warn message)
    message))

