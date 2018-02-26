

(in-package :cl-user)

;; ###########################################
;; Demo on How to Use the Babel2 web-interface ;;
;; ###########################################

;; Loading the package and starting up the server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; First of all, we need to load the package:
(ql:quickload :server-interface)

;; As, in this demo, we want to use it in combination with fcg,
;; we also load the fcg package, and we we'll load a demo grammar as well.

(ql:quickload :fcg)
(fcg:load-demo-grammar)

;; Start the server, by default at port 8920
;; You can add :port xxxx as a key to start-server,
;; or set *ext-web-server-port* and *web-server-host-address* in your init file.
(si:start-server)

;; Writing a handle-http-request method
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We need to define WHAT we want to do and HOW we want to do it.
;; Imagine we want to parse or produce sentences with the grammar
;; that we just loaded into *fcg-constructions*
;;
;; We don't care whether it's get or post requests, but we do care
;; whether it's comprehend or formulate.
;;
;; The HTTP call should always have handler-method 
;; Other information goes automatically in 'data'

(defmethod si:handle-http-request (http-request request-type (handler-method (eql :comprehend)))
  (let* ((data (cdr (assoc :data http-request)))
         (utterance (cdr (assoc :utterance data))))
    (fcg:comprehend utterance :silent t)))

(defmethod si:handle-http-request (http-request request-type (handler-method (eql :formulate)))
  (let* ((data (cdr (assoc :data http-request)))
         (meaning (cdr (assoc :meaning data))))
    (string-trim "()" (format nil "~a" (fcg:formulate (read-from-string meaning) :silent t)))))

;; Sending Requests
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now we can just send the http request, e.g. using cURL
;; Paste the following line in your terminal, and watch the response

;; curl 'http://localhost:8920' -d '{"handler-method": "comprehend", "utterance": "the linguist likes the mouse"}'
;; AND
;; curl 'http://localhost:8920' -d '{"handler-method": "formulate", "meaning": "((DEEP-AFFECTION X-138 Y-51) (UNIQUE Y-51) (MOUSE Y-51) (UNIQUE X-138) (LINGUIST X-138))"}'
;;
;; Congratulations:: you have succesfully set up an FCG server!!
;;

;; Stopping the server
;; (you might want to add the port number as a key
;;  if you didn't use the default)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(si:stop-server)
