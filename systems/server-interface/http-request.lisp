(in-package :server-interface)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File parse-http-request                                                                            ;;
;;                                                                                                    ;;
;; This files contains the necessary functions to read an http-request                                ;;
;;                                                                                                    ;;
;; (GET or POST) from a stream. It parses to a SEXP containing:                                       ;;
;; IMPORTANT: NOW LOOKS A BIT DIFFERENT (JSON)                                                        ;;
;;                                                                                                    ;;
;; - For a GET Request                                                                                ;;
;;        - For the header:                                                                           ;;
;;              ("Request-Type" . "GET")                                                              ;;
;;              ("Data" ("veld1" . "test") ("veld2" . "nog-data") ("veld-3" . "nog-informatie"))      ;;
;;                                                                                                    ;;
;;        - Then, for every line                                                                      ;;
;;              ("Accept" . "*/*")                                                                    ;;
;;              ("User-Agent" . "curl/7.47.1")                                                        ;;
;;              ("Host" . "localhost:8920")                                                           ;;
;;                                                                                                    ;;
;; - For a POST Request                                                                               ;; 
;;        - For the header:                                                                           ;;
;;              ("Request-Type" . "POST")                                                             ;;
;;                                                                                                    ;;
;;        - Then, for every line                                                                      ;;
;;              ("Content-Type" . "application/x-www-form-urlencoded")                                ;;
;;              ("Content-Length" . "31")                                                             ;;
;;              ("Accept" . "*/*")                                                                    ;;
;;              ("User-Agent" . "curl/7.47.1")                                                        ;;
;;              ("Host" . "localhost:8920")                                                           ;;
;;                                                                                                    ;;
;;        - For the body                                                                              ;;
;;              ("Data" ("veld1" . "test") ("veld2" . "nog-data") ("veld-3" . "nog-informatie"))      ;;
;;                                                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun parse-http-request (stream)
  "Reads http Get or Post request from stream and returns s-expression containing the information."
  (let* ((header (remove-trailing-cariage-return (read-line stream)))
         (request-type (first (split-sequence:split-sequence #\Space header)))
         (request-body nil))

    ;; Add request type
    (push (cons :request-type request-type) request-body)
    
    ;; Read everything in the stream
    (loop for line = (read-line stream)
          while (not (carriage-return-only-p line))
          do
          (push (parse-as-cons (remove-trailing-cariage-return line)) request-body))
    
    ;; Add data for Post Requests
    (when (equalp "POST" request-type)
      (let ((post-data "")
            (content-length (read-from-string (cdr (assoc "Content-Length" request-body :test 'equalp)))))
        (dotimes (n content-length)
          (setf post-data (string-append post-data (string (read-char stream)))))
        (push (cons :data (cl-json:decode-json-from-string post-data)) request-body)))

    ;; Add data for Get Requests
    (when (equalp "GET" request-type)
      (let ((get-data (second (split-sequence:split-sequence '#\? (second (split-sequence:split-sequence #\Space header))))))
        (push (cons :data (parse-attributes get-data)) request-body)))
    ;; Return parsed http request
    request-body))

(defun parse-as-cons (string)
  "splits at ': ' and returns cons cell of left and write"
  (let ((list (cl-ppcre:split ": " string)))
    (cons (first list) (second list))))

(defun parse-attributes (string)
  "parses http attributes 'a=1&b=2' to '((a . 1) (b . 2))"
  (let ((parsed-attributes nil)
        (attribute-list (cl-ppcre:split "&" string)))
    (dolist (attribute attribute-list)
      (let ((at-val-pair (cl-ppcre:split "=" attribute)))
        (push (cons (first at-val-pair) (second at-val-pair)) parsed-attributes)))
    (reverse parsed-attributes)))

(defun carriage-return-only-p (string)
  "returns true if a string only contains a carriage return symbol"
  (equalp string (format nil "~a" (code-char 13))))

(defun remove-trailing-cariage-return (string)
  "returns the part of string in front of a carriage return symbol"
  (first (split-sequence:split-sequence (code-char 13) string)))
  
(defun http-send-line (stream line)
  "Send a line of text to the HTTP stream, terminating it with CR+LF."
  (princ line stream)
  (princ (code-char 13) stream)  ;; carriage-return
  (princ (code-char 10) stream))

