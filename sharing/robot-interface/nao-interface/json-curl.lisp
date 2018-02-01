
(in-package :nao-interface)

(export '(make-json curl))

#|
(defun make-json (action &key (data (list)))
  "Encode json given an action and optional data. Data must be a-list"
  (encode-json-to-string-for-shell `((action . ,action) (data ,@data))))

;; curl using exec-and-return
(defun curl (json &key (host "localhost") port)
 "Send curl request to nao."
  (let ((response (exec-and-return "curl" (format nil "~a:~a" host port) "-s" "-d" json)))
    (when response (decode-json-from-string (first response)))))

;; curl using dexador
(defun curl (json &key (host "localhost") port)
  "Send curl request and returns the answer."
  (let ((response (dex:post (format nil "http://~a:~a" host port)
                            :headers '(("Content-Type" . "application/json"))
                            :content json)))
    (when response (cl-json:decode-json-from-string response))))

;; Quick hack for demo
(defun curl-picture (json &key (host "localhost") port)
  (let (response)
    (while (null response)
      do (progn
           (setf response (exec-and-return "curl" (format nil "~a:~a" host port) "-s" "-d" json))
           (unless response (sleep 1))))
    (decode-json-from-string (first response))))

|#

;; http-request using drakma
(defun make-json (action &key (data (list)))
  "Encode json given an action and optional data. Data must be a-list"
  (encode-json-to-string `((action . ,action) (data ,@data))))

(defun curl (json &key (host "localhost") port)
  "Send http-request to nao and decode the answer"
  (let ((stream (http-request (format nil "http://~a:~a" host port)
                              :method :post
                              :content json
                              :want-stream t)))
    (when stream
      (decode-json-from-string (read-line stream)))))

(defun curl-picture (json &key (host "localhost" port))
  (let (response)
    (while (null response)
      do (progn
           (setf response (curl json :host host :port port))
           (unless response (sleep 1))))
    response))

(defun send-and-test-response-key (data key value &key (host "localhost") port (test #'eq))
  "Send a http request and test for a value in the response"
  (let ((response (curl data :host host :port port)))
    (when (assoc key response)
      (funcall test (rest (assoc key response)) value))))

(defun send-and-get-response-key (data key &key (host "localhost") port)
  "Send a http request and get a field from the response"
  (let ((response (curl data :host host :port port)))
    (when (assoc key response)
      (rest (assoc key response)))))

