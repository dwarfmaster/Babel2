
(in-package :nao-interface)

;; + http-request using drakma +

(defun make-json (action &key (data (list)))
  "Encode json given an action and optional data. Data must be a-list"
  (encode-json-to-string `((action . ,action) (data ,@data))))

(defun send-http (uri json)
  "Send http-request to the uri and decode the answer"
  (with-open-stream (stream (http-request  uri
                                          :method :post
                                          :content json
                                          :want-stream t
                                          :connection-timeout nil
                                          :read-timeout nil
                                          :write-timeout nil))
    (decode-json-from-string (read-line stream))))

(defun curl-picture (json &key (host "localhost") port)
  (let (response)
    (while (null response)
      do (progn
           (setf response (send-http (format nil "http://~a:~a"
                                             host port)
                                     json))
           (unless response
             (sleep 1))))
    response))

(defun test-response-key (response &key key value (test #'eql))
  "Check if a key-value is present in the JSON response"
  (when (assoc key response)
    (funcall test (rest (assoc key response)) value)))

(defun get-response-key (response &key key)
  "Get a key from the JSON response"
  (when (assoc key response)
    (rest (assoc key response))))

