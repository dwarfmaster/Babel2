
(in-package :nao-interface)

(export '(make-json curl))

(defun make-json (action &key (data (list)))
  "Encode json given an action and optional data. Data must be a-list"
  (encode-json-to-string-for-shell `((action . ,action) (data ,@data))))

(defun curl (json &key (host "localhost") port)
 "Send curl request to nao."
  (let ((response (exec-and-return "curl" (format nil "~a:~a" host port) "-s" "-d" json)))
    (when response (decode-json-from-string (first response)))))

#|
;; curl using dexador
(defun curl (json &key (host "localhost") port)
  "Send curl request and returns the answer."
  (let ((response (dex:post (format nil "http://~a:~a" host port)
                            :headers '(("Content-Type" . "application/json"))
                            :content json)))
    (when response (cl-json:decode-json-from-string response))))
|#

;; Quick hack for demo
(defun curl-picture (json &key (host "localhost") port)
  (let (response)
    (while (null response)
      do (progn
           (setf response (exec-and-return "curl" (format nil "~a:~a" host port) "-s" "-d" json))
           (unless response (sleep 1))))
    (decode-json-from-string (first response))))

