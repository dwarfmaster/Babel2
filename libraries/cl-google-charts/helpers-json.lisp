;;; Copyright (c) 2012, Michael Spranger (http://www.michael-spranger.com).
;;; All rights reserved.
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; This file has been adapted from writer.lisp from jsown
;;; see https://github.com/madnificent/jsown.git

(in-package :cl-google-charts)

(declaim (optimize (speed 3) (debug 3) (safety 0)))

(defgeneric to-json (object &key)
  (:documentation "Writes the given object to json in a generic way."))

(defmethod to-json ((string string) &key stream)
  (format stream "'~{~a~}'"
          (loop for char across string
                collect (case char
                          (#\newline  "\\n")
                          (#\return "\\r")
                          (#\tab "\\t")
                          (#\" "\\\"")
                          (#\' "\\\'")
                          (t char)))))

(defmethod to-json ((number number) &key stream)
  (if stream
    (format stream "~a" number)
    number))

(defmethod to-json ((ratio ratio) &key stream)
  (to-json (coerce ratio 'float) :stream stream))

(defmethod to-json ((list list) &key stream)
  ;; *pretty-print* makes printing very slow, internal json objects needn't have this
  (let ((*print-pretty* nil)) 
    (if (eq (car list) :obj)
      (object-to-json (cdr list) :stream stream)
      (list-to-json list :stream stream))))

(defmethod to-json ((true (eql t)) &key stream)
  (format stream "true"))

(defmethod to-json ((true (eql :t)) &key stream)
  (format stream "true"))

(defmethod to-json ((true (eql :true)) &key stream)
  (format stream "true"))

(defmethod to-json ((false (eql :false)) &key stream)
  (format stream "false"))

(defmethod to-json ((false (eql :f)) &key stream)
  (format stream "false"))

(defmethod to-json ((false (eql :null)) &key stream)
  (format stream "null"))

(defmethod to-json ((false (eql :n)) &key stream)
  (format stream "null"))

(defmethod to-json ((false (eql :undefined)) &key stream)
  (format stream "undefined"))

(defun object-to-json (list &key stream)
  (format stream "{~{~{~A:~A~}~^,~}}"
	  (loop for item in list collect
                (list (to-json (car item))
                      (to-json (cdr item))))))

(defun list-to-json (list &key stream)
  (format stream "[~{~A~^,~}]"
	  (mapcar #'to-json list)))


