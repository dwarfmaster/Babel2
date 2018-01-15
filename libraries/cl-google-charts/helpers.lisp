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

(in-package  :cl-google-charts)

(defun hyphen->camel-case (string)
  "takes a string of the form test-tost or TEST-TOST and turns them into TestTost"
  (apply 'concatenate 'string
         (loop for s in (split-sequence:split-sequence #\- string)
               collect (string-capitalize (string-downcase s)))))

(defparameter *sig-counter* 0)

(defun get-new-sig ()
  (incf *sig-counter*)
  *sig-counter*)

(defparameter *id-counter* 0)

(defun make-id (symb &key omit-minus)
  (incf *id-counter*)
  (if omit-minus
    (make-symbol (format nil "~a~a" symb *id-counter*))
    (make-symbol (format nil "~a-~a" symb *id-counter*))))
  
