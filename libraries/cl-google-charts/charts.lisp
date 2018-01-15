;;; Copyright (c) 2011, Michael Spranger (http://www.michael-spranger.com).
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

(in-package :cl-google-charts)

(defclass chart ()
  ((id :initarg :id :accessor id :initform (make-id 'chart))
   (options :initarg :options :accessor options :initform nil
            :documentation "the options for the chart. if you want total control then
                            just pass a string here that will be given to the js chart object
                            otherwise you can use a notation similar to ((\"theme\" . \"test\") (\"chartArea.top\" . 12)
                            which translates to \"[\"theme\":\"test\",\"chartArea.top\":12]
                            please see chart-examples.lisp")
   (data-source
    :initarg :data :accessor data
    :initarg :data-source :accessor data-source 
    :initform nil
    :documentation "the source of data")
   (refresh-interval :initarg :refresh-interval
                     :accessor refresh-interval :initform 1
                     :documentation "how often the server should poll for data (only needed if data is dynamic)")))

(defmethod initialize-instance :after ((chart chart) &key data-source data)
  ;; you can pass a list to data-source and this will try and make a data table
  ;; out of it, first row will be interpreted as column definition
  (let ((d (or data data-source)))
    (when (and (listp d)
               (not (null d)))
      (setf (data chart)
            (make-instance 'data-table :data d)))))
          

(defclass area-chart (chart) ())

(defclass bar-chart (chart)
  ())

(defclass bubble-chart (chart)
  ())

(defclass candlestick-chart (chart)
  ())

(defclass column-chart (chart)
  ())

(defclass combo-chart (chart)
  ())

(defclass gauge (chart)
  ())

(defclass geo-chart (chart)
  ())

(defclass line-chart (chart)
  ())

(defclass pie-chart (chart)
  ())

(defclass scatter-chart (chart)
  ())

(defclass stepped-area-chart (chart)
  ())

(defclass table (chart)
  ())

(defclass tree-map (chart)
  ())

(defclass motion-chart (chart)
  ())

(defclass annotated-time-line (chart)
  ())

(defclass intensity-map (chart)
  ())
