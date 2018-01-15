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

(in-package :cl-google-charts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; data-table

(defclass data-table ()
  ((columns :initarg :columns :accessor columns
            :initarg :description :accessor description
            :initform nil
            :documentation "columns describe the data
                            format ((id type label role)(id-2 type-2 label-2 role-2))
                            id should be a string
                            type 'string is default (possible values include 'string 'number 'boolean),
                            can also be passed as keywords (useful because we don't export date)
                            label (optional) will be set to id if not provided
                            role (optional).")
   (rows :initarg :rows :accessor rows
         :initarg :data :accessor data 
         :initform nil)))

(defmethod initialize-instance :after ((d data-table) &key columns description
                                       data rows &allow-other-keys)
  (let ((cols (or columns description))
        (rows (or rows data)))
    ;; accepts '("id-1" "id-2") as columns or as first row
    (cond
     ;; passed list of strings as columns
     ((stringp (car cols))
      (assert (= (length cols) (length (first rows))))
      (setf (columns d)
            (loop
             for c in cols
             for e in (first rows)
             collect (list c (cond ((typep e 'string)
                                    'string)
                                   ((typep e 'number)
                                    'number)
                                   (t (error "unknown data type")))
                           c))))
     ;; passed no cols, first row is only strings
     ((and (null cols)
           (loop for e in (first rows)
                 always (stringp e)))
      (setf (columns d)
            (loop
             for id in (first rows)
             for e in (second rows)
             collect (list id
                           (cond ((typep e 'string)
                                  'string)
                                 ((typep e 'number)
                                  'number)
                                 ((typep e 'boolean)
                                  'boolean)
                                 ((and (listp e)(cdr (assoc :year e)))
                                  'date)
                                 (t (error "unknown data type")))
                           id)))
      (setf (rows d)(cdr rows)))
     ;; passed no cols, first row is not only strings
     ((null cols)
      (setf (columns d)
            (loop
             for e in (first rows)
             for i from 1
             for id = (make-symbol (format nil "col-~a" i))
             collect (list id
                           (cond ((typep e 'string)
                                  'string)
                                 ((typep e 'number)
                                  'number)
                                 (t (error "unknown data type")))
                           id))))))
  ;; cleanup columns to have at least (id type label)
  (setf (columns d)
        (loop for c in (columns d)
              if (= (length c) 1)
              collect (list (first c) 'string (first c))
              else if (= (length c) 2)
              collect (list (first c) (second c) (first c))
              else
              collect c)))

(defmethod to-json ((table data-table) &key stream)
  (format stream "{~@[cols: [~{~a~^,~}],~]rows: [~{~a~^,~}]}"
          (when (columns table)
            (loop for c in (columns table)
                  collect (format nil "{id: '~a',type: '~a',label: '~a'~@[,role: '~a'~]}"
                                  (first c)
                                  (string-downcase (symbol-name (second c)))
                                  (third c)
                                  (when (fourth c)
                                    (fourth c)))))
          (loop
           for row in (rows table)
           collect
           (format nil "{c:[~{~a~^,~}]}" ;; 
                   (loop
                    for column in (columns table)
                    for entry in row
                    for type = (second column)
                    if (and (member type '(string number boolean :string :number :boolean
                                                  tooltip :tooltip))
                            (atom entry))
                    collect (format nil "{v:~a}" (if entry (to-json entry) "null"))
                    else if (and (member type '(string number boolean :string :number :boolean))
                                 (listp entry)
                                 (first entry))
                    collect (format nil "{v:~a,~@[f:~a~]}"
                                    (format nil "{v:~a}" (if entry (to-json entry) "null"))
                                    (and (cdr entry) (to-json (cdr entry))))
                    else if (and (member type '(date :date) :test 'equalp)
                                 (cdr (assoc :year entry)))
                    collect
                    (format nil
                            "{v:'Date(~a,~a,~a~@[,~a~]~@[,~a~]~@[,~a~])'}"
                            (cdr (assoc :year entry))
                            (cdr (assoc :month entry))
                            (cdr (assoc :day entry))
                            (cdr (assoc :hour entry))
                            (cdr (assoc :second entry))
                            (cdr (assoc :millisecond entry)))
                    else if (and (member type '(date :date))
                                 (cdr (assoc :year (first entry))))
                    collect
                    (format nil
                            "{v: 'Date(~a,~a,~a~@[,~a~]~@[,~a~]~@[,~a~])'~@[,f:'~a'~]}"
                            (cdr (assoc :year entry))
                            (cdr (assoc :month entry))
                            (cdr (assoc :day entry))
                            (cdr (assoc :hour entry))
                            (cdr (assoc :second entry))
                            (cdr (assoc :millisecond entry))
                            (and (cdr entry)(to-json (cdr entry))))
                                 
                    else
                    do (error "data type not supported or unknown format of data"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dynamic data sources

(defclass dynamic-data-source (data-table)
  ((id :initarg :id :accessor id 
       :initarg :data-id :accessor data-id
       :initform (format nil "~(~a~)" (make-id 'data-source))
       :documentation "some id used for storing and querying data. This should be a lowercase string")
   (sig :initarg :sig :accessor sig
        :initform (get-new-sig)
        :documentation "this should change everytime data is updated in the table
                        so that a data handler can be notified of the change")))
   
(defparameter *dynamic-data-sources* (make-hash-table :test 'equalp))

(defun add-dynamic-data-source (data-source)
  (declare (type dynamic-data-source data-source))
  (setf (gethash (id data-source) *dynamic-data-sources*)
        data-source))

(defun reset-dynamic-data-sources ()
  (setf *dynamic-data-sources* (make-hash-table :test 'equalp)))

(defun update-dynamic-data-source (data-source)
  (setf (sig (gethash (id data-source) *dynamic-data-sources*))
        (get-new-sig)))

(defun handle-request-dynamic-data-sources (data-id tqx)
  "handles a request by the client and generates response string
   data-id - the data id of the dynamic data source (used to query *dynamic-data-sources*)
   tqx - a google visualization api specific format (see implementing data sources in google documentation
   returns a response string" 
  (let* ((parameters (loop for s in (split-sequence:split-sequence #\; tqx)
                           for s-split = (split-sequence:split-sequence #\: s)
                           collect (cons (first s-split)(second s-split))))
         (response-handler (or (cdr (assoc "responseHandler" parameters :test #'equalp))
                               "google.visualization.Query.setResponse"))
         (req-id (cdr (assoc "reqId" parameters :test #'equalp)))
         (sig (cdr (assoc "sig" parameters :test #'equalp)))
         ;; (tq (assqv "tq" parameters :test #'equalp)) ;; ignore tq
         (data (gethash data-id *dynamic-data-sources*)))
    (cond
     ((null data) ;; data with id not found
      (format nil "~a({status:'error',version:0.6~@[,reqId:~a~],errors:[{reason:'unknown_data_source_id',message:'could not find data source with data-id ~a'}]})"
              response-handler
              req-id
              data-id))
     ((and (not (null sig)) ;; sig in request and different from sig in source
           (equal (format nil "~d" (sig data))
                  sig))
      (format nil "~a({status:'error',version:0.6~@[,reqId:~a~],errors:[{reason:'not_modified',message:'Data not modified'}],sig:~a})"
              response-handler
              req-id
              sig))
     (t ;; all other cases
        (let ((s (make-string-output-stream)))
          (format s "~a({status:'ok',version:0.6~@[,reqId:~a~],sig:~a,table:~a})"
                  response-handler
                  req-id
                  (sig data)
                  (to-json data))
          (get-output-stream-string s))))))

;; add default request handler to "/gdata"
(hunchentoot:define-easy-handler (gdata :uri "/gdata") (tqx data-id)
  (setf (hunchentoot:content-type*) "application/json")
  (handle-request-dynamic-data-sources data-id tqx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; external-data-source

(defclass external-data-source ()
  ((source-url :initarg :source-url :accessor source-url
               :documentation "the url, e.g. http://www.google.com/fusiontables/gvizdata?tq=")
   (query :initarg :query :accessor query
          :documentation "the query, example SELECT Year, Austria, Bulgaria, Denmark, Greece FROM 641716")))


