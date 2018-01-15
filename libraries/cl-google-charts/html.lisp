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

;; default for dynamic data source provider
(defparameter *dynamic-data-source-url* "http://localhost:8000/gdata")

(defparameter *google-api-url* "http://www.google.com/jsapi") 

(defparameter *google-chart-init-js* "google.load('visualization', '1', {'packages': ['table', 'corechart', 'geochart','gauge','treemap', 'motionchart','intensitymap','orgchart','annotatedtimeline']});")

(defun get-head-html (&key stream)
  "Include the code generated from this function in the head of your html page
   It loads the google library and the loads packages from the library necessary
   to create the charts"
  (format stream "<script type=\"text/javascript\" src=~s></script><script type=\"text/javascript\">~a</script>"
          *google-api-url*
          *google-chart-init-js*))

;; (get-head-html)

(defun chart->html (chart &key stream
                          style
                          (url *dynamic-data-source-url*))
  "generates a div and javascript that loads the chart, returns a string
   behaves differently for dynamic or static data sources
   id - the id of the div for the graph
   stream - if you want that the string is written to some stream
   url - the url of the dynamic data source, this defaults to http://localhost:8000/gdata (only for dynamic graphs)
   dynamic-query-var - the variable holding the dynamic query, if nil, then delete is called (only for dynamic graphs)"
  (let ((chart-name (hyphen->camel-case (symbol-name (type-of chart))))
        (options (if (options chart)
                   (if (stringp (options chart))
                     (options chart)
                     (object-to-json (options chart))))))
    (cond
     ((typep (data chart) 'dynamic-data-source)
      ;; dynamic graph
      (format stream "<div id='~a'~@[ style='~a'~]></div>
                   <script type=\"text/javascript\">new google.visualization.ChartWrapper({chartType: '~a', dataSourceUrl: '~a?data-id=~a', refreshInterval: ~a~@[, options: ~a~], containerId: '~a'}).draw();</script>"
              (id chart)
              style
              chart-name
              url
              (id (data chart))
              (refresh-interval chart)
              options
              (id chart)))
     ;; external data source
     ((typep (data chart) 'external-data-source)
      (format stream "<div id='~a'~@[ style='~a'~]></div>
                   <script type=\"text/javascript\">new google.visualization.ChartWrapper({chartType: '~a', dataSourceUrl: '~a'~@[, query: '~a'~]~@[, refreshInterval: ~a~]~@[, options: ~a~], containerId: '~a'}).draw();</script>"
              (id chart)
              style
              chart-name
              (source-url (data chart))
              (query (data chart))
              (refresh-interval chart)
              options
              (id chart)))
     ;; static graph
     (t
      (format stream "<div id='~a'~@[ style='~a'~]></div><script type=\"text/javascript\">new google.visualization.~a(document.getElementById('~a')).draw(new google.visualization.DataTable(~a)~@[,~a~]);</script>"
              (id chart) style chart-name
              (id chart)
              (to-json (data chart))
              options)))))
