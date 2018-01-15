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

(in-package :common-lisp-user)

(defpackage :cl-google-charts
  (:documentation "A package for support of the google charts api (js)")
  (:use :common-lisp :hunchentoot)
  (:export :date 
   :data-table :columns :rows :data-id
   :dynamic-data-source :add-dynamic-data-source :reset-dynamic-data-sources :update-dynamic-data-source
   :external-data-source
   :chart :area-chart :bar-chart :bubble-chart
   :candlestick-chart :candlestick-chart
   :column-chart :combo-chart :gauge :geo-chart
   :line-chart :pie-chart :scatter-chart
   :stepped-area-chart :table :tree-map
   :motion-chart
   :annotated-time-line :intensity-map :org-chart
   :chart->html))
