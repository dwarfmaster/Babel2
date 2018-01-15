;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          i18n.lisp
;;;; Purpose:       UFFI test file of i18n functions
;;;; Author:        Kevin M. Rosenberg
;;;; Date Started:  Feb 2010
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2010 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi-tests)

(deftest :i18n/sto/1
    (uffi:string-to-octets "")
  #())

(deftest :i18n/sto/2
    (uffi:string-to-octets "A")
  #(65))

(deftest :i18n/sto/3
    (uffi:string-to-octets "abc")
  #(97 98 99))

(deftest :i18n/sto/4
    (uffi:string-to-octets "abc" :null-terminate t)
  #(97 98 99 0))

;; Below is UTF-8 encoded, 27 octets / 20 lisp characters
(deftest :i18n/sto/5
    (uffi:string-to-octets "Iñtërnâtiônàlizætiøn" :encoding :utf-8)
  #(73 195 177 116 195 171 114 110 195 162 116 105 195 180 110 195 160 108 105 122 195 166 116 105 195 184 110))

(deftest :i18n/sto/6
    (uffi:string-to-octets "Iñtërnâtiônàlizætiøn" :encoding :utf-8 :null-terminate t)
  #(73 195 177 116 195 171 114 110 195 162 116 105 195 180 110 195 160 108 105 122 195 166 116 105 195 184 110 0))

(deftest :i18n/lsto/1
    (length (uffi:string-to-octets "Iñtërnâtiônàlizætiøn" :encoding :utf-8))
  27)

(deftest :i18n/lsto/2
    (length (uffi:string-to-octets "Iñtërnâtiônàlizætiøn" :encoding :utf-8 :null-terminate t))
  28)

(deftest :i18n/feoc/1
    (uffi:foreign-encoded-octet-count "")
  0)

(deftest :i18n/feoc/2
    (uffi:foreign-encoded-octet-count "A")
  1)

(deftest :i18n/feoc/3
    (uffi:foreign-encoded-octet-count "abc")
  3)

(deftest :i18n/feoc/4
    (uffi:foreign-encoded-octet-count "Iñtërnâtiônàlizætiøn"
                                      :encoding :utf-8)
  27)


(deftest :i18n/ots/1
    (let ((octets '()))
      (uffi:octets-to-string (make-array (list (length octets)) :element-type '(unsigned-byte 8)
                                         :initial-contents octets)))
  "")

(deftest :i18n/ots/2
    (let ((octets '(65)))
      (uffi:octets-to-string (make-array (list (length octets)) :element-type '(unsigned-byte 8)
                                         :initial-contents octets)))
  "A")

(deftest :i18n/ots/3
    (let ((octets '(97 98 99)))
      (uffi:octets-to-string (make-array (list (length octets)) :element-type '(unsigned-byte 8)
                                         :initial-contents octets)))
  "abc")

(deftest :i18n/ots/4
    (let ((octets '(73 195 177 116 195 171 114 110 195 162 116 105 195 180
                    110 195 160 108 105 122 195 166 116 105 195 184 110)))
      (uffi:octets-to-string (make-array (list (length octets)) :element-type '(unsigned-byte 8)
                                         :initial-contents octets)
                             :encoding :utf-8))
  "Iñtërnâtiônàlizætiøn")
