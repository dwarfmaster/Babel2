;;; -*- mode: lisp -*-

;;; Time-stamp: <2011-03-25 17:27:31 tony>
;;; Creation:   <2011-03-25 17:24:35 tony>
;;; File:       conditions.lisp
;;; Author:     Tamas Papp
;;; Maintainer: AJ Rossini <blindglobe@gmail.com> (file author -- source is from Tamas Papp)
;;; Copyright:  (c)2011--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    XARRAY conditions

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :xarray)

;;;; Conditions for the wrong number of subscripts, subscripts being
;;;; out of bounds, or writing non-writable elements or elements with
;;;; incorrect type are available. (!!! see notes there)

(define-condition xref-subscript-out-of-bounds (error)
  ((subscripts :initarg :subscripts :reader subscripts)
   (dimensions :initarg :dimensions :reader dimensions)))

(define-condition xref-wrong-number-of-subscripts (error)
  ((subscripts :initarg :subscripts :reader subscripts)
   (rank :initarg :rank :reader rank)))

(define-condition xref-setting-readonly (error)
  ;; !! maybe give some info on the writability?
  ((subscripts :initarg :subscripts :reader subscripts)))

(define-condition xref-incompatible-type (error)
  ;; !! maybe give some info on the type?
  ((subscripts :initarg :subscripts :reader subscripts)))

(define-condition xdim-invalid-axis-number (error)
  ())
