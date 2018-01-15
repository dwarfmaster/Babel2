;;; -*- mode: lisp -*-

;;; Time-stamp: <2014-01-13 08:42:15 tony>
;;; Creation:   
;;; File:       atoms.lisp
;;; Author:     Tamas Papp
;;; Maintainer: AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    default dispatch for objects using xarray methods

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :xarray)

;;;;  Unless otherwise specified by specialized methods, all objects
;;;;  are xref'able as an array of rank 0, similarly to CL's way of
;;;;  handling arrays like this.

(defmethod xelttype ((object t) &key list-of-rows list-of-columns)
  (type-of object))

(defmethod xdims ((object t))
  nil)

;;;;  Note: we MUST NOT implement xdim (TP)
;;;; FIXME: Tony sez: document why?

(defmethod xref ((object t) &rest subscripts)
  (when subscripts
    (error 'xref-wrong-number-of-subscripts))
  object)

;; (setf xref) is of course not defined
    
(defmethod xsimilar (rank (object t))
  (declare (ignore rank))
  `(array :element-type t))
