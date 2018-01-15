;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;; Time-stamp: <2014-01-13 08:44:59 tony>
;;; Creation:   ??
;;; File:       array.lisp
;;; Author:     Tamas Papp < >
;;; Maintainer: AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2012--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;; Purpose:    Implementation of the xarray API for lisp arrays. 

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :xarray)

;;;;  The native Lisp ARRAY is the of course the most important of
;;;;  xrefable objects.  The interface maps to CL functions in a
;;;;  straightforward manner.

(defmethod xelttype ((object array) &key list-of-rows list-of-columns)
  (array-element-type object))

(defmethod xrank ((object array))
  (array-rank object))

(defmethod xdims ((object array))
  (array-dimensions object))

(defmethod xdim ((object array) axis-number)
  (array-dimension object axis-number))

(defmethod xsize ((object array))
  (array-total-size object))

(defmethod xref ((object array) &rest subscripts)
  (apply #'aref object subscripts))

(defmethod (setf xref) (value (object array) &rest subscripts)
  (setf (apply #'aref object subscripts) value))

;;; Extended interface

(defmethod xcreate ((class (eql 'array)) dimensions &optional options)
    (bind (((&key (element-type t)) options))
      (make-array dimensions :element-type element-type)))

(defmethod xsimilar (rank (object array))
  (declare (ignore rank))
  `(array :element-type ,(array-element-type object)))

(defmethod as* ((class (eql 'array)) object copy-p options)
  (declare (ignore copy-p))
  (bind (((&key (element-type t)) options)
         (array (make-array (xdims object) :element-type element-type))
         (dimensions (coerce (xdims object) 'fixnum-vector)))
    (if (subtypep (xelttype object) element-type)
        ;; coerce
        (dotimes (i (xsize object))
          (setf (row-major-aref array i)
                (coerce (apply #'xref object (rm-subscripts dimensions i))
                        element-type)))
        ;; no mapping 
        (dotimes (i (xsize object))
          (setf (row-major-aref array i)
                (apply #'xref object (rm-subscripts dimensions i)))))
    array))

;;;  1-dimensional arrays can also be referred to as vector.

(defmethod as* ((class (eql 'vector)) object copy-p options)
  (assert (= (xrank object) 1) () "OBJECT is not a vector.")
  (as* 'array object copy-p options))


;;; Convenience functions for vector and array construction.  All
;;; return simple-arrays of the specified type, the versions with *
;;; use numeric-type-classifier.
;;;
;;; When we have additional utilities specific to list arrays, they
;;; should be here.  If these functions are generally useful, they
;;; should be promoted to generics in the interface.lisp file, and
;;; these should become methods.

(defun cvector (element-type &rest elements)
  "Return a (simple-array element-type (*)) containing elements,
coerced to element-type."
  (let ((vector (make-array (length elements) :element-type element-type)))
    (fill-array-with-list vector elements)))

(defun carray (element-type dimensions &rest elements)
  "Return a (simple-array element-type dimensions) containing elements,
coerced to element-type."
  (unless (= (length elements) (reduce #'* dimensions))
    (error "incorrect number of elements provided"))
  (let ((vector (make-array dimensions :element-type element-type)))
    (fill-array-with-list vector elements)))

(defun cvector* (&rest elements)
  "Return a (simple-array element-type (*)) containing elements,
coerced to element-type, where the elemen-type is obtained using
numeric-type-classifier."
  (apply #'cvector (numeric-type-classifier elements) elements))

(defun carray* (dimensions &rest elements)
  "Return a (simple-array element-type dimensions) containing elements,
coerced to element-type, where the elemen-type is obtained using
numeric-type-classifier."
  (apply #'carray (numeric-type-classifier elements) dimensions elements))
