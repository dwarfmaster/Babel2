;;; -*- mode: lisp -*-

;;; Time-stamp: <2014-01-13 08:42:33 tony>
;;; Creation:   ??
;;; File:       interface.lisp
;;; Author:     Tamas Papp
;;; Maintainer: AJ Rossini <blindglobe@gmail.com>
;;; Copyright:  (c)2009--, AJ Rossini.  Currently licensed under MIT
;;;             license.  See file LICENSE.mit in top-level directory
;;;             for information.
;;;             (c)   --, Tamas Papp.  Contributions licensed under
;;;             FIXME: need to check and confirm!
;;; Purpose:    interface description through generics.

;;; What is this talk of 'release'? Klingons do not make software
;;; 'releases'.  Our software 'escapes', leaving a bloody trail of
;;; designers and quality assurance people in its wake.

(in-package :xarray)

;;;; General interface for objects accessible with xref (objects like
;;;; this are called xrefable).
;;;;
;;;; We distinguish two levels of the interface: the basic (element
;;;; access, dimension information) and the extended (type
;;;; information, creation of similar objects).


;;;; Basic interface
;;;;
;;;; Objects accessible with xref are array-like objects of
;;;;
;;;;    (xdims object)
;;;;    (= (nth N (xdims object)) (xdim object N))
;;;;
;;;; dimensions, where elements are indexed with
;;;;
;;;;    (xrank object)
;;;;
;;;; subscripts, each ranging from
;;;;
;;;;    (list 0 (1- (nth dimension (xdims object)))) 
;;;;
;;;; inclusive.  Not all elements need to be setable, if
;;;; they are not, trying to call
;;;;
;;;;    (setf xref)
;;;;
;;;; on that element will signal a condition.
;;;;
;;;; Objects can have a particular type imposed on elements, which can
;;;; be checked by looking at the contents of 
;;;;
;;;;    (xeltype object <ref>)
;;;;
;;;; and finally
;;;;
;;;;    (xref object <ref>)
;;;;
;;;; will access the particular structure needed.  Elements returned
;;;; by xref are guaranteed to be a subtype of this type, and for
;;;;
;;;;    (setf (xref object <ref>) <new object>)
;;;;
;;;; we have that <new object> must consist of elements that are an
;;;; appropriate subtype.

;;; Mixin the following virtual superclass to ensure that we can use any default methods.  
(defclass xarray-like ()
  ()
  (:documentation "mixin virtual superclass to indicate support for
  dispatch and generics.  There should be no objects instantiated with
  this class."))

;;; Generics to use

(defgeneric xelttype (object &key list-of-rows list-of-columns)
  (:documentation "Return the type of elements.  If no restriction is
  imposed, return T.  By default, T is expected, numerically-oriented
  matrices being the exception and hence different.  

  If there is :list-of-rows or :list-of-columns specified, then return
  a list with the types for the rows or columns, respectively.  These
  would be a repeated list of the same type in the case of a numerical
  single-typed matrix.

  We do not throw an error for this not being specified - it is only
  specified FOR restrictions, not when there are no restrictions.")
  (:method ((object xarray-like) &key list-of-rows list-of-columns) T))

;;; For accessing dimensions and sizes

;;; In most cases, you only need to implement xdims, the rest will be
;;; defined in a sane way by generic functions.  The rest only needs
;;; to be implemented for efficiency reasons, but this should be a
;;; minor concern for most applications.

(defgeneric xdims (object)
  (:documentation "Return a list of dimensions of object.  The list
  does not share structure with anything, so it can be freely
  modified.")
  (:method ((object xarray-like)) 
    (error "Need to implement XDIMS for type %s" (type-of object))))

;; The following (xdim, xrank, xsize) use XDIMS by default, but could use optimized
;; dispatches if such exist for a particular table-like data
;; structure.

(defgeneric xdim (object dim)
  (:documentation "Return the size of the dim-th dim.  We use the
  default unless there is some sensible reason to implement
  otherwise.")
  (:method ((object xarray-like) (dim integer))
    (let ((dim (nth dim (xdims object))))
      (if dim dim (error 'xdim-invalid-axis-number)))))

(defgeneric xrank (object)
  (:documentation "Returns the number of dimensions of object.")
  (:method ((object xarray-like))
    (length (xdims object))))

(defgeneric xsize (object)
  (:documentation "Return the total number of elements in object.")
  ;; (:method ((object t)) (reduce #'* (xdims object) :initial-value 1))
  (:method ((object xarray-like)) (reduce #'* (xdims object) :initial-value 1)))

;;;; Accessors for elements.  (setf xref) can signal an error for
;;;; read-only elements, or does not need to be defined at all.

(defgeneric xref (object &rest subscripts)
  (:documentation
   "Accesses the element of the object specified by subscripts.")
  (:method ((object xarray-like) &rest subscripts) 
    (error "Need to implement XREF for type %s" (type-of object))))

(defgeneric (setf xref) (value object &rest subscripts)
  (:documentation
   "Accesses the element of the object specified by subscripts."))

;;; xsetf allow to set elements of an xrefable object to those of
;;; another.
;;;
;;; ??? should we lose the function? -- Tamas
;;; NO.  -tony
;;; 
;;; Ok, maybe.  The point is that it might be better to set up
;;; (setf xref) and (setf xslice) so that they work, using views
;;; rather than mapping functions.  
;;;
;;; It is also nice to be able to do either (mapping, or view-setting
;;; with an isomorphic structure.

(defgeneric xsetf (destination source &key map-function)
  (:documentation "Copy the elements of source to destination.
     Map-function, if given, will be used to map the elements, the
     default is conversion (if necessary) with coerce.")
  (:method (destination source &key (map-function
				     (element-conversion-function (xelttype source)
								  (xelttype destination))))
    (unless (equalp (xdims source) (xdims destination))
      (error "source and destination do not have conforming dimensions"))
    (let ((dimensions (xdims source)))
      (if (and map-function (not (eq map-function #'identity)))
	  ;; map-function is not given or identity, don't apply
	  (dotimes (i (xsize source))
	    (let ((subscripts (cm-subscripts dimensions i)))
	      (setf (apply #'xref destination subscripts)
		    (apply #'xref source subscripts))))
	  ;; use map-function
	  (dotimes (i (xsize source))
	    (let ((subscripts (cm-subscripts dimensions i)))
	      (setf (apply #'xref destination subscripts)
		    (funcall map-function
			     (apply #'xref source subscripts)))))))
    destination))

;;;;  An object is characterized by its CLASS (a symbol), various
;;;;  class-specific OPTIONS (a list of keyword-value pairs, can be
;;;;  empty, all should have reasonable defaults so that they are
;;;;  optional) and the dimensions.
;;;;
;;;;  The XSIMILAR method should return (CONS CLASS OPTIONS) which
;;;;  provides the parameters for creating a "similar" object with
;;;;  given dimensions, where "similarity" is of course something
;;;;  object specific, and designed to be a convenient default.  The
;;;;  purpose is to provide reasonable defaults for XMAP, AS, etc,
;;;;  when called with target specification T or (cons T OPTIONS).
;;;;
;;;;  AS, COPY-AS, and XCREATE are object conversion/creation methods
;;;;  that specialize on class, and are also given OPTIONS.  When
;;;;  called with T or (cons T OPTIONS), AS and COPY-AS uses
;;;;  information returned by XSIMILAR (merging the options).
;;;;  XCREATE* takes class and options from xsimilar called on the
;;;;  first argument, and merges that with the options.
;;;;
;;;;  The class for DIMENSIONS should always be a list.
;;;;
;;;;  XSIMILAR should always give an object which, when created, has
;;;;  all its elements writable.

(defgeneric xsimilar (rank object)
  (:documentation "Return (CONS CLASS OPTIONS) for creating a similar
     object with new rank.  If rank is T, use rank of object.  NOTE: for
     methods, make sure you specialize rank to fixnum if you are not
     handling T.

     This method needs to be clarified a bit -- it is more about
     providing meta data for rebuilding, not about determining some
     notion of equalness.")
  (:method ((rank (eql t)) object)
    (xsimilar (xrank object) object))
  (:method (rank (object xarray-like))
    (xsimilar (xrank object) object)) )

(defgeneric xcreate (class dimensions &optional options)
  (:documentation "Return a new object of given type and dimensions,
  with additional options.  Dimensions can be a list, or a single
  number.  xcreate can also be called as
  (XCREATE (CONS CLASS OPTIONS) DIMENSIONS), in which case it will
  split the cons, merge OPTIONS and call XCREATE again.")
  (:method ((class list) dimensions &optional options)
    (xcreate (car class)
	     dimensions
	     (merge-options (cdr class) options))))

(defun xcreate-similar (target-spec object dimensions
			&optional more-options)
  "If TARGET-SPEC is T or (CONS T OPTIONS), use xsimilar to determine
target spec using object (and also merge options), otherwise use
target-spec directly to create an object.  This function is meant for
internal use, when mapping functions need to determine a target spec
from one of the arguments."
  (bind ((dimensions (if (eq dimensions t) (xdims object) dimensions))
	 ((:values class options)
	  (if (atom target-spec)
	      (values target-spec nil)
	      (values (car target-spec)
		      (cdr target-spec)))))
    (xcreate (if (eq class t)
		 (xsimilar (length dimensions) object)
		 class)
	     dimensions (merge-options options more-options))))

(defgeneric as* (class object copy-p options)
  (:documentation "Return an object converted to a given class, with
other properties (eg element types for arrays) as specified by the
optional keyword arguments.  The result may share structure with
object, unless COPY-P.  Similarly to XCREATE, class can be (cons class
options).  When class is nil, XSIMILAR is called to obtain the result
type.

Usage note: libraries should specialize this method, but the user
interface is AS or COPY-AS.")
  (:method (class object copy-p options)
    ;; fallback case: object created by xcreate, copied elementwise
    (declare (ignore copy-p))
    (let* ((dims (xdims object))
	   (result (xcreate class dims options)))
      (dotimes (i (xsize object))
	(let ((subscripts (cm-subscripts dims i)))
	  (setf (apply #'xref result subscripts)
		(apply #'xref object subscripts))))
      result))
  (:method ((class (eql t)) object copy-p options)
    ;; take type from xsimilar
    (as* (xsimilar t object) object copy-p options))
  (:method ((class list) object copy-p options)
    ;; split class and merge options
    (as* (car class) object copy-p
	 (merge-options (cdr class) options))))

(defun as (class object &rest options)
  "Convert OBJECT to CLASS.  May share structure."
  (as* class object nil options))

(defun copy-as (class object &rest options)
  "Convert OBJECT to CLASS as a deep copy, with the enforcement of no
shared structure."
  (as* class object t options))
