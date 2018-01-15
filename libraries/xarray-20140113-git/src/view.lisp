(in-package :xarray)

;;;; Views are (sub)representations of xarray-able objects, which
;;;; provide an xarray interface.  Their sole purpose is mapping the
;;;; subscripts they are addressed with into another set of
;;;; subscripts, which is used to address another object called their
;;;; ancestor.  (i.e. the original from where the view comes from)
;;;;
;;;; Below you find some standard views with agnostic (and perhaps not
;;;; super-fast) implementations.  Perhaps we can make them faster by
;;;; assuming more about the objects and sometimes combining views.
;;;; In my view, this would be premature optimization, I will do this
;;;; when I see serious examples of views being a bottleneck (which I
;;;; find quite hard to imagine). -- Tamas

;;;; ??? Currently I am allowing permutation and slice indexes to
;;;; appear more than once, this could be useful.  Should I forbid
;;;; this?  Not in the CL spirit. -- Tamas

(defclass xview ()
  ((ancestor :initarg :ancestor
	     :reader ancestor 
	     :documentation
	     "the view of an underlying object that is accessible with
                             xref")))

;; xarray API 

(defmethod xelttype ((object xview) &key list-of-rows list-of-columns)
  (xelttype (ancestor object)
	    :list-of-rows list-of-rows
	    :list-of-columns list-of-columns))

(defmethod xsimilar (rank (object xview))
  (xsimilar rank (ancestor object)))

(defmethod print-object ((object xview) stream)
  (print-unreadable-object (object stream :type t :identity t)
    ;; TAKEing the easy way out, need to write this decently one day
    (print (copy-as 'array object) stream)))

(defun original-ancestor (view)
  "Find the original ancestor (ie the one that is not a view).  Simply
returns objects which are not views.  Since views can be nested, this
needs to follow the chain backwards."
  (tagbody
     top
     (when (typep view 'xview)
       (setf view (ancestor view))
       (go top)))
  view)

;;; Permutations
;;;
;;; Permutations are a view which simply interchange the dimension
;;; indexes.

(defgeneric permutation (object &rest permutation)
  (:documentation "Takes an xarray-able object and creates a View
  which is a permutation of indexes."))

;;; permutation-view
;;;
;;; A general permutation, the only assumption is that object is
;;; xrefable.

(defclass permutation-xview (xview)
  ((permutation :initarg :permutation :type fixnum-vector
		:documentation "permutation")
   (dimensions :initarg :dimensions :reader dimensions :type fixnum-vector
	       :documentation "dimensions")))

(defmethod permutation (object &rest permutation)
  (let ((permutation (coerce permutation 'fixnum-vector)))
    (unless (vector-within-dimension-p #|valid-permutation-p|#
	     permutation (xrank object))
      (error "permutation ~a is not valid" permutation))
    (make-instance 'permutation-xview :ancestor object
		   :permutation permutation)))

(defmethod initialize-instance :after ((object permutation-xview) &key)
  ;; save dimensions
  (with-slots (ancestor permutation dimensions) object
    (setf dimensions 
	  (coerce (permute-sequence permutation (xdims ancestor))
		  'fixnum-vector)))
  object)

(defmethod xrank ((object permutation-xview))
  (xrank (ancestor object)))

(defmethod xdims ((object permutation-xview))
  (coerce (dimensions object) 'list))

(defmethod xdim ((object permutation-xview) axis-number)
  (aref (dimensions object) axis-number))

(defmethod xref ((object permutation-xview) &rest subscripts)
  (with-slots (ancestor permutation) object
    (apply #'xref ancestor (permute-sequence permutation subscripts))))

(defmethod (setf xref) (value (object permutation-xview) &rest subscripts)
  (with-slots (ancestor permutation) object
    (setf (apply #'xref ancestor (permute-sequence permutation subscripts))
	  value)))

;;;; transpose
;;;;
;;;; !!! maybe I should write transpose as a special case of
;;;; !!! permutation.  Could make it much faster.  Do it when needed.

(defgeneric transpose (object)
  (:documentation "Tranposed view.")
  (:method (object)
    (assert (= (xrank object) 2)
	    (object)
	    "Can only transpose a rank-2 object.  Current object is rank ~S" (xrank object))
    (permutation object 1 0)))

;;;; xslices
;;;;
;;;; An xslice is a view on a subset of indexes in each dimension.  When
;;;; a slice contains only a single index in a dimension, that
;;;; dimension can be dropped from the xslice.  For valid index
;;;; specifications, see parse-index-specifications.

(defgeneric xslice (object &rest index-specifications)
  (:documentation "Slice of an object."))

(defgeneric (setf xslice) (value object &rest index-specifications)
  (:documentation "setting a slice of an object."))

;;;; slice-view
;;;;
;;;; A general slice, the only assumption is that object is xrefable.

(defclass xslice-xview (xview)
  ((index-specifications :initarg :index-specifications
			 :reader index-specifications
			 :type fixnum-vector
			 :documentation "vector of index specifications")
   (dimensions :initarg :dimensions :reader dimensions
	       :type fixnum-vector
	       :documentation "dimensions, cached")))

(defun parse-index-specification (index-specification dimension)
  "Parse a index specification, returning either

- an integer i, with the dimension dropped,

- a pair (start . length), where start is the starting index, and
  length is the number of valid indexes.  If dimension is negative,
  indexing is decreasing from start.  This is used for contiguous
  indexes

- a vector of indexes, for non-contiguous indexing.

All resulting indexes are valid, ie they are integers in
[0,dimension).

Range specifications:

Negative integers are interpreted as counted backwards from the right
edge of the domain, ie i < 0 denotes element dimension-i.  The l

Valid index-specification specifications (a and b are integers):
 
 a                    index a, dimension dropped
 (list a)             index a, dimension not dropped
 (list a b)           range between a and b, inclusive.  If b < a, reversed.
 :all                 all valid indexes, increasing order
 :rev                 all valid indexes, decreasing order
 (vector i1 ... in)   vector of indexes, must be a set (no repetition)."
  (flet ((convert-and-check (i)
	   "Convert negative indexes if necessary, also check that they are valid."
	   (cond
	     ((and (<= 0 i) (< i dimension)) i)
	     ((and (minusp i) (<= 0 (+ dimension i))) (+ dimension i))
	     (t (error "subscript ~a is not in [0,~a)" i dimension)))))
    (cond
      ;; all
      ((and (symbolp index-specification) (eq index-specification :all))
       (cons 0 dimension))
      ;; all, reversed
      ((and (symbolp index-specification) (eq index-specification :rev))
       (cons (1- dimension) (- dimension)))
      ;; single index, dimension dropped
      ((integerp index-specification)
       (convert-and-check index-specification))
      ;; range or single index (dimension not dropped)
      ((and (listp index-specification) 
	    (every #'integerp index-specification))
       (ecase (length index-specification)
	 (1 (cons (convert-and-check (car index-specification)) 1))
	 (2 (destructuring-bind (a b) index-specification
	      (let ((left (convert-and-check a))
		    (right (convert-and-check b)))
		(cons left (- right left (if (<= left right) -1 1))))))))
      ;; vector, arbitrary specification
      ((vectorp index-specification)
       (if (vector-within-dimension-p
	    index-specification dimension)
	   index-specification
	   (error "~a is not a valid integer subset of [0,~a)"
		  index-specification dimension)))
      (t (error "can't interpret index-specification ~a" 
		index-specification)))))

(defun index-specification-dimension (index-specification)
  "Return dimension of parsed index-specification.  Internal function,
no error checking.  Return nil for dropped dimensions."
  (etypecase index-specification
    (integer nil)			      ; dropped
    (cons (abs (cdr index-specification)))    ; range
    (vector (length index-specification))))   ; enumerated indexes

(defmethod xslice (object &rest index-specifications)
  "Implementation note: we cache dimensions."
  (let* ((index-specifications (map 'vector
				    #'parse-index-specification
				    index-specifications
				    (xdims object)))
	 (dimensions (iter
		       (for is :in-vector index-specifications)
		       (for d := (index-specification-dimension is))
		       (when d
			 (collecting d)))))
    (assert (= (length index-specifications) (xrank object)))
    (make-instance 'xslice-xview :ancestor object
		   :index-specifications index-specifications
		   :dimensions (coerce dimensions 'fixnum-vector))))

(defmethod xrank ((object xslice-xview))
  (length (dimensions object)))

(defmethod xdims ((object xslice-xview))
  (coerce (dimensions object) 'list))

(defmethod xdim ((object xslice-xview) axis-number)
  (aref (dimensions object) axis-number))

(defun convert-slice-subscripts (index-specifications subscripts)
  "Convert subscripts using index-specifications."
  (iter
    (for is :in-vector index-specifications)
    (generate subscript :in subscripts)
    (collecting
      (etypecase is
	(integer is)
	(cons (let* ((start (car is))	    ; first index of range
		     (length* (cdr is))	    ; has sign in it
		     (length (abs length*)) ; with sign removed
		     (ss (next subscript))) ; subscript
		(unless (and (<= 0 ss) (< ss length))
		  (error "subscript ~a is not in [0,~a)" ss length))
		(+ start (* (signum length*) ss))))
	(vector (aref is (next subscript)))))))

(defmethod xref ((object xslice-xview) &rest subscripts)
  (with-slots (ancestor index-specifications dimensions) object
  ;; Check that the length of subscripts matches rank.
    (unless (= (length dimensions) (length subscripts))
      (error "incorrect number of subscripts"))
    ;; convert and apply
    (apply #'xref ancestor 
	   (convert-slice-subscripts index-specifications subscripts))))

(defmethod (setf xref) (value (object xslice-xview) &rest subscripts)
  (with-slots (ancestor index-specifications dimensions) object
  ;; Check that the length of subscripts matches rank.
    (unless (= (length dimensions) (length subscripts))
      (error "incorrect number of subscripts"))
    ;; convert and apply
    (setf (apply #'xref ancestor 
		 (convert-slice-subscripts index-specifications subscripts))
	  value)))

;;;; drop dimensions of 1

(defun drop (object)
  "Return a view with the unit dimensions dropped."
  (apply #'xslice object
	 (mapcar (lambda (d)
		   (cond
		     ((< d 1) (error "don't know how to drop zero dimensions"))
		     ((= 1 d) 0)
		     (t :all)))
		 (xdims object))))

;;;; !!!! row-major-projection is deprecated and will be removed.  I
;;;; !!!! am only using column-major projections now, and that
;;;; !!!! includes a special case too.

;; ;;;; row-major-projection
;; ;;;;
;; ;;;; A row-major-projection is a view that maps elements to an
;; ;;;; xrefable object using a flattened index calculated as if the
;; ;;;; storage model was row-major.

;; (defgeneric row-major-projection (object &rest dimensions)
;;   (:documentation "Row major projection to an xrefable object.  Total
;;   size needs to match the product of dimensions."))

;; ;;;; row-major-projection-view
;; ;;;;
;; ;;;; A general, unoptimized case that makes no assumption on the
;; ;;;; storage model.  If you are projecting onto an array, you might
;; ;;;; want to use a more specialized class if you are concerned about
;; ;;;; speed.

;; (defclass row-major-projection-view (view)
;;   ((dimensions :initarg :dimensions :reader dimensions
;; 	       :type fixnum-vector
;; 	       :documentation "dimensions")
;;    (ancestor-dimensions :initarg :ancestor-dimensions
;; 			:reader ancestor-dimensions
;; 			:type (simple-array integer (*))
;; 			:documentation "dimensions of ancestor")))

;; (defmethod initialize-instance :after ((object row-major-projection-view) &key)
;;   ;; save ancestor-dimensions
;;   (with-slots (ancestor ancestor-dimensions) object
;;     (setf ancestor-dimensions 
;; 	  (coerce (xdims ancestor) 'fixnum-vector)))
;;   ;; !!! note: do we want to cache coefficients for calculating rm-index? not now
;;   object)

;; (defmethod row-major-projection (object &rest dimensions)
;;   (unless (= (reduce #'* dimensions) (xsize object))
;;     (error "Size of the object does not match the product of dimensions."))
;;   (make-instance 'row-major-projection-view 
;; 		 :ancestor object
;; 		 :dimensions (coerce dimensions 'fixnum-vector)))

;; (defmethod xrank ((object row-major-projection-view))
;;   (xrank (ancestor object))) ; !!!! a bug? should be (length dimensions)

;; (defmethod xdims ((object row-major-projection-view))
;;   (coerce (dimensions object) 'list))

;; (defmethod xdim ((object row-major-projection-view) axis-number)
;;   (aref (dimensions object) axis-number))

;; (defmethod xsize ((object row-major-projection-view))
;;   (reduce #'* (dimensions object)))

;; (defmethod xref ((object row-major-projection-view) &rest subscripts)
;;   (with-slots (ancestor dimensions ancestor-dimensions) object
;;     (apply #'xref ancestor (rm-subscripts ancestor-dimensions
;; 					  (rm-index dimensions subscripts)))))

;; (defmethod (setf xref) (value (object row-major-projection-view)
;; 			&rest subscripts)
;;   (with-slots (ancestor dimensions ancestor-dimensions) object
;;     (setf (apply #'xref ancestor (rm-subscripts ancestor-dimensions
;; 					  (rm-index dimensions subscripts)))
;; 	  value)))


;;;; column-major-projection
;;;;
;;;; A column-major-projection is a view that maps elements to an
;;;; xrefable object using a flattened index calculated as if the
;;;; storage model was column-major.

(defgeneric column-major-projection (object &rest dimensions)
  (:documentation "Row major projection to an xrefable object.  Total
  size needs to match the product of dimensions.  If dimensions is
  omitted, it is taken to be the xsize of the object."))

;;;; column-major-projection-view
;;;;
;;;; A general, unoptimized case that makes no assumption on the
;;;; storage model.  If you are projecting onto a object with a CM
;;;; storage model, you might want to use a more specialized class if
;;;; you are concerned about speed.

(defclass column-major-projection-xview (xview)
  ((dimensions :initarg :dimensions :reader dimensions
	       :type list
	       :documentation "dimensions")
   (ancestor-dimensions :initarg :ancestor-dimensions
			:reader ancestor-dimensions
			:type list
			:documentation "dimensions of ancestor")))

(defmethod xrank ((object column-major-projection-xview))
  (length (dimensions object)))

(defmethod xdims ((object column-major-projection-xview))
  (copy-list (dimensions object)))

(defmethod xdim ((object column-major-projection-xview) axis-number)
  (nth (dimensions object) axis-number))

(defmethod xsize ((object column-major-projection-xview))
  (reduce #'* (dimensions object) :initial-value 1))

(defmethod xref ((object column-major-projection-xview) &rest subscripts)
  (with-slots (ancestor dimensions ancestor-dimensions) object
    (apply #'xref ancestor (cm-subscripts ancestor-dimensions
					  (cm-index dimensions subscripts)))))

(defmethod (setf xref) (value (object column-major-projection-xview)
			&rest subscripts)
  (with-slots (ancestor dimensions ancestor-dimensions) object
    (setf (apply #'xref ancestor (cm-subscripts ancestor-dimensions
					  (cm-index dimensions subscripts)))
	  value)))


;;;; column-major-projection-flat-view
;;;;
;;;; A special case projecting onto a flat vector.

(defclass column-major-projection-flat-xview (xview)
  ((xsize :initarg :xsize :reader xsize :type fixnum :documentation "total size")
   (ancestor-dimensions :initarg :ancestor-dimensions
			:reader ancestor-dimensions
			:type list
			:documentation "dimensions of ancestor")))

(defmethod xrank ((object column-major-projection-flat-xview))
  1)

(defmethod xdims ((object column-major-projection-flat-xview))
  (list (xsize object)))

(defmethod xdim ((object column-major-projection-flat-xview) axis-number)
  (if (zerop axis-number)
      (xsize object)
      (error 'xdim-invalid-axis-number)))

;;; xsize is a reader

(defmethod xref ((object column-major-projection-flat-xview) &rest subscripts)
  (when (cdr subscripts)
    (error 'xref-wrong-number-of-subscripts))
  (let ((index (car subscripts)))
    ;;    (assert (within-dimension-p index (xsize object)))
    (with-slots (ancestor dimensions ancestor-dimensions) object
      (apply #'xref ancestor (cm-subscripts ancestor-dimensions index)))))

(defmethod (setf xref) (value (object column-major-projection-flat-xview)
			&rest subscripts)
  (when (cdr subscripts)
    (error 'xref-wrong-number-of-subscripts))
  (let ((index (car subscripts)))
    ;;    (assert (within-dimension-p index (xsize object)))
    (with-slots (ancestor dimensions ancestor-dimensions) object
      (setf (apply #'xref ancestor (cm-subscripts ancestor-dimensions index))
            value))))


;;;; column-major-projection will choose the optimized case if there
;;;; are no dimensions given, or for a single dimension.

(defmethod column-major-projection (object &rest dimensions)
  (let ((xsize (xsize object)))
    (unless (or (null dimensions) (= (reduce #'* dimensions) xsize))
      (error "Size of the object does not match the product of dimensions."))
    (if (or (null dimensions) (equal dimensions (list xsize)))
        ;; flat
        (make-instance 'column-major-projection-flat-xview 
                       :ancestor object
                       :ancestor-dimensions (xdims object)
                       :xsize xsize)
        ;; non-flat
        (make-instance 'column-major-projection-xview 
                       :ancestor object
                       :ancestor-dimensions (xdims object)
                       :dimensions dimensions))))



(defgeneric flat (object)
  (:documentation "Flat index for an object.
 flat-xview

 An xview where elements can be accessed by a \"flat\" index on
 [0,total size), but the actual mapping is
 implementation-dependent.  Mainly used for elementwise access
 where the order of elements does not matter, especially
 elementwise reductions with commutative operations (eg sum,
 product, maximum, etc).

 There are two special considerations for this xview: (1) it only
 has to implement reading elements, not setting them, (2) it has
 to implement ancestor-subscripts, which map the flat index to
 that of the ancestor.

 NOTE: flat-xviews do NOT have to be compatible across classes!  Eg
 for Lisp arrays a flat-xview could be row-major, while for some
 other object it could be column major, etc.  Only use FLAT XVIEWs
 if you truly don't care about the order.
"))

(defclass flat-xview (xview)
  ((xsize :reader xsize :type fixnum :documentation "total size")
   (ancestor-dimensions :reader ancestor-dimensions
			:type list
			:documentation "dimensions of ancestor")))

(defmethod initialize-instance :after ((object flat-xview) &key)
  ;; save ancestor-dimensions
  (with-slots (ancestor ancestor-dimensions xsize) object
    (setf ancestor-dimensions 
	  (coerce (xdims ancestor) 'list)
          xsize (reduce #'* ancestor-dimensions)))
  object)

(defmethod flat (object)
  (make-instance 'flat-xview :ancestor object))

(defmethod xrank ((object flat-xview))
  1)

(defmethod xdims ((object flat-xview))
  (list (xsize object)))

(defmethod xdim ((object flat-xview) axis-number)
  (if (zerop axis-number)
      (xsize object)
      (error 'xdim-invalid-axis-number)))

(defmethod xref ((object flat-xview) &rest subscripts)
  (when (cdr subscripts)
    (error 'xref-wrong-number-of-subscripts))
  (let ((index (car subscripts)))
    (assert (within-dimension-p index (xsize object)))
    (apply #'xref (ancestor object) (cm-subscripts (ancestor-dimensions object) index))))

(defgeneric ancestor-subscripts (object index)
  (:documentation "Map the flat index the subscripts of the ancestor.")
  ;; The purpose of this method is to help with the implementation of
  ;; generic functions that find the subscripts of the largest element, etc.
  (:method ((object flat-xview) index)
    (assert (within-dimension-p index (xsize object)))
    (cm-subscripts (ancestor-dimensions object) index)))
