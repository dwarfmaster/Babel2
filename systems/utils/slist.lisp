;;;; Sorted list ADT.

(in-package #:utils)

;; ############################################################################
;; slist element base ADT:
;; ----------------------------------------------------------------------------

(export '(slist-element
          inc-weight
          modified-weight
          weight))

(defclass slist-element ()
  ((slists
    :documentation "The lists of slists in which this element is listed."
    :initarg :slists
    :initform '()
    :accessor slists)))

(defun inc-weight (el delta &key (slot-name 'weight) (max-value 1))
  (declare (type slist-element el))
  (let ((new-weight (min (+ (slot-value el slot-name) delta) max-value)))
    (setf (slot-value el slot-name) new-weight)
    (modified-weight el)
    new-weight))

(defun modified-weight (el)
  (declare (type slist-element el))
  (loop for slist in (slot-value el 'slists)
        do (slist-update slist el)))


;; ############################################################################
;; sorted list ADT:
;; ----------------------------------------------------------------------------

(export '(slist
	  make-slist
	  slist-elements))

(defstruct (slist (:constructor make-slist (weight-slot-name)))
  "A list of elements that is kept sorted according to the weight of the
   elements.
   Slots:
   - weight-slot-name: the name of the slot in which to find the weight of an
        element.
   - elements: the elements in the sorted list"
  (weight-slot-name nil :type symbol)
  (elements '() :type list))

;; TODO: consider if the elements should be optionally copied
;; (defmethod copy-object-content ((source slist) (target slist))
;;   (setf (slist-elements target) (slist-elements source)))
;;
(defmethod copy-object ((slist slist))
  slist)

;; ----------------------------------------------------------------------------

(export '(slist-size
	  slist-insert
	  slist-remove
	  slist-update
          dec-all-weights))

(defun slist-size (slist)
  "Return the number of elements in the sorted list."
  (declare (type slist slist))
  (length (slist-elements slist)))

(defun sle-weight (slist element)
  (declare (type slist slist)
           (type slist-element element))
  (slot-value element (slist-weight-slot-name slist)))

(defun slist-insert-aux (slist element)
  "Insert an element and the associated weight in the sorted list, and return the
   slist."
  (declare (type slist slist)
           (type slist-element element))
  (let ((weight (sle-weight slist element)))
    (labels ((rec (rest)
               (cond ((null rest) (list element))
                     ((> weight (sle-weight slist (car rest)))
                      (cons element rest))
                     (t (cons (car rest) (rec (cdr rest)))))))
      (setf (slist-elements slist) (rec (slist-elements slist))))
    slist))

(defun slist-insert (slist element)
  "Insert an element and the associated weight in the sorted list, and return the
   slist."
  (declare (type slist slist)
           (type slist-element element))
  (unless (member slist (slot-value element 'slists))
    (push slist (slot-value element 'slists))
    (slist-insert-aux slist element)))

(defun slist-remove (slist element)
  "Remove the given element from the sorted-list and return the slist."
  (declare (type slist slist)
           (type slist-element element))
  (when (member slist (slot-value element 'slists))
    (setf (slot-value element 'slists) (remove slist (slot-value element 'slists)))
    (setf (slist-elements slist) (remove element (slist-elements slist)))
    slist))

(defun slist-update (slist element)
  "Update the slist when the weight of the given element has changed, and
   return the slist."
  (declare (type slist slist)
           (type slist-element element))
  (when (member slist (slot-value element 'slists))
    (setf (slist-elements slist) (remove element (slist-elements slist)))
    (slist-insert-aux slist element)
    slist))

(defun dec-all-weights (slist delta &key (removal-threshold 0))
  (declare (type slist slist))
  (setf (slist-elements slist)
        (loop with slot-name = (slist-weight-slot-name slist)
              for element in (slist-elements slist)
              for new-weight = (max (- (slot-value element slot-name) delta) 0)
              when (> new-weight removal-threshold)
              do (setf (slot-value element slot-name) new-weight)
              and collect element)))


;; ############################################################################
