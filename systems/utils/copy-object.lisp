;;;; Methods for copying objects

(in-package :utils)

;; ############################################################################

(export '(copy-object copy-object-content))

#-ecl
(define-method-combination call-all-applicable-methods-of-copy-object-content ()
  ((methods () :required t))
  (:arguments first-argument)
 "calls all methods that are applicable one after the other"
 `(progn (loop with method-specializers 
	      = (loop for implemented-method 
		   in (closer-mop:generic-function-methods #'copy-object-content)
		   collect (closer-mop:method-specializers implemented-method))
	    for class in (closer-mop:class-precedence-list (class-of ,first-argument))
	    when (and (closer-mop:class-direct-slots class) 
		      (not (find class method-specializers :key #'first)))
	    do (error "~%Please implement copy-object-content for class ~a"
		      (class-name class)))
	 ,@(loop for method in methods collect `(call-method ,method))))

(defgeneric copy-object (object)
  (:documentation "makes and returns a copy of object")) 

(defgeneric copy-object-content (source destination)
  #-ecl
  (:method-combination call-all-applicable-methods-of-copy-object-content)
  (:documentation "copies the contents of source to destination."))

(defmethod copy-object ((object t))
  (cond ((subtypep (class-of object) 'structure-object)
	 (error "Please implement copy-object for ~a" (class-of object)))
	((functionp object)
	 object)
	(t (let ((copy (make-instance (class-of object))))
	     (copy-object-content object copy)
	     copy))))

(defmethod copy-object ((object symbol)) object)
(defmethod copy-object ((object number)) object)
(defmethod copy-object ((object string)) (copy-seq object))

(defun my-copy-object-list (list)
  "Does not crash for example in (test . test)."
  (cond
   ((null list) nil)
   ((symbolp list) (copy-object list))
   (t
    (cons (copy-object (first list))
          (my-copy-object-list (rest list))))))

(defmethod copy-object ((object list))
  (my-copy-object-list object))

(defmethod copy-object ((source hash-table))
  (let ((copy (make-hash-table
	       :test (hash-table-test source)
	       :size (hash-table-size source)
	       :rehash-size (hash-table-rehash-size source)
	       :rehash-threshold (hash-table-rehash-threshold source))))
    (maphash #'(lambda (key val)
		 (setf (gethash key copy) val))
	     source)
    copy))

;;;; Example.

;; (defclass A () ((a :accessor a :initarg :a :initform nil)))
;; (defclass B () ((b :accessor b :initarg :b :initform nil)))
;; (defclass C (B) ((c :accessor c :initarg :c :initform nil)))
;; (defclass D (A C) ((d :accessor d :initarg :d :initform nil)))

;; (defmethod copy-object-content ((source A) (destination A))
;;   (print "(copy-object-content a a)")
;;   (setf (a destination) (copy-object (a source))))

;; (defmethod copy-object-content ((source B) (destination B))
;;   (print "(copy-object-content b b)")
;;   (setf (b destination) (copy-object (b source))))

;; (defmethod copy-object-content ((source C) (destination C))
;;   (print "(copy-object-content c c)")
;;   (setf (c destination) (copy-object (c source))))
  
;; (defmethod copy-object-content ((source d) (destination d))
;;   (print "(copy-object-content d d)")
;;   (setf (d destination) (copy-object (d source))))
  
;; (defmethod print-object ((o d) stream)
;;   (format stream "<D: a: ~a, b: ~a, c: ~a, d: ~a>" (a o) (b o) (c o) (d o)))

;; (defparameter object (make-instance 'd :a '(1 2 3) :b "foo" :c 12 :d t))

;; (print (copy-object object))

;; This is the output of the last statement of the example:

;; "(copy-object-content d d)" 
;; "(copy-object-content a a)" 
;; "(copy-object-content c c)" 
;; "(copy-object-content b b)" 
;; <D: a: (1 2 3), b: foo, c: 12, d: T>


;; ############################################################################
