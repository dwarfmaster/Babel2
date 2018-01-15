;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-
;;; $Id: cldoc.asd,v 1.1.1.1 2005/11/18 14:52:18 ihatchondo Exp $
;;; ---------------------------------------------------------------------------
;;;     Title: Common Lisp Universal Documentation Generator: system definition
;;;   Created: 2005 4 23 2:30
;;;    Author: Iban Hatchondo <hatchond@yahoo.fr>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005 by Iban Hatchondo

;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

;;; ASDF system definition for Cludg.

(defpackage :cldoc.system
  (:use :cl :asdf))

(in-package :cldoc.system)

(defsystem :cldoc
  :components
  ((:file "package")
   (:file "cludg" :depends-on ("package"))
   (:file "cache-system" :depends-on ("cludg"))
   (:file "string-parser" :depends-on ("cludg"))
   (:file "html" :depends-on ("cludg" "cache-system" "string-parser"))))

#+:sbcl
(defmethod perform :around (o f)
  ;; SBCL signals an error if DEFCONSTANT is asked to redefine a
  ;; constant unEQLly. For CLUDG's purposes, however, we are defining
  ;; structured constants (lists and arrays) not for EQLity, but for
  ;; the purposes of constant-folding operations such as (MEMBER FOO
  ;; +BAR+), so it is safe to abort the redefinition provided the
  ;; structured data is sufficiently equal.
  (handler-bind
      ((sb-ext:defconstant-uneql
	   (lambda (c)
	     ;; KLUDGE: this really means "don't warn me about
	     ;; efficiency of generic array access, please"
	     (declare (optimize (sb-ext:inhibit-warnings 3)))
	     (let ((old (sb-ext:defconstant-uneql-old-value c))
		   (new (sb-ext:defconstant-uneql-new-value c)))
	       (typecase old
		 (list (when (equal old new) (abort c)))
		 (string (when (and (typep new 'string)
				    (string= old new))
			   (abort c)))
		 (simple-vector
		  (when (and (typep new 'simple-vector)
			     (= (length old) (length new))
			     (every #'eql old new))
		    (abort c)))
		 (array
		  (when (and (typep new 'array)
			     (equal (array-dimensions old)
				    (array-dimensions new))
			     (equal (array-element-type old)
				    (array-element-type new))
			     (dotimes (i (array-total-size old) t)
			       (unless (eql (row-major-aref old i)
					    (row-major-aref new i))
				 (return nil))))
		    (abort c))))))))
    (call-next-method)))
