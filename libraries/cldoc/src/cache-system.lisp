;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLUDG; -*-
;;; $Id: cache-system.lisp,v 1.1.1.1 2005/11/18 14:52:17 ihatchondo Exp $
;;; ---------------------------------------------------------------------------
;;;     Title: Common Lisp Universal Documentation Generator cache system
;;;   Created: 2005 10 23 12:30
;;;    Author: Iban Hatchondo <hatchond@yahoo.fr>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005 by Iban Hatchondo

;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

;;; Caching machinery for hyper link creation.

(in-package :cludg)

;;;

(defvar *name->meta-decriptors* (make-hash-table :test #'equal))
(defvar *descriptor->meta-decriptors* (make-hash-table :test #'eq))

(defstruct (meta-descriptor)
  "Holds a symbol-descriptor object with enough additional information to
   construct indexes for cross referencing and hyper link creation."
  (desc nil :type (or null symbol-descriptor))
  (file "" :type string)
  (anchor "" :type string)
  (index -1 :type fixnum))

(defun clear-cache ()
  "Clears meta descriptor cache."
  (clrhash *name->meta-decriptors*))
;  (clrhash *descriptor->meta-decriptors*))
;; Commented out by Paul)

(defun cache-meta-descriptors ()
  "Returns all the meta-descriptors of the cache system."
  (loop for val being each hash-value in *descriptor->meta-decriptors*
	collect val))  

(defun cache-descriptor (descriptor index src-file dest-dir prefix output-type)
  (declare (type symbol-descriptor descriptor))
  (declare (type string src-file))
  (let* ((doc-file (make-output-pathname src-file dest-dir output-type prefix))
	 (meta-desc (make-meta-descriptor
		      :desc descriptor
		      :file (enough-namestring doc-file (truename dest-dir))
		      :anchor (format nil "_~a" (gensym))
		      :index index)))
    (push meta-desc (gethash (name descriptor) *name->meta-decriptors*))
    (setf (gethash descriptor *descriptor->meta-decriptors*) meta-desc)))

(defun belongs-p (desc package-name)
  "Returns T if the specified symbol-descriptor belongs to the named package.
   The package search will be case insensitive."
  (with-slots (package) desc
    (eq (find-package-caseless package) (find-package-caseless package-name))))

(defun meta-descriptor-href (meta-desc &optional relative)
  "Returns the meta href href that links the specified descriptor."
  (let ((file (meta-descriptor-file meta-desc)))
    (when (eq relative :local) (setf relative file))
    (format nil "~a#~a" 
	    (namestring (make-pathname-relative :from relative :to file))
	    (meta-descriptor-anchor meta-desc))))

(defun lookup-meta-descriptor (desc &optional desc-type package)
  "Returns the meta-descriptor structure if any that holds a symbol-descriptor
   that is eq to desc if desc if a symbol-descriptor object. If desc is a
   string that names a descriptor then the meta-descriptor that holds a
   symbol-descriptor with name desc and type desc-type is returned.
    If package is specified then the returned meta-descriptor must be located
   in the specified package. 
    desc-type, if given can be a symbol specifying a class type or a string
   that should be equal to the desc-type slot of the symbol descriptor holds
   by the meta descriptor candidate."
  (declare (type (or string symbol-descriptor) desc))
  (when (typep desc 'symbol-descriptor)
    (let ((meta-descriptor (gethash desc *descriptor->meta-decriptors*))) 
      (return-from lookup-meta-descriptor meta-descriptor)))
  (flet ((type-p (sd type)
	   (if (stringp type) (string= (desc-type sd) type) (typep sd type))))
    (loop for md in (gethash desc *name->meta-decriptors*)
	  when (and md (type-p (meta-descriptor-desc md) desc-type))
	   when (or (not package) (belongs-p (meta-descriptor-desc md) package))
	   do (return-from lookup-meta-descriptor md))))

(defun lookup-meta-descriptor-anchor (desc &optional desc-type package)
  "Returns the meta anchor if any for the specified descriptor or named
   descriptor if desc is a string. (see: {defun lookup-meta-descriptor} )"
  (let ((meta-desc (lookup-meta-descriptor desc desc-type package)))
    (when meta-desc (meta-descriptor-anchor meta-desc))))

(defun lookup-meta-descriptor-href (desc &optional desc-type package relative)
  "Returns the meta href if any that links the specified descriptor or named
   descriptor if desc is a string. If relative is given, it must be a string
   designator for a filename. The returned href will be computed relatively
   to this filename. (see: {defun lookup-meta-descriptor} )."
  (let ((meta-desc (lookup-meta-descriptor desc desc-type package)))
    (when meta-desc (meta-descriptor-href meta-desc relative))))

(defun initialise-cache (o-dir o-type path-prefix &rest filenames)
  "Initialises the cache used to creates anchor and meta links in the futur
   generated documentation. Returns as a multiple value a list of
  (list filename (list symbol-descriptor)) and the list of unhandled forms."
  (clear-cache)
  (flet ((cache (desc index file)
	   (cache-descriptor desc index file o-dir path-prefix o-type)))
    (loop with *current-package* = "common-lisp-user"
	  with *unhandled-forms* = '()
	  for file in filenames
	  for index from 0
	  for descriptors = '()
	  do (with-descriptor-read (file desc)
	       (unless (typep desc 'in-package-form) (cache desc index file))
	       (when (typep desc 'structured-object-descriptor)
		 (loop for acc in (slot-accessors desc)
		       do (cache acc index file)))
	       (when (typep desc 'defstruct-descriptor)
		 (with-slots (predicate copier constructors) desc
		   (loop for constructor in constructors
			 do (cache constructor index file))
		   (when predicate (cache predicate index file))
		   (when copier (cache copier index file))))
	       (push desc descriptors))
	  collect (list file (reverse descriptors)) into files-and-descriptors
	  finally (return (values files-and-descriptors *unhandled-forms*)))))
