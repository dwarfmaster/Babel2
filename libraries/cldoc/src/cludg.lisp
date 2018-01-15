;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLUDG; -*-
;;; $Id: cludg.lisp,v 1.8 2007-01-11 00:05:06 ihatchondo Exp $
;;; ---------------------------------------------------------------------------
;;;     Title: Common Lisp Universal Documentation Generator
;;;   Created: 2005 10 23 12:30
;;;    Author: Iban Hatchondo <hatchond@yahoo.fr>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005 by Iban Hatchondo

;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package :CLUDG)

;; For use with FCG
(defun extract-documentation-fcg (driver dest-dir asdf-system  &key table-of-contents-title)
  "extracting documentation in an FCG way (conserving method numbers)"
  (clrhash *descriptor->meta-decriptors*)
  (let ((filenames  (get-asdf-system-files asdf-system)))
    (dolist (file filenames)
      (cldoc:extract-documentation 
       driver 
       dest-dir 
       (list file)
       :table-of-contents-title table-of-contents-title
       :path-prefix (namestring (asdf:component-relative-pathname asdf-system)))))
  (clrhash *descriptor->meta-decriptors*)
  t)

;;;

(defparameter *cludg-directory*
  (asdf:component-pathname (asdf:find-system :cldoc))
  "The directory in which CLUDG is stored.")

;;;
;;; Protocol methods and class.
;;;

(defclass driver () () (:documentation "This is a protocol class."))

(defgeneric extract-documentation (driver dest-dir filenames &rest initargs)
  (:documentation "Reads all files specified in filenames and extract their
   documentation using the specified output-format driver.
    Because it is performing some symbol package resolution it needs packages
   definition to be, at least loaded. A simple way to satisfy this condition
   is to load, such as require would, the systems to be documented before
   starting documentation extraction.
    If filenames is an ASDF system then its components files will be retreived
   first.
    The extracted documentation will be written in a newly created file
   with the same name as the processed one but with the filename extension
   relatives to the output driver. If dest-dir is non NIL then it will be
   used to construct the output pathname, otherwise the output pathname will
   be constructed using the one of the currently processed file. The &rest
   initargs argument is for driver specific options.
    All implementation must support the following option:
    - :path-prefix (or null string): a string that will be used to dertermine
       the source folder where files to be processed can be found. If needed,
       this path-prefix (string) will be added to each filename of the source
       list. The destination folder is then constructed given this parameter.
       If specified, the path-prefix string is pruned from the destination
       folder in order to give the smallest pathname. Default to:
       *load-truename*.
        For instance, one could write:
         ;;; (extract-documentation 'html \"/foo/doc/html\"
         ;;;   '(\"foo.lisp\" \"gee/bar.lisp\")
         ;;;   :path-prefix \"/home/zorglub/prj-foo/src/\")
       to extract the documentation of the files 'foo' and 'gee/bar', that
       are located in /home/zorglub/prj-foo/src/, under the following tree:
       /foo/doc/html/foo.html and /foo/doc/html/gee/bar.html.
        But if an ASDF system is given for filenames then the :path-prefix
       will be found using asdf:component-relative-pathname method. Then to
       extract the documentation of an ASDF system one could write:
         ;;; (extract-documentation 'html \"/foo/doc/html\"
         ;;;   (asdf:find-system :the-foo-system))")
  (:method (driver dest-dir filenames &rest initargs)
    (declare (ignorable dest-dir filenames initargs))
    (error "No extract-documentation method found for ~a." driver)))

(defgeneric dformat (sym-descriptor output-driver stream)
  (:documentation "Writes the given symbol-descriptor on the stream in the 
   wanted output format using the specified output driver.")
  (:method (sym-desc output-driver stream)
    (declare (ignorable stream))
    (error "No dformat method found for ~a with ~a." sym-desc output-driver)))

(defgeneric dformat-documentation (sym-desc output-driver stream)
  (:documentation "Writes the given symbol-descriptor documentation on the
   stream in the wanted output format using the specified output driver.")
  (:method (sym-desc output-driver stream)
    (declare (ignorable output-driver stream))
    (error "No dformat-documentation method found for ~a." sym-desc)))

(defun get-iso-date-time ()
  "Returns string with date and time according to ISO 8601."
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            year month date hour minute second)))

;;;;
;;;; Misc.
;;;;

(defvar *descriptor-handlers* (make-hash-table :test #'equal)
  "Hash table of entries of the form (handler description),
   where definer is the car of the definition form handled (for example,
   DEFUN or DEFMACRO), handler is a function which takes the form as input
   and returns the descriptor object associated with.")

(defvar *unhandled-forms* nil
  "List of un-handled forms. This list is filled at runtime.")

(defvar *current-package* "common-lisp-user")

(defvar *output-types* (make-hash-table :test #'eq)
  "Cache for string filename extension - output-format association.")

(defun extract-doc (forms)
  "Returns the first string found in the given list of forms.
   NIL is returned if no string is found before the first non
   declare form."
  (flet ((declare-p (form) (and (listp form) (eq (car form) 'DECLARE))))
    (loop for sub-form in forms
	  until (or (stringp sub-form) (not (declare-p sub-form)))
	  finally (return (and (stringp sub-form) sub-form)))))

(defun grok-new-lines (string)
  "Returns a list of the string lines contained in the given string."
  (declare (type string string))
  (with-input-from-string (stream string)
    (loop with eof = (gensym) for line = (read-line stream nil eof nil)
	  until (eq line eof) collect line)))

(defun find-package-caseless (name)
  (declare (type string name))
  (or (find-package name) (find-package (string-upcase name))))

(defun standard-io-name (string)
  (declare (type string string))
  (if (eq :upcase (readtable-case *readtable*))
      (string-upcase string)
      (string-downcase string)))

(defun cludg-mksym (&rest parts)
   "Returns a symbol name made from the concatenation of the given parts."
   (apply #'concatenate 'string (mapcar #'string parts)))

;;;;
;;;; API for form handling.
;;;;

(defmacro define-descriptor-handler (definer arglist description &body body)
  "Defines a new documentation handler. DEFINER is the car of the
   definition form handled (e.g., defun), DESCRIPTION is a one-word
   string equivalent of definer (e.g., \"function\"), and ARGLIST
   and BODY together define a function that takes the form as input
   and returns the descriptor object of the form."
  `(setf (gethash ',definer *descriptor-handlers*)
	 (list #'(lambda ,arglist
		   ,@body)
	       ,description)))

(defun find-descriptor-handler (definer)
  "Given the car of a form, finds the appropriate description
   handler for the form if one exists."
  (gethash definer *descriptor-handlers*))

(defun register-output-type (output-format output-type)
  "Register the filename extension (a string) that will be
   associated with the specified output-format."
  (declare (type string output-type))
  (setf (gethash output-format *output-types*) output-type))

(defun find-output-type (output-format)
  "Returns the filename extension (a string) that is
   associated to the specified output-format."
  (gethash output-format *output-types*))

(defmacro remap-char (char stream &rest clauses)
  ;; macro helper for define-purgers.
  `(case ,char
    ,@(loop for (c replace-string) in clauses
	    collect `(,c (format ,stream ,replace-string)))
    (t (format ,stream "~C" ,char))))

(defmacro define-purgers (&key string-purger lambda-list-purger)
  "Defines two functions of one parameter to purge from dangerous characters:
    - :string-purger (name clauses &rest options):
      Defines a function of one parameter, a string, that will purge
      that string of any `dangerous' characters for your driver.
      -- name (symbol): the name of the defined string purger function.
      -- clauses (list): a list of pair as: (character replacement-string).
      -- options (list): supported options: (:documentation string).
    - :lambda-list-purger (name clauses &rest options):
     Defines a function of one parameter, a lambda-list, that will purge
     that lambda-list of any `dangerous' characters for your driver.
       -- name (symbol): the name of the defined lambda-list purger function.
       -- clauses (list): a list of pair as: (symbol replacement-string).
          The symbols are the Common Lisp symbols that might occures in 
          lambda-list.
       -- options (list): supported options: (:documentation string)."
  (destructuring-bind (ll-purger ll-clauses &rest ll-options) lambda-list-purger
    (destructuring-bind (s-purger sp-clauses &rest sp-options) string-purger
      (let ((ll-doc (find :documentation ll-options :key #'car))
	    (sp-doc (find :documentation sp-options :key #'car)))
	`(progn 
	   (defun ,(intern (cludg-mksym s-purger)) (string)
	     ,@(when sp-doc (cdr sp-doc))
	     (with-output-to-string (stream)
	       (loop for c across string
		     do (remap-char c stream ,@sp-clauses))))
	   (defun ,(intern (cludg-mksym ll-purger)) (lambda-list)
	     ,@(when ll-doc (cdr ll-doc))
	     (flet ((make-word ()
		      (make-array 10 :adjustable t :fill-pointer 0
				  :element-type 'character))
		    (remap-word (word stream)
		      (when (> (length word) 0)
			(cond 
			  ,@(loop for (string replace-string) in ll-clauses
				  collect `((string-equal word ,string)
					    (format stream ,replace-string)))
			  (t (loop for c across word
				   do (remap-char c stream ,@sp-clauses)))))))
	       (with-output-to-string (result)
		 (loop with word = (make-word)
		       for char across (format nil "~{~s~^ ~}" lambda-list)
		       if (member char '(#\( #\) #\Space) :test #'char=)
		       do (remap-word word result)
		          (remap-char char result ,@sp-clauses)
			  (setf word (make-word))
		       else do (vector-push-extend char word)
		       finally (remap-word word result))))))))))

(defmacro with-descriptor-read ((filespec descriptor) &body body)
  "with-descriptor-read uses open to create a file stream to file named by
   filespec. Filespec is the name of the file to be opened.
     with-descriptor-read evaluates the body as an implicit progn with the
   descriptor bound to the successive values returned by the handler of the
   currently readed form. If no handler exists then the name of the readed
   form will be added to the list of unhandled forms: *unhandled-forms*.
     When control leaves the body, either normally or abnormally (such as by
   use of throw), the file is automatically closed."
  `(with-open-file (is ,filespec :direction :input :if-does-not-exist :error)
     (labels ((parse-form (form)
		(let ((handler (car (find-descriptor-handler (car form)))))
		  (if handler 
		      (multiple-value-bind (,descriptor restart subforms)
			  (funcall handler form)
			(if restart 
			    (loop for sub in subforms do (parse-form sub))
			    (when ,descriptor ,@body)))
		      (push (car form) *unhandled-forms*)))))
       (loop with eof = (gensym)
	     with *print-case* = :downcase
	     for form = (read is nil eof nil) until (eq form eof)
	     do (parse-form form)))))

;;; Pathname manipulation.

(defun add-prefix-if-necessary (prefix filenames)
  "Adds the specified prefix to each filename when necessary."
  (loop for file in filenames
	if (probe-file (merge-pathnames file prefix))
	   collect (namestring (merge-pathnames file prefix))
	else if (probe-file file) collect file
	else do (error "file not found: ~a" file)))

(defun make-pathname-relative (&key from to)
  "Returns the relative pathname that goes from `from' to `to'."
  (declare (type (or null pathname string) from to))
  (when (null from) (return-from make-pathname-relative to))
  (flet ((make-up (nth) (and (> nth 0) (make-list nth :initial-element :up))))
    (loop with nth = 1
	  with fdirs = (pathname-directory from) 
	  with tdirs = (pathname-directory to)
	  for fdir in (cdr fdirs) and tdir in (cdr tdirs)
	  while (string= fdir tdir) do (incf nth)
	  finally 
	   (let ((up (make-up (- (length fdirs) nth))))
	     (push :relative up)
	     (return 
	       (make-pathname
		 :directory (nconc up (nthcdr nth tdirs))
		 :name (pathname-name to)
		 :type (pathname-type to)))))))

(defun default-pathname (lisp-file dest-dir)
  "Returns the common directories between the lisp-file and the dest-dir."
  (loop with fdirs = (pathname-directory lisp-file) 
	with odirs = (pathname-directory dest-dir)
	for fdir in (cdr fdirs) and odir in (cdr odirs)
	while (string= fdir odir) collect odir into path
	finally (return (make-pathname :directory (cons :absolute path)))))

(defun make-output-pathname (lisp-file dest-dir output-type &optional prefix)
  "Returns the pathname of the output file where to write the documentation
   to. The resulting pathname will be relative to the dest-dir. Any common
   pathname denominator between the lisp-file truename and the dest-dir
   truename will be pruned."
  (ensure-directories-exist dest-dir)
  (setf lisp-file (truename lisp-file))
  (setf dest-dir (truename dest-dir))
  (let ((default-path (default-pathname lisp-file dest-dir)))
    (when prefix
      (setf lisp-file (enough-namestring lisp-file (truename prefix))))
    (ensure-directories-exist
        (make-pathname 
	  :directory (pathname-directory
		      (merge-pathnames
		       (enough-namestring lisp-file default-path)
		       dest-dir))
	  :name (pathname-name lisp-file)
	  :type (find-output-type output-type)))))

 (defun get-asdf-system-files (asdf-module &optional (directory ""))
   (loop for component in (asdf:module-components asdf-module)
	 for relative-pathname = (asdf:component-relative-pathname component)
	 if (typep component 'asdf:cl-source-file)
	   collect (namestring (merge-pathnames relative-pathname directory))
	 else if (typep component 'asdf:module)
           append (get-asdf-system-files component relative-pathname)))

;;;;
;;;; Protocol descripor classes.
;;;;

(defclass symbol-descriptor ()
  ((package :type string :initarg :package :initform "" :reader dpackage)
   (name :type string :initarg :name :reader name)
   (type :type string :initarg :type :reader desc-type)
   (doc :type (or null string) :initarg :doc :reader doc))
  (:documentation "This is a protocol class and so must not be instancied."))

(defclass structured-object-descriptor (symbol-descriptor)
  ((inheritence :type list :initarg :inheritence :reader inheritence)
   (slots :type list :initarg :slots :reader slots)
   (slot-accessors :type list :reader slot-accessors :initform nil))
  (:documentation "This is a protocol class and so must not be instancied."))

(defclass param-descriptor (symbol-descriptor)
  ((value :type list :initarg :value :reader value))
  (:documentation "This is a protocol class and so must not be instancied."))

(defclass lambda-descriptor (symbol-descriptor)
  ((lambda-list
     :type list
     :initarg :lambda-list
     :reader lambda-list)
   (qualifiers
     :type list
     :initform nil
     :initarg :qualifiers
     :reader method-qualifiers))
  (:documentation "This is a protocol class and so must not be instancied."))

(defclass slot-descriptor (symbol-descriptor) 
  ((cname :type string :initarg :class-name :reader cname)
   (readers :type list :initform nil :initarg :readers :reader readers)
   (writers :type list :initform nil :initarg :writers :reader writers)
   (accessors :type list :initform nil :initarg :accessors :reader accessors)
   (initargs :type list :initform nil :initarg :initargs :reader initargs)
   (initform :type list :initform nil :initarg :initform :reader initform)
   (allocation-type
     :type keyword
     :initarg :allocation-type
     :initform :instance
     :reader allocation-type))
  (:documentation "This descripor is made for describe the slots of 
    class, conditon and structure."))

(defgeneric fully-qualified-name (symbol-descriptor)
  (:documentation "Returns the string that represent the fully qualified
   name of the given symbol-descriptor.")
  (:method ((symdesc symbol-descriptor))
    (with-slots ((pname package) name) symdesc
      (multiple-value-bind (sym status)
	  (find-symbol (standard-io-name name) (find-package-caseless pname))
	(declare (ignore sym))
	(format nil "~(~a~):~:[~;:~]~a" pname (eq status :internal) name)))))

;;;
;;; Common Lisp common descripor classes.
;;;

(defclass defpackage-descriptor (symbol-descriptor) ()
  (:documentation "This descripor is made for: defpackage"))

(defclass defconstant-descriptor (param-descriptor) ()
  (:documentation "This descriptor is made for: defconstant."))

(defclass defparameter-descriptor (param-descriptor) ()
  (:documentation "This descriptor is made for: defparameter."))

(defclass defvar-descriptor (param-descriptor) ()
  (:documentation "This descriptor is made for: defvar."))

(defclass deftype-descriptor (param-descriptor)
  ((args :type list :initarg :args :reader deftype-args))
  (:documentation "This descriptor is made for: deftype."))

(defclass defun-descriptor (lambda-descriptor) ()
  (:documentation "This descriptor is made for: defun."))

(defclass defmacro-descriptor (lambda-descriptor) ()
  (:documentation "This descriptor is made for: defmacro."))

(defclass defsetf-descriptor (lambda-descriptor) ()
  (:documentation "This descriptor is made for: defsetf."))

(defclass defsetf-short-descriptor (defsetf-descriptor)
  ((update-fn :initarg :update-fn :initform nil :reader update-fn)
   (lambda-list :initform nil))
  (:documentation "This descriptor handles the short form of defsetf:
   (defsetf foo update-fn docstring)"))

(defclass defsetf-long-descriptor (defsetf-descriptor)
  ((extra-args
     :type list
     :initform nil
     :initarg :extra-args
     :reader extra-args))
  (:documentation "This descriptor handles the long form of defsetf:
   (defsetf name lambda-list (values-to-store) doc-string body)"))

(defclass defgeneric-descriptor (lambda-descriptor) ()
  (:documentation "This descriptor is made for: defgeneric."))

(defclass defmethod-descriptor (lambda-descriptor) ()
  (:documentation "This descriptor is made for: defmethod."))

(defclass defclass-descriptor (structured-object-descriptor) ()
  (:documentation "This descriptor is made for: defclass."))

(defclass define-condition-descriptor (structured-object-descriptor) ()
  (:documentation "This descriptor is made for: define-condition."))

(defclass defstruct-descriptor (structured-object-descriptor)
  ((constructors :initform nil :type list :reader constructors)
   (predicate :initform nil :type (or null lambda-descriptor) :reader predicate)
   (copier :initform nil :type (or null lambda-descriptor) :reader copier)
   (printer :initform nil :type (or null lambda-descriptor) :reader printer)
   (named :initform nil :type boolean :reader named)
   (struct-type :initform 'class :reader struct-type)
   (conc-name :initform nil :type (or null symbol) :reader conc-name))
  (:documentation "This descriptor is made for: defstruct."))

(defclass defstruct-slot-descriptor (slot-descriptor) ()
  (:documentation "This descriptor is made for structure slots."))

;;; Fully qualified name handling.

(defclass in-package-form (symbol-descriptor) 
  ((in-package :type string :initarg :in-package :reader dest-package)))

(defun split-name (name &optional package-name)
  (let* ((space (position #\Space name))
	 (colon (position #\: name :start (if space (1+ space) 0))))
    (if colon 
	(let* ((colon2 (position #\: name :start (1+ colon)))
	       (name-part0 (when space (subseq name 0 (1+ space))))
	       (name-part1 (subseq name (1+ (or colon2 colon)))))
	  (values (concatenate 'string name-part0 name-part1)
		  (subseq name (if space (1+ space) 0) colon)))
	(values name (or package-name *current-package*)))))

(defun resolve-symbol-package-name (symbol &optional package-name)
  "Returns the home package name of symbol and the name of symbol."
  (multiple-value-bind (sym-name pname)
      (split-name (format nil "~s" symbol) package-name)
    (let* ((package (find-package-caseless pname))
	   (sym (find-symbol (standard-io-name sym-name) package)))
      (values (package-name (symbol-package sym)) sym-name))))

(defmethod initialize-instance :after ((desc symbol-descriptor) &rest rest)
  (declare (ignore rest))
  (with-slots (name package) desc
    (multiple-value-setq (name package) (split-name name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;                                                                       ;;;;
;;;;                   Common Lisp form handlers declaration               ;;;;
;;;;                                                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-descriptor-handler IN-PACKAGE (form)
  "in-package"
  (let ((package (find-package-caseless (format nil "~a" (second form)))))
    (when package
      (setf *current-package* (package-name package))))
  (make-instance 'in-package-form
    :name (format nil "~s" (first form))
    :in-package *current-package*
    :type ""))

(define-descriptor-handler EVAL-WHEN (form)
  "eval-when"
  (values nil :restart (cddr form)))

(define-descriptor-handler PROGN (form)
  "progn"
  (values nil :restart (cdr form)))

;; => Shall we handle the following forms ?
;; set-dispatch-macro-character
;; define-compiler-macro
;; define-method-combination
;; define-modify-macro
;; define-setf-expander
;; define-symbol-macro
;; let/flet/macrolet

;;;;
;;;; Standard handlers.
;;;;

(define-descriptor-handler DEFPACKAGE (form)
  "package"
  (make-instance 'defpackage-descriptor
    :type (format nil "~s" (first form))
    :name (package-name (find-package-caseless (format nil "~a" (second form))))
    :doc (second (find :documentation (cddr form) :key #'car))))

(define-descriptor-handler DEFCONSTANT (form)
  "constant"
  (make-instance 'defconstant-descriptor
    :type (format nil "~s" (first form))
    :name (format nil "~s" (second form))
    :value (list (third form))
    :doc (fourth form)))

(define-descriptor-handler DEFPARAMETER (form)
  "parameter"
  (make-instance 'defparameter-descriptor
    :type (format nil "~s" (first form))
    :name (format nil "~s" (second form))
    :value (list (third form))
    :doc (fourth form)))  

(define-descriptor-handler DEFVAR (form)
  "variable"
  (make-instance 'defvar-descriptor
    :type (format nil "~s" (first form))
    :name (format nil "~s" (second form))
    :value (list (third form))
    :doc (fourth form)))

(define-descriptor-handler DEFTYPE (form)
  "type"
  (make-instance 'deftype-descriptor
    :type (format nil "~s" (first form))
    :name (format nil "~s" (second form))
    :args (third form)
    :value (last form)
    :doc (extract-doc (cdddr form))))

(define-descriptor-handler DEFUN (form)
  "function"
  (make-instance 'defun-descriptor
    :type (format nil "~s" (first form))
    :name (format nil "~s" (second form))
    :lambda-list (third form)
    :doc (extract-doc (cdddr form))))

(define-descriptor-handler DEFMACRO (form)
  "macro"
  (make-instance 'defmacro-descriptor
    :type (format nil "~s" (first form))
    :name (format nil "~s" (second form))
    :lambda-list (third form)
    :doc (extract-doc (cdddr form))))

(define-descriptor-handler DEFSETF (form)
  "setf mapping"
  ;; name args doc. 
  ;; defsetf has two formats:
  ;;    (defsetf name update-fn doc)
  ;;    (defsetf name lambda-list (store-variable) doc-string body)
  (if (listp (third form))
      (make-instance 'defsetf-long-descriptor
	:type (format nil "~s" (first form))
	:name (format nil "(setf ~s)" (second form))
	:extra-args (third form)
	:lambda-list (fourth form)
	:doc  (extract-doc (cddddr form)))
      (make-instance 'defsetf-short-descriptor
	:type (format nil "~s" (first form))
	:name (format nil "(setf ~s)" (second form))
	:update-fn (third form)
	:doc (let ((d (fourth form))) (and (stringp d) d)))))

;;;
;;; Auto documentation control string.
;;; Redefine the following control string if you want to localize
;;; or change the automated generated documention for 
;;; desfstruct/define-condtion/class's readers/writers/accessors/...
;;;

(defvar *copier-control-string*
  "Returns a copy of the specified ~s."
  "Defstruct copier function control string for automatic documentation.
   This control string has one parameter.")

(defvar *predicate-control-string*
  "Returns T if the specified object is of type ~s."
  "Defstruct predicate function control string for automatic documentation.
   This control string has one parameter.")

(defvar *printer-control-string* 
  "Prints the specified ~s on the given stream."
  "Defstruct printer function control string for automatic documentation.
   This control string has one parameter.")

(defvar *constructor-control-string*
  "Returns a newly created ~s."
  "Defstruct constructor function control string for automatic documentation.
   This control string has one parameter.")

(defvar *slot-reader-control-string*
  "Returns the ~a of the specified ~a~%"
  "Slot reader method control string for automatic documentation.
   This control string has two parameters: slot-name, class-name.")

(defvar *slot-writer-control-string*
  "Changes the ~a of the specified ~a~%"
  "Slot writer method control string for automatic documentation.
   This control string has two parameters: slot-name, class-name.")

(defvar *slot-accessor-control-string*
  "Returns and (with setf) changes the ~a of the specified ~a~%"
  "Slot accessor method control string for automatic documentation.
   This control string has two parameters: slot-name, class-name.")

;;;
;;; defstruct machinery.
;;;

(defun mk-fname (s1 s2 &aux (pkg (find-package-caseless *current-package*)))
  (with-standard-io-syntax (intern (cludg-mksym s1 s2) pkg)))

(defun make-defun (name lambda-list &rest docs)
  (when name
    (make-instance 'defun-descriptor
      :type "defun"
      :name (format nil "~s" name)
      :lambda-list lambda-list
      :doc (apply #'format nil docs))))

(defun make-constructor (fname sname lambda-list given-p slots)
  (labels ((make-arg (slot-def)
	     (destructuring-bind (name &optional initform &rest rest)
		 (if (listp slot-def) slot-def (list slot-def))
	       (declare (ignore rest))
	       (if initform (list name initform) name)))
	   (default-lambda-list (slots)
	     (cons '&key (loop for slot in slots collect (make-arg slot)))))
    (let ((lambda-list (if given-p lambda-list (default-lambda-list slots))))
      (make-defun fname lambda-list *constructor-control-string* sname))))

(defun make-copier (fname sname)
  (make-defun fname (list sname) *copier-control-string* sname))

(defun make-predicate (fname sname)
  (make-defun fname (list (intern "OBJECT")) *predicate-control-string* sname))

(defun make-printer (fname sname option)
  (make-defun
      fname
      (if (eq option :print-object)
	  (list sname (intern "STREAM"))
	  (list sname (intern "STREAM") (intern "DEPTH")))
      *printer-control-string*
      sname))

(defun find-option (key options)
  (loop for option in options
	unless (listp option) do (setf option (list option)) end
	when (eq (car option) key) do (return option)))	

(defun parse-defstruct-option (name struct-desc form typed-p named-p slot-defs)
  (with-slots
	(struct-type constructors copier predicate
	 printer inheritence conc-name)
      struct-desc
    (destructuring-bind (option &rest args) form
      (case option
	(:conc-name 
	 (destructuring-bind (&optional (prefix (mk-fname name '-))) args
	   (setf conc-name prefix)))
	(:constructor
	 (destructuring-bind
	       (&optional (fname (mk-fname 'MAKE- name)) (ll nil given-p))
	     args
	   (if fname
	       (push
		(make-constructor fname name ll given-p slot-defs)
		constructors)
	       (setf constructors nil))))
	(:copier 
	 (destructuring-bind (&optional (fname (mk-fname 'COPY- name))) args
	   (setf copier (make-copier fname name))))
	(:predicate 
	 (destructuring-bind (&optional (fname (mk-fname name '-P))) args
	   (when named-p
	     (setf predicate (make-predicate fname name)))))
	((:print-function :print-object)
	 (destructuring-bind (&optional (fname (gensym))) args
	   (unless typed-p
	     (setf printer (make-printer fname name option)))))
	(:include
	 (when args
	   (destructuring-bind (super &rest slots) args
	     (declare(ignore slots))
	     (setf inheritence (list super)))))
	(:type (setf struct-type (or (car args) 'class)))))))

(defun parse-defstruct-options (sname struct-desc forms &optional slot-defs)
  (with-slots (named name struct-type) struct-desc
    (let ((type (second (find-option :type forms))))
      (setf struct-type (or type 'class))
      (setf named (if (find-option :named forms) T (not type)))
      (loop for form in forms
	    unless (listp form) do (setf form (list form)) end
	    do (parse-defstruct-option
		   sname struct-desc form type named slot-defs)))))

(defun parse-defstruct-slot (struct-desc slot)
  (with-slots (name conc-name) struct-desc
    (destructuring-bind (slot-name &optional initform &key type read-only) slot
      (let ((slot-acc (if conc-name (mk-fname conc-name slot-name) slot-name)))
	(make-instance 'defstruct-slot-descriptor
	  :class-name name
	  :name (format nil "~s" slot-name)
	  :type (format nil "~s" type)
	  :initform (if (listp initform) initform (list initform))
	  :readers (when read-only (list slot-acc))
	  :accessors (unless read-only (list slot-acc)))))))

(defun parse-defstruct-slots (struct-desc slots)
  (loop for slot in slots
	collect (parse-defstruct-slot
		    struct-desc (if (listp slot) slot (list slot)))))

(defun make-slot-accessors (type names name cname lambda-list control-string)
  (mapcar #'(lambda (function-name)
	      (make-instance type
		 :type (if (eq type 'defun-descriptor) "defun" "defgeneric")
		 :name (format nil "~s" function-name)
		 :lambda-list lambda-list
		 :doc (format nil control-string name cname)))
	  names))

(defun make-slot-lambdas (class-desc lambda-list)
  (with-slots ((cname name) slot-accessors slots) class-desc
    (flet ((slot-acc (slot-name names control-string)
	     (when names
	       (push (make-slot-accessors
		      (if (typep class-desc 'defstruct-descriptor)
			  'defun-descriptor 'defgeneric-descriptor)
		      names slot-name cname lambda-list control-string)
		     slot-accessors))))
      (loop for slot in slots
	    do (with-slots (readers writers accessors name) slot
		 (slot-acc name readers *slot-reader-control-string*)
		 (slot-acc name writers *slot-writer-control-string*)
		 (slot-acc name accessors *slot-accessor-control-string*))))
    (setf slot-accessors (apply #'concatenate 'list slot-accessors))))

(define-descriptor-handler DEFSTRUCT (form)
  "structure"
  (let* ((slot-defs (if (stringp (third form)) (cdddr form) (cddr form)))
	 (options (if (listp (second form)) (second form) (list (second form))))
	 (sname (first options))
	 (desc (make-instance 'defstruct-descriptor
		 :type (format nil "~s" (first form))
		 :name (format nil "~s" sname)
		 :inheritence nil
		 :doc (when (stringp (third form)) (third form))))
	 (*current-package* (dpackage desc)))
    (parse-defstruct-options sname desc '(:copier :predicate :conc-name))
    (unless (find-option :constructor options)
      (parse-defstruct-options sname desc '(:constructor) slot-defs))
    (with-slots ((struct-slots slots)) desc
      (setf struct-slots (parse-defstruct-slots desc slot-defs))
      (make-slot-lambdas desc (list (first options))))
    (parse-defstruct-options sname desc (cdr options) slot-defs)
    desc))

;;;; 
;;;; CLOS Related handlers.
;;;;

(defun find-qualifiers-and-lambda-list (forms)
  "Returns the list of qualifiers if any and the arguments lambda-list."
  (do ((qualifiers '() (list* (first forms) qualifiers))
       (forms forms (rest forms)))
      ((listp (first forms))
       (values (nreverse qualifiers) (first forms)))))

(defun handle-slots (class-name slots)
  "Returns a list of slot-descriptor object that represent the 
   given list slot definition."
  ;; Class slots description
  ;; {:reader reader-function-name}* | 
  ;; {:writer writer-function-name}* | 
  ;; {:accessor reader-function-name}* | 
  ;; {:allocation allocation-type} | 
  ;; {:initarg initarg-name}* | 
  ;; {:initform form} |
  ;; {:type type-specifier} |
  ;; {:documentation string} 
  ;;
  ;; Condition slots description  
  ;; {:reader symbol}* |  
  ;; {:writer function-name}* |  
  ;; {:accessor symbol}* |  
  ;; {:allocation allocation-type} |  
  ;; {:initarg symbol}* |  
  ;; {:initform form} |  
  ;; {:type type-specifier}
  (flet ((get-repeated (options)
	   (loop for (key value) on options by #'cddr
		 when (eq key :reader) collect value into readers end
		 when (eq key :writer) collect value into writers end
		 when (eq key :accessor) collect value into accessors end
		 when (eq key :initarg) collect value into initargs end
		 finally (return (list readers writers accessors initargs))))
	 (listify (a) (if (listp a) a (list a))))
    (loop for slot in (mapcar #'listify slots)
	  for options = (cdr slot)
	  for (readers writers accessors initargs) = (get-repeated options)
	  collect
	    (make-instance 'slot-descriptor
	      :class-name class-name
	      :name (format nil "~s" (first slot))
	      :type (format nil "~s" (getf options :type))
	      :doc (getf options :documentation)
	      :initform (listify (getf options :initform))
	      :allocation-type (getf options :allocation :instance)
	      :readers readers :writers writers :accessors accessors
	      :initargs initargs))))

(define-descriptor-handler DEFMETHOD (form)
  "method"
  ;; name arglist documentation-string
  ;; (defmethod name {qualifiers}* lambda-list [ {decl}* || doc ] body)
  (multiple-value-bind (qualifiers lambda-list) 
      (find-qualifiers-and-lambda-list (cddr form))
    (make-instance 'defmethod-descriptor
      :type (format nil "~s" (first form))
      :name (format nil "~s" (second form))
      :qualifiers qualifiers
      :lambda-list lambda-list		       
      :doc (extract-doc (if qualifiers (cddddr form) (cdddr form))))))

(define-descriptor-handler DEFGENERIC (form)
  "generic function"
  (make-instance 'defgeneric-descriptor
    :type (format nil "~s" (first form))
    :name (format nil "~s" (second form))
    :lambda-list (third  form)
    :doc (second (find :documentation (cdddr form) :key #'car))))

(define-descriptor-handler DEFCLASS (form)
  "class"
  (let ((class
	 (make-instance 'defclass-descriptor
	   :type (format nil "~s" (first form))
	   :name (format nil "~s" (second form))
	   :inheritence (third form)
	   :slots (handle-slots (format nil "~s" (second form)) (fourth form))
	   :doc (second (find :documentation (nthcdr 4 form) :key #'car)))))
    (make-slot-lambdas class (list (second form)))
    class))

(define-descriptor-handler DEFINE-CONDITION (form)
  "condition"
  (let ((condition
	 (make-instance 'define-condition-descriptor
	    :type (format nil "~s" (first form))
	    :name (format nil "~s" (second form))
	    :inheritence (third form)
	    :slots (handle-slots (format nil "~s" (second form)) (fourth form))
	    :doc (second (find :documentation (nthcdr 4 form) :key #'car)))))
    (make-slot-lambdas condition (list (second form)))
    condition))

;;;
;;; Dummy text driver.
;;;

;; Associate "txt" extension with the :text output format.
(register-output-type :text "txt")

(defclass text (driver) ()
  (:documentation "Dummy CLUDG text driver."))

(defmethod extract-documentation
    ((text (eql 'text)) dest-dir (system asdf:system) &rest rest)
  (let ((files (get-asdf-system-files system)))
    (unless (getf rest :path-prefix)
      (setf (getf rest :path-prefix)
	    (namestring (asdf:component-relative-pathname system))))
    (apply #'extract-documentation text dest-dir files rest)))

(defmethod extract-documentation ((text (eql 'text)) dest-dir files &rest rest)
  (declare (ignorable text))
  (cond ((not (stringp dest-dir)) (setf dest-dir "./"))
	((char/= (char dest-dir (1- (length dest-dir))) #\/)
	 (setf dest-dir (concatenate 'string dest-dir "/"))))
  (ensure-directories-exist dest-dir)
  (let ((path-prefix
	 (or (getf rest :path-prefix)
	     (directory-namestring (or *load-truename* "."))))
	(*unhandled-forms* '()))
    (setf files (add-prefix-if-necessary path-prefix files))
    (loop with *print-case* = :downcase
	  for ifile in files
	  for of = (make-output-pathname ifile dest-dir :text path-prefix)
	  do (with-open-file (os of :direction :output :if-exists :supersede)
	       (with-descriptor-read (ifile descriptor)
		 (dformat descriptor :text os))))
    *unhandled-forms*))

(defmethod dformat-documentation (desc (driver (eql :text)) os)
  (format os "~@[~%;;;~%;;~{~<~%;;~:;;   ~a~>~^~}~2%~]"
	  (when (doc desc) (grok-new-lines (doc desc)))))
  
(defmethod dformat (desc (driver (eql :text)) os)
  (format os "~%~78<;;; ~a~;[~a]~>" (name desc) (desc-type desc))
  (dformat-documentation desc driver os))

(defmethod dformat ((desc lambda-descriptor) (driver (eql :text)) os)
  (format os "~%~78<;;; ~a~;[~a]~>" (name desc) (desc-type desc))
  (format os "~@[~%~<;;;     ~{~a~^ ~}~>~]" (lambda-list desc))
  (dformat-documentation desc driver os))

(defmethod dformat ((desc in-package-form) (driver (eql :text)) os)
  (declare (ignorable desc driver os)))
