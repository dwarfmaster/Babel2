;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLUDG; -*-
;;; $Id: html.lisp,v 1.10 2006/01/08 16:49:16 ihatchondo Exp $
;;; ---------------------------------------------------------------------------
;;;     Title: Common Lisp Universal Documentation Generator: HTML driver
;;;   Created: 2005 10 23 2:30
;;;    Author: Iban Hatchondo <hatchond@yahoo.fr>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005 by Iban Hatchondo

;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

;;; XHTML 1.0 Strict CLUDG driver.

(in-package :cludg)

;;;

(defconstant +HTML-DOCTYPE+
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")

(defconstant +default-charset+ "ISO-8859-1" 
  "Default charset is: ISO-8859-1")

;; Associate "html" extension with the :html output format.
(register-output-type :html "html")

(defclass html (driver) 
  ((string-parser-initargs :type list :initarg :string-parser-initargs)
   (doc-formater
     :type function
     :initarg :doc-formater
     :initform #'format-doc
     :reader doc-formater)
   (filter
     :type (or null function)
     :initarg :filter
     :initform #'filter-defmethod-with-defgeneric
     :reader filter)
   (sort-predicate
     :type (or null function)
     :initarg :sort-predicate
     :initform #'default-sort-predicate
     :reader sort-predicate)
   (css-pathname
     :type string
     :initarg :css-pathname
     :initform (make-default-css-namestring)
     :reader css-pathname)
   (charset
     :type string
     :initarg :charset
     :initform +default-charset+
     :reader charset)
   (copy-css-into-output-dir
     :type boolean
     :initform t
     :initarg :copy-css-into-output-dir)
   (toc-title
     :type (or null string)
     :initarg :table-of-contents-title
     :initform nil
     :reader toc-title))
  (:documentation "CLDOC XHTML 1.0 Strict driver.

    Use the following options with extract-documentation to customize the
    produced output:
     - :toc-title (or null string): a string that will be used as a title in
       the table of contents. If not given then the toc title will be: Table
       of Contents.
     - :doc-formater (function): a designator for a function of two arguments.
       Its first arguments will be an html-driver instance and its second
       argument will be a list of string that represents each lines of the
       original documentation string. It is expected that this function will 
       output the strings, using the html machinery.
        The default doc-formater has some simple DWIM (Do What I Mean)
       capabilities. It recognizes both indent and empty-line paragraph breaks,
       bulleted lists, code sample, hyper link and sections (like in the
       Hyperspec). The default {defun format-doc} function delegates the 
       DWIM capabilities to the {defclass doctree} class.
     - :filter (or null function): a designator for a function of one argument.
       Its argument will be a symbol-descriptor object. The symbol-descriptor
       will be outputted if and only if this function returns NIL. 
     - :sort-predicate (or null function): a designator for a function of two
       arguments that returns a generalized boolean.
        Predicate should return true if and only if the first argument is
       strictly less than the second (in some appropriate sense). If the first
       argument is greater than or equal to the second (in the appropriate
       sense), then the predicate should return false.
     - :css-pathname (string): a string pathname designator for the css file
       to use in the generated documentation. The default one is the cludg.css
       file delivered with this driver.
     - :charset (string): a string designator for the charset of the generated
       documentation. The default one is: +default-charset+
     - :copy-css-into-output-dir (boolean): if you want the css file to be
       copied in the destination directory. This will allow your created
       documentation to be completely independent of your hard drive
       hierarchy. Otherwise relative reference to the given pathname will be
       generated. Default is set to T. And if :css-pathname is not specified
       the file delivered with CLDOC will simply be copied into the output 
       directory (see :css-pathname).

    All the options supported by the {defclass doctree} class are supported
    when passed to the {defgeneric extract-documentation} method.

    To localise the automatic documentation , if your documentation strings 
    are not in english, the default generation language, you have to modify
    the following variables: 
     - *class-inheritence*
     - *condition-inheritence*
     - *struct-inheritence*
     - *slot-reader-control-string*
     - *slot-writer-control-string*
     - *slot-accessor-control-string*
     - *copier-control-string*
     - *predicate-control-string*
     - *printer-control-string* 
     - *constructor-control-string*"))

(defun get-initargs
    (initargs &optional (default-initargs
			 '(:copy-css-into-output-dir :filter :sort-predicate
			   :charset :table-of-contents-title :doc-formater
			   :css-pathname)))
  (loop with foo = (gensym)
	for initarg in default-initargs
	for value = (getf initargs initarg foo)
	unless (eq value foo) do (remf initargs initarg)
	and collect initarg and collect value))

(defmethod extract-documentation ((driver (eql 'html)) dest-dir
				  (system asdf:system)
				  &rest initargs &key &allow-other-keys)
  (unless (getf initargs :path-prefix)
    (setf (getf initargs :path-prefix)
	  (namestring (asdf:component-relative-pathname system))))
  (let ((files (get-asdf-system-files system)))
    (apply #'extract-documentation driver dest-dir files initargs)))

(defmethod extract-documentation ((driver (eql 'html)) dest-dir filenames
				  &rest initargs &key &allow-other-keys)
  (declare (ignorable driver))
  (let ((pp (getf initargs :path-prefix)))
    (remf initargs :path-prefix)
    (let ((args (get-initargs initargs)))
      (make-html-doc
         (apply #'make-instance 'html :string-parser-initargs initargs args)
	 filenames
	 :path-prefix (or pp (directory-namestring (or *load-truename* ".")))
	 :dest-dir (or dest-dir ".")))))

;;;

(deftype card8 () `(unsigned-byte 8))

(defmacro with-gensym (symbols &body body)
  `(let ,(loop for s in symbols collect `(,s (gensym)))
     ,@body))

(defun copy-css (html-driver &key dest-dir)
  (with-slots (css-pathname copy-css-into-output-dir) html-driver
    (if copy-css-into-output-dir
	(let* ((name (file-namestring (truename css-pathname)))
	       (new-css-path (merge-pathnames name dest-dir)))
	  (prog1 (namestring new-css-path)
	    (unless (probe-file new-css-path)
	      (with-open-file
		  (os new-css-path :direction :output :element-type 'card8)
		(write-sequence 
		 (with-open-file (is css-pathname :element-type 'card8)
		   (loop with offset of-type fixnum = 0
			 with size of-type fixnum = (file-length is)
			 with vec = (make-array size :element-type 'card8)
			 while (< offset size)
			 do (setf offset (read-sequence vec is :start offset))
			 finally (return vec)))
		 os)))))
	css-pathname)))

(defun default-filter (desc)
  "Returns true if the given symbol-descriptor is not an external symbol
   of its package or if is a defmethod descriptor for which a defgeneric
   descriptor exists."
  (when (typep desc 'defpackage-descriptor)
    (return-from default-filter nil))
  (multiple-value-bind (sym status)
      (let* ((name (standard-io-name (name desc)))
	     (space (position #\Space name :test #'char=)))
	(when space
	  (setf name (subseq name (1+ space) (1- (length name)))))
	(find-symbol name (find-package-caseless (dpackage desc))))
    (declare (ignore sym))
    (if (eq status :external) (filter-defmethod-with-defgeneric desc) T)))

(defun filter-defmethod-with-defgeneric (desc)
  "Returns true if the given symbol-descriptor is a defmethod descriptor
   and thus if the specified defmethod has a defgeneric."
  (when (typep desc 'defmethod-descriptor)
    (lookup-meta-descriptor (name desc) 'defgeneric-descriptor)))

(defun make-default-css-namestring ()
  (namestring
   (merge-pathnames
    "cludg.css"
    (make-pathname :defaults (directory-namestring *cludg-directory*)))))

(defun symbol-descriptor-weight (sym-desc-instance)
  "Returns the weight, a valuation, used to sort the final documentation."
  (typecase sym-desc-instance
    ;(in-package-form -1)
    (defpackage-descriptor 0)
    (defclass-descriptor 1)
    (define-condition-descriptor 2)
    (defstruct-descriptor 3)
    (deftype-descriptor 4)
    (defconstant-descriptor 5)
    (param-descriptor 6)
    (defgeneric-descriptor 7)
    (defmethod-descriptor 8)
    (defun-descriptor 9)
    (defsetf-descriptor 10)
    (defmacro-descriptor 11)
    (t 12)))

(defun desc-sort (descs predicate &key key)
  "Sort such as stable-sort but non destructively the given list of descs."
  (if (null predicate) descs
      (stable-sort (copy-list descs) predicate :key key)))

(defun default-sort-predicate (desc1 desc2)
  (< (symbol-descriptor-weight desc1) (symbol-descriptor-weight desc2)))

(defun next (list index dest-dir &optional prefix)
  "Returns the relative pathanme to the next (aka 1+ index) element of list."
  (declare (type list list))
  (declare (type fixnum index))
  (when (< index (1- (length list)))
    (flet ((mkout (file) (make-output-pathname file dest-dir :html prefix)))
      (make-pathname-relative 
         :from (mkout (car (elt list index)))
	 :to (mkout (car (elt list (1+ index))))))))

(defun prev (list index dest-dir &optional prefix)
  "Returns the relative pathanme to previous (aka 1- index) element of list."
  (declare (type list list))
  (declare (type fixnum index))
  (unless (zerop index)
    (flet ((mkout (file) (make-output-pathname file dest-dir :html prefix)))
      (make-pathname-relative 
         :from (mkout (car (elt list index)))
	 :to (mkout (car (elt list (1- index))))))))

(defun alphabetical-order (desc1 desc2)
  "Returns true if the name of the first descriptor is lexicographicaly
   inferior to the name of the second descriptor."
  (flet ((get-name (desc)
	   (let ((name (name desc)))
	     (if (starts-with name "(") (subseq name 1) name))))
    (let ((name1 (get-name desc1))
          (name2 (get-name desc2)))
      (if (alpha-char-p (char name1 0))
          (if (alpha-char-p (char name2 0)) (string-lessp name1 name2) T)
          (unless (alpha-char-p (char name2 0))
            (string-lessp name1 name2))))))

;;;
;;; Macros for HTML writing.
;;;

(defvar *html-output-stream* nil "An output stream to write HTML forms.")

(defmacro with-html-output ((output-stream) &body body)
  "Binds *html-output-stream* to the output-stream in order to use the 
   html-write, with-tag, htmlify-doc and with-html-description macros
   without their output-stream optional argument."
  `(let ((*html-output-stream* ,output-stream))
     ,@body))

(defmacro html-write (control-string &rest args)
  "Produces formatted output by outputting the characters of control-string
   and observing that a tilde introduces a directive. Most directives use
   one or more elements of args to create their output."
  `(format *html-output-stream* ,control-string ,@args))

(defmacro with-tag
    ((tagname (&rest attributes) &optional (stream '*html-output-stream*))
     &body body)
  "Writes the desired tag and its attributes to the given stream or the 
   one binded on *html-output-stream*."
  (with-gensym (os)
    `(let ((,os ,stream))
       (format ,os "<~a~{~^ ~a=\"~a\"~}~:[~;/~]>~%"
	      ,tagname (list ,@attributes) ,(zerop (length body)))
       (prog1 (progn ,@body)
         ,@(unless (zerop (length body))
             `((format ,os "</~a>~%" ,tagname)))))))

(defmacro with-html-page
    ((os &key csshref content-type head-title nav-name index prev next)
     &body body)
  "Binds *html-output-stream* to os as if binded by with-html-output and
   executes the body in inside the BODY tag. A simple implementation can be:
    ;;; (with-tag (:html ())
    ;;;   (with-tag (:head ()) (do-header))
    ;;;   (with-tag (:body ()) ,@body))"
  `(with-html-output (,os)
     (html-write "~a~2%" +HTML-DOCTYPE+)
     (with-tag (:html ())
       (with-tag (:head ())
	 (with-tag (:link (:rel "Stylesheet" :type "text/css" :href ,csshref)))
	 (with-tag (:meta (:http-equiv "Content-Type" :content ,content-type)))
	 (with-tag (:title ()) (html-write ,head-title)))
       (with-tag (:body ())
	 (make-navbar :name ,nav-name :index ,index :prev ,prev :next ,next)
	 ,@body
	 (make-footer)))))

(defun htmlify-doc (desc &key (doc-string (doc desc)) (purge-p t) html-driver)
  "Presents the given doc-string according to our html template.
   - doc-string (string): the documentation string to write.
   - purge-p (boolean): If T the documentation string will be purged of
     potentially dangerous character for HTML.
   - html-driver (driver): the html-driver to use."
  (when (and doc-string (string/= doc-string ""))
    (with-tag (:div (:class "doc-body"))
      (funcall (doc-formater html-driver)
	       desc
	       html-driver
	       (mapcar #'(lambda (s) (if purge-p (purge-string-for-html s) s))
		       (grok-new-lines doc-string))))))

(defmacro with-html-description
    ((&key (divclass "defun") name arg-list type anchor) &body body)
  "Presents lisp forms according to our html documentation template."
  (with-gensym (hanchor args)
    `(with-tag (:div ,(when divclass `(:class ,divclass)))
       (with-tag (:div ,(when divclass `(:class "defunsignatures")))
	 (let ((,hanchor ,anchor))
	   (when ,hanchor (with-tag (:a (:id ,hanchor)) "")))
	 (with-tag (:table (:cellpadding 0 :cellspacing 0 :width "100%"))
	   (with-tag (:colgroup (:span 3))
	     (with-tag (:col (:width "0*")))
	     (with-tag (:col (:width "1*")))
	     (with-tag (:col (:width "0*"))))
	   (with-tag (:tbody ())
	     (with-tag (:tr ())
	       (with-tag (:td (:class "symbol-name"))
		 (html-write "~a&nbsp;&nbsp;" ,name))
	       (with-tag (:td (:class "lambda-list"))
		 (let ((,args ,arg-list))
		   (when ,args (html-write "~a" ,args))))
	       (with-tag (:td (:class "symbol-type"))
		 (html-write "&nbsp;[~@(~a~)]" ,type))))))
       ,@body)))

(defun make-footer ()
  "Appends CLDOC link and generation date."
  (with-tag (:div (:class "cludg-footer"))
    (html-write "Generated by&nbsp;")
    (with-tag (:a (:href "mailto:ihatchondo@common-lisp.net" :lang "en"))
      (html-write "CLDOC"))
    (html-write "- ~a" (get-iso-date-time))))

(defun make-navbar (&key index next prev name)
  "Adds the HTML code for the navigation bar."
  (declare (type (or null pathname) prev next index))
  (when (or (not name) (not index)) (return-from make-navbar nil))
  (with-tag (:div (:id "navbar"))
    (with-tag (:table
	       (:cellspacing 0 :cellpadding 0 :border 0 :style "width: 100%;"))
      (with-tag (:colgroup (:span 3))
	(with-tag (:col (:width "0*")))
	(with-tag (:col (:width "0*")))
	(with-tag (:col (:width "1*"))))
      (with-tag (:tr ())
	(with-tag (:td (:align "left" :valign "baseline"))
	  (when prev
	    (with-tag (:a (:href (namestring prev)))
	      (html-write "Prev:&nbsp;~a" (pathname-name prev))))
	  (with-tag (:br ()))
	  (when next
	    (with-tag (:a (:href (namestring next)))
	      (html-write "Next:&nbsp;~a"  (pathname-name next)))))
	(with-tag (:td ()) (html-write "&nbsp;&nbsp;&nbsp;&nbsp;"))
	(with-tag (:td (:align "left" :valign "baseline"))
	  (with-tag (:span (:class "upchain"))
	    (with-tag (:b ()) (html-write "~a" name))
	    (with-tag (:br ()))
	    (with-tag (:a (:href (namestring index)))
	      (html-write "~:(~a~)" (pathname-name index))))
	  (html-write "&nbsp;&nbsp;&nbsp;&nbsp;"))))))

(defun toc-path-from (from dest-dir)
  (declare (type (or string pathname) from))
  (let ((toc (merge-pathnames "index.html" dest-dir)))
    (make-pathname-relative :from from :to toc)))

;;;
;;; HTML index creation facilities 
;;;

(defgeneric href-title (symbol-descriptor)
  (:documentation "Returns a string for the title attribute of an href.")
  (:method ((symdesc symbol-descriptor))
    (purge-string-for-html (fully-qualified-name symdesc)))
  (:method ((symdesc lambda-descriptor))
    (let ((name (purge-string-for-html (fully-qualified-name symdesc)))
	  (ll (format nil "(~{~s~^ ~})" (lambda-list symdesc))))
      (concatenate 'string name " " (purge-string-for-html ll)))))

(defmacro with-index-header
    ((index hdriver dest-dir title &key (head-title title)) &body body)
  (with-gensym (href ttitle iindex ddir)
    `(with-slots (filter css-pathname charset) ,hdriver
       (let* ((*print-case* :downcase)
	      (,iindex ,index)
	      (,ddir ,dest-dir)
	      (,ttitle ,title)
	      (,href (make-pathname-relative
		         :from (truename ,ddir) :to (truename css-pathname))))
	 (with-open-file (os ,iindex :direction :output :if-exists :supersede)
	   (with-html-page
	       (os :csshref (namestring ,href)
		   :content-type (format nil "text/html; charset=~a" charset)
		   :head-title ,head-title
		   :nav-name ,ttitle
		   :index (toc-path-from (pathname os) ,ddir))
	     (with-tag (:div (:class "cludg-index-body"))
	       (when ,ttitle (with-tag (:h2 ()) (html-write "~a~%" ,ttitle)))
	       (with-tag (:div ()) ,@body))))
	 (enough-namestring (truename ,iindex) (truename ,ddir))))))

(defun make-abc-index-entry (filename &key char-code non-alphabetic)
  (let* ((name (file-namestring filename))
	 (href (format nil "~a#_~a" name (or char-code non-alphabetic))))
    (with-tag (:a (:href href))
      (html-write
       (if char-code (format nil "~c" (code-char char-code)) "non-alphabetic"))
      (html-write "&nbsp;&nbsp;"))))

(defun make-index-entry (meta-descriptors &key char-code non-alphabetic filter)
  (flet ((char-code-string () (format nil "~:@(~c~)..." (code-char char-code)))
	 (first-char-p (name char)
	   (let ((c (char name 0)))
	     (char-equal char (if (char= c #\() (char name 1) c)))))
    (with-tag (:a (:id (format nil "_~a" (or char-code non-alphabetic)))) "")
    (with-tag (:div (:class "abc-entry"))
      (with-tag (:h3 ())
	(html-write (if char-code (char-code-string) "non-alphabetic")))
      (loop with entry = (and char-code (code-char char-code))
            for mdesc in meta-descriptors
	    for desc = (meta-descriptor-desc mdesc)
            if (or (and entry (first-char-p (name desc) entry)) non-alphabetic)
            do (unless (and filter (funcall filter desc))
                 (with-tag (:div (:class "index-entry"))
                   (with-tag (:a (:href (meta-descriptor-href mdesc)
				  :title (href-title desc)))
                     (html-write "~a," (purge-string-for-html (name desc))))
                   (with-tag (:em ())
                     (html-write "~a" (html-printable-type desc)))))
               (pop meta-descriptors)
            else do (loop-finish)
            finally (return meta-descriptors)))))

(defun write-index (filename dest-dir title html-driver meta-descriptors)
  (let ((na-anchor (format nil "~a" (gensym)))
	(index-file (namestring (merge-pathnames filename dest-dir))))
    ;; Remove defpackage-descriptor of the meta-descriptors if any.
    (let ((desc (meta-descriptor-desc (car meta-descriptors))))
      (when (typep desc 'defpackage-descriptor)
        (setf meta-descriptors (cdr meta-descriptors))))
    (with-index-header (index-file html-driver dest-dir title)
      ;; generate a b c d ... links
      (loop for i from (char-code #\a) to (char-code #\z)
	    do (make-abc-index-entry index-file :char-code i))
      ;; add non-alphabetic
      (make-abc-index-entry index-file :non-alphabetic na-anchor)
      ;; the index itself
      (loop for i from (char-code #\a) to (char-code #\z)
            do (setf meta-descriptors
                     (make-index-entry
                         meta-descriptors :char-code i :filter filter)))
      ;; add non-alphabetic
      (make-index-entry
         meta-descriptors
	 :non-alphabetic na-anchor
	 :filter filter))))

#|
(defun make-toc (html-driver dest-dir package-index-files)
  "Writes the table of contents. Returns the file truename of the
   table of contents."
  (let ((previous-directory '())
        (toc (namestring (merge-pathnames "index.html" dest-dir))))
    (with-index-header (toc html-driver dest-dir nil :head-title "TOC")
      (with-slots (toc-title) html-driver
	(if toc-title
	    (with-tag (:h1 (:class "center")) (html-write toc-title))
	    (with-tag (:h2 ()) (html-write "Table of Contents"))))
      (with-tag (:br ()))
      (with-tag (:ul ())
	(loop with i = 0
	      for (key pkg-index-href files) in package-index-files
	      for part-num from 1 do
	       (with-tag (:li (:style "list-style-type: none"))
		 (with-tag (:a (:href pkg-index-href))
		   (html-write "Part ~@R:&nbsp;~a~%" part-num key))
		 (with-tag (:ul ())
		   (loop for file in (sort files #'string-lessp :key #'(lambda(file)
                                                                         (second (pathname-directory file))))
			 for name = (pathname-name file)
                         for directory = (pathname-directory file)
                         do
                         (unless (equal (second directory) (second previous-directory))
                           (html-write "<br>")
                           (with-tag (:a (:href toc))
                                (html-write "Module: ~a~%" (second (pathname-directory file)))))
			  (with-tag (:li (:style "list-style-type: none"))
			    (with-tag (:a (:href file))
			      (if (string/= name "the-index")
				  (html-write "~d&nbsp;~a~%" (incf i) name)
				  (html-write "&nbsp;Index"))))
                          (setf previous-directory directory)))))))))
|#



(defun make-toc (html-driver dest-dir package-index-files)
  "Writes the table of contents. Returns the file truename of the
   table of contents."
  (let ((previous-directory '())
        (toc (namestring (merge-pathnames "index.html" dest-dir))))
    (with-index-header (toc html-driver dest-dir nil :head-title "TOC")
      (with-slots (toc-title) html-driver
	(if toc-title
          (with-tag (:h1 (:class "center")) (html-write toc-title))
          (with-tag (:h2 ()) (html-write "Table of Contents"))))
      (with-tag (:br ()))
      (with-tag (:ul ())
	(loop with i = 0
	      for (key pkg-index-href files) in (reorder-package-index-files package-index-files)
	      for part-num from 1 do
              (with-tag (:li (:style "list-style-type: none"))
                (with-tag (:a (:href pkg-index-href))
                  (html-write "Part ~@R:&nbsp;~a~%" part-num key))
                (with-tag (:ul ())
                  
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ;; Handle top-level files (not inside folder) ;;
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  (let* ((top-level-files (find 0 (get-ordered-filenames files) :key #'(lambda (list)
                                                                                        (length (pathname-directory (car list))))))
                        (package (find "package.html" top-level-files :test #'equalp))
                        (other-top-level-files (remove "package.html" top-level-files :test #'equalp)))

                    ;; Write package file on top
                    (when (and (or package other-top-level-files)
                               (not (and (eq 1 (length other-top-level-files))
                                         (string= "the-index" (pathname-name (first other-top-level-files))))))
                      (with-tag (:p ())
                        (with-tag (:i ())
                          (html-write "General"))))

                    (with-tag (:ul ())
                    (when package
                      (with-tag (:li (:style "list-style-type: none"))
			    (with-tag (:a (:href package))
				  (html-write "~d&nbsp;~a~%" (incf i) (pathname-name package)))))

                    ;; Write other top-level-files
                  
                    (dolist (file (sort other-top-level-files #'string-lessp))
                      (with-tag (:li (:style "list-style-type: none"))
			    (with-tag (:a (:href file))
                              (if (string/= (pathname-name file) "the-index")
				  (html-write "~d&nbsp;~a~%" (incf i) (pathname-name file))
				  (html-write "&nbsp;Index")))))))

                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ;; Handle files of modules subfolder)         ;;
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                  (let* ((modules (remove 0 (get-ordered-filenames files) :key #'(lambda (list)
                                                                                        (length (pathname-directory (car list)))))))
                    
                      (dolist (module modules)

                        ;; Write module
                        ;(with-tag (:br ()))
                        (with-tag ;(:a (:href toc))
                            (:p ())
                          (with-tag (:i ())
                          (html-write "Module: ~a~%" (second (pathname-directory (first module))))))
                      
                        ;; Write files of module
                        (with-tag (:ul ())
                          (dolist (file (sort module #'string-lessp))
                            (with-tag (:li (:style "list-style-type: none"))
                              (with-tag (:a (:href file))
                                (html-write "~d&nbsp;~a~%" (incf i) (pathname-name file)))))))))))))))

(defun reorder-package-index-files (package-index-files)
  (let* ((copy-pif (copy-list package-index-files))
         (index-pi (find "index" package-index-files :test 'equalp :key 'first))
         (main-pi (first (sort copy-pif '> :key #'(lambda(x)
                                                         (length (third x))))))
         (rest (remove main-pi (remove index-pi copy-pif))))
    (append (list main-pi) (sort rest #'string-lessp :key 'first) (list index-pi))))
    
;; (get-ordered-filenames (get-asdf-system-files (asdf/system:find-system :fcg)))
(defun get-ordered-filenames (files)
  (let* ((sorted-files (sort files #'string-greaterp :key #'(lambda (file)
                                                           (second (pathname-directory file)))))
         (prev-file nil)
         (listed-files nil))
    (dolist (file sorted-files)
      (if (or (not prev-file)
           (not (equal (second (pathname-directory file))
                      (second (pathname-directory prev-file)))))
        (push (list file) listed-files)
        (push file (car listed-files)))
      (setf prev-file file))
    listed-files))

(defun get-defpackage-descriptor (meta-descriptors package-table)
  "Finds all the defpackage-descriptor and insert them in the package-table."
  (loop for val in meta-descriptors
	for desc = (meta-descriptor-desc val)
	when (typep desc 'defpackage-descriptor)
	do (setf (gethash (name desc) package-table) (list val))))

(defun get-descriptors-by-package (html-driver mdescriptors package-table)
  "Finds all descriptors of each package and add them to their package entry
   of the package-table. Descriptors will be filtered according to the 
   filter of the html-driver instance."
  (loop with filter = (filter html-driver)
	for meta-desc in mdescriptors
	for desc = (meta-descriptor-desc meta-desc)
	for add-p = (not (or (not filter) (funcall filter desc)))
	for pname = (dpackage desc)
        ;; Search the meta-desc package-name entry
	if (and add-p (gethash pname package-table))
	do (push meta-desc (gethash pname package-table))
        ;; Else search the meta-desc (string-upcase package-name) entry
	else if (and add-p (gethash (string-upcase pname) package-table))
	do (push meta-desc (gethash (string-upcase pname) package-table))
        ;; Else meta-desc package entry is not in the table. Lets create the
        ;; entry and add the meta-desc if desc is not a defpackage-descriptor.
        else if (and add-p (not (typep desc 'defpackage-descriptor)))
        do (push meta-desc (gethash pname package-table))))

(defun make-indexes (dest-dir html-driver)
  "Creates package index files, global index and table of contents."
  (declare (type string dest-dir))
  (let ((meta-descriptors '())
	(the-index "the-index.html")
	the-href)
    ;; Sort meta-descriptors by alphabetic order.
    (setf meta-descriptors (desc-sort (cache-meta-descriptors)
				      #'alphabetical-order
				      :key #'meta-descriptor-desc))
    ;; Write general html index file.
    (setf the-href (write-index
		    the-index dest-dir "Index" html-driver meta-descriptors))
    (let ((package-table (make-hash-table :test #'equal))
	  (package-index-files '()))
      ;; Find all defpackage descriptor.
      (get-defpackage-descriptor meta-descriptors package-table)
      ;; Find all html descriptors of each packages.
      (get-descriptors-by-package html-driver meta-descriptors package-table)
      ;; Write a descriptors index file for each package.
      (loop for key being each hash-key in package-table using (hash-value mds)
	    for file = (format nil "~a-index.html" key)
	    for href = (write-index file dest-dir key html-driver (reverse mds))
	    for files = (mapcar #'meta-descriptor-file
			   (stable-sort mds #'< :key #'meta-descriptor-index))
	    do (push
		(list key href (delete-duplicates files :test #'string-equal))
		package-index-files))
      ;; Sort package-index-files in reverse file index order.
      (flet ((key (l)
	       (meta-descriptor-index (car (gethash (car l) package-table)))))
	(setf package-index-files
	      (stable-sort package-index-files #'> :key #'key)))
      ;; Push the-index in package-index-files.
      (push (list "Index" the-href (list the-index)) package-index-files)
      ;; Write table of contents in "index.html in the initial order".
      (make-toc html-driver dest-dir (reverse package-index-files)))))

;;;
;;; Summary tables facilities.
;;;

(defun find-descs (desc-type descs)
  "Returns the list of symbol descriptors that are of type desc-type."
  (loop for desc in descs when (typep desc desc-type) collect desc))

(defun make-summary (summary-title descs filter &key key)
  "Creates a summary table with the given descriptors. The created table will
   have two columns. The first one will present hyper link with the descriptor
   name and the second one the summary as returned by the :key function.
   Descriptor list will be filtered according to the filter function when
   provided. The :key argument is a designator for a function of one argument
   of type symbol-descriptor and returns a string."
  (when filter (setf descs (delete-if filter descs)))
  (when descs
    (with-tag (:table (:style "width: 100%;" :class "summary-table"))
      (with-tag (:tr (:class "table-heading-color"))
	(with-tag (:th (:class "summary" :colspan "2"))
	  (html-write summary-title )))
      (loop for desc in (desc-sort descs #'alphabetical-order)
	    for mdesc = (lookup-meta-descriptor desc)
	    when mdesc
	    do (with-tag (:tr (:class "table-row-color"))
		 (with-tag (:td (:class "summary-name"))
		   (with-tag (:a (:href (meta-descriptor-href mdesc :local)))
		     (html-write (purge-string-for-html (name desc)))))
		 (with-tag (:td (:class "summary"))
		   (let ((string (funcall key desc)))
		     (when string (html-write "~a" string)))))))))

(defun make-constant-summary (descs filter)
  "Creates a summary table for defconstant descriptors if any."
  (let ((defconstants (find-descs 'defconstant-descriptor descs)))
    (flet ((key (desc) (purge-lambda-list-for-html (value desc))))
      (make-summary "Constant summary" defconstants filter :key #'key))))

(defun make-class-summary (descs filter)
  "Creates summary tables for defclass, define-condition and defstruct
   descriptors if any."
  (flet ((key (desc) (format-inheritence desc)))
    (mapc #'(lambda (title descs) (make-summary title descs filter :key #'key))
	  '("Class summary" "Condition summary" "Structure summary")
	  (list (find-descs 'defclass-descriptor descs)
		(find-descs 'define-condition-descriptor descs)
		(find-descs 'defstruct-descriptor descs)))))

(defun make-function-summary (descs filter)
  "Creates summary tables for defun, defsetf, defmethod, defgeneric and 
   defmacro descriptors if any."
  ;; Because neither the defstruct nor the defclass nor the define-condition
  ;; accessors are present in the list of descriptors they will be retrieved
  ;; manually and added to defun or defmethod list before call make-summary.
  (flet ((mapconc (fun list) (apply #'concatenate 'list (mapcar fun list))))
    (let* ((defmethods '(or defgeneric-descriptor defmethod-descriptor))
	   (defuns '(or defun-descriptor defsetf-descriptor))
	   (class-types '(or defclass-descriptor define-condition-descriptor))
	   (defstructs (find-descs 'defstruct-descriptor descs))
	   (classes (mapconc #'slot-accessors (find-descs class-types descs)))
	   (structs (mapconc #'slot-accessors defstructs)))
      ;; Add desfstruct copier, predicate, printer and constructors if any.
      (nconc structs (mapconc #'constructors defstructs))
      (loop for struct in defstructs
	    do (with-slots (copier predicate printer) struct
		 (and copier (push copier structs))
		 (and predicate (push predicate structs))
		 (and printer (push printer structs))))
      ;; Last but not least: make summary !
      (flet ((key (desc) (purge-lambda-list-for-html (lambda-list desc))))
	(mapc #'(lambda (title descs)
		  (make-summary title descs filter :key #'key))
	      '("Method summary" "Function summary" "Macro summary")
	      (list (concatenate 'list (find-descs defmethods descs) classes)
		    (concatenate 'list (find-descs defuns descs) structs)
		    (find-descs 'defmacro-descriptor descs)))))))
;;;
;;; Public
;;;

(defvar *class-inheritence* "inherits from"
  "Defclass inheritence indication control string for automatic documentation.
   This control string has no parameter.")

(defvar *condition-inheritence* "inherits from"
  "Define-condition inheritence indication control string for automatic
   documentation. This control string has no parameter.")

(defvar *struct-inheritence* "includes"
  "Defstruct include indication control string for automatic documentation.
   This control string has no parameter.")

(defun resolve-link (symdesc strings)
  (let ((schemes '("http://" "ftp://"))
	(file (meta-descriptor-file (lookup-meta-descriptor symdesc))))
    (if (some #'(lambda (scheme) (starts-with (first strings) scheme)) schemes)
	(values T (format nil "~{~a~^ ~}" strings))
	(multiple-value-bind (name package) (split-name (second strings))
	  (let ((href (lookup-meta-descriptor-href
		         (or name "") (first strings) package file)))
	    (values (if href T NIL) href name))))))

(defun format-doc (symdesc html-driver strings)
  "Default documentation string formater. The Do What I Mean capabilities
   are delegated to the create-doctree-from-string method of the doctree
   protocol in coordination with with-tree-loop iterator to produced the
   final output."
  (with-slots ((spi string-parser-initargs)) html-driver
    (let* ((link-delims (getf spi :link-delimiters +default-link-delimiters+))
	   (left-link-delim (first link-delims))
	   (right-link-delim (second link-delims)))
      (labels ((map-over-tree (tree)
		 (with-tree-loop (element tree)
		   (if (stringp element)
		       (html-write "~a " element)
		       (case (tree-tag element)
			 (:keyword
			  (with-tag (:span (:class "keyword"))
			    (map-over-tree element)))
			 (:hyper-link
			  (let ((link '()))
			    (with-tree-loop (e element) (push e link))
			    (multiple-value-bind (found-p href name)
				(resolve-link symdesc (reverse link))
			      (if (and found-p href)
				  (with-tag (:a (:href href))
				    (html-write (or name href)))
				  ;; No link can be created from the given
				  ;; information. Maybe the author was not
				  ;; thinking to an hyper link, for this
				  ;; reason the text will be outputed as 
				  ;; it was initially found.
				  (html-write "~a~{~a~^ ~}~a"
					      left-link-delim
					      (reverse link)
					      right-link-delim)))))
			 (t (with-tag ((tree-tag element) ())
			      (map-over-tree element))))))))
	(map-over-tree
	   (apply #'create-doctree-from-string 'doctree strings spi))))))

(defun make-html-doc (hdriver filenames &key (dest-dir ".") path-prefix)
  "Reads all files specified in filenames and extract their documentation
   using HTML as output. The extracted documentation will be written in a
   newly created file with the same name as the processed one. If dest-dir
   is specified then it will be used to construct the output pathname,
   otherwise the output pathname will be constructed using the one of the
   currently processed input file pruned from path-prefix."
  (declare (type list filenames))
  (declare (type string dest-dir))
  (unless (char= (char dest-dir (1- (length dest-dir))) #\/)
    (setf dest-dir (concatenate 'string dest-dir "/")))
  (ensure-directories-exist dest-dir)
  (setf filenames (add-prefix-if-necessary path-prefix filenames))
  (multiple-value-bind (files-and-descriptors *unhandled-forms*)
      (apply #'initialise-cache dest-dir :html path-prefix filenames)
    (with-slots (filter css-pathname charset sort-predicate) hdriver
      (setf css-pathname (copy-css hdriver :dest-dir dest-dir))
      (make-indexes dest-dir hdriver)
      (loop with css-truename = (truename css-pathname)
	    with *current-package* = "common-lisp-user"
	    with *print-case* = :downcase
	    with html-content = (format nil "text/html; charset=~a" charset)
	    for (ifile descriptors) in files-and-descriptors and index from 0
	    for title = (pathname-name (truename ifile))
	    for of = (make-output-pathname ifile dest-dir :html path-prefix)
	    for css = (make-pathname-relative :from of :to css-truename)
	    for next = (next files-and-descriptors index dest-dir path-prefix)
	    for prev = (prev files-and-descriptors index dest-dir path-prefix)
	    do (with-open-file (os of :direction :output :if-exists :supersede)
		 (with-html-page
		     (os :csshref (namestring css) :content-type html-content
			 :head-title title :nav-name (pathname-name os)
			 :index (toc-path-from of (truename dest-dir))
			 :prev prev :next next)
		   ;; Append all documentation.
		   (with-tag (:div (:class "cludg-doc-body"))
		     (with-tag (:h2 ()) (html-write "~a~%" title))
		     (let ((descs (desc-sort descriptors sort-predicate)))
		       (make-constant-summary descs filter)
		       (make-class-summary descs filter)
		       (make-function-summary descs filter)
		       (loop for desc in descs
			     do (with-slots (name type) desc
				  (unless (and filter (funcall filter desc))
				    (dformat desc hdriver os))))))))))
    (remove-duplicates *unhandled-forms*)))

;;;
;;; Purger.
;;;

(define-purgers
  :string-purger 
    (purge-string-for-html
     ((#\& "&amp;")
      (#\" "&quot;")
      (#\< "&lt;")
      (#\> "&gt;"))
     (:documentation "Tries to purge a string from characters that
       are potentially dangerous for HTML."))
  :lambda-list-purger
    (purge-lambda-list-for-html
     (("&key" "<em>&amp;key</em>")
      ("&optional" "<em>&amp;optional</em>")
      ("&rest" "<em>&amp;rest</em>")
      ("&allow-other-keys" "<em>&amp;allow-other-keys</em>")
      ("&body" "<em>&amp;body</em>")
      ("&aux" "<em>&amp;aux</em>")
      ("&environment" "<em>&amp;environment</em>")
      ("&whole" "<em>&amp;whole</em>"))
     (:documentation "Tries to purge a lambda-list from characters that are
       potentially dangerous for HTML.")))

;;;
;;; Misc.
;;;

(defgeneric html-printable-type (symbol-descriptor)
  (:documentation "Returns an HTML string that will be used to instead of
   the desc-type. If such a overload is not found the value returned by
   desc-type will be used.")
  (:method ((symbol-descriptor symbol-descriptor))
    (desc-type symbol-descriptor))
  (:method ((symbol-descriptor defpackage-descriptor)) "Package")
  (:method ((symbol-descriptor defclass-descriptor)) "Class")
  (:method ((symbol-descriptor define-condition-descriptor)) "Condition")
  (:method ((symbol-descriptor defstruct-descriptor)) "Structure")
  (:method ((symbol-descriptor deftype-descriptor)) "Type")
  (:method ((symbol-descriptor defconstant-descriptor)) "Constant")
  (:method ((symbol-descriptor defparameter-descriptor)) "Variable")
  (:method ((symbol-descriptor defvar-descriptor)) "Variable")
  (:method ((symbol-descriptor defun-descriptor)) "Function")
  (:method ((symbol-descriptor defmacro-descriptor)) "Macro")
  (:method ((symbol-descriptor defsetf-descriptor)) "Setf&nbsp;Function")
  (:method ((symbol-descriptor defgeneric-descriptor)) "Generic&nbsp;Function")
  (:method ((symbol-descriptor slot-descriptor)) "Slot")
  (:method ((symbol-descriptor defmethod-descriptor))
    (case (car (method-qualifiers symbol-descriptor))
      (:before "Before&nbsp;Method")
      (:around "Around&nbsp;Method")
      (:after "After&nbsp;Method")
      (t "Method"))))

(defgeneric find-inheritence-word (symbol-descriptor)
  (:documentation "Returns an HTML string that will be used to instead of
   the desc-type. If such a overload is not found the value returned by
   desc-type will be used.")
  (:method ((symbol-descriptor symbol-descriptor)) "")
  (:method ((symbol-descriptor defclass-descriptor)) *class-inheritence*)
  (:method ((symbol-descriptor defstruct-descriptor)) *struct-inheritence*)
  (:method ((symbol-descriptor define-condition-descriptor))
    *condition-inheritence*))

(defun format-inheritence (structured-object-descriptor)
  "Resolves the inheritence/inclusion of structured-object-descriptor in order
   to return an HTML string that describes the inheritence with hyper links."
  (flet ((em (string) (format nil "<em class=\"cl\">~a</em>" string))
	 (href (href string) (format nil "<a href=\"~a\">~a</a>" href string)))
    (let ((inheritence (inheritence structured-object-descriptor))
	  (meta-desc (lookup-meta-descriptor structured-object-descriptor))
	  (super-desc-type (type-of structured-object-descriptor))
	  (package-name (dpackage structured-object-descriptor)))
      (when inheritence
	(format nil "~a ~{~a~#[~; and ~:;, ~]~}"
		(find-inheritence-word structured-object-descriptor)
		(mapcar #'(lambda (super)
			    (multiple-value-bind (pname ssuper)
				(resolve-symbol-package-name super package-name)
			      (let ((s (em (purge-string-for-html ssuper)))
				    (href (lookup-meta-descriptor-href
					   ssuper super-desc-type pname
					   (meta-descriptor-file meta-desc))))
				(if href (href href s) s))))
			inheritence))))))

;;;
;;; dformat-documentation protocol.
;;;

(defmethod dformat-documentation (desc (driver html) stream)
  (declare (ignorable stream))
  (htmlify-doc desc :html-driver driver))

(defmethod dformat-documentation
    ((desc structured-object-descriptor) (driver html) os)
  (htmlify-doc desc :html-driver driver)
  (when (slots desc)
    (with-tag (:div (:class "defclass-initargs"))
      (loop for slot in (slots desc)
	    for iargs = (format nil "~{~s~^ ~}" (initargs slot))
	    when (initargs slot)
	    do (with-html-description
		   (:divclass nil :name iargs :type "Initarg"))))
    (with-tag (:div (:class "defclass-slots-doc"))
      (loop for slot in (slots desc) do (dformat slot driver os)))
    (with-tag (:div (:class "defclass-generics"))
      (loop for desc in (slot-accessors desc)
	    unless (and (filter driver) (funcall (filter driver) desc))
	    do (dformat desc driver os)))))

;;;
;;; dformat protocol.
;;;

(defmethod dformat ((desc in-package-form) (driver html) os)
  (declare (ignorable driver os desc))
  (setf *current-package* (dest-package desc)))

(defmethod dformat ((desc defpackage-descriptor) (driver html) os)
  (with-html-description
      (:name (purge-string-for-html (name desc))
       :type (html-printable-type desc)
       :anchor (lookup-meta-descriptor-anchor desc)
       :divclass "defpackage")
    (dformat-documentation desc driver os)))

(defmethod dformat ((desc param-descriptor) (driver html) os)
  (with-html-description
      (:name (purge-string-for-html (name desc))
       :type (html-printable-type desc)
       :anchor (lookup-meta-descriptor-anchor desc)
       :divclass "defparam")
    (dformat-documentation desc driver os)))

(defmethod dformat ((desc deftype-descriptor) (driver html) os)
  (with-html-description
      (:name (purge-string-for-html (name desc))
       :type (html-printable-type desc)
       :arg-list (purge-lambda-list-for-html (value desc))
       :anchor (lookup-meta-descriptor-anchor desc)
       :divclass "deftype")
    (dformat-documentation desc driver os)))

(defmethod dformat ((desc defconstant-descriptor) (driver html) os)
  (with-html-description
      (:name (purge-string-for-html (name desc))
       :type (html-printable-type desc)
       :arg-list (purge-lambda-list-for-html (value desc))
       :anchor (lookup-meta-descriptor-anchor desc)
       :divclass "defconstant")
    (dformat-documentation desc driver os)))

(defmethod dformat ((desc lambda-descriptor) (driver html) os)
  (with-html-description
      (:name (purge-string-for-html (name desc))
       :arg-list (purge-lambda-list-for-html (lambda-list desc))
       :type (html-printable-type desc)
       :anchor (lookup-meta-descriptor-anchor desc))
    (dformat-documentation desc driver os)))

(defmethod dformat ((desc defsetf-short-descriptor) (driver html) os)
  (with-html-description
      (:name (purge-string-for-html (name desc))
       :arg-list (purge-lambda-list-for-html (list (update-fn desc)))
       :type (html-printable-type desc)
       :anchor (lookup-meta-descriptor-anchor desc))
    (dformat-documentation desc driver os)))

(defmethod dformat ((desc defsetf-long-descriptor) (driver html) os)
    (let ((name (purge-string-for-html (name desc))))
      (with-slots (extra-args) desc
	(when extra-args
	  (setf name (format nil "(setf ~a <em class=\"args\">~a</em>)"
			     (subseq name 6 (1- (length name)))
			     (purge-lambda-list-for-html extra-args)))))
      (with-html-description
	  (:name name
	   :arg-list (purge-lambda-list-for-html (list (lambda-list desc)))
	   :type (html-printable-type desc)
	   :anchor (lookup-meta-descriptor-anchor desc))
	(dformat-documentation desc driver os))))

(defmethod dformat ((desc structured-object-descriptor) (driver html) os)
  (with-html-description 
      (:name (purge-string-for-html (name desc))
       :arg-list (format-inheritence desc)
       :type (html-printable-type desc)
       :anchor (lookup-meta-descriptor-anchor desc)
       :divclass "defclass")
    (dformat-documentation desc driver os)))

(defmethod dformat ((desc defstruct-descriptor) (driver html) os)
  (flet ((format-defun (desc)
	   (unless (and (filter driver) (funcall (filter driver) desc))
	     (dformat desc driver os))))
    (with-html-description
	(:name (purge-string-for-html (name desc))
	 :type (html-printable-type desc)
	 :anchor (lookup-meta-descriptor-anchor desc)
	 :divclass "defstruct")
      (dformat-documentation desc driver os)
      (with-tag (:div (:class "defstruct-defuns"))
	(mapc #'format-defun (constructors desc))
	(when (copier desc) (format-defun (copier desc)))
	(when (predicate desc) (format-defun (predicate desc)))))))

(defmethod dformat ((desc slot-descriptor) (driver html) os)
  (declare (ignorable os))
  (when (doc desc)
    (with-html-description
	(:divclass nil :name (name desc) :type (html-printable-type desc))
      (dformat-documentation desc driver os))))

(defmethod dformat ((desc defstruct-slot-descriptor) (driver html) os)
  (declare (ignorable desc driver os)))
