;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; *************************************************************************
;;;; FILE IDENTIFICATION
;;;;
;;;; Name:          i18n.lisp
;;;; Purpose:       non-ASCII character support
;;;; Programmer:    Kevin M. Rosenberg
;;;; Date Started:  Feb 2010
;;;;
;;;; This file, part of UFFI, is Copyright (c) 2002-2010 by Kevin M. Rosenberg
;;;;
;;;; *************************************************************************

(in-package #:uffi)

#-(or (and lispworks unicode) (and sbcl sb-unicode)
      (and allegro ics) (and clisp i18n)
      (and openmcl openmcl-unicode-strings))
(pushnew 'no-i18n cl:*features*)

(defvar *default-foreign-encoding*
  nil
  "Normalized name of default external character format to use
for foreign string conversions. nil means use implementation default
encoding.")

(defvar *foreign-encoding-mapping*
    #+(and lispworks unicode)
    '((:ascii . :ascii) (:latin-1 . :latin-1) (:ucs-2 . :unicode)
      (:utf-8 . :utf-8) (:jis . :jis) (:sjis . :sjis) (:gbk . :gbk))
    #+(and sbcl sb-unicode)
    '((:ascii . :ascii) (:latin-1 . :latin-1) (:utf-8 . :utf-8)
      (:ucs-2 . :ucs-2) (:sjis . :sjis) (:gbk . :gbk))
    #+(and allegro ics)
    '((:ascii . :ascii) (:latin-1 . :latin1) (:utf-8 . :utf-8)
      (:sjis . :shift-jis) (:euc-jp . :euc) (:gbk . :gb2313)
      (:ucs-2 . :unicode))
    #+(and clisp unicode)
    '((:ascii . charset:ascii) (:ucs-2 . charset:ucs-2)
      (:utf-8 . charset:utf-8) (:latin-1 . charset:iso-8859-1)
      (:jis . charset:jis_x0201) (:jis . charset:shift-jis)
      (:gbk . charset:gbk) (:euc-jp . charset:euc-jp))
    #+(and openmcl openmcl-unicode-strings)
    '((:ascii . :ascii) (:latin-1 . :iso-8859-1) (:utf-8 . :utf-8)
      (:ucs-2 . :ucs-2)
      #+nil (:euc-jp . :euc-jp)
      )
    #-(or (and lispworks unicode) (and sbcl sb-unicode)
          (and allegro ics) (and clisp unicode)
          (and openmcl openmcl-unicode-strings))
    nil
  "Mapping between normalized external format name and implementation name.")

(defvar *foreign-encodings*
  (mapcar 'car *foreign-encoding-mapping*)
  "List of normalized names of external formats support by underlying implementation.")

(defun lookup-foreign-encoding (normalized)
  (cdr (assoc normalized *foreign-encoding-mapping* :test 'eql)))

(defmacro string-to-octets (str &key encoding null-terminate)
  (declare (ignorable encoding))
  #-(or allegro lispworks openmcl sbcl)
  (map-into (make-array (length str) :element-type '(unsigned-byte 8))
            #'char-code str)

  #+allegro
  (let ((fe (gensym "FE-"))
        (ife (gensym "IFE-"))
        (s (gensym "STR-"))
        (nt (gensym "NT-")))
    `(let* ((,fe (or ,encoding *default-foreign-encoding*))
            (,ife (when ,fe (lookup-foreign-encoding ,fe)))
            (,s ,str)
            (,nt ,null-terminate))
       (values
        (if ,ife
            (excl:string-to-octets ,s :external-format ,ife :null-terminate ,nt)
            (excl:string-to-octets ,s :null-terminate ,nt)))))

  #+ccl
  ;; simply reading each char-code from the LENGTH of string handles
  ;; multibyte characters in testing with CCL 1.5
  (let ((len (gensym "LEN-"))
        (out (gensym "OUT-")))
    `(let ((,len (length ,str)))
       (if (,null-terminate)
           (progn
             (let ((,out (map-into (make-array (1+ ,len) :element-type '(unsigned-byte 8))
                                   #'char-code ,str)))
               (setf (char ,out ,len) 0)
               ,out))
           (map-into (make-array len :element-type '(unsigned-byte 8))
                     #'char-code str))))

  #+lispworks
  ;; simply reading each char-code from the LENGTH of string handles multibyte characters
  ;; just fine in testing LW 6.0 and CCL 1.4
  (let ((len (gensym "LEN-"))
        (out (gensym "OUT-")))
    `(let ((,len (length ,str)))
       (if (,null-terminate)
           (progn
             (let ((,out (map-into (make-array (1+ ,len) :element-type '(unsigned-byte 8))
                                   #'char-code ,str)))
               (setf (char ,out ,len) 0)
               ,out))
           (map-into (make-array len :element-type '(unsigned-byte 8))
                     #'char-code str))))

  #+sbcl
  (let ((fe (gensym "FE-"))
        (ife (gensym "IFE-"))
        (s (gensym "STR-"))
        (nt (gensym "NT-")))
    `(let* ((,fe (or ,encoding *default-foreign-encoding*))
            (,ife (when ,fe (lookup-foreign-encoding ,fe)))
            (,s ,str)
            (,nt ,null-terminate))
       (if ,ife
           (sb-ext:string-to-octets ,s :external-format ,ife :null-terminate ,nt)
           (sb-ext:string-to-octets ,s :null-terminate ,nt))))

)

(defmacro octets-to-string (octets &key encoding)
  "Converts a vector of octets to a Lisp string."
  (declare (ignorable encoding))
  #-(or allegro lispworks openmcl sbcl)
  (let ((out (gensym "OUT-"))
        (code (gensym "CODE-")))
    `(with-output-to-string (,out)
       (loop for ,code across ,octets
          do (write-char (code-char ,code) ,out))))

  #+allegro
  (let ((fe (gensym "FE-"))
        (ife (gensym "IFE-"))
        (oct (gensym "OCTETS-")))
    `(let* ((,fe (or ,encoding *default-foreign-encoding*))
            (,ife (when ,fe (lookup-foreign-encoding ,fe)))
            (,oct ,octets))
       (values
        (if ,ife
            (excl:octets-to-string ,oct :external-format ,ife)
            (excl:octets-to-string ,oct)))))

  #+lispworks
  ;; With LW 6.0, writing multibyte character just one octet at a time
  ;; produces expected formatted output, but strings lengths are too
  ;; long and consists only of octets, not wide characters
  ;;
  ;; Below technique of using fli:convert-from-foreign-string works tp
  ;; correctly create string of wide-characters. However, errors occur
  ;; during formatted printing of such strings with an error such as
  ;; "#\U+30D3 is not of type BASE-CHAR"
  (let ((fe (gensym "FE-"))
        (ife (gensym "IFE-"))
        (oct (gensym "OCTETS-")))
    `(let* ((,fe (or ,encoding *default-foreign-encoding*))
            (,ife (when ,fe (lookup-foreign-encoding ,fe)))
            (,oct ,octets))
       (fli:with-dynamic-foreign-objects
           ((ptr (:unsigned :byte) :initial-contents (coerce ,oct 'list)))
         (fli:convert-from-foreign-string ptr
                                          :length (length ,oct)
                                          :null-terminated-p nil
                                          :external-format ,ife))))

  #+(or ccl openmcl)
  ;; With CCL 1.5, writing multibyte character just one octet at a time tests fine
  (let ((out (gensym "OUT-"))
        (code (gensym "CODE-")))
    `(with-output-to-string (,out)
       (loop for ,code across ,octets
          do (write-char (code-char ,code) ,out))))

  #+sbcl
  (let ((fe (gensym "FE-"))
        (ife (gensym "IFE-"))
        (oct (gensym "OCTETS-")))
    `(let* ((,fe (or ,encoding *default-foreign-encoding*))
            (,ife (when ,fe (lookup-foreign-encoding ,fe)))
            (,oct ,octets))
       (if ,ife
           (sb-ext:octets-to-string ,oct :external-format ,ife)
           (sb-ext:octets-to-string ,oct))))

)

(defun foreign-encoded-octet-count (str &key encoding)
  "Returns the octets required to represent the string when passed to a ~
foreign function."
  (declare (ignorable encoding))
  ;; AllegroCL 8-bit, CCL, and Lispworks give correct value without converting
  ;; to external-format. AllegroCL 16-bit, SBCL, and CLISP requires conversion
  ;; with external-format

  #+(or (and allegro ics) (and sbcl sb-unicode) (and clisp i18n))
  (length (string-to-octets str :encoding
                            (or encoding *default-foreign-encoding*)))

  #-(or (and allegro ics) (and sbcl sb-unicode) (and clisp i18n))
  (length str)

)
