#|
Copyright (C) 2007, 2008  David Owen <dsowen@fugue88.ws>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Lesser Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser Public License for more details.

You should have received a copy of the GNU Lesser Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
|#

(defpackage #:dso-lex
    (:documentation "Allows the definition of lexers.  See DEFLEXER.")
  (:use #:cl #:cl-ppcre #:dso-util)
  (:export #:deflexer #:make-lexer #:lex-all))

(in-package #:dso-lex)



;;; regex manipulation

(defun anchor-and-mode (regex)
  `(:sequence (:flags :single-line-mode-p) :start-anchor ,regex))

(defun wrap (regex) (anchor-and-mode `(:regex ,regex)))

(defun combine (regex-list)
  (let ((mapped (mapcar
		 (lambda (regex) `(:register (:regex ,regex)))
		 regex-list)))
    (when (rest mapped) (setq mapped `((:alternation ,@mapped))))
    (anchor-and-mode (car mapped))))



;;; creating lexing forms

(defun break-defs (defs)
  (let (regexs classes filters)
    (dolist (d (reverse defs) (values regexs classes filters))
      (destructuring-bind (regex class &optional filter) d
        (push regex regexs)
        (push class classes)
        (push filter filters)))))

(defun greedy-lexer-form (input-var start-var defs)
  (multiple-value-bind (regexs classes filters) (break-defs defs)
    (setf regexs (mapcar 'wrap regexs))
    `(let ((classes ,(coerce classes 'vector))
           (filters ,(coerce filters 'vector))
           max
           at)
       ,@(mapcar
          (lambda (i)
            `(let ((end (nth-value 1 (scan ',(nth i regexs) ,input-var :start ,start-var))))
               (when (and end (or (null at) (> end max)))
                 (setf max end
                       at ,i))))
          (range (length regexs)))
       (when at
         (let ((image (make-array (- max ,start-var)
                                  :element-type 'character
                                  :displaced-to ,input-var
                                  :displaced-index-offset ,start-var))
               (filter (aref filters at)))
           (values (aref classes at)
                   (if filter (funcall filter image) image)
                   max))))))

(defun lexer-form (input-var start-var defs)
  (let ((regex (combine (mapcar #'first defs)))
        (classes (map 'vector #'second defs))
        (filters (map 'vector #'third defs)))
    `(let ((parts (nth-value 3 (scan (quote ,regex) ,input-var
                                     :start ,start-var))))
       (let ((idx (position-if #'identity parts)))
         (when idx
           (let ((end (aref parts idx)))
             (let ((image (make-array (- end ,start-var)
                                      :element-type 'character
                                      :displaced-to ,input-var
                                      :displaced-index-offset ,start-var))
                   (filter (aref ,filters idx)))
               (values (aref ,classes idx)
                       (if filter (funcall filter image) image)
                       end))))))))



;;; creating lexing functions

(defun make-lexer (defs &key priority-only)
  "Returns a lexer function.  The DEFS consists of token-class
definitions, each being a list of a regular expression, the name of
the class, and an optional filter.  The returned function takes as
arguments an input sequence and an optional start position, and
returning the matched token-class, image, and image-length as values.

Unless PRIORITY-ONLY is true, the longest match will win, and
rule-priority will only be used to break ties.  Otherwise, the first
match wins.

Example:

 (let ((lexer (make-lexer '((\"[0-9]+\" number parse-integer)
                            (\"[a-zA-Z]\" letter)))))
   (funcall lexer \"2pi\" 1))"
  (eval `(lambda (input &optional (start 0))
           ,(if priority-only
                (lexer-form 'input 'start defs)
                (greedy-lexer-form 'input 'start defs)))))

(defmacro deflexer (name (&key priority-only) &body defs)
  "Defines a lexer, called as a function of the given NAME, and returning
the matched token-class, image, and image-length as values.  The body
consists of token-class definitions, each being a list of a regular
expression, the name of the class, and an optional filter.

Unless PRIORITY-ONLY is true, the longest match will win, and
rule-priority will only be used to break ties.  Otherwise, the first
match wins.

Example:

 (deflexer lexer ()
   (\"[0-9]+\" number parse-integer)
   (\"[a-zA-Z]\" letter))

 (lexer \"2pi\" 1)"
  `(defun ,name (input &optional (start 0))
     ,(if priority-only
          (lexer-form 'input 'start defs)
          (greedy-lexer-form 'input 'start defs))))

(defun lex-all (lexer input)
  (labels ((scan (start tokens)
             (if (> (length input) start)
		 (multiple-value-bind (class image remainder)
		     (funcall lexer input start)
		   (when class
		     (scan remainder (cons (cons class image) tokens))))
		 (nreverse tokens))))
    (scan 0 '())))
