;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLUDG; -*-
;;; $Id: package.lisp,v 1.5 2006/01/09 22:23:43 ihatchondo Exp $
;;; ---------------------------------------------------------------------------
;;;     Title: Common Lisp Universal Documentation Generator package definition
;;;   Created: 2005 10 23 12:30
;;;    Author: Iban Hatchondo <hatchond@yahoo.fr>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005 by Iban Hatchondo

;;; The authors grant you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(common-lisp:in-package :common-lisp-user)

(defpackage "CLDOC"
  ;; form minion (on of the #lisp irc bot):
  ;; CLUDG: Crisp Lacunose Unkirk Dactyliomancy Geneticist
  ;; was the original name.
  (:nicknames cludg)
  (:use common-lisp)
  (:export
   #:*constructor-control-string* #:*predicate-control-string*
   #:*printer-control-string* 
   #:*class-inheritence* #:*condition-inheritence*
   #:*struct-inheritence* #:*slot-reader-control-string*
   #:*slot-writer-control-string* #:*slot-accessor-control-string*
   #:+html-doctype+ #:+default-charset+ #:+default-link-delimiters+
   #:+default-section-prefix+ #:+default-code-prefix+ #:+default-section-names+
   #:define-descriptor-handler
   #:find-descriptor-handler
   #:register-output-type
   #:find-output-type
   #:grok-new-lines
   #:define-string-purger
   #:define-lambda-list-purger
   ;; functions, class & protocol.
   #:driver
   #:html
   #:text
   #:extract-documentation
   #:dformat
   #:dformat-documentation
   ;; cache system.
   #:meta-descriptor
   #:meta-descriptor-href #:meta-descriptor-desc #:meta-descriptor-file
   #:clear-cache #:cache-descriptor #:initialise-cache
   #:lookup-meta-descriptor #:lookup-meta-descriptor-anchor
   #:lookup-meta-descriptor-href #:cache-meta-descriptors
   ;; documentation string parsing utilities
   #:doctree
   #:doctree-tree
   #:bulleted-list
   #:last-list-item
   #:add-to-bulleted-list-item
   #:add-to-bulleted-list
   #:handle-string
   #:add-to-paragraph
   #:paragraph-handle-line
   #:add-to-code-block
   #:create-doctree-from-string
   #:starts-with
   #:start-para-p
   #:with-tree-loop)
  (:documentation "Common Lisp Universal Documentation Generator.
 
   CLDOC reads lisp source files and generates documentation using the 
   selected output driver. Because it is performing some symbol package
   resolution it needs packages definition to be, at least loaded.
   A simple way to satisfy this condition is to load, such as require would,
   the systems to be documented before starting documentation extraction.

    It currently has an HTML driver that generates XHTML 1.0 Strict.
   This HTML driver has some simple DWIM (Do What I Mean) capabilities
   using the doctree string parser facilities:
    - Recognize both indent and empty-line paragraph breaks.
    - Recognizes bulleted lists (the list grammar can be specified).
    - Recognizes code segments: by default each lines are prefixed with ';;; '.
    - Recognizes links: two kinds of hyper link are possible both using the
      same grammar: 
      [opening-char(defun|defclass|defgeneric|...) symbol-name closing-char] |
      [opening-char(http://|ftp://)address closing-char].
      opening-char and closing char can be customized, see the :link-delimiters
      option of {defclass cldoc:doctree} .
      
      ;;; (defun foo ()
      ;;;  \"-- URL's: {http://common-lisp.net/cldoc}
      ;;;   -- Common Lisp symbols: {defgeneric cldoc:extract-documentation}\"
      ;;;  (values))
      
      will produce:
       -- URL's: {http://common-lisp.net/cldoc}
       -- Common Lisp symbols: {defgeneric cldoc:extract-documentation}

    Unlike Albert, {http://albert.sourceforge.net} , it does not allow
   programmers to insert comments at the source code level which are
   incorporated into the generated documentation. 
    Its goal was not to produce a LispDoc ala JavaDoc but to create a simple
   and easy way to take advantage of the Lisp documentation string. So instead
   of copying and pasting it in some commentary section with extra special
   documentation tool markup stuff, the idea was to find a elegant way of
   parsing the doc string.

   To get started for documentation extraction see the extract-documentation
   generic function.
    
   If you want your particular macro top-level form to be parsed, then use the
   define-descriptor-handler macro. A basic use case of this macro would be:
    ;;; ;; Extracted from doc-cludg.lisp
    ;;; (cldoc::define-descriptor-handler DEFINE-STRING-PURGER (form)
    ;;;   \"string purger\"
    ;;;   (setf (car form) 'cldoc::define-string-purger)
    ;;;   (values nil :restart (list (let ((*print-case* :upcase))
    ;;;                                (macroexpand-1 form)))))

   In the above example the created handler will call macroexpand-1 on the 
   form and return the macro expansion for it to parsed. Instead if this one
   could also have tried to handle by itself the DEFINE-STRING-PURGER because
   it returns a DEFUN form. This would have give:
    ;;; (cldoc::define-descriptor-handler DEFINE-STRING-PURGER (form)
    ;;;   \"string purger\"
    ;;;   (setf (car form) 'cldoc::define-string-purger)
    ;;;   (let* ((*print-case* :upcase)
    ;;;          (macroexp (macroexpand-1 form))
    ;;;          (handler (car (find-descriptor-handler (car macroexp)))))
    ;;;     (when handler
    ;;;       (funcall handler form))))

   But in general a macro expansion result in more than one DEFUN form, and
   thus, the  multiple value return (nil :restart macro-expansion) provides
   a more generic way to handle most cases.
   Another option would have been be to create a new symbol-description
   subclass for this form and to implement the form parsing in the handler.

   Here is the code that has been used to generate CLDOC HTML documentation:
    ;;;  (in-package :cldoc)
    ;;; 
    ;;; ;; Special cldoc handler
    ;;; 
    ;;; (cldoc::define-descriptor-handler DEFINE-STRING-PURGER (form)
    ;;;   \"string purger\"
    ;;;   (setf (car form) 'cldoc::define-string-purger)
    ;;;   (values nil :restart (list (let ((*print-case* :upcase))
    ;;; 			       (macroexpand-1 form)))))
    ;;; 
    ;;; (cldoc::define-descriptor-handler DEFINE-LAMBDA-LIST-PURGER (form)
    ;;;   \"lambda purger\"
    ;;;   (setf (car form) 'cldoc::define-lambda-list-purger)
    ;;;   (values nil :restart (list (let ((*print-case* :upcase))
    ;;; 			       (macroexpand-1 form)))))
    ;;; 
    ;;; ;; Extract doc.
    ;;; 
    ;;; (cldoc:extract-documentation 'cldoc:html \"docu\"
    ;;;   (asdf:find-system :cldoc)
    ;;;   :table-of-contents-title
    ;;;   \"Common Lisp Universal Documentation Generator\")

   This project has been mainly inspired by user-manual from Mark Kantrowitz,
   the CSS file Gilbert Baumann made for McCLIM documentation and of course
   by the JavaDoc tool from Sun."))
