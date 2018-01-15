          Common Lisp Universal Documentation Generator.
 
  CLDOC reads lisp source files and generates documentation using the 
 selected output driver. Because it is performing some symbol package
 resolution it needs packages definition to be, at least loaded.
 A simple way to satisfy this condition is to load, such as require
 would, the systems to be documented before starting documentation
 extraction. 

  It currently has an HTML driver that generates XHTML 1.0 Strict.
 This HTML driver has some simple DWIM (Do What I Mean) capabilities
 using the doctree string parser facilities:
  - Recognize both indent and empty-line paragraph breaks.
  - Recognizes bulleted lists (the list grammar can be specified).
  - Recognizes code segments: by default each lines are prefixed with
    ';;; '.
  - Recognizes links: for standard URL's and symbol referencing.
    (see the :link-delimiters option of the doctree class).

  Unlike Albert (http://albert.sourceforge.net/) it does not allow
 programmers to insert comments at the source code level which are
 incorporated into the generated documentation. 
  Its goal was not to produce a LispDoc ala JavaDoc but to create a
 simple and easy way to take advantage of the Lisp documentation
 string. So instead of copying and pasting it in some commentary
 section with extra special documentation tool markup stuff, the idea
 was to find a elegant way of parsing the doc string.
  To get started for documentation extraction see the
 extract-documentation generic function.
 
  If you want your particular macro top-level form to be parsed, then
  use the define-descriptor-handler macro. A basic use case of this
  macro would be: 
    ;; Extracted from doc-cludg.lisp
    (cldoc::define-descriptor-handler DEFINE-STRING-PURGER (form)
      "string purger"
     (setf (car form) 'cldoc::define-string-purger)
     (values nil :restart (list (let ((*print-case* :upcase))
                                  (macroexpand-1 form)))))

  In the above example the created handler will call macroexpand-1 on
 the form and return the macro expansion for it to parsed. Instead if
 this one could also have tried to handle by itself the
 DEFINE-STRING-PURGER because it returns a DEFUN form. This would
 have give: 
    (cldoc::define-descriptor-handler DEFINE-STRING-PURGER (form)
      "string purger"
      (setf (car form) 'cldoc::define-string-purger)
      (let* ((*print-case* :upcase)
             (macroexp (macroexpand-1 form))
             (handler (car (find-descriptor-handler (car macroexp)))))
        (when handler
          (funcall handler form))))

  But in general a macro expansion result in more than one DEFUN form,
 and thus, the  multiple value return (nil :restart macro-expansion)
 provides a more generic way to handle most cases. Another option
 would have been be to create a new symbol-description subclass for
 this form and to implement the form parsing in the handler.  Here is
 the code that has been used to generate CLUDG HTML documentation: 

    ;;;; doc-cludg.lisp
    (in-package :cldoc)
 
    ;; Special cldoc handler
 
    (cldoc::define-descriptor-handler DEFINE-STRING-PURGER (form)
      "string purger"
      (setf (car form) 'cldoc::define-string-purger)
      (values nil :restart (list (let ((*print-case* :upcase))
                                   (macroexpand-1 form)))))
 
    (cldoc::define-descriptor-handler DEFINE-LAMBDA-LIST-PURGER (form)
      "lambda purger"
      (setf (car form) 'cldoc::define-lambda-list-purger)
      (values nil :restart (list (let ((*print-case* :upcase))
                                   (macroexpand-1 form)))))
 
    ;; Extract doc.

    (cldoc:extract-documentation 'cldoc:html \"docu\"
      '("package.lisp"
        "cludg.lisp"
        "cache-system.lisp"
        "string-parser.lisp"
        "html.lisp")
       :table-of-contents-title
       "Common Lisp Universal Documentation Generator")

    ;;;; End of doc-cludg.lisp

  This project has been mainly inspired by user-manual from Mark
 Kantrowitz, the CSS file Gilbert Baumann made for McCLIM
 documentation and of course by the JavaDoc tool from Sun.

