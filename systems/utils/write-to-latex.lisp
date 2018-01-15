(in-package :utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some functions for writing from lisp to a latex file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun latex-installed ()
  "checks wheter babel can access your pdflatex program, explains you why not otherwise"
  (let ((latex-installed (exec-and-return "which" "pdflatex")))
    (if latex-installed
      latex-installed
      (error "PdfLaTeX was not found on your computer. Either it is not installed or it is not accessible in your Lisp editors path (which is most often NOT the same as your bash terminal's path. You may want to enter 'which pdflatex' in a terminal, look at your pdflatex path and make a symlink to usr/local/bin with the following command: ln -s /your/path/to/pdflatex /usr/bin/pdflatex )"))))

(defun write-to-latex (outfile &key author title date body (open t))
  ;; check whether pdflatex can be reached
  (latex-installed)
  (with-open-file (outstream outfile :direction :output :if-exists :supersede)
    ;; Write latex header
    (format outstream "\\documentclass[11pt,a4paper]{article}~%")
    (format outstream "\\usepackage[utf8]{inputenc}~%")
    (format outstream "\\usepackage[english]{babel}~%")
    (when title
      (format outstream "\\title{~a}" title))
    (when author
      (format outstream "\\author{~a}" author))
    (when date
      (format outstream "\\date{~a}" date))
    ;; Write latex body
    (format outstream "\\begin{document}~%")
    (format outstream "\\maketitle~%~%")
    (when body
      (format outstream "~a" body))
    ;; Write latex footer
    (format outstream "\\end{document}~%"))
  ;; Run pdflatex
  (run-prog "pdflatex" :args `("-interaction=nonstopmode"
                               "-output-directory" ,(format nil "~a" (make-pathname :directory (pathname-directory outfile)))
                               ,(format nil "~a" outfile))) ;">/dev/null"))
  ;; Open result
  (let ((pdf-file (make-pathname :directory (pathname-directory outfile)
                                 :name (pathname-name outfile)
                                 :type "pdf")))
    (unless (probe-file pdf-file)
      (error "No pdf file could be found, probably pdflatex crashed while compiling."))
    (when open (run-prog "open" :args `(,pdf-file)))
    (format nil "written to ~a" outfile)))

(defun latex-make-section (&key title body)
  "takes strings section-title and section-body and returns latex code for this section"
  (format nil "\\section{~a}~%~a~%" title body))

(defun latex-make-body (&rest sections-or-other-elements)
  "appends latex sections etc."
  (format nil "~{~a~%~}~%~%" sections-or-other-elements))

;; Example

#|
(let ((outfile (babel-pathname :directory '(".tmp") :name "test" :type "tex")))

  (write-to-latex outfile
                  :author "Paul Van Eecke"
                  :title "My first lisp-to-latex file"
                  :date "\\today"
                  :body (latex-make-body
                         (latex-make-section :title "How it works"
                                             :body "This is one long string with body and it can
contain any \\LaTeX code, as long as you use double backslashes as escape signs.")
                         (latex-make-section :title "Section two"
                                             :body "A second section"))))
|#






