(in-package :fcg)

;; Functions for converting from FCG Light constructions to the LateX visualisation
;; Structure:
;;     1. Main Functions
;;     2. Feature conversion
;;     3. Helper functions

;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Main Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun fcg-light-construction-set->latex (cxn-set &key (stream t)
                                            (feature-types (feature-types cxn-set)))
  (format stream "\\documentclass[11pt,landscape]{scrartcl}~%\\newcommand{\\tab}{\\hspace{2em}}~%\\usepackage{amsmath}~%\\begin{document}")
  (dolist (cxn (reverse (constructions cxn-set)))
    (fcg-light->latex cxn :stream stream :feature-types feature-types))
 (format stream "~%\\end{document}"))

(defun fcg-light->latex (cxn &key (stream t) (tag (name cxn))
                             (feature-types (or (feature-types *fcg-constructions*)
                                                '((args sequence)
                                                  (form set-of-predicates)
                                                  (meaning set-of-predicates)
                                                  (subunits set)))))
  "takes an fcg-light cxn and prints it into a latex align environment"
  ;; LaTeX: open align environment
  (format stream "~% ~%\\newpage~%\\begin{footnotesize}~%\\begin{align*}") 
  ;; Loop over units of contributing part, preprocess and print
  (if (contributing-part cxn)
    (dolist (contributing-unit (contributing-part cxn))
      (print-contributing-unit contributing-unit stream feature-types))
    (format stream "\\emptyset"))
  ;; LaTeX: print right arrow
  (format stream "\\\leftarrow \\\\~%" )
  ;; Loop over units of conditional part, preprocess and print
  (dolist (conditional-unit (conditional-part cxn))
    (print-conditional-unit conditional-unit stream feature-types))
  ;; when tag is not nil, print the tag
  (when tag (format stream "\\tag\{~(~a~)\}~%" tag)) ;; print the tag
  ;; LaTeX: close align environment
  (format stream "\\end{align*}~%\\\end{footnotesize}"))

(defun print-contributing-unit (contributing-unit stream feature-types)
  "prints a contributing unit as a LaTeX tabular to stream"
  (let ((unit-name (name contributing-unit))
        (fs (unit-structure contributing-unit)))
    ;; LaTeX: open unit
    (format stream "&\\left[")
    (format stream "~%\\begin{tabular}{l}")
    ;; LaTeX: print table header with unit name
    (format stream "~%{\\textbf ~(~a~)}\\\\~%\\hline \\hline " unit-name)
    ;; For every top-level feature: convert feature and print it
    (dolist (feature fs)
        (convert-feature feature 0 stream feature-types nil))
    ;; LaTeX: close tabular for unit
    (format stream "\\end{tabular}~%\\right]")))

(defun print-conditional-unit (conditional-unit stream feature-types)
  "prints a contributing unit as a LaTeX tabular to stream"
  (let ((unit-name (name conditional-unit))
        (formulation-lock (formulation-lock conditional-unit))
        (comprehension-lock (comprehension-lock conditional-unit)))
    ;; LaTeX: open unit
    (format stream "&\\left[")
    (format stream "~%\\begin{tabular}{l}")
    ;; LaTeX: print table header with unit name
    (format stream "~%{\\textbf ~(~a~)}\\\\~%\\hline \\hline " unit-name)
    ;; For every top-level feature of the formulation-lok: convert feature and print it
    (if (and formulation-lock (not (eql (first formulation-lock) 'emptyset)))
      (dolist (feature formulation-lock)
        (convert-feature feature 0 stream feature-types nil))
      (format stream "~%$\\emptyset$ \\\\~% "))
    ;; LaTeX: print lock-divider
    (format stream "\\hline ")
    ;; For every top-level feature of the comprehension-lock convert feature and print it
    (if (and comprehension-lock (not (eql (first formulation-lock) 'emptyset)))
      (dolist (feature comprehension-lock)
        (convert-feature feature 0 stream feature-types nil))
      (format stream "~%$\\emptyset$ \\\\~% "))
    ;; LaTeX: close tabular for unit
    (format stream "\\end{tabular}~%\\right]")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Feature Conversion ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-feature (feature tab-nr stream feature-types special-feature-type)
  (cond
   ;; Feature is not of a special type (such as set, sequence, predicate, set-of-predicates)
   ((not special-feature-type)
    (cond 
     ;; Feature is atom
     ((atom feature)
      (format stream "~(~a~)\\\\~%" feature))
     ((eql (first feature) 'hash) 
      (format stream "\\# ")
      (convert-feature (rest feature) tab-nr stream feature-types nil))
     ;; Feature is feature structure, subfeature is a set
     ((eql (second (assoc (first feature) feature-types)) 'set)
      (print-tab tab-nr stream)
      (format stream  "~(~a~): " (first feature))
      (convert-feature (second feature) tab-nr stream feature-types 'set))
     ;; Feature is feature structure, subfeature is a sequence
     ((eql (second (assoc (first feature) feature-types)) 'sequence)
      (print-tab tab-nr stream)
      (format stream  "~(~a~): " (first feature))
      (convert-feature (second feature) tab-nr stream feature-types 'sequence))
     ;; Feature is feature structure, subfeature is a set of predicates
     ((eql (second (assoc (first feature) feature-types)) 'set-of-predicates)
      (print-tab tab-nr stream)
      (format stream  "~(~a~): " (first feature))
      (print-tab tab-nr stream)
      (convert-feature (second feature) tab-nr stream feature-types 'set-of-predicates))
     ;; Feature is feature structure, value an atom
     ((and (eql (length (rest feature)) 1) (atom (second feature)))
      (print-tab tab-nr stream)
      (format stream  "~(~a~): " (first feature))
      (convert-feature (second feature)  tab-nr stream feature-types nil))
     ;; Feature is feature structure, subfeatures are feature structures
     (t
      (print-tab tab-nr stream)
      ;;
      (if (listp (first feature))
        (convert-feature (first feature) tab-nr stream feature-types nil)
        (progn
          (format stream  "~(~a~): " (first feature))
          (format stream "\\\\~%")))
      (incf tab-nr)
      (dolist (subfeature (rest feature))
        (convert-feature subfeature tab-nr stream feature-types nil)))))
   ;; Feature is a set of atoms
   ((eql special-feature-type 'set)
    (let ((first-set-element? t))
      (format stream "\\{")
      (dolist (set-element feature)
        (if first-set-element?
          (setf first-set-element? nil)
          (format stream ", "))
        (format stream "~(~a~)" set-element)))
    (format stream "\\}\\\\~%"))
   ;; Feature is a set of predicates
   ((eql special-feature-type 'set-of-predicates)
    (let ((first-set-element? t))
      (format stream "\\{")
      (dolist (predicate feature)
        (if first-set-element?
          (setf first-set-element? nil)
          (progn
            (format stream ",\\\\")
            (print-tab (+ 3 tab-nr) stream)))
        (convert-predicate predicate stream))) ;; process and print the predicate
    (format stream "\\}\\\\~%"))
   ;; Feature is a sequence of atoms
   ((eql special-feature-type 'sequence)
    (let ((first-sequence-element? t))
      (format stream "\\lbrack ")
      (dolist (sequence-element feature)
        (if first-sequence-element?
          (setf first-sequence-element? nil)
          (format stream ", "))
        (format stream "~(~a~)" sequence-element)))
    (format stream "\\rbrack\\\\~%"))))

(defun convert-predicate (predicate stream)
  (format stream "~(~a~)(" (first predicate))
  (let ((first-pred-element? t))
  (dolist (argument (rest predicate))
    (if first-pred-element?
      (setf first-pred-element? nil)
      (format stream ", "))
    (format stream "~(~a~)" argument))
  (format stream ")")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Helper Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-tab (tab-nr stream)
  (dotimes (n tab-nr) 
    (format stream "\\tab ")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transient Structure ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;convert-feature-to-new-notation
(defun convert-transient-structure-to-latex (ts &key (stream t)
                                                (tag nil))
  "Converts a FCG transient structure into a latex 'align' environment.
   Takes as arguments a FCG transient structure, an output stream and optionally a tag, ie. a string
   you want to appear as the tag in latex (e.g. cxn-name)"
  (format stream "\\documentclass[11pt]{scrartcl}~%\\usepackage{amsmath}~%\\begin{document}")
  (format stream "~%\\begin{footnotesize}~%\\begin{align*}") ;; latex: open align environment
  (let ((transient-structure (left-pole-structure (first ts)))
        (first-unit-to-print? t))
    (dolist (unit transient-structure)
      (when (not (equal (first unit) 'root)) 
        (if first-unit-to-print?
          (progn
            (setf first-unit-to-print? nil)
            (format stream "~%"))
          (format stream "\\\\~%"))
        (convert-ts-unit unit stream))))
  (when tag (format stream "~%\\tag\{~(~a~)\}" tag)) ;; print tag
  (format stream "~%\\end{align*}~%\\\end{footnotesize}~%\\end{document}")) ;; latex close align environment

(defun convert-ts-unit (unit stream &key (feature-types (or (feature-types *fcg-constructions*)
                                                            '((args sequence)
                                                              (form set-of-predicates)
                                                              (meaning set-of-predicates)))))
  "takes as input an FCG-3 transient structure unit and a stream, writes it as a tabular to the stream"
  (let ((unit-name (first unit))
        (feature-list (rest unit)))
    (format stream "&\\begin{tabular}{| l | }~%\\hline") ;; open tabular
    (format stream "~%{\\textbf ~(~a~)}\\\\~%\\hline \\hline " unit-name) ;; write unit name as table header
    (dolist (feature feature-list) ;; For each top-level feature of the unit
      (when (or (not (listp feature))
                (not (equal (first feature) 'footprints)))
        (convert-feature feature 0 stream feature-types nil))) ;; process and print feature
    (format stream "\\hline~%\\end{tabular}"))) ;; Latex: close unit




