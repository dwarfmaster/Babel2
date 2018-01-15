
(in-package :fcg)

;;;;; Some abstractions to work with the web interface.
;;;;; -------------------------------------------------
(export '(show show-inventory
          header paragraph bullet bullets hline
          browser-warning))

;;;;;; General functions.
;;;;;; -----------------------------------------------
(defun show (entity)
  "Show any entity in the web interface."
  (add-element (make-html entity)))

;;;;;; For displaying text.
;;;;;; -----------------------------------------------
(defun header (string &optional (level 'h1))
  "Display a header in the web interface."
  (add-element
   `((,level) ,string)))

(defun hline ()
  "Display a horizontal line in the web interface."
  (add-element '((hr))))

(defun paragraph (string &key style)
  "Display a piece of text in the web interface."
  (add-element
   `((p ,@(if style (list :style style) nil)) ,string)))

(defun bullet (text &key style)
  "Put a piece of text in a bullet point."
  (let ((bullet (format nil "<ul> <li> ~a </li> </ul>" text)))
    (paragraph bullet :style style)))

(defun bullets (list-of-strings &key style)
  "Display several strings as a list of bullet points."
  (let ((bullets (string-append "<ul>"
                                (apply #'string-append
                                       (loop for string in list-of-strings
                                             collect (format nil "<li>~a</li>" string)))
                                "</ul>")))
    (paragraph bullets :style style)))

(defun browser-warning (&key (warning "We recommend using Firefox or Safari for an optimal viewing of this page.")
                             (style "color:red"))
  "Display a warning in the browser."
  (paragraph warning :style style))
