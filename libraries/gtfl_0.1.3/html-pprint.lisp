;;;;; Copyright (c) 2009-2012, Martin Loetzsch
;;;;; All rights reserved.

;;;;; Redistribution and use in source and binary forms, with or
;;;;; without modification, are permitted provided that the following
;;;;; conditions are met:

;;;;;  Redistributions of source code must retain the above copyright
;;;;;  notice, this list of conditions and the following disclaimer.

;;;;;  Redistributions in binary form must reproduce the above
;;;;;  copyright notice, this list of conditions and the following
;;;;;  disclaimer in the documentation and/or other materials provided
;;;;;  with the distribution.

;;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
;;;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;;;;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;;;;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;;;;; IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
;;;;; THE POSSIBILITY OF SUCH DAMAGE.

(in-package :gtfl)

;; this file provides functionality for displaying automatically resizing 
;; s-expressions in a web browser. See example at the bottom of file.

(export 'html-pprint)

;; the client side javascript. when a user clicks on a symbol, all
;; symbols with the same name are highlighted.
(define-js 'highlight-symbol "
function highlightSymbol(symbolName, elementId, backgroundColor) {
  var nodes = document.getElementsByName(symbolName);
  var highlight = (document.getElementById(elementId).style.backgroundColor == '');
  for (i=0;i<nodes.length;i++) {
    if (highlight) {
      nodes[i].parentNode.style.backgroundColor = backgroundColor;
    } else {
      nodes[i].parentNode.style.backgroundColor = '';
    }
  }
}
")

(defparameter *colors* (list "#CCCCFF" "#CCFFCC" "#FFFF99" "#FFCC99" "#CCFFFF" 
			     "#CCFF99" "#FFFFCC" "#FF99FF" "#FFCCCC" "#99CCCC" 
			     "#FFCC66" "#99FF99" "#CCCCCC" "#99FFFF" "#66FF99")
  "colors for highlighing symbols")

(defvar *symbol-name->color* (make-hash-table :test #'equal)
  "makes sure that all instances of the same symbol get the same background color")

(defun get-color-for-symbol (symbol-name)
  "Returns for the same symbols the same color"
  (or (gethash symbol-name *symbol-name->color*)
      (setf (gethash symbol-name *symbol-name->color*) 
	    (elt *colors* (random (length *colors*))))))

(defun reset-html-pprint ()
  (setf *symbol-name->color* (make-hash-table :test #'equal)))

(pushnew #'reset-html-pprint *reset-functions*)



(define-css 'pprint "
div.pprint { margin-top:5px;}
div.pprint * { font-family:Courier;font-size:9pt;line-height:10px;display:inline-block; }
div.pprint span.table { display:inline-table;border-collapse:collapse;}
div.pprint span.table > span { display:table-cell;vertical-align:top; }
")


(defun html-pprint-aux (thing &optional (append ""))
  (cond 
    ;; "a"
    ((stringp thing) 
     (who (:span :class "table"
                 (:span "&quot;" (esc thing) "&quot;" (str append)))))
    ;; 1
    ((numberp thing)
     (who (:span :class "table" (:span (princ thing) (princ append)))))
    ;; 'a
    ((symbolp thing) 
     (let* ((symbol-name (symbol-name thing))
	    (element-id (make-id-string "s"))
	    (background-color (get-color-for-symbol symbol-name)))
       (who
        (:span :class "table"
               (:span :id element-id :style "white-space:nowrap;"
                      :onclick (format nil "highlightSymbol('~a','~a','~a');"
                                       symbol-name element-id background-color)
                      (:a :name symbol-name) ;; for being xhtml 1.0 conform
                      (let ((*print-case* :downcase))
                        (prin1 thing))
                      (if (equal append "") ""
                          (htm (:span :style "background-color:white;" 
                                      (str append)))))))))
    ;; '(a . b)
    ((and (consp thing) (cdr thing) (not (consp (cdr thing)))) 
     (who
      (:span :class "table"
             (:span (:span :class "table" (:span "(")))
             (:span (html-pprint-aux (car thing)))
             (:span (:span :class "table" (:span "&#160;.&#160;")))
             (:span (html-pprint-aux (cdr thing) (conc ")" append))))))
    ;; '(a b)
    ((listp thing) 
     (who
      (:span :class "table"
             (:span (:span :class "table" (:span "(")))
             (:span 
              (loop with l = (length thing)  for i from 1  
                 for x in thing
                 do (html-pprint-aux x (if (< i l) "" (conc ")" append)))
                   (when (< i l) (princ " ")))))))
    (t 
     (who
      (:span :class "table"
             (:span
              :style "white-space:nowrap"
              (let* ((*print-right-margin* 50) (*print-escape* t)
                     (*print-lines* 1) (*print-pretty* t) (*print-case* :downcase))
                (esc (format nil "~w~a" thing append)))))))))


(defun html-pprint (thing &key max-width)
  "Creates html code for displaying an x-expression in the web
   browser. The result lookes like coming from pprint (proper
   indention), but the layout is dynamic depending on the available
   width."
  (who
   (:div
    :class "pprint"
    :style (when max-width
	     (let* ((lisp-pprint-string
		     (let ((*print-right-margin* max-width) (*print-pretty* t))
		       (format nil "~w" thing)))
		    (longest-line-length
		     (loop for line in (split "\\n" lisp-pprint-string)
			maximize (length line))))
	       (format nil "max-width:~dpx;" (* 7.2 (+ longest-line-length 1)))))
    (html-pprint-aux thing))))



(defun html-pprint-example ()
  (gtfl-out 
   (let ((some-list
          `((("foo-1" "bar-1") (foo-2 bar-2) (:foo-3 :bar-3) 
             (,(make-symbol "FOO-4") ,(make-symbol "BAR-4")))
            ((foo-5 . bar-5) (foo-6 . bar-6)
             (0 1 2 3 4 5 6 7 8 9  ,(asdf:find-system :gtfl)
                )))))
     (who 
      (:h2 "html-pprint example")
      (:p "resize your browser to see the dynamic rendering 
                  of s-expressions. Click on any symbol to highlight all 
                  other symbols with the same name.")
      (:p "default parameters:")
      (:div :style "border:1px solid #aaa;" (html-pprint some-list))
      (:table :style "border-collapse:collapse;margin-top:10px;"
              (:tbody (:tr (loop for i from 1 to 2 
                              do (htm (:td :style "border:1px solid #aaa;"
                                           (html-pprint some-list)))))))
      (:p "with " (:tt ":max-width 50") ":")
      (:div :style "border:1px solid #aaa;display:inline-table"
            (html-pprint some-list :max-width 50))
      (:p "with " (:tt ":max-width 100") ":")
      (:div :style "border:1px solid #aaa;display:inline-table"
            (html-pprint some-list :max-width 100))))))

;;(html-pprint-example)




