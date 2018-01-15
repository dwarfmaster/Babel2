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

;;;;; Function make-expandable/collapsable-element can be used to create
;;;;; html elements that expand when the user clicks on them and that
;;;;; collapse again when they are clicked a second time. Furthermore,
;;;;; several elements can be expanded/ collapsed at once using another
;;;;; button.
;;;;;
;;;;; To save browser resources (memory, time), the expanded and collapsed
;;;;; version of each element are kept in Lisp and are only sent to the
;;;;; browser when needed: the version that is initally sent to the client
;;;;; contains only the collapsed version and when the expand link is
;;;;; clicked, it is replaced with the expanded version stored on the lisp
;;;;; side.
;;;;;
;;;;; see the example at the bottom of the file.
;;;;;

(export '(make-expandable/collapsable-element
	  make-expand/collapse-link make-expand/collapse-all-link
	  *create-static-expandable/collapsable-elements*))
 
(defvar *expandable/collapsable-elements* (make-hash-table :test #'equal)
  "contains for each element id a list (collapsed-element expanded-element)")

(defvar *expand/collapse-all-ids* (make-hash-table :test #'equal)
  "contains for each expand/collapse-all id the ids of all group elements.")

(defun reset-expandable/collapsable-elements ()
  "resets the hash tables"
  (setf *expandable/collapsable-elements* (make-hash-table :test #'equal))
  (setf *expand/collapse-all-ids* (make-hash-table :test #'equal)))

(pushnew #'reset-expandable/collapsable-elements *reset-functions*)

;; client side javascript
(define-js 'expand/collapse "
function expand(id) {
    ajax_expand_or_collapse_node(id,true);
}

function collapse(id) {
    ajax_expand_or_collapse_node(id,false);
}

function expandAll(id) {
    ajax_expand_or_collapse_all(id,true);
}

function collapseAll(id) {
    ajax_expand_or_collapse_all(id,false);
}

function expandStatic(id) {
    var node = document.getElementById(id);
    node.firstChild.style.display = 'none';
    node.lastChild.style.display = 'inline';
}

function collapseStatic(id) {
    var node = document.getElementById(id);
    node.firstChild.style.display = 'inline';
    node.lastChild.style.display = 'none';
}

function expandAllStatic(id) {
    var nodes = document.getElementsByName(id);
    for (i=0;i<nodes.length;i++) {
	nodes[i].parentNode.firstChild.style.display = 'none';
	nodes[i].parentNode.lastChild.style.display = 'inline';
    }
}

function collapseAllStatic(id) {
    var nodes = document.getElementsByName(id);
    for (i=0;i<nodes.length;i++) { 
	nodes[i].parentNode.firstChild.style.display = 'inline';
	nodes[i].parentNode.lastChild.style.display = 'none';
    }
}
")

(defvar *create-static-expandable/collapsable-elements* nil
  "When this is set to t, both the expanded and collapsed version will
be embedded in the html code (one visible and the other hidden). This
makes the html code bigger (and thus rendering slower), but allows to
save generated html pages, with the expand/collapse functionality
still working when not connected to the web server.")

(defun make-expandable/collapsable-element (element-id expand/collapse-all-id 
					    collapsed-element expanded-element
					    &key (expand-initially nil))
  "Creates an element with an expanded and a collapsed version."
  (assert (stringp element-id))
  (assert (stringp expand/collapse-all-id))
  (assert (or (stringp collapsed-element) (functionp collapsed-element)))
  (assert (or (stringp expanded-element) (functionp expanded-element)))
  (let ((collapsed-element
	 (if (and (functionp collapsed-element) 
		  (or *create-static-expandable/collapsable-elements*
		      (not expand-initially)))
	     (with-output-to-string (*standard-output*) (funcall collapsed-element))
	     collapsed-element))
	(expanded-element 
	 (if (and (functionp expanded-element) 
		  (or *create-static-expandable/collapsable-elements*
		      expand-initially))
	     (with-output-to-string (*standard-output*) (funcall expanded-element))
	     expanded-element)))
    (unless *create-static-expandable/collapsable-elements*
      (setf (gethash element-id *expandable/collapsable-elements*) 
	    (list collapsed-element expanded-element))
      
      (setf (gethash expand/collapse-all-id *expand/collapse-all-ids*)
	    (cons element-id (gethash expand/collapse-all-id 
				      *expand/collapse-all-ids*))))
    (who 
     (:div 
      :style "display:inline-block" 
      :id element-id 
      (if *create-static-expandable/collapsable-elements*
	  (htm (:div :style (if expand-initially "display:none;" "display:inline")
		      (princ collapsed-element))
               (when *create-static-expandable/collapsable-elements*
                 ;; in xhtml 1 :name is only allowed in some elements, e,g <a ../>
                 (htm (:a :name expand/collapse-all-id)))
	       (:div :style (if expand-initially "display:inline;" "display:none")
		      (princ expanded-element)))
	  (princ (if expand-initially expanded-element collapsed-element)))))))


(defmacro make-expand/collapse-link (element-id expand? title &rest content)
  "makes a link for expanding/collapsing an element"
  `(who (:a :href (conc "javascript:" (if ,expand? "expand" "collapse")
			(if *create-static-expandable/collapsable-elements* 
			    "Static" "") 
			"('" ,element-id "');")
	    :title (or ,title (if ,expand? "expand" "collapse"))
	    ,@content)
        nil))


(defmacro make-expand/collapse-all-link (expand/collapse-all-id expand?
					 title &rest content)
  "makes a link for expanding/collapsing a group of elements"
  `(who (:a :href (conc "javascript:" (if ,expand? "expandAll" "collapseAll")
			(if *create-static-expandable/collapsable-elements* 
			    "Static" "") 
			"('" ,expand/collapse-all-id "');")
	    :title (or ,title (if ,expand? "expand all" "collapse all"))
	    ,@content)))


(defun expand-or-collapse-node-aux (id expand?)
  "A helper function that sends the correct version of an element to the browser"
  (let* ((entry (gethash id *expandable/collapsable-elements*))
	 (value (if expand? (second entry) (first entry))))
    ;; when the stored thing is a closure, call it and cache the result
    (if (functionp value) 
	(if expand? 
	    (setf (second entry) 
		  (with-output-to-string (*standard-output*) (funcall value)))
	    (setf (first entry) 
		  (with-output-to-string (*standard-output*) (funcall value)))))
    (replace-element-content 
     id (princ (if expand? (second entry) (first entry))))))

(defun-ajax expand-or-collapse-node (id expand?-string) (*ajax-processor*)
  "Called from the html page to expand/collapse a single element"
  (expand-or-collapse-node-aux id (if (equal expand?-string "true") t nil)))


(defun-ajax expand-or-collapse-all (id expand?-string) (*ajax-processor*)
  "Called from the html page to expand/collapse a set of elements"
  (loop for element-id in (gethash id *expand/collapse-all-ids*)
     do (expand-or-collapse-node-aux 
	 element-id (if (equal expand?-string "true") t nil))))


;; the example
(defun expandable-elements-example ()
  (let ((expand/collapse-all-id (make-id-string "expand-collapse-all-expample"))
	(element-id-1 (make-id-string "expand-collapse-expample"))
	(element-id-2 (make-id-string "expand-collapse-expample")))
    (gtfl-out (:h2 "expandable elements example"))
    (gtfl-out (:p "click on the links to see what happens"))
    (gtfl-out 
     (:p ;; the expand/collapse-all button
      (make-expandable/collapsable-element 
       (make-id-string) expand/collapse-all-id
       (who2s (make-expand/collapse-all-link expand/collapse-all-id t 
		  "custom expand all title" "expand all"))
       (who2s (make-expand/collapse-all-link expand/collapse-all-id nil
		  nil "collapse all")))))
    (gtfl-out 
     (:p 
      ;; with expanded and collapsed version pre-computed
      (:div 
       :style "border:1px solid #aaa;display:inline-block;margin-right:10px;"
       (make-expandable/collapsable-element 
	element-id-1 expand/collapse-all-id
	(who2s 
	 (:div
	  (make-expand/collapse-link element-id-1 t nil "expand")
	  (:br) "collapsed"))
	(who2s
	 (:div
	  (make-expand/collapse-link element-id-1 nil nil "collapse")
	  (:br) (:div :style "font-size:150%" "expanded")))))

      ;; with closures. The value for the expanded version is only
      ;; computed when the expand button is hit.
      (:div 
       :style "border:1px solid #aaa;display:inline-block;margin-right:10px;"
       (make-expandable/collapsable-element 
	element-id-2 expand/collapse-all-id
	(who2s 
	 (:div (make-expand/collapse-link element-id-2 t nil "expand with closure")
	       (:br) "collapsed"))
	(who-lambda
	 (:div (make-expand/collapse-link element-id-2 nil nil "collapse")
	       (:br) 
	       (:div :style "font-size:150%" 
		     "expanded version, computed when expand button clicked")))))))))


;;(expandable-elements-example)
