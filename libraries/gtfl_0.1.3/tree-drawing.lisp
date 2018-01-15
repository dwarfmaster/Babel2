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

;; Automatically resizing tree visualizations in html. See example
;; at the bottom of the file.

(export 'draw-node-with-children)


(define-css 'tree "
table.tree { border-collapse:collapse }
table.tree > tbody > tr > td { padding:0px;vertical-align:middle; }
table.tree div.child { position:relative; }
table.tree div.vl-top { height:50%;position:absolute;bottom:0px; }
table.tree div.vl-middle { height:100%;position:absolute;top:0px; }
table.tree div.vl-bottom { height:50%;position:absolute;top:0px; }
table.tree div.vl-left { left:0px; }
table.tree div.vl-right { right:0px; }
")

(defun draw-node-with-children (node children &key (right-to-left nil)
				(color "#888") (width "10px") 
				(line-width "1px") (style "solid"))
  "A function for recursively drawing trees in html. It draws a node
   with connections to it's children (which can be trees
   themselves). See example below."
  (assert (functionp node))
  (assert (listp children))
  (assert (loop for child in children always (functionp child)))
  (labels ;; these will be called in a different order depending on right-to-left
      ((node ()
	 ;; a td for the parent node
	 (who (:td (funcall node))))
       (h-line ()
	 ;; a single horizontal line to and from the vertical connector
	 (when children 
	   (who 
            (:td (:div :style (conc "border-top:" line-width " " style " " 
                                    color ";width:" width))))))
       (v-line (pos)
	 ;; the vertical connector between the horizontal lines
	 (unless (= (length children) 1) ;; only one child -> no vertical line
	   (who
            (:div 
             :class (conc (cond ((= pos 1) "vl-top")
                                ((= pos (length children)) "vl-bottom")
                                (t "vl-middle"))
                          (if right-to-left " vl-right" " vl-left"))
             :style (conc "border-left:" line-width " " color " " style)))))
       (child (child)
	 ;; a td for a child node"
         (who (:td (funcall child))))
       (children ()
	 (when children
	   (who 
            (:td 
             (loop for child in children
                for i from 1 
                do (who
                    (:div 
                     :class "child" 
                     :style (when right-to-left "clear:both;float:right;")
                     (:table :class "tree" 
                             (:tbody
                              (:tr 
                               (if right-to-left
                                   (progn (child child) (h-line))
                                   (progn (h-line) (child child))))))
                     (v-line i)))))))))
    (who 
     (:div 
      :style (when right-to-left "clear:both;float:right;")
      (:table 
       :class "tree"
       (:tbody 
        (:tr
         (if right-to-left 
             (progn (children) (h-line) (node))
             (progn (node) (h-line) (children)))))))
     (:div :style "clear:both"))))


(defun tree-drawing-example ()
  (let ((right-to-left nil) (color "#888") (width "10px") 
        (line-width "1px") (style "solid")
	(example-tree '("top node." 
			("child node one with three children" 
			 ("first out of three children") 
			 ("second out of three children")
			 ("third out of three children"))
			("child node two with one child" 
			 ("very long text. very long text. very long text. very long text. very long text. very long text. very long text. very long text. ")))))
    (labels (;; this makes a div with a border for each node
	     (draw-node (content) 
	       (who (:div :style "padding:4px;border:1px solid #888;margin-top:4px;margin-bottom:4px;background-color:#eee;"
			  (princ content))))

	     ;; recursive tree drawing
	     (draw-tree (tree) 
	       (draw-node-with-children 
		;; an anonymous function for drawing the current node 
		(who-lambda (draw-node (car tree)))
		;; anonymous functions for each child of the node
		(mapcar #'(lambda (x) (who-lambda (draw-tree x)))
			(cdr tree))
		:right-to-left right-to-left :color color
		:width width :line-width line-width :style style)))
      (gtfl-out (:h2 "tree drawing example"))
      (gtfl-out (:p "resize your browser window to see the dynamic tree 
                    rendering in action."))
      (gtfl-out (:p "with default parameters:"))
      (gtfl-out (draw-tree example-tree))
      (gtfl-out (:div "&#160;"))
      (gtfl-out 
       (:table 
	(:tr 
	 (:td :style "vertical-align:top"
	  "with parameters " 
	  (:tt ":right-to-left t :color &quot;black&quot; 
                :width &quot;30px&quot; :style &quot;dotted&quot;")
	  ":" (setf right-to-left t  color "black"  width "30px"  
                    line-width "1px"  style "dotted")
	  (draw-tree example-tree))
	 (:td 
	  :style "padding-left:20px" "with parameters " 
	  (:tt ":color &quot;#33a;&quot; :line-width &quot;2px&quot;") ":"
	  (setf right-to-left nil  color "#33a"  width "10px"  
                line-width "2px" style "solid")
	  (draw-tree example-tree))))))))

(defun my-tree-drawing-example (example-tree)
  (let ((right-to-left nil) (color "#888") (width "10px") 
        (line-width "1px") (style "solid"))
    (labels (;; this makes a div with a border for each node
	     (draw-node (content) 
	       (who (:div :style "padding:4px;border:1px solid #888;margin-top:4px;margin-bottom:4px;background-color:#eee;"
			  (princ content))))

	     ;; recursive tree drawing
	     (draw-tree (tree) 
	       (draw-node-with-children 
		;; an anonymous function for drawing the current node 
		(who-lambda (draw-node (car tree)))
		;; anonymous functions for each child of the node
		(mapcar #'(lambda (x) (who-lambda (draw-tree x)))
			(cdr tree))
		:right-to-left right-to-left :color color
		:width width :line-width line-width :style style)))
      (gtfl-out (:h2 "tree drawing example"))
      (gtfl-out (:p "resize your browser window to see the dynamic tree 
                    rendering in action."))
      (gtfl-out (:p "with default parameters:"))
      (gtfl-out (draw-tree example-tree))
      (gtfl-out (:div "&#160;"))
      (gtfl-out 
       (:table 
	(:tr 
	 (:td :style "vertical-align:top"
	  "with parameters " 
	  (:tt ":right-to-left t :color &quot;black&quot; 
                :width &quot;30px&quot; :style &quot;dotted&quot;")
	  ":" (setf right-to-left t  color "black"  width "30px"  
                    line-width "1px"  style "dotted")
	  (draw-tree example-tree))
	 (:td 
	  :style "padding-left:20px" "with parameters " 
	  (:tt ":color &quot;#33a;&quot; :line-width &quot;2px&quot;") ":"
	  (setf right-to-left nil  color "#33a"  width "10px"  
                line-width "2px" style "solid")
	  (draw-tree example-tree))))))))

;(tree-drawing-example
;     '(sentence 
;       (np
;        (Article (the ))
;       (Adjective (busy))
;        (Noun (violinist)))
;       (v (meets))
;       (np
;        (proper-noun (wolfgang)))))


(defun draw-node (node)
  (let
      ((structure (extract-constituent-structure node)))
    (my-tree-drawing-example
     (make-tree structure))))

;(my-tree-drawing-example (make-tree cl-user:test))


