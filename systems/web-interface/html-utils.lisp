
(in-package :web-interface)

;;;;; this is a collection of general html creation methods 
;;;;; if you want to share some of your html code, add it here.

(export '(draw-node-with-children
          make-toggle-display-style-link-parameters
          make-toggle-display-style-all-link-parameters
	  make-expandable/collapsable-element 
	  make-expand/collapse-link-parameters make-expand/collapse-link
	  make-expand/collapse-all-link-parameters
          make-expand/collapse-all-link
          html-hide-rest-of-long-list
          expand-or-collapse-node-aux
          expand-or-collapse-node
          make-div-with-menu
	  wait-for-response make-response-link-parameters
	  html-pprint
          store-wi-object get-wi-object
          get-highlighted-element))

;; #########################################################
;; draw-node-with-children
;; ---------------------------------------------------------

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
  (labels ;; these will be called in a different order depending on right-to-left
      ((node ()
	 ;; a td for the parent node
	 `((td) ,node))
       (h-line ()
	 ;; a single horizontal line to and from the vertical connector
	 (if children 
           `((td) ((div :style ,(mkstr "border-top:" line-width " " style " " 
                                       color ";width:" width))))
           ""))
       (v-line (pos)
	 ;; the vertical connector between the horizontal lines
	 (if (= (length children) 1) 
           "";; only one child -> no vertical line
           `((div :class ,(mkstr (cond ((= pos 1) "vl-top")
                                       ((= pos (length children)) "vl-bottom")
                                       (t "vl-middle"))
                                 (if right-to-left " vl-right" " vl-left"))
                  :style ,(mkstr "border-left:" line-width " " color " " style)))))
       (child (child)
	 ;; a td for a child node"
         `((td) ,child))
       (children ()
                 (if children
                   `((td)
                     ,@(loop for child in children
                             for i from 1 
                             collect
                             `((div :class "child" 
                                    :style ,(if right-to-left "clear:both;float:right;" ""))
                               ((table :class "tree")
                                ((tbody)
                                 ((tr)
                                  ,@(if right-to-left
                                      (list (child child) (h-line))
                                      (list (h-line) (child child))))))
                               ,(v-line i)
                               )))
                   "")))
    `((div :style "clear:both;")
      ((div :style ,(if right-to-left "float:right;" ""))
       ((table :class "tree")
        ((tbody)
         ((tr)
          ,@(if right-to-left 
              (list (children) (h-line) (node))
              (list (node) (h-line) (children))))))))))

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
    (labels (;; recursive tree drawing
	     (draw-tree (tree) 
	       (draw-node-with-children 
		;; an anonymous function for drawing the current node 
		`((div :style "padding:4px;border:1px solid #888;margin-top:4px;margin-bottom:4px;background-color:#eee;")
                  ,(car tree))
		;; anonymous functions for each child of the node
		(mapcar #'(lambda (x) (draw-tree x))
			(cdr tree))
		:right-to-left right-to-left :color color
		:width width :line-width line-width :style style)))
      (add-element '((h2) "tree drawing example"))
      (add-element '((p) "resize your browser window to see the dynamic tree 
                         rendering in action."))
      (add-element '((p) "with default parameters:"))
      (add-element (draw-tree example-tree))
      (add-element '((div) "&#160;"))
      (add-element 
       `((table)
         ((tr)
          ((td) 
           "with parameters " 
           ((tt) ":right-to-left t :color &quot;black&quot; 
                  :width &quot;30px&quot; :style &quot;dotted&quot;")
           ":" 
           ,(progn
              (setf right-to-left t  color "black"  width "30px" 
                    line-width "1px" style "dotted") "")
           ,(draw-tree example-tree))
          ((td :style "padding-left:20px")
           "with parameters " 
           ((tt) ":color &quot;#33a;&quot; :line-width &quot;2px&quot;") ":"
           ,(progn
              (setf right-to-left nil  color "#33a"  width "10px"  
                    line-width "2px" style "solid")  "")
           ,(draw-tree example-tree))))))))

;;(tree-drawing-example)

;; #########################################################
;; make-toggle-display-element
;; ---------------------------------------------------------
;;;;; this allows to switch on switch off elements by
;;;;; manipulating toggling ther display style from none to
;;;;; inline (default) or something user provided
;;;;; this works on every element such as table rows, div
;;;;; and so on
;;;;; the difference to make-expandable/collapsable-element
;;;;; is that the element is always on the client (browser)
;;;;; and only its style is manipulated

(define-js 'toggle-display-style "
function toggle_display_style( id, value1, value2) {
   var div = document.getElementById(id);
   if( div.style.display == value1){
     div.style.display = value2;
   }
   else{
     div.style.display=value1;
   }
}
function toggle_display_style_all( name, value1, value2) {
  var nodes = document.getElementsByName( name);
  for (i=0;i<nodes.length;i++) {
    toggle_display_style( nodes[i].getAttribute('id'), value1, value2);
  }
}
")

(define-static-js 'toggle-display-style "
function toggle_display_style( id, value1, value2) {
   var div = document.getElementById(id);
   if( div.style.display == value1){
     div.style.display=value2;
   }
   else{
     div.style.display=value1;
   }
}
function toggle_display_style_all( name, value1, value2) {
  var nodes = document.getElementsByName(name);
  for (i=0;i<nodes.length;i++) {
    toggle_display_style( nodes[i].getAttribute('id'), value1, value2);
  }
}
")

(defun make-toggle-display-style-link-parameters
       (element-id display-value-1 display-value-2 title)
  "to be used when creating a <a >click</a> link for setting display style"
  `(:href ,(format nil "javascript:toggle_display_style(~('~a','~a','~a'~));" 
		   element-id display-value-1 display-value-2)
    :title ,title))

(defun make-toggle-display-style-all-link-parameters
       (element-name display-value-1 display-value-2 title)
  "to be used when creating a <a >click</a> link for setting display style"
  `(:href ,(format nil "javascript:toggle_display_style_all(~('~a','~a','~a'~));" 
		   element-name display-value-1 display-value-2)
    :title ,title))

;; #########################################################
;; make-expandable/collapsable-element
;; ---------------------------------------------------------

;;;;;
;;;;; this functionality helps creating html elements that expand when the
;;;;; user clicks on them and that collapse again when the user clicks a
;;;;; second time. Additionally, several elements can be expanded/ collapsed
;;;;; at once with a single click on a further button.
;;;;;
;;;;; to save browser resources (memory, time), the expanded/ collapsed 
;;;;; versions of the element are kept in Lisp and only sent to the browser
;;;;; on request (using ajax).
;;;;;
;;;;; additionally, you can pass closures such as (lambda () `((div) "foo"))
;;;;; for both the expanded and collapsed version. These are then only called
;;;;; when the respective version is requested and thus it is avoided to 
;;;;; compute elements that never get expanded.
;;;;;
;;;;; see the example at the bottom of the file.
;;;;;


;; contains for each element id a list (collapsed-element expanded-element expanded?)
(defvar *expandable/collapsable-elements* (make-hash-table :test #'equal))

;; contains for each expand/collapse-all id the ids of all related elements.
(defvar *expand/collapse-all-ids* (make-hash-table :test #'equal))

(defun reset-expandable/collapsable-elements ()
  (setf *expandable/collapsable-elements* (make-hash-table :test #'equal))
  (setf *expand/collapse-all-ids* (make-hash-table :test #'equal)))

(pushnew #'reset-expandable/collapsable-elements *reset-functions*)

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
")

(define-static-js 'expand/collapse "
function staticExpand(id, expanded) {
  xhr = new AsyncXMLHttpRequest();
  xhr.id = id
  xhr.onfinish = function(xhr) { 
    var node = document.getElementById(xhr.id);
    while (node.firstChild) { node.removeChild(node.firstChild); }
    node.appendChild (document.importNode(xhr.responseXML.firstChild,true));
  }
  xhr.open('GET', id + '-' + (expanded ? 'expanded' : 'collapsed') + '.xml'); 
  xhr.send(null);
}

function expand(id) {
  staticExpand(id, true);
}

function collapse(id) {
  staticExpand(id, false);
}

function expandAll(id) {
  var nodes = document.getElementsByName(id);
  for (i=0;i<nodes.length;i++) {
    expand(nodes[i].getAttribute('id'));
  }
}

function collapseAll(id) {
  var nodes = document.getElementsByName(id);
  for (i=0;i<nodes.length;i++) {
    collapse(nodes[i].getAttribute('id'));
  }
}
")

(defun make-expandable/collapsable-element (element-id expand/collapse-all-id 
					    collapsed-element expanded-element
					    &key (expand-initially nil))
  "Adds both the expanded and the collapsed version to the hashtable and 
   packages the collapsed version into a properly named div."
  (flet ((write-static-file (element expanded?)
           (assert (and (listp element) (symbolp (caar element))))
           (setf element ;; add html namespace
                 (cons (cons (caar element)
                             (append '(:xmlns "http://www.w3.org/1999/xhtml")
                                     (cdar element)))
                       (cdr element)))
           (let ((path (merge-pathnames 
                        *static-html-output-dir*
                        (make-pathname 
                         :name (format nil "~(~a~)-~:[collapsed~;expanded~]"
                                       element-id expanded?)
                         :type "xml"))))
             (print path)
             (bordeaux-threads:make-thread 
              #'(lambda ()
                  (with-open-file (stream path :direction :output 
                                          :if-exists :supersede)
                    (princ "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>" stream)
                    (princ (render-xml element) stream)))))))
    (let ((element-id-string (string-downcase (mkstr element-id)))
          (expand/collapse-all-id-string 
           (string-downcase (mkstr expand/collapse-all-id)))
          (collapsed-element (if (and (functionp collapsed-element) 
                                      (or *static-html* (not expand-initially)))
                                 (funcall collapsed-element)
                                 collapsed-element))
          (expanded-element (if (and (functionp expanded-element) 
                                     (or *static-html* expand-initially))
                                (funcall expanded-element) expanded-element)))
      (when *static-html*
        (write-static-file expanded-element t)
        (write-static-file collapsed-element nil))
      (setf (gethash element-id-string *expandable/collapsable-elements*) 
            (list collapsed-element expanded-element))
      (setf (gethash expand/collapse-all-id-string *expand/collapse-all-ids*)
            (cons element-id-string (gethash expand/collapse-all-id-string 
                                             *expand/collapse-all-ids*)))
      `((div :id ,element-id-string :name ,expand/collapse-all-id-string)
        ,(if expand-initially expanded-element collapsed-element)))))

    
(defun make-expand/collapse-link-parameters (element-id expand? &optional title)
  "to be used when creating a <a >click</a> link for expanding/ collapsing nodes"
  `(:href ,(format nil "javascript:~a('~(~a~)');" 
		   (if expand? "expand" "collapse") element-id)
	   :title ,(or title (if expand? "expand" "collapse"))))

(defun make-expand/collapse-all-link-parameters (expand/collapse-all-id expand? &optional title)
  "can be used to create collapse-all/ expand-all links"
  `(:href ,(format nil "javascript:~a('~(~a~)');" 
		   (if expand? "expandAll" "collapseAll") expand/collapse-all-id)
	  :title ,(or title (if expand? "expand all" "collapse all"))))

(defun make-expand/collapse-all-link (expand/collapse-all-id content 
                                      &optional expand-title collapse-title)
  "A helper function that creates an <a ../> element for expanding/
   collapsing a group of elements"
  (make-expandable/collapsable-element
   (make-id) expand/collapse-all-id 
   `((a ,@(make-expand/collapse-all-link-parameters 
           expand/collapse-all-id t expand-title))
     ,content)
   `((a ,@(make-expand/collapse-all-link-parameters 
           expand/collapse-all-id nil collapse-title))
     ,content)))

(defun expand-or-collapse-node-aux (id expand?)
  "A helper function that sends the correct version of an element to the browser"
  (let* ((entry (gethash id *expandable/collapsable-elements*))
	 (value (if expand? (second entry) (first entry))))
    ;; when the stored thing is a closure, call it and cache the result
    (if (functionp value) 
	(if expand? 
	    (setf (second entry) (funcall value))
	    (setf (first entry) (funcall value))))
    (replace-element-content id (if expand? (second entry) (first entry)))))

(defun-ajax expand-or-collapse-node (id expand?-string) (*ajax-processor*)
  "Called from the html page to expand/collapse a single element"
  (expand-or-collapse-node-aux id (if (equal expand?-string "true") t nil))
  (render-xml nil))

(defun-ajax expand-or-collapse-all (id expand?-string) (*ajax-processor*)
  "Called from the html page to expand/collapse a set of elements"
  (loop for element-id in (gethash id *expand/collapse-all-ids*)
     do (expand-or-collapse-node-aux element-id (if (equal expand?-string "true") t nil)))
  (render-xml nil))


;; the example. run it and direct your browser to localhost:8000
(defun expand/collapse-example ()
  (add-element 
   (let ((expand/collapse-all-id (make-id 'expand-collapse-all-expample))
	 (element-id-1 (make-id 'expand-collapse-expample))
	 (element-id-2 (make-id 'expand-collapse-expample))
	 (element-id-3 (make-id 'expand-collapse-expample)))
     `((p) 
       ,(make-expandable/collapsable-element 
	 (make-id) expand/collapse-all-id
	 `((a ,@(make-expand/collapse-all-link-parameters 
		 expand/collapse-all-id t "custom expand all title"))
	   "expand all")
	 `((a ,@(make-expand/collapse-all-link-parameters expand/collapse-all-id nil))
	   "collapse all"))
       " "
       ;; standard example
       ((div :style "border:1px solid #aaa;display:inline-block;margin-right:10px;")
	,(make-expandable/collapsable-element 
	  element-id-1 expand/collapse-all-id
	  `((div) 
	    ((a ,@(make-expand/collapse-link-parameters element-id-1 t)) "expand")
	    ((br)) "collapsed")
	  `((div)
	    ((a ,@(make-expand/collapse-link-parameters element-id-1 nil)) "collapse")
	    ((br)) ((div :style "font-size:200%") "expanded"))))
       ;; with the same link for expand/collapse
       ((div :style "border:1px solid #aaa;display:inline-block;margin-right:10px;")
	,(make-expandable/collapsable-element 
	  element-id-2 expand/collapse-all-id
	  `((div) 
	    ((a ,@(make-expand/collapse-link-parameters element-id-2 t "custom expand title"))
	     "click!")
	    ((br)) "collapsed")
	  `((div)
	    ((a ,@(make-expand/collapse-link-parameters element-id-2 nil "custom collapse title"))
	     "click!")
	    ((br)) ((div :style "font-size:200%") "expanded"))))
       ;; with closures
       ((div :style "border:1px solid #aaa;display:inline-block;margin-right:10px;")
	,(make-expandable/collapsable-element 
	  element-id-3 expand/collapse-all-id
	  (lambda () 
	    `((div) 
	      ((a ,@(make-expand/collapse-link-parameters element-id-3 t)) "expand")
	      ((br)) "collapsed"))
	  (lambda ()
	    `((div)
	      ((a ,@(make-expand/collapse-link-parameters element-id-3 nil)) "collapse")
	      ((br)) ((div :style "font-size:200%") "expanded")))))))))


;;(expand/collapse-example)



;; #########################################################
;; html-pprint
;; ---------------------------------------------------------


(defparameter *colors* (list "#CCCCFF" "#CCFF99" "#FFFF99" "#FFCC99" "#FFCDCD" 
			     "#CCCC99" "#99CC99" "#999DDD" "#FFCC66" "#99CCCC" 
			     "#FF9966" "#99FF99" "#CCCCCC" "#AAAAFF" "#66FF99")
  "colors for highlighing symbols")

(defvar *symbol-name->color* (make-hash-table :test #'equal)
  "makes sure that all instances of the same symbol get the same background color")


(defun reset-html-pprint ()
  (setf *symbol-name->color* (make-hash-table :test #'equal)))

(pushnew #'reset-html-pprint *reset-functions*)


(defun get-color-for-symbol (symbol-name)
  "Returns for the same symbols the same color"
  (or (gethash symbol-name *symbol-name->color*)
      (setf (gethash symbol-name *symbol-name->color*) (random-elt *colors*))))


(define-js 'highlight-symbol "
function highlightSymbol(symbolName, elementId, backgroundColor) {
  var nodes = document.getElementsByName(symbolName);
  var highlight = (document.getElementById(elementId).style.backgroundColor == '');
  for (i=0;i<nodes.length;i++) {
    if (highlight) {
      nodes[i].parentNode.style.backgroundColor = backgroundColor;
      nodes[i].style.color = 'black';
    } else {
      nodes[i].parentNode.style.backgroundColor = '';
      nodes[i].style.color = '';
    }
  }
}
")

(define-static-js 'highlight-symbol (gethash 'highlight-symbol *js-definitions*))

(define-css 'pprint "
div.pprint { margin-top:0px;}
div.pprint * { font-family:Courier;font-weight:normal;font-size:9pt;line-height:10px;display:inline-block; }
div.pprint span.table { margin-top:0px; margin-bottom:0px; display:inline-table;border-collapse:collapse;}
div.pprint span.table > span { display:table-cell;vertical-align:top; margin-top:0px; margin-bottom:0px; }
")

;; This helper function was added in order to handle lists that end with . x
(defun html-length-list (lst)
  (cond ((null lst) 0)
        ((atom lst) 1)
        (t
         (1+ (html-length-list (rest lst))))))

(defun html-pprint-list-aux (lst &optional (append ""))
  "Avoids the web interface from crashing when the list contains (x . y)"
  (cond
   ((not (listp lst))
    (append (list `(((span) "&#160;.&#160;")) " ")
            (list (html-pprint-aux lst (mkstr ")" append)) "")))
   ((not (listp (rest lst)))
    (append
     (list (html-pprint-aux (car lst)) "")
     (list ""
           '((span) "&#160;.&#160;")
           " ")
     (list (html-pprint-aux (rest lst) (mkstr ")" append)) "")))
   ((null (rest lst))
    (list (html-pprint-aux (first lst) (mkstr ")" append)) ""))
   (t
    (append (list (html-pprint-aux (first lst)) " ")
            (html-pprint-list-aux (rest lst) append)))))

;; This helper method was added in order to be able to have specific
;; html-pprint-methods for special operators in fcg. e.g. ==>
(defmethod html-pprint-list ((thing list) (first-el t) &optional (append ""))
  `((span :class "table")
    ((span) "(")
    ((span)
     ,@(html-pprint-list-aux thing append))))

(defun html-pprint-aux (thing &optional (append ""))
  (cond 
   ;; "a"
   ((stringp thing) 
    `((span :class "table")
      ((td) "&quot;" ,(escape-for-html thing) "&quot;" ,append)))
   ;; 1
   ((numberp thing)
    `((span :class "table") ((td) ,thing) ,append))
   ;; 'a
   ((symbolp thing) 
    (let* ((symbol-name (symbol-name thing))
           (element-id (mkstr (make-id "s")))
           (background-color (get-color-for-symbol symbol-name)))
      `((span :class "table")
        ((span :id ,element-id :style "white-space:nowrap;"
               :onclick ,(format nil "highlightSymbol('~a','~a','~a');"
                                 (escape-for-html symbol-name)
                                 element-id background-color))
         ((a :name ,(escape-for-html symbol-name))) ;; for being xhtml 1.0 conform
         ,(if (keywordp thing) ":" "")
         ,(escape-for-html (format nil "~(~a~)" thing))
         ,(if (equal append "") ""
            `((span :style "background-color:white;") ,append))))))
   ;; '(a . b)
   ((and (consp thing) (cdr thing) (not (consp (cdr thing)))) 
    `((span :class "table")
      ((span) ((span :class "table") ((span) "(")))
      ((span) ,(html-pprint-aux (car thing)))
      ((span) ((span :class "table") ((span) "&#160;.&#160;")))
      ((span) ,(html-pprint-aux (cdr thing) (mkstr ")" append)))))
   ;; '(a b)
   ((listp thing) 
    (html-pprint-list thing (first thing) append))
   (t 
    `((span :class "table")
      ((span :style "white-space:nowrap")
       ,(let* ((*print-right-margin* 50) (*print-escape* t)
               (*print-lines* 1) (*print-pretty* t) (*print-case* :downcase))
          (escape-for-html (format nil "~w~a" thing append))))))))

(defun html-pprint (thing &key max-width)
  "Creates html code for displaying an x-expression in the web
   browser. The result lookes like coming from pprint (proper
   indention), but the layout is dynamic depending on the available
   width."
  `((div 
     :class "pprint"
     :style ,(if max-width
               (let* ((lisp-pprint-string
                       (let ((*print-right-margin* max-width)
                             (*print-pretty* t))
                         (format nil "~w" thing)))
                      (longest-line-length
                       (loop for line in (cl-ppcre:split 
                                          "\\n" lisp-pprint-string)
                             maximize (length line))))
                 (format nil "max-width:~dpx;" (* 7.2 (+ longest-line-length 1))))
               ""))
    ,(html-pprint-aux thing)))

(defun html-pprint-example ()
  (let ((some-list
         `(((> < test test (test-4 . test-5) . (test-3 "test" . test-1))
            ("foo-1" "bar-1") (foo-2 bar-2) (:foo-3 :bar-3) 
	    (,(make-symbol "FOO-4") ,(make-symbol "BAR-4")))
	   ((foo-5 . bar-5) (foo-6 . bar-6)
	    (0 1 2 3 4 5 6 7 8 9 )))))
    (clear-page)
    (add-element '((h1) "html-pprint example"))
    (add-element '((p) "Click on symbols and/or resize your browser."))
    (add-element 
     `((div :style "border:1px solid #aaa;margin-bottom:10px;") 
       ,(html-pprint some-list)))
    (add-element 
     `((table :style "margin-bottom:10px;border-collapse:collapse;")
       ((tbody) ((tr) ,@(loop for i from 1 to 3 
                              collect `((td :style "border:1px solid #aaa;") 
                                        ,(html-pprint some-list)))))))
    (add-element 
     `((div :style "border:1px solid #aaa;margin-bottom:10px;") 
       ,(html-pprint some-list :max-width 50)))
    (add-element 
     `((div :style "border:1px solid #aaa;margin-bottom:10px;") 
       ,(html-pprint some-list :max-width 80)))))

;; (html-pprint-example)

;; #########################################################
;; html-hide-rest-of-long-list
;; ---------------------------------------------------------

(defun html-hide-rest-of-long-list (list threshold draw-fn)
  "Appends the result of draw-fn to every element in list. When the
   list is longer than 'threshold' elements, then only the first
   elements are shown together with a link for expanding the rest."
  (if (> (length list) threshold)
      (append
       (mapcar draw-fn (subseq list 0 threshold))
       (list (let ((id (make-id 'list)))
               (make-expandable/collapsable-element 
                id (make-id)
                `((a ,@(make-expand/collapse-link-parameters id t))
                  "... and " ,(- (length list) threshold) " more")
                `((span)
                  ,@(mapcar draw-fn (subseq list threshold)))))))
      (mapcar draw-fn list)))


(defun html-hide-rest-of-long-list-example ()
  (let ((long-list
         (loop for i from 1 to 23 collect i)))
    (clear-page)
    (add-element '((h1) "html-hide-rest-of-long-list example"))
    (add-element 
     `((p) "long list: "
       ,@(html-hide-rest-of-long-list 
          long-list 10 
          #'(lambda (x) 
              `((span :style "background-color:#282;color:#fff;margin:4px;")
                ,x)))))))

;;(html-hide-rest-of-long-list-example)


;; #########################################################
;; make-div-with-menu
;; ---------------------------------------------------------

(define-css 'menu "
div.menu { 
  display:none;position:absolute;
  background-color:#fff;padding-right:3px;padding-left:3px;font-weight:normal;
  filter:alpha(opacity=85);-moz-opacity:0.85;-khtml-opacity: 0.85;opacity: 0.85;
}
div.normal-size-menu { height:16px;top:-17px;left:-1px;font-size:11px; }
div.big-size-menu { height:22px;top:-23px;left:-1px;font-size:18px; }
div.menu a { margin-right:2px;}
")


(define-js 'menu "
var shiftPressed = false;

document.onkeydown = function (e) {
  if (e.keyCode == 16) { shiftPressed = true; }
}
document.onkeyup = function(e) { shiftPressed = false; }

function showMenu(id) {
  var menu = document.getElementById(id);
  menu.style.display= 'inline';
  if (shiftPressed) { menu.setAttribute('class','menu big-size-menu'); } 
  else { menu.setAttribute('class','menu normal-size-menu'); }
}

function hideMenu(id) {
  if (document.getElementById(id)) { document.getElementById(id).style.display= 'none';}
}

")

(defun make-div-with-menu (&key menu-items div-attributes div-children)
  "Makes a div that shows a menu above when the mouse is over it."
  (if wi::*static-html*
      `((div ,@div-attributes) ,@div-children)
      (let ((menu-id (format nil "~(~a~)" (make-id 'menu)))
            (attributes 
             (if (find :style div-attributes)
                 (loop for (attr val) on div-attributes by #'cddr
                    if (eq attr :style)
                    append (list :style (mkstr val ";position:relative"))
                    else append (list attr val))
                 (append '(:style "position:relative") div-attributes))))
        `((div ,@attributes :onmouseover ,(mkstr "showMenu('" menu-id "');")
               :onmouseout ,(mkstr "hideMenu('" menu-id "');")
               :onclick ,(mkstr "hideMenu('" menu-id "');"))
          ((div :class "menu" :id ,menu-id)
           ,@menu-items)
          ,@div-children))))

(defun make-div-with-menu-example ()
  (add-element '((p) "move the mouse over the div"))
  (add-element 
   (make-div-with-menu 
    :menu-items '(((a :href "javascript:alert('action a');") "a")
                  ((a :href "javascript:alert('action b');") "b"))
    :div-children 
    '(((div :style "border:1px solid red;padding:3px;display:inline-block")
       "some div")))))

;;(make-div-with-menu-example)

(defun get-highlighted-element (element
                                &key
                                (string-tab "")
                                (if-string-print-as-string t)
                                (negated? nil))
  "function that returns html code for highlighting an element in the web interface.
When 'negated?' (comes from FCG-light notation), the element appears in red"
  (let* ((symbol-name (mkstr element))
         (element-id (mkstr (make-id "s")))
         (background-color (get-color-for-symbol symbol-name)))
    `((span :class "table")
      ((span :id ,element-id :style ,(if negated?
                                       "white-space:nowrap;color:#DF0101;"
                                       "white-space:nowrap;")
             :onclick ,(format nil "highlightSymbol('~a','~a','~a');"
                               symbol-name element-id background-color))
       ((a :name ,symbol-name)) ;; for being xhtml 1.0 conform
       ,(if (and (stringp element)
                 if-string-print-as-string)
          (format nil (string-append string-tab "&quot;~(~a~)&quot;") element)
          (format nil (string-append string-tab "~(~a~)") element))))))

;; #########################################################
;; make-html for t and blackboard
;; ---------------------------------------------------------

(defmethod make-html ((object t) &key &allow-other-keys)
  (labels ((simple-list? (x) 
             (if (consp x)
                 (and (simple-list? (car x)) (simple-list? (cdr x)))
                 (or (symbolp x) (numberp x) (not x) (stringp x)))))
    (if (simple-list? object)
        (html-pprint object)
        ;; This can get very slow due to the escaping!!!
        `((pre) ,(let ((*print-escape* nil)
                       (*print-right-margin* 80))
                      (cl-who:escape-string (format nil "~:w" object)))))))


(define-css 'two-col "
table.two-col { border-collapse:collapse; }
table.two-col > tbody > tr > td { padding-bottom:6px; vertical-align:top; }
table.two-col > tbody > tr > td:first-child { padding-right:10px; }
table.two-col > tbody > tr > td  h3 { margin-top:0px;}
")

(define-css 'three-col "
table.three-col { border-collapse:collapse; }
table.three-col > tbody > tr > td { padding-bottom:6px; vertical-align:top; }
table.three-col > tbody > tr > td:first-child { padding-right:10px; }
table.three-col > tbody > tr > td:second-child { padding-right:10px; }
table.three-col > tbody > tr > td  h3 { margin-top:0px;}
")

(defmethod make-html ((blackboard blackboard) &key)
  `((table :class "two-col")
    ((tbody)
     ,@(loop for x in (reverse (data-fields blackboard))
          collect `((tr)
                    ((td) ,(string-replace (format nil "~(~a~)" (car x)) "-" " "))
                    ((td) ,(make-html (cdr x))))))))

;; #########################################################
;; Hash table
;; ---------------------------------------------------------

(define-css 'hash-table "
div.hash-table { display:inline-block; margin-right: 7px; margin-bottom:3px;margin-top:3px;position:relative;border:1px solid #40241A}
div.hash-table > div.title { color:#fff;padding-left:2px;padding-right:2px;padding-top:0px;padding-bottom:1px;background-color: #24447A; }
div.hash-table > div.title a { color:#fff; font-size:9pt; font-weight:normal;}
div.hash-table-expanded { border:1px solid #40241A; }
div.hash-table-expanded > div.title { padding-left:5px;padding-right:5px;padding-top:1px;padding-bottom:2px; }
div.hash-table > table > tbody > tr > td { padding-left:5px; padding-bottom:2px;padding-top:2px;}
")

(defgeneric make-html-hash-table-title (hash-table &key)
  (:documentation "returns some html for displaying the title of a hash table"))

(defmethod make-html-hash-table-title ((hash hash-table) &key (title "Hash-table"))
  `((span) 
    ,(format nil "~a" title)))

(defmethod make-html ((hash hash-table) &key (title "Hash-table"))
  "html visualisation for hash-table"
  (let* ((element-id-1 (make-id 'hash-table))
         (hash-table-title (make-html-hash-table-title hash :title title))
         (collapsed-version
	  (lambda ()
	    `((div :class "hash-table")
	      ((div :class "title") 
	       ((a ,@(make-expand/collapse-link-parameters 
		      element-id-1 t "show hash-table"))
		,hash-table-title)))))
         (expanded-version-1
          (lambda ()
	    `((div :class "hash-table hash-table-expanded")
	      ((div :class "title") 
	       ((a ,@(make-expand/collapse-link-parameters 
                      element-id-1 nil "hide hash-table"))
		,hash-table-title))
              ((table :class "two-col")
               ((tbody)
                ((tr)
                 ((td :style "font-weight: bold;") ,(format nil "key"))
                 ((td :style "font-weight: bold;") ,(format nil "value")))
                 ,@(loop for key being the hash-keys of hash
                         using (hash-value value)
                         collect `((tr)
                                   ((td) ,(get-highlighted-element key :if-string-print-as-string nil))
                                   ((td)
                                    ,@(loop for el  in value
                                            collect `((td)
                                                      ,(get-highlighted-element el :if-string-print-as-string nil))))))))))))
    `((div :class "hash-table" :style ,(format nil "border:0px solid"))
      ,(make-expandable/collapsable-element 
        element-id-1 (make-id 'hash-table) collapsed-version
        expanded-version-1))))

;; #########################################################
;; wait-for-response
;; ---------------------------------------------------------

;; Blocks execution until the user clicks on a response button.
;; See (wait-for-response-example)

(defvar *response* nil)

(define-js 'wait-for-response "
function reply(response) {
    ajax_set_response(response);
}
")

(defun-ajax set-response (response) (*ajax-processor*)
  "Called from the html page to set *response*"
  (setf *response* response)
  (render-xml nil))

(defun make-response-link-parameters (response)
  `(:href ,(mkstr "javascript:reply('" response "');") 
	  :title ,(mkstr "reply with " response)))

(defun wait-for-response ()
  (setf *response* nil)
  (loop until *response* do (sleep 0.1))
  *response*)

(defun wait-for-response-example ()
  (clear-page)
  (add-element `((p) "Please select: " 
		 ((a ,@(make-response-link-parameters "foo")) "foo") " " 
		 ((a ,@(make-response-link-parameters "bar")) "bar")))
  (add-element `((p) "You replied: " ,(wait-for-response))))

;;(wait-for-response-example)
		 


;; #########################################################
;; store-wi-object get-wi-object
;; ---------------------------------------------------------

(defvar *wi-objects* (make-hash-table :test #'equal))

(defun reset-wi-objects ()
  (setf *wi-objects* (make-hash-table :test #'equal)))

(pushnew #'reset-wi-objects *reset-functions*)

(defun store-wi-object (object id)
  (unless *static-html*
    (setf (gethash (mkstr id) *wi-objects*) object)))

(defun get-wi-object (id-string)
  (gethash id-string *wi-objects*))

