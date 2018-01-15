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


;; this creates the index.html in the folder below

(asdf:operate 'asdf:load-op 'gtfl)

(load (merge-pathnames (make-pathname :directory (list :relative "examples")
                                      :name "asdf-dependency-tree" :type "lisp")
                       (asdf:component-pathname (asdf:find-system :gtfl))))

(in-package :gtfl)

(define-css 'documentation "
h3 {margin-top:40px;}
div.abstract > * { margin-left:40px;}
p.description-header { margin-top:40px; margin-bottom:20px; }
.description { margin-left:40px;}
pre { background-color:#e0e0e0;padding:3px;display:table;margin-bottom:5px; }
p + pre { margin-top:-10px;}
div.example { border-left:4px solid red;padding-left:20px;margin-top:5px;margin-bottom:20px;}
a { text-decoration:none; }
a:hover, h3 {text-decoration: underline}
tt {white-space:nowrap;display:inline-block;}
")

(defun escape-id (id)
  (regex-replace-all "[/*]" id "_"))

(defun link (text &key id (tt t))
  (who (:a :href (conc "#" (escape-id (or id text)))
           (if tt (htm (:tt (princ text))) (princ text)))))

(defun contents-line (text &key id (tt t))
  (who (:li (link text :id (or id text) :tt tt))))

(defmacro description-header (type name parameters &optional result)
  `(who (:p :class "description-header" :id (escape-id ,name)
            "[" ,type "]" (:br) (:b ,name " ") (:i ,@parameters)
            ,@(when result `(" "(:tt "=>") " " (:i ,result))))))

(defmacro description (&rest content)
  `(who (:p :class "description" ,@content)))

(defun arrow ()
  (who (:tt :style "color:red" "=>")))


(defun abstract ()
  (who 
   (:div 
    :class "abstract"
    (:h3 "Abstract")
    (:p "GTFL is a graphical terminal for Common Lisp. The client is a
         html page running in a web browser and GTFL provides
         mechanisms for sending content to the client page from within
         Lisp (using "
        (:a :href "http://www.weitz.de/hunchentoot/" "HUNCHENTOOT") 
        " and "
        (:a :href "http://martin-loetzsch.de/ht-simple-ajax" 
            "HT-SIMPLE-AJAX")
        ").")
    (:p "It is meant for Lisp programmers who want to debug or
         visualize their own algorithms. Instead of printing tracing
         information to the Lisp listener (which everybody normally
         does to understand what's going on), more readable graphical
         representations of the algorithm's internals are sent to the
         GTFL client page.")
    (:p "GTFL also comes with mechanisms for visualizing
         complex (hierarchical) data or control structures. It
         provides functions for drawing trees and for hiding
         complexity in elements that expand when the user clicks on
         them.")
    (:p "Two real-life examples for an application of GTFL can be found "
        (:a :href "http://martin-loetzsch.de/gtfl/application-example-1.html" "here")
        " and "
        (:a :href "http://martin-loetzsch.de/gtfl/application-example-2.html" "here")
        ". These are debug traces of linguistic parsing and production in the "
        (:a :href "http://fcg-net.org" "Fluid Construction Grammar") 
        " (FCG) framework. Such traces help the developers FCG to
        understand and debug their programs and they help users of FCG
        to see what their linguistic rules are doing. By encapsulating
        visualizations for representations in expandable html
        elements, the complete trace fits into one browser window and
        still includes every little detail and intermediate processing
        step of the involved FCG algorithms (which would be thousands
        of pages debugging output to the listener).")
    (:p "Below is another example of using GTFL for visualizing Lisp
         data structures. It was created with a few lines of code ("
        (:a :href "examples/asdf-dependency-tree.lisp"
            "examples/asdf-dependency-tree.lisp")
        ") and shows the dependencies of an asdf system. Click
         on the black nodes to reveal details about the systems
         and click them a second time to hide the details again:")
    (:div :class "example gtfl" :id "asdf-dependencies"
          (let ((*create-static-expandable/collapsable-elements* t))
            (draw-asdf-dependency-tree :gtfl)))
    (:p "GTFL comes with a " 
        (:a :href "http://www.opensource.org/licenses/bsd-license.php"
            "BSD-style license")
        " so you can basically do with it whatever you want.")
    (:p (:span :style "color:red" "Download shortcut:") " "
        (:a :href "http://martin-loetzsch.de/gtfl/gtfl.tar.gz"
            "http://martin-loetzsch.de/gtfl/gtfl.tar.gz") "."))
   (:hr)
   (:p "Sponsored links")
   (:p (:script :type "text/javascript"
                :src "http://martin-loetzsch.de/sponsoring/text-wide.js")
       (:script :type "text/javascript"
                :src "http://martin-loetzsch.de/sponsoring/gads.js"))
   (:p (:script :type "text/javascript"
                :src "http://martin-loetzsch.de/sponsoring/image-wide.js")
       (:script :type "text/javascript"
                :src "http://martin-loetzsch.de/sponsoring/gads.js"))
   (:hr)))
         
        
    

(defun table-of-contents ()
  (who 
   (:h3 "Contents")
   (:ol 
    (contents-line "Download and installation" 
                   :id "download-and-installation" :tt nil)
    (contents-line "Browser compatibility" :id "browser-compatibility" :tt nil)
    (:li (link "The GTFL terminal" :id "the-gtfl-terminal" :tt nil)
         (:ol (contents-line "start-gtfl")
              (contents-line "*gtfl-address*")
              (contents-line "*gtfl-port*")
              (contents-line "gtfl-out")
              (contents-line "replace-element-content")
              (contents-line "append-to-element")
              (contents-line "reset-gtfl")
              (contents-line "*reset-functions*")
              (contents-line "who")
              (contents-line "who2s")
              (contents-line "who-lambda")
              (contents-line "define-css")
              (contents-line "define-js")
              (contents-line "make-id-string")))
    (:li (link "Expandable elements" :id "expandable-elements" :tt nil)
         (:ol (contents-line "make-expandable/collapsable-element")
              (contents-line "make-expand/collapse-link") 
              (contents-line "make-expand/collapse-all-link")
              (contents-line "*create-static-expandable/collapsable-elements*")))
    (:li (link "Tree drawing" :id "tree-drawing" :tt nil)
         (:ol (contents-line "draw-node-with-children")))
    (:li (link "Resizing s-expressions" :id "resizing-s-expressions" :tt nil)
         (:ol (contents-line "html-pprint")))
    (contents-line "Acknowledgements" :id "acknowledgements" :tt nil))))


(defun download-and-installation ()
  (who
   (:h3 :id "download-and-installation" "Download and installation" )
   (:p "GTFL together with examples and this documentation can be downloaded from "
       (:a :href "http://martin-loetzsch.de/gtfl/gtfl.tar.gz"
           "http://martin-loetzsch.de/gtfl/gtfl.tar.gz") 
       ". The current version is " 
       (princ (asdf:component-version (asdf:find-system :gtfl))) ".")
   (:p "GTFL directly relies on " (:a :href "http://www.weitz.de/cl-who/" "CL-WHO")
       " for html generation, "
       (:a :href "http://www.weitz.de/hunchentoot/" "HUNCHENTOOT") 
       " (version >= 1.1.0) for running the web server and "
       (:a :href "http://martin-loetzsch.de/ht-simple-ajax" 
           "HT-SIMPLE-AJAX")
       " for the asynchrounous client/server communication. And these
        libraries themselves require quite a number of other
        libraries (see the dependency
        graph " (:a :href "#asdf-dependencies" "above")
        "). Make sure you have recent versions of everything.")
   (:p "If you don't want to download all these libraries manually, you can use "
       (:a :href "http://www.quicklisp.org/" "Quicklisp") " or "
       (:a :href "http://www.cliki.net/ASDF-Install" "ASDF-INSTALL") ":")
   (:pre "(ql:quickload \"gtfl\")")
   (:pre "(asdf-install:install 'gtfl)")
   (:p "Once everything is installed, GTFL is compiled and loaded with:")
   (:pre "(asdf:operate 'asdf:load-op :gtfl)")))
 
(defun browser-compatibility ()
  (who
   (:h3 :id "browser-compatibility" "Browser compatibility")
   (:p "As of 2012, all contemporary web browsers except Internet
        Explorer work well with gtfl.")
   (:p "The output of GTFL is XHTML 1.0 Strict and CSS level 2.1
        conform as can be checked below (this page is a document
        created with GTFL):"
       (:br) "&#160;&#160;&#160;"
       (:a :href "http://validator.w3.org/check?uri=http://martin-loetzsch.de/gtfl/"
           (:img :style "border:0;width:88px;height:31px"
                 :src "valid-xhtml10.png"
                 :alt "Valid XHTML 1.0 Strict"))
       "&#160;&#160;&#160;"
       (:a :href "http://jigsaw.w3.org/css-validator/validator?uri=http://martin-loetzsch.de/gtfl/"
           (:img :style "border:0;width:88px;height:31px"
                 :src "valid-css21.gif"
                 :alt "Valid CSS level 2.1")))))

(defun the-gtfl-terminal ()
  (who
   (:h3 :id "the-gtfl-terminal" "The GTFL terminal")
   (:p "GTFL consists of two main components that are defined in "
       (:a :href "gtfl.lisp" (:i "gtfl.lisp")) 
       ": a html client page running in a web browser and a Lisp web
        server that mediates between your program and the client page.
        In order to push stuff from Lisp to the client, the Lisp side
        of GTFL maintains a &quot;request list&quot; into which the
        output routines put their content. The client page has a
        continuously running event loop that polls this list every
        200ms using asynchronous AJAX calls.")
   (:p "Here's now some basic usage examples:")
   (:pre "(" (link "start-gtfl") ")")
   (:p "This starts the web server. You should see now the client page at "
       (:a :href "http://localhost:8000" "http://localhost:8000")
       " (or the address and port you have set).")
   (:p "Now you can push any content there, for example")
   (:pre "(" (link "gtfl-out") " (:h1 &quot;hello world&quot;))")
   (:div (arrow) "This will show up in the client page:")
   (:div :class "example gtfl" (:h1 "hello world"))
   (:p "GTFL uses " (:a :href "http://www.weitz.de/cl-who/" "CL-WHO") 
       " for rendering s-expressions into XHTML. If you are not familiar
        with CL-WHO then read its documentation first.")
   (:p "More examples:")
   (:pre "(" (link "gtfl-out") " (:p &quot;some text, &quot; (:span :style &quot;color:red;&quot; &quot;and some in red.&quot;))
          (:p &quot;and a second paragraph&quot;))")
   (:div (arrow))
   (:div :class "example gtfl"
         (:p "some text, " (:span :style "color:red;" "and some in red."))
         (:p "and a second paragraph"))
   (:pre "(defparameter *element-id* nil)

(" (link "gtfl-out") " (:p &quot;a paragraph, &quot; 
              (:span :id (setf *element-id* (" (link "make-id-string") ")) :style &quot;border:1px solid red&quot;
                     &quot;and a span as child element&quot;)))")
   (:div (arrow))
   (:div :class "example gtfl"
         (:p "a paragraph, " 
             (:span :style "border:1px solid red"
                    "and a span as child element")))
   (:pre "(" (link "replace-element-content") " *element-id* &quot;and &quot; (:b &quot;new&quot;) &quot; span content&quot;)")
   (:div (arrow))
   (:div :class "example gtfl"
         (:p "a paragraph, " 
             (:span :style "border:1px solid red"
                    "and " (:b "new") " span content")))
   (:pre "(" (link "gtfl-out") " (:p &quot;a paragraph, &quot; 
              (:span :id (setf *element-id* (" (link "make-id-string") ")) :style &quot;border:1px solid red&quot;
                     &quot;and a span as child element&quot;)))")
   (:div (arrow))
   (:div :class "example gtfl"
         (:p "a paragraph, " 
             (:span :style "border:1px solid red"
                    "and a span as child element")))
   (:pre "(" (link "append-to-element") " *element-id* &quot;, and &quot; (:b &quot;more&quot;) &quot; content&quot;)")
   (:div (arrow))
   (:div :class "example gtfl"
         (:p "a paragraph, " 
             (:span :style "border:1px solid red"
                    "and a span as child element" ", and " (:b "more") " content")))
   
   (description-header "Special variable" "*gtfl-address*" nil)
   (description 
    "The address to use for the web server. Default: &quot;localhost&quot;.")
   (description-header "Special variable" "*gtfl-port*" nil)
   (description 
    "The port to use for the web server. Default: 8000.")
   (description-header "Function" "start-gtfl" nil "no values")
   (description "Starts the web server at the specified address.")
   (description-header "Macro" "gtfl-out" 
                       ((:tt "&amp;rest") " expressions") "no values")
   (description "Adds some content to the bottom of the client page.")
   (description (:i "expressions") " is something that's ok within CL-WHO's "
                (:tt "with-html-output") " macro.")
   (description "See examples above.")
   (description-header "Macro" "replace-element-content" 
                       ("id " (:tt "&amp;rest") " expressions") "no values")
   (description "Replaces the content of the element with "
               (:i "id") " (a string) by " (:i "expressions") ".")
   (description "See examples above.") 
   (description-header "Macro" "append-to-element"
                       ("id " (:tt "&amp;rest") " expressions") "no values")
   (description "Appends " (:i "expressions") " to the element with "
                (:i "id") ".")
   (description "See examples above.") 
   (description-header "Function" "reset-gtfl" nil "no values")
   (description 
    "Clears the content area of the client page and resets things on the Lisp side.")
   (description 
    "This function is called either")
   (:ul :class "description" (:li "directly,")
         (:li "when the 'reset' button on the client page was clicked,")
         (:li "when the client page is (re)loaded."))
   (description-header "Special variable" "*reset-functions*" nil) 
   (description 
    "A list of functions that are called by " (link "reset-gtfl") 
    ". If you want to reset some of your stuff in this case, add your function here: "
    (:tt "(pushnew #'my-reset-function *reset-functions*)") ".")
   (description-header "Macro" "who" ((:tt "&amp;rest") " expressions") "no values")
   (description 
    "Writes rendered html to " (:tt "*standard-output*") 
    ". This is a shortcut for " (:tt "(with-html-output (*standard-output*) &lt;expressions>)") ".")
   (description (:i "expressions") " is something that's ok within CL-WHO's "
                (:tt "with-html-output") " macro.")
   (description "Example:")
   (:pre :class "description" "GTFL> (" (link "who") " (:div :foo &quot;bar&quot; &quot;baz&quot;))
&lt;div foo=&quot;bar&quot;>baz&lt;/div>
NIL")
   (description-header "Macro" "who2s" ((:tt "&amp;rest") " expressions") "html string")
   (description 
    "Renders expression into a html string. This is a shortcut for "
    (:tt "(with-html-output-to-string (*standard-output*) &lt;expressions>)") ".")
   (description "Example:")
   (:pre :class "description" "GTFL> (" (link "who2s") " (:div :foo &quot;bar&quot; &quot;baz&quot;))
&quot;&lt;div foo=\&quot;bar\&quot;>baz&lt;/div>&quot;")
   (description-header "Macro" "who-lambda"
                       ((:tt "&amp;rest") " expressions") "anonymous function")
   (description 
    "Makes an anonymous function that writes the rendered html to " 
    (:tt "*standard-output*") ".")
   (description "Example:")
   (:pre :class "description" "GTFL> (" (link "who-lambda") ") (:div :foo &quot;bar&quot; &quot;baz&quot;))
#&lt;Anonymous Function #x300043366A4F>
GTFL> (funcall *)
&lt;div foo=&quot;bar&quot;>baz&lt;/div>
NIL")
   (description-header "Function" "define-css" ("id css") "no values")
   (description 
    "Adds css fragments to the client page. When the client page is
     created, all these code fragments are concatenated into one big
     inline css. Note that you will have to reload the client page
     when you change css definitions.")
   (description 
    (:i "id") " is an id for the code fragment. Repeated calls with
    the same id overwrite previous definitions. " (:i "css") " is 
    the css code fragment. You are responsible for adding line endings.")
   (description "Example:")
   (:pre :class "description"
        "(" (link "define-css") " 'foo &quot;
div.foo {font-weight:bold;}
&quot;)")
   (description-header "Function" "define-js" ("id js") "no values")
   (description 
    "The same as " (link "define-css") " above, but for javascript code.")
   (:pre :class "description" "(" (link "define-js" ) " 'bar &quot;
function bar () { return (1 + 1); }
&quot;)")
   (description-header "Function" "make-id-string" 
                       ((:tt "&amp;optional") " base") "string")
   (description "Creates an uniquely numbered id string that can be used as 
                 an id for html elements.")
   (description (:i "base") " is the prefix of the id (default: &quot;id&quot;).")
   (description "Example:")
   (:pre :class "description" "GTFL> (" (link "make-id-string") ")
&quot;id-1&quot;
GTFL> (" (link "make-id-string") ")
&quot;id-2&quot;
GTFL> (" (link "make-id-string") " &quot;foo&quot;)
&quot;foo-1&quot;
GTFL> (" (link "make-id-string") " &quot;foo&quot;)
&quot;foo-2&quot;")))


(defun expandable-elements ()
  (who
   (:h3 :id "expandable-elements" "Expandable elements")
   (:p "The file " (:a :href "expandable-elements.lisp"
                       (:i "expandable-elements.lisp")) " contains
        functionality to create html elements that expand when the
        user clicks on them and that collapse again when they are
        clicked a second time. Furthermore, several elements can be
        expanded/ collapsed at once using another button.")
   (:p "In order to save browser resources (memory, time), the
        expanded and collapsed version of each element are kept in
        Lisp and are only sent to the browser when needed: the version
        that is initally sent to the client contains only the
        collapsed version and when the expand link is clicked, it is
        replaced with the expanded version stored on the lisp side.")
   (:p "Example:")
   (:pre "(" (link "gtfl-out") " 
 (:div 
   (let ((expand/collapse-all-id (" (link "make-id-string") "))
         (" (link "*create-static-expandable/collapsable-elements*") " t))
         (" (link "make-expandable/collapsable-element") " 
          (" (link "make-id-string") ") expand/collapse-all-id
          (" (link "who2s") " (" (link "make-expand/collapse-all-link") " expand/collapse-all-id t nil &quot;expand all&quot;))
          (" (link "who2s") " (" (link "make-expand/collapse-all-link") " expand/collapse-all-id nil nil &quot;collapse all&quot;)))
         (loop repeat 3
            for element-id = (" (link "make-id-string") ")
            do (htm (:div :style &quot;border:1px solid #aaa;display:inline-block;margin-left:10px;&quot;
                          (" (link "make-expandable/collapsable-element") " 
                           element-id expand/collapse-all-id
                           (" (link "who2s") " (:div (" (link "make-expand/collapse-link") " element-id t nil &quot;expand&quot;)
                                        (:br) &quot;collapsed&quot;))
                           (" (link "who2s") " (:div (" (link "make-expand/collapse-link") " element-id nil nil &quot;collapse&quot;) 
                                        (:br) (:div :style &quot;font-size:150%&quot; &quot;expanded&quot;))))))))))")
   (:div (arrow))
   (:div 
    :class "example gtfl"
    (let ((expand/collapse-all-id (make-id-string))
          (*create-static-expandable/collapsable-elements* t))
      (make-expandable/collapsable-element 
       (make-id-string) expand/collapse-all-id
       (who2s (make-expand/collapse-all-link expand/collapse-all-id t 
                  nil "expand all"))
       (who2s (make-expand/collapse-all-link expand/collapse-all-id nil
                  nil "collapse all")))
      (loop repeat 3
         for element-id = (make-id-string)
         do (htm (:div 
                  :style "border:1px solid #aaa;display:inline-block;margin-left:10px;"
                  (make-expandable/collapsable-element 
                   element-id expand/collapse-all-id
                   (who2s (:div (make-expand/collapse-link element-id t nil "expand")
                                (:br) "collapsed"))
                   (who2s (:div (make-expand/collapse-link element-id nil nil "collapse") 
                                (:br) (:div :style "font-size:150%" "expanded")))))))))
   (description-header 
    "Function" "make-expandable/collapsable-element"
    ("element-id expand/collapse-all-id collapsed-element expanded-element " 
     (:tt "&amp;key") " expand-initially") "no values")
   (description 
    "Creates an element that allows to switch between an expanded
     and a collapsed version.")
   (description
    (:i "element-id")
    " (a string) is the id given to the element so that
     function " (link "make-expand/collapse-link") " can reference it. "
    (:i "expand/collapse-all-id") 
    " (a string) is the id of a group of elements that can be
    expanded/collapsed at the same time (see "
    (link "make-expand/collapse-all-link") ").")
   (description 
    (:i "collapsed-element") " and " (:i "expanded-element")
    " are the collapsed and expanded version of the element. They can
       be either an expression that evaluates to a html string, e.g. "
    (:tt (esc "\"<div foo/>\"")) " or "
    (:tt "(" (link "who2s") " :div &quot;foo&quot;)") ", or an anonymous function
      that writes an html string, e.g. " 
    (:tt "(" (link "who-lambda") " (:div &quot;foo&quot;))") " or "
    (:tt "#'(lambda () (princ &quot;&lt;div foo/>&quot;))")
    ". In the latter case the expanded version only gets computed when
       requested by the client, which avoids unneccessary computation.")
   (description 
    "When " (:i "expand-initially") " is " (:tt "t") " then the
       expanded version is shown initially.")
   
   (description-header 
    "Macro" "make-expand/collapse-link"
    ("element-id expand? title " (:tt "&amp;rest") " content") "no values")
   (description "Makes a link for expanding/collapsing an element.")
   (description (:i "element-id") " is the id of the element to expand
     or collapse. When " (:i "expand?") " is " (:tt "t") ", then the
     element gets expanded, otherwise collapsed. " (:i "title") " is
     the title of the link (shown when the mouse is over the
     link). When nil, then &quot;expand&quot; or &quot;collapse&quot;
     are used. " (:i "body") " is the content of the link (a cl-who
     expression).")
   (description "Example:")
   (:pre :class "description"
         "GTFL> (" (link "make-expand/collapse-link") " &quot;foo&quot; t nil &quot;expand&quot;)
&lt;a href=&quot;javascript:expand('foo');&quot; title=&quot;expand&quot;>expand&lt;/a>
NIL
GTFL> (" (link "make-expand/collapse-link") " &quot;foo&quot; nil &quot;click here to collapse&quot; (:b &quot;collapse&quot;))
&lt;a href=&quot;javascript:collapse('foo');&quot; title=&quot;click here to collapse&quot;>&lt;b>collapse&lt;/b>&lt;/a>
NIL")
   
   (description-header
    "Macro" "make-expand/collapse-all-link"
    ("expand/collapse-all-id expand? title " (:tt "&amp;rest") " content") "no values")
   (description "Makes a link for expanding/collapsing a group of elements.")
   (description (:i "expand/collapse-all-id") "is the id of the
     element group to expand/collapse. All other parameters as
     above.")

   (description-header 
    "Special variable" "*create-static-expandable/collapsable-elements*" nil)
   (description "When this is set to " (:tt "t") ", both the expanded
     and collapsed version will be embedded in the html code (one
     visible and the other hidden). This makes the html code
     bigger (and thus rendering slower), but allows to save generated
     html pages, with the expand/collapse functionality still working
     when not connected to the web server. Note that in the example
     above this was also set to " (:tt "t") ", because otherwise the
     example would not work in this static html page that is not
     connected to the lisp server.")))


(defparameter *example-tree* '("top node."
                               ("child node one with three children" 
                                ("first out of three children") 
                                ("second out of three children")
                                ("third out of three children"))
                               ("child node two with one child" 
                                ("very long text. very long text. very long text. very long text. very long text. very long text. very long text."))))
   
(defun draw-node (string) 
  (who 
   (:div :style "padding:4px;border:1px solid #888;margin-top:4px;margin-bottom:4px;background-color:#eee;"
         (princ string))))

(defun draw-tree* (tree) 
  (draw-node-with-children 
   (who-lambda (draw-node (car tree)))
   (mapcar #'(lambda (x) 
               (who-lambda (draw-tree* x))) (cdr tree))))

(defun tree-drawing ()
  (who 
   (:h3 :id "tree-drawing" "Tree drawing")
   (:p "In " (:a :href "tree-drawing.lisp" (:i "tree-drawing.lisp"))
       " there is a function for recursively drawing trees. It takes
       html expressions for drawing a node and its children (which can
       be trees themselves) and connects them with horizontal and
       vertical lines. The decision how to layout the tree (i.e. how
       wide to draw each node) is left to the html rendering engine of
       the web browser.")
   (:p "Examples (resize the browser window to see the dynamic layout
        in action):")
   (:pre "(defparameter *example-tree* 
  '(&quot;top node.&quot;
    (&quot;child node one with three children&quot; 
     (&quot;first out of three children&quot;) (&quot;second out of three children&quot;) (&quot;third out of three children&quot;))
    (&quot;child node two with one child&quot; 
     (&quot;very long text. very long text. very long text. very long text. 
       very long text. very long text. very long text.&quot;))))

(defun draw-node (string) 
  (" (link "who") " 
   (:div :style &quot;padding:4px;border:1px solid #888;margin-top:4px;margin-bottom:4px;background-color:#eee;&quot;
         (princ string))))


(defun draw-tree* (tree) 
  (" (link "draw-node-with-children") " 
   (" (link "who-lambda") " (draw-node (car tree)))
   (mapcar #'(lambda (x) (" (link "who-lambda") " (draw-tree* x))) (cdr tree))))

(" (link "gtfl-out") " (draw-tree* *example-tree*))
")
   (:div (arrow))
   (:div :class "example gtfl" (draw-tree* *example-tree*))
   (:table 
    :cellpadding "0" :cellspacing "0"
    (:tr 
     (:td "With parameters " (:tt ":right-to-left t :color &quot;green&quot; :style &quot;dotted&quot;") 
              " " (arrow)
          (:div 
           :class "example gtfl"
           (defun draw-tree* (tree) 
             (draw-node-with-children 
              (who-lambda (draw-node (car tree)))
              (mapcar #'(lambda (x) (who-lambda (draw-tree* x))) (cdr tree))
              :right-to-left t :color "green" :style "dotted"))
           (draw-tree* *example-tree*)))
     (:td 
      :style "padding-left:25px"
      "With parameters " (:tt ":color &quot;#33a&quot; :line-width &quot;2px&quot;")
      " " (arrow)
      (:div 
       :class "example gtfl"
       (defun draw-tree* (tree) 
         (draw-node-with-children 
          (who-lambda (draw-node (car tree)))
          (mapcar #'(lambda (x) (who-lambda (draw-tree* x))) (cdr tree))
          :color "#33a" :line-width "2px"))
       (draw-tree* *example-tree*)))
     ))
  (description-header 
   "Function" "draw-node-with-children" 
   ("node children  " (:tt "&amp;key") " right-to-left color width line-width style")
    "no values")
  (description 
   "A function for recursively drawing trees in html. It draws a node
with connections to it's children (which can be trees themselves).")
  (description 
    (:i "node") " is a parameterless function that creates the html
    code for the parent node. " (:i "children") " is a list of
    functions for drawing the children of the node. The reason to use
    closures instead of for example strings here is both
    efficiency (no intermediate structures are built) and to allow for
    re-ordering of elements (when the tree is drawn from right to left.")
  (description 
   "When " (:i "right-to-left") " is " (:tt "t") ", then the top node
    of the tree is drawn at the right. " (:i "color") " sets the color
    of the connecting lines (e.g. &quot;#ff0000&quot; or
    &quot;red&quot;, default is &quot;#888&quot;), " (:i "width") "
    the the width of the horizontal connectors (e.g. &quot;25px&quot;,
    default is &quot;10px&quot;), " (:i "line-width") " the the
    thickness of the lines (e.g. &quot;3px&quot;, default is
    &quot;1px&quot;) and style the line style (&quot;solid&quot;,
    &quot;dotted&quot; or &quot;dashed&quot;, default is &quot;solid&quot;).")))


(defun resizing-s-expressions ()
  (who 
   (:h3 :id "resizing-s-expressions" "Resizing s-expressions")
   (:p "File " (:a :href "html-pprint.lisp" (:i "html-pprint.lisp"))
       " provides the function " (link "html-pprint") " to display
        s-expressions in html. This could of course be done
        with " (:tt "(:pre (pprint x))")
        ", but this has the disadvantage that the result will have a
        fixed width. " (link "html-pprint") " produces output that is
        very similar to the one of " (:tt "pprint") " but that also
        dynamically resizes depending on the available width (while 
        maintaining proper indentation).")
   (:p "Examples (resize browser window to see the dynamic relayout
   and click on the symbols to highlight other symbols with the same
   name):")
   (:pre "(defparameter *example-list*
  `(((&quot;foo-1&quot; &quot;bar-1&quot;) (foo-2 bar-2) (:foo-3 :bar-3) 
     (,(make-symbol &quot;FOO-4&quot;) ,(make-symbol &quot;BAR-4&quot;))
     (foo-5 . bar-5))
    ((0 1 2 3 4 5 6 7 8 9) ,(asdf:find-system :gtfl))))

(" (link "gtfl-out") " (:div :style &quot;border:1px solid #aaa;&quot; (" 
               (link "html-pprint") " *example-list*)))")
   (:div (arrow))
   (defparameter *example-list*
     `((("foo-1" "bar-1") (foo-2 bar-2) (:foo-3 :bar-3) 
        (,(make-symbol "FOO-4") ,(make-symbol "BAR-4"))
        (foo-5 . bar-5))
       ((0 1 2 3 4 5 6 7 8 9) ,(asdf:find-system :gtfl))))
   (:div :class "example gtfl"
         (:div :style "border:1px solid #aaa;" (html-pprint *example-list*)))
   (:pre "(" (link "gtfl-out") "
 (:table :style &quot;border-collapse:collapse;&quot;
         (:tr (loop for i from 1 to 3 
                 do (htm (:td :style &quot;border:1px solid #aaa;&quot;
                              (" (link "html-pprint") " *example-list*)))))))
")
   (:div (arrow))
   (:div :class "example gtfl"
         (:table :style "border-collapse:collapse;"
                 (:tbody (:tr (loop for i from 1 to 3 
                                 do (htm (:td :style "border:1px solid #aaa;"
                                              (html-pprint *example-list*))))))))

   (:div "with " (:tt ":max-width 50") " " (arrow))
   (:div :class "example gtfl"
         (:div :style "border:1px solid #aaa;display:inline-table"
               (html-pprint *example-list* :max-width 50)))
   (:div "with " (:tt ":max-width 100") " " (arrow))
   (:div :class "example gtfl"
         (:div :style "border:1px solid #aaa;display:inline-table"
               (html-pprint *example-list* :max-width 100)))

   (description-header 
    "Function" "html-pprint"
    ("thing " (:tt "&amp;key") " max-width") "no values")
   (description
    "Displaying an x-expression in the html. The result lookes like
     coming from pprint (proper indention), but the layout is dynamic
     depending on the available width.")
   (description 
    (:i "thing") " is the thing to display. " (:i "max-width") "limits
    the width of the result (in characters). When " (:tt "nil") ", then 
    the full available width is used")))
        
(defun acknowledgements ()
  (who
   (:h3 :id "acknowledgements" "Acknowledgements")
   (:p "GTFL was initially developed as part of the Babel2 framework ("
       (:a :href "http://emergent-languages.org/" "http://emergent-languages.org/")
       ").")
   (:p (:a :href "http://arti.vub.ac.be/~jorisb/" "Joris Bleys") " and "
       (:a :href "http://arti.vub.ac.be/~pieter/" "Pieter Wellens") 
       " have helped a lot in improving the system by finding bugs and
       suggesting many of its current features.")
   (:p "This page was created with GTFL itself (see "
       (:a :href "examples/index-html.lisp" "examples/index-html.lisp")
       "). The layout and structure is heavily inspired by (or
       directly copied from) "
       (:a :href "http://weitz.de/documentation-template/"
           "DOCUMENTATION-TEMPLATE") ".")
   (:p "Last change: "
       (let ((time (multiple-value-list (get-decoded-time))))
         (format t "~a/~2,'0d/~2,'0d ~2,'0d:~2,'0d:~2,'0d"
                 (sixth time) (fifth time) (fourth time)
                 (third time) (second time) (first time)))
       " by " (:a :href "http://martin-loetzsch.de/" "Martin Loetzsch"))
   (:hr)
   (:p "Sponsored links")
   (:p (:script :type "text/javascript"
                :src "http://martin-loetzsch.de/sponsoring/links-wide.js")
       (:script :type "text/javascript"
                :src "http://martin-loetzsch.de/sponsoring/gads.js"))
   (:p (:script :type "text/javascript"
                :src "http://martin-loetzsch.de/sponsoring/image-wide.js")
       (:script :type "text/javascript"
                :src "http://martin-loetzsch.de/sponsoring/gads.js"))
   (:script :src "http://www.google-analytics.com/ga.js" 
            :type "text/javascript")
   (:script :type "text/javascript" "
try { var pageTracker = _gat._getTracker('UA-12372300-1'); pageTracker._trackPageview();} catch(err) {}
")))






(defparameter *target* (merge-pathnames 
                        (asdf:component-pathname (asdf:find-system :gtfl))
                        (make-pathname :name "index" :type "html")))


(with-open-file (*standard-output* *target*  :direction :output 
                                   :if-exists :supersede )
  (with-html-output (*standard-output* nil :prologue t)
    (:html 
     :xmlns "http://www.w3.org/1999/xhtml"
     (:head
      (:title "GTFL - A Graphical Terminal For Lisp" )
      (:script :type "text/javascript" "//<![CDATA[ "
               (loop for definition being the hash-values of *js-definitions*
                  do (princ definition)) " //]]>")
      (:style :type "text/css" 
              (loop for definition being the hash-values of *css-definitions*
                 do (write-string definition))))
     (:body    
      (:h1 "GTFL - A Graphical Terminal For Lisp")
      
      (abstract)
      (table-of-contents)
      (download-and-installation)
      (browser-compatibility)
      (the-gtfl-terminal)
      (expandable-elements)
      (tree-drawing)
      (resizing-s-expressions)
      (acknowledgements)))))


;; on macosx this also opens the page in the default browser
(asdf:run-shell-command (format nil "open ~a" *target*))













;; for browser tests with http://browsershots.org 
(defparameter *target* (merge-pathnames 
                        (asdf:component-pathname (asdf:find-system :gtfl))
                        (make-pathname :name "browser-test" :type "html")))


(with-open-file (*standard-output* *target*  :direction :output 
                                   :if-exists :supersede )
  (with-html-output (*standard-output* nil :prologue t)
   (:html 
    :xmlns "http://www.w3.org/1999/xhtml"
    (:head
     (:title "gtfl browser test")
     (:script :type "text/javascript" "//<![CDATA[ "
              (loop for definition being the hash-values of *js-definitions*
                 do (princ definition)) " //]]>")
     (:style :type "text/css" 
             (loop for definition being the hash-values of *css-definitions*
                do (write-string definition))))
    (:body    
     (:h1 "gtfl browser test")
     (:p "Try this page in different browsers or with "
         (:a :href "http://browsershots.org" "http://browsershots.org") ".")
     (:p "Tree drawing: You should see black nodes with white text and black
          horizontal and vertical lines between the nodes:")
     (:div :class "example gtfl" 
           (let ((*create-static-expandable/collapsable-elements* t))
             (draw-asdf-dependency-tree :gtfl)))
     (:p "Resizing S-Expressions: The output should look like proper
          output from " (:tt "pprint") ". The first box should occupy
          the whole screen width, the second table too, the third should
          be very small and the last double the width of the fourth.")
     (:div :class "example gtfl" (html-pprint-example))
     ))))


;;(asdf:run-shell-command (format nil "open ~a" *target*))


