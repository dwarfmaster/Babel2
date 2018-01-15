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

(asdf:operate 'asdf:load-op 'gtfl)

(in-package :gtfl)

;; starting gtfl. When you want to use another address or port 
;; than localhost:8000 then please set the *gtfl-address* and 
;; *gtfl-port* variables
(start-gtfl)

;; reset the page (the same as clicking on the 'reset' in the browser)
;; this deletes all content on the client page and resets things on 
;; the lisp side
(reset-gtfl)

;; displaying stuff
;; any expressions that are ok in cl-who's with-html-output macro can go here

(gtfl-out (:h1 "hello world"))
(gtfl-out (:p "some text, " 
              (:span :style "color:red;" "and some in red."))
          (:p "and a second paragraph"))




;; replacing content of existing elements
(defparameter *element-id* nil)

(gtfl-out (:p "a paragraph, " 
              (:span :id (setf *element-id* (make-id-string)) 
                     :style "border:1px solid red"
                     "and a span as child element")))
(replace-element-content *element-id* "and " (:b "new") " span content")


;; appending content to existing elements

(gtfl-out (:p "a paragraph, " 
              (:span :id (setf *element-id* (make-id-string))
                     :style "border:1px solid red"
                     "and a span as child element")))

(append-to-element *element-id* ", and " (:b "more") " content")

;; expandable elements (see expandable-elements.lisp)
(expandable-elements-example)

;; drawing trees (see tree-drawing.lisp)
(tree-drawing-example)

;; pprinting s-expressions (see html-pprint.lisp)
(html-pprint-example)
