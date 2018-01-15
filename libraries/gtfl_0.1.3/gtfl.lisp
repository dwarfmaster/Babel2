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

(export '(*gtfl-address* *gtfl-port* start-gtfl
	  gtfl-out replace-element-content append-to-element
	  reset-gtfl *reset-functions*
	  who who2s who-lambda
	  define-css define-js
          make-id-string))

;; we require the version of hunchentoot to be >= 1.1.0
(unless (string>= (asdf:component-version (asdf:find-system :hunchentoot)) "1.2.0")
  (error "Please install a hunchentoot with version >= 1.2.0"))

;; #########################################################
;; web server definition and startup
;; ---------------------------------------------------------

;; Address and port. Change this to something else when needed
(defvar *gtfl-address* "localhost")
(defvar *gtfl-port* 8000)

;; the hunchentoot server instance
(defvar *gtfl-server* nil)


;; starts the web server
(defun start-gtfl ()
  (if *gtfl-server* 
      (format t "~% ***** gtfl already running at http://~a:~d *****~%" 
	      *gtfl-address* *gtfl-port*)
      (progn
	(setf *gtfl-server* 
	      (start (make-instance 'easy-acceptor
                                    :port *gtfl-port* :address *gtfl-address*)))
        (setf (acceptor-access-log-destination *gtfl-server*) nil)
	(format t "~% ***** started gtfl at http://~a:~d *****~%" 
		*gtfl-address* *gtfl-port*))))
  

;; we opt for <a :href="foo.htm"/> instead of <a :href='foo.htm'/>
(setf *attribute-quote-char* #\")

;; we want to see when something goes wrong
(setq *catch-errors-p* nil)

;; #########################################################
;; ajax processor and dispatch table
;; ---------------------------------------------------------

(defvar *ajax-processor* (make-instance 'ajax-processor))

(setq *dispatch-table*
      (list 'dispatch-easy-handlers 
	    (create-ajax-dispatcher *ajax-processor*)))


;; #########################################################
;; cl-who shortcuts
;; ---------------------------------------------------------

;; we always use *standard-output* as the stream for writing
;; html expressions to. This way we avoid passing streams 
;; around and it is very easy to test html generating 
;; functions.

(defmacro who (&rest expressions)
  "shortcut for with-html-output"
  `(progn
     (with-html-output (*standard-output*) ,@expressions)
     nil))

(defmacro who2s (&rest expressions)
  "shortcut for with-html-output-to-string"
  `(with-html-output-to-string (*standard-output*)
     ,@expressions))

(defmacro who-lambda (&rest expressions)
  "makes an anonymous function that generates cl-who output for expression"
  `(lambda () 
     (progn (with-html-output (*standard-output*) ,@expressions) nil)))



;; #########################################################
;; define-css and define-js
;; ---------------------------------------------------------

;; collections of css and javascript code fragments that are 
;; automatically included in the head of the client html page
(defvar *css-definitions* (make-hash-table))
(defvar *js-definitions* (make-hash-table))


(defun define-css (id css)
  "adds css code fragments to the client page"
  (setf (gethash id *css-definitions*) css))

(defun define-js (id js)
  (setf (gethash id *js-definitions*) js))


;; #########################################################
;; client/ server communication
;; ---------------------------------------------------------

;; This is a list of "requests". The lisp side puts stuff into that list.
;; The client regularily polls this list and 
;; handles them in function 'processRequestQueue' (see javascript below).
(defvar *requests* nil)


;; empty the requests list when the page is reset
(defun reset-requests () (setf *requests* nil))


;; the client side script frequently polls the requests and processes them
(define-js 'request-handling "
// handles the response from lisp and then polls the next requests after 200 ms
function requestsCallBack (result) {
  for (i=0;i<result.firstChild.firstChild.childNodes.length;i++) 
  {
    var request = document.importNode(result.firstChild.firstChild.childNodes[i],true);
    switch (request.nodeName) 
    {
    case 'reset': case 'RESET':
      var content = document.getElementById('content');
      while (content.firstChild) {
        content.removeChild(content.firstChild);
      }
      break;
    case 'add-element': case 'ADD-ELEMENT':
      while (request.firstChild) {
        document.getElementById('content').appendChild(request.firstChild);
      }
      window.scrollTo(0,100000000);
      break;
    case 'replace-element-content': case 'REPLACE-ELEMENT-CONTENT':
      var id = request.getElementsByTagName('id')[0].firstChild.nodeValue;
      var content = request.getElementsByTagName('content')[0];
      var node = document.getElementById(id);
      while (node.firstChild) { node.removeChild(node.firstChild); }
      while (content.firstChild) { node.appendChild (content.firstChild) };
      break;
    case 'append-to-element': case 'APPEND-TO-ELEMENT':
      var id = request.getElementsByTagName('id')[0].firstChild.nodeValue;
      var content = request.getElementsByTagName('content')[0];
      var node = document.getElementById(id);
      while (content.firstChild) { node.appendChild (content.firstChild) };
      break;
    default:
      alert('unhandled request: ' + request.nodeName);
      break;
    }
  }

  window.setTimeout(getRequests,200);
}
    
// asynchronously polls the content of *requests* on the lisp side
function getRequests () {
  ajax_get_requests(requestsCallBack);
}
")


;; The ajax handler to get to the requests to the client
(defun-ajax get-requests () (*ajax-processor*)
  (let ((result (who2s (:requests (mapcar #'princ (reverse *requests*))))))
    (setf *requests* nil)
    result))




;; #########################################################
;; the client html page
;; ---------------------------------------------------------

(define-css 'client "
.gtfl, .gtfl td { font-size: 9pt; font-family: Helvetica Neue, Helvetica, Arial; }
.gtfl a { color: #000066; text-decoration:none; }
.gtfl a:hover {text-decoration: underline}
.gtfl a.button { font-size: 8pt;}
.gtfl hr { border:0px;color:#777;background-color:#777;height:1px;width:100%;}
")

(setf *prologue* "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>
<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
     \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")


;; the client html page
(define-easy-handler  (client-page :uri "/") ()
  (reset-gtfl)
  (with-html-output-to-string (*standard-output* nil :prologue t)
    (:html :xmlns "http://www.w3.org/1999/xhtml"
     (:head
      (:title "gtfl")
      (princ (generate-prologue *ajax-processor*))
      (:script :type "text/javascript" "//<![CDATA[ "
	       (loop for definition being the hash-values of *js-definitions*
 		  do (princ definition)) " //]]>")
      (:style :type "text/css" 
	      (loop for definition being the hash-values of *css-definitions*
		 do (write-string definition)))) 
     (:body :onLoad "window.setTimeout(getRequests,500);"
      (:div :id "content" :class "gtfl")
      (:p :class "gtfl" 
	  (:a :class "button" :href "javascript:ajax_reset();" "reset"))))))



;; #########################################################
;; how to get stuff on the client page
;; ---------------------------------------------------------

(defmacro gtfl-out (&rest expressions)
  "Adds some content to the bottom of the client page. expressions
   can be anything that is ok in cl-who's with-html-output macro."
  `(progn
     (push (who2s (:add-element ,@expressions)) *requests*)
     nil))

(defmacro replace-element-content (id &rest expressions)
  "replaces the content of the element with id (a string) by expressions"
  `(progn
     (push (who2s (:replace-element-content (:id (str ,id))
					    (:content ,@expressions)))
	   *requests*)
     nil))

(defmacro append-to-element (id &rest html-expressions)
  "appends expressions to the element with id (a string)"
  `(progn
     (push (who2s (:append-to-element (:id (str ,id))
				      (:content ,@html-expressions)))
	   *requests*)
     nil))



;; #########################################################
;; make-id-string
;; ---------------------------------------------------------

;; counters for strings
(defvar *nid-table* (make-hash-table :test #'equal))

(defun make-id-string (&optional (base "id"))
  "Creates an uniquely numbered id string"
  (format nil "~a-~d" base (if (gethash base *nid-table*)
			       (incf (gethash base *nid-table*))
			       (setf (gethash base *nid-table*) 1))))



;; #########################################################
;; resetting gtfl
;; ---------------------------------------------------------

(defvar *reset-functions* nil
  "a list of functions that are called whenever gtfl is reset,
   i.e. when the 'reset' button is clicked or when (reset-gtfl) is
   called. For resetting own stuff in these cases add your
   reset-function here, for example
   (pushnew #'my-reset-function *reset-functions*)")

(pushnew #'reset-requests *reset-functions*)


(defun reset-gtfl ()
  "Clears the content area of the client page and resets things"
  (mapcar #'funcall *reset-functions*)
  (push "<reset> </reset>" *requests*)
  nil)

;; the ajax handler for the 'reset' button
(defun-ajax reset () (*ajax-processor*)
  (reset-gtfl)
  nil)

