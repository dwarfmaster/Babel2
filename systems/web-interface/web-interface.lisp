;;;; 
;;;; An easy interface to display stuff in a web browser
;;;;
;;;; This webserver is automatically started when the asdf system is loaded.
;;;;
;;;; You can add your own page handlers or use the default one at 
;;;; http://localhost:8000/. That one frequently pulls the content of the 
;;;; *instructions* global variable and does something according to what's there.
;;;; 

(in-package :web-interface)

(export '(make-html
          clear-page *no-reset-button*
          define-wi-data
          define-css define-css-link 
          define-js define-js-library
          add-element replace-element-content
          append-to-element
          create-static-html-page
          create-static-html-page-webservice))


;; #########################################################
;; make-html
;; ---------------------------------------------------------

(defgeneric make-html (object &key &allow-other-keys)
  (:documentation "Makes html code from object. The result is an
                   s-expression representation of an html element
                   representing the object. You can turn this
                   representation into actual xhtml by
                   calling (render-xml result) on it."))


;; #########################################################
;; reset functions
;; ---------------------------------------------------------

;; a list of functions that are called whenever the web-interface is reset
(defvar *reset-functions* nil)

;; #########################################################
;; define-wi-data
;; ---------------------------------------------------------

;; the data collection (data resetable will be reset any time clear-page is called)
(defvar *data-resetable* (make-hash-table :test 'equalp))
(defvar *data* (make-hash-table :test 'equalp))

;; use this function to get your data published
(defun define-wi-data (id data &key (format :json) resetable)
  (if resetable
    (setf (gethash (downcase (mkstr id)) *data-resetable*) (cons format data))
    (setf (gethash (downcase (mkstr id)) *data*) (cons format data))))

(defun handle-wi-data-request ()
  (let* ((script-name (script-name* *request*))
         (data-name (string-trim "/" (subseq script-name
                                             (length "/data"))))
         (data (or (gethash data-name *data*)
                   (gethash data-name *data-resetable*))))
    (unless data
      (error "Error in handle-wi-data-request: no such data: ~A" data))
    
    (setf (reply-external-format*) hunchentoot::+utf-8+)
    (if (eq (car data) :json)
      (setf (content-type*) "application/json; charset=\"utf-8\"")
      (setf (content-type*) "text/xml; charset=\"utf-8\""))
    (no-cache)
    (cdr data)))

(defun reset-wi-data ()
  (setf *data-resetable* (make-hash-table :test 'equalp)))

(pushnew #'reset-wi-data *reset-functions*)

(define-wi-data 'test "[{test : 'test'}]")

;; #########################################################
;; definition of the web server and startup
;; ---------------------------------------------------------

;; we want to see if something goes wrong!
(setq *catch-errors-p* nil)

;; The address is http://localhost:8000/ or, if you add something like
;; (defparameter *web-server-host-address* "my-machine")
;; to your init-babel-user.lisp, http://my-machine:8000/
(defvar *address* (if (boundp 'cl-user::*web-server-host-address*)
                    (eval 'cl-user::*web-server-host-address*)
                    #+:windows "127.0.0.1" ;; on windows localhost doesn't seem to resolve to 127.0.0.1
                    #-:windows "localhost" 
                    ))

;; address and port
(defvar *port* (if (boundp 'cl-user::*web-server-port*)
                   (eval 'cl-user::*web-server-port*)
                   8000))
(defvar *my-server* nil)

(defun start-web-interface (&key (port *port*) (address *address*))
  (if *my-server* 
      (format t "~% ***** web interface already running at http://~a:~d *****~%"
              *address* *port*)
      (progn
	(setf *my-server* 
              (start (make-instance 'easy-acceptor 
                                    :port port :address address
                                    #+(and ccl :windows) :read-timeout #+(and ccl :windows) nil ;; this only applies to windows
                                    #+(and ccl :windows) :write-timeout #+(and ccl :windows) nil
                                    :document-root (babel-pathname :directory '("systems" "web-interface"))
                                    :message-log-destination nil
                                    :access-log-destination nil)))
	(format t "~% ***** started web interface at http://~a:~d *****~%"
                *address* *port*))))
  
;; automatically start the server when the asdf system is loaded
(when (and cl-user::*automatically-start-web-interface*
           (not *my-server*))
  (handler-case (start-web-interface) 
    (t (error) 
      (declare (ignore error))
      (format t "~% ***** Could not start web-interface, probably
      address already in use. Try setting *port* to different value."))))


;; ajax function interface
(defparameter *ajax-processor* (make-instance 'ajax-processor))

;; handling different requests
(setq *dispatch-table*
      (list 'dispatch-easy-handlers
	    (create-folder-dispatcher-and-handler 
	     "/Babel2/" (babel-pathname))
	    (create-folder-dispatcher-and-handler 
	     "/Babeldocs/" (babel-pathname :directory '(:up "Babeldocs")))
            (create-prefix-dispatcher "/data" #'handle-wi-data-request)
	    (create-ajax-dispatcher *ajax-processor*)
	    (create-static-file-dispatcher-and-handler 
	     "/favicon.ico" (babel-pathname 
                             :directory '("systems" "web-interface")
                             :name "favicon" :type "ico") "image/png")))

;; #########################################################
;; define-css
;; ---------------------------------------------------------

;; a collection of different css definitions
(defvar *css-definitions* (make-hash-table))

;; use this function to get your css sent to the client
(defun define-css (id css)
  (setf (gethash id *css-definitions*) css))

(defun get-combined-css-definitions ()
  `((style :type "text/css") 
    ,@(loop for definition being the hash-values of *css-definitions*
         collect definition)))


;; #########################################################
;; define-css-links
;; ---------------------------------------------------------

;; a collection of different css link definitions
(defvar *css-link-definitions* (make-hash-table))

;; use this function to get your css links sent to the client
(defun define-css-link (id url)
  (setf (gethash id *css-link-definitions*) url))

(defun get-combined-css-link-definitions ()
  (loop for url being the hash-values of *css-link-definitions* 
     collect `((link :rel "stylesheet" :href ,url))))

;; #########################################################
;; define-javascript
;; ---------------------------------------------------------

;; a collection of different javascript code blocks
(defparameter *js-libraries* nil)

;; use this function to get your javascript libraries loaded by the client
(defun define-js-library (id url)
  (let ((found (assoc id *js-libraries*)))
    (if found 
        (setf (rest found) url)
        (setf *js-libraries* (append *js-libraries* (list (cons id url)))))))

(defun get-combined-js-library-definitions ()
  (loop for (nil . url) in  *js-libraries*
        collect 
        `((script :type "text/javascript" :src ,url))))

;; a collection of different javascript code blocks
(defvar *js-definitions* (make-hash-table))

;; use this function to get your javascript code sent to the client
(defun define-js (id js)
  (setf (gethash id *js-definitions*) js))

(defun get-combined-js-definitions ()
  `((script :type "text/javascript") "
//<![CDATA[
" ,@(loop for definition being the hash-values of *js-definitions*
          collect definition) "
//]]>"))

;; #########################################################
;; client/server communication
;; ---------------------------------------------------------

;; This is a list of "requests". The lisp side puts stuff into that list.
;; The client regularily polls this list and 
;; handles them in function 'processRequestQueue' (see javascript below).
(defvar *requests* nil)


;; empty the requests list when the page is reset
(defun reset-requests () (setf *requests* nil))

(pushnew #'reset-requests *reset-functions*)


;;; The ajax handler to get to the instructions
(defun-ajax get-requests () (*ajax-processor*)
  (let ((result nil))
    #+(or mcl openmcl) (ccl:without-interrupts 
			 (setf result *requests*
			       *requests* nil))
    #-(or mcl openmcl) (setf result *requests*
			     *requests* nil)
    (render-xml (append (list '(requests)) (reverse result)))))

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
        var firstChild = request.firstChild;
        document.getElementById('content').appendChild(firstChild);
      //content_changed(firstChild);
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

// start of the event loop
function initializeRequestsQueue () {
  window.setTimeout(getRequests,500);
}
")

;; #########################################################
;; the main page
;; ---------------------------------------------------------

(define-css 'main "
body, td { font-size: 9pt; font-family: Helvetica Neue, Helvetica, Arial; }
a { color: #000066; text-decoration:none; }
a:hover {text-decoration: underline}
a.button { font-size: 8pt;}
hr { border:0px;color:#777;background-color:#777;height:1px;width:100%;}
")

(defvar *no-reset-button* nil)

;; the main page
(define-easy-handler (main-page :uri "/") ()
  (render-xml
   `((html :xmlns "http://www.w3.org/1999/xhtml")
     ((head)
      ((title) "Babel web interface")
      ((link :rel "icon" :href "/favicon.ico" :type "image/png"))
      ,(generate-prologue *ajax-processor*)
      ,@(get-combined-js-library-definitions)
      ,@(get-combined-css-link-definitions)
      ,(get-combined-js-definitions)
      ,(get-combined-css-definitions))
     ((body :onLoad "window.setTimeout(getRequests,500);")
      ((div :id "content"))
      ,@(unless *no-reset-button*
                '(((p) ((a :class "button" :href "javascript:ajax_reset();") 
                        "reset"))))))))


;; #########################################################
;; how to get stuff on the main page
;; ---------------------------------------------------------

(defvar *static-html* nil)
(defvar *static-elements* nil)

(defun add-element (element)
  "Adds an element to the content area of the main page. Element
   should be an s-expression notation of an html node
   or a list thereof"
  (if (symbolp (caar element))
    (progn
      (when *static-html* (push element *static-elements*))
      (push `((add-element) ,element) *requests*))
    (mapcar 'add-element element))
  t)

(defun replace-element-content (id element)
  (push `((replace-element-content) 
	  ((id) ,id) 
	  ((content) ,element))
	*requests*)
  t)

(defun append-to-element (id element)
  (push `((append-to-element) 
	  ((id) ,id) 
	  ((content) ,element))
	*requests*)
  t)

(defun clear-page ()
  "Clears the content area of the main page and resets things"
  (mapcar #'funcall *reset-functions*)
  (push `((reset)) *requests*))

(defun-ajax reset () (*ajax-processor*)
  (clear-page)
  (render-xml nil))


;; #########################################################
;; creating static web demos
;; ---------------------------------------------------------

(defvar *static-html-output-dir* nil)

(defvar *static-js-definitions* (make-hash-table))


;; static js code
(defun define-static-js (id js)
  (setf (gethash id *static-js-definitions*) js))

(defun get-combined-static-js-definitions ()
  `((script :type "text/javascript") "
//<![CDATA[
" ,@(loop for definition being the hash-values of *static-js-definitions*
         collect definition) "
//]]>"))

;; static js libraries (use this only with libraries that cannot be
;; stored in the code of the web page (e.g. google graph)
(defvar *static-js-libraries* (make-hash-table))

(defun define-static-js-library (id js)
  (setf (gethash id *static-js-libraries*) js))

(defun get-combined-static-js-libraries ()
  (loop for url being the hash-values of *static-js-libraries*
        collect 
        `((script :type "text/javascript" :src ,url))))


(defparameter *AsyncXMLHttpRequest.js*
  (babel-pathname :directory '("systems" "web-interface")
                  :name "AsyncXMLHttpRequest" :type "js"))

(defun write-static-page (stream title)
  (princ "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>" stream)
  (princ (render-xml
          `((html :xmlns "http://www.w3.org/1999/xhtml")
            ((head)
             ((meta :charset "utf-8"))
             ((title) ,title)
             ((script :src "AsyncXMLHttpRequest.js"))
             ,@(get-combined-static-js-libraries)
             ,(get-combined-static-js-definitions)
             ,(get-combined-css-definitions))
            ((body)
             ,@(reverse *static-elements*))))
         stream))

(defmacro create-static-html-page (title &body body)
  "Wraps some code that sends output to the web interface to create a
   static page that can be viewed without a lisp running in the
   background."
  `(let* ((*static-html-output-dir*
           (babel-pathname 
            :directory 
            (append (or *static-html-output-dir* (list ".tmp"))
                    (list (multiple-value-bind (sec min hour day month year)
                              (decode-universal-time (get-universal-time))
                            (format nil "~d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-html" 
                                    year month day hour min sec))))))
          (html-file (merge-pathnames 
                      *static-html-output-dir*
                      (make-pathname :name "index" :type "html")))
          (js-file (make-pathname :directory (pathname-directory
                                              *static-html-output-dir*)
                                  :name "AsyncXMLHttpRequest" :type "js")))
     (setf *static-elements* nil)
     (clear-page)
     (ensure-directories-exist *static-html-output-dir*)
     (print *static-html-output-dir*)
     (utils:run-prog  "cp" :args (list (mkstr *AsyncXMLHttpRequest.js*)
                                       (mkstr js-file)))
     (print js-file)
     (let ((*static-html* t))
       ,@body)
     (with-open-file (stream html-file :direction :output :if-exists :supersede)
       (write-static-page stream ,title)
       (force-output stream))
     (print html-file) (princ #\newline)
     (run-prog "open" :args (list (mkstr html-file)))
     html-file))



