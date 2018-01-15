
(in-package :web-interface)

(export 'render-xml)

;;;;; render-xml turns s-expressions such as
;;;;; '((div :attribute "value") "foo")
;;;;; into
;;;;; <div attribute="value">foo</div>
;;;;;

(defgeneric render-xml-aux (element stream)
  (:documentation "renders s-expressions into xml"))

(defmethod render-xml-aux ((element t) stream)
  (declare (ignore stream))
  (error (let ((*print-pretty* t) (*print-lines* 5))
	   (format nil "Objects of type ~(~a~) not expected in (render-xml).~%object: ~:w" 
		   (type-of element) element))))

(defmethod render-xml-aux ((element symbol) stream)
  (write element :stream stream))

(defmethod render-xml-aux ((element string) stream)
  (write element :stream stream))

(defmethod render-xml-aux ((element number) stream)
  (write element :stream stream))

(defmethod render-xml-aux ((element list) stream)
  (unless (and (listp (car element)) (symbolp (caar element))
	       (listp (cdar element)) (evenp (length (cdar element)))
	       (loop for x in (cdar element) by #'cddr always (keywordp x))
	       (loop for x in (cddar element) by #'cddr always (stringp x))
	       (listp (cdr element)))
    (let ((*print-escape* t) (*print-pretty* t) (*print-lines* 10) (*print-case* :downcase))
      (error (format nil "Wrong element format in (render-xml).~%expected: ((element :attribute-1 \"value 1\" :attribute-2 \"value 2\" ...) ...),~%provided:~%~s" (write element :stream nil)))))

  (write-char #\< stream)
  (write (caar element) :stream stream :case :downcase)
  (loop for attribute in (cdar element) by #'cddr
     for value in (cddar element) by #'cddr
     do (write-char #\space stream)
       (write attribute :stream stream :case :downcase)
       (write-char #\= stream)
       (write value :stream stream :escape t))
  
  (write-char #\> stream)
  (loop for e in (cdr element)
     do (render-xml-aux e stream))
  (write-char #\< stream)
  (write-char #\/ stream)
  (write (caar element) :stream stream :case :downcase)
  (write-char #\> stream))

(defun render-xml (element)
  (let ((stream (make-string-output-stream))
	(*print-escape* nil))
    (render-xml-aux element stream)
    (get-output-stream-string stream)))
