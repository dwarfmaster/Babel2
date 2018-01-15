(in-package :network)

;; Some functionality for drawing of networks

;; -----------
;; S-Dot Drawing
;; -----------

(export '(draw-dot render-ps render-as render-as-removal
render-as-change get-color))

(defun string->file (string file-path)
  (let ((file (open file-path :direction :output :if-exists :supersede)))
    (format file string)
    (close file)))


(defgeneric draw-dot (node &key &allow-other-keys)
  (:documentation "Draws the given object in dot"))

(defmethod draw-dot ((node net-node) &key &allow-other-keys)
  `(s-dot::node ((s-dot::id ,(mkdotstr (id node)))
		(s-dot::label ,(string-for-s-dot node)))))

(defmethod draw-dot ((edge net-edge) &key (include-edge-labels nil) (directed t) &allow-other-keys)
  (let (features)
    (push `(s-dot::from ,(mkdotstr (id (start edge)))) features)
    (push `(s-dot::to ,(mkdotstr (id (end edge)))) features)
    (when include-edge-labels
      (push `(s-dot::label ,(mkdotstr (label edge))) features))
    (unless directed
      (push `(s-dot::dir "none") features))
    `(s-dot::edge ,(nreverse features))))

(defmethod draw-dot ((net network) &key (rankdir "TB") (include-edge-labels nil))
  (let ((draw-dots (loop for prim in (primitives net)
		      collect (draw-dot prim :include-edge-labels include-edge-labels))))
    ;; (setf draw-dots (append draw-dots (loop for edge in (edges net)
    ;;                                      collect (draw-dot edge :draw-label draw-edge-labels))))
    `(s-dot::graph ((s-dot::rankdir ,rankdir))
                   (s-dot::cluster ((s-dot::id "network")
                                    ;; (s-dot::label ,(mkdotstr (name net)))
                                    ))
                   ,@draw-dots)))

;;(defun render-ps (object path-name)
 ;; (s-dot:render-s-dot path-name "ps" (draw-dot object)))

;; ----------------------------
;; Actionscript (flash) drawing
;; ----------------------------

(defgeneric render-as (foo &optional level)
  (:documentation "Returns a string for the given object in
  action-script to be used by flash"))

(defmethod render-as ((foo t) &optional level)
  (declare (ignore level)))

(defgeneric render-as-removal (foo &optional level)
  (:documentation "Returns a string for the REMOVAL of the given object in
  action-script to be used by flash"))

(defgeneric render-as-change (foo))

(defgeneric get-color (primitive)
  (:documentation "Returns a color value as a string for the
  primtive."))

(defmethod get-color ((primitive t))
  "0xB5EAAA")

(defmethod render-as ((node net-node) &optional (level 1))
  (declare (ignore level))
  (format nil "~%addTextNode(\"~a\", \"~a\", ~a, ~a, ~a, ~a, ~a);"
	  (id node) (string-for-s-dot node)
	  (get-color node) 1 100 "false" "0x000000"))

(defmethod render-as-removal ((node net-node) &optional (level 1))
  (declare (ignore level))
  (format nil "~%removeNode(\"~a\");" (id node)))

(defmethod render-as-change ((node net-node))
  (format nil "~%changeTextNode(\"~a\",~a,~a,~a);"
	  (id node) (get-color node) 
	  1 10))

(defmethod render-as ((edge net-edge) &optional (level 1))
  (let ((result "")
	(label-id (mkstr (id (start edge)) "-" 
			      (label edge) "-" 
			      (id (end edge)))))
    (setf result (concatenate 'string result
			      (format nil "~%addCircleNode(\"~a\",\"~a\",~a,2,~a,~a);"
				      label-id (mkstr (label edge))
				      (get-color edge) 
				      level 0.1
				      )))
    (setf result (concatenate 'string result
			      (format nil "~%addEdge(\"~a\",\"~a\",~a,~a, ~a);"
				      (id (start edge))
				      label-id
				      0.1
				      (get-color edge)
				      0.1)))
    (setf result (concatenate 'string result
			      (format nil "~%addEdge(\"~a\",\"~a\",~a,~a,~a);"
				      label-id
				      (id (end edge))
				      0.1
				      (get-color edge)
				      0.1)))
    result))

(defmethod render-as ((net network) &optional (level 1))
    (loop 
       with result = ""
       for node in (primitives net)
       do  (setf result (concatenate 'string result (render-as node level)))
       finally (return result)))
