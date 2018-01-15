;;;;;
;;;;; functionalities for creating graphics with graphviz dot and for
;;;;; displaying the resulting images in the web interface
;;;;;

(in-package :web-interface)

(export '(make-dot-id escape-dot-label
          s-dot->image s-dot->svg))


(defun make-dot-id (pos thing)
  "makes something like 'CONTEXT154' out of 4 and '?CONTEXT-15"
  (format nil "~a~a" 
          (if (symbolp thing)
              (string-replace (string-replace (symbol-name thing) "-" "dash")
                              "?" "var")
              (make-id))
          pos))

(defun escape-dot-label (thing)
  (string-replace 
   (string-replace 
    (format nil "~(~a~)" thing)
    ">" "\\>")
   "<" "\\<"))



;; ############################################################################
;; s-dot->image
;; ----------------------------------------------------------------------------

(defvar *graphviz-output-directory* 
  (babel-pathname :directory '(".tmp")))

(defun s-dot->image (s-dot-expression &key path (format "png") (open nil))
  "Renders s-dot-expression into dot syntax and then runs graphviz dot
   to create an image file. Returns the pathname of the generated
   graphic. When :open t, then it tries to open the resulting file."
  (let* ((path (or path
                   (monitors::make-file-name-with-time-and-experiment-class
                    (merge-pathnames *graphviz-output-directory*
                                     (make-pathname :name "" 
                                                    :type format))
                    (mkstr (make-id 'graph))))))
    (ensure-directories-exist *graphviz-output-directory*)
    ;; create the graphic file
    (pipe-through 
        ;; on windows the dot tool does not run if it can't write to stdout
        ;; so we also have to open an input pipe
        (input output "dot" (mkstr "-T" format) "-o"
               #+(or :win32 :windows) (format nil "\"c:~a\"" (mkstr path))
               #-(or :win32 :windows) (mkstr path))
      (s-dot:s-dot->dot input s-dot-expression)
      (when input (close input))
      (when output (close output)))
    
    ;; although we run dot with :wait t, on some machines it might
    ;; happen that the resulting image is not accessible yet, so we
    ;; wait. Although we also don't wait for more than 5 seconds
    ;; because the file might, for some erroneous reason, never be
    ;; written (e.g. dot is not found on the system).
    (loop for i from 1 to 100
       until (probe-file path)
       do (sleep 0.05))
    ;; try to open it
    (when open 
      (cond 
        ((equal (software-type) "Darwin")
         (run-prog "open" :args (list (format nil "~a" path))))
        ((equal (software-type) "Linux")
         (run-prog "see" :args (list (format nil "~a" path))))
        ((equal (software-type) "Microsoft Windows")
         (run-prog "cmd" 
                   :args (list "/C"
                               (string-replace 
                                (format nil "\"c:~a\"" path) "/" "\\"))))))
    path))



;; ############################################################################
;; s-dot->svg
;; ----------------------------------------------------------------------------

(defun s-dot->svg (s-dot-expression)
  "Renders the s-dot expression to the dot format, then runs dot on it
   and returns the resulting svg xml expression."
  (pipe-through (input output "dot" "-Tsvg")
    (s-dot:s-dot->dot input s-dot-expression)
    (unless (eq input output) (close input))
    (let ((lines nil) (line nil))
      (loop do (setf line (read-line output nil))
         ;; skip stuff before <svg  ...
         until (and (> (length line) 3) (equal (subseq line 0 4) "<svg")))
      (loop ;; skip comments
         do (unless (and (> (length line) 3) (equal (subseq line 0 4) "<!--"))
              (push line lines))
           (setf line (if (equal line "</svg>")
                          nil
                          (read-line output nil)))
         while line)
      (when output (close output))
      (reduce #'string-append (reverse lines)))))
