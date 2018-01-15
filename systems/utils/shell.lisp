;;;; Some utilities for starting other programs
;;;; Copied and adapted from 
;;;; http://cvs.sourceforge.net/viewcvs.py/clocc/clocc/src/port/shell.lisp?view=markup

;;; Shell Access
;;;
;;; Copyright (C) 1999-2005 by Sam Steingold
;;; This is open-source software.
;;; GNU Lesser General Public License (LGPL) is applicable:
;;; No warranty; you may copy/modify/redistribute under the same
;;; conditions with the source code.
;;; See <URL:http://www.gnu.org/copyleft/lesser.html>
;;; for details and the precise copyright document.

(in-package :utils)

(export '(run-prog pipe-through pipe-input pipe-output close-pipe with-open-pipe exec-and-print exec-and-return open-file-in-OS copy-file))


;;; helper function on gcl
;;; helper function copied from 
;;; http://cvs.sourceforge.net/viewcvs.py/clocc/clocc/src/port/ext.lisp?view=markup
(defun remove-plist (plist &rest keys)
  "Remove the keys from the plist.
Useful for re-using the &REST arg after removing some options."
  (do (copy rest)
      ((null (setq rest (nth-value 2 (get-properties plist keys))))
       (nreconc copy plist))
    (do () ((eq plist rest))
      (push (pop plist) copy)
      (push (pop plist) copy))
    (setq plist (cddr plist))))

;;;
;;; Shell interface
;;;
(defun run-prog (prog &rest opts &key args (wait t) &allow-other-keys)
  "Common interface to shell. Does not return anything useful."
  (declare (ignorable wait)) 
  (setq opts (remove-plist opts :args :wait))
  #+allegro (apply #'excl:run-shell-command (apply #'vector prog prog args)
                   :wait wait opts)
  #+(and clisp lisp=cl)
  (apply #'ext:run-program prog :arguments args :wait wait opts)
  #+(and clisp (not lisp=cl)) (if wait
				  (apply #'lisp:run-program prog :arguments args opts)
				  (lisp:shell (format nil "~a~{ '~a'~} &" prog args)))
  #+cmu (apply #'ext:run-program prog args :wait wait opts)
  #+gcl (apply #'si:run-process prog args)
  #+liquid (apply #'lcl:run-program prog args)
  #+lispworks (apply #'sys::call-system
                     (format nil "~a~{ '~a'~}~@[ &~]" prog args (not wait))
                     opts)
  #+lucid (apply #'lcl:run-program prog :wait wait :arguments args opts)
  #+sbcl (apply #'sb-ext:run-program prog args :search t :wait wait opts)
  #+ccl (ccl:run-program prog args :wait wait :sharing :external)
  #-(or allegro clisp cmu gcl liquid lispworks lucid sbcl ccl)
  (error 'not-implemented :proc (list 'run-prog prog opts)))

(defun pipe-output (prog &key args (wait nil))
  "Return an output stream which will go to the command."
  (declare (ignorable wait))
  #+allegro (excl:run-shell-command (format nil "~a~{ ~a~}" prog args)
                                    :input :stream :wait wait)
  #+clisp (#+lisp=cl ext:make-pipe-output-stream
           #-lisp=cl lisp:make-pipe-output-stream
                     (format nil "~a~{ ~a~}" prog args))
  #+cmu (ext:process-input (ext:run-program prog args :input :stream
                                            :output t :wait wait))
  #+gcl (si::fp-input-stream (apply #'si:run-process prog args))
  #+lispworks (sys::open-pipe (format nil "~a~{ ~a~}" prog args)
                              :direction :output)
  #+lucid (lcl:run-program prog :arguments args :wait wait :output :stream)

  #+sbcl (sb-ext:process-input (sb-ext:run-program prog args :input :stream
                                                   :output t :wait wait :search t))
  #+ccl (ccl:external-process-input-stream 
	 (ccl:run-program prog args :input :stream :wait wait 
			  :output t :sharing :external))
  #-(or allegro clisp cmu gcl lispworks lucid sbcl ccl)
  (error 'not-implemented :proc (list 'pipe-output prog args)))

;;; not tested yet on all platforms
(defun pipe-input (prog &key args (wait nil))
  "Return an input stream from which the command output will be read."
  (declare (ignorable wait))
  #+allegro (excl:run-shell-command (format nil "~a~{ ~a~}" prog args)
                                    :output :stream :wait nil)
  #+(and clisp lisp=cl) (ext:make-pipe-input-stream (format nil "~a~{ ~a~}" prog args))
  #+(and clisp (not lisp=cl)) (lisp:make-pipe-input-stream (format nil "~a~{ ~a~}" prog args))
  #+cmu (ext:process-output (ext:run-program prog args :output :stream
                                             :input nil :wait wait))
  #+gcl (si::fp-output-stream (apply #'si:run-process prog args))
  #+lispworks (sys::open-pipe (format nil "~a~{ ~a~}" prog args)
                              :direction :input)
  #+lucid (lcl:run-program prog :arguments args :wait nil :input :stream)
  #+sbcl (sb-ext:process-output (sb-ext:run-program prog args :output :stream
                                                    :search t :error t :input t :wait wait))
  #+ccl (ccl:external-process-output-stream 
	 (ccl:run-program prog args :output :stream :wait wait
			  :sharing :external))
  #-(or allegro clisp cmu gcl lispworks lucid sbcl ccl)
  (error 'not-implemented :proc (list 'pipe-input prog args)))


(defmacro pipe-through ((stream-to-prog stream-from-prog prog &rest args) &body body)
  "opens an input stream and and an output stream from a program. It
   is your responsibility to close both streams"
  (declare (ignorable args prog))
  `(let ((,stream-to-prog nil)
         (,stream-from-prog nil))
     #+allegro(multiple-value-bind
                    (input/output error pid)
                  (excl.osi::run-shell-command 
                   (format nil "~a~{ ~a~}" ,prog (list ,@args))
                   :input :stream
                   :output :stream
                   :wait nil)
                (setf ,stream-to-prog input/output)
                (setf ,stream-from-prog input/output))
     #+ccl(let ((proc (ccl:run-program ,prog (list ,@args) :input :stream :wait nil
                                       :output :stream :sharing :external)))
            (setf ,stream-to-prog (ccl:external-process-input-stream proc))
            (setf ,stream-from-prog (ccl:external-process-output-stream proc)))
     #+lispworks(progn
                  (setf ,stream-to-prog 
                        (sys:run-shell-command 
                         (format nil "~a~{ ~a~}" ,prog (list ,@args))
                         :input :stream :output :stream :wait nil))
                  (setf ,stream-from-prog ,stream-to-prog))
     #+sbcl(let ((proc (sb-ext:run-program ,prog (list ,@args)
					   :output :stream :input :stream
                                           :search t :error t :wait nil)))
             (setf ,stream-to-prog (sb-ext:process-input proc))
             (setf ,stream-from-prog (sb-ext:process-output proc)))
     #-(or allegro ccl lispworks sbcl)
     (error "pipe-through not implemented on this platform")
         
     (unwind-protect (progn ,@body)
       (when ,stream-to-prog (close ,stream-to-prog))
       (when ,stream-from-prog (close ,stream-from-prog)))))


;;; Allegro CL: a simple `close' does NOT get rid of the process.
;;; The right way, of course, is to define a Gray stream `pipe-stream',
;;; define the `close' method and use `with-open-stream'.
;;; Unfortunately, not every implementation supports Gray streams, so we
;;; have to stick with this to further the portability.
;;; [2005] actually, all implementations support Gray streams (see gray.lisp)
;;; but Gray streams may be implemented inefficiently

(defun close-pipe (stream)
  "Close the pipe stream."
  (declare (stream stream))
  (close stream)
  ;; CLOSE does not close constituent streams
  ;; CLOSE-CONSTRUCTED-STREAM:ARGUMENT-STREAM-ONLY
  ;; http://www.lisp.org/HyperSpec/Issues/iss052.html
  (typecase stream
    (two-way-stream
     (close (two-way-stream-input-stream stream))
     (close (two-way-stream-output-stream stream))))
  #+allegro (sys:reap-os-subprocess))

(defmacro with-open-pipe ((pipe open) &body body)
  "Open the pipe, do something, then close it."
  `(let ((,pipe ,open))
    (declare (stream ,pipe))
    (unwind-protect (progn ,@body)
      (close-pipe ,pipe))))


(defun exec-and-print (program &rest args)
  "runs a shell command and prints the output to the listener"
  (handler-case 
      (with-open-pipe (stream (pipe-input program :args args :wait t))
	(loop for line = (read-line stream nil)
	   when line do (princ line) (princ #\newline) (force-output t)
	   while line))
    (error () nil)))

(defun exec-and-return (program &rest args)
  "runs a shell command and returns what it prints to standard output and standard error"
  (let (outputlines)
        (with-open-pipe (stream (pipe-input program :args args :wait t))
          (loop for line = (read-line stream nil nil)
                while line
                do (push line outputlines)))
    (reverse outputlines)))

(defun open-file-in-OS (path)
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

(defun copy-file (source dest)
  (exec-and-return "cp" (mkstr source) (mkstr dest)))

;;(exec-and-print "dot" "-V")
;;(exec-and-print "gnuplot" "--version")
;;(exec-and-print "gnuplot" "-e" "\"show term\"")



