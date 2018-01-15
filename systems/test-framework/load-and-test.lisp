;; the package relies on :test-framework for some symbols and *babel-path* for paths
;; supports ccl and lispworks as host 
;; and anything else as inferior

(asdf:operate 'asdf:load-op :test-framework)

(defpackage :load-and-test
  (:use :common-lisp :cl-user :test-framework))

(in-package :load-and-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers 

(defun make-random-string (&optional (length 30))
  "Generates and returns a random string length LENGTH.  The
string will consist solely of decimal digits and ASCII letters."
  (with-output-to-string (s)
    (dotimes (i length)
      (write-char (ecase (random 5)
                    ((0 1) (code-char (+ #.(char-code #\a) (random 26))))
                    ((2 3) (code-char (+ #.(char-code #\A) (random 26))))
                    ((4) (code-char (+ #.(char-code #\0) (random 10)))))
                  s))))

(defun pipe-input (prog &key args (wait nil))
  "Return an input stream from which the command output will be read."
  #+lispworks (sys::open-pipe (format nil "~a~{ ~a~}" prog args)
                              :direction :input)
  #+ccl (ccl:external-process-output-stream 
         (ccl:run-program prog args :output :stream :wait wait
                          :sharing :external)))
(defun close-pipe (stream)
  "Close the pipe stream."
  (declare (stream stream))
  (close stream)
  (typecase stream
    (two-way-stream
     (close (two-way-stream-input-stream stream))
     (close (two-way-stream-output-stream stream)))))

(defun exec-and-print (program &rest args)
  "runs a shell command and prints the output to the listener"
  (handler-case 
      (let ((stream (pipe-input program :args args :wait t)))
        (declare (stream stream))
        (unwind-protect
             (loop for line = (read-line stream nil)
                when line do (princ line) (princ #\newline) (force-output t)
                while line))
        (close-pipe stream))
    (error (e) 
      (format 
       t "~%An error occured trying to run ~a ~@[with arguments ~{~a~}~]. Error is ~a" 
       program args e))))

(defun exec (program &rest args)
  "runs a shell command and prints the errors to the listener"
  (handler-case 
      (let ((stream (pipe-input program :args args :wait t)))
        (declare (stream stream))
        (unwind-protect
             (loop 
                for line = (read-line stream nil)
                while line))
        (close-pipe stream))
    (error (e) 
      (format 
       t "~%An error occured trying to run ~a ~@[with arguments ~{~a~}~]. Error is ~a" 
       program args e))))

(defun clean-babel (&key verbose)
  (when verbose
    (format t "~%.. Cleaning all~%"))
  (funcall (if verbose 'exec-and-print 'exec)
           "bash"
           (format nil "~a"
                   (merge-pathnames 
                    (cl-user::babel-pathname)
                    (make-pathname :name "clean" :type "sh")))
           "--all"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; main test function

(defun load-and-test-package (test-package lisp-command
                              &key (path (merge-pathnames 
                                          (make-pathname 
                                           :directory '(:relative ".tmp")) 
                                          (cl-user::babel-pathname)))
                              clean
                              verbose)
  """loads a particular package for some lisp and runs the tests
     lisp-command --  '(\"ccl\" (\"--batch\" \"--quiet\")) or '(\"lispworks\" ()) or ...
     returns (load-error ((package test error-message) ... ))"""
  (format t "~2%.. load and test package ~(~s~) in ~s~2%" 
          test-package
          (car lisp-command))
  (let* ((output (merge-pathnames 
                  path 
                  (make-pathname :name (format nil "load-and-test-~a-out" 
                                               (make-random-string 10))
                                 :type "lisp"))))
    ;; 1) clean (nix only) - run bash clean.sh
    (when clean (clean-babel :verbose verbose))
    ;; 2) run external/inferior lisp
    (let ((external-process-fn #+sbcl 'sb-ext:run-program
                               #+ccl 'ccl:run-program  
                               #+lispworks 'system::run-shell-command)
          
          (program
           `((progn
               (setf *automatically-start-web-interface* nil)
               (setf asdf::*compile-file-warnings-behaviour* :warn)
               ;; (setf asdf::*compile-file-errors-behaviour* :error)
               (setf asdf::*compile-file-failure-behaviour* :error)
               (asdf:operate 'asdf:load-op :test-framework)
               (setf test-framework::*raise-errors* t)
               (let (*errors*
                     *warnings*)
                 (format t "~%testing ~a" ,test-package)
                 (catch 'abort
                   (handler-bind
                       ((asdf::style-warning #'(lambda (e)
                                                 (format t "~%STYLE: ~a" e)
                                                 (push e *warnings*)
                                                 (muffle-warning)))
                        (warning #'(lambda (e)
                                     (format t "~%WARNING: ~a" e)
                                     (push e *warnings*)
                                     (muffle-warning)))
                        (error 
                         #'(lambda (e)
                             (format t "~%ERROR: ~a" e)
                             (push e *errors*)
                             (or (and (find-restart 'asdf::accept)
                                      (invoke-restart 'asdf::accept))
                                 (continue)
                                 (abort)
                                 (throw 'abort nil)))))
                     (asdf:operate 'asdf:load-op ,test-package)))
                 (format t "~%finished asdf load-op.")
                 (format t "~%loaded successfully ~a" (not *errors*))
                 (format t "~%# errors: ~a" (length *errors*))
                 (format t "~%# warnings: ~a" (length *warnings*))
                 (format t "~%# run tests: ~a" (length test-framework::*all-tests*))
                 (format t "~%# errors in tests: ~a" 
                         (length test-framework::*run-tests-error-messages*))
                 
                 (format t "~%writing results to ~s." ,output)
                 (with-open-file (stream ,output
                                         :direction :output :if-exists :supersede)
                   (format stream "~%(~S ~S ~S ~S)"                            
                           (cons :errors
                                 (loop for e in *errors*
                                    collect (format nil "~a" e)))
                           (cons :warnings
                                 (loop for e in *warnings*
                                    collect (format nil "~a" e)))
                           (cons :run-tests-errors
                                 (loop
                                    for m in test-framework::*run-tests-error-messages*
                                    collect (list
                                             (package-name (symbol-package (first m)))
                                             (symbol-name (first m))
                                             (second m))))
                           (cons :run-tests
                                 (loop 
                                    for e in test-framework::*all-tests*
                                    collect (format nil "~a" e)))))
                 (format t "~%results written. exiting.")))))
          process input-stream output-stream)

      ;; start external/inferior lisp
      #+:ccl
      (progn
        (setf process (apply (symbol-function external-process-fn)
                             (car lisp-command)
                             (append (cdr lisp-command)
                                     (list :wait nil :input 
                                           :stream :output :stream 
                                           :error :output :sharing :external))))
        (setf output-stream (ccl:external-process-input-stream
                             process))
        (setf input-stream
              (ccl:external-process-output-stream process)))
      #+:lispworks
      (multiple-value-bind (in/out/err temp pid)
          (apply (symbol-function external-process-fn)
                 lisp-command)
        (declare (ignore temp))
        (setf process pid)
        (setf output-stream in/out/err)
        (setf input-stream in/out/err))

      ;; check stream
      (let ((c (read-char input-stream nil)))
        (if c (unread-char c input-stream)
            (error "could not start lisp process~% ~a"
                   (format nil "~(~w~)"
                           (cons external-process-fn lisp-command))))
        ;; stream program
        (loop 
           initially (when verbose
                       (format t ".. writing commands to inferior process ~s~%" 
                               (car lisp-command)))
           for s in program
           do
           (when verbose
             (format t "  ~S~%" s))             
             (write-string (format nil "~S" s) output-stream)
           (princ  #\lf output-stream)
           finally
           (force-output t)
           (force-output output-stream))

        ;; close output stream to inferior lisp
        (close output-stream)

        ;; wait for finish
        (when verbose
          (format t "~%.. waiting for process ~s to finish~%~%" 
                  (car lisp-command)) )
        (loop
           do
           ;; wait
           (sleep 0.01)
           ;; output stdout/stderr of processes
           (loop
              while (setf c (read-char-no-hang input-stream nil))
              do
              (unread-char c input-stream)
              (let ((line (read-line input-stream)))
                (when verbose
                  (format t "~%inferior lisp: ~a" line))))
           ;; check processes if still running
           while 
           #+ccl(equal (ccl:external-process-status process) :running)
           #+(or lispworks6 lispworks5) (sys:pid-exit-status process :wait nil)
           #+lispworks7 (when (eql (system::pipe-stream-pid stream) p)
                                (sys:pipe-exit-status stream :wait nil)))

        ;; read in results
        (when verbose
          (format t "~%.. finished inferior process. reading results."))
        (with-open-file (stream output
                                :direction :input)
          (read stream))))))

(defun load-and-test-packages 
    (packages 
     &key 
     (lisp-commands '(("ccl" . ("ccl" ("--batch" "--quiet")))
                      ("lw " . ("lispworks" ("-multiprocessing")))))
     clean
     verbose)
  "pass some packages and it will be loading them"
  (loop 
     for package in packages
     append (loop for (lisp . command) in lisp-commands
               collect (list package lisp 
                             (load-and-test-package package command 
                                                    :clean clean
                                                    :verbose verbose)))))

(defun print-load-and-test-results (results)
  (loop 
     for res in results
     for report = (third res)
     for errors = (cdr (assoc :errors report))
     for warnings = (cdr (assoc :warnings report))
     for run-tests-errors = (cdr (assoc :run-tests-errors report))
     ;; for run-tests = (second (assoc :run-tests report))
     do (format t "~%~(~a - ~a - ~a~)" (first res) (second res)
                (cond
                  ((and (null errors) (null warnings)(null run-tests-errors)) 
                   "ok")
                  (errors "error(s) on load")
                  (t (format nil "warnings: ~3s / test errors: ~3s" 
                             (length warnings)
                             (length run-tests-errors)))))))

;; (progn (setf *results* (load-and-test-packages '(:irl-2012) :clean t :verbose nil)) (print-load-and-test-results *results*))

;; (progn (setf *results* (load-and-test-packages '(:utils-2012 :irl-2012 :fcg-2012 :tasks-and-processes :experiment-framework :determination :german-space) :clean t :verbose nil)) (print-load-and-test-results *results*))


(defun load-and-test-babel-packages ()
  (let* ((packages '(:utils
                     :monitors
                     :web-interface
                     :irl			
                     :fcg
                     :experiment-framework
                     :tasks-and-processes
                     :meta-layer-learning
                     :action-behavior-framework))
         (results (load-and-test-packages packages :clean t :verbose nil)))
    (print-load-and-test-results results)))

;; (defparameter *results* (load-and-test-babel-packages))
;; (asdf:operate 'asdf:load-op :cl-store)
;; (defparameter *old-results* (cl-store::restore "load-and-test-results"))
;; (cl-store::store *results* "load-and-test-results")
;; (equalp *results* *old-results*)
;; (defparameter *results* (load-and-test-packages '(:utils) :lisp-commands '((lw  lispworks))  :verbose t))

;;(pprint (load-and-test-packages '(:utils) :lisp-commands '(("lw " . ("lispworks" ("-multiprocessing")))) :verbose t :clean t))

