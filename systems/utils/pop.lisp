(in-package :utils)

;;; Parallel operators
;;; original version by Pascal Costanza

#+lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(mp:make-mailbox
            mp:mailbox-send
            mp:mailbox-read
            mp:process-run-function)))

#+(and sbcl sb-thread)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(sb-thread:make-waitqueue 
            sb-thread:condition-notify
            sb-thread:condition-wait
            sb-thread:make-mutex
            sb-thread:with-mutex
            sb-thread:make-thread)))

(export '(make-mailbox
          mailbox-send
          mailbox-read

          *max-threads*
          active-threads
          get-threads

          prun
          pmap))

#+(and sbcl sb-thread)
(progn
  (defstruct (mailbox (:constructor make-mailbox ()))
    (queue (make-waitqueue))
    (lock (make-mutex))
    (head '())
    (tail '()))

  (defun mailbox-read (mailbox)
    (with-mutex ((mailbox-lock mailbox))
      (loop until (mailbox-head mailbox)
            do (condition-wait 
                (mailbox-queue mailbox) 
                (mailbox-lock mailbox)))
      (prog1 (pop (mailbox-head mailbox))
        (unless (mailbox-head mailbox)
          (setf (mailbox-tail mailbox) nil)))))

  (defun mailbox-send (mailbox object)
    (let ((cell (list object)))
      (with-mutex ((mailbox-lock mailbox))
        (if (mailbox-head mailbox)
          (setf (cdr (mailbox-tail mailbox)) cell)
          (setf (mailbox-head mailbox) cell))
        (setf (mailbox-tail mailbox) cell)
        (condition-notify (mailbox-queue mailbox))))))

#-(or lispworks (and sbcl sb-thread))
(progn
  (defstruct (mailbox (:constructor make-mailbox ()))
    (head '())
    (tail '()))

  (defun mailbox-read (mailbox)
    (prog1 (pop (mailbox-head mailbox))
      (unless (mailbox-head mailbox)
        (setf (mailbox-tail mailbox) nil))))

  (defun mailbox-send (mailbox object)
    (let ((cell (list object)))
      (if (mailbox-head mailbox)
        (setf (cdr (mailbox-tail mailbox)) cell)
        (setf (mailbox-head mailbox) cell))
      (setf (mailbox-tail mailbox) cell))))

(defparameter *max-threads* 3
  "The maximum number of threads that can be spawned concurrently.
Configure this to reflect the number of available cores, hyperthreads, etc.
Note: There is no strict enforcement that this number will never be
exceeded, because everything here is based on heuristics and estimates.
It is recommended that *max-threads* is actually a bit lower than the number
of concurrent threads granted by the operating system, to account for
background threads required by the Common Lisp runtime, etc.")

(defvar *active-threads* 0)

(declaim (inline active-threads))

(defun active-threads ()
  "Active-threads returns an estimate how many threads are executing concurrently at the moment."
  *active-threads*)

(defun get-threads (&optional (amount *max-threads*))
  "Get-threads returns an estimate how many threads can be spawned concurrently at the moment.
The desired amount of threads can be passed as an optional argument.
The result depends on how many threads are estimated to be already executing at the moment
(see the function active-threads), and the maximal number of threads that can be spawned.
Get-threads always returns at least 1. The result can, for example, be used to determine
the length of sequences passed to the function pmap.
The overall behavior can be configured by setting *max-threads*."
  (max 1 (min amount (- *max-threads* *active-threads*))))

(defmacro aincf (place)
  (declare (ignorable place))
  #+(and lispworks (not lispworks6)) `(incf ,place)
  #+lispworks6 `(sys:atomic-incf ,place)
  #+(and sbcl (not sb-thread)) `(incf ,place)
  #+(and sbcl sb-thread)
  (let ((old (gensym)) (new (gensym)) (cmp (gensym)))
    `(do* ((,old ,place ,place)
           (,new (1+ ,old) (1+ ,old))
           (,cmp (sb-ext:compare-and-swap (symbol-value ',place) ,old ,new)
                 (sb-ext:compare-and-swap (symbol-value ',place) ,old ,new)))
          ((eq ,old ,cmp) ,new))))

(defmacro adecf (place)
  (declare (ignorable place))
  #+(and lispworks (not lispworks6)) `(decf ,place)
  #+lispworks6 `(sys:atomic-decf ,place)
  #+(and sbcl (not sb-thread)) `(decf ,place)
  #+(and sbcl sb-thread)
  (let ((old (gensym)) (new (gensym)) (cmp (gensym)))
    `(do* ((,old ,place ,place)
           (,new (1- ,old) (1- ,old))
           (,cmp (sb-ext:compare-and-swap (symbol-value ',place) ,old ,new)
                 (sb-ext:compare-and-swap (symbol-value ',place) ,old ,new)))
          ((eq ,old ,cmp) ,new))))

(defmacro prun (name (&rest bindings) &body body)
  "Prun executes a body of code concurrently, with bindings that are visible in the newly
spawned thread (as if per let).
Increments the value returned by active-threads on enter, and decrements it on exit.
The return value of prun is unspecified."
  (declare (ignorable body name))
  (assert (loop for binding in bindings
                always (and (consp binding)
                            (consp (cdr binding))
                            (not (cddr binding)))))
  #+lispworks
  `(process-run-function
    ,name '()
    (lambda ,(mapcar #'car bindings)
      (aincf *active-threads*)
      (unwind-protect (progn ,@body)
        (adecf *active-threads*)))
    ,@(mapcar #'cadr bindings))
  #+sbcl
  `(let ,bindings
     (make-thread
      (lambda ()
        (aincf *active-threads*)
        (unwind-protect (progn ,@body)
          (adecf *active-threads*)))
      :name ,name)))

#+(or lispworks6 (and sbcl sb-thread))
(defun pmap (result-type function &rest args)
  "Pmap works like the predefined function map in Common Lisp. So:
It applies a function to successive lists of arguments in which each argument is obtained
from each argument sequence (list or vector).
Pmap returns nil if result-type is nil. Otherwise, map returns a sequence such that each
element is the result of applying the function to the corresponding elements in the
argument sequences. The result sequence is as long as the shortest of the argument
sequences. The consequences are unspecified if any of the arguments is an improper list.
If the result-type is a subtype of list, the result will be a list.
If the result-type is a subtype of vector, the result will be a subtype of vector.

In other words, (map 'list function args ...) is like (mapcar function args ...),
and (map nil function args) is like (mapc function args), but map and pmap can receive and
return not only lists, but also vectors.

Pmap has the following differences to Common Lisp's map:
The function may be applied to the elements of the argument sequences in unspecified
order, and potentially in parallel. So the function should never make any assumptions
about the order of application, and should especially assume that accesses to shared
resources in the function passed to pmap may execute concurrently.
If the shortest argument sequence has length 0, nothing will be applied.
If the shortest argument sequence has length 1, no concurrent threads will be spawned,
but the function will be applied to the first element of each argument sequence
in the current thread, and a sequence with exactly one return value (or nil) will be
returned.
If the shortest argument sequence has a length that is roughly the same as the number
of threads that can be concurrently spawned at the moment, pmap will spawn that many
threads and apply the function to a single element in each thread.
If the shortest argument sequence has a length that is larger than the number of threads
that can be concurrently spawned at the moment, pmap will spawn as many threads as
possible and apply the function to several elements in each thread. Elements will be evenly
distributed to all the threads.
If pmap spawns any threads, it will block until all of them are done.
There is no attempt to do 'work stealing', or other forms of load balancing, in the current
version of pmap."
  (multiple-value-bind (argvs length) (loop for arg in args
                                            for argv = (coerce arg 'vector)
                                            collect argv into argvs
                                            minimize (length argv) into length
                                            finally (return (values argvs length)))
    (case length
      (0 (when result-type (coerce '() result-type)))
      (1 (let ((result (apply function (loop for argv in argvs collect (svref argv 0)))))
           (when result-type (coerce (list result) result-type))))
      (t (let* ((resultv (when result-type (make-array length)))
                (mailbox (make-mailbox))
                (threads (get-threads length))
                (block-size (ceiling length threads)))
           (if (> block-size 1)
               (loop for start below length by block-size
                     for end = (min (+ start block-size) length) do
                     (prun "pmap" ((start start) (end end)
                                   (blocks (loop for argv in argvs collect (subseq argv start end))))
                           (if result-type
                               (setf (subseq resultv start end)
                                     (apply #'map 'vector function blocks))
                             (apply #'map 'nil function blocks))
                           (mailbox-send mailbox t)))
             (loop for index below length do
                   (prun "pmap" ((index index)
                                 (args (loop for argv in argvs collect (svref argv index))))
                         (let ((result (apply function args)))
                           (when result-type (setf (svref resultv index) result))
                           (mailbox-send mailbox t)))
                   finally (setq threads length)))
           (loop repeat threads do (mailbox-read mailbox))
           (when result-type (coerce resultv result-type)))))))

#-(or lispworks6 (and sbcl sb-thread))
(progn
  (declaim (inline pmap))

  (defun pmap (result-type function &rest args)
    (declare (dynamic-extent args))
    (apply #'map result-type function args))

  (define-compiler-macro pmap (result-type function &rest args)
    `(map ,result-type ,function ,@args)))
