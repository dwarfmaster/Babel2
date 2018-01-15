
(in-package :experiment-framework)

;;
;; run-parallel-batch runs a batch in parallel lisp processes
;; this makes only sense when your machine has multiple processors
;; 
;; example:
;;
;; (run-parallel-batch :asdf-system "naming-game" 
;;                     :package "naming-game"
;;                     :experiment-class "naming-game" 
;;                     :number-of-interactions 5000 
;;                     :number-of-series 10
;;                     :monitors '("naming-game::plot-success+lexicon-size"))

(export '(run-parallel-batch 
          parallel-batch-run-client-process
          run-parallel-batch-for-different-configurations
          create-graphs-for-different-experimental-conditions
          create-graphs-for-different-experimental-configurations
          create-graphs-for-different-population-sizes
	        create-bar-plot-for-different-experimental-conditions
          create-bar-plots-for-different-experimental-conditions
          create-configuration-a-vs-configuration-b-bar-plot))

;; if *max-nr-parallel-processes* is set to a number,
;; then a maximum of parallel processes is started
;; e.g., use this if you have large number of series to run on
;; hardware which only has few cores
(defparameter *max-nr-parallel-processes* nil) 

;; the following two defparams are used for starting a particular 
;; client/inferior lisp 
(defparameter *inferior-lisps* '((sbcl . ("sbcl" ("--noinform" "--disable-debugger")))
                                 (ccl . ("ccl" ("--batch" "--quiet")))
                                 (lispworks . ("lispworks" ())))
  "inferior lisps default command line options 
   format is command (arguments)")

;; default argument to run-client-process
(defvar *inferior-lisp* (assqv #+sbcl 'sbcl #+ccl 'ccl #+lispworks 'lispworks
                                     *inferior-lisps*)
  "the standard inferior lisp used in run-client-processes")

(defun run-client-processes (&key asdf-system package experiment-class
                             number-of-interactions number-of-series 
                             (max-nr-parallel-processes
                              *max-nr-parallel-processes*)
                             monitors
                             configurations number-of-data-points
                             (configuration-output-directory 
                              (babel-pathname :directory '(".tmp")))
                             (inferior-lisp *inferior-lisp*))
  "A helper function for run-parallel-batch and
   create-graphs-for-different-experimental-conditions"
  ;; start client lisps
  (let* ((temp-file-names (loop for i from 1 to number-of-series
                             collect (mkstr (babel-pathname :directory '(".tmp"))
                                            "data-" (get-universal-time) "-"
                                            (make-random-string) ".dat")))
         (external-process-fn #+sbcl 'sb-ext:run-program
                              #+ccl 'ccl:run-program
                              #+lispworks 'system::run-shell-command)
         (external-process-fn-options #+sbcl '(:wait nil :input :stream :output :stream
                                               :search t)
                                      #+ccl '(:wait nil :input :stream :output :stream 
                                              :error :output :sharing :external)
                                      #+lispworks '(:input :stream :output :stream
                                                    :error-output :output
                                                    :wait nil
                                                    :save-exit-status t))
         (series-run 0))
    ;; run processes taking into account max-nr-parallel-processes
    (loop
       ;;with series-run = 0
       while (< series-run number-of-series)
       for series-to-run = (if max-nr-parallel-processes
                               (min max-nr-parallel-processes
                                    (- number-of-series series-run))
                               number-of-series)
       for processes = nil
       for input-streams = nil
       for output-streams = nil
       do
       ;; collect processes and i/o streams
       (loop
          for j from 1 to series-to-run
          for process = nil
          for input-stream = nil
          for output-stream = nil
          do
          (format t "~%.. starting process ~a:" (+ series-run j))
          (format t "~%  ~(~w~)~%" (cons external-process-fn inferior-lisp))
          do
          #+:sbcl
          (progn
            (setf process (apply (symbol-function external-process-fn)
                                 (append inferior-lisp external-process-fn-options)))
            (setf output-stream (sb-ext:process-input process))
            (setf input-stream (sb-ext:process-output process)))
          #+:ccl
          (progn
            (setf process (apply (symbol-function external-process-fn)
                                 (append inferior-lisp external-process-fn-options)))
            (setf output-stream (ccl:external-process-input-stream
                                 process))
            (setf input-stream
                  (ccl:external-process-output-stream process)))
          #+:lispworks
          (multiple-value-bind (in/out/err temp pid)
              (apply (symbol-function external-process-fn)
                     (format nil "~a ~{~a~^ ~}" (first inferior-lisp) (second inferior-lisp))
                     external-process-fn-options)
            (declare (ignore temp))
            (setf process pid)
            (setf output-stream in/out/err)
            (setf input-stream in/out/err))
          do (pushend process processes)
          (pushend output-stream output-streams)
          (pushend input-stream input-streams))
         
       ;; test process pipes
       (loop
          for input-stream in input-streams
          for c = (read-char input-stream nil)
          if c
          do (unread-char c input-stream)
          else
          do
          (error "could not start lisp process~% ~a"
                 (format nil "~(~w~)"
                         (cons external-process-fn 
                               (append inferior-lisp external-process-fn-options)))))

       ;; piping the commands
       (loop
        for output-stream in output-streams
        for counter from (+ series-run 1)
        for file-name = (nth (1- counter) temp-file-names)
        for commands = (list
                        "(setf cl-user::*automatically-start-web-interface* nil)"
                        "(setf test-framework::*dont-run-tests-when-loading-asdf-systems* t)"
                        "(asdf:operate 'asdf:load-op :experiment-framework :verbose nil)"
                        ;; somehow this has to run twice for lispworks
                        #+lispworks"(asdf:operate 'asdf:load-op :experiment-framework :verbose nil)"
                        (mkstr 
                         "(experiment-framework::parallel-batch-run-client-process"
                         " :asdf-system :" asdf-system 
                         " :package \"" package 
                         "\" :experiment-class \"" experiment-class 
                         "\" :number-of-interactions " number-of-interactions 
                         " :monitors " (format nil "'(~{\"~a\"~^ ~})" monitors) 
                         " :configurations " (format nil "~s" (format nil "~s" configurations))
                         " :number-of-data-points " (format nil "\"~a\"" number-of-data-points)
                         " :file-name \"" file-name 
                         "\" :random-number " (+ 100 (random (get-universal-time))) 
                         (if (= counter 1)
                           (mkstr " :configuration-output-directory \"" 
                                  configuration-output-directory "\"") "")
                         ")")
                        "(utils:quit-lisp)"
                        "(sleep 0.1)" ;; make sure the status of the process can be changed
                        )
        do
        ;; write commands to processes
        (format t ".. writing commands to process ~a:~%" counter)
        (loop for c in commands
              do
              (format t "  ~a~%" c)
              (write-string c output-stream)
              (princ  #\lf output-stream))
        (force-output t)
        (force-output output-stream)
        (finish-output output-stream)
        #-lispworks
        (close output-stream))
     
       ;; wait until the clients terminate and print the output
       (format t "~%.. waiting for processes ~a to ~a to finish~%~%"
               (1+ series-run) (+ series-run series-to-run))
       (loop
          with running-processes = (copy-list processes)
          with running-input-streams = (copy-list input-streams)
          with running-process-numbers = (loop for i from 1 to (length running-processes)
                                            collect (+ series-run i))
          while running-processes
          do
          ;; wait
          (sleep 0.01)
          ;; output stdout/stderr of processes
          (loop
             for input-stream in running-input-streams
             for n in running-process-numbers
             for char = nil
             while (setf char (read-char-no-hang input-stream nil))
             do
             (unread-char char input-stream)
             (format t "~%process ~d: ~a" n (read-line input-stream)))

          ;; check processes if still running
          (loop
             for p in running-processes
             when 
             #+sbcl(not (equal (sb-ext:process-status p) :running))
             #+ccl(not (equal (ccl:external-process-status p) :running))
             #+(or lispworks6 lispworks5) (sys:pid-exit-status p :wait nil)
             #+lispworks7 (loop for stream in output-streams
                                when (eql (system::pipe-stream-pid stream) p)
                                return (sys:pipe-exit-status stream :wait nil))
             collect p into processes-finished
             finally
             (loop for p in processes-finished
                for i = (position p running-processes)
                do
                (setf running-processes (remove p running-processes))
                (setf running-input-streams (remove (nth i running-input-streams)
                                                    running-input-streams))
                (setf running-process-numbers
                      (remove (nth i running-process-numbers)
                              running-process-numbers)))))
       #+sbcl(loop for p in processes
                do (sb-ext:process-close p))
       (setf series-run (+ series-run series-to-run)))

    ;; after all processes finished read data
    (format t "~%.. reading the temp files written by the client processes~%")
    (force-output t)
    (sleep 0.1)
    (loop
       with series-number = 0
       with results = nil
       for file-name in temp-file-names
       do (with-open-file (stream file-name :direction :input)
            (unless stream (error "could not open ~s" file-name))
            (let ((*package* (find-package (read-from-string package))))
              (setf results (read stream)))
            (loop for result in results
               for monitor = (monitors::get-monitor (first result))
               do (loop for x in (reverse (second result))
                     do (push x (caar (slot-value monitor 'monitors::values))))
               (loop for x in (reverse (third result))
                  do (push x (caar (slot-value monitor 'monitors::average-values)))))
            (setf series-number (incf series-number))
            (monitors:notify monitors:series-finished series-number)))))

;; this is the function that is run by the inferior/client lisp
(defun parallel-batch-run-client-process (&key asdf-system package experiment-class 
					  number-of-interactions
                                          monitors configurations file-name
					  number-of-data-points random-number
                                          configuration-output-directory)
  (declare (ignorable random-number))
  
  ;; set random seed
  (setf *random-state* 
        #+sbcl(sb-kernel::%make-random-state 
               (sb-kernel::init-random-state ;;!! Add ":state" keyword if you are using a SBCL version prior to 1.2.12
                (logand (random random-number) #xFFFFFFFF)))
        #+ccl(make-random-state t)
        #+lispworks (make-random-state))

  ;; load the requested asdf system
  (asdf:operate 'asdf:load-op asdf-system :verbose nil)

  ;; set the package
  (setf *package* (find-package (read-from-string package)))
  
  ;; make the experiment class
  (let ((experiment 
         (make-instance 
          (read-from-string experiment-class)
          :entries (read-from-string configurations))))
    
    ;; Some configurations are also set by the experiment, so
    ;; they have to be set again after the experiment was created.
    (loop for configuration in (read-from-string configurations)
       do (set-configuration experiment (car configuration) (cdr configuration)))

    ;; activate the monitors
    (monitors:deactivate-all-monitors)
    (loop for monitor in monitors
       do (monitors::activate-monitor-method (read-from-string monitor)))
    (monitors:notify monitors:reset-monitors)
    (monitors:activate-monitor experiment-framework:trace-experiment-in-repl)

    ;; run a series
    (experiment-framework:run-series experiment number-of-interactions)

    ;;write the results to a temp file
    (format t "~%.. collecting data recorded by data-recorders.~%")
    (force-output t)
    (let* ((number-of-data-points-parsed (read-from-string number-of-data-points))
           (recorded-data
            (loop for monitor being the hash-value of monitors::*monitors*
               when (and (subtypep (class-of monitor) 'monitors:data-recorder)
                         (monitors::active monitor))
               collect 
                 (list (monitors::id monitor) 
                       (if number-of-data-points-parsed
                           (loop with source = (caar (slot-value monitor 'monitors::values))
                              for i from 0 to (- number-of-data-points-parsed 1)
                              for index = (round (* (/ number-of-interactions
                                                       number-of-data-points-parsed) i))
                              collect (nth index source))
                           (caar (slot-value monitor 'monitors::values)))
                       (if number-of-data-points-parsed
                           (loop with source = (caar (slot-value monitor 'monitors::average-values))
                              for i from 0 to (- number-of-data-points-parsed 1)
                              for index = (round (* (/ number-of-interactions
                                                       number-of-data-points-parsed) i))
                              collect (nth index source))
                           (caar (slot-value monitor 'monitors::average-values)))))))
      (format t ".. writing recorded data to ~s.~%" file-name)
      (force-output t)
      (with-open-file (stream file-name :direction :output :if-exists nil)
        (unless stream (error "could not open ~s" file-name))
        (write recorded-data :stream stream))
      
      ;; When requested, then the configuration of the experiment is
      ;; written to a file
      (when configuration-output-directory
        (let ((file-name
               (monitors::make-file-name-with-time-and-experiment-class
                (merge-pathnames configuration-output-directory
                                 (make-pathname :name "configuration" 
                                                :type "lisp"))
                experiment-class)))
          (with-open-file (stream file-name :direction :output :if-exists :supersede)
            (pprint (entries experiment)) stream)
          (format t "~%.. created ~a.~%" file-name)))
      
      (format t ".. finished.~%")
      (force-output t)
      (sleep 0.2))))


(defun run-parallel-batch (&key asdf-system package experiment-class
			   number-of-interactions number-of-series 
                           (max-nr-parallel-processes
                            *max-nr-parallel-processes*)
                           monitors configurations)
  ;; parameter configurations should be called configuration since it
  ;; allows only one.
  (assert (typep asdf-system 'string))
  (assert (typep package 'string))
  (assert (typep experiment-class 'string))
  (assert (typep number-of-interactions 'number))
  (assert (typep number-of-series 'number))		    
  
  ;; load the asdf system
  (let ((test-framework::*dont-run-tests-when-loading-asdf-systems* t))
    (asdf:operate 'asdf:load-op asdf-system))

  ;; activate the monitors
  (monitors:deactivate-all-monitors)
  (loop for monitor in (reverse monitors)
     do (monitors::activate-monitor-method (read-from-string monitor)))
  (monitors:notify monitors:reset-monitors)
  
  (run-client-processes :asdf-system asdf-system
			:package package
			:experiment-class experiment-class
			:number-of-interactions number-of-interactions
			:number-of-series number-of-series
                        :max-nr-parallel-processes max-nr-parallel-processes
			:monitors monitors
			:configurations configurations)
  
  (format t ".. creating the graphs.")
  (monitors:notify monitors:batch-finished experiment-class)
  (format t ".. done.~%"))

(defun run-parallel-batch-for-different-configurations
       (&key asdf-system package experiment-class number-of-interactions number-of-series 
             (max-nr-parallel-processes *max-nr-parallel-processes*)
             monitors shared-configuration configurations 
             (output-dir (error "Please supply an :output-dir for monitoring")))
  "Runs multiple batches of parallel series. Every batch takes a
different (named) configuration. Each configuration in configurations
should be a pair like (name . configuration-list). You can use
shared-configuration to set configuration values that are shared among
all batches. Values in configurations have precedence over values in
shared-configration, should there be a conflict. output-dir can be set
to the directory of (or subdir in) your experiment. If it is set then
all data-outputting monitors will be overridden to output there. For
each named configuration a subdir will be made there with the given
name."
  (loop for configuration in configurations
        unless (and (listp configuration) 
                    (symbolp (first configuration)) 
                    (listp (second configuration)))
        do (error "Configurations should be a list of pairs like (name configuration-list)")
        do 
        ;; merge shared-configuration and current configuration
        (setf (second configuration)
              (loop with local-config = (make-configuration :entries (second configuration)) 
                    for (key . value) in shared-configuration
                    do (set-configuration local-config key value :replace nil)
                    finally (return (entries local-config))))
       
        ;; adapt file-writing monitors so they output in the correct output-dir
        (loop for monitor-string in monitors
              for monitor = (monitors::get-monitor (read-from-string monitor-string))
              when (slot-exists-p monitor 'file-name)
              do (setf (slot-value monitor 'file-name)
                       (ensure-directories-exist
                        (merge-pathnames (make-pathname :directory `(:relative ,(string-downcase (symbol-name (first configuration))))
                                                        :name (pathname-name (file-name monitor)) 
                                                        :type (pathname-type (file-name monitor)))
                                         output-dir))))
       
        ;; run the actual batch for the current configuration
        (run-parallel-batch :asdf-system asdf-system 
                            :package package
                            :experiment-class experiment-class
                            :number-of-interactions number-of-interactions 
                            :number-of-series number-of-series
                            :max-nr-parallel-processes max-nr-parallel-processes
                            :monitors monitors 
                            :configurations (second configuration))))


(defun create-combined-graphs-from-different-runs
    (&key 
     asdf-system ;; (string) the asdf system to load 
     package ;; (string) the package to use 
     file-prefix ;; (string) a prefix for file names of generated graphs (usually experiment class)
     experiment-classes ;; (list of strings) class names of experiments to run
     configurations-per-experiment ;; (list of lists of conses) configurations for each experiment
     captions ;; (list of strings) graph captions for different runs
     number-of-interactions-per-experiment ;; (list of numbers) the number interactions to run
     number-of-data-points ;; (number) how many values to record in each run
     number-of-series ;; (number) how many series to run per experiment
     (max-nr-parallel-processes *max-nr-parallel-processes*)  ;; how many series to run in parallel
     data-recorders ;; (list of strings) data recorders to generate graphs for
     average-data ;; (list of booleans) whether data is averaged
     parameters-for-graphic-generators ;; (list of :key value pairs) parameters for making graphs
     )
  ;; nazi checks
  (assert (typep asdf-system 'string))
  (assert (typep package 'string))
  (assert (typep file-prefix 'string))
  (assert (typep experiment-classes 'list))
  (assert (loop for experiment-class in experiment-classes always (typep experiment-class 'string)))
  (assert (typep configurations-per-experiment 'list))
  (assert (= (length configurations-per-experiment) (length experiment-classes)))
  (assert (loop for configurations in configurations-per-experiment
	     always (listp configurations)))
  (assert (typep captions 'list))
  (assert (= (length captions) (length experiment-classes)))
  (assert (loop for caption in captions always (typep caption 'string)))
  (assert (typep number-of-interactions-per-experiment 'list))
  (assert (= (length number-of-interactions-per-experiment) (length experiment-classes)))
  (assert (loop for number-of-interactions in number-of-interactions-per-experiment
	     always (typep number-of-interactions 'number)))
  (assert (typep number-of-data-points 'number))
  (assert (typep number-of-series 'number))
  (assert (typep data-recorders 'list))
  (assert (> (length data-recorders) 0))
  (assert (loop for data-recorder in data-recorders always (typep data-recorder 'string)))
  (assert (typep average-data 'list))
  (assert (= (length average-data) (length data-recorders)))
  (assert (typep parameters-for-graphic-generators 'list))
  (assert (= (length parameters-for-graphic-generators) (length data-recorders)))

  ;; load the asdf system
  (let ((test-framework::*dont-run-tests-when-loading-asdf-systems* t))
    (asdf:operate 'asdf:load-op asdf-system))

  ;; create dummy monitors
  (let* ((original-data-recorders 
	  (loop for data-recorder in data-recorders
	     for monitor = (monitors::get-monitor (read-from-string data-recorder))
	     unless (and monitor 
			 (subtypep (monitors::get-monitor monitor) 'monitors::data-recorder))
	     do (error "~a is not a data-recorder" data-recorder)
	     collect monitor))
	 (dummy-data-recorder-lists
	  (loop repeat (length original-data-recorders)
	     for i from 1
	     collect (loop for j from 1 to (length experiment-classes)
			collect (monitors::make-monitor-unless-already-defined
				 (intern (mkstr "dummy-data-recorder-" i "-" j))
				 ''data-recorder
				 (list :documentation "This is a dummy monitor created by create-combined-graphs-from-different-runs. Please ignore it")))))
	 (dummy-graphic-generators
	  (loop for dummy-data-recorder-list in dummy-data-recorder-lists
	     for parameters in parameters-for-graphic-generators
	     for average in average-data
	     for i from 1
	     collect (monitors::make-monitor-unless-already-defined
		      (intern (mkstr "dummy-gnuplot-graphic-generator-" i))
		      ''gnuplot-graphic-generator
		      `(:documentation "This is a dummy monitor created by create-combined-graphs-from-different-runs. Please ignore it"
			:data-sources ',(loop for dummy-data-recorder in dummy-data-recorder-list
					   for id = (monitors::id dummy-data-recorder)
					   collect (if average `(average ,id) id))
			:caption ',captions
			,@parameters)))))

    ;; activate the monitors
    (monitors:deactivate-all-monitors)
    (loop for data-recorder in original-data-recorders
       do (monitors::activate-monitor-method data-recorder))
    (loop for dummy-graphic-generator in dummy-graphic-generators
       do (monitors::activate-monitor-method dummy-graphic-generator))
    (monitors:notify monitors:reset-monitors)
        
    ;; run the experiments
    (loop for experiment-class in experiment-classes 
       for number-of-interactions in number-of-interactions-per-experiment
       for configurations in configurations-per-experiment
       for e from 0
       do (run-client-processes
           :asdf-system asdf-system
           :package package
           :experiment-class experiment-class
           :configurations configurations
           :number-of-interactions number-of-interactions
           :number-of-series number-of-series
           :max-nr-parallel-processes max-nr-parallel-processes
           :number-of-data-points number-of-data-points
           :monitors data-recorders)
	 ;; copying the recorded data into the dummy data recorders
	 (loop for original-data-recorder in original-data-recorders
	    for r from 0
	    for dummy-data-recorder = (nth e (nth r dummy-data-recorder-lists))
	    do (setf (cdar (slot-value dummy-data-recorder 'values))
		     (cdar (slot-value original-data-recorder 'values)))
	      (setf (cdar (slot-value dummy-data-recorder 'monitors::average-values))
		     (cdar (slot-value original-data-recorder 'monitors::average-values)))
	      (monitors::handle-reset-monitors-event 
	       original-data-recorder (monitors::id original-data-recorder) 
	       'monitors::reset-monitors)))
    
    ;; create the graphs
    (format t ".. creating the graphs.")
    (notify batch-finished file-prefix)
    (format t ".. done.")))

(defun create-graphs-for-different-experimental-conditions 
       (&key asdf-system package experiment-base-class
             experiment-classes number-of-interactions number-of-series 
             data-recorders average-data captions parameters-for-graphic-generators
             (configuration nil) ;; shared configuration by all experiments
             (configurations nil) ;; or configuration per experiment
             (max-nr-parallel-processes *max-nr-parallel-processes*))
  ;; nazi checks
  (assert (typep experiment-base-class 'string))
  (assert (typep number-of-interactions 'number))
  ;; please provide either configuration or configurations, but not both
  (assert (not (and configuration configurations)))

  (print configuration)
  (create-combined-graphs-from-different-runs
   :asdf-system asdf-system :package package :file-prefix experiment-base-class
   :experiment-classes experiment-classes
   :captions captions
   :number-of-interactions-per-experiment (loop repeat (length experiment-classes)
                                                collect number-of-interactions)
   :number-of-data-points number-of-interactions
   :configurations-per-experiment (if configurations
                                    configurations
                                    (loop repeat (length experiment-classes)
                                          collect configuration))
   :number-of-series number-of-series
   :max-nr-parallel-processes max-nr-parallel-processes
   :data-recorders data-recorders :average-data average-data
   :parameters-for-graphic-generators parameters-for-graphic-generators))

(defun create-graphs-for-different-experimental-configurations
    (&key asdf-system package experiment-class
     configurations number-of-interactions number-of-series
     (max-nr-parallel-processes *max-nr-parallel-processes*)
     data-recorders average-data captions parameters-for-graphic-generators)
  ;; nazi checks
  (assert (typep number-of-interactions 'number))

  (create-combined-graphs-from-different-runs
   :asdf-system asdf-system :package package :file-prefix experiment-class
   :experiment-classes (loop repeat (length configurations) collect experiment-class)
   :captions captions
   :number-of-interactions-per-experiment (loop repeat (length configurations)
					     collect number-of-interactions)
   :number-of-data-points number-of-interactions
   :configurations-per-experiment configurations
   :number-of-series number-of-series
   :max-nr-parallel-processes max-nr-parallel-processes
   :data-recorders data-recorders :average-data average-data
   :parameters-for-graphic-generators parameters-for-graphic-generators))


(defun create-graphs-for-different-population-sizes 
    (&key asdf-system package experiment-class population-sizes
     number-of-interactions-per-agent number-of-series
     (max-nr-parallel-processes *max-nr-parallel-processes*)
     data-recorders average-data parameters-for-graphic-generators)
  (assert (typep experiment-class 'string))
  (assert (typep number-of-interactions-per-agent 'number))
  (assert (typep population-sizes 'list))
  (assert (> (length population-sizes) 0))
  (assert (loop for population-size in population-sizes
	     always (and (numberp population-size) (>= population-size 2))))
 
  (create-combined-graphs-from-different-runs
   :asdf-system asdf-system :package package :file-prefix experiment-class
   :experiment-classes (loop repeat (length population-sizes) collect experiment-class)
   :captions (loop for population-size in population-sizes 
		collect (format nil "~a agents" population-size))
   :number-of-interactions-per-experiment 
   (loop for population-size in population-sizes 
      collect (* number-of-interactions-per-agent (/ population-size 2)))
   :number-of-data-points number-of-interactions-per-agent
   :configurations-per-experiment (loop for population-size in population-sizes 
				     collect `((population-size . ,population-size)))
   :number-of-series number-of-series
   :max-nr-parallel-processes max-nr-parallel-processes
   :data-recorders data-recorders :average-data average-data
   :parameters-for-graphic-generators parameters-for-graphic-generators))



;; bar plots
(defun create-bar-plots-for-different-experimental-conditions
    (&key asdf-system package experiment-class 
     number-of-interactions number-of-series
     (max-nr-parallel-processes *max-nr-parallel-processes*)
     configurations-a configurations-b labels-a labels-b x-label
     data-recorders bar-plot-parameter-lists)
  (assert (= (length data-recorders) (length bar-plot-parameter-lists)))
  (let ((test-framework::*dont-run-tests-when-loading-asdf-systems* t))
    (asdf:operate 'asdf:load-op asdf-system))
  
  (deactivate-all-monitors)
  (loop for data-recorder in data-recorders
     for monitor = (monitors::get-monitor (read-from-string data-recorder))
     if (and monitor 
             (subtypep (monitors::get-monitor monitor) 'monitors::data-recorder))
     do (monitors::activate-monitor-method monitor)
     else do (error "~a is not a data-recorder" data-recorder))
  (loop for a in configurations-a
     collect 
     (loop for b in configurations-b
        do 
        (monitors:notify monitors:reset-monitors)
        (run-client-processes 
         :asdf-system asdf-system
         :package package
         :experiment-class experiment-class
         :configurations (append a b)
         :number-of-interactions number-of-interactions
         :number-of-series number-of-series
         :max-nr-parallel-processes max-nr-parallel-processes
         :monitors data-recorders)
        collect 
        (loop for monitor in data-recorders
           for data-recorder = (monitors::get-monitor (read-from-string monitor))
           for values =
           (mapcar #'car 
                   (cdar (slot-value data-recorder 'monitors::average-values)))
           collect (cons (average values) (stdev values))))
     into data
     finally 
     (format t "~%.. creating the graphs")
     (loop for parameters in bar-plot-parameter-lists
        for i from 0
        do (destructuring-bind (&key title y-label (error-bars t) 
                                     (key-location "above") y-min y-max 
                                     (draw-y-grid t) (grid-color "#aaaaaa")
                                     (grid-line-width 0.5) 
                                     (colors *great-gnuplot-colors*)
                                     graphic-type file-name)
               parameters
             (let ((file-name (monitors::make-file-name-with-time file-name)))
               (with-open-pipe (stream (pipe-to-gnuplot))
                 (format stream "~cset output \"~a\"" #\linefeed file-name)
                 (format stream "~cset terminal ~a" #\linefeed graphic-type)
                 (when title
                   (format stream "~cset title ~s" #\linefeed title))
                 (format stream "~cset style histogram ~a gap 1" 
                         #\linefeed (if error-bars "errorbars" "clustered"))
                 (format stream "~cset style fill solid" #\linefeed)
                 (format 
                  stream 
                  "~cset grid back ~:[noytics~;ytics lt 4 lc rgb \"~a\" lw ~a~]" 
                  #\linefeed draw-y-grid grid-color grid-line-width)
                 (format stream "~cset key ~a" #\linefeed key-location)
                 (format stream "~cset xlabel ~:[~;~:*~s~]" #\linefeed x-label)
                 (format stream "~cset ylabel ~:[~;~:*~s~]" #\linefeed y-label)
                 (format stream "~cset xtics nomirror (~{~{~s ~a~}~^, ~})"
                         #\linefeed
                         (loop for title in labels-b for n from 0 collect (list title n)))
                 (format stream "~cset xrange [-0.5:~,2f]" 
                         #\linefeed (- (length (car data)) 0.5))
                 (format 
                  stream 
                  "~cset ytics nomirror~cset yrange [~:[*~;~:*~d~]:~:[*~;~:*~d~]]" 
                  #\linefeed #\linefeed y-min y-max)

                 (format stream "~cplot " #\linefeed)
                 (loop for i from 0 to (- (length data) 1)
                    for color = (nth (mod i (length colors)) colors)
                    do (format 
                        stream 
                        "'-' title ~s with histograms lt -1 lw 2 lc rgb ~s~:[~;, ~]"
                        (nth i labels-a) color
                        (< i (- (length data) 1))))
                 (loop for a in data
                    do (loop for b in a 
                          for (value . stdev) = (nth i b)
                          do (format stream "~c~,3f ~:[~;~,3f~]" #\linefeed 
                                     value error-bars stdev))
                    (format stream "~ce"  #\linefeed)))
               (format t "~%.. wrote ~a" file-name))))
       (format t "~%.. done.")))


(defun create-bar-plot-for-different-experimental-conditions
    (&key asdf-system package experiment-classes data-recorders 
     number-of-interactions-per-experiment number-of-series
     (max-nr-parallel-processes *max-nr-parallel-processes*)
     configurations-a configurations-b labels-a labels-b
     title (x-label nil) (y-label nil) 
     (error-bars t) (key-location "above") y-min y-max 
     (draw-y-grid nil) (grid-color "#aaaaaa") (grid-line-width 0.5)
     (colors *great-gnuplot-colors*) graphic-type file-name)
  
  (let ((test-framework::*dont-run-tests-when-loading-asdf-systems* t))
    (asdf:operate 'asdf:load-op asdf-system))
  
  (loop for data-recorder in data-recorders
     for monitor = (monitors::get-monitor (read-from-string data-recorder))
     do (unless (and monitor 
		     (subtypep (monitors::get-monitor monitor) 'monitors::data-recorder))
	  (error "~a is not a data-recorder" data-recorder)))
    
  (loop for a in configurations-a
     collect 
       (loop for b in configurations-b
	  for experiment-class in experiment-classes
	  for data-recorder in data-recorders
	  for number-of-interactions in number-of-interactions-per-experiment
	  for monitor = (monitors::get-monitor (read-from-string data-recorder))
	  do 
	    (monitors::deactivate-all-monitors)
	    (monitors::activate-monitor-method monitor)
            (monitors:notify monitors:reset-monitors)
            (run-client-processes 
             :asdf-system asdf-system
             :package package
             :experiment-class experiment-class
             :configurations (append a b)
             :number-of-interactions number-of-interactions
             :number-of-series number-of-series
             :max-nr-parallel-processes max-nr-parallel-processes
             :monitors (list data-recorder))
	  collect 
            (let ((values 
                   (mapcar #'car 
                           (cdar (slot-value 
                                  monitor 'monitors::average-values)))))
              (list (average values) (stdev values))))
     into data
     finally (format t ".. creating the graph.")
       (let ((file-name (monitors::make-file-name-with-time file-name)))
         (with-open-pipe (stream (pipe-to-gnuplot))
           (format stream "~cset output \"~a\"" #\linefeed file-name)
           (format stream "~cset terminal ~a" #\linefeed graphic-type)
           (when title
             (format stream "~cset title ~s" #\linefeed title))
           (format stream "~cset style histogram ~a gap 1" 
                   #\linefeed (if error-bars "errorbars" "clustered"))
           (format stream "~cset style fill solid" #\linefeed)
           (format stream "~cset grid back ~:[noytics~;ytics lt 4 lc rgb \"~a\" lw ~a~]" 
                   #\linefeed draw-y-grid grid-color grid-line-width)
           (format stream "~cset key ~a" #\linefeed key-location)
           (format stream "~cset xlabel ~:[~;~:*~s~]" #\linefeed x-label)
           (format stream "~cset ylabel ~:[~;~:*~s~]" #\linefeed y-label)
           (format stream "~cset xtics nomirror (~{~{~s ~a~}~^, ~})" #\linefeed
                   (loop for title in labels-b for n from 0 collect (list title n)))
           (format stream "~cset xrange [-0.5:~,2f]" 
                   #\linefeed (- (length (car data)) 0.5))
           (format stream "~cset ytics nomirror~cset yrange [~:[*~;~:*~d~]:~:[*~;~:*~d~]]" 
                   #\linefeed #\linefeed y-min y-max)
	 
	 
           (format stream "~cplot " #\linefeed)
           (loop for i from 0 to (- (length data) 1)
              for color = (nth (mod i (length colors)) colors)
              do (format stream "'-' title ~s with histograms lt -1 lc rgb ~s~:[~;, ~]"
                         (nth i labels-a) color
                         (< i (- (length data) 1))))
           (loop for a in data
              for i from 0
              do (loop for b in a 
                    do (format stream "~c~,3f ~:[~;~,3f~]" #\linefeed 
                               (first b) error-bars (second b)))
              (format stream "~ce"  #\linefeed)))
         (format t "~%.. wrote ~a" file-name))
       (format t "~%.. done.")))

(defun create-configuration-a-vs-configuration-b-bar-plot 
    (&key asdf-system package experiment-class data-recorder 
     number-of-interactions number-of-series
     (max-nr-parallel-processes *max-nr-parallel-processes*)
     configurations-a configurations-b labels-a labels-b
     title (x-label nil) (y-label nil) 
     (error-bars t) (key-location "above") y-min y-max 
     (draw-y-grid nil) (grid-color "#aaaaaa") (grid-line-width 0.5)
     (colors *great-gnuplot-colors*) graphic-type file-name)
  (create-bar-plot-for-different-experimental-conditions
   :asdf-system asdf-system :package package 
   :experiment-classes (loop repeat (length configurations-b)
			  collect experiment-class)
   :data-recorders (loop repeat (length configurations-b)
		      collect data-recorder)
   :number-of-interactions-per-experiment (loop repeat (length configurations-b)
					     collect number-of-interactions)
   :number-of-series number-of-series
   :max-nr-parallel-processes max-nr-parallel-processes
   :configurations-a configurations-a :configurations-b configurations-b
   :labels-a labels-a :labels-b labels-b
   :title title :x-label x-label :y-label y-label
   :error-bars error-bars :key-location key-location :y-min y-min :y-max y-max
   :draw-y-grid draw-y-grid :grid-color grid-color :grid-line-width grid-line-width
   :colors colors :graphic-type graphic-type :file-name file-name))




