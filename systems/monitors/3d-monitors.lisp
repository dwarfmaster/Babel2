;;;;
;;;; File 3d-monitors.lisp
;;;;
;;;; Monitors for plotting unspecified 3d plots.
;;;; 

(in-package :monitors)

(export '(3d-recorder record-value 3d-printer 3d-gnuplot-display
	  3d-gnuplot-file-writer 
	  3d-gnuplot-graphic-generator
	  3d-gnuplot-movie-generator
	  3d-gnuplot-display-and-graphic-generator
	  3d-gnuplot-display-and-movie-generator
	  3d-gnuplot-graphic-and-movie-generator
	  3d-gnuplot-display-and-graphic-and-movie-generator))

;; ############################################################################
;; 3d-recorder
;; ----------------------------------------------------------------------------

(defclass 3d-recorder (monitor)
  ((values
    :documentation "Contains a list of 3d points (also a list) for each
interaction."
    :initform (list nil) 
    :accessor get-values)
   (current-values 
    :documentation "A list of 3d points for the current  interaction."
    :initform nil 
    :accessor current-values)
   (record-once-per-series
    :documentation "When t, this recorder only stores one value per series."
    :initform nil
    :accessor record-once-per-series
    :initarg :record-once-per-series)
   (only-last-value
    :documentation "When t, this recorder only stores the most recent value
per series."
    :initform nil
    :accessor only-last-value
    :initarg :only-last-value))
  (:documentation "Records 3d values."))

(defmethod initialize-instance :around ((monitor 3d-recorder) 
					&key id &allow-other-keys)
  (let* ((previous-monitor (get-monitor id)))
    (call-next-method)
    (when previous-monitor ;; copy the previous data
      (setf (slot-value monitor 'values) 
	    (get-values previous-monitor))
      (setf (slot-value monitor 'current-values) 
	    (current-values previous-monitor))
      (setf (slot-value monitor 'record-once-per-series)
	    (record-once-per-series previous-monitor))
      (setf (slot-value monitor 'only-last-value)
	    (only-last-value previous-monitor)))
    (subscribe-to-event id 'interaction-started)
    (subscribe-to-event id 'interaction-finished)
    (subscribe-to-event id 'series-finished)
    (subscribe-to-event id 'reset-monitors)))

(defmethod handle-interaction-finished-event :after 
    ((monitor 3d-recorder) 
     (monitor-id symbol)
     (event (eql 'interaction-finished))
     (experiment t)
     (interaction t)
     (interaction-number number))
  (with-slots (current-values values record-once-per-series
			      only-last-value) monitor
    (unless (and record-once-per-series (caar values))
      ;; first we check whether all values are lists of length 3 and each
      ;; element of that list is a number; if it contains a fourth element
      ;; it should be a string that will be plotted at the location
      ;; specified by the first three elements of the point
       (loop for 3d-point in current-values
 	 do (assert (and (>= (length 3d-point) 3)
 			 (subtypep (type-of (first 3d-point)) 'number)
 			 (subtypep (type-of (second 3d-point)) 'number)
 			 (subtypep (type-of (third 3d-point)) 'number)))
         (assert (or (= (length 3d-point) 3)
                     (subtypep (type-of (fourth 3d-point)) 'string)))
         (assert (not (> (length 3d-point) 4))))
      ;; store current-values
      (if only-last-value
	  (setf (caar values) (list current-values))
	  (push current-values (caar values))))))

(defmethod handle-series-finished-event :after ((monitor 3d-recorder) 
						(monitor-id symbol)
						(event (eql 'series-finished))
						(series-number number))
  ;; add a nil as 'seed' for the list of the next series
  (push nil (car (slot-value monitor 'values))))

(defmethod handle-reset-monitors-event :before ((monitor 3d-recorder) 
						(monitor-id symbol)
						(event (eql 'reset-monitors)))
  ;; funny things to keep pointer in handlers alive
  (setf (car (slot-value monitor 'values)) nil)
  (push nil (car (slot-value monitor 'values))))

(defmethod record-value ((monitor 3d-recorder) (values t))
  (setf (slot-value monitor 'current-values) values))

;; ############################################################################
;; 3d-handler
;; ----------------------------------------------------------------------------

(defclass 3d-handler (monitor)
  ((sources :documentation "Pointers to the values slot of the 
associated recorders."
	    :initform nil :reader sources)
   (monitor-ids-of-sources
    :documentation "The monitor ids of the assocciated recorders."
    :type cons :initform nil :reader monitor-ids-of-sources))
  (:documentation "Handles the recorded data of a 3d-recorder."))

(defmethod initialize-instance :around ((monitor 3d-handler) 
					&key data-sources 
					&allow-other-keys)
  (setf (error-occured-during-initialization monitor) t)
  (unless data-sources (error "Please provide :data-sources."))
  (check-type data-sources list)
  (setf (error-occured-during-initialization monitor) nil)
  (call-next-method)
  (setf (error-occured-during-initialization monitor) t)
  (loop for data-source in data-sources
     do (let ((3d-recorder (get-monitor data-source)))
	  (unless 3d-recorder (error "Monitor ~a is not defined." 3d-recorder))
	  (unless (typep 3d-recorder '3d-recorder)
	    (error "Monitor ~a is not of type 3d-recorder." 3d-recorder))
	  (push data-source (slot-value monitor 'monitor-ids-of-sources))
	  (push (get-values 3d-recorder) (slot-value monitor 'sources))))
  (setf (error-occured-during-initialization monitor) nil))

(defmethod activate-monitor-method :after ((monitor 3d-handler) 
					   &optional active)
  (when active
    (dolist (monitor-id (monitor-ids-of-sources monitor))
      (when (active (get-monitor monitor-id))
	(activate-monitor-method monitor-id nil))
      (activate-monitor-method monitor-id t))))

;; ############################################################################
;; 3d-printer
;; ----------------------------------------------------------------------------

(defclass 3d-printer (3d-handler)
  ((interval :initarg :interval :accessor interval :initform 1
	     :documentation "Only every interval-th interaction is 
printed."))
  (:documentation "Prints the values of a 3d-recorder after each 
interval-th interaction."))

(defmethod initialize-instance :around ((monitor 3d-printer) 
					&key id &allow-other-keys)
  (call-next-method)
  (subscribe-to-event id 'interaction-finished))

(defmethod handle-interaction-finished-event :around 
    ((monitor 3d-printer)
     (monitor-id symbol)
     (event (eql 'interaction-finished))
     (experiment t) 
     (interaction t)
     (interaction-number number))
  (call-next-method)
  (when (= (mod interaction-number (interval monitor)) 0)
    (format t "~%~a: " interaction-number)
    (loop for source in (sources monitor)
       for monitor-id in (monitor-ids-of-sources monitor)
       do (format t "~%  ~a" monitor-id)
	 (loop for 3d-point in (caaar source)
	    do (format t "~%    ~{~a ~}" 3d-point)))))

;; ############################################################################
;; 3d-gnuplotter
;; ----------------------------------------------------------------------------

(defclass 3d-gnuplotter (3d-handler)
  ((stream :documentation "Where the plotter writes it output."
	   :reader plot-stream :initform nil)
   (colour-fn
    :documentation "Function which takes in a 3d point and returns a rgb colour
in gnuplot format, for example 0x000000 for black or 0x3CFD00 for green."
    :type function 
    :initarg :colour-fn 
    :accessor colour-fn 
    :initform #'(lambda (3d-point) (declare (ignore 3d-point)) 
			"0x3CFD00"))
   (xyz-labels
    :documentation "A list containing a symbol for each axis."
    :type cons
    :initarg :xyz-labels
    :accessor xyz-labels
    :initform nil)
   (point-sizes
    :documentation "The point-sizes of the scatter plot."
    :type cons
    :initarg :point-sizes
    :accessor point-sizes
    :initform nil)
   (point-types
    :documentation "The point-types of the scatter plot."
    :type cons
    :initarg :point-types
    :accessor point-types
    :initform nil)
   (title
    :documentation "The title of the plot."
    :type string
    :initarg :title
    :accessor title)
   (x-max
    :documentation "The maximum for the x axis. Nil results in automatic scaling"
    :type number
    :initarg :x-max
    :initform nil
    :reader x-max)
   (x-min
    :documentation "The minimum for the x axis. Nil results in automatic scaling"
    :type number
    :initarg :x-min
    :initform nil
    :reader x-min)
   (y-max
    :documentation "The maximum for the y axis. Nil results in automatic scaling"
    :type number
    :initarg :y-max
    :initform nil
    :reader y-max)
   (y-min
    :documentation "The minimum for the y axis. Nil results in automatic scaling"
    :type number
    :initarg :y-min
    :initform nil
    :reader y-min)
   (z-max
    :documentation "The maximum for the z axis. Nil results in automatic scaling"
    :type number
    :initarg :z-max
    :initform nil
    :reader z-max)
   (z-min
    :documentation "The minimum for the z axis. Nil results in automatic scaling"
    :type number
    :initarg :z-min
    :initform nil
    :reader z-min)
   (random-offset
    :documentation "When not nil adds a random offset to the position of the labels which
      can be provided as an extra string as a fourth element in the data-points of the 
      data-sources. When all data-sources consist of true 3d-points, this value is ignored."
    :type boolean
    :initarg :random-offset
    :initform nil
    :reader random-offset))
  (:documentation "A gnuplotter based on the 3d-recorder."))

(defmethod initialize-instance :around ((monitor 3d-gnuplotter) 
					&key
					&allow-other-keys)
  (call-next-method)
  (setf (error-occured-during-initialization monitor) t)
  (with-slots (xyz-labels point-sizes point-types title x-min x-max
			  y-min y-max z-min z-max) monitor
    (when (and xyz-labels (not (= (length xyz-labels) 3)))
      (error "xyz-labels should be a list of three symbols"))
    (when x-max (check-type x-max number))
    (when x-min (check-type x-min number))
    (when y-max (check-type y-max number))
    (when y-min (check-type y-min number))
    (when z-max (check-type z-max number))
    (when z-min (check-type z-min number))
    (when title (check-type title string))
    (unless (= (length (monitor-ids-of-sources monitor))
	       (length point-sizes) (length point-types))
      (error "number of point-sizes should be equal to the number of point-types should
be equal to the number of data-sources.")))
  (setf (error-occured-during-initialization monitor) nil))

(defun plot-3d-data (monitor sets-of-3d-points)
  (with-slots (stream colour-fn xyz-labels x-max x-min y-max 
		      y-min z-max z-min random-offset) monitor
    (format stream "~cset hidden3d" #\linefeed)
    (if xyz-labels
	(progn
	  (format stream "~cset xlabel '~a'" #\linefeed 
		  (first xyz-labels))
	  (format stream "~cset ylabel '~a'" #\linefeed 
		  (second xyz-labels))
	  (format stream "~cset zlabel '~a'" #\linefeed 
		  (third xyz-labels)))
	(progn
	  (format stream "~cunset xlabel" #\linefeed)
	  (format stream "~cunset ylabel" #\linefeed)
	  (format stream "~cunset zlabel" #\linefeed)))
    (format stream "~cset xrange [~:[*~;~:*~d~]:~:[*~;~:*~d~]]" 
	    #\linefeed x-min x-max)
    (format stream "~cset yrange [~:[*~;~:*~d~]:~:[*~;~:*~d~]]" 
	    #\linefeed y-min y-max)
    (format stream "~cset zrange [~:[*~;~:*~d~]:~:[*~;~:*~d~]]" 
	    #\linefeed z-min z-max)
    (format stream "~cunset label" #\linefeed)
    (let ((true-3d-sets nil)
	  (labeled-3d-sets nil))
      (loop for set-of-3d-points in sets-of-3d-points
	 for point-type in (reverse (point-types monitor))
	 for point-size in (reverse (point-sizes monitor))
	 for 3d-set = (list set-of-3d-points point-type point-size)
	 do (cond ((= (length (first set-of-3d-points)) 3)
		   (push 3d-set true-3d-sets))
		  ((= (length (first set-of-3d-points)) 4)
		   (push 3d-set labeled-3d-sets))))
      (when labeled-3d-sets
	(loop for labeled-3d-set in labeled-3d-sets
	   do (loop for 4d-point in (first labeled-3d-set)
		 do (format stream "~cset label \"~a\" at ~a, ~a, ~a center tc rgb \"~a\" offset ~a, ~a, ~a"
			    #\linefeed
			    (fourth 4d-point)
			    (first 4d-point)
			    (second 4d-point)
			    (third 4d-point)
			    (let ((hex-colour (funcall colour-fn (subseq 4d-point 0 3))))
			      (setf (char hex-colour 1) #\#)
			      (subseq hex-colour 1))
                            (if random-offset
                                (if (> (random 1.0) 0.5) (- (random 1.0)) (random 1.0)) 0)
                            (if random-offset
                                (if (> (random 1.0) 0.5) (- (random 1.0)) (random 1.0)) 0)
                            (if random-offset
                                (if (> (random 1.0) 0.5) (- (random 1.0)) (random 1.0)) 0)))))
      (when true-3d-sets
	(format stream "~csplot " #\linefeed)
	(loop for (true-3d-set . remaining-sets) on true-3d-sets
	   do (format stream "'-' using 1:2:3:4 notitle with points pt ~a ps ~a lc rgb variable"
		     (second true-3d-set) (third true-3d-set))
	     (when remaining-sets
	       (format stream ", ")))
	(loop for true-3d-set in true-3d-sets
	   do (loop for 3d-point in (first true-3d-set)
		 do (format stream "~c~{~a ~}~a" #\linefeed 3d-point 
			    (funcall colour-fn 3d-point)))
	     (format stream "~ce~c"  #\linefeed #\linefeed))))))

;; ############################################################################
;; 3d-gnuplot-display
;; ----------------------------------------------------------------------------

(defclass 3d-gnuplot-display (3d-gnuplotter)
  ((display-update-interval
    :documentation "How often the display is redrawn."
    :type number :initarg :display-update-interval
    :accessor display-update-interval :initform 10))
  (:documentation "Plots values in realtime on a display using gnuplot."))

(defmethod initialize-instance :around ((monitor 3d-gnuplot-display)
					&key id &allow-other-keys)
  (let ((previous-monitor (get-monitor id)))
    (call-next-method)
    (when previous-monitor
      (setf (slot-value monitor 'stream) (plot-stream previous-monitor)))
    (subscribe-to-event id 'interaction-finished)))

(defmethod handle-interaction-finished-event :around 
    ((monitor 3d-gnuplot-display)
     (monitor-id symbol)
     (event (eql 'interaction-finished))
     (experiment t)
     (interaction t)
     (interaction-number number))
  (call-next-method)
  (when (or (= (mod interaction-number (display-update-interval monitor)) 0)
	    (= interaction-number 1))
    (unless (plot-stream monitor)
      (setf (slot-value monitor 'stream) (pipe-to-gnuplot)))
    (format (slot-value monitor 'stream) "~cset term x11" #\linefeed)
    (format (slot-value monitor 'stream) "~cset title \"~a (~a)\"" #\linefeed
	    (title monitor) interaction-number)
    (plot-3d-data monitor (mapcar #'caaar (sources monitor))) 
    (finish-output (plot-stream monitor))))

(defmethod activate-monitor-method :after ((monitor 3d-gnuplot-display) 
					   &optional active)
  "Opens or closes a pipe to gnuplot."
  (declare (ignore active))
  (if (active monitor)
      (unless (plot-stream monitor)
	(setf (slot-value monitor 'stream) (pipe-to-gnuplot)))
      (when (plot-stream monitor)
	(close-pipe (plot-stream monitor))
	(setf (slot-value monitor 'stream) nil))))

;; ############################################################################
;; 3d-gnuplot-graphic-generator
;; ----------------------------------------------------------------------------

(defclass 3d-gnuplot-graphic-generator (3d-gnuplotter)
  ((graphic-file-name 
    :documentation "The file name (path) of the graphic file to produce."	      
    :type pathname 
    :initarg :graphic-file-name 
    :accessor graphic-file-name))
  (:documentation "Generates a graphic file at the end of each series."))

(defmethod initialize-instance :around ((monitor 3d-gnuplot-graphic-generator)
					&key id graphic-file-name 
					&allow-other-keys)
  (setf (error-occured-during-initialization monitor) t)
  (unless (pathnamep graphic-file-name)
    (error ":graphic-file-name should be a pathname"))
  (setf (error-occured-during-initialization monitor) nil)
  (call-next-method)
  (setf (error-occured-during-initialization monitor) t)
  (subscribe-to-event id 'series-finished)
  (setf (error-occured-during-initialization monitor) nil))

(defmethod handle-series-finished-event ((monitor 3d-gnuplot-graphic-generator) 
					 (monitor-id symbol)
					 (event (eql 'series-finished))
					 (series-number number))
  (with-slots (graphic-file-name stream) monitor
    (unless (plot-stream monitor)
      (setf (slot-value monitor 'stream) (pipe-to-gnuplot)))
    (let ((file-spec 
	   (mkstr (make-pathname
		   :directory (pathname-directory graphic-file-name))
		  (pathname-name graphic-file-name) "-"
		  series-number "." (pathname-type graphic-file-name))))
      (format t "~% - generating graphic ~a" file-spec)
      (format stream "~cset term postscript" #\linefeed)
      (format stream "~cset output \"~a\"" #\linefeed file-spec)
      (format stream "~cset title \"~a\"" #\linefeed 
	      (title monitor))
      (plot-3d-data monitor (mapcar #'(lambda (source)
					(first (cadar source)))
				    (sources monitor)))
      (finish-output stream))))
				  
(defmethod activate-monitor-method :after
    ((monitor 3d-gnuplot-graphic-generator) 
     &optional active)
  "Opens or closes a pipe to gnuplot."
  (declare (ignore active))
  (if (active monitor)
      (unless (plot-stream monitor)
	(setf (slot-value monitor 'stream) (pipe-to-gnuplot)))
      (when (plot-stream monitor)
	(close-pipe (plot-stream monitor))
	(setf (slot-value monitor 'stream) nil))))

;; ############################################################################
;; 3d-gnuplot-movie-generator
;; ----------------------------------------------------------------------------

#+(or sbcl lispworks)
(defclass 3d-gnuplot-movie-generator (3d-gnuplotter)
  ((movie-file-name 
    :documentation "The file name (path) of the graphic file to produce."	      
    :type pathname 
    :initarg :movie-file-name 
    :accessor movie-file-name)
   (frame-filenames 
    :documentation "Contains the file-names of the movieframes."
    :type cons
    :initform nil
    :accessor frame-filenames)
   (movie-update-interval
    :type number
    :initarg :movie-update-interval
    :accessor movie-update-interval
    :initform 25)
   (delta-rotz
    :documentation "How many degrees the view is turned after each frame."
    :type number
    :accessor delta-rotz
    :initarg :delta-rotz
    :initform 1))
  (:documentation "Generates a movie file at the end of each series."))

#+(or sbcl lispworks)
(defmethod initialize-instance :around ((monitor 3d-gnuplot-movie-generator)
					&key id movie-file-name 
					&allow-other-keys)
  (setf (error-occured-during-initialization monitor) t)
  (unless (pathnamep movie-file-name)
    (error ":movie-file-name should be a pathname"))
  (setf (error-occured-during-initialization monitor) nil)
  (call-next-method)
  (setf (error-occured-during-initialization monitor) t)
  (subscribe-to-event id 'series-finished)
  (setf (error-occured-during-initialization monitor) nil))

#+(or sbcl lispworks)
(defmethod handle-series-finished-event :after
    ;; note: this method has to be a :after method because otherwise
    ;;  an instance of a mixed class of both 3d-gnuplot-movie-generator 
    ;;  and 3d-gnuplot-graphic generator would only call one handle-
    ;;  series-finished-event
    ((monitor 3d-gnuplot-movie-generator) 
     (monitor-id symbol)
     (event (eql 'series-finished))
     (series-number number))
  (with-slots (movie-file-name frame-filenames movie-update-interval 
			       delta-rotz stream) monitor
    (setf frame-filenames nil)
    (let ((base-file-name
	   (mkstr (make-pathname
		   :directory (pathname-directory movie-file-name))
		  (pathname-name movie-file-name) "-"
		  series-number))
	  (interaction-number 0) 
	  (rotz 30))
      (unless (plot-stream monitor)
	(setf (slot-value monitor 'stream) (pipe-to-gnuplot)))
      (format stream "~cset term postscript" #\linefeed)
      ;; first we generate all frames
      (let ((values-for-series
	     (mapcar #'(lambda (source) (reverse (cadar source)))
		     (sources monitor))))
	(loop for sets-of-3d-points 
	   in (reduce #'(lambda (prev new)
			  (mapcar #'(lambda (x y) (push x y))
				  new prev))
		      (reverse values-for-series)
		      :initial-value (make-list 
				      (length (first values-for-series))
				      :initial-element nil))
	   do (progn 
		(incf interaction-number)
		(when (= (mod interaction-number movie-update-interval) 0)
		  (push (mkstr base-file-name "-"
			       (format nil "~8,'0d" interaction-number)
			       ".ps")
			frame-filenames)
		  (format t "~% - generating ~a" (car frame-filenames))
		  (format stream "~cset output \"~a\"" #\linefeed 
			  (car frame-filenames))
		  (format stream "~cset title \"~a (~a)\"" #\linefeed 
			  (title monitor) interaction-number)
		  (format stream "~cset view ,~a" #\linefeed rotz)
		  (plot-3d-data monitor sets-of-3d-points)
		  (finish-output stream)
		  (setf rotz (mod (+ rotz delta-rotz) 360))))))
      ;; next we use the linux convert command to collect all
      ;; frames in to a single file (only tested mpg so far)
      (let ((mpeg-file-name (mkstr base-file-name "." 
				   (pathname-type movie-file-name))))
	(format t "~% - generating ~a ..." mpeg-file-name)
        ;; lispworks version not tested yet
	(let ((process
               #+lispworks
               (system::call-system
                (format nil "convert~{ ~a~} -rotate 90 -quality 100 ~a"
                        (reverse frame-filenames)
                        mpeg-file-name))
	       #+sbcl
               (sb-ext::run-program 
		"convert" 
		(append (reverse frame-filenames)
			(list "-rotate" "90" "-quality" "100" mpeg-file-name))
		:search t)
               ))
	  (if #+sbcl
              (= (sb-ext:process-exit-code process) 0)
              #+lispworks
              (= 0 process)
              (format t " done")
              (warn "The command to create the output movie file 
returned a non-zero exit-code. Please verify whether both 'convert' and the 
'mpeg2encode' commands are readily available in your terminal."))))
      ;; finally we remove all frame files
      #+sbcl (sb-ext::run-program "rm" (reverse frame-filenames) :search t)
      #+lispworks (system::call-system (format nil "rm~{ ~a~}" (reverse frame-filenames)))
      (setf frame-filenames nil))))

#+(or sbcl lispworks)
(defmethod activate-monitor-method :after ((monitor 3d-gnuplot-movie-generator) 
					   &optional active)
  "Opens or closes a pipe to gnuplot."
  (declare (ignore active))
  (if (active monitor)
      (unless (plot-stream monitor)
	(setf (slot-value monitor 'stream) (pipe-to-gnuplot)))
      (when (plot-stream monitor)
	(close-pipe (plot-stream monitor))
	(setf (slot-value monitor 'stream) nil))))

;; ############################################################################
;; mixed classes
;; ----------------------------------------------------------------------------

(defclass 3d-gnuplot-display-and-graphic-generator 
    (3d-gnuplot-display 3d-gnuplot-graphic-generator)
  ()
  (:documentation "Both displays a graph and generates a graphic file at the 
end of each series."))

(defclass 3d-gnuplot-display-and-movie-generator 
    (3d-gnuplot-display 3d-gnuplot-movie-generator)
  ()
  (:documentation "Both displays a graph and generates a movie file at the end
of each series."))

(defclass 3d-gnuplot-graphic-and-movie-generator
    (3d-gnuplot-graphic-generator 3d-gnuplot-movie-generator)
 ()
  (:documentation "Both displays generates a graphic and a movie file at the end
of each series."))

(defclass 3d-gnuplot-display-and-graphic-and-movie-generator 
    (3d-gnuplot-display 3d-gnuplot-graphic-generator 3d-gnuplot-movie-generator)
  ()
  (:documentation "Both displays a graph, generates a graphic file and 
generates a movie file at the end of each series."))

;; ############################################################################
;; 3d-gnuplot-file-writer
;; ----------------------------------------------------------------------------

(defclass 3d-gnuplot-file-writer (3d-gnuplotter)
  ((gnuplot-file-name
    :documentation "The file name (path) of the gnuplot file to produce"
    :type pathname
    :initarg :gnuplot-file-name
    :accessor gnuplot-file-name)
   (only-final-state
    :documentation "When t, only writes final state to file."
    :type boolean
    :initarg :only-final-state
    :accessor only-final-state
    :initform t)
   (file-writer-interval
    :documentation "How often the intermediatery data states are written."
    :type integer
    :accessor file-writer-interval
    :initarg :file-writer-interval
    :initform 10)
   (delta-rotz
    :documentation "How many degrees the view is turned after each write."
    :type number
    :accessor delta-rotz
    :initarg :delta-rotz
    :initform 0.1))
  (:documentation "Generates a complete gnuplot script that can generate 
a 3d plot."))

(defmethod initialize-instance :around ((monitor 3d-gnuplot-file-writer)
					&key id gnuplot-file-name 
					&allow-other-keys)
  (setf (error-occured-during-initialization monitor) t)
  (unless (pathnamep gnuplot-file-name)
    (error "gnuplot-file-name should be a pathname"))
  (setf (error-occured-during-initialization monitor) nil)
  (call-next-method)
  (subscribe-to-event id 'series-finished))

(defmethod handle-series-finished-event ((monitor 3d-gnuplot-file-writer) 
					 (monitor-id symbol)
					 (event (eql 'series-finished))
					 (series-number number))
  (with-slots (stream gnuplot-file-name) monitor
    (let ((file-spec
	   (mkstr (make-pathname
		   :directory (pathname-directory gnuplot-file-name))
		  (pathname-name gnuplot-file-name) "-"
		  series-number "." (pathname-type gnuplot-file-name))))
      (format t "~& - writing gnuplot file ~a" file-spec)
      (with-open-file (file-stream file-spec :direction :output)
	(setf stream file-stream)
	(format stream "~cset term x11" #\linefeed)
	;; this code assumes that an extra nil is already pushed to
	;; values by series-finished of recorder
	(if (only-final-state monitor)
	    (progn
	      (format stream "~cset title \"~a\"" #\linefeed 
		      (title monitor))
	      (plot-3d-data monitor 
			    (mapcar #'(lambda (source)
					(first (cadar source)))
				    (sources monitor))))
	    (let ((rotz 30) (interaction-number 0)
		  (values-for-series
		   (mapcar #'(lambda (source)
			       (reverse (cadar source)))
			   (sources monitor))))
	      (loop for sets-of-3d-points 
		 in (reduce #'(lambda (prev new)
				(mapcar #'(lambda (x y)
					    (push x y))
					new prev))
			    (reverse values-for-series)
			    :initial-value (make-list 
					    (length (first values-for-series))
					    :initial-element nil))
		 do (progn
		      (when (= (mod (incf interaction-number)
				    (file-writer-interval monitor)) 0)
			(plot-3d-data monitor sets-of-3d-points)
			(format stream "~cset view ,~a" #\linefeed rotz)
			(format stream "~cset title \"~a (~a)\"" #\linefeed
				(title monitor) interaction-number)
			(setq rotz (mod (incf rotz (delta-rotz monitor))  
					360))))))))
      (setf stream nil))))