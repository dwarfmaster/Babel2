;;;;
;;;; File: plot-monitors.lisp
;;;;
;;;; Monitors for plotting data
;;;;

(in-package :monitors)

;; ############################################################################
;; pipe-to-gnuplot
;; ----------------------------------------------------------------------------

(export 'pipe-to-gnuplot)

(defun pipe-to-gnuplot ()
  "Starts a gnuplot process and returns a stream for writing to that process"
   (pipe-output #+(or :win32 :windows) "pgnuplot" 
                #-(or :win32 :windows) "gnuplot"
                :args (list "-persist" "-") :wait nil))


;; ############################################################################
;; gnuplotter
;; ----------------------------------------------------------------------------

(export '*great-gnuplot-colors*)
(defparameter *great-gnuplot-colors*
  '("#328888" "dark-goldenrod" "dark-red" "navy" "dark-green" "gray30" "light-red" "green"
    "dark-orange" "royalblue" "sea-green" "dark-pink" "purple" "orange-red" "gray50" "dark-khaki"
    "dark-turquoise" "salmon" "dark-magenta"  "dark-yellow" "violet" "light-green"))

(defclass gnuplotter (data-handler)
  ((title
    :documentation "The title of the plot."
    :type string
    :initarg :title
    :accessor title)
   (minimum-number-of-data-points 
    :documentation "At least that many values are plotted along the x-axis.
        It can be more depending on the dynamically adapted step size."
    :initform 500 :initarg :minimum-number-of-data-points
    :accessor minimum-number-of-data-points)
   (error-bars :documentation "When a symbol is provided here, error
   bars are plotted. Current options to calculate error bars include
   standard deviation (:stdev) and minimum maximum values (:min-max)"
	       :type symbol :initarg :error-bars :accessor error-bars :initform :stdev)
   (key-location :documentation "Where the key is placed (this is directly passed to 'set key')"
		 :type string :initform "below" :initarg :key-location :accessor key-location)
   (caption :documentation "A list of strings containing a caption for each source"
	    :type list :initarg :caption :initform nil :reader caption)
   (y1-max :documentation "The maximum for the left y axis. Nil results in automatic scaling"
	   :initarg :y1-max :initform nil :reader y1-max)
   (y1-min :documentation "The minimum value for the left y axis. Nil means automatic scaling"
	   :initarg :y1-min :initform nil :reader y1-min)
   (y2-max :documentation "The maximum for the right y axis. Nil means automatic scaling"
	   :initarg :y2-max :initform nil :reader y2-max)
   (y2-min :documentation "The minimum for the right y axis. Nil means automatic scaling"
           :initarg :y2-min :initform nil :reader y2-min)
   (lines :documentation "Whether you want lines connecting the datapoints."
          :initarg :lines :initform t :reader lines :type symbol)
   (line-width :documentation "The width of the lines for curves and errorbars"
	       :initarg :line-width :initform 2 :reader line-width :type integer)
   (draw-y1-grid :documentation "Whether to draw horizontal lines at the y1 ticks"
		 :initform nil :initarg :draw-y1-grid :reader draw-y1-grid)
   (draw-y2-grid :documentation "Whether to draw horizontal lines at the y2 ticks"
		 :initform nil :initarg :draw-y2-grid :reader draw-y2-grid)
   (use-y-axis :documentation "A list containing the number 1 or 2 depending on whether the 
                               data sis scaled for the left or the right y axis" 
	       :type list :initarg :use-y-axis :initform nil :reader use-y-axis)
   (x-label :documentation "A label for the x axis"
	    :type (or string null) :initarg :x-label :initform nil :reader x-label)
   (y1-label :documentation "A label for the left y-axis"
	     :type (or string null) :initarg :y1-label :initform nil :reader y1-label)
   (y2-label :documentation "A label for the right y-axis"
	     :type (or string null) :initarg :y2-label :initform nil :reader y2-label)
   (stream :documentation "Where the plot-data method writes its output"
	   :reader plot-stream :initform nil)
   (colors :documentation "A list of line colors to use for plotting." 
	   :accessor colors :initform *great-gnuplot-colors*)
   (divide-indices-by :documentation "A constant by which the indices (x-values) are divided by."
		      :accessor divide-indices-by :initform 1 :initarg :divide-indices-by))
  (:documentation "Generic class for plotting with gnuplot"))


(defmethod initialize-instance :around ((monitor gnuplotter) 
					&key data-sources caption line-width
					y1-min y1-max y2-min y2-max use-y-axis
                                        (title nil)
					&allow-other-keys)
  (call-next-method)
  (setf (error-occured-during-initialization monitor) t)
  (when y1-max (check-type y1-max number))
  (when y1-min (check-type y1-min number))
  (when y2-max (check-type y2-max number))
  (when y2-min (check-type y2-min number))
  (when line-width (check-type line-width integer))
  (when (and use-y-axis  (or (not (listp use-y-axis)) 
			     (not (= (length use-y-axis) (length data-sources)))
			     (not (loop for x in use-y-axis
				     always (and (numberp x)
						 (or (= x 1) (= x 2)))))))
    (error "use-y-axis should be a list containing 1 or 2 for each data-source"))
  (when (and caption (or (not (listp caption))
			 (not (= (length caption) (length data-sources)))
			 (not (loop for x in caption always (or (not x) (stringp x))))))
    (error "caption should be a list containing a string or nil for each data-source"))
  (setf (error-occured-during-initialization monitor) nil)
  (unless title (setf (title monitor) (format nil "~(~a~)" (id monitor)))))

(defparameter *plot-with-averaged-by-step* nil)

;; parameters:
;; - data:
;;     - data form: (source-data-set*)
;;     - source-data-set form: (batch-data*), i.e. all data from one 
(defun compute-index-and-data-points-and-error-bars-aux
       (data minimum-number-of-data-points error-bars divide-indices-by
             &key (steps nil))
  "computes for each source in data a (index data-points error-bars) list.
   data is a list batches of series of values, as recorded for example by data-recorders"
  (loop
   for source in data
   ;; the length of the longest series determines the range
   for range = (loop for series in source maximize (length series))
   ;; the minimum number of samples used
   for s = (/ range minimum-number-of-data-points)
   ;; in order to have error bars at nice positions, round them
   for _steps = (or steps 
                    (cond ((<= s 1) 1) ((<= s 5) 2) ((<= s 10) 5)
                          ((<= s 20) 10) ((<= s 25) 20) ((<= s 50) 25)
                          ((<= s 100) 50) ((<= s 250) 100) ((<= s 500) 250)
                          ((<= s 1000) 500) ((<= s 2500) 1000) ((<= s 500) 2500)
                          (t 5000)))
   ;; error bars at nice positions
   for error-bar-distance = (cond ((<= range 101) 10)
                                  ((<= range 201) 20)
                                  ((<= range 501 50)) 
                                  ((<= range 1001) 100)
                                  ((<= range 2001) 200)
                                  ((<= range 5001) 500)
                                  ((<= range 10001) 1000) 
                                  ((<= range 20001) 2000)
                                  ((<= range 50001) 5000)
                                  ((<= range 100001) 10000)
                                  ((<= range 200001) 20000)
                                  ((<= range 500000) 50000)
                                  ((<= range 1000001) 100000)
                                  (t 200000))
   collect
   (loop
    for i from 0 to (- range 1) by _steps
    for values =
    (remove
     nil
     (loop
      for series in source
      unless (< (length series) (+ i 1))
      collect
      (if (and *plot-with-averaged-by-step* (> i 0))
        (let ((vs (remove
                   nil
                   (loop for j from (- i (- _steps 1)) to i
                         collect (nth (- (length series) j 1)
                                      series)))))
          (when vs (average vs)))
        (nth (- (length series) i 1) series))))
    for average = (when values (average values))
    collect (/ i divide-indices-by) into index
    collect average into average-values
    when (and values
              error-bars
              (> (length source) 1)
              (> i 0) (= (mod i error-bar-distance) 0)
              (> (length values) 1))
    collect (if (eq error-bars :min-max)
              (let ((max (loop for el in values maximize el))
                    (min (loop for el in values minimize el)))
                (list (/ i divide-indices-by) average 
                      min max))
              ;; default :stdev
              (let ((stdev (stdev values :average average)))
                (list (/ i divide-indices-by) average 
                      (- average stdev) (+ average stdev))))
    into errorbars
    finally (return (list index average-values errorbars)))))

(defun compute-index-and-data-points-and-error-bars (data monitor)
  "computes for each source in data a (index data-points error-bars) list.
   data is a list batches of series of values, as recorded for example by data-recorders"
  (compute-index-and-data-points-and-error-bars-aux
   (mapcar #'car data)
   (minimum-number-of-data-points monitor)
   (error-bars monitor)
   (divide-indices-by monitor)))

(defgeneric plot-data (monitor)
  (:documentation "Writes the data to the stream of the monitor"))

(defun plot-data-aux (data stream &key key-location
                      use-y-axis y1-min y1-max y2-min y2-max
                      caption
                      monitor-ids-of-sources
                      (lines t)
                      (line-width 2)
                      (colors *great-gnuplot-colors*)
                      (draw-y1-grid nil) (draw-y2-grid nil)
                      (x-label nil)
                      (y1-label nil)
		      (y2-label nil)
                      (grid-color "#aaaaaa")
                      (grid-line-width 0.5))
  (assert stream)
  (format stream "~cset grid back noxtics" #\linefeed)
  (format stream "~cset grid back ~:[noytics~;ytics lt 4 lc rgb \"~a\" lw ~a~]" 
          #\linefeed draw-y1-grid grid-color grid-line-width)
  (format stream "~cset grid back ~:[noy2tics~;y2tics lt 4 lc rgb \"~a\" lw ~a~]" 
          #\linefeed draw-y2-grid grid-color grid-line-width)
  (format stream "~cset key ~a" #\linefeed key-location)
  (format stream "~cset ytics nomirror~cset yrange [~:[*~;~:*~d~]:~:[*~;~:*~d~]]" 
          #\linefeed #\linefeed y1-min y1-max)
  (format stream "~cset xlabel ~:[~;~:*~s~]" #\linefeed x-label)
  (format stream "~cset ylabel ~:[~;~:*~s~]" #\linefeed y1-label)
  (format stream "~cset y2label ~:[~;~:*~s~]" #\linefeed y2-label)
  (if (member 2 use-y-axis)
      (format stream "~cset y2tics nomirror~cset y2range [~:[*~;~:*~d~]:~:[*~;~:*~d~]]" 
              #\linefeed #\linefeed y2-min y2-max)
      (format stream "~cunset y2tics" #\linefeed))
  (format stream "~cplot " #\linefeed)
  (loop for source in data 
     for source-number from 0
     for color = (nth (mod source-number (length colors)) colors)
     do (format stream "~:[~*~*~*~*~;'-' axes x1y~:[1~;~:*~d~] notitle with errorbars lt ~a ps 0.01 lw ~a lc rgb ~s,~] '-' axes x1y~:[1~;~:*~d~] title ~s ~a lt ~a lw ~a lc rgb ~s~:[~;, ~]"
                (third source) (nth source-number use-y-axis)
                (+ 1 (mod source-number 8)) line-width color
                (nth source-number use-y-axis)
                (if (and caption (nth source-number caption))
                    (nth source-number caption)
                    (nth source-number (reverse monitor-ids-of-sources)))
                (if lines "with lines" "with points")
                (+ 1 (mod source-number 8)) line-width color
                (< source-number (- (length data) 1))))
  (loop for source in data
     do (when (third source)
          (loop for error-bar in (third source) 
             do (format stream "~c~{~,3f ~,3f ~,3f ~,3f~}"  #\linefeed error-bar))
          (format stream "~ce"  #\linefeed))
     (mapcar #'(lambda (index average-values) 
                 (format stream "~c~,3f ~,3f"  #\linefeed index average-values))
             (first source) (second source))
     (format stream "~ce~c" #\linefeed #\linefeed)))

(defmethod plot-data ((monitor gnuplotter))
  (let ((data (compute-index-and-data-points-and-error-bars
               (reverse (sources monitor)) monitor))
	(stream (plot-stream monitor)))
    (plot-data-aux data stream
                   :draw-y1-grid (draw-y1-grid monitor)
                   :draw-y2-grid (draw-y2-grid monitor)
                   :key-location (key-location monitor)
                   :y1-min (y1-min monitor)
                   :y1-max (y1-max monitor)
                   :use-y-axis (use-y-axis monitor)
                   :y2-min (y2-min monitor)
                   :y2-max (y2-max monitor)
                   :colors (colors monitor)
                   :lines (lines monitor)
                   :line-width (line-width monitor)
		   :x-label (x-label monitor)
		   :y1-label (y1-label monitor)
		   :y2-label (y2-label monitor)
                   :caption (caption monitor)
                   :monitor-ids-of-sources (monitor-ids-of-sources monitor))))


;; ############################################################################
;; gnuplot-display
;; ----------------------------------------------------------------------------

(export '(gnuplot-display))

(defclass gnuplot-display (gnuplotter)
  ((update-interval
    :documentation "How often the display is redrawn"
    :type number
    :initarg :update-interval
    :accessor update-interval
    :initform 10))
  (:documentation "Plots values in realtime on a display using gnuplot"))

(defmethod initialize-instance :around ((monitor gnuplot-display)
					&key id &allow-other-keys)
  (let ((previous-monitor (get-monitor id)))
    (call-next-method)
    (when previous-monitor
      (setf (slot-value monitor 'stream) (plot-stream previous-monitor)))
    (subscribe-to-event id 'interaction-finished)))

(defmethod handle-interaction-finished-event :after ((monitor gnuplot-display) 
						     (monitor-id symbol) 
						     (event (eql 'interaction-finished))
						     (experiment t) (interaction t)(interaction-number number))
  (when (= (mod interaction-number (update-interval monitor)) 0)
    (unless (plot-stream monitor)
      (setf (slot-value monitor 'stream) (pipe-to-gnuplot)))
    (format (plot-stream monitor) "~cset title \"~(~a~) (~d:~d)\""
	    #\linefeed (title monitor) (length (car (first (sources monitor))))
	    (length (caar (first (sources monitor)))))
    (plot-data monitor)
    (finish-output (plot-stream monitor))))

(defmethod activate-monitor-method :after ((monitor gnuplot-display) &optional active)
  "Opens or closes a pipe to gnuplot."
  (declare (ignore active))
  (if (active monitor)
      (unless (plot-stream monitor)
	(setf (slot-value monitor 'stream) (pipe-to-gnuplot)))
      (when (plot-stream monitor)
	(close-pipe (plot-stream monitor))
	(setf (slot-value monitor 'stream) nil))))


;; ############################################################################
;; gnuplot-graphic-generator
;; ----------------------------------------------------------------------------

(export '(gnuplot-graphic-generator))

(defclass gnuplot-graphic-generator (gnuplotter)
  ((file-name
    :documentation "The file name (path) of the graphic file to produce"
    :type pathname :initarg :file-name :accessor file-name)
   (colored 
    :documentation "When t, different data lines have different colors. 
                    Only has effect in some terminals"
    :initform t :initarg :colored :accessor colored)
   (dashed 
    :documentation "When t, different data lines have different dashes. 
                    Only has effect in some terminals"
    :initform t :initarg :dashed :accessor dashed)
   (add-time-and-experiment-to-file-name
    :documentation "When t, the file name is prefixed with the name of the experiment class
                    and a yyyy-mm-dd-hh-mm-ss string"
    :initform t :initarg :add-time-and-experiment-to-file-name
    :accessor add-time-and-experiment-to-file-name)
   (allowed-types
    :documentation "These graphic types are allowed"
    :type list
    :initform '("postscript" "pdf" "svg" "gif" )
    :reader allowed-types)
   (graphic-type
    :documentation "The type of graphic to produce"
    :type string
    :initarg :graphic-type
    :reader graphic-type))
  (:documentation "Generates a graphic file at the end of run-batch"))

(defmethod initialize-instance :around ((monitor gnuplot-graphic-generator)
					&key id graphic-type file-name &allow-other-keys)
  (setf (error-occured-during-initialization monitor) t)
  (unless (pathnamep file-name)
    (error "file-name should be a pathname"))
  (setf (error-occured-during-initialization monitor) nil)
  (call-next-method)
  (setf (error-occured-during-initialization monitor) t)
  (unless (find graphic-type (allowed-types monitor) :test #'equal)
    (error "graphic-type should be one of ~a" (allowed-types monitor)))
  (subscribe-to-event id 'batch-finished)
  (setf (error-occured-during-initialization monitor) nil))


(defmethod handle-batch-finished-event ((monitor gnuplot-graphic-generator)
					(monitor-id symbol) (event (eql 'batch-finished))
					(experiment-class string))
  (let ((file-name (if (add-time-and-experiment-to-file-name monitor)
		       (make-file-name-with-time-and-experiment-class 
			(file-name monitor) experiment-class)
		       (file-name monitor))))
    (format t "~%monitor ~(~a~): generating ~a ...~%" (id monitor) file-name)
    (force-output t)
    (when (slot-value monitor 'stream)
      (close-pipe (slot-value monitor 'stream)))
    (setf (slot-value monitor 'stream) (pipe-to-gnuplot))
    (format (plot-stream monitor) "~cset output \"~a\"" #\linefeed file-name)
    (format (plot-stream monitor) "~cset terminal ~a font \"Helvetica\" linewidth ~a rounded ~:[solid~;dashed~] ~:[monochrome~;color~]"
	    #\linefeed (graphic-type monitor) (line-width monitor) 
	    (dashed monitor) (colored monitor))
    (plot-data monitor)
    (format (plot-stream monitor) "~cexit~c"  #\linefeed #\linefeed)
    (finish-output (plot-stream monitor))
    (close-pipe (slot-value monitor 'stream))
    (setf (slot-value monitor 'stream) nil)))



;; ############################################################################
;; gnuplot-display-and-graphic-generator
;; ----------------------------------------------------------------------------

(export '(gnuplot-display-and-graphic-generator))

(defclass gnuplot-display-and-graphic-generator (gnuplot-display
						 gnuplot-graphic-generator)
  ()
  (:documentation "Both displays a graph and generates a graphic file in the end"))


;; ############################################################################
;; gnuplot-raw-data-file-writer
;; ----------------------------------------------------------------------------

(export '(gnuplot-raw-data-file-writer))

(defclass gnuplot-raw-data-file-writer (gnuplotter)
  ((file-name :documentation "The file name (path) of the raw data file to produce"
	      :type pathname :initarg :file-name :accessor file-name))
  (:documentation "Generates a space separated raw data file that can be
                   processed by gnuplot at the end of run-batch"))

(defmethod initialize-instance :around ((monitor gnuplot-raw-data-file-writer)
					&key id file-name &allow-other-keys)
  (setf (error-occured-during-initialization monitor) t)
  (unless (pathnamep file-name)
    (error "file-name should be a pathname"))
  (setf (error-occured-during-initialization monitor) nil)
  (call-next-method)
  (subscribe-to-event id 'batch-finished))

(defmethod handle-batch-finished-event ((monitor gnuplot-raw-data-file-writer)
					(monitor-id symbol) (event (eql 'batch-finished))
					(experiment-class string))
  (let ((data (compute-index-and-data-points-and-error-bars (reverse (sources monitor)) monitor)))
    (with-open-file (file (file-name monitor) :direction :output)
      (loop for index in (first (first data))
	 for n from 0
	 do (format file "~d ~{~,3f~^ ~}~c" index 
		    (mapcar (lambda (x) (nth n x))
			    (mapcar #'second data)) #\linefeed)))
    (when (third (first data))
      (let ((file-name (make-pathname :directory (pathname-directory (file-name monitor))
				      :type (pathname-type (file-name monitor))
				      :name (concatenate 'string 
							 (pathname-name (file-name monitor))
							 "_errorbar"))))
	(with-open-file (file file-name :direction :output)
	  (loop for index in (mapcar #'first (third (first data)))
	     for n from 0
	     do (format file "~d ~{~{~,3f ~,3f ~,3f~^ ~}~^ ~}~c" index 
			(mapcar (lambda (x) (cdr (nth n x)))
				(mapcar #'third data)) #\linefeed)))))))


;; ############################################################################
;; gnuplot-file-writer
;; ----------------------------------------------------------------------------

(export '(gnuplot-file-writer))

(defclass gnuplot-file-writer (gnuplot-graphic-generator)
  ((gnuplot-file-name
    :documentation "The file name (path) of the gnuplot file to produce"
    :type pathname
    :initarg :gnuplot-file-name
    :accessor gnuplot-file-name))
  (:documentation "Generates a complete gnuplot file that can generate a graphic
     file at the end of run-batch"))

(defmethod initialize-instance :around ((monitor gnuplot-file-writer)
					&key id gnuplot-file-name &allow-other-keys)
  (setf (error-occured-during-initialization monitor) t)
  (unless (pathnamep gnuplot-file-name)
    (error "gnuplot-file-name should be a pathname"))
  (setf (error-occured-during-initialization monitor) nil)
  (call-next-method)
  (subscribe-to-event id 'batch-finished))

(defmethod handle-batch-finished-event ((monitor gnuplot-file-writer)
					(monitor-id symbol) (event (eql 'batch-finished))
					(experiment-class string))
  (with-open-file (file (gnuplot-file-name monitor) :direction :output)
    (setf (slot-value monitor 'stream) file)
    (format (plot-stream monitor) "~cset output \"~a\"" #\linefeed (file-name monitor))
    (format (plot-stream monitor) "~cset terminal ~a font \"Helvetica\" linewidth ~a rounded ~:[solid~;dashed~] ~:[monochrome~;color~]"
	    #\linefeed (graphic-type monitor) (line-width monitor) 
	    (dashed monitor) (colored monitor))
    (plot-data monitor)
    (setf (slot-value monitor 'stream) nil)))

;; ############################################################################
