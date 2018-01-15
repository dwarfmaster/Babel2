;;;;
;;;; File alist-monitors.lisp
;;;;
;;;; Monitors for plotting unspecified alists
;;;; 

(in-package :monitors)

(export '(alist-recorder set-value-for-symbol incf-value-for-symbol
	  alist-printer
	  alist-gnuplot-display alist-gnuplot-graphic-generator
	  alist-gnuplot-display-and-graphic-generator))

;; #####################################################################
;; alist-recorder
;; ---------------------------------------------------------------------

(defclass alist-recorder (monitor)
  ((values
    :documentation "An alist of symbols and batches of series of 'values' for each interaction"
    :initform nil :reader get-values)
   (average-values
    :documentation "An alist of symbols and the averaged values for the batches of series .."
    :initform (list nil) :reader average-values)
   (current-values :documentation "An alist of symbols and current values"
		   :initform nil :accessor current-values)
   (empty-list 
    :documentation "A list with the same structure as the previous
                    three but nils instead of values. This list is
                    copied when a new symbol shows up."
    :initform (list (list nil)) :accessor empty-list)
   (first-value-positions :documentation "The position of the first value of each symbol"
			  :initform nil :accessor first-value-positions)
   (average-window
    :documentation "Values are averaged over the last n interactions"
    :initarg :average-window :initform 100 :accessor average-window)
   (keep-previous-values 
    :documentation "When t and no value was recorded for a particular
                    symbol, then the value of the previous interaction
                    is used, otherwise 0"
    :initarg :keep-previous-values :accessor keep-previous-values
    :initform nil))
  
  (:documentation "Records averaged values for a alist"))


(defmethod initialize-instance :around ((monitor alist-recorder) 
					&key id &allow-other-keys)
  (let* ((previous-monitor (get-monitor id)))
    (call-next-method)
    (when previous-monitor ;; copy the previous data
      (setf (slot-value monitor 'values) (get-values previous-monitor))
      (setf (slot-value monitor 'average-values) (average-values previous-monitor))
      (setf (slot-value monitor 'current-values) (current-values previous-monitor))
      (setf (slot-value monitor 'empty-list) (empty-list previous-monitor))
      (setf (slot-value monitor 'first-value-positions)
            (first-value-positions previous-monitor)))
    (subscribe-to-event id 'interaction-started)
    (subscribe-to-event id 'interaction-finished)
    (subscribe-to-event id 'series-finished)
    (subscribe-to-event id 'reset-monitors)))

(defmethod handle-interaction-started-event :before ((monitor alist-recorder) (monitor-id symbol)
						     (event (eql 'interaction-started))
						     (experiment t)(interaction t)(interaction-number number))
  (declare (ignorable experiment interaction interaction-number))
  ;; set the current values of each symbol to 0
  (unless (keep-previous-values monitor)
    (loop for cons in (current-values monitor)
       do (setf (cdr cons) 0))))

(defmethod handle-interaction-finished-event :after ((monitor alist-recorder) (monitor-id symbol)
						     (event (eql 'interaction-finished))
						     (experiment t) (interaction t)(interaction-number number))
  ;; store the current-values and compute average values
  (declare (ignorable experiment interaction interaction-number))
  (push nil (caar (empty-list monitor)))
  (loop for values-cons in (get-values monitor)
     for average-values-cons in (car (average-values monitor))
     for current-value-cons in (current-values monitor)
     for first-value-position-cons in (first-value-positions monitor)
     for range = (min (average-window monitor)
		      (+ 1 (- (length (caar (cdr values-cons))) (cdr first-value-position-cons))))
     do (push (cdr current-value-cons) (caar (cdr values-cons)))
     (push (/ (apply #'+ (subseq (caar (cdr values-cons)) 0 range)) range)
	   (caar (cdr average-values-cons)))))

(defmethod handle-series-finished-event :after ((monitor alist-recorder) (monitor-id symbol)
						(event (eql 'series-finished))
						(series-number number))
  (push nil (car (empty-list monitor)))
  (loop for values-cons in (get-values monitor)
     for average-values-cons in (car (average-values monitor))
     for first-value-position-cons in (first-value-positions monitor)
     do (push nil (car (cdr values-cons)))
       (push nil (car (cdr average-values-cons)))
       (push nil (cdr first-value-position-cons))))

(defmethod handle-reset-monitors-event :before ((monitor alist-recorder) (monitor-id symbol)
						(event (eql 'reset-monitors)))
  (setf (slot-value monitor 'values) nil)
  (setf (car (slot-value monitor 'average-values)) nil)
  (setf (cdr (slot-value monitor 'average-values)) nil)
  (setf (slot-value monitor 'current-values) nil)
  (setf (slot-value monitor 'first-value-positions) nil)
  (setf (slot-value monitor 'empty-list) (list (list nil))))


;; #####################################################################
;; set-value-for-symbol and incf-value-for-symbol
;; ---------------------------------------------------------------------

(defgeneric set-value-for-symbol (monitor symbol value)
  (:documentation "Sets a value for a symbol in a alist recorder"))

(defmethod set-value-for-symbol ((monitor alist-recorder) (symbol symbol) (value number))
  (let ((current-value-cons (assoc symbol (current-values monitor))))
    (if current-value-cons
	(setf (cdr current-value-cons) value)
	(progn
	  (push (cons symbol (mapcar #'copy-list (empty-list monitor)))
                (slot-value monitor 'values))
	  (push (cons symbol (mapcar #'copy-list (empty-list monitor))) 
		(car (slot-value monitor 'average-values)))
	  (push (cons symbol (length (caar (empty-list monitor)))) 
		(slot-value monitor 'first-value-positions))
	  (push (cons symbol value) (slot-value monitor 'current-values))))))

(defgeneric incf-value-for-symbol (monitor symbol value)
  (:documentation "Increases the current value for symbol (initallly 0) by value"))

(defmethod incf-value-for-symbol ((monitor alist-recorder) (symbol symbol) (value number))
  (let ((current-value-cons (assoc symbol (current-values monitor))))
    (if current-value-cons
	(incf (cdr current-value-cons) value)
	(set-value-for-symbol monitor symbol value))))


;; #####################################################################
;; alist-handler
;; ---------------------------------------------------------------------

(defclass alist-handler (monitor)
  ((data :documentation "A pointer to the average-values slot of the associated recorder"
	 :initform nil :reader data)
   (recorder :documentation "The id of the assocciated recorder"
	     :initarg :recorder :type symbol :reader recorder))
  (:documentation "Handles the recorded data of an alist-recorder"))

(defmethod initialize-instance :around ((monitor alist-handler) 
					&key recorder &allow-other-keys)
  (setf (error-occured-during-initialization monitor) t)
  (unless recorder (error "Please provide a :recorder."))
  (setf (error-occured-during-initialization monitor) nil)
  (call-next-method)
  (setf (error-occured-during-initialization monitor) t)
  (let ((alist-recorder (get-monitor recorder)))
    (unless alist-recorder (error "Monitor ~a is not defined" recorder))
    (unless (typep alist-recorder 'alist-recorder)
      (error "Monitor ~a is not of type alist-recorder" recorder))
    (setf (slot-value monitor 'data) (average-values alist-recorder)))
  (setf (error-occured-during-initialization monitor) nil))

(defmethod activate-monitor-method :after ((monitor alist-handler) &optional active)
  ;; deactivate the recorder and activate it then again to make sure it is 
  ;; is in front of the 'active-monitors' list of 'interaction-finished etc.
  (when (active (get-monitor (recorder monitor)))
    (activate-monitor-method (recorder monitor) nil))
  (when active 
    (activate-monitor-method (recorder monitor) t)))


;; #####################################################################
;; alist-printer
;; ---------------------------------------------------------------------


(defclass alist-printer (alist-handler)
  ((interval :initarg :interval :accessor interval :initform 1
	     :documentation "Only every nth interaction is printed"))
  (:documentation "Prints the values of an alist recorder after each interaction"))

(defmethod initialize-instance :around ((monitor alist-printer) &key id &allow-other-keys)
  (call-next-method)
  (subscribe-to-event id 'interaction-finished))

(defmethod handle-interaction-finished-event :around ((monitor alist-printer)
						      (monitor-id symbol)
						      (event (eql 'interaction-finished))
						      (experiment t) 
						      (interaction t)
                              (interaction-number number))
  (call-next-method)
  (when (= (mod interaction-number (interval monitor)) 0)
    (format t "~%~a: " interaction-number)
    (loop for (symbol . average-values) in (reverse (car (data monitor)))
       do (format t "~(~a~): ~,2f; " symbol (caaar average-values)))))


;; #####################################################################
;; alist-plotter
;; ---------------------------------------------------------------------

(defclass alist-gnuplotter (alist-handler)
  ((minimum-number-of-data-points 
    :documentation "At least that many values are plotted along the x-axis.
                    It can be more depending on the dynamically adapted step size."
    :initform 500 :initarg :minimum-number-of-data-points :accessor minimum-number-of-data-points)
   (error-bars :documentation "When t, error bars are plotted"
	       :type boolean :initarg :error-bars :accessor error-bars :initform nil)
   (key-location :documentation "Where the key is placed (this is directly passed to 'set key')"
		 :type string :initform "below" :initarg :key-location :accessor key-location)
   (y-max :documentation "The maximum for the left y axis. Nil results in automatic scaling"
	  :initarg :y-max :initform nil :reader y-max)
   (y-min :documentation "The minimum value for the left y axis. Nil means automatic scaling"
	  :initarg :y-min :initform nil :reader y-min)
   (x-label :documentation "A label for the x axis"
	    :type (or string null) :initarg :x-label :initform nil :reader x-label)
   (y-label :documentation "A label for the y axis"
	    :type (or string null) :initarg :y-label :initform nil :reader y-label)
   (line-width :documentation "The width of the lines for curves and errorbars"
	       :initarg :line-width :initform 2 :reader line-width :type integer)
   (draw-y-grid :documentation "Whether to draw horizontal lines at the y axis ticks"
		:initform nil :initarg :draw-y-grid :reader draw-y-grid)
   (hide-legend :documentation "Whether to draw a legend"
		:initform nil :initarg :hide-legend :reader hide-legend)
   (display-threshold :documentation "Whether to draw a set of data
   points (average score above threshold)"
		:initform 0 :initarg :display-threshold :reader display-threshold)
   (stream :documentation "Where the plot-data method writes its output"
	   :reader plot-stream :initform nil)
   (colors :documentation "A list of line colors to use for plotting." 
	   :accessor colors :initform *great-gnuplot-colors*)
   (divide-indices-by :documentation "A constant by which the indices (x-values) are divided by."
		      :accessor divide-indices-by :initform 1 :initarg :divide-indices-by))
  (:documentation "A gnuplotter based on the alist-recorder"))



(defmethod initialize-instance :around ((monitor alist-gnuplotter) 
					&key y-min y-max line-width x-label y-label
					&allow-other-keys)
  (call-next-method)
  (setf (error-occured-during-initialization monitor) t)
  (when y-max (check-type y-max number))
  (when y-min (check-type y-min number))
  (when line-width (check-type y-min number))
  (when x-label (check-type x-label string))
  (when y-label (check-type y-label string))
  (setf (error-occured-during-initialization monitor) nil))



(defmethod plot-data ((monitor alist-gnuplotter))
  (let ((data (compute-index-and-data-points-and-error-bars 
	       (mapcar #'cdr (reverse (car (data monitor))))
	       monitor))
	(stream (plot-stream monitor)))
    (assert stream)
    (format stream "~cset grid back noxtics" #\linefeed)
    (format stream "~cset grid back ~:[noytics~;ytics lt 4 lc rgb \"#aaaaaa\" lw 0.5~]" 
	    #\linefeed (draw-y-grid monitor))
    (format stream "~cset key ~a" #\linefeed (key-location monitor))
    (when (hide-legend monitor)
      (format stream "~cunset key" #\linefeed ))
    (format stream "~cset xlabel ~:[~;~:*~s~]" #\linefeed (x-label monitor))
    (format stream "~cset ylabel ~:[~;~:*~s~]" #\linefeed (y-label monitor))
    (format stream "~cset ytics nomirror~cset xrange [0:*]~cset yrange [~:[*~;~:*~d~]:~:[*~;~:*~d~]]" 
	    #\linefeed #\linefeed #\linefeed (y-min monitor) (y-max monitor))
    (format stream "~cunset y2tics" #\linefeed)
    (format stream "~cplot " #\linefeed)
    (setf data
	  (remove-if #'(lambda (source)
			(< (average (remove nil (second source))) (display-threshold monitor)))
		     data))
    
    (loop for source in data 
       for source-number from 0
       for color = (nth (mod source-number (length (colors monitor))) (colors monitor))
       do (format stream "~:[~*~*~;'-' axes x1y1 notitle with errorbars ps 0.01 lw ~a lc rgb ~s,~] '-' axes x1y1 title \"~(~a~)\" with lines lw ~a lc rgb ~s~:[~;, ~]" 
		  (third source) (line-width monitor) color
		  (nth source-number (reverse (mapcar #'car (car (data monitor)))))
		  (line-width monitor)
		  color (< source-number (- (length data) 1))))
    (loop for source in data
       do (when (third source)
	    (loop for error-bar in (third source) 
	       do (format stream "~c~{~d ~,3f ~,3f ~,3f~}"  #\linefeed error-bar))
	    (format stream "~ce"  #\linefeed))
	 (mapcar #'(lambda (index average-value) 
		     (when average-value
		       (format stream "~c~d ~,3f"  #\linefeed index average-value)))
		 (first source) (second source))
	 (format stream "~ce~c" #\linefeed #\linefeed))))


;; ############################################################################
;; alist-gnuplot-display
;; ----------------------------------------------------------------------------

(defclass alist-gnuplot-display (alist-gnuplotter)
  ((update-interval
    :documentation "How often the display is redrawn"
    :type number :initarg :update-interval
    :accessor update-interval :initform 100))
  (:documentation "Plots values in realtime on a display using gnuplot"))


(defmethod initialize-instance :around ((monitor alist-gnuplot-display)
					&key id &allow-other-keys)
  (let ((previous-monitor (get-monitor id)))
    (call-next-method)
    (when previous-monitor
      (setf (slot-value monitor 'stream) (plot-stream previous-monitor)))
    (subscribe-to-event id 'interaction-finished)))

(defmethod handle-interaction-finished-event :around ((monitor alist-gnuplot-display)
						      (monitor-id symbol)
						      (event (eql 'interaction-finished))
						      (experiment t)
						      (interaction t)
                              (interaction-number number))
  (call-next-method)
  
  (when (= (mod interaction-number (update-interval monitor)) 0)
    (unless (plot-stream monitor)
      (setf (slot-value monitor 'stream) (pipe-to-gnuplot)))
    (format (plot-stream monitor) "~cset title \"~(~a~) (~d:~d)\""
	    #\linefeed (id monitor) (length (car (cdaar (data monitor))))
	    (length (caar (cdaar (data monitor)))))
    (plot-data monitor)
    (finish-output (plot-stream monitor))
    ))


(defmethod activate-monitor-method :after ((monitor alist-gnuplot-display) &optional active)
  "Opens or closes a pipe to gnuplot."
  (declare (ignore active))
  (if (active monitor)
      (unless (plot-stream monitor)
	(setf (slot-value monitor 'stream) (pipe-to-gnuplot)))
      (when (plot-stream monitor)
	(close-pipe (plot-stream monitor))
	(setf (slot-value monitor 'stream) nil))))


;; #####################################################################
;; alist-gnuplot-graphic-generator
;; ---------------------------------------------------------------------


(defclass alist-gnuplot-graphic-generator (alist-gnuplotter)
  ((file-name :documentation "The file name (path) of the graphic file to produce"
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
   (allowed-types :documentation "These graphic types are allowed"
		  :type list :initform '("postscript" "pdf" "svg" "gif") :reader allowed-types)
   (graphic-type :documentation "The type of graphic to produce" 
		 :type string :initarg :graphic-type :reader graphic-type))
  (:documentation "Generates a graphic file at the end of run-batch"))

(defmethod initialize-instance :around ((monitor alist-gnuplot-graphic-generator)
					&key id graphic-type file-name &allow-other-keys)
  (setf (error-occured-during-initialization monitor) t)
  (unless (pathnamep file-name)
    (error ":file-name should be a pathname"))
  (setf (error-occured-during-initialization monitor) nil)
  (call-next-method)
  (setf (error-occured-during-initialization monitor) t)
  (unless (find graphic-type (allowed-types monitor) :test #'equal)
    (error "graphic-type should be one of ~a" (allowed-types monitor)))
  (subscribe-to-event id 'batch-finished)
  (setf (error-occured-during-initialization monitor) nil))

 

(defmethod handle-batch-finished-event ((monitor alist-gnuplot-graphic-generator)
					(monitor-id symbol) (event (eql 'batch-finished))
					(experiment-class string))
  (let ((file-name (if (add-time-and-experiment-to-file-name monitor)
		       (make-file-name-with-time-and-experiment-class 
			(file-name monitor) experiment-class)
		       (file-name monitor))))
    (format t "~%monitor ~(~a~): generating ~a~%" (id monitor) file-name)
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
;; alist-gnuplot-display-and-graphic-generator
;; ----------------------------------------------------------------------------

(defclass alist-gnuplot-display-and-graphic-generator (alist-gnuplot-display
						       alist-gnuplot-graphic-generator)
  ()
  (:documentation "Both displays a graph and generates a graphic file in the end"))


