(in-package :plot-raw-data)

(export '(raw-files->evo-plot))

;; ########## Main functions #########

(defun raw-files->evo-plot
    (&key
     (raw-file-paths '(error "Please provide :raw-file-paths, e.g. ((\"my-exp\" \"raw-data\" \"success\") ((\"my-exp\" \"raw-data\"\"coherence\"))"))
     (title nil) (captions nil)
     (average-windows (mapcar #'(lambda (path) (declare (ignore path)) 100) raw-file-paths))
     (plot-file-name (reduce #'(lambda (str1 str2) (string-append str1 "+" str2)) 
                             raw-file-paths :key #'(lambda (path) (first (last path)))))
     (plot-directory (butlast (first raw-file-paths))) (graphic-type "pdf")
     (file-type "lisp")
     (start nil) (end nil) (series-numbers nil)
     (key-location "below") (points nil)
     use-y-axis y1-min y1-max y2-min y2-max
     (line-width 2) (colors *great-gnuplot-colors*)
     (draw-y1-grid t) (draw-y2-grid nil)
     (x-label "Number of games played") (y1-label nil) (y2-label nil)
     (grid-line-width 0.5)
     (divide-indices-by 1)
     (average-mode :mean)
     (logscale nil) (fsize 10)
     (error-bars :stdev) ;; '(:percentile 5 95)
     (error-bar-modes '(:filled)) ;; '(:lines :filled)
     (open t))
  "Takes the :raw-file-paths and generates one single merged evo-plot
for them. An evo-plot is a line-plot that has number of games on the
x-axis."
  (when (numberp average-windows) ;; support for passing a single number as average-windows
    (setf average-windows (mapcar  #'(lambda (path) (declare (ignore path)) average-windows) 
                                   raw-file-paths)))
  (when (and (listp average-windows) 
             (not (= (length raw-file-paths) (length average-windows))))
    (error "Length of raw-file-names should be equal to length of
     average-windows, or it can be a single number."))
  (let ((colors (loop for color in colors
                   for other-color in *great-gnuplot-colors*
                   collect (or color other-color)))
        ;; reads in the data, does quick preprocessing, then does
        ;; averaging and finally returns batch-points
        (data-set (collect-data-for-evo-plots raw-file-paths
                                              :file-type file-type
                                              :start start
                                              :end end
                                              :series-numbers series-numbers
                                              :windows average-windows))
        (captions (loop for file-path in raw-file-paths
                     for i from 0
                     collect (or (nth i captions)
                                 (first (last file-path))))))
    (plot-evo-data data-set
                   :file-name plot-file-name
                   :directory plot-directory
                   :graphic-type graphic-type
                   :key-location key-location
                   :use-y-axis use-y-axis
                   :y1-min y1-min
                   :y1-max y1-max
                   :y2-min y2-min
                   :y2-max y2-max
                   :caption captions
                   :title title
		   :logscale logscale
                   :line-width line-width
                   :colors colors
                   :draw-y1-grid draw-y1-grid
                   :draw-y2-grid draw-y2-grid
                   :x-label x-label
                   :y1-label y1-label
                   :y2-label y2-label
                   :grid-line-width grid-line-width
                   :divide-indices-by divide-indices-by
                   :error-bars error-bars
                   :error-bar-modes error-bar-modes
                   :average-mode average-mode
		   :points points :fsize fsize
                   :open open)))

;; ##################### evo-plot creation ######################

(defun compute-data-points (data &key 
                            (minimum-number-of-data-points 500) (divide-indices-by 1) 
                            (error-bars nil) (average-mode :mean))
  (let* ((data (loop for source in data
                  collect (reverse source))))
    (compute-index-and-data-points
     data 
     ;; I think this value is way too big (for speed I'm just putting a fixed 500 now, like with monitors)
     ;(/ (length (car (car data))) 2) 
     minimum-number-of-data-points
     error-bars divide-indices-by average-mode)))

(defun nearest-multiple (x m)
  "Finds the nearest number from x upward that is divisible by m"
  (if (= (mod x m) 0)
      x
      (nearest-multiple (1+ x) m)))

(defun compute-index-and-data-points
       (data-set minimum-number-of-data-points error-bars
        divide-indices-by average-mode &key (steps nil))
  "computes for each source in data-set a (index data-points error-bars) list.
   data is a list batches of series of values, as recorded for example
   by data-recorders"
  (loop for source in data-set
     for counter from 0
     ;; the length of the longest series determines the range
     for range = (length source)
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
     for error-bar-distance =  (cond ((<= range 101) 10)
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
     ;; offset so that error-bars don't overlap making them unreadable
     for error-bar-offset = (* (nearest-multiple (floor (/ error-bar-distance (length data-set))) _steps) counter)
       ;; nearest-multiple for making sure i doesn't jump over mod
       ;; offset and distance (for lines errorbars)
     collect
       (loop
          for i from 0 to (- range 1) by _steps
          for batch-point = (nth (- (length source) i 1) source)
          collect (/ i divide-indices-by) into index
          collect (case average-mode (:median (median-val batch-point))
                        (t (avg-val batch-point))) into average-values          
          when (and batch-point
                    error-bars
                    (> (length source) 1)
                    (or (= (mod i (/ error-bar-distance 10)) 0)
                        (= i (- range 2)))
                    (> (length (batch-data batch-point)) 1))
          collect 
            (cons (/ i divide-indices-by)
                  (cond ((eq error-bars :min-max)
                         (list (min-val batch-point) (max-val batch-point)))
                        ((and (consp error-bars) 
                              (eq (first error-bars) :percentile))
                         (list (percentile batch-point (second error-bars))
                               (percentile batch-point (third error-bars))))
                        ((and (consp error-bars)
                              (> (length error-bars) 1)
                              (eq (first error-bars) :stdev))
                         (list (- (case average-mode (:median (median-val batch-point))
                                    (t (avg-val batch-point))) (* (second error-bars) (stdev-val batch-point)))
                               (+ (case average-mode (:median (median-val batch-point))
                                    (t (avg-val batch-point))) (* (second error-bars) (stdev-val batch-point)))))
                        (t ;; default :stdev
                         (list (- (case average-mode (:median (median-val batch-point))
                                        (t (avg-val batch-point))) (stdev-val batch-point)) 
                               (+ (case average-mode (:median (median-val batch-point))
                                        (t (avg-val batch-point))) (stdev-val batch-point))))))
          into errorbars-filled
          when (and batch-point
                    error-bars
                    (> (length source) 1)
                    (> i 0)
                    (= (mod (+ i error-bar-offset) error-bar-distance) 0)
                    (> (length (batch-data batch-point)) 1))
          collect 
            (append (list (/ i divide-indices-by) (case average-mode (:median (median-val batch-point))
                                                        (t (avg-val batch-point))))
                    (cond ((eq error-bars :min-max)
                           (list (min-val batch-point) (max-val batch-point)))
                          ((and (consp error-bars) 
                                (eq (first error-bars) :percentile))
                           (list (percentile batch-point (second error-bars))
                                 (percentile batch-point (third error-bars))))
                          ((and (consp error-bars)
                                (> (length error-bars) 1)
                                (eq (first error-bars) :stdev))
                           (list (- (case average-mode (:median (median-val batch-point))
                                      (t (avg-val batch-point))) (* (second error-bars) (stdev-val batch-point)))
                                 (+ (case average-mode (:median (median-val batch-point))
                                      (t (avg-val batch-point))) (* (second error-bars) (stdev-val batch-point)))))
                          (t ;; default :stdev
                           (list (- (case average-mode (:median (median-val batch-point))
                                          (t (avg-val batch-point))) (stdev-val batch-point)) 
                                 (+ (case average-mode (:median (median-val batch-point))
                                          (t (avg-val
          batch-point))) (stdev-val batch-point)))))) into errorbars-lines 
          finally (return (list index average-values
          errorbars-filled errorbars-lines)))))


(defun plot-evo-data (data &key 
                      (file-name "evo-plot")
                      (directory '(".tmp"))
                      (key-location "below")
                      (use-y-axis (error "Please provide :use-y-axis: A list of one y-axis assignemts (0 or 1) per data source."))
                       y1-min y1-max y2-min y2-max
                      (caption (error "Please provide :caption: A list of one captions per data source."))
                      monitor-ids-of-sources                    
                      (line-width 2)
                      (colors *great-gnuplot-colors*)
                      (draw-y1-grid nil) ;;(draw-y2-grid nil)
                      (x-label nil)
                      (y1-label nil)
		      (y2-label nil) 
                      (grid-line-width 0.5)
                      (graphic-type "pdf")
                      (dashed t)
                      ;; (colored t) ;; not used?
		      (points nil)
                      (error-bars nil)
                      (error-bar-modes '(:lines))
                      (title "")
		      (logscale nil)
                      (divide-indices-by 1)
                      (average-mode :mean)
                      (open t)
                      (fsize 10)
                      (typeface "Helvetica")
                      &allow-other-keys)
  (let ((data (compute-data-points data :divide-indices-by divide-indices-by
                                   :error-bars error-bars :average-mode average-mode))
        (file-path (babel-pathname :name file-name
                                   :type (if (equal graphic-type "postscript") "ps" graphic-type)
                                   :directory directory)))
    (ensure-directories-exist file-path)
    (with-open-stream
      (stream (monitors::pipe-to-gnuplot))
    (set-gnuplot-parameters stream
                            :output file-path :terminal graphic-type :title title
                            :draw-y1-grid draw-y1-grid :grid-line-width grid-line-width
                            :key-location key-location :x-label x-label :y1-label y1-label
                            :y2-label y2-label :y1-min y1-min :y1-max y1-max
                            :y2-min y2-min :y2-max y2-max
			    :dashed dashed :fsize fsize :typeface typeface)
    (when logscale (intern (downcase (mkstr logscale)))
	  (format stream "~cset logscale ~a" #\linefeed 
		  (intern (downcase (mkstr logscale)))))
    (format stream "~cset grid back noxtics" #\linefeed)
    
    (format stream "~cset ytics nomirror" #\linefeed)
    (format stream "~cset y2tics nomirror" #\linefeed)
    (unless (member 2 use-y-axis)
      (format stream "~cunset y2tics" #\linefeed))
    (format stream "~cset style fill transparent solid 0.20 border" #\linefeed)
    (format stream "~cplot " #\linefeed)

    (when (member :filled error-bar-modes)
      (loop for source in data 
         for source-number from 0
         for color = (nth (mod source-number (length colors)) colors)
         do (when (third source)
              (format stream "'-' axes x1y~:[1~;~:*~d~] notitle with filledcurves lc rgb ~s,"
                      (nth source-number use-y-axis) color ;; (1+ (rem source-number 2))
                      ))))
    (when (member :lines error-bar-modes)
      (loop for source in data 
         for source-number from 0
         for color = (nth (mod source-number (length colors)) colors)
         do (when (fourth source)
            (format stream "'-' axes x1y~:[1~;~:*~d~] notitle with errorbars lw ~a dt ~a lc rgb ~s,"
                    (nth source-number use-y-axis) line-width (+ 2 (mod source-number 8)) color))))
    (loop for source in data 
       for source-number from 0
       for color = (nth (mod source-number (length colors)) colors)
       do (format stream "'-' axes x1y~:[1~;~:*~d~] title ~s ~a  dt ~a lc rgb ~s ~:[~;, ~]"
                  (nth source-number use-y-axis)
                  (if (and caption (nth source-number caption))
                      (nth source-number caption)
                      (nth source-number (reverse monitor-ids-of-sources)))
		  (if points
		      "with points 3"
		      (format nil "with lines lw ~a" line-width))
                    (+ 2 (mod source-number 8)) color
                  (< source-number (- (length data) 1))))

    (when (member :filled error-bar-modes)
      (loop for source in data
         do (when (third source) ;; error-bars
              (loop for error-bar in (third source) 
                 do (format stream "~c~{~,3f ~,3f ~,3f~}"  #\linefeed error-bar))
              (format stream "~ce"  #\linefeed))))
    (when (member :lines error-bar-modes)
      (loop for source in data
         do (when (fourth source) ;; error-bars
              (loop for error-bar in (fourth source) 
                 do (format stream "~c~{~,3f ~,3f ~,3f ~,3f~}"  #\linefeed error-bar))
              (format stream "~ce"  #\linefeed))))
    (loop for source in data
       do (mapcar #'(lambda (index average-values) 
                      (format stream "~c~,3f ~,3f"  #\linefeed index average-values))
                  (first source) (second source))
         (format stream "~ce~c" #\linefeed #\linefeed))
  (format stream "~cexit~c"  #\linefeed #\linefeed)
  (finish-output stream)
  (close-pipe stream)
  (when open (sleep 0.5) (open-file-in-os file-path)))))

