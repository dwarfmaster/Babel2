(in-package :plot-raw-data)

(export '(raw-files->bar-plot file-structure->grouped-bar-plot file-structure->stacked-bar-plot))

;; ########## Main functions #########

(defun raw-files->bar-plot 
    (&key 
     (raw-file-paths '(error "Please provide :raw-file-paths, e.g. ((\"my-exp\" \"raw-data\" \"success\") ((\"my-exp\" \"raw-data\"\"coherence\"))"))
     (file-type "lisp")
     (plot-name (reduce #'(lambda (str1 str2) (string-append str1 "+" str2)) 
                        raw-file-paths :key #'(lambda (path) (first (last path)))))
     (plot-directory (butlast (first raw-file-paths)))
     (data-labels (mapcar #'(lambda (path) (first (last path))) raw-file-paths)) 
     (only-x-last-interactions nil)
     (y-max nil) (y-min nil) (key-location "above")
     (logscale nil) (box-width 1.5) 
     (title nil) (x-label "") (y-label "")
     (error-bars :stdev) (open t))
  "All :raw-file-paths  are plotted in a
bar-plot. With :only-x-last-interactions you can specifiy that the bars
should only show the average of the last x measurements. If nil then
the full average will be taken."
  (let ((gnuplot-data (collect-data-for-bar-plots
                       raw-file-paths :file-type file-type 
                       :only-x-last-interactions only-x-last-interactions)))
    (create-bar-plot :data gnuplot-data
                     :plot-name plot-name
                     :plot-directory plot-directory
                     :error-bars error-bars
                     :title title
                     :y-max y-max
                     :y-min y-min
                     :data-labels data-labels :key-location key-location
                     :logscale logscale :box-width box-width
                     :x-label x-label :y-label y-label
                     :open open)))

(defun file-structure->grouped-bar-plot
    (&key (raw-file-paths '(error "Please provide :raw-file-paths, e.g. ((\"my-exp\" \"raw-data\" \"success\") ((\"my-exp\" \"raw-data\"\"coherence\"))"))
     (plot-name (reduce #'(lambda (p1 p2) 
                            (string-append p1 "+" (first (last p2)))) (first raw-file-paths)))
     (plot-directory (butlast (first (first raw-file-paths))))
     (file-type "lisp")
      ;; label for each bar in a cluster 
     (labels-a (mapcar #'(lambda (path) (first (last path))) (first raw-file-paths)))
     ;; label for each cluster of bars
     (labels-b (mapcar #'(lambda (path) (first (last (butlast (first path))))) raw-file-paths))
     (only-x-last-interactions nil) y-max y-min
     (key-location "above") (colors monitors::*great-gnuplot-colors*)
     (title nil) (x-label "") (y-label "") 
     (error-bars nil) (open t))
  "Filepath should be a list of lists of file-paths where each sublist
represents a group of bars. With :only-x-last-interactions you can specifiy that the bars should
only show the average of the last x measurements. If nil then the full
average will be taken."
  (let ((gnuplot-data (collect-data-for-grouped-bar-plots raw-file-paths 
                                                          :file-type file-type
                                                          :only-x-last-interactions only-x-last-interactions)))
    (create-bar-plot-a-vs-b :data gnuplot-data :colors colors
                            :plot-name plot-name
                            :plot-directory plot-directory
                            :error-bars error-bars
                            :title title :key-location key-location
                            :y-max y-max :y-min y-min
                            :labels-a labels-a :labels-b labels-b
                            :x-label x-label :y-label y-label
                            :open open)))

(defun file-structure->stacked-bar-plot 
    (&key 
     (raw-file-paths '(error "Please provide :raw-file-paths, e.g. ((\"my-exp\" \"raw-data\" \"success\") ((\"my-exp\" \"raw-data\"\"coherence\"))"))
     (plot-name (reduce #'(lambda (p1 p2) 
                            (string-append p1 "+" (first (last p2)))) (first raw-file-paths)))
     (plot-directory (butlast (first (first raw-file-paths))))
     ;; label for each cluster of bars
     (labels-a (mapcar #'(lambda (path) (first (last path))) (first raw-file-paths))) 
     (labels-b (mapcar #'(lambda (path) (first (last (butlast (first path))))) raw-file-paths)) ;; label for each bar in a cluster 
     (only-x-last-interactions nil) y-max 
     (title nil) (x-label "") (x-label-rotate 0) (y-label "") (open t))
  "For each directory, all of the given files will be searched and
then plotted in a stacked bar plot. With :only-x-last-interactions you can
specifiy that the bars should only show the average of the last x
measurements. If nil then the full average will be taken."
  (let ((gnuplot-data (collect-data-for-grouped-bar-plots raw-file-paths 
                                                          :only-x-last-interactions only-x-last-interactions)))
    (create-stacked-bar-plot-a-b :data gnuplot-data
                                 :plot-name plot-name
                                 :plot-directory plot-directory
                                 :title title
                                 :labels-a labels-a :labels-b labels-b 
                                 :x-label x-label :y-label y-label
                                 :rotate-xlabels-by x-label-rotate
                                 :y-max y-max 
                                 :open open)))





;; ##################### bar-plot creation ######################

          
        
;; (defun collect-data-for-grouped-file-paths (file-paths &key (only-x-last-interactions 100))
;;   "Assumes a list of lists of file-paths. Each sublist represents a
;; seperate group of bars."
;;   (loop for path-group in file-paths
;;      collect (collect-data-for-file-paths path-group :only-x-last-interactions only-x-last-interactions)))

;; adapted from Michael

(defun calculate-error-string (gnuplot-batch-point error-bars)
  (cond ((not error-bars) "")
        ((eq error-bars :stdev)
         (format nil "~,3f" (stdev-val gnuplot-batch-point)))
        ((eq error-bars :min-max)
         (format nil "~,3f ~,3f" 
                 (min-val gnuplot-batch-point) (max-val gnuplot-batch-point)))
        ((and (consp error-bars)
              (eq (first error-bars) :percentile))
         (format nil "~,3f ~,3f" 
                 (percentile gnuplot-batch-point (second error-bars))
                 (percentile gnuplot-batch-point (third error-bars))))))
  

(defun create-bar-plot
       (&key
        (data (error "Please supply :data")) ; a list of gnuplot-batch-points
        (plot-name "bar-plot") (plot-directory '(".tmp")) (graphic-type "pdf")
        error-bars ;; t, nil, :stdev, :min-max
        (title nil) y-label x-label (y-min 0) y-max
        (key-location "above")
        (box-width 1.5) (draw-y-grid t) (grid-color "#aaaaaa")
        (grid-line-width 2) (color (first monitors::*great-gnuplot-colors*))
        (line-width 1.5)
        (logscale nil) 
        (fsize 10)
        (typeface "Helvetica")
        (rotate-xlabels-by 0)
        (data-labels  '("a1" "a2"))
        (open t))
  (format t "~%.. creating bar-plot graph.")
  (let ((plot-path (babel-pathname :directory plot-directory
                                   :name plot-name
                                   :type graphic-type)))
    (with-open-stream
        (stream (monitors::pipe-to-gnuplot))
      (set-gnuplot-parameters stream
       :output plot-path :terminal graphic-type :title title 
       :draw-y1-grid draw-y-grid :grid-color grid-color :grid-line-width grid-line-width
       :key-location key-location :x-label x-label :y1-label y-label :line-width line-width
       :fsize fsize :typeface typeface)
      (format stream "~cset style fill transparent solid 0.50 border" #\linefeed)
      (when logscale 
        (format stream "~cset logscale ~a" #\linefeed logscale))
      (set-histogram stream :error-bars error-bars :gap 1 :line-width line-width)
      ;(set-fill stream "solid")
      (set-boxwidth stream box-width)
      (set-x-labels stream data-labels rotate-xlabels-by)
      (set-range stream "x" -0.5 (- (length data) 0.5))
      (format stream "~cset ytics nomirror" #\linefeed)
      ;; autorange does not take into account errorbars
      ;; so we compute ourselves
      (set-range stream "y" y-min
                 (or y-max
                     (loop for a in data
                        maximize (max (max-val a) 
                                      (+ (avg-val a) (stdev-val a))))))
      (format stream "~cplot '-' notitle with histograms lt -1 lc rgb ~s lw ~a"
              #\linefeed color line-width)
      (loop 
         for a in data
         for i from 1
         for error-string  = (calculate-error-string a error-bars)
         do (format stream "~c ~,3f ~a" #\linefeed (avg-val a) error-string))
      (format stream "~ce" #\linefeed))
  (format t "~%.. wrote ~a" plot-path)
  (format t "~%.. done.")
  (when open (sleep 0.5) (open-file-in-os plot-path))))

;; (defun create-whisker-plot
;;        (&key
;;         (data (error "Please supply :data")) ; a list of gnuplot-batch-points
;;         (plot-name "whisker-plot") (plot-directory '(".tmp")) (graphic-type "pdf")
;;         error-bars ;; t, nil, :stdev, :min-max
;;         (title nil) y-label x-label (y-min 0.0) (y-max 1.0)
;;         (key-location "above")
;;         (box-width 1.5) (draw-y-grid t) (grid-color "#aaaaaa")
;;         (grid-line-width 0.5) (color (first monitors::*great-gnuplot-colors*))
;;         (data-labels  '("a1" "a2"))
;;         (open t))
;;   (format t "~%.. creating whisker-plot graph.")
;;   (let ((plot-path (babel-pathname :directory plot-directory
;;                                    :name plot-name
;;                                    :type graphic-type))
;;         (data-length (length data)))
;;     (with-open-stream
;;         (stream (monitors::pipe-to-gnuplot))
;;       (set-gnuplot-parameters stream
;;        :output plot-path :terminal graphic-type :title title 
;;        :draw-y1-grid draw-y-grid :grid-color grid-color :grid-line-width grid-line-width
;;        :key-location key-location :x-label x-label :y1-label y-label)
;;       (format stream "~cset style fill empty" #\linefeed)
;;       ;(set-histogram stream :error-bars error-bars :gap 1)
;;       (set-fill stream "solid")
;;       (set-boxwidth stream box-width)
;;       (set-x-labels stream data-labels)
;;       (set-range stream "x" -0.5 (- data-length 0.5))
;;       (format stream "~cset ytics nomirror" #\linefeed)
;;       ;; autorange does not take into account errorbars
;;       ;; so we compute ourselves
;;       (set-range stream "y" y-min
;;                  (or y-max
;;                      (loop for a in data
;;                         maximize (max (max-val a) 
;;                                       (+ (avg-val a) (stdev-val a))))))
;;       (format stream "~cplot '-' notitle with candlesticks lt 3 lw 2 lc rgb ~s"
;;               #\linefeed color)
;;       (loop for a in data
;;          for i from 0
;;          for error-string  = (calculate-error-string a error-bars)
;;          do (format stream "~c~a ~,3f ~a" #\linefeed i (avg-val a) error-string)
;;            (format t "~c~a ~,3f ~a" #\linefeed i (avg-val a) error-string))
;;       (format stream "~ce" #\linefeed))
;;   (format t "~%.. wrote ~a" plot-path)
;;   (format t "~%.. done.")
;;   (when open (sleep 0.5) (open-file-in-os plot-path))))



(defun create-bar-plot-a-vs-b
    (&key
     (data (error "Please supply :data"))
     (plot-name "bar-plot-a-vs-b") (plot-directory '(".tmp")) 
     (graphic-type "pdf")
     error-bars ;; t, nil, :stdev, :min-max
     (title "")
     x-label y-label y2-label     
     (labels-a '("a1" "a2")) (labels-b '("b1" "b2"))
     (key-location "above")
     y-min y-max y2-max y-axes 
     (draw-y-grid t) (grid-color "#aaaaaa") (grid-line-width 0.5) 
     (colors monitors::*great-gnuplot-colors*)
     (fsize 10)
     (typeface "Helvetica")
     (line-width 1.5) 
     (open t))
  ;; we require as many y-axes as data entries (a) or none
  (assert (or (not y-axes)
              (= (length y-axes) (length data))))
  (assert (and labels-a labels-b))
  (format t "~%.. creating bar-plot graph.")
  (let ((plot-path (babel-pathname :directory plot-directory
                                   :name plot-name
                                   :type graphic-type))
        (cluster-length (length (first data)))
        (data-length (length data)))
    (with-open-stream
        (stream (monitors::pipe-to-gnuplot))
      (set-gnuplot-parameters stream
       :output plot-path :terminal graphic-type :title title 
       :draw-y1-grid draw-y-grid :grid-color grid-color :grid-line-width grid-line-width
       :key-location key-location :x-label x-label :y1-label y-label :y2-label y2-label :line-width line-width
       :fsize fsize :typeface typeface)
      (format stream "~cset style fill transparent solid 0.50 border" #\linefeed)
      (set-x-labels stream labels-b)
      (set-histogram stream :error-bars error-bars :gap 1)
      (set-fill stream "solid")
      (set-range stream "x" -0.5 (- data-length 0.5))
      (format stream "~cset ytics nomirror" #\linefeed)   
      (labels ((max-y-value (data y-axes axis-to-check)
                 (loop for a in data
                    for i_a from 0
                    for y-axis = (nth i_a y-axes)
                    when (or (null y-axis) (= y-axis axis-to-check))
                    maximize (loop for b in a
                                maximize (max (max-val b) 
                                              (+ (avg-val b) (stdev-val b)))))))
        (set-range stream "y" y-min (or y-max (max-y-value data y-axes 1)))
        (when y-axes
          (format stream "~cset y2tics nomirror" #\linefeed)
          (set-range stream "y2" y-min (or y2-max (max-y-value data y-axes 2)))))
      (format stream "~cplot " #\linefeed)
      (loop for i from 0 to (- cluster-length 1)
         for color = (nth (mod i (length colors)) colors)
         for y-axis = (nth i y-axes)
         do (format stream "'-' title ~s with histograms ~@[axes x1y~a~] lt -1 lc rgb ~s~:[~;, ~]"
             (nth i labels-a) y-axis color (< i (- cluster-length 1))))
      ;; and now we finally write the actual data
      (loop for i from 0 to (1- cluster-length)
         do (loop for cluster in data
               for bar = (nth i cluster)
               for error-string  = (calculate-error-string bar error-bars)
               do (format stream "~c~,3f ~a";~@[~,3f~]"
                          #\linefeed (avg-val bar) error-string
                          ))
           (format stream "~ce"  #\linefeed)))
  (format t "~%.. wrote ~a" plot-path)
  (format t "~%.. done.")
  (when open (sleep 0.5) (open-file-in-os plot-path))))

(defun create-stacked-bar-plot-a-b
  (&key
   (data (error "Please supply :data")) ;; list of list of gnuplot-batch-points
   (plot-name "stacked-bar-plot-a-b") (plot-directory '(".tmp")) (graphic-type "pdf")
   (title nil)
   y-label x-label (key-location "below")
   (y-min 0.0) y-max (box-width 0.5)
   (draw-y-grid t) (grid-color "#aaaaaa")
   (grid-line-width 0.5) (line-width 1.5) (colors monitors::*great-gnuplot-colors*)
   (rotate-xlabels-by 0) 
   (fsize 10)
   (typeface "Helvetica")
   labels-a labels-b (open t))
  "stacks a, bundled with b"
  (assert (and labels-a labels-b))
  ;; some sanity checks
  ;(assert (= (length labels-a) (length data)))
  ;(assert (> (length labels-b) 1)) ;; there is a bug in gnuplot
  ;; which renders labels-b == 1 useless
  ;(assert (loop for dat-a in data
  ;           always(= (length dat-a) (length labels-b))))
  
  (format t "~%.. creating bar-plot graph.")
  (let ((plot-path (babel-pathname :directory plot-directory
                                   :name plot-name
                                   :type graphic-type)))
  (with-open-stream
      (stream (monitors::pipe-to-gnuplot))
    (set-gnuplot-parameters stream
       :output plot-path :terminal graphic-type :title title 
       :draw-y1-grid draw-y-grid :grid-color grid-color :grid-line-width grid-line-width
       :key-location key-location :x-label x-label :y1-label y-label :line-width line-width
       :fsize fsize :typeface typeface)
    (format stream "~cset style fill transparent solid 0.50 border" #\linefeed)
   ; (format stream "~cset terminal pdf linewidth ~a rounded solid color fsize ~a " #\linefeed line-width fsize)
    (format stream "~cset style data histograms" #\linefeed)
    (format stream "~cset style histogram rowstacked" #\linefeed)
    (set-fill stream "solid")
    (format stream "~cset boxwidth ~a" #\linefeed box-width)
    (set-x-labels stream labels-b rotate-xlabels-by)
    (format stream "~cset xrange [*:*]"  #\linefeed )
    (format stream "~cset ytics nomirror" #\linefeed )
    (set-range stream "y" y-min y-max)
    (format stream "~cplot " #\linefeed)
    (loop
       for i from 0
       for a in labels-a
       for color in colors
       when (> i 0)
       do (format stream ", ")
       do (format stream "'-' title ~s lt -1 lc rgb ~s" a color))
    (loop for i from 0 to (- (length (first data)) 1)
         do (loop for a in data
               for b = (nth i a)
               do (format stream "~c~,3f" #\linefeed (avg-val b)))
           (format stream "~ce"  #\linefeed)))
  (format t "~%.. wrote ~a" plot-path)
  (format t "~%.. done.")
  (when open (sleep 0.5) (open-file-in-os plot-path))))