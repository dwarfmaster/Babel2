(in-package :plot-raw-data)

;; ########## Read-data #########

(defun read-raw (&key (name '(error "Please supply :name"))
                 (directory '(error "Please supply :directory"))
                 (file-type "dat"))
  "If possible use lisp format instead of dat (txt) as it is much
faster for reading and writing."
  (if (string-equal file-type "dat")
      (read-dat :name name :directory directory :file-type file-type)
      (read-lisp :name name :directory directory :file-type file-type)))

(defun read-lisp (&key (name '(error "Please supply :name"))
                 (directory '(error "Please supply :directory"))
                 (file-type "lisp"))
  "Returns a list of lists where the first list contains the
interaction numbers. The remaining lists are the values for every
series."
  (let ((file (open (babel-pathname 
                     :name name
                     :type file-type
                     :directory directory))))
    (let ((result (car (read file nil nil nil)))
          series-length)
      (setf result (loop for series in result
                      collect (list->array series)))
      (setf series-length (length (first result)))
      (push (loop 
               with game-series = (make-array series-length :fill-pointer 0)
               for counter from 0 to (1- series-length)
               do (vector-push counter game-series)
               finally (return game-series))
            result))))

(defun read-dat (&key (name '(error "Please supply :name"))
                 (directory '(error "Please supply :directory"))
                 (file-type "dat"))
  "Returns a list of lists where the first list contains the
interaction numbers. The remaining lists are the values for every
series."
  (let ((file (open (babel-pathname 
                     :name name
                     :type file-type
                     :directory directory))))
    (transpose
     (loop
        with data-reached = nil
        for line = (read file nil nil)
        while line
        for split = (split line #\Space)
        for ints = (when (or data-reached 
                             (not (equal (first split) "#")))
                     (setf data-reached t)
                     (loop for x in split
                        collect (read-from-string x nil nil)))
        when (and ints (numberp (car ints)))
        collect (remove nil ints)))))
  
(defun transpose (table)
  (loop for c-i from 0 below (reduce #'max (mapcar #'length table))
        collect
        (loop for row in table
              collect (nth c-i row))))

;; ############## Abstraction for batches #####################

(defclass gnuplot-batch-point  ()
  ((name :accessor name :initarg :name :initform "" :type string
         :documentation "Name of what was measured, e.g. communicative success")
   (batch-data :accessor batch-data :initarg :batch-data :initform nil :type list
                :documentation "Unprocessed data with an element for each serie.")
   (avg-val :accessor avg-val :initarg :avg-val :initform nil :type (or null number)
            :documentation "The average of the batch-data")
   (median-val :accessor median-val :initarg :median-val :initform nil :type (or null number)
            :documentation "The median of the batch-data")
   (stdev-val :accessor stdev-val :initarg :stdev-val :initform nil :type (or null number)
            :documentation "The standard deviation of the batch-data")
   (max-val :accessor max-val :initarg :max-val :initform nil :type (or null number)
            :documentation "The max value in the batch-data")
   (min-val :accessor min-val :initarg :min-val :initform nil :type (or null number)
            :documentation "The min value in the batch-data"))
  (:documentation "Keeps the data for one one entry of a raw batch
  file. For example if there were 10 series of 1000 games in the batch
  there would be 1000 gnuplot-batch-points.  It also then keeps all
  sorts of interesting data, like average, stdev, max, min, ..."))

(defmethod print-object ((point gnuplot-batch-point) stream)
  (pprint-logical-block (stream nil)
    (format stream "<point (~a):~:_ avg-val: ~a,~:_ median-val: ~a,~:_ stdev-val: ~a,~:_ min-val: ~a,~:_ ~
max-val: ~a,~:_" 
            (name point) (avg-val point) (median-val point) (stdev-val point) 
            (min-val point) (max-val point))))

(defun calculate-missing-slots (batch-point)
  "Given a batch-point that has at least its batch-data slot filled
this function will fill in all other information like stdev, median,
average, max and min. If some information is already provided it is
not recalculated, which means the function cannot used as an update
function."
  (with-slots (batch-data) batch-point
    (unless batch-data 
      (warn "Cannot calculate missing slots when :batch-data is empty"))
    (loop 
       with sorted-data
       for point in batch-data
       for length from 1
       unless (median-val batch-point)
       do (setf sorted-data (sorted-insert sorted-data point))
       maximize point into max
       minimize point into min
       sum point into sum
       finally
         (unless (avg-val batch-point)
           (setf (avg-val batch-point) (/ sum length)))
         (unless (median-val batch-point)
           (setf (median-val batch-point) (median sorted-data :already-sorted t)))
         (unless (max-val batch-point)
           (setf (max-val batch-point) max))
         (unless (min-val batch-point)
           (setf (min-val batch-point) min))
         (unless (stdev-val batch-point)
           (setf (stdev-val batch-point) (stdev batch-data :average (avg-val batch-point))))
         (return batch-point))))

(defun limit (nr low up)
  "Helper function which returns nr if it is between low and
up. Otherwise it returns either low or up (depending on whether it was
lower or higher)"
  (cond ((< nr low) low)
        ((> nr up) up)
        (t nr)))

(defun percentile (gnuplot-batch-point p)
  ;; formula taken from definition 3 on http://cnx.org/content/m10805/latest/
  ;;; It uses interpolation to estimate the percentile
  (let* ((batch-data (sort (copy-list (batch-data gnuplot-batch-point)) #'<))
         (n (length batch-data))
         (rank (limit (* (/ p 100) (+ 1 n)) 1 n)))
    (multiple-value-bind (ir fr) (floor rank)
      (cond ((= ir rank)
             (nth (- rank 1) batch-data))
            (t
             + (* fr (- (nth ir batch-data) (nth (- ir 1) batch-data)))
             (nth (- ir 1) batch-data))))))


;; ############## Handle data for plot #####################

;; (defun create-baseline-data (baseline-value number-of-interactions number-of-series)
;;   (loop for j from 1 to number-of-interactions
;;      collect (make-instance 'gnuplot-batch-point
;;                             :name 'baseline
;;                             :batch-data (loop for i from 1 to number-of-series 
;;                                            collect baseline-value)
;;                             :avg-val baseline-value
;;                             :stdev-val 0
;;                             :min-val baseline-value
;;                             :max-val baseline-value)))

;; (defun data-set-add-baseline (data-set baseline-value)
;;   (append data-set
;;             (list (create-baseline-data
;;                    baseline-value
;;                    (data-set-shortest-series-length data-set)
;;                    (data-set-minimal-number-of-series data-set))))))


(defun collect-data-from-files (files directory)
  (loop for file in files
        collect (read-raw :name file :directory directory)))

(defun collect-data-for-evo-plots (file-paths &key 
                                   (file-type "dat")
                                   (only-x-last-interactions nil)
                                   (start nil)
                                   (end nil)
                                   (series-numbers nil)
                                   (windows nil))
  "Assumes a list of file-paths."
  (loop 
     with windows = (if (listp windows)
                        windows
                        (loop for i from 1 to (length file-paths)
                           collect windows))
     for path in file-paths
     for counter from 0
     collect
       (let* ((raw-data (read-raw :name (first (last path)) :directory (butlast path)
                                  :file-type file-type))
              (series-last-index (length (first raw-data)))
              (data (data-remove-index-row raw-data)))
         ;; if only-x-last-interactions we reduce data to only those
         (when only-x-last-interactions
           (setf data (data-subseries data (- series-last-index only-x-last-interactions) 
                                      series-last-index)))
         (when (or start end)
           (setf data (data-subseries data (or start 0) end)))
         (when series-numbers
          (setf data (data-pick-series data series-numbers)))
         ;; calculating the average values (first per serie and then
         ;; over the series) and the requested errors
         (when windows 
           (setf data (data-apply-average-window data (nth counter windows))))
         (compute-batch-points-for-evo-plot data (first (last path))))))

(defun create-batch-points (name data)
  (loop for point in (first data)
        if point
        collect (make-instance 
                 'gnuplot-batch-point
                 :name name
                 :batch-data (list point))
        else
        collect nil))

(defun compute-batch-points-for-evo-plot (data name)
  (loop with result = (create-batch-points name data)            
     for series in (rest data)
     do (loop for point in series
           for batch-point in result
           do (push point (batch-data batch-point)))
     finally 
       (return (loop for batch-point in result
                  collect (calculate-missing-slots batch-point)))))

(defun collect-data-for-bar-plots (file-paths &key (file-type "lisp") (only-x-last-interactions nil))
  "Assumes a list of lists of file-paths. Each sublist represents a
seperate group of bars."
  (loop for path in file-paths
     collect
     (let* ((raw-data (read-raw :name (first (last path)) :file-type file-type :directory (butlast path)))
            (series-last-index (1- (length (first raw-data))))
            (data (data-remove-index-row raw-data)))
       (when only-x-last-interactions
         (setf data (data-subseries data (1+ (- series-last-index only-x-last-interactions))
                                    series-last-index)))
       (setf data (loop for series in data
                        collect (array->list series)))
       ;; if average-only-last we reduce data to only those
       (compute-batch-point data (first (last path))))))

(defun compute-batch-point (data name)
  ;; calculating the average values (first per serie and then
  ;; over the series) and the requested errors
  (loop for batch-data in data
        for average = (average batch-data)
        collect average into averaged-data
        maximize average into max
        minimize average into min
        finally (let ((avg-val (average averaged-data)))
                  (return (make-instance 'gnuplot-batch-point
                                         :name name
                                         :batch-data averaged-data
                                         :avg-val avg-val
                                         :stdev-val (stdev averaged-data :average avg-val)
                                         :min-val min
                                         :max-val max)))))

(defun collect-data-for-grouped-bar-plots (file-path-groups &key 
                                           (file-type "lisp") (only-x-last-interactions nil))
  (loop for path-group in file-path-groups
     collect (collect-data-for-bar-plots path-group 
                                         :file-type file-type
                                         :only-x-last-interactions only-x-last-interactions)))

(defun data-set-subseries (data-set start end)
  (loop for data in data-set
        collect (data-subseries data start end)))

(defun data-set-reduce-data-point-density (data &optional (percentage 10))
  "For speed optimization (and drawing/printing the pdf) you might
want to reduce the number of points. You can give a percentage of
point to keep. For example, instead of drawing a point for every game
you can choose to only draw every ten games, which means only 10%,
which you can get by passing 10 as percentage."
  (loop for series in data
     collect (loop for nr in series
                for counter from 0
                when (= (mod counter (/ 100 percentage)) 0)
                collect nr)))
       
(defun data-subseries (data start end)
  (loop for series in data
     collect (subseq series start end)))

(defun data-number-of-series (data)
  (length data))

(defun data-set-minimal-number-of-series (data-set)
  (reduce #'min data-set :key #'data-number-of-series))

(defun data-set-reduce-number-of-series (data-set &optional (series-numbers nil))
  (let ((series-numbers (or series-numbers
                            (loop for i from 0 below (data-set-minimal-number-of-series data-set)
                                  collect i))))
    (loop for data in data-set
          collect (data-pick-series data series-numbers))))

(defun data-pick-series (data series-numbers)
  (loop for series-number in series-numbers
        for series = (nth series-number data)
        if series
        collect series
        else
        do (error "~a out of bounds" series-number)))

(defun data-shortest-series-length (data)
  (reduce #'min data :key #'length))

(defun data-set-shortest-series-length (data-set)
  (reduce #'min data-set :key #'data-shortest-series-length))

(defun data-remove-index-row (data)
  (cdr data))

(defun data-set-remove-index-rows (data-set)
  (loop for data in data-set
        collect (data-remove-index-row data)))

(defun data-reduce (data &optional (maximal-series-length nil))
  (let ((length (if maximal-series-length
                        (min maximal-series-length
                             (data-shortest-series-length data))
                        (data-shortest-series-length data))))
    (data-subseries data 0 length)))

;; ############## Smooth data #####################

(defun series-apply-average-window (series window)
  (series-apply-normal-window-aux series window))

(defun pre-compute-bell-curves (window var-square)
  (loop 
     with table = (make-hash-table :size window)
     for i from  (- window) to 0
     do (setf (gethash i table) (bell-curve i var-square))
     finally (return table)))

(defun get-bell-curve (window cache)
  (gethash window cache))

;;(defun series-apply-normal-window-aux (series left-window)
;;  (let* ((var-square (* (/ left-window 2) (/ left-window 2)))
;;         (bell-curve-cache (pre-compute-bell-curves left-window var-square)))
;;    (loop 
;;       for i from 0 below (length series)
;;       for start = (max (- i left-window) 0)
;;       for total-bell = (if (= start 0)
;;                            (loop for j from 0 below (1+ i)
;;                               sum (fast-bell-curve (- (* j j)) var-square))
;;                            total-bell)                            
;;       collect (apply-bell-curve series start (1+ i) total-bell bell-curve-cache))))

;;(defun apply-bell-curve (series min-inclusive max-exclusive total bell-curve-cache)
;;  (/ (loop for i from min-inclusive to (1- max-exclusive)
;;        for point = (aref series i)
;;        sum (* point (get-bell-curve (- i (1- max-exclusive)) bell-curve-cache)))
;;     total))

;;(defun fast-bell-curve (x-square-minus var-square)
;;  ;; for the special case where some vars are constants
;;  (exp (/ x-square-minus var-square)))


(defun series-apply-normal-window-aux (series left-window)
  (let* ((var-square (* (/ (1+ left-window) 2) (/ (1+ left-window) 2)))
         (bell-curve-cache (pre-compute-bell-curves left-window var-square)))
    (loop 
     for i from 0 below (length series)
     for start = (max (- i left-window) 0)
     collect (let ((total-bell-curve (total-bell-curve series start i bell-curve-cache))
                   (total-applied (total-applied series start i bell-curve-cache)))
               (if (and (> total-applied 0) (> total-bell-curve 0))
                 (/ total-applied total-bell-curve)
                 0)))))

(defun total-bell-curve (series min-inclusive index bell-curve-cache)
  (loop for i from min-inclusive to index
        for point = (aref series i)
        if point
        sum (get-bell-curve (- i index) bell-curve-cache)))

(defun total-applied (series min-inclusive index bell-curve-cache)
  (loop for i from min-inclusive to index
        for point = (aref series i)
        if point
        sum (* point (get-bell-curve (- i index) bell-curve-cache))))

(defun bell-curve (window var-square)
  (exp (/ (- (* window window)) var-square)))

(defun data-apply-average-window (data window)
  (loop for series in data
        collect (series-apply-average-window series window)))