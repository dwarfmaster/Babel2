(in-package :plot-raw-data)

(export '(set-output set-terminal set-title set-histogram set-fill
set-border set-grid set-key-location set-label set-boxwidth
set-x-tic-labels set-x-labels set-range set-gnuplot-parameters))

;; primitive functions for guiding gnuplot

(defun set-output (stream plot-path)
  (format stream "~cset output \"~a\"" #\linefeed plot-path))

(defun set-output-piped (stream plot-path)
  (format stream "~cset output '| ps2pdf - \"~a\"'" #\linefeed plot-path))

(defun set-terminal (stream &key (graphic-type "pdf") (line-width 2.5) (dashed t) (font "Helvetica, 10"))
  (format stream "~cset terminal ~a linewidth ~a color ~a rounded font '~a'" #\linefeed graphic-type line-width 
	  (if dashed "dashed" "solid") font))

(defun set-title (stream title)
  (format stream "~cset title ~s" #\linefeed title))

(defun set-histogram (stream &key (error-bars nil) (gap 1) (line-width 2))
  (format stream "~cset style histogram ~a gap ~a lw ~a" 
          #\linefeed (if error-bars "errorbars" "clustered") gap line-width))

(defun set-fill (stream &optional (type "empty"))
  (format stream "~cset style fill ~a" #\linefeed type))

(defun set-border (stream &optional (type "border"))
  (format stream "~cset style fill ~a" #\linefeed type))

(defun set-grid (stream &key (where "back") (axis "y") (color "#aaaaaa") (line-width 0.5))
  (format stream "~cset grid ~a ~atics lt 4 lc rgb \"~a\" lw ~a" 
          #\linefeed where axis color line-width))

(defun set-key-location (stream &optional (location "below"))
  (format stream "~cset key ~a" #\linefeed location))

(defun set-label (stream axis label)
  (format stream "~cset ~alabel ~:[~;~:*~s~]" #\linefeed axis label))

(defun set-boxwidth (stream width)
  (format stream "~cset boxwidth ~,2f" #\linefeed width))

(defun set-x-tic-labels (stream labels &optional (rotate-by 0))
  (format stream "~cset xtics nomirror rotate by ~a (~{~{~s ~a~}~^, ~})" #\linefeed
          rotate-by
          (loop for title in labels
             for n from 0 collect (list title n))))

(defun set-x-labels (stream labels &optional (rotate-by 0))
  (set-x-tic-labels stream labels rotate-by))

(defun set-range (stream axis min max)
  (format stream "~cset ~arange [~:[*~;~:*~d~]:~:[*~;~:*~d~]]" 
              #\linefeed axis min max))

(defun set-gnuplot-parameters
    (stream 
     &key 
     (output nil output-supplied-p)
     (terminal nil terminal-supplied-p)
     (title nil title-supplied-p)
     (key-location nil key-location-supplied-p) 
     (y1-min nil y1-min-supplied-p)
     (y1-max nil y1-max-supplied-p)
     (y2-min nil y2-min-supplied-p)
     (y2-max nil y2-max-supplied-p)
     (x-label nil x-label-supplied-p)
     (y1-label nil y1-label-supplied-p)
     (y2-label nil y2-label-supplied-p)
     (draw-y1-grid nil draw-y1-grid-supplied-p)
     (draw-y2-grid nil draw-y2-grid-supplied-p)
     (grid-color "#aaaaaa") (grid-line-width 0.5) 
     (dashed t) (fsize 10)
     (line-width 1.5) (typeface "Helvetica"))
  "A convenience function to write a set of gnuplot commands to the
gnuplot stream. Only when a key :value is supplied will anything be
written. As such it will never overwrite already written values."
  (declare (ignorable draw-y1-grid draw-y2-grid))
  (let ((font (format nil "~a, ~a" typeface fsize)))
    (if (equal terminal "piped.pdf")
      (progn
        (when output-supplied-p
          (set-output-piped stream output))
        (when terminal-supplied-p ;; isn't this always true if you are here?
          (set-terminal stream
                        :graphic-type "postscript enhanced" :dashed dashed
                        :line-width line-width :font font)))
      (progn
        (when output-supplied-p
          (set-output stream output))
        (when terminal-supplied-p
          (set-terminal stream :graphic-type terminal :dashed dashed
                         :line-width line-width :font font))))
    (when title-supplied-p
      (set-title stream title))
    (when key-location-supplied-p
      (set-key-location stream key-location))
    (when (or y1-min-supplied-p y1-max-supplied-p)
      ;; might potentially overwrite if already set
      (set-range stream "y" y1-min y1-max))
    (when (or y2-min-supplied-p y2-max-supplied-p)
      ;; might potentially overwrite if already set
      (set-range stream "y2" y2-min y2-max))
    (when x-label-supplied-p
      (set-label stream "x" x-label))
    (when y1-label-supplied-p
      (set-label stream "y" y1-label))
    (when y2-label-supplied-p
      (set-label stream "y2" y2-label))
    (when draw-y1-grid-supplied-p
      (set-grid stream :axis "y" :color grid-color :line-width grid-line-width))
    (when draw-y2-grid-supplied-p
      (set-grid stream :axis "y2" :color grid-color :line-width grid-line-width))))

  
  
