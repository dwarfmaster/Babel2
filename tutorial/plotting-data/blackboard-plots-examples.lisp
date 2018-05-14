(ql:quickload :plot-raw-data)

(in-package :plot-raw-data)

;; note: Under OSX :graphic-type "pdf" often doesn't work. Try "piped.pdf" instead.

;; #####################################
;; Example 1, serialize plot data

;; Example 1a simple bar-plots
(let ((serialized-data
       (serialize-plot-data ;;first the data represented as list of batches (a batch is a list of lists of values)
                            '(((0 1 1) (0 0 1)) ((0 nil 1) (1 0 0)))
                            ;; and the parameters to be parsed to create-bar-plot
                            :data-labels '("a" "b")
                            :y-max 1
                            :graphic-type "piped.pdf")))
  ;; create bar plot from data
  (create-bar-plot-from-serialized-data serialized-data))

;; Example 1b grouped bar-plots
(let ((serialized-data
       (serialize-plot-data ;;first the data represented as list of list of batches
                            '((((1 1 1) (1 1 1)) ((0 0 1) (1 1 0)))
                              (((1 1 1) (1 0 1)) ((0 0 0) (1 0 0))))
                            ;; and the parameters to be parsed to create-bar-plot
                            :labels-a '("a" "b") :labels-b '("1" "2")
                            :graphic-type "piped.pdf")))
  ;; create grouped bar plot from data
  (create-bar-plot-a-vs-b-from-serialized-data serialized-data))

;; Example 1c stacked bar-plots
(let ((serialized-data
       (serialize-plot-data ;;first the data represented as list of list of batches
                            '((((1 1 1) (1 1 1)) ((0 0 1) (1 1 0)))
                              (((1 1 1) (1 0 1)) ((0 0 0) (1 0 0))))
                            ;; and the parameters to be parsed to create-bar-plot
                            :labels-a '("a" "b") :labels-b '("1" "2")
                            :graphic-type "piped.pdf")))
  ;; create grouped bar plot from data
  (create-stacked-bar-plot-from-serialized-data serialized-data))

;; Example 1d evo-plots

(let ((serialized-data
       (serialize-plot-data '(((1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0)
                               (1.0 0.8 0.7 0.5 0.2 0.1 0.0 0.0))
                              ((0.1 nil 0.1 0.1 0.1 0.1 0.1 0.1)
                               (0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.4)))
                            :file-name "Test"
                            :directory '()
                            :title "Test"
                            :key-location "bottom"
                            :caption '("A" "B")
                            :graphic-type "piped.pdf"
                            :use-y-axis '(1 1)
                            :average-windows '(0 1))))
  (create-evo-plot-from-serialized-data serialized-data))

;; #####################################
;; Example 2, save/load serialized data

;; temp path
(defparameter *example-output-path*  (make-pathname :directory (append (pathname-directory cl-user::*babel-path*)
                                                                       (list ".tmp"))
                                                    :name "plot-data" 
                                                    :type "lisp"))

(let ((serialized-data
       (serialize-plot-data ;;first the data represented as list of batches (a batch is a list of lists of values)
                            '(((0 1 1) (0 0 1)) ((0 0 1) (1 0 0)))
                            ;; and the parameters to be parsed to create-bar-plot
                            :data-labels '("a" "b"))))
  ;; write data to disk
  (write-serialized-plot-data serialized-data *example-output-path*)

  ;; read data back
  (create-bar-plot-from-serialized-data (read-serialized-plot-data *example-output-path*)))


;; #####################################
;; Example 3, use blackboard as data source

(let* ((blackboard1 (make-blackboard :data-fields '((success . ((0 1 1) (0 0 1))))))
       (blackboard2 (make-blackboard :data-fields '((success . ((0 0 1) (1 0 0))))))
       ;; retrieve the data
       (plot-data (get-plot-data-from-blackboards (list blackboard1 blackboard2) 'success))
       (serialized-data (serialize-plot-data plot-data :data-labels '("a" "b"))))
  (create-bar-plot-from-serialized-data serialized-data))


;; #####################################
;; Example 3, all together

(let* ((blackboard1a (make-blackboard :data-fields '((success . ((0 1 1) (1 1 1))))))
       (blackboard1b (make-blackboard :data-fields '((success . ((0 1 1) (1 0 1))))))
       (blackboard2a (make-blackboard :data-fields '((success . ((1 0 1) (1 0 0))))))
       (blackboard2b (make-blackboard :data-fields '((success . ((0 0 1) (1 0 0))))))
       ;; retrieve the data
       (plot-data (get-plot-data-from-blackboards (list
                                                   (list blackboard1a blackboard2a)
                                                   (list blackboard1b blackboard2b))
                                                  'success))
       
       (serialized-data (serialize-plot-data plot-data :labels-a '("1" "2") :labels-b '("a" "b"))))
  (write-serialized-plot-data serialized-data *example-output-path*)
  (create-bar-plot-a-vs-b-from-serialized-data (read-serialized-plot-data *example-output-path*)))
