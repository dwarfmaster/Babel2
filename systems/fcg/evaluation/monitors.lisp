;; Katrien Beuls, October 2017
(in-package :fcg)


(define-monitor record-nr-of-meaning-chunks
                :class 'data-recorder
                :average-window 100
                :documentation "Records the number of meaning chunks per sentence.")

(define-event-handler (record-nr-of-meaning-chunks nr-of-meaning-chunks-recorded)
  (record-value monitor average-nr-of-chunks))

(define-monitor export-nr-of-meaning-chunks
                :class 'lisp-data-file-writer
                :documentation "Exports average number of meaning chunks after comprehension"
                :data-sources '(record-nr-of-meaning-chunks)
                :file-name (babel-pathname :directory '(".tmp")
                                           :name "meaning-chunks" :type "lisp")
                :add-time-and-experiment-to-file-name t
                :column-separator " "
                :comment-string "#")

(define-monitor plot-nr-of-meaning-chunks
    :class 'gnuplot-graphic-generator
    :documentation "Plots the average number of meaning chunks after comprehension."
    :data-sources '(record-nr-of-meaning-chunks)
    :update-interval 100
    :y1-label "# meaning chunks"
    :draw-y1-grid t
    :use-y-axis '(1)
    :caption '("nr-of-meaning-chunks")
    :file-name (babel-pathname :directory '(".tmp")
                               :name "meaning-chunks"
			       :type "pdf")
    :graphic-type "pdf")


(define-monitor record-nr-of-words
                :class 'data-recorder
                :average-window 10
                :minimum-number-of-data-points 10
                :documentation "Records the number of words per sentence.")

(define-event-handler (record-nr-of-words nr-of-words-recorded)
  (record-value monitor nr-of-words))

(define-monitor export-nr-of-words
                :class 'lisp-data-file-writer
                :documentation "Exports average number of words in the sentence during evalution"
                :data-sources '((average record-nr-of-words))
                :file-name (babel-pathname :directory '(".tmp")
                                           :name "sentence-lengths" :type "lisp")
                :add-time-and-experiment-to-file-name nil
                :column-separator " "
                :comment-string "#")

(define-monitor display-nr-of-words
                :class 'gnuplot-display-and-graphic-generator
                ;:minimum-number-of-data-points 10
                :documentation "Plots the average sentence length."
                :data-sources '((average record-nr-of-words))
                :caption '("nr-of-words")
                :use-y-axis '(1)
                :file-name (babel-pathname :directory '(".tmp")
			       :name "sentence-lengths"
			       :type "pdf")
                :graphic-type "pdf")

(define-monitor plot-nr-of-words
    :class 'gnuplot-graphic-generator
    :documentation "Plots the average sentence length."
    :data-sources '(record-nr-of-words)
    :y1-label "sentence length"
    :use-y-axis '(1)
    :draw-y1-grid t
    :caption '("nr-of-words")
    :file-name (babel-pathname :directory '(".tmp")
			       :name "sentence-lengths"
			       :type "pdf")
    :graphic-type "pdf")

(define-monitor record-branching-factor-comprehension
                :class 'data-recorder
                :average-window 100
                :documentation "Records the number of words per sentence.")

(define-event-handler (record-branching-factor-comprehension branching-factor-comprehension-recorded)
  (record-value monitor average-branching-factor))

(define-monitor export-branching-factor-comprehension
                :class 'lisp-data-file-writer
                :documentation "Exports average branching factors for parsing corpus sentences"
                :data-sources '(record-branching-factor-comprehension)
                :file-name (babel-pathname :directory '(".tmp")
                                           :name "branching-factors-comprehension" :type "lisp")
                :add-time-and-experiment-to-file-name t
                :comment-string "#")


(define-monitor plot-branching-factor-comprehension
    :class 'gnuplot-graphic-generator
    :documentation "Plots the average branching factor in comprehension."
    :data-sources '(record-branching-factor-comprehension)
    :y1-label "branching factor"
    :use-y-axis '(1)
    :caption '("comprehension-bf")
    :draw-y1-grid t
    :file-name (babel-pathname :directory '(".tmp")
			       :name "branching-factors"
			       :type "pdf")
    :graphic-type "pdf")

