
(in-package :guessing-game)

;; ----------------------------------------------------------------------
;; trace-tasks-and-processes

(define-monitor trace-tasks-and-processes)

(define-event-handler (trace-tasks-and-processes run-task-finished)
  (add-element (make-html task)))

;; ----------------------------------------------------------------------
;; trace-learning

(define-monitor trace-learning)

(define-event-handler (trace-learning notify-learning-finished)
  (loop for problem in problems
     do (add-element (make-html problem)))
  (loop for fix in fixes
     do 
       (add-element (make-html fix))))

;; ----------------------------------------------------------------------
;; record/export communicative-success

(define-monitor record-communicative-success
    :class 'data-recorder)

(define-event-handler (record-communicative-success interaction-finished)
  (assert (length= 2 (interacting-agents experiment)))
  (record-value monitor
                (if (and (communicated-successfully (speaker experiment))
                         (communicated-successfully (hearer experiment)))
                  1.0
                  0.0)))

(define-monitor export-communicative-success
    :class 'lisp-data-file-writer
    :documentation "Exports communicative success"
    :data-sources '(record-communicative-success)
    :file-name (babel-pathname :name "CS" :type "lisp" 
                               :directory '("tutorial" "guessing-game" "raw-data"))
    :add-time-and-experiment-to-file-name nil)

;; ----------------------------------------------------------------------
;; record/export communicative-success

(define-monitor record-mapping-alignment
    :class 'data-recorder)

(define-event-handler (record-mapping-alignment interaction-finished)
  (assert (length= 2 (interacting-agents experiment)))
  (let ((speaker (speaker experiment))
        (hearer (hearer experiment)))
  (if (and (utterance speaker)
           (eq (parse/produce (utterance speaker) speaker)
               (parse/produce (utterance speaker) hearer))
           (string-equal
            (parse/produce (parse/produce (utterance speaker) speaker) speaker)
            (parse/produce (parse/produce (utterance speaker) speaker) hearer)))
  (record-value monitor 1.0)
    (record-value monitor 0.0))))

(define-monitor export-record-mapping-alignment
    :class 'lisp-data-file-writer
    :documentation "Exports communicative success"
    :data-sources '(record-mapping-alignment)
    :file-name (babel-pathname :name "CS" :type "lisp" 
                               :directory '("tutorial" "guessing-game" "raw-data"))
    :add-time-and-experiment-to-file-name nil)

;; ----------------------------------------------------------------------
;; record/export nr-of-constructions

(define-monitor record-nr-of-constructions
    :class 'data-recorder)

(define-event-handler (record-nr-of-constructions interaction-finished)
  (record-value monitor 
                (/ (loop 
                    for agent in (population experiment)
                    sum (length (lexicon agent)))
                    (length (population experiment)))))


(define-monitor export-nr-of-constructions
    :class 'lisp-data-file-writer
    :documentation "Exports number of constructions"
    :data-sources '(record-nr-of-constructions)
    :file-name (babel-pathname :name "nr-of-constructions" :type "lisp" 
                               :directory '("tutorial" "guessing-game" "raw-data"))
    :add-time-and-experiment-to-file-name nil
    :column-separator " "
    :comment-string "#")

;; ----------------------------------------------------------------------
;; diplays

(define-monitor display-success
    :class 'gnuplot-display
    :documentation "Displays communicative success, the average number
                    of cxn and average number of chunks"
    :update-interval 250
    :data-sources '((average record-communicative-success)
                    (average record-nr-of-constructions)
                    (average record-mapping-alignment))
    :caption '("communicative success" "number of constructions"
                                       "mapping alignment")
    :key-location "bottom right" :x-label "number of interactions"
    :y1-label "comm, mapping" :y2-label "# of cxn"
    :error-bars t :use-y-axis '(1 2 1) :y1-min 0 :y1-max 1 :draw-y1-grid t)

