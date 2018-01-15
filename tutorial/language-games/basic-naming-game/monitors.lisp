;; ----------
;; Monitoring
;; ----------

(in-package :naming-game)


(define-monitor plot-communicative-success
    :class 'gnuplot-graphic-generator
    :documentation "Plots communicative success"
    :data-sources '((average default-record-communicative-success))
    :update-interval 100
    :caption '("communicative success" )
    :x-label "games" 
    :y1-label "communicative success"
    :use-y-axis '(1) 
    :y1-max 1.0 :y1-min 0 
    :draw-y1-grid t :error-bars :min-max
    :graphic-type "pdf"
    :file-name (babel-pathname :name "communicative-success" :type "pdf" 
                               :directory '("tutorial" "basic-naming-game" "graphs"))
    :add-time-and-experiment-to-file-name t)

(define-monitor export-communicative-success
    :class 'lisp-data-file-writer
    :documentation "Exports communicative success"
    :data-sources '(default-record-communicative-success)
    :file-name (babel-pathname :name "communicative-success" :type "lisp" 
                               :directory '("tutorial" "basic-naming-game" "raw-data"))
    :add-time-and-experiment-to-file-name nil)

(define-monitor record-nr-of-competitors
    :class 'data-recorder)

(define-event-handler (record-nr-of-competitors interaction-finished)
  (record-value monitor 
                (loop 
                   with expressable-meanings = nil
                   with expressable-forms = nil
                   for agent in (population experiment)
                   do (loop for lex in (lexicon agent)
                         do (pushnew (meaning lex) expressable-meanings)
                         do (pushnew (form lex) expressable-forms))
                   finally (return (/ (length expressable-forms)
                                      (length expressable-meanings))))))

(define-monitor export-nr-of-competitors
    :class 'lisp-data-file-writer
    :documentation "Exports number of competitors"
    :data-sources '(record-nr-of-competitors)
    :file-name (babel-pathname :name "nr-of-names-per-meaning" :type "lisp" 
                               :directory '("tutorial" "basic-naming-game" "raw-data"))
    :add-time-and-experiment-to-file-name nil
    :column-separator " "
    :comment-string "#")

(defun prefered-lexes (agent)
  (all-biggest #'score (find-all (topic agent) (lexicon agent) :key #'meaning)))

(defun prefered-lex (agent)
  (let ((prefered-names (prefered-lexes agent)))
    (when (= (length prefered-names) 1)
      (first prefered-names))))

(define-monitor record-alignment-success
    :class 'data-recorder)

(define-event-handler (record-alignment-success interaction-finished)
  (record-value monitor 
                (if (communicated-successfully (first (interacting-agents experiment)))
                    (let* ((speaker (speaker experiment))
                           (hearer (hearer experiment))
                           (hearer-prefered-lex (prefered-lex hearer))
                           (hearer-prefers-same-form? (and hearer-prefered-lex
                                                           (equal (form hearer-prefered-lex)
                                                                  (form (applied-lex speaker))))))
                      (if hearer-prefers-same-form? 1 0))
                    0)))

(define-monitor export-alignment-success
    :class 'lisp-data-file-writer
    :documentation "Exports alignment success"
    :data-sources '(record-alignment-success)
    :file-name (babel-pathname :name "alignment-success" :type "lisp" 
                               :directory '("tutorial" "basic-naming-game" "raw-data"))
    :add-time-and-experiment-to-file-name nil
    :column-separator " "
    :comment-string "#")

(define-monitor record-alist-name-competition
    :documentation "Records the competition for one name in the population"
    :class 'alist-recorder
    :average-windows 1)

(define-event-handler (record-alist-name-competition interaction-finished)
    (loop 
       with name-counter = nil
       for agent in (population experiment)
       do (loop for lex in (lexicon agent)
             for found = (find (form lex) name-counter :key #'first)
             if found do (incf (second found))
             else do (push (list (form lex) 1) name-counter))
       finally (loop for (name counter) in name-counter
                  do (set-value-for-symbol monitor (intern name) 
                                           (/ counter (length (population experiment)))))))

(define-monitor plot-name-competition 
    :class 'alist-gnuplot-graphic-generator
    :recorder 'record-alist-name-competition
    :average-windows 1
    :draw-y-1-grid t
    :y-label "Population adoption"
    :x-label "Total number of interactions"
    :file-name (babel-pathname :directory (list "tutorial" "basic-naming-game" "graphs")
			       :name "name-competition"
			       :type "pdf")
    :graphic-type "pdf")

(defun create-name-competition-graph (&key 
                                       (configurations nil)
                                       (nr-of-interactions 2000))
  (format t "~%Running experiments in order to create a name
  competition graph. Please be patient.")
 ; (push (cons :total-nr-of-objects 1) configurations)
  (activate-monitor plot-name-competition)
  (run-batch 'ng-experiment nr-of-interactions 1
             :configuration (make-configuration :entries configurations))
  (deactivate-monitor plot-name-competition)
  (format t "~%Graphs have been created"))

(defparameter *naming-game-monitors* 
  '("export-communicative-success" "export-nr-of-competitors" "export-alignment-success"))

(defun run-experiments (strategies 
                        &key
                          (nr-of-objects 5)
                          (population-size 10)
                          (who-aligns :both) ;; can also be :speaker or :hearer
                          (initial-score 0.5)
                          (number-of-interactions 2000) (number-of-series 4)
                          (monitors 
                           '("export-communicative-success" "export-alignment-success"
                             "export-nr-of-competitors")))
  (format t "~%Starting experimental runs")
  (run-batch-for-different-configurations
   :experiment-class 'ng-experiment
   :number-of-interactions number-of-interactions
   :number-of-series number-of-series
   :monitors monitors
   :shared-configuration `((:total-nr-of-objects . ,nr-of-objects)
                           (:population-size . ,population-size)
                           (:who-aligns? . ,who-aligns)
                           (:initial-score . ,initial-score))
   :configurations strategies
   :output-dir (babel-pathname :directory 
                               `("tutorial" "basic-naming-game" "raw-data")))
  (format t "~%Experimental runs finished and data has been generated. You can now plot graphs."))

(defun create-graph-for-single-strategy (&key experiment-name measure-names)
  (format t "~%Creating graph for experiment ~a with measures ~a" experiment-name measure-names)
  (raw-files->evo-plot
   :raw-file-paths 
   (loop for measure-name in measure-names
      collect `("tutorial" "basic-naming-game" "raw-data" ,experiment-name ,measure-name))
   :average-windows 100
   :plot-directory '("tutorial" "basic-naming-game" "graphs")
   :error-bars '(:percentile 10 90) :error-bar-modes '(:filled))
  (format t "~%Graphs have been created"))

(defun create-graph-comparing-strategies (&key experiment-names measure-name)
  (format t "~%Creating graph for experiments ~a with measure ~a" experiment-names measure-name)
  (raw-files->evo-plot 
   :raw-file-paths 
   (loop for experiment-name in experiment-names
      collect `("tutorial" "basic-naming-game" "raw-data" ,experiment-name ,measure-name))
   :average-windows 100 :y1-label measure-name
   :captions experiment-names                  
   :plot-directory '("tutorial" "basic-naming-game" "graphs")
   :error-bars :stdev :error-bar-modes '(:lines))
  (format t "~%Graphs have been created"))
