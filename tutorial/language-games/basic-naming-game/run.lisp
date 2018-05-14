;; Load all the code necessary to run basic Naming Games
(ql:quickload 'basic-naming-game)

(in-package :naming-game)

;----------------------------------------
;;               Examples
; ---------------------------------------

;; You can play around with different strategies by calling the
;; function run-experiments. In the call below we pass it only a
;; single experiment named no-alignment and we pass it only one
;; parameter (:key . :value) pair, namely the :alignment-strategy set
;; to :no-alignment.

;; All the possible values for :alignment-strategy are:
;;; :no-alignment, :frequency, :minimal-NG, :lateral-inhibition and
;;; :lateral-inhibition-2
(activate-monitor plot-communicative-success)
(activate-monitor trace-interaction-in-repl)
(run-batch 'ng-experiment 1000 1)
(run-experiments '(;; (name-of-experiment ((:key . :value)(:key . :value)...))
                   (no-alignment ((:alignment-strategy . :no-alignment)))
                   (talking-heads ((:alignment-strategy . :th-lateral-inhibition)))
                   ))

;; run-experiments will create directories for each experiment-name in
;; Babel2/examples/basic-naming-game/raw-data. In this case a
;; directory "no-alignment" should have been created. The three files
;; found in this directory are data gathered from the experimental run.

;;; communicative-success: A value of 1 when the hearer understood the
;;; spoken name. In other words when no invention or adoption took
;;; place. Otherwise 0.

;;; alignment-success: A value of 1 when communicative-success=1 AND
;;; when the hearer would have spoken the same name if he was speaker
;;; for the chosen topic.

;;; nr-of-names-per-meaning: The average number of forms per
;;; meaning. In case of full alignment this value should go to 1.

;; These data files can be plotted using two functions:
;; create-graph-for-single-strategy
;; create-graph-comparing-strategies.

(create-graph-for-single-strategy :experiment-name "no-alignment" 
                                  :measure-names '("alignment-success" 
                                                   "communicative-success"))

(create-graph-for-single-strategy :experiment-name "talking-heads" 
                                  :measure-names '("alignment-success" 
                                                   "communicative-success"))

(create-graph-for-single-strategy :experiment-name "no-alignment" 
                                  :measure-names '("nr-of-names-per-meaning"))

;; We can also plot name competitors and their acceptance in the
;; population as follows:
(create-name-competition-graph
 :configurations '((:total-nr-of-objects . 5)
                   (:population-size . 10)
                   (:who-aligns? . :both)
                   (:initial-score . 1.0)
                   (:alignment-strategy . :no-alignment))
 :nr-of-interactions 2000)


;; The next call runs two different experiments called imitation and
;; lateral-inhibition (with different alignment modes). We also pass
;; some other parameters such as the number of language games to play,
;; the number of series (for averaging), and the population-size. For
;; 30000 games and 4 series, this means that in total 120000 games
;; will be played per experiment. Given two experiments the following
;; call will run 240000 naming games.

(run-experiments '((frequency ((:alignment-strategy . :frequency)))
                   (imitation ((:alignment-strategy . :imitation)))
                   (minimal-ng ((:alignment-strategy . :minimal-NG)))
                   (talking-heads ((:alignment-strategy . :th-lateral-inhibition)
                                   (:initial-score . 0.5)))
                   (lateral-inhibition 
                    ((:alignment-strategy . :lateral-inhibition)
                     (:initial-score . 0.5))))
                :number-of-interactions 30000
                :population-size 50
                :number-of-series 4)

;; We can compare strategies with create-graph-comparing-strategies as
;; follows:
(create-graph-comparing-strategies :experiment-names '("frequency" "imitation" 
                                                       "minimal-ng" "talking-heads"
                                                       "lateral-inhibition")
                                   :measure-name "communicative-success")

(create-graph-comparing-strategies :experiment-names '("frequency" "imitation" 
                                                       "minimal-ng" "talking-heads"
                                                       "lateral-inhibition")
                                   :measure-name "alignment-success")


;----------------------------------------
;;               Questions
; ---------------------------------------

;; To answer the following questions you will have to run experiments
;; (by calling run-strategies) and plot graphs (by calling
;; create-graph-...). 


;;; Question 1a:
;; Can a population of agents reach communicative success with the
;; :no-alignment strategy? Can they reach alignment success? Show this
;; using graphs. Try to explain why.

;;; Question 1b:
;; If you run the experiment again with a larger :population-size,
;; what is the impact on the number of competing forms? Create graphs
;; that support your findings.


;;; Question 2:
;; Explain in your own words how the :imitation strategy works (look
;; at the source code for this). Does it reach communicative and/or
;; alignment success?

;;; Question 3:
;; Explain in your own words how the :minimal-NG strategy works. Does
;; it improve upon the :imitation strategy?

;;; Extra:

;;; Extra question 1: 
;; Choose any of the remaining strategies (e.g. :lateral-inhibition,
;; or :frequency) and play around with any of the parameters
;; (e.g. population size, nr-of-objects, :who-aligns?, ...)

;;; Extra question 2:
;; Is there a special relation between lateral inhibition strategies
;; and the minimal-NG strategy (hint: there is) and explain.

;;; Extra question 3:
;; Of all strategies, which strategies require the capacity to
;; enumerate competitors and which do not.


