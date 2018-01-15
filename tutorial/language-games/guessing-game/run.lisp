
(asdf:operate 'asdf:load-op 'guessing-game)

(in-package :guessing-game)


(progn
  ;;(activate-monitor trace-interaction)
  (activate-monitor trace-tasks-and-processes)
  (activate-monitor trace-learning)
  (activate-monitor print-a-dot-for-each-interaction))


(defparameter *experiment*
  (make-instance 'minimal-gg-experiment
                 :configuration '((:population-size . 5)
                                  (:total-nr-of-objects . 10)
                                  (:context-size . 2)
                                  (:strategy . minimal))))

(run-interaction *experiment*)


(deactivate-all-monitors)

(activate-monitor display-success)
(activate-monitor print-a-dot-for-each-interaction)

(run-series *experiment* 250)

