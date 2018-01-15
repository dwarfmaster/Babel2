(in-package :meta-layer-learning)

(export '(trace-learning trace-learning-verbose))

;; ############################################################################
;; Monitors for tracing learning mechanisms
;; ----------------------------------------------------------------------------

(export '(repairing-started repairing-finished repaired
          diagnostic-started diagnostic
          diagnostic-returned-problems
          diagnosed-problems
          repair-strategy
          repaired-thing))

;; diagnostic started
(define-event diagnostic-started (diagnostic t)(id t))

;; diagnostic returned problem
(define-event diagnostic-returned-problems (diagnostic t) (problems list))

;; when diagnosed problems
(define-event diagnosed-problems (diagnostic  t)(problems list))

;; when repairing started
(define-event repairing-started (repair-strategy t) (problem t) 
	      (repaired-thing t))

;; when repairing finished
(define-event repairing-finished (repair-strategy t) (repaired t) 
	      (repaired-thing t))

(define-event repaired (repair-strategy t) (repaired-thing t))
