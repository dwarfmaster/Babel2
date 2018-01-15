
(in-package :experiment-framework)

;; ############################################################################
;; trace-interaction monitor:
;; ----------------------------------------------------------------------------

(export '(trace-interaction-in-repl
	  trace-experiment-in-repl
	  print-a-dot-for-each-interaction))

(define-monitor trace-interaction-in-repl
                :class 'trace-monitor
                :documentation "Prints some high-level information about each interaction")

(define-event-handler (trace-interaction-in-repl interaction-started)
  (format (monitor-stream monitor) "~%")
  (print-with-overline monitor #\# 
		       (format nil "# Started interaction ~a."
			           interaction-number)))

(define-event-handler (trace-interaction-in-repl interacting-agents-determined)
  (format (monitor-stream monitor) "~%# Interacting agents: ~a" 
	  (interacting-agents experiment)))

;; ############################################################################
;; trace-experiment-in-repl monitor:
;; ----------------------------------------------------------------------------

(define-monitor trace-experiment-in-repl :class 'trace-monitor
		:documentation "Prints some information about series and batches.")

(define-event-handler (trace-experiment-in-repl interaction-started)
  (when (= (mod (interaction-number interaction) 100) 0)
    (format-monitor "~%# Interaction ~a." (interaction-number interaction))))

(define-event-handler (trace-experiment-in-repl series-finished)
  (format-monitor "~%# Finished series ~a." series-number))

(define-event-handler (trace-experiment-in-repl batch-finished)
  (print-with-overline monitor #\# "# Finished batch."))

;; ############################################################################
;; print-a-dot-for-each-interaction monitor:
;; ----------------------------------------------------------------------------

(define-monitor print-a-dot-for-each-interaction 
                :documentation "Prints a '.' for each interaction")

(define-event-handler (print-a-dot-for-each-interaction interaction-finished)
  (format t "."))

;; ############################################################################
;; Data monitors
;; ----------------------------------------------------------------------------

(export '(default-record-communicative-success))

(define-monitor default-record-communicative-success 
                :class 'data-recorder
                :documentation "records the communicative success
	                        of the first of the interacting agents.")

(define-event-handler (default-record-communicative-success interaction-finished)
  (record-value monitor 
                (if (communicated-successfully interaction)
                  1 0)))

;; ############################################################################
