
(in-package :experiment-framework)

(export '(trace-interaction))

(define-monitor trace-interaction
                :documentation "Displays information about an interaction in the web interface")

;; ============================================================================
;; interaction-started/interaction-finished
;; ============================================================================

(define-event-handler (trace-interaction interaction-started)
  (add-element `((h2) ,(format nil "Interaction ~a"
                               interaction-number))))

(define-event-handler (trace-interaction interaction-finished)
  (add-element
   `((p) "Interaction " 
     ((b) ,(if (communicated-successfully interaction)
             "succeeded" "failed")))))


