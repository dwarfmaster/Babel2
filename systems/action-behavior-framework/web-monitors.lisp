
(in-package :action-behavior-framework)

;; ============================================================================
;; run-agent-started/run-agent-finished
;; ============================================================================

(define-event-handler (trace-interaction run-agent-started)
  (add-element `((hr)))
  (add-element `((h3) ,(format nil "Running agent ~a (~(~a~))." 
				  (id agent) (discourse-role agent)))))

;; ============================================================================
;; consolidation-started
;; ============================================================================

(define-event-handler (trace-interaction consolidation-started)
  (add-element `((hr)))
  (add-element `((h3) ,(format nil "Consolidating agent ~a (~(~a~))." 
                               (id agent) (discourse-role agent)))))

;; ============================================================================
;; world-updated
;; ============================================================================

(define-event-handler (trace-interaction world-updated)
  (add-element (make-html (first (actions world)))))
