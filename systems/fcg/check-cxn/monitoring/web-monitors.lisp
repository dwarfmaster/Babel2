;; Event-handler to output the results of the check-cxn-tests

(in-package :fcg)

(define-event-handler (trace-fcg-processing-level check-cxns-done)
  (let ((report-items (get-report-items report)))
    (when (and (> (length report-items) 0) (item (first report-items)))
      (add-element `((p) ,(string-append "<p style='color:darkorange'><b>Warning: " (string (description report)) "</b></p>")))
      (loop for report-item in report-items
         do (add-element `((p) ,(message report-item) ,(html-pprint (item report-item))))))))