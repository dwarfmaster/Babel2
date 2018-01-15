
(in-package :fcg)

;; Taken from https://stackoverflow.com/a/4367540
(defun string-replace2 (search replace string &optional count)
  (loop for start = (search search (or result string)
                            :start2 (if start (1+ start) 0))
        while (and start
                   (or (null count) (> count 0)))
        for result = (concatenate 'string
                                  (subseq (or result string) 0 start)
                                  replace
                                  (subseq (or result string)
                                          (+ start (length search))))
        do (when count (decf count))
        finally (return-from string-replace2 (or result string))))

(defmethod make-html ((d gp-data) &key (configuration nil))
  (let ((s-dot-graph (unit-bindings->graph :data d :prefered-font "Arial"
                                           :visualization-configuration configuration)))
    `((div)
     ,(string-replace2 "Â¬" "¬" (s-dot->svg s-dot-graph)))))

;(activate-monitor trace-fcg)
;(add-element (make-html (read-data)))