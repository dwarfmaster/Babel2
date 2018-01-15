
(in-package :fcg)
          


(defmethod make-html ((d gp-data) &key (configuration nil))
  (let ((s-dot-graph (unit-bindings->graph :data d :prefered-font "Arial"
                                           :visualization-configuration configuration)))
    `((div)
     ,(s-dot->svg s-dot-graph))))

;(activate-monitor trace-fcg)
;(add-element (make-html (read-data)))