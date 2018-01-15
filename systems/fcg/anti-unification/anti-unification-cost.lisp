(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function for dispatching cost calculation to right function ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-anti-unification-cost (cost-type cost-params pattern source)
  "if cost-type is found in cost-params, return cost of this cost-type
   otherwise return 0"
  (let ((cost (assoc cost-type cost-params :test 'string=)))
    (cond ((not cost)
           (warn "You didn't specify a cost for ~a, taking 0 as default cost." cost-type)
           0)
          ((numberp (second cost))
           (second cost))
          (t
           (let ((base-cost (funcall (second cost) pattern source)))
             (* base-cost (third cost)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for calculating cost based on pattern and source  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun depth-of-replaced-pattern (pattern source)
  (declare (ignore source))
  (if (variable-p pattern)
    0
    (1+ (depth pattern))))

