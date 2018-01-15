(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; One Application of anti-unification is to make FCG's matching phase more flexible. ;;
;;                                                                                    ;;
;; Functions contributing to this belong in this file...                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun match-robust (processing-cxn transient-structure direction &key (cost-params nil))
  "Takes a processing-cxn and a transient structure and returns a processing-cxn
   that matches the transient-structure and its anti-unification."
  (let ((matching-pattern (matching-pattern processing-cxn direction))
        (source (left-pole-structure transient-structure)))
    (cond
     ;; Case: matching-pattern matches with transient structure
     ((match-structures matching-pattern source)
      (values processing-cxn 0))
     ;; Case: matching-pattern does not match with transient structure
     (t
      (let* ((a-u (first (anti-unify matching-pattern source :fcg +no-bindings+ +no-bindings+ :cost-params cost-params)))
             (a-u-matching-pattern (first a-u))
             (pattern-bindings (second a-u))
             (cost (fifth a-u))
             (removed-pattern-unit (first (sixth a-u)))
             (processing-cxn (if removed-pattern-unit
                               (remove-unit-from-cxn removed-pattern-unit processing-cxn)
                               processing-cxn))
             (cxn-new-bindings (substitute-bindings-cxn (reverse pattern-bindings) processing-cxn))
             (anti-unified-cxn (replace-matching-pattern cxn-new-bindings a-u-matching-pattern direction)))
        (values anti-unified-cxn cost))))))

(defun remove-unit-from-cxn (removed-pattern-unit processing-cxn)
  (let ((new-cxn (copy-object processing-cxn))
        (new-left-pole nil)
        (new-right-pole nil))
    ;; For left pole
    (dolist (unit (left-pole-structure processing-cxn))
      (if (j-unit-p unit)
        (unless (equalp (j-unit-name unit) removed-pattern-unit)
          (push unit new-left-pole))
        (unless (equalp (unit-name unit) removed-pattern-unit)
          (push unit new-left-pole))))
    ;; For right pole
    (dolist (unit (right-pole-structure processing-cxn))
      (if (j-unit-p unit)
        (unless (equalp (j-unit-name unit) removed-pattern-unit)
          (push unit new-right-pole))
        (unless (equalp (unit-name unit) removed-pattern-unit)
          (push unit new-right-pole))))
    (setf (left-pole-structure new-cxn) (reverse new-left-pole))
    (setf (right-pole-structure new-cxn) (reverse new-right-pole))
    new-cxn))

(defun match-robust-specialise (processing-cxn transient-structure direction)
  ""
  (let ((matching-pattern (matching-pattern processing-cxn direction))
        (source (left-pole-structure transient-structure)))
    (cond
     ;; Case: matching-pattern matches with transient structure
     ((match-structures matching-pattern source)
      processing-cxn)
     ;; Case: matching-pattern does not match with transient structure
     (t
      (let* ((a-u (first (anti-unify matching-pattern source :fcg-specialise)))
             (a-u-matching-pattern (first a-u))
             (pattern-bindings (second a-u))
             (cxn-new-bindings (substitute-bindings-cxn (reverse pattern-bindings) processing-cxn))
             (anti-unified-cxn (replace-matching-pattern cxn-new-bindings a-u-matching-pattern direction)))
        anti-unified-cxn)))))

(defun matching-pattern (processing-cxn direction)
  "returns the pattern that will be matched during processing"
  (let (matching-pattern)
    (if (eql direction '<-)
      (setf matching-pattern (right-pole-structure processing-cxn))
      (setf matching-pattern (left-pole-structure processing-cxn)))
    (remove-j-units matching-pattern)))

(defun replace-matching-pattern (processing-cxn matching-pattern direction)
  "Replaces the matching-pattern of processing-cxn with matching-pattern
   Effectively destroys processing-cxn!!"
    (let ((j-units (get-j-units (pole-structure (match-pole processing-cxn direction)))))
      (setf (pole-structure (match-pole processing-cxn direction)) (append matching-pattern j-units))
      processing-cxn))
