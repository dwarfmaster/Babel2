(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anti-unifying FCG constructions is not easy, mainly because the construction and transient structure  ;;
;; consist of sets of units (free order) of which the unit-names are variables. Therefore, we first need ;;
;; to pair up all units from the construction with a unit from the transient structure.                  ;;
;;                                                                                                       ;;
;; The functions which achieve this are located in this file...                                          ;;
;;                                                                                                       ;;
;; In short, we loop over all units in the construction (pattern) an find out which units from the       ;;
;; transient structure match them. Then, a search process pairs up all possibilities: units that match   ;;
;; one or more units in the transient structure are always paired up with them, other units from the     ;;
;; construction get paired to all combination of remaining units in the transient structure.             ;;
;;                                                                                                       ;;
;; It is implemented as a search process now and should always return all possibilities                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun reorder-source-units (pattern source cost-params)
  "Input: list of cxn-units as pattern
          list of transient structure units as source
          list of cost-paramaters for calculating reordering-cost
   Returns: list of subsets of source with:
            - as many units as in pattern
            - aligned order (matching-units at same place, all combinations of other units)"
  (let ((reordering-solutions (find-matching-units pattern source cost-params))
        (list-of-reordered-sources nil))
    ;; Loop over possible reorderings
    (dolist (rs reordering-solutions)
      (let ((solution-structure (solution-structure rs))
            (reordered-source (loop for n from 1 to (length pattern)
                                    collect n))
            (removed-pattern-units nil)
            (pattern-unit-to-remove nil))
        ;; Loop over units
        (dolist (unit solution-structure)
          (if (equalp (cdr unit) 'remove-unit)
            (setf pattern-unit-to-remove (car unit))
            (let* ((pattern-index (- (car unit) 1))
                   (source-index (- (cdr unit) 1))
                   (source-feature (nth source-index source)))
              (setf (nth pattern-index reordered-source) source-feature))))
        (if pattern-unit-to-remove
          (progn
            (setf reordered-source (delete pattern-unit-to-remove reordered-source))
            (push (unit-name (nth (- pattern-unit-to-remove 1) pattern)) removed-pattern-units)
            (unless (equalp 'root (unit-name (nth (- pattern-unit-to-remove 1) pattern)))
              (push (list reordered-source (cost rs) removed-pattern-units) list-of-reordered-sources)))
          (push (list reordered-source (cost rs) nil) list-of-reordered-sources))))
    (sort list-of-reordered-sources '< :key 'second)))

(defun find-matching-units (pattern source cost-params)
  "given pattern and source (lists of fcg-units), and cost-params return:
   - list of objects with all possibilities for binding units in pattern with source, maximizing number of matching
     units objects contain: solution-structure, cost and non-matched-units of source"
  (let ((source-unit-numbers (loop for n from 1 upto (length source)
                              collect n))
        (all-matching-units nil)
        (pattern-length (length pattern)))
    (loop for pattern-unit in pattern
          for i from 1 upto pattern-length
          do
          (let ((matches-for-this-unit nil))
            (loop for source-unit in source
                  for j from 1 upto (length source)
                  do
                  (when (match-structures (list pattern-unit) (list source-unit))
                    (push j matches-for-this-unit)))
            (push (cons i matches-for-this-unit) all-matching-units)))
    (search-reordering-solutions all-matching-units
                                 (if (> (length pattern) 1)
                                   (cons 'remove-unit source-unit-numbers)
                                   source-unit-numbers)
                                 pattern-length
                                 cost-params)))
  
(defun search-reordering-solutions (matching-units source-unit-numbers pattern-length cost-params)
  "Given an a-list of numbers of matching units, the numbers of units in source, the number of units
   in the construction and cost params, returns a series of solution states
   This is implemented as a search process."
  (let ((solutions nil)
        (queue (list (make-instance 'reordering-search-state
                                    :solution-structure (loop for i from 1 upto pattern-length
                                                              collect
                                                              (list i))
                                    :remaining-matching-units matching-units
                                    :remaining-source-units source-unit-numbers
                                    :cost 0))))
    (loop until (not queue)
          do
          (let ((current-state (pop queue)))
            (if (reordering-solution-p (solution-structure current-state))
              ;; If current-state is solution: add to solutions
              (unless (duplicate-search-state current-state solutions)
                (push current-state solutions))
              ;; Else: see wheter there are still units in solution structure which had matches
              (let ((non-matched-units (loop for unit in (solution-structure current-state)
                                             when (and (not (cdr unit)) (find (first unit) (remaining-matching-units current-state) :key 'first))
                                             collect (first unit))))
                (if non-matched-units
                  ;; If there are some, add their expansions as states to the queue
                  (dolist (nmu non-matched-units)
                    (let ((possible-matches (loop for mu in (remaining-matching-units current-state)
                                                  when (eq (car mu) nmu)
                                                  collect (second mu))))

                      (dolist (pm possible-matches)
                        (let ((new-search-state (make-instance 'reordering-search-state
                                                               :solution-structure (substitute (cons nmu pm) nmu (solution-structure current-state) :key 'first)
                                                               :remaining-matching-units (remove pm (remaining-matching-units current-state) :key 'cdr)
                                                               :remaining-source-units (remove pm (remaining-source-units current-state))
                                                               :cost (cost current-state))))
                          (unless (duplicate-search-state new-search-state queue)
                            (push new-search-state queue))))))
                  ;; Else: assign other non-matching units to these
                  (let ((non-matched-unit (loop for unit in (solution-structure current-state)
                                                unless (cdr unit)
                                                return (first unit))))
                    (dolist (su (remaining-source-units current-state))
                      (push
                       (make-instance 'reordering-search-state
                                      :solution-structure (substitute (cons non-matched-unit su) non-matched-unit (solution-structure current-state) :key 'first)
                                      :remaining-matching-units '()
                                      :remaining-source-units (remove su (remaining-source-units current-state))
                                      :cost (if (equalp su 'remove-unit)
                                              (+ (cost current-state) (get-anti-unification-cost 'removed-pattern-unit cost-params nil nil))
                                              (+ (cost current-state) (get-anti-unification-cost 'non-matching-unit cost-params nil nil))))
                       queue))))))))
    solutions))

;; ################################################################
;; Defining reordering-search-states and helper methods for these #
;; ################################################################

(defclass reordering-search-state ()
  ((solution-structure
    :type (or list null)
    :initform nil
    :initarg :solution-structure
    :accessor solution-structure)
   (remaining-matching-units
    :type (or list null)
    :initform nil
    :initarg :remaining-matching-units
    :accessor remaining-matching-units)
   (remaining-source-units
    :type (or list null)
    :initform nil
    :initarg :remaining-source-units
    :accessor remaining-source-units)
   (cost
    :initform 0
    :initarg :cost
    :accessor cost)))

(defun reordering-solution-p (solution-structure)
  "solution structure is a solution if it has a non-nil cdr
   returns true if so, false otherwise"
  (let ((solution t))
    (dolist (s solution-structure)
      (unless (cdr s)
        (setf solution nil)))
    solution))

(defun duplicate-search-state (nss queue)
  "returns nil if nss is a search space with equivalent solution-structure
   to any search state in queue"
  (let ((duplicate-p nil))
    (dolist (ss queue)
      (when (equal (solution-structure nss) (solution-structure ss))
        (setf duplicate-p t)))
    duplicate-p))


