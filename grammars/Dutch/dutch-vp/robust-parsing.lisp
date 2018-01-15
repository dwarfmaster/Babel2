(in-package :dutch-vp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File by Paul                                           ;;
;; Functions for robust parsing, based on re-entrance     ;;
;; November-December 2014                                 ;;
;; Adapted to the new FCG notation November-December 2015 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-robust (form &key (ranking-metric 'levenshtein-distance) (cxn-inventory *fcg-constructions*) (search-beam nil))
  "Takes a list of strings and optionally a hypothesis ranking strategy as input. Parses the input. If no correct parse could be found, it tries to
   find tense indications (see get-tense), renames the variables in the incomplete parse and produces all possibilities. Then, based on the
   requested strategy, it returns the highest ranked possibility"
  ;; 1. Comprehend the input utterance using the grammar
  (multiple-value-bind (original-parse-result node)
      (comprehend form :cxn-inventory cxn-inventory)
    (if (solution-p node)
      ;; if you find a solution, return it
      original-parse-result
      ;; else, reconstruct...
      (let* ((tense-predicates (get-tense form cxn-inventory)) ;; ... the tense ...
             (meaning-predicates (comprehend-without-tense form cxn-inventory)) ;; ... and as much meaning as possible
             (possible-corrections (formulate-all (rename-variables-uniquely (append tense-predicates meaning-predicates)) :cxn-inventory cxn-inventory :n search-beam))) ;; get possibilities!
        ;; for the different strategies
        (cond
         ;; For the WORD-ORDER STRATEGY
         ((equal ranking-metric 'word-order)
          (let* ((infinitives-of-input (get-infinitives form cxn-inventory)) ;; get the infinitives of the words
                 (a-list-of-possible-corrections-and-metric)) 
            (dolist (possible-correction possible-corrections) ;; loop through the correction hypotheses
              (push `(,possible-correction . ,(compare-infinitives-word-order infinitives-of-input (get-infinitives possible-correction cxn-inventory)))
                    a-list-of-possible-corrections-and-metric)) ;; add (score . correction) to the list
            (let ((best-guess (caar (sort a-list-of-possible-corrections-and-metric #'> :key #'cdr)))) ;; sort the list (best hypothesis first)
              (values (comprehend best-guess :cxn-inventory cxn-inventory) best-guess)))) ;; return meaning and form of the highest ranked hypothesis
         ;; For the NUMBER-OF-ERRORS STRATEGY
         ((equal ranking-metric 'number-of-errors)
          (let (a-list-of-possible-corrections-and-metric)
            (dolist (possible-correction possible-corrections) ;; loop through the correction hypotheses
              (push `(,possible-correction . ,(compare-forms possible-correction form))
                    a-list-of-possible-corrections-and-metric)) ;; add (score . correction) to the list
            (let ((best-guess (caar (sort a-list-of-possible-corrections-and-metric #'> :key #'cdr)))) ;; sort the list (best hypothesis first)
              (values (comprehend best-guess :cxn-inventory cxn-inventory) best-guess)))) ;; return meaning and form of the highest ranked hypothesis
         ;; For the LEVENSHTEIN DISTANCE STRATEGY
         ((equal ranking-metric 'levenshtein-distance)
          (let* ((a-list-of-possible-corrections-and-metric))
            (dolist (possible-correction possible-corrections) ;; loop through the correction hypotheses
              (push `(,possible-correction . ,(levenshtein-distance-lists form possible-correction))
                    a-list-of-possible-corrections-and-metric)) ;;add (score . correction) to the list
            (let ((best-guess (caar (sort a-list-of-possible-corrections-and-metric #'< :key #'cdr)))) ;; sort the list (best hypothesis first)
              (values (comprehend best-guess :cxn-inventory cxn-inventory) best-guess))))))))) ;; return meaning and form of the highest ranked hypothesis

(defun compare-forms (comparatum comparandum)
  "takes as input two lists of strings. Retuns the number of elements
   occuring in both lists."
  (let ((equal-forms 0)
        (comparatum-copy (copy-object comparatum)))
    (loop for form in comparandum
          do
          (when (find form comparatum-copy :test #'equalp)
            (setf comparatum-copy (remove form comparatum-copy))
            (incf equal-forms)))
     equal-forms))

(defun rename-variables-uniquely (list-of-meaning-predicates)
  "takes as input a list of meaning predicates and returns
   the same list, but with all variables uniquely renamed
   e.g. (rename-variables-uniquely '((a ?b) (a ?b) (a d)))
   returns ((a ?b-11)(a ?b-13)(a d))"
  (let (list-of-meaning-predicates-with-renamed-variables)
    (dolist (meaning-predicate list-of-meaning-predicates)
      (let (meaning-predicate-with-renamed-variables)
        (dolist (meaning meaning-predicate)
          (if (variable-p meaning)
            (push (make-var meaning) meaning-predicate-with-renamed-variables)
            (push meaning meaning-predicate-with-renamed-variables)))
        (push (reverse meaning-predicate-with-renamed-variables) list-of-meaning-predicates-with-renamed-variables)))
    (reverse list-of-meaning-predicates-with-renamed-variables)))

(defun compare-infinitives-word-order (comparatum comparandum)
  "takes as input two lists of strings. Loops synchroneously trough
   both lists and compares each element. Retuns the number of equal
   elements at the same position in the list."
  (let ((equal-infinitives 0))
        (loop for inf1 in comparatum
              for inf2 in comparandum
              do
              (when (equal inf1 inf2)
                (incf equal-infinitives)))
    equal-infinitives))

(defun get-infinitives (list-of-verb-forms fcg-cxn-inventory)
  "Takes list of strings as input, returns a list with their infinitives."
  (let (list-of-infinitives)
    (dolist (verb-form list-of-verb-forms)
      (dolist (cxn (constructions fcg-cxn-inventory))
        (when (equalp (symbol-name (cdr (assoc :label (attributes cxn)))) "morph")
          (let ((comprehension-lock (comprehension-lock (first (conditional-part cxn)))) ;; morph cxn only have one conditional-unit
                (formulation-lock (formulation-lock (first (conditional-part cxn)))))
            ;; Check whether this construction expresses verb-form
            (dolist (top-feature comprehension-lock)
              (when (listp top-feature)
                (when (eql (feature-name top-feature) 'hash)
                  (when (equalp (third (caaddr top-feature)) verb-form)
                    ;; Search for the infinitive
                    (dolist (feature formulation-lock)
                      (when (equalp (symbol-name (feature-name feature)) "syn-cat")
                        (dolist (subfeature (cdr feature))
                          (when (listp subfeature)
                            (when (equalp (symbol-name (feature-name subfeature)) "lemma")
                                  (push (second subfeature) list-of-infinitives)
                                  (return))))))))))))))
    (reverse list-of-infinitives)))

(defun get-tense (form fcg-cxn-inventory)
  "requires a list of strings as input. Parses the first string morphologically and if it can find
   (present +) on the right place, it returns '((deictic-time-point origo) (overlaps highest-event origo))
   Otherwise, it returns '((deictic-time-point origo) (before highest-event origo)). If there is no tense specification
   in the feature structure, it performs the function recursively on the cdr of the list. If there could be found no tense
   specification at all, the function returns the by default '((deictic-time-point origo) (overlaps highest-event origo)) (present tense)"
  (let ((copy-of-cxn-set (copy-object fcg-cxn-inventory))
        (tense nil))
    (set-configuration copy-of-cxn-set :parse-goal-tests '(:no-applicable-cxns))
    (set-configuration copy-of-cxn-set :parse-order '(morph))
    (loop for word in form
          until tense
          do
          (multiple-value-bind (meaning cip-solution) (comprehend (list word) :cxn-inventory copy-of-cxn-set :silent t)
            (declare (ignore meaning))
            (let ((ts (left-pole-structure (car-resulting-cfs (cipn-car cip-solution)))))
              (cond ((member-of-tree '(present +) ts)
                     (setf tense '((time-point deictic ?origo) (time-relation overlaps ?highest-event ?origo))))
                    ((member-of-tree '(past +) ts)
                     (setf tense '((time-point deictic ?origo) (time-relation before ?highest-event ?origo))))))))
    (if tense
      tense
      '((time-point deictic ?origo) (time-relation overlaps ?highest-event ?origo)))))

(defun comprehend-without-tense (form fcg-cxn-inventory)
  "returns the partial meaning without using the tense-constructions"
  (let ((copy-of-cxn-set (copy-object fcg-cxn-inventory)))
    (set-configuration copy-of-cxn-set :parse-goal-tests '(:no-applicable-cxns))
    (set-configuration copy-of-cxn-set :parse-order '(morph lex vp aspect modality))
    (comprehend form :cxn-inventory copy-of-cxn-set :silent t)))

(defun levenshtein-distance-lists (L1 L2)
  "Calculates the Levenshtein distance between the elements of two lists, returns an editing distance (int)."
  (let ((n (length L1))
	(m (length L2)))
    ;; Check trivial cases
    (cond ((= 0 n) (return-from levenshtein-distance-lists m))
	  ((= 0 m) (return-from levenshtein-distance-lists n)))
    (let ((col (make-array (1+ m) :element-type 'integer))
	  (prev-col (make-array (1+ m) :element-type 'integer)))
      ;; We need to store only two columns---the current one that
      ;; is being built and the previous one
      (dotimes (i (1+ m))
	(setf (svref prev-col i) i))
      ;; Loop across all elements of each list
      (dotimes (i n)
	(setf (svref col 0) (1+ i))
	(dotimes (j m)
	  (setf (svref col (1+ j))
		(min (1+ (svref col j))
		     (1+ (svref prev-col (1+ j)))
		     (+ (svref prev-col j)
			(if (equalp (nth i L1) (nth j L2)) 0 1)))))
	(rotatef col prev-col))
      (svref prev-col m))))
