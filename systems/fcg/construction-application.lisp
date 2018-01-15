
(in-package :fcg)

;;;;; Application of a single construction

(export '(fcg-apply -> <- 
	  cxn-application-result make-cxn-application-result
	  car-status car-source-cfs car-applied-cxn car-match-bindings
	  car-first-merge-bindings car-second-merge-bindings 
          car-first-merge-structure car-second-merge-structure 
          car-resulting-cfs car-direction car-first-merge-added
	  car-second-merge-added set-self get-self get-current-cxn
          set-current-direction get-current-direction))


(defgeneric fcg-apply (cxn/construction-inventory cfs direction 
                                                  &key &allow-other-keys))


;; ############################################################
;; cxn-application-result
;; ############################################################

(defstruct (cxn-application-result (:conc-name car-))
  (status 'initial)
  ;; status can be 
  ;; initial cxn-matched cxn-applied first-merge-failed bad-hierarchy-after-first-merge
  ;; second-merge-failed bad-hierarchy-after-second-merge structure-not-changed
  (source-cfs nil)
  (applied-cxn nil)
  (match-bindings nil)
  (first-merge-bindings nil)
  (second-merge-bindings nil)
  (first-merge-structure nil)
  (second-merge-structure nil)
  (first-merge-added nil)
  (second-merge-added nil)
  (resulting-cfs nil)
  (direction nil))


(defmethod print-object ((car cxn-application-result) stream)
  (if *print-pretty* 
      (pprint-logical-block (stream nil)
	(format stream "<car ~(~a~):~:_ source-cfs:~:_  ~a,~:_ applied-cxn: ~a,~
                        ~:_ match-bindings:~:_  ~w,~:_ first-merge-bindings:~:_  ~:w,~
                        ~:_ second-merge-bindings:~:_  ~w,~
                        ~:_ resulting-cfs:~:_  ~:w,~:_ direction: ~a>"
		(car-status car) (car-source-cfs car) 
		(when (car-applied-cxn car) (name (car-applied-cxn car)))
		(car-match-bindings car) (car-first-merge-bindings car) 
		(car-second-merge-bindings car) (car-resulting-cfs car) (car-direction car))
	(format stream ")"))
      (format stream "<car ~(~a~)~:[~; ~(~a~)~]>" (car-status car) (car-applied-cxn car)
	      (when (car-applied-cxn car) (name (car-applied-cxn car))))))


(defmethod copy-object ((car cxn-application-result))
  (copy-structure car))

(defmethod left-pole-structure ((car cxn-application-result))
  (left-pole-structure (car-resulting-cfs car)))
(defmethod right-pole-structure ((car cxn-application-result))
  (right-pole-structure (car-resulting-cfs car)))

(defmethod left-pole-domain ((car cxn-application-result))
  (left-pole-domain (car-resulting-cfs car)))
(defmethod right-pole-domain ((car cxn-application-result))
  (right-pole-domain (car-resulting-cfs car)))



;; ############################################################
;; helper functions
;; ############################################################

(defun select-source (cxn pattern source)
  (declare (ignore pattern))
  (ecase (match-source cxn)
    (full source)
    (parents-only
     (get-units-without-superunits source))))

(defun match-poles (cxn coupled-feature-structure direction
                        &optional (bindings +no-bindings+) &key cxn-inventory)
  (let* ((matching-pole (ecase direction
                          (-> (left-pole cxn))
                          (<- (right-pole cxn))))
         (source (ecase (pole-domain matching-pole)
                   (sem (left-pole-structure
                         coupled-feature-structure))
                   (syn (right-pole-structure
                         coupled-feature-structure))))
         (matching-pattern (pole-structure matching-pole)))
    (match-structures matching-pattern
                      (select-source cxn
                                     matching-pattern
                                     source)
                      bindings :cxn-inventory cxn-inventory)))

(defun merging-pole (cxn direction)
  ;; based on the direction it gives you the pole that will be merged
  (ecase direction 
    (-> (left-pole cxn))
    (<- (right-pole cxn))))

(defun merge-poles (cxn coupled-feature-structure direction
                        &optional (bindings +no-bindings+) source unified configuration second-merge? &key cxn-inventory)
  ;; If there's a configuration given...
  (when (and configuration 
             (get-configuration configuration 
                                'replace-symbols-with-their-original-variable))
    (setf bindings (replace-symbols-with-their-original-variable bindings)))
  ;; Extract the features to merge from the transient structure
  (let* ((merging-pole (merging-pole cxn direction))
         (merging-pattern (pole-structure merging-pole)))
    ;; Check whether it's formulation, second-merge, FCG-Light 
    (when (and second-merge?        ;; second-merge
               (eq direction '<-)   ;; direction of second-merge in formulation
               (cxn-inventory cxn)) ;; FCG-light
      ;; Get a list of negations from the non-j-units in the merging-pole
      (let ((negations (get-negations (remove-j-units merging-pattern))))
        (when negations
          ;; set merging-pattern to new merging-pattern without negations of features added by the same cxn
          (setf merging-pattern (remove-negations-of-added-features negations merging-pattern cxn))))
      ;; Now, check for overwritten feature
      (let ((ow-feat (get-overwrites cxn)))
        (when ow-feat
          ;; set merging-pattern to new merging-pattern without negations of features added by the same cxn
          (setf merging-pattern (remove-overwrites ow-feat merging-pattern)))))
    ;; Merge the merging-pattern with the transient structure.
    (merge-structures
     merging-pattern
     (or source
         (ecase (pole-domain merging-pole)
           (sem (left-pole-structure coupled-feature-structure))
           (syn (right-pole-structure coupled-feature-structure))))
     bindings unified :cxn-inventory cxn-inventory)))

;; For replacing the overwrite feature in the second merge-formulation
(defun remove-overwrites (ow-feat merging-pattern)
  (let ((new-merging-pattern nil))
    (dolist (unit merging-pattern)
      (let ((rel-f (find (if (j-unit-p unit) (j-unit-name unit) (unit-name unit))
                         ow-feat
                         :key 'cdr :test 'string=)))
        (if rel-f ;; looks at this point like (((-> x y) (-> y z)) . unit)
          (let ((new-unit (copy-object unit)))
            (dolist (f (first rel-f))
              (setf new-unit (replace-overwrite f unit)))
            (push new-unit new-merging-pattern))
          (push unit new-merging-pattern))))
    (reverse new-merging-pattern)))

(defun replace-overwrite (overwr unit)
  "takes an (-> orig new) and replaces it everywhere in unit"
  (let ((orig-pattern (second overwr))
        (new-pattern (third overwr)))
    (cond
     ((null unit) nil)
     ((atom unit) (if (equalp unit orig-pattern)
                    new-pattern
                    unit))
     ((and (listp unit)
           (equalp unit orig-pattern))
      new-pattern)
     (t (mapcar #'(lambda (x)
                    (replace-overwrite overwr x))
                unit)))))

(defun get-overwrites (cxn)
  "returns overwritten features as a list, with the features per unit
  ((((-> (x) (y)) (-> y z)) . unit-1) (((-> (l) (m)) (-> n o)) . unit-2))"
  (let ((left-pole-j-units (get-j-units (left-pole-structure cxn)))
        (overwr-features nil))
    (dolist (unit left-pole-j-units)
      (let ((unit-name (j-unit-name unit))
            (overwr-feat (find-overwrites unit)))
        (when overwr-feat
          (push (cons overwr-feat unit-name) overwr-features))))
    overwr-features))

(defun find-overwrites (tree)
  "finds '-> features in tree (unit) and returns them as a list"
    (cond ((atom tree)
           nil)
          ((equalp '-> (first tree))
           (append (list tree) (find-overwrites (cdr tree))))
          (t
           (append (find-overwrites (car tree))
                   (find-overwrites (cdr tree))))))

(defun get-negations (pattern)
  "returns the negations in a pattern, list per unit, relies on get-unit-negations"
  (let (list-of-negations)
    (dolist (unit pattern)
      (let ((negations (get-unit-negations (rest unit))))
        (when negations
          (push (append (list (first unit)) negations) list-of-negations))))
    list-of-negations))

(defun get-unit-negations (pattern)
  "returns a list of negations in a feature-structure"
  (cond ((atom pattern) nil)
        ((or (and (atom (car pattern)) (string= (mkstr (car pattern)) 'footprints)) (eql (car pattern) '==0))
          (list pattern))
	((consp pattern)
	 (append (get-unit-negations (car pattern))
		 (get-unit-negations (cdr pattern))))))

;; Not waterproof! only removes ==0 x  when litterally found in ts,
;; the problem is solved for footprints only
(defun remove-negations-of-added-features (list-of-negated-features pattern cxn)
  "removes the negated features in pattern if they were added in first merge by the same cxn"
  ;; first, we check which features are added in de j-units of the construction
  (let ((harmful-negations nil)
        (first-merge-js (get-j-units (left-pole-structure cxn))))
    (dolist (negated-feature list-of-negated-features)
      (let ((corresponding-j-unit (find (first negated-feature) first-merge-js :key #'j-unit-name :test #'string=)))
        (when corresponding-j-unit
          (let (harmful-negations-for-unit)
            (dolist (negation (rest negated-feature))
              ;; If harmful footprints: push 'footprints to harmful negations for this unit
              (if (and (atom (first negation)) (string= (first negation) 'footprints)
                       (find-if #'(lambda(x) (and (consp x) (string= (feature-name x) 'footprints))) (rest corresponding-j-unit)))
                (push 'footprints harmful-negations-for-unit)
                ;; Otherwise, if negation occurs litterally in j-unit, push it as well
                (when (member-of-tree (second negation) corresponding-j-unit)
                  (push negation harmful-negations-for-unit))))
            (push (cons (first negated-feature) harmful-negations-for-unit) harmful-negations)))))
    ;; then, we remove these harmful negations from the corresponding units in the pattern
    (when harmful-negations
      (dolist (unit harmful-negations)
        (dolist (negation (cdr unit))
          (if (and (atom negation) (string= 'footprints negation))
            (setf pattern (remove-footprints-feature pattern (car unit)))
            (setf pattern (replace-by-variable negation pattern)))))))
    pattern)

(defun remove-footprints-feature (pattern unit-name)
  "deletes footprints features from unit with 'unit-name' in pattern"
  (let (pattern-to-return)
    (dolist (unit pattern)
      (if (and (not (j-unit-p unit)) (string= (first unit) unit-name))
        (push (cons unit-name (remove 'footprints (cdr unit) :test 'string= :key 'first)) pattern-to-return)
        (push unit pattern-to-return)))
    (reverse pattern-to-return)))

(defun compute-resulting-cfs (car)
  (with-slots (resulting-cfs applied-cxn direction first-merge-structure
                             second-merge-structure source-cfs) car
    (let ((pole-1 (ecase direction
                    (-> (left-pole applied-cxn))
                    (<- (right-pole applied-cxn))))
          (pole-2 (ecase direction
                    (-> (right-pole applied-cxn))
                    (<- (left-pole applied-cxn)))))
      ;; create new-cfs
      (setf resulting-cfs (make-instance 'coupled-feature-structure
                                         :data (copy-object (data source-cfs))))
      ;; first pole
      (if (not (eq (left-pole-domain applied-cxn)
                   (right-pole-domain applied-cxn)))
	  (setf (pole-structure (ecase (pole-domain pole-1)
				  (sem (left-pole resulting-cfs))
				  (syn (right-pole resulting-cfs))))
		first-merge-structure)
	  (setf (pole-structure (ecase (pole-domain pole-1)
				  (sem (right-pole resulting-cfs))
				  (syn (left-pole resulting-cfs))))
		(copy-object (ecase (pole-domain pole-1)
			       (sem (pole-structure (right-pole source-cfs)))
			       (syn (pole-structure (left-pole source-cfs)))))))
      ;; second pole
      (setf (pole-structure (ecase (pole-domain pole-2)
                              (sem (left-pole resulting-cfs))
                              (syn (right-pole resulting-cfs))))
            second-merge-structure)
      car)))



;; ############################################################
;; fcg-apply
;; ############################################################

(define-event matched-cxn 
  (cxn construction) (cfs coupled-feature-structure) (direction symbol))

(defun opposite (direction)
  (ecase direction
    (<- '->)
    (-> '<-)))

(defgeneric match-cxn (cxn cfs direction &key cxn-inventory)
  (:documentation "Tries to match the cxn on the given
  cfs. Essntially passes functionality to match-poles"))

(defmethod match-cxn ((cxn construction) (cfs coupled-feature-structure)
                      (direction symbol) &key cxn-inventory)
  "Gets bindings back from match-poles as a list of bindings hypotheses. Loops through these
   and makes a cxn-application-result or each of them."
  (notify matched-cxn cxn cfs direction)
  (loop for match-bindings in (match-poles cxn cfs direction +no-bindings+ :cxn-inventory cxn-inventory)
     collect (make-cxn-application-result
	      :status 'cxn-matched
	      :source-cfs cfs
	      :applied-cxn cxn
	      :direction direction
	      :match-bindings  match-bindings)))


(defun valid-boundaries? (syn-struct)
  (let ((result t))
    (loop for unit in syn-struct
       while result
       do (let ((bs (unit-feature-value unit 'boundaries)))
	    (when bs
	      (setq result (not (equal (first bs) (second bs)))))))
    result))

(defun invalid-merge? (direction car merge-result &key configuration)
  (declare (ignore configuration))
  ;; check 1: no addition of meaning or form:
  (let ((result (> (length (loop for unit in (mr-expr merge-result)
			      append (ecase direction 
				       (-> (unit-feature-value unit 'meaning))
				       (<- (unit-feature-value unit 'form)))))
		   (length (loop for unit 
                              in (ecase direction 
                                   (-> (left-pole-structure (car-source-cfs car)))
                                   (<- (right-pole-structure (car-source-cfs car))))
			      append (ecase direction 
				       (-> (unit-feature-value unit 'meaning))
				       (<- (unit-feature-value unit 'form))))))))
    result))

(let (self direction config construction)
  (defun set-self (transient-structure) (setf self transient-structure))
  (defun get-self () self)
  (defun set-current-direction (direction-symbol)
    (setf direction direction-symbol))
  (defun get-current-direction ()
    direction)
  (defun set-current-cxn (cxn)
    (setf construction cxn))
  (defun get-current-cxn ()
    construction)
  (defun set-current-configuration (configuration)
    (setf config configuration))
  (defun reset-current-configuration ()
    (setf config nil))
  (defun get-current-configuration ()
    config))

(defmethod fcg-apply ((cxn construction) (cfs coupled-feature-structure)
                      (direction symbol) &key (configuration nil) cxn-inventory)

  ;; we could get rid of these closures if we would just set the
  ;; data to the blackboard of the cxn-inventory, then it would also be parallelisable
  (set-self cfs)
  (set-current-cxn cxn)
  (set-current-direction direction)
  (if configuration
    (set-current-configuration configuration)
    (reset-current-configuration))

  (let (match-cars failed-cars first-merge-cars second-merge-cars)
    ;; match phase
    (setq match-cars (match-cxn cxn cfs direction :cxn-inventory cxn-inventory))
    
    ;; for merging without matching
    (when (and (not match-cars) (attr-val cxn :merge-always))
      (setq match-cars (list (make-cxn-application-result
			      :status 'match-failed-by-try-to-merge
			      :source-cfs cfs
			      :applied-cxn cxn
			      :direction direction
			      :match-bindings +no-bindings+))))

    ;; first merge phase
    (dolist (car match-cars)
      (let ((merge-results (merge-poles (car-applied-cxn car)
                                        (car-source-cfs car)
                                        (car-direction car)
                                        (car-match-bindings car)
                                        nil
                                        (not (attr-val cxn :merge-always))
                                        configuration
                                        nil ;; not second-merge
                                        :cxn-inventory cxn-inventory))) 
	(cond ((not merge-results)
	       (setf (car-status car) 'first-merge-failed)
	       (push car failed-cars))
	      (t (setq 
                  first-merge-cars
                  (append 
                   first-merge-cars
                   (loop for merge-result in merge-results
                      if (and (attr-val cxn :merge-always)
                              (eq (car-status car) 'match-failed-by-try-to-merge)
                              (invalid-merge? direction car merge-result))
                      do (let ((failed-car (copy-object car)))
                           (setf (car-status failed-car) 'merge-without-match-failed)
                           (push failed-car failed-cars))
                      else 
                      append (loop for first-merge-bindings
                                in (mr-bsl merge-result)
                                for rcar = (copy-object car)
                                do (progn
                                     (setf (car-first-merge-bindings rcar)
                                           first-merge-bindings)
                                     (setf (car-first-merge-structure rcar)
                                           (mr-expr merge-result))
                                     (setf (car-first-merge-added rcar)
                                           (mr-added merge-result)))
                                collect rcar))))))))

    ;; second merge phase
    (dolist (car first-merge-cars)
      (let ((merge-results
             (merge-poles (car-applied-cxn car)
                          (car-source-cfs car)
                          (opposite direction)
                          (car-first-merge-bindings car)
                          (when (eq (left-pole-domain (car-applied-cxn car))
                                    (right-pole-domain (car-applied-cxn car)))
                            (car-first-merge-structure car))
                          nil
                          configuration
                          'second-merge ;; second-merge
                           :cxn-inventory cxn-inventory))) 

	(cond ((not merge-results)
	       (setf (car-status car) 'second-merge-failed)
	       (push car failed-cars))
	      (t (setq second-merge-cars
		       (append 
                        second-merge-cars
                        (loop for merge-result in merge-results append
                             (loop for second-merge-bindings
                                in (mr-bsl merge-result)
                                for rcar = (copy-object car)
                                do (progn
                                     (setf (car-status rcar) 
                                           (if (eq (car-status rcar) 
                                                   'match-failed-by-try-to-merge)
                                               'cxn-applied-without-match
                                               'cxn-applied))
                                     (setf (car-second-merge-bindings rcar)
                                           second-merge-bindings)
                                     (setf (car-second-merge-structure rcar)
                                           (mr-expr merge-result))
                                     (setf (car-second-merge-added rcar)
                                           (mr-added merge-result)))
                                collect rcar))))))))
    
    ;; make sure that any new bindings during the second merging phase are
    ;; also applied during the first merging phase
    (mapc #'(lambda (car)
              (setf (car-first-merge-structure car)
                    (substitute-bindings (car-second-merge-bindings car)
                                         (car-first-merge-structure car))))
          second-merge-cars)

    ;; compute the resulting cfs
    (mapc #'compute-resulting-cfs second-merge-cars)

    ;; check the boundaries
    (setf second-merge-cars
          (loop for car in second-merge-cars
             if (valid-boundaries? (right-pole-structure 
                                    (car-resulting-cfs car)))
             collect car
             else do (setf (car-status car) 'invalid-boundaries)
               (push car failed-cars)))

    ;; finally we check whether something changed
    (setq second-merge-cars
          (loop for car in second-merge-cars
             if (equivalent-coupled-feature-structures cfs (car-resulting-cfs car))
             do (setf (car-status car) 'structure-not-changed)
	       (push car failed-cars)
             else collect car))
    
    ;; done!
    (values second-merge-cars failed-cars)))

(define-event cxn-application-started (cxn construction) (direction symbol))
(define-event cxn-application-finished (resulting-cars list) (failed-cars list))

(defmethod fcg-apply :around ((cxn construction) (cfs coupled-feature-structure)
			      (direction symbol) &key (notify t))
  (when notify (notify cxn-application-started cxn direction))
  (multiple-value-bind (succeeded-cars failed-cars) (call-next-method)
    (when notify (notify cxn-application-finished succeeded-cars failed-cars))
    (values succeeded-cars failed-cars)))


