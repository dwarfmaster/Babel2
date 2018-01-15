
(in-package :fcg)

(defmethod cip-node-test ((node cip-node) (mode symbol))
  (error "Please implement cip-node-test for mode ~w" mode))

;; ---------------------------------------------------------
;; duplicate detection

;; TODO: The following three functions are unfinished and in
;; fact not working. It is part of an attempt to optimize.  

;; (defun remove-tag-bindings (bindings)
;;   (loop for binding in bindings
;;      when (symbolp (rest binding))
;;      collect binding))

;; (defun equivalent-bindings (node-1 node-2)
;;   (permutation-of? (remove-tag-bindings (car-match-bindings (cipn-car node-1)))
;;                    (remove-tag-bindings (car-match-bindings (cipn-car node-2)))
;;                    :key #'rest :test #'(lambda (a b) (unify-atom a b nil))))

;; (defun equivalent-node? (node-1 node-2)
;;   (and (permutation-of? (applied-constructions node-1)
;;                         (applied-constructions node-2) 
;;                         :key #'name)
;;        (loop 
;;           for up-node-1 in (upward-branch node-1 :include-initial t)
;;           for up-node-2 in (upward-branch node-2 :include-initial t)
;;           until (eq up-node-1 up-node-2)
;;           collect up-node-1 into up-1
;;           collect up-node-2 into up-2
;;           finally 
;;             (return (loop for node-in-1 in up-1
;;                        always (find node-in-1 up-2
;;                                     :test #'(lambda (node-1 node-2)
;;                                               (and (equal (name (car-applied-cxn (cipn-car node-1))) 
;;                                                           (name (car-applied-cxn (cipn-car node-2))))
;;                                                    (equivalent-bindings node-1 node-2)))))))))

(defun duplicate-nodes? (node other-node)
  (and (not (eq node other-node))
       (not (duplicate other-node))
       (permutation-of? (applied-constructions node)
                        (applied-constructions other-node)
                        :key #'name)
       ;; (equivalent-node? node other-node)
       (equivalent-coupled-feature-structures 
        (car-resulting-cfs (cipn-car node))
        (car-resulting-cfs (cipn-car other-node)))))

(defun find-duplicate (node other-node)
  (or (when (duplicate-nodes? node other-node) other-node)
      (loop for child in (children other-node)
            for duplicate = (find-duplicate node child)
            when duplicate do (return duplicate))))

(defmethod cip-node-test ((node cip-node) (mode (eql :check-duplicate)))
  "Checks whether the node is a duplicate of another one in the tree"
  (let ((duplicate (find-duplicate node (top-node (cip node)))))
    (if duplicate
      (progn
        (setf (duplicate node) duplicate)
        (push 'duplicate (statuses node))
        nil)
      t)))

;; ---------------------------------------------------------
;; search depth limit

(require-configuration :max-search-depth)

(defmethod cip-node-test ((node cip-node) (mode (eql :restrict-search-depth)))
  (if (< (length (all-parents node))
         (get-configuration (construction-inventory (cip node)) 'max-search-depth))
      t
      (and (push 'max-search-depth-reached (statuses node)) nil)))

;; ---------------------------------------------------------
;; limiting the total number of nodes

(require-configuration :max-nr-of-nodes)

(defmethod cip-node-test ((node cip-node) (mode (eql :restrict-nr-of-nodes)))
  (if (< (created-at node)
         (get-configuration (construction-inventory (cip node)) 'max-nr-of-nodes))
      t
      (and (push 'max-nr-of-nodes-reached (statuses node)) nil)))


;; ---------------------------------------------------------
;; 

(defmethod cip-node-test ((node cip-node) (mode (eql :no-morphs-if-unconnected-structure)))
  "Don't apply morph or tense cxn-sets, when structure unconnected"
    (if (and (equalp '-> (direction (cip node)))
           (or
            (string= 'morph (attr-val (car (applied-constructions node)) :label))
            (string= 'tense (attr-val (car (applied-constructions node)) :label))))
    (progn
      (let* ((cfs (car-resulting-cfs (cipn-car node)))
             (left-pole (left-pole-structure cfs))
             (subunitlist)
             (unitlist))
        (loop for unit in left-pole
              do
              (unless (equalp (car unit) 'root)
                (loop for subunit in unit
                      do
                      (if (not (atom subunit))
                        (if (eq (car subunit) 'subunits)
                          (push (cdr subunit) subunitlist))))
                (push (car unit) unitlist)))
        (if (= 1 (length (set-difference unitlist (flatten subunitlist))))
          t
          (progn
            (push 'unconnected-before-morphs (statuses node))
            nil))))
    t))


;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; +++                                           :UPDATE-REFERENCES                                         +++
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

;; This is not a real cip-node-test, but a function that updates the form references in the transient structure
;; if new units are built that have subunits. The references are only updated in comprehension.

;; Helper functions
;; ----------------
(defun get-outer-boundaries (list-of-boundary-numbers)
  "Given a list of lower-level boundaries, return the outer boundaries."
  (let ((inner-boundary (first (first list-of-boundary-numbers)))
        (outer-boundary (second (first list-of-boundary-numbers))))
    (dolist (bd (rest list-of-boundary-numbers) (list inner-boundary outer-boundary))
      (when (< (first bd) inner-boundary)
        (setf inner-boundary (first bd)))
      (when (> (second bd) outer-boundary)
        (setf outer-boundary (second bd))))))

(defun update-list-of-boundaries (old-boundaries node)
  "Use the hierarchy feature to calculate the new boundaries."
  (let* ((unit-structure (fcg-get-transient-unit-structure node))
         (hierarchy-feature (get-configuration (construction-inventory node) :update-boundaries-feature))
         (new-boundaries nil))
    ;; Defining some local helper functions.
    (labels ((get-outer-values-of-domain (unit-names)
               (let* ((subboundaries
                       (remove-if #'null (mapcar #'(lambda(unit-name)
                                                     (calculate-boundary (assoc unit-name unit-structure :test #'string= :key #'symbol-name)))
                                                 unit-names)))
                      (inner-bd (first (first subboundaries)))
                      (outer-bd (second (first subboundaries))))
                 (when subboundaries
                   (dolist (bd (rest subboundaries))
                     (when (< (first bd) inner-bd) (setf inner-bd (first bd)))
                     (when (> (second bd) outer-bd) (setf outer-bd (second bd))))
                   (list inner-bd outer-bd))))
             (calculate-boundary (unit)
               (let* ((name (first unit))
                      (new-entry (assoc name new-boundaries :test #'string= :key #'symbol-name)))
                 (if new-entry ;; If we already handled this unit...
                   (rest new-entry) ;; return the values we know
                   ;; If not, we check whether it has a "domain"
                   (let ((domain (unit-feature-value unit hierarchy-feature)))
                     (if (null domain) ;; if there is no domain then get the old boundary
                       (let ((old-bd (assoc name old-boundaries :test #'string= :key #'symbol-name)))
                         (setf new-boundaries (cons-if old-bd new-boundaries))
                         (rest old-bd))
                       ;; If there are, we get the outer values.
                       (let ((unit-boundaries (get-outer-values-of-domain domain))
                             (original-boundaries (rest (assoc name old-boundaries))))
                         (when unit-boundaries
                           (when original-boundaries
                             (setf unit-boundaries (list (if (< (first original-boundaries) (first unit-boundaries))
                                                           (first original-boundaries) (first unit-boundaries))
                                                         (if (> (second original-boundaries) (second unit-boundaries))
                                                           (second original-boundaries) (second unit-boundaries)))))
                           (push (cons name unit-boundaries) new-boundaries)
                           unit-boundaries))))))))
      (dolist (unit unit-structure)
        (calculate-boundary unit))
      ;; Now also add previous boundaries...
      (dolist (boundary old-boundaries)
        (unless (assoc (first boundary) new-boundaries :test #'string= :key #'symbol-name)
          (push boundary new-boundaries)))
      (sort new-boundaries #'< :key #'second))))

;;; Remi: old implementation of update-list-of-boundaries turned out to be too brittle because
;;;       it depends on the :added slot in a construction-application-result, which is not
;;;       reliable.
;;;   "Checks among units that were merged whether the boundaries need to be updated."
;;;   (let ((new-units (append (car-first-merge-added (cipn-car node))
;;;                            (car-second-merge-added (cipn-car node)))))
;;;     (dolist (unit new-units)
;;;       (let* ((unit-name (unit-name unit))
;;;              (existing-boundary (rest (assoc unit-name boundaries))) ;; E.g. (0 1)
;;;              (subunit-boundaries
;;;               (remove nil (loop for subunit in (unit-feature-value unit (get-hierarchy-feature))
;;;                                        collect (rest (assoc subunit boundaries)))))) ;; E.g.: ((0 1) (1 2))
;;;         (when subunit-boundaries
;;;           (let ((new-boundary `(,unit-name ,@(get-outer-boundaries (cons-if existing-boundary subunit-boundaries)))))
;;;             (setf boundaries (substitute-or-cons new-boundary boundaries))))))
;;;     boundaries))

;; Updating the references
;; -----------------------
(defmethod cip-node-test ((node cip-node) (mode (eql :update-references)))
  ;; Check if rendering and de-rendering methods are set correctly:
;;;   (unless (eq (get-configuration (construction-inventory node) :render-mode) :render-with-scope)
;;;     (warn "Please set your :render-mode to :render-with-scope when you use the :update-references node test. Your render mode is currently set to ~a" (get-configuration (construction-inventory node) :render-mode)))
;;;   (unless (eq (get-configuration (construction-inventory node) :de-render-mode) :de-render-with-scope)
;;;     (warn "Please set your :de-render-mode to :de-render-with-scope when you use the :update-references node test. Your render mode is currently set to ~a" (get-configuration (construction-inventory node) :de-render-mode)))
  ;;Add the boundaries feature type to feature types
  (unless (assoc 'boundaries (feature-types (original-cxn-set (construction-inventory node))))
    (setf (feature-types (original-cxn-set (construction-inventory node)))
          (cons '(boundaries sequence) (feature-types (original-cxn-set (construction-inventory node))))))
  
  ;; Only do the updating in comprehension. Probably it is not necessary in formulation.
  (if (eql '-> (fcg-get-direction node))
    t
    (let* ((form-predicates (get-updating-references node)) ;; By default, these are MEETS and PRECEDES.
           (transient-structure (set-self (fcg-get-transient-structure node))) ;; setting SELF to the resulting cfs
           (unit-structure (fcg-get-transient-unit-structure transient-structure))
           (root (get-root unit-structure))
           (remaining-form-constraints 
            (remove-if-member form-predicates (unit-feature-value root 'form) :key #'first :test #'string=))
           (structure-without-root (remove-root-unit unit-structure))
           (constraints-already-removed-from-root
            (fcg-extract-selected-form-constraints structure-without-root form-predicates))
           ;; 1/ Assumption: It suffices to only look at the merge-results to see whether there are new boundaries.
           (updated-boundaries (update-list-of-boundaries (fcg-get-boundaries unit-structure) node))
           ;; 2/ Infer the word order constraints.
           (word-order-constraints (set-difference
                                    ;; Get all the ordering constraints as if we were de-rendering.
                                    (infer-all-constraints-from-boundaries updated-boundaries form-predicates)
                                    ;; But remove the ones we already moved out of the root-unit.
                                    constraints-already-removed-from-root :test #'unify))

           ;; 3/ Update the ROOT unit.
           (new-root `(root
                       (boundaries ,updated-boundaries)
                       (form ,(append remaining-form-constraints word-order-constraints))
                       ,@(remove-if-member '(form boundaries) (unit-body root) :test #'string= :key #'first))))

      ;; Update the transient structure and return T.
      (setf (left-pole-structure transient-structure)
            (cons new-root structure-without-root))
      t)))



;; Updating the references
;; -----------------------
(defmethod cip-node-test ((node cip-node) (mode (eql :update-references-dependency-grammar)))
  ;;eigenlijk wil je een verschil maken tussen oorspronkelijke form constraints en nieuwe
  ;;de oorspronkelijke wil je houden en de rest inderdaad overschrijven
  (unless (eq (get-configuration (construction-inventory node) :render-mode) :render-with-scope)
    (warn "Please set your :render-mode to :render-with-scope when you use the :update-references node test. Your render mode is currently set to ~a" (get-configuration (construction-inventory node) :render-mode)))
  (unless (eq (get-configuration (construction-inventory node) :de-render-mode) :de-render-with-scope)
    (warn "Please set your :de-render-mode to :de-render-with-scope when you use the :update-references node test. Your render mode is currently set to ~a" (get-configuration (construction-inventory node) :de-render-mode)))
  ;;Add the boundaries feature type to feature types
  (unless (assoc 'boundaries (feature-types (original-cxn-set (construction-inventory node))))
    (setf (feature-types (original-cxn-set (construction-inventory node)))
          (cons '(boundaries sequence) (feature-types (original-cxn-set (construction-inventory node))))))
  
  ;; Only do the updating in comprehension. Probably it is not necessary in formulation.
  (if (eql '-> (fcg-get-direction node))
    t
    (let* ((form-predicates (get-updating-references node)) ;; By default, these are MEETS and PRECEDES.
           (transient-structure (set-self (fcg-get-transient-structure node))) ;; setting SELF to the resulting cfs
           (unit-structure (fcg-get-transient-unit-structure transient-structure))
           (root (get-root unit-structure))
           (init-root (get-root (fcg-get-transient-unit-structure (fcg-get-transient-structure (first (upward-branch node))))))
           (remaining-form-constraints ;(unit-feature-value root 'form) )
            (remove-if-member form-predicates (unit-feature-value root 'form) :key #'first :test #'string=))
           (original-form-constraints
            (unit-feature-value init-root 'form))
           (structure-without-root (remove-root-unit unit-structure))
           (constraints-already-removed-from-root
            (fcg-extract-selected-form-constraints structure-without-root form-predicates))
           ;; 1/ Assumption: It suffices to only look at the merge-results to see whether there are new boundaries.
           (updated-boundaries (update-list-of-boundaries (fcg-get-boundaries unit-structure) node))
           ;; 2/ Infer the word order constraints.
           (word-order-constraints (set-difference
                                    ;; Get all the ordering constraints as if we were de-rendering.
                                    (infer-all-constraints-from-boundaries updated-boundaries form-predicates)
                                    ;; But remove the ones we already moved out of the root-unit.
                                    constraints-already-removed-from-root :test #'unify))

           ;; 3/ Update the ROOT unit.
           (new-root `(root
                       (boundaries ,updated-boundaries)
                       (form ,(remove-duplicates (append remaining-form-constraints word-order-constraints
                                                         original-form-constraints)
                                                 :test #'(lambda (x y)
                                                           (and (eq (first x) (first y))
                                                                (eq (second x) (second y))
                                                                (eq (third x) (third y))))))
                       ,@(remove-if-member '(form boundaries) (unit-body root) :test #'string= :key #'first))))
    ;  (pp new-root)
      ;; Update the transient structure and return T.
      (setf (left-pole-structure transient-structure)
            (cons new-root structure-without-root))
      t)))

;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; +++                                           :UPDATE-BOUNDARIES                                         +++
;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defmethod cip-node-test ((node cip-node) (mode (eql :update-boundaries)))
  
  ;; Only do the updating in comprehension. Probably it is not necessary in formulation.
  (if (eql '-> (fcg-get-direction node))
    t
    (let* ((transient-structure (set-self (fcg-get-transient-structure node))) ;; setting SELF to the resulting cfs
           (unit-structure (fcg-get-transient-unit-structure transient-structure))
           (root (get-root unit-structure))
           (structure-without-root (remove-root-unit unit-structure))
           ;; 1/ Assumption: It suffices to only look at the merge-results to see whether there are new boundaries.
           (updated-boundaries (update-list-of-boundaries (fcg-get-boundaries unit-structure) node))

           ;; 2/ Update the ROOT unit.
           (new-root `(root
                       (boundaries ,updated-boundaries)
                      ; (form ,(unit-feature-value root 'form))
                       ,@(remove-if-member '( boundaries) (unit-body root) :test #'string= :key #'first))))
      ;; Update the transient structure and return T.
      (setf (left-pole-structure transient-structure)
            (cons new-root structure-without-root))
      t)))

;;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
