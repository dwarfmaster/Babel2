
(in-package :fcg)


;; #############################################################################
;; construction-inventory-processor, cip-node
;; -----------------------------------------------------------------------------

(export '(construction-inventory-processor 
          construction-inventory direction initial-cfs node-counter
          succeeded-nodes create-construction-inventory-processor
          fcg-apply-with-n-solutions fcg-apply-exhaustively upward-branch cip-enqueue))

(defclass construction-inventory-processor ()
  ((construction-inventory 
    :type construction-inventory :initarg :construction-inventory 
    :accessor construction-inventory
    :initform (error "please provide a :construction-inventory")
    :documentation "The construction inventory to apply")
   (direction
    :type symbol :initarg :direction :accessor direction
    :initform (error "please provide a :direction")
    :documentation "the directino, '-> or '<-")
   (initial-cfs 
    :type coupled-feature-structure :initarg :initial-cfs :accessor initial-cfs
    :initform (error "please provide an :initial-cfs"))
   (top-node 
    :type (or null cip-node) :initform nil :accessor top-node
    :documentation "The top node of the search process")
   (cxn-supplier-gen
     :type t :accessor cxn-supplier-gen
     :initform nil
     :documentation "Generic data for the cxn supplier"
     )
   (queue 
    :type list :initform nil :accessor queue
    :documentation "All nodes to be processed sorted by priority")
   (node-counter
    :type number :initform 0 :accessor node-counter
    :documentation "A counter for the number of nodes in the tree")
   (succeeded-nodes 
    :type list :initform nil :accessor succeeded-nodes
    :documentation "All succeeded nodes of the search process"))
  (:documentation "The state of a FCG search process for applying
                   construction inventories"))

(defmethod initialize-instance :after ((cip construction-inventory-processor) &key)
  (let* ((fcg-2 (not (original-cxn-set (construction-inventory cip)))) ;; if there's no original-cxn-set, it's old FCG
        (top-node
         (make-instance 'cip-node
                        :construction-inventory (construction-inventory cip)
                        :statuses '(initial)
                        :car (make-cxn-application-result 
                              :source-cfs (initial-cfs cip)
                              :direction (direction cip)
                              :resulting-cfs (initial-cfs cip))
                        :cip cip :created-at 0
                        :diagnostics (unless fcg-2 (diagnostics (original-cxn-set (construction-inventory cip))))
                        :repairs (unless fcg-2 (repairs (original-cxn-set (construction-inventory cip)))))))
    (setf (top-node cip) top-node)
    (setf (queue cip) (list top-node)))
  (setf (cxn-supplier-gen cip)
        (create-gen-cxn-supplier (construction-inventory cip) (get-configuration cip :cxn-supplier-mode)))
  )

(defgeneric create-construction-inventory-processor
    (construction-inventory mode &key cfs direction &allow-other-keys)
  (:documentation "Creates a construction-inventory-processor for the
  given construction-inventory"))

(defmethod create-construction-inventory-processor
           ((construction-inventory construction-inventory)
            (mode t)
            &key initial-cfs direction &allow-other-keys)
   (make-instance 'construction-inventory-processor
                 :construction-inventory construction-inventory
                 :direction direction
                 :initial-cfs initial-cfs))

(defmethod copy-object ((cip construction-inventory-processor))
  cip)

(defmethod get-configuration ((cip construction-inventory-processor) key &key)
  (get-configuration (construction-inventory cip) key))

(export '(cip-node statuses cxn-supplier cipn-car cxn-applied
          applied-constructions priority goal-test-data all-parents
          children cip fully-expanded?  duplicate created-at
          traverse-depth-first siblings))

(defclass cip-node (object-w-learning)
  ((construction-inventory 
    :type construction-inventory :initarg :construction-inventory 
    :accessor construction-inventory
    :initform (error "please provide a :construction-inventory")
    :documentation "The construction inventory to apply")
   (cxn-applied
    :type symbol :initform t :accessor cxn-applied :initarg :cxn-applied
    :documentation "t -- when node is created from a successful construction
                    application (fcg-apply call), nil -- otherwise")
   (statuses 
    :type list :initarg :statuses :initform nil :accessor statuses
    :documentation "A list of all the statuses the node has been in")
   (cxn-supplier
    :type t :initform nil :accessor cxn-supplier
    :documentation "An object that can be asked to return the next 
                    construction to be applied via cip-next-cxn")
   (car 
    :type cxn-application-result :initarg :car :accessor cipn-car
    :documentation "The result of construction application")
   (applied-constructions 
    :type list :initarg :applied-constructions :initform nil 
    :accessor applied-constructions
    :documentation "All constructions that have been applied so far")
   (priority
    :type number :initarg :priority :initform 0.0 :accessor priority
    :documentation "The higher, the more in front in the queue")
   (goal-test-data 
    :type blackboard :accessor goal-test-data :initform (make-blackboard)
    :documentation "Goal tests can store any information here")
   (all-parents 
    :type list :initarg :all-parents :initform nil :accessor all-parents
    :documentation "The consisting of the parent node, the parent of
                    the parent, and so on.")
   (children 
    :type list :initarg :children :initform nil :accessor children
    :documentation "All children of the node")
   (cip 
    :type construction-inventory-processor :initarg :cip :accessor cip
    :documentation "A pointer to the application process")
   (fully-expanded?
    :type t :initarg :fully-expanded? :initform nil :accessor fully-expanded?
    :documentation "Whether a node has been fully expanded.")
   (duplicate 
    :type (or null cip-node) :initform nil 
    :accessor duplicate 
    :documentation "The node that this node is a duplicate of")
   (created-at 
    :type number :initarg :created-at :accessor created-at
    :documentation "the number of previous search tree expansions when
                    this node was created. only used for debugging"))
  (:documentation "Represents a node in the search tree"))

(defmethod parent ((node cip-node))
  (first (all-parents node)))

(defun upward-branch (cipn &key (include-initial t))
  "Returns the given cipn and all its parents"
  (cons cipn (if include-initial
                 (all-parents cipn)
                 (butlast (all-parents cipn)))))

(defgeneric siblings (cip-node)
  (:documentation "Returns all siblings of the give node. Does not include itself."))

(defmethod siblings ((node cip-node))
  (remove node (children (parent node))))

(defmethod get-configuration ((cip cip-node) key &key)
  (get-configuration (cip cip) key))
   
(defmethod print-object ((cipn cip-node) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
        (format stream "<cipn ~a, ~a, ~a,~:_ statuses: ~(~a~),~:_
        applied: ~(~w~),~:_ car: ~:w>"
                (created-at cipn) (priority cipn) (fully-expanded? cipn) 
                (statuses cipn) (mapcar #'name (applied-constructions cipn))
                (cipn-car cipn)))
      (format stream "<cipn ~a, ~a, ~a: ~{~(~a~)~^, ~}>" 
              (created-at cipn) (priority cipn) (fully-expanded? cipn) 
              (statuses cipn))))

(defmethod copy-object ((cipn cip-node))
  cipn)

(defgeneric traverse-depth-first (tree-or-node &key collect-fn do-fn &allow-other-keys)
  (:documentation "Traverses the given tree (or starting form a given
  node) top down in a depth first fashion. The user has to supply
  either a collect-fn or a do-fn which will be called on each
  individual node."))

(defmethod traverse-depth-first :before ((node construction-inventory-processor) 
					 &key (collect-fn nil) (do-fn nil) &allow-other-keys)
  (unless (or do-fn collect-fn)
    (error "You need to supply a collect-fn or a do-fn")))

(defmethod traverse-depth-first ((node cip-node) &key (collect-fn nil) (do-fn nil) &allow-other-keys)
  (if do-fn
      (progn
	(funcall do-fn node)
	(loop for child in (children node)
	   do (traverse-depth-first child :collect-fn collect-fn :do-fn do-fn)))
      ;; else there is a collect-fn
      (cons (funcall collect-fn node)
	    (loop for child in (children node)
	       append (traverse-depth-first child :collect-fn collect-fn :do-fn do-fn)))))

(defmethod traverse-depth-first ((tree construction-inventory-processor) 
				 &key (collect-fn nil) (do-fn nil) &allow-other-keys)
  (traverse-depth-first (top-node tree) :collect-fn collect-fn :do-fn do-fn))


;; #############################################################################
;; hooks for search customization
;; -----------------------------------------------------------------------------

(export '(create-cxn-supplier next-cxn cip-node-test cip-goal-test cip-priority))

(defgeneric create-gen-cxn-supplier (inventory mode)
  (:documentation "Creates and return a cxn-supplier helper for a new construction inventory"))

(defgeneric create-cxn-supplier (node mode gen-supplier)
  (:documentation "Creates and returns a cxn pool for a new node"))

(defgeneric next-cxn (cxn-supplier node)
  (:documentation "Returns the next construction to try from a pool"))


(require-configuration :node-tests)

(defgeneric cip-node-test (node mode)
  (:documentation "Tests whether a node should be further explored"))


(require-configuration :parse-goal-tests)
(require-configuration :production-goal-tests)

(defgeneric cip-goal-test (node mode)
  (:documentation "Tests whether a cip node is a solution"))


(require-configuration :queue-mode)

(defgeneric cip-enqueue (node cip mode)
  (:documentation "Puts a node into the queue"))

(require-configuration :priority-mode)

(defgeneric cip-priority (node mode)
  (:documentation "Computes a number for the priority of a node in the queue"))

;; #############################################################################
;; next-cip-solution
;; -----------------------------------------------------------------------------

(export '(next-cip-solution solution
          cip-started cip-next-node cip-node-expanded cip-finished))

(defgeneric cip-add-child (cip-node cxn-application-result &key)
  (:documentation "Creates and adds a new child based on the given node
  and car to the given node."))

(defmethod cip-add-child ((node cip-node) (car cxn-application-result)
                          &key (cxn-applied t))
  (let ((child 
         (make-instance 'cip-node
                        :cxn-applied cxn-applied
                        :construction-inventory (construction-inventory node)
                        :statuses (list (car-status car))
                        :car car 
                        :all-parents (cons node (all-parents node))
                        :applied-constructions (cons (car-applied-cxn car)
                                                     (applied-constructions node))
                        :cip (cip node)
                        :created-at (incf (node-counter (cip node)))
                        :diagnostics (diagnostics node)
                        :repairs (repairs node)
                        :problems (problems node))))
    (push child (children node))
    child))

;; If call-next-method returns nil, then set fully-expanded? to true,
;; as we don't want to continue this path + return nil
(defmethod cip-node-test :around ((node cip-node) (mode t))
  (not (setf (fully-expanded? node) (not (call-next-method)))))

(defmethod cip-priority ((node cip-node) (mode (eql :depth-first)))
  (length (all-parents node)))



;; -------------------------------:depth-first-prefer-local-bindings------------------------------------------
;; -----------------:best-first-minimize-domains-and-maximize-semantic-coherence------------------------------
;; Experimental cip-priorities, not thoroughly tested yet.
;; TODO: write tests.
;;
;; 15/03/2016: The new version of this cip-priority method looks at all units involved in matching, and not to
;;             particular constraints anymore. It is therefore compatible with any kind of grammar (both constituent-,
;;             dependency-, or a mixed grammr.
;; 30/06/2016: Making parts of the node test standalone functions so they can be shared.
;; 01/07/2016: Including priority based on semantic coherence. Very rough approach.

;; Helper functions:
(defun fcg-get-processing-strategies (x)
  "Get the processing strategies of the inventory."
  (get-configuration x :processing-strategies))

(defun fcg-get-processing-strategy-weight (x strategy-name)
  "Get the weight of a particular strategy."
  (let ((strategies (if (eql (type-of x) 'cons)
                      x
                      (fcg-get-processing-strategies x))))
    (second (assoc strategy-name strategies))))

(defun compute-base-priority (cip-node)
  "Compute the base priority, which is the score of the parent or 0.0 for the initial node."
  (let ((parent (parent cip-node)))
    (if parent (1+ (priority parent)) 0.0)))

;; 1/ Minimize domains. The name of this strategy comes from Rijkhoff (1992), and is related to
;;                      Ted Gibson's Dependency Locality Theory and Hawkins (2004). It makes variable
;;                      equalities more expensive if they are bound to unit names that are located at
;;                      greater distance than hypotheses that are close to each other.


(defun compute-dependency-locality-cost-through-boundaries (bindings unit-structure)
  "Computes a cost for new bindings based on their distance in the utterance."
  ;; 18/10: Reintroduced the original function (Remi)
  ;;        Using sequence breaks phrasal grammars.
  ;;        Question: I don't understand why sequence is necessary for the dependency grammars...
  ;;                  Is it because only the original boundaries are allowed to matter?
  ;; Remi 26/10: small update to give slight edge for larger constructions
  (if (null bindings) 0.0
    (let ((inner-bd nil) (outer-bd nil) (sentence-bd 1.0) ;; bd = boundaries
          ;; Look into the bindings to find which units were matched. Collect their names.
          (units-involved (loop for binding in bindings
                                when (assoc (rest binding) unit-structure)
                                collect (rest binding))))
      (if units-involved
        ;; Now check the boundaries for each matched unit, and find the inner and outer boundaries.
        ;; At the same time, we also find the sentence boundary.
        (let* ((boundaries (fcg-get-boundaries unit-structure))
               (inner-scope-bd nil))
          (dolist (boundary boundaries)
            (let ((unit-name (first boundary))
                  (inner-sc-bd (second boundary))
                  (bd (third boundary)))
              ;; Calculate the locality cost
              (when (> bd sentence-bd)
                (setf sentence-bd bd))
              (when (find unit-name units-involved)
                (when (or (null inner-scope-bd) (< inner-sc-bd inner-scope-bd))
                  (setf inner-scope-bd inner-sc-bd))
                (when (or (null inner-bd) (> inner-bd bd))
                  (setf inner-bd bd))
                (when (or (null outer-bd) (< outer-bd bd))
                  (setf outer-bd bd)))))
          ;; Now we calculate the cost.
          (- (/ (- outer-bd inner-bd) sentence-bd) (if inner-scope-bd (* 0.01 (- outer-bd inner-scope-bd)) 0.0)))
        0.0))))

(defun compute-dependency-locality-cost (bindings unit-structure)
  "Computes a cost for new bindings based on their distance in the utterance."
  (if (null bindings) 0.0
    (let ((inner-bd nil) (outer-bd nil) (sentence-bd 1.0) ;; bd = boundaries
          (sequence (fcg-get-sequence unit-structure))
          ;; Look into the bindings to find which units were matched. Collect their names.
          (units-involved (loop for binding in bindings
                                when (assoc (rest binding) unit-structure)
                                collect (rest binding))))
      (if units-involved
        ;; Now check the boundaries for each matched unit, and find
        ;; the inner and outer boundaries.  At the same time, we also
        ;; find the sentence boundary.  Update by Katrien (4/10/16):
        ;; we use the sequence feature and not the boundaries (to
        ;; allow the use of dependency grammars)
        (progn (dolist (unit-name sequence)
                 (let ((bd (+ (position unit-name sequence) 1)))
                   (when (> bd sentence-bd)
                     (setf sentence-bd bd))
                   (when (find unit-name units-involved)
                     (when (or (null inner-bd) (> inner-bd bd))
                       (setf inner-bd bd))
                     (when (or (null outer-bd) (< outer-bd bd))
                       (setf outer-bd bd)))))
          ;; Now we calculate the cost. 
          (/ (- outer-bd inner-bd) sentence-bd))
        0.0))))
    
(defmethod cip-priority ((node cip-node) (mode (eql :depth-first-prefer-local-bindings)))
  "returns number based on how far precedes constraints are apart (highest first)"
  (let ((base-priority (compute-base-priority node)))
    ;; In production we do not take locality into account.
    (if (or (eq (direction (cip node)) '->) (null (parent node)))
      base-priority
      (let* ((match-bindings (car-match-bindings (cipn-car node)))
             (unit-structure (fcg-get-transient-unit-structure node))
             (dependence-locality-cost (compute-dependency-locality-cost match-bindings unit-structure)))
        ;; Return the priority.
        (+ base-priority (- 1.0 dependence-locality-cost))))))

;; 2/ Maximize semantic coherence: Prefer networks in which meaning variables are connected to other variables.
(defun count-unique-elements (list &optional (count 0))
  "Count how many elements occur only once in a list."
  (if (null list)
    count
    (let ((duplicate-p (member (first list) (rest list))))
      (count-unique-elements (remove (first list) list) (if duplicate-p count (incf count))))))

(defun compute-semantic-coherence-cost (transient-structure-or-cip-node)
  "A crude measure for coherence based on variable equalities without assumptions about the meaning space."
  (let* ((total-nr-of-variables 0.0)
         (meaning-variables (loop for meaning in (fcg-extract-meanings transient-structure-or-cip-node)
                                  for vars = (rest meaning)
                                  do (setf total-nr-of-variables (+ total-nr-of-variables (length vars)))
                                  append vars))
         (nr-of-unique-variables (count-unique-elements meaning-variables)))
    (if meaning-variables
      (/ nr-of-unique-variables total-nr-of-variables)
      1.0)))

(defmethod cip-priority ((node cip-node) (mode (eql :best-first-minimize-domains-and-maximize-semantic-coherence)))
  ;; This cip-priority mode ranks hypotheses based on the principles of "Minimize Domains" (Rijkhoff, 1992; also
  ;; see Hawkins, 2004 and Ted Gibson's Dependency Locality Theory) and on semantic coherence. Other potential
  ;; preferences may include iconicity, etc., but are currently not considered.
  (let ((base-priority (compute-base-priority node)))
    ;; In production, we do not take a special priority mode into account and we revert to depth-first.
    (if (or (eq (direction (cip node)) '->) (null (parent node)))
      base-priority
      (let* (;; Preliminaries
             (match-bindings (car-match-bindings (cipn-car node)))
             (unit-structure (fcg-get-transient-unit-structure node))
             ;; Calculate the dependency-locality-cost
             (weight-minimize-domains (or (fcg-get-processing-strategy-weight node :minimize-domains) 1.0))
             (dependency-locality-cost (compute-dependency-locality-cost match-bindings unit-structure))
             ;; Calculate the semantic coherence cost
             (weight-maximize-coherence (or (fcg-get-processing-strategy-weight node :maximize-coherence) 1.0))
             (semantic-coherence-cost (compute-semantic-coherence-cost unit-structure))
             ;; Total weight.
             (total-weight (+ weight-minimize-domains weight-maximize-coherence)))
        (+ base-priority (- total-weight
                            (* weight-minimize-domains dependency-locality-cost)
                            (* weight-maximize-coherence semantic-coherence-cost)))))))

(defmethod cip-priority ((node cip-node) (mode (eql :best-first-minimize-boundary-distance-and-maximize-semantic-coherence)))
  ;; This cip-priority mode ranks hypotheses based on the principles of "Minimize Domains" (Rijkhoff, 1992; also
  ;; see Hawkins, 2004 and Ted Gibson's Dependency Locality Theory) and on semantic coherence. Other potential
  ;; preferences may include iconicity, etc., but are currently not considered.
  (let ((base-priority (compute-base-priority node)))
    ;; In production, we do not take a special priority mode into account and we revert to depth-first.
    (if (or (eq (direction (cip node)) '->) (null (parent node)))
      base-priority
      (let* (;; Preliminaries
             (match-bindings (car-match-bindings (cipn-car node)))
             (unit-structure (fcg-get-transient-unit-structure node))
             ;; Calculate the dependency-locality-cost
             (weight-minimize-domains (or (fcg-get-processing-strategy-weight node :minimize-domains) 1.0))
             (dependency-locality-cost (compute-dependency-locality-cost-through-boundaries match-bindings unit-structure))
             ;; Calculate the semantic coherence cost
             (weight-maximize-coherence (or (fcg-get-processing-strategy-weight node :maximize-coherence) 1.0))
             (semantic-coherence-cost (compute-semantic-coherence-cost unit-structure))
             ;; Total weight.
             (total-weight (+ weight-minimize-domains weight-maximize-coherence)))
        (+ base-priority (- total-weight
                            (* weight-minimize-domains dependency-locality-cost)
                            (* weight-maximize-coherence semantic-coherence-cost)))))))

;; ------------------------------------------------------------------------------------------------------------

(defmethod cip-enqueue ((node cip-node) (cip construction-inventory-processor)
                        (mode (eql :by-priority)))
  (setf (priority node)  
        (cip-priority node (get-configuration cip :priority-mode)))
  (unless (priority node)
    (error "The priority of the new node is NIL. You used priority
mode ~a. Please check why it did not calculate a priority score." (get-configuration cip :priority-mode)))
  (setf (queue cip) (sorted-insert (queue cip) node :key #'priority :test #'>)))

(defun last-applied-construction (node)
  (first (applied-constructions node)))

(defun cxn-in-path-of-sibling-p (node)
  (when (and (parent node)
             (siblings node))
    (let ((cxns-in-sibling-paths
           (loop for sibling in (siblings node)
                 for sibling-cxn = (last-applied-construction sibling)
                 append (remove sibling-cxn
                                (remove nil
                                        (traverse-depth-first sibling :collect-fn #'(lambda (node)
                                                                              (when (fully-expanded? node)
                                                                                (last-applied-construction node)))))
                                :key #'identity :test #'equalp))))
      (find (name (last-applied-construction node)) cxns-in-sibling-paths :key #'name :test #'equalp))))
    
(defmethod cip-enqueue ((node cip-node) (cip construction-inventory-processor)
                        (mode (eql :depth-first-avoid-duplicates)))
  (setf (priority node)  
        (cip-priority node (get-configuration cip :priority-mode)))
  (unless (priority node)
    (error "The priority of the new node is NIL. You used priority
mode ~a. Please check why it did not calculate a priority score." (get-configuration cip :priority-mode)))
  (if (cxn-in-path-of-sibling-p node)
    (queue cip)
    (setf (queue cip) (sorted-insert (queue cip) node :key #'priority :test #'>))))

(defmethod cip-enqueue ((node cip-node) (cip construction-inventory-processor)
                        (mode (eql :by-my-priority)))
  (setf (priority node)  
        (cip-priority node (get-configuration cip :priority-mode)))
  (setf (queue cip) (sorted-insert (queue cip) node :key #'priority :test #'>=)))

(defmethod cip-enqueue ((node cip-node) (cip construction-inventory-processor)
                        (mode (eql :breadth-first)))
  "Implements breadth-first search" 
  (setf (priority node)  
        (cip-priority node (get-configuration cip :priority-mode)))
  (setf (queue cip) (sorted-insert (queue cip) node :key #'priority :test #'<=)))

(defun cip-run-goal-tests (node cip)
  (setf (goal-test-data node) (make-blackboard))
  (when (loop for mode in (get-configuration
                           cip
                           (if (eq (direction cip) '<-) 
                             :parse-goal-tests
                             :production-goal-tests))
           always (cip-goal-test node mode))
    (push 'succeeded (statuses node))
    (push node (succeeded-nodes cip))
    t))


(define-event cip-started (cip construction-inventory-processor))
(define-event cip-next-node (cipn cip-node))
(define-event cip-node-expanded (cipn cip-node))
(define-event cip-finished (solution t) (cip construction-inventory-processor))
(define-event cip-restart-requested (cipn cip-node))

(defgeneric expand-cip-node (node mode)
  (:documentation "Returns children to be queued."))

(defun apply-sequentially? (node cxn)
  (let ((sequential-labels 
	 (get-configuration (cip node) 
			    :cxn-sets-with-sequential-application))
	(cxn-label (attr-val cxn :label)))
    (or (attr-val cxn :apply-sequentially)
	(if (consp cxn-label)
	    (intersection cxn-label sequential-labels)
	    (find cxn-label sequential-labels)))))
	
(defmethod expand-cip-node ((node cip-node) (mode (eql :default)))
  (loop 
   with nodes-to-queue = nil
   with failed-nodes = nil
   with cxn-inventory = (construction-inventory node)
   for cxn = (next-cxn (cxn-supplier node) node)
   when cxn
   do 
   (multiple-value-bind (succeeded-cars failed-cars)
       (fcg-apply (safe-cxn cxn (applied-constructions node))
                  (car-resulting-cfs (cipn-car node))
                  (direction (cip node))
                  :notify nil
                  :configuration (configuration cxn-inventory)
                  :cxn-inventory cxn-inventory)
     (loop for car in succeeded-cars
           do (push (cip-add-child node car)
                    nodes-to-queue)
           when (apply-sequentially? node cxn)
           do (setf (fully-expanded? node) t) (return))

     (loop for car in failed-cars
           do (push (cip-add-child node car :cxn-applied nil)
                    failed-nodes)))
   when nodes-to-queue do (return nodes-to-queue)
   while cxn
   finally (setf (fully-expanded? node) t)))

(require-configuration :node-expansion-mode)

(defun get-cip-leaves (cip)
  "Helper function: get all leaves (final nodes) from the cip search
tree."
  (remove nil
          (traverse-depth-first cip
                                :collect-fn #'(lambda (node) 
                                                (when (fully-expanded? node) node)))))

(defun get-last-cip-node (cip) 
  "Helper function: extract last node that is consulted from a
construction inventory processor (cip). Useful when there is no
solution."
  (let ((last-node
	 (loop for node in (get-cip-leaves cip)
	    when (find 'succeeded (statuses node))
	    return node)))
    (if last-node
	last-node
	(first (last (get-cip-leaves cip))))))

(defun next-cip-solution (cip &key (notify t))
  "runs the construction inventory application search process until
   the next solution is found"
  (when notify (notify cip-started cip))
  (loop 
   with solution = nil
   with queue-mode = (get-configuration cip :queue-mode)
   for node = (pop (queue cip))
   when node
   do (unless (cxn-supplier node) ;; node handled the first time
        (setf (cxn-supplier node) 
              (create-cxn-supplier
               node (get-configuration cip :cxn-supplier-mode) (cxn-supplier-gen (cip node)))))
   (when notify (notify cip-next-node node))
       
   (loop for child in (expand-cip-node  ;; make children
                                        node (get-configuration cip :node-expansion-mode))
         ;; node tests
         when (loop for mode in (get-configuration cip :node-tests)
                    always (cip-node-test child mode))
         do (cip-enqueue child cip queue-mode))

   ;; For meta-layer
   (when (and (get-configuration cip :use-meta-layer)
              (diagnostics node))
     (multiple-value-bind (new-problems new-fixes)
         (notify-learning node :trigger 'new-node)
       (when new-problems
         (loop for problem in new-problems
               do (push (type-of problem) (statuses node)))
         (push 'diagnostic-triggered (statuses node)))
       ;; Loop through the new-fixes (they should have a list of construction-application-results in
       ;; their data-field 'fixed-cars), make nodes of them, add them as children, and enqueue them 
       (loop for fix in new-fixes ;;
             for fixed-cars = (get-data fix 'fixed-cars)
             do (loop for fixed-car in fixed-cars
                      do
                      (let ((fixed-child (cip-add-child node fixed-car)))
                        (push (type-of (issued-by fix)) (statuses fixed-child))
                        (push 'added-by-repair (statuses fixed-child))
                        (cip-enqueue fixed-child cip queue-mode))))))
   
   ;; goal tests
   (let ((goal-test-succeeded? (cip-run-goal-tests node cip)))
     (when goal-test-succeeded?
       (setf solution node) ;; node is a solution!
       (when (and (get-configuration cip :use-meta-layer)
                  (get-configuration cip :consolidate-repairs)
                  (repairs node))
           (consolidate-repair-cxns node))) ;; consolidate repairs!
       
     (unless (or (fully-expanded? node) ;;there are other children in the making
                 goal-test-succeeded?) ;;and the node did NOT pass the goal test
       (cip-enqueue node cip queue-mode))) ;;requeue it so the next children can be explored

   (when notify (notify cip-node-expanded node))
   until (or solution
             (not (queue cip)))
   finally
   (unless (or solution
               (succeeded-nodes cip))
     (setf solution (get-last-cip-node cip))
     (push 'goal-test-failed (statuses solution)))
   (when notify (notify cip-finished solution cip))
   (return (values solution cip))))

;; #############################################################################
;; fcg-apply
;; -----------------------------------------------------------------------------

(defmethod fcg-apply ((construction-inventory construction-inventory)
                      (cfs coupled-feature-structure) (direction symbol)
		      &key (notify t))
  (next-cip-solution (create-construction-inventory-processor
		      construction-inventory 
		      (get-configuration
                       construction-inventory
                       'construction-inventory-processor-mode)
		      :initial-cfs cfs :direction direction) :notify notify))

;; You have to pass through fcg-apply specialised on
;; construction-inventory for expansion (==>) to work. If you do not
;; then you have to set the expansion data manually with
;; set-fcg-expanion-data.
(defmethod fcg-apply :before ((construction-inventory construction-inventory) 
                              (cfs coupled-feature-structure) (direction symbol)
                              &key (notify t))
  (declare (ignorable notify cfs direction))
  (set-fcg-expansion-data (expansion-data construction-inventory)))

(define-event fcg-apply-w-n-solutions-started (n t)
  (construction-inventory construction-inventory)
  (direction t))

(define-event fcg-apply-w-n-solutions-finished (solutions t) (cip construction-inventory-processor))

(defun fcg-apply-with-n-solutions (construction-inventory cfs direction n &key (notify t))
  "returns the first n solutions of a construction inventory application"
  (assert (or (not n) (> n 0)))
  (when notify (notify fcg-apply-w-n-solutions-started n construction-inventory direction))
  (loop 
     with solutions = nil
     with cip = nil
     initially (multiple-value-bind (solution new-cip)
                   (fcg-apply construction-inventory cfs direction 
                              :notify nil)
                 (when solution (push solution solutions))
                 (setf cip new-cip))
     for i from 2 to (or n 32000) ;; from 2 because we already did one in the initially
     for (solution new-cip) = (multiple-value-list (next-cip-solution cip :notify nil))
     do (setf cip new-cip) ; potentially a new cip if there was a restart
     while solution do (setf solutions (append solutions (list solution)))
     finally
       (when notify (notify fcg-apply-w-n-solutions-finished solutions cip))
     (return (values solutions cip))))

(defun fcg-apply-exhaustively (construction-inventory cfs direction &key (notify t))
  "returns all solutions of a construction inventory application"
  (fcg-apply-with-n-solutions construction-inventory cfs direction nil :notify notify))

;; #############################################################################
;; produce
;; -----------------------------------------------------------------------------
#|(let ((hierarchy-feature 'subunits)) ;;old clojure
    (defun set-hierarchy-feature (feature-name)
      (setf hierarchy-feature feature-name))
    (defun get-hierarchy-feature ()
      hierarchy-feature))
|#

(defun initialize-transient-structure-blackboard (transient-structure
                                                  utterance/meaning construction-inventory direction)
  "Initializes the data fields in the transient structure according to
the preprocessing tools set in the configuration of the construction
inventory. Typically, POS tags, named entities and noun chunks are
added here. Preprocessing is only used in parsing currently."
  (if (eq direction '<-)
    (progn
      (unless (field? transient-structure :utterance)
        (set-data transient-structure :utterance (listify utterance/meaning)))
      (loop for (field preprocessing-function) in (get-configuration construction-inventory :preprocessing-tools)
            do (set-data transient-structure field (funcall (eval preprocessing-function) transient-structure))))
    (unless (field? transient-structure :meaning)
      (set-data transient-structure :meaning utterance/meaning)))
  )

(defmethod produce ((meaning list) (construction-inventory construction-inventory)
                    &optional silent)
  "Default produce method for a construction-inventory."
  (let ((initial-cfs (create-initial-structure 
		      meaning 
		      (get-configuration construction-inventory :create-initial-structure-mode))))
    (initialize-transient-structure-blackboard initial-cfs meaning construction-inventory '->)
    (unless silent (notify produce-started meaning construction-inventory initial-cfs))
    (multiple-value-bind (solution cip)
        (fcg-apply construction-inventory initial-cfs '->)
      (let ((utterance
             (and solution
                  (or (find-data (goal-test-data solution) 'utterance)
                      (render 
                       (car-resulting-cfs (cipn-car solution)) 
                       (get-configuration construction-inventory :render-mode)
                      :node solution)))))
        (unless silent (notify produce-finished utterance))
        (values utterance solution cip)))))

(defmethod produce-all ((meaning list) (construction-inventory construction-inventory)
                        &key silent n)
  ;(set-hierarchy-feature (first (hierarchy-features construction-inventory)))
  (let ((initial-cfs (create-initial-structure 
		      meaning 
		      (get-configuration construction-inventory :create-initial-structure-mode))))
    (unless silent (notify produce-all-started n meaning construction-inventory))
    (multiple-value-bind (solutions cip)
        (if n
          (fcg-apply-with-n-solutions construction-inventory initial-cfs '-> n
                                      :notify (not silent))
          (fcg-apply-exhaustively construction-inventory initial-cfs '->
                                  :notify (not silent)))
      (let ((utterances
             (mapcar #'(lambda(solution)
                         (or (find-data (goal-test-data solution) 'utterance)
                             (render 
                              (car-resulting-cfs (cipn-car solution)) 
                              (get-configuration construction-inventory :render-mode)
                              :node solution)))
                     solutions)))
        (unless silent (notify produce-all-finished utterances))
        (values utterances solutions cip)))))

;; #############################################################################
;; parse
;; -----------------------------------------------------------------------------

(defmethod parse ((utterance t) (construction-inventory construction-inventory)
                  &optional silent)
  ;(set-hierarchy-feature (first (hierarchy-features construction-inventory))) ;; to check
  (let ((initial-cfs (de-render utterance (get-configuration construction-inventory :de-render-mode)
                                :cxn-inventory (original-cxn-set construction-inventory))))
    
    (initialize-transient-structure-blackboard initial-cfs utterance construction-inventory '<-)
                                       
    (unless silent (notify parse-started (listify utterance) initial-cfs))
    (multiple-value-bind
        (solution cip)
        (fcg-apply construction-inventory initial-cfs '<- :notify (not silent))
      (let ((meaning 
             (and solution
                  (extract-meanings
                   (left-pole-structure (car-resulting-cfs (cipn-car solution)))))))
        (unless silent (notify parse-finished meaning construction-inventory))
        (values meaning solution cip)))))

(defmethod parse ((utterance t) (construction-inventory construction-inventory-collection)
                  &optional silent)
  ;(set-hierarchy-feature (first (hierarchy-features construction-inventory)))
  (let ((initial-cfs (de-render utterance (get-configuration construction-inventory :de-render-mode))))
    (initialize-transient-structure-blackboard initial-cfs utterance construction-inventory '<-)
    (unless silent (notify parse-started (listify utterance) initial-cfs))
    (multiple-value-bind
        (solution cip)
        (fcg-apply construction-inventory initial-cfs '<- :notify (not silent))
      (setf solution (first (succeeded-nodes cip)))
      (let ((meaning 
             (and solution
                  (extract-meanings
                   (left-pole-structure (car-resulting-cfs (cipn-car solution)))))))
        (unless silent (notify parse-finished meaning construction-inventory))
        (values meaning solution cip)))))

(defmethod parse-all ((utterance t) (construction-inventory construction-inventory)
                      &key silent (n nil))
  "find all parse results, if n is a number, max n solutions will be returned"
  (let ((initial-cfs (de-render utterance (get-configuration construction-inventory :de-render-mode)
                                :cxn-inventory (original-cxn-set construction-inventory))))
    (initialize-transient-structure-blackboard initial-cfs utterance construction-inventory '<-)
    (unless silent (notify parse-all-started n (listify utterance)))
    (multiple-value-bind (solutions cip)
        (if n
          (fcg-apply-with-n-solutions construction-inventory initial-cfs '<- n
                                      :notify (not silent))
          (fcg-apply-exhaustively construction-inventory initial-cfs '<-
                                  :notify (not silent)))
      (let ((meanings (mapcar #'(lambda(solution)
                                  (extract-meanings
                                   (left-pole-structure
                                    (car-resulting-cfs (cipn-car solution)))))
                              solutions)))
        (unless silent (notify parse-all-finished meanings
                               construction-inventory))
        (values meanings solutions cip)))))


;; #############################################################################
;; Utility functions
;; -----------------------------------------------------------------------------

(export '(fcg-get-transient-structure
          fcg-get-direction formulation-p comprehension-p
          fcg-get-applied-cxn fcg-get-transient-unit-structure
          fcg-extract-selected-form-constraints
          fcg-extract-meanings
          solution-p))

(defun fcg-get-transient-structure (x &key (pick-cfs-fn #'car-resulting-cfs))
  "Find the transient structure in an object."
  (typecase x
    (cip-node (funcall pick-cfs-fn (cipn-car x)))
    (cxn-application-result (funcall pick-cfs-fn x))
    (coupled-feature-structure x)
    (cons x)
    (otherwise
     (error (format nil "Add a case for type ~a to #'fcg-get-transient-structure."
                    (type-of x))))))

(defun fcg-get-transient-unit-structure (x &key (pick-cfs-fn #'car-resulting-cfs))
  "Returns the pole that is used in FCG-light."
  (cond ((member (type-of x) '(cip-node cxn-application-result))
         (left-pole-structure (fcg-get-transient-structure x :pick-cfs-fn pick-cfs-fn)))
        ((eql 'coupled-feature-structure (type-of x))
         (left-pole-structure x))
        ((consp x) x)
        (t
         (error (format nil "Add a case for type ~a to #'fcg-get-transient-unit-structure."
                        (type-of x))))))

(defun fcg-extract-selected-form-constraints (object &optional (form-predicates (get-updating-references)))
  "Extract a selection of form constraints from a transient structure in FCG-light (meets and precedes by default)."
  (mappend #'(lambda(unit)
               (remove-if-not #'(lambda(form-constraint)
                                  (member (first form-constraint) form-predicates :test #'string=))
                              (unit-feature-value unit 'form)))
           (fcg-get-transient-unit-structure object)))

(defun fcg-extract-meanings (x)
  "Assumes only one pole."
  (extract-meanings (fcg-get-transient-unit-structure x)))

(defun fcg-get-direction (x)
  "Retrieve the direction of processig."
  (typecase x
    (cip-node (direction (cip x)))
    (construction-inventory-processor (direction x))
    (otherwise
     (error (format nil "Add a case for type ~a to #'fcg-get-direction."
                    (type-of x))))))

(defun formulation-p (x)
  "Returns T if we're in a formulation task."
  (equal '-> (fcg-get-direction x)))

(defun comprehension-p (x)
  "Returns T if we're in a comprehension task."
  (equal '<- (fcg-get-direction x)))

(defun fcg-get-applied-cxn (x)
  "Return the construction that was applied."
  (typecase x
    (cip-node (car-applied-cxn (cipn-car x)))
    (cxn-application-result (car-applied-cxn x))
    (otherwise
     (error (format nil "Add a case for type ~a to #'fcg-get-applied-cxn."
                    (type-of x))))))

(defun consolidate-repair-cxns (node)
  (let ((applied-cxns (applied-constructions node))
        (fcg-construction-set (original-cxn-set (construction-inventory node))))
    (dolist (cxn applied-cxns)
      (let ((fcg-cxn (get-original-cxn cxn)))
        (unless (find-cxn fcg-cxn fcg-construction-set)
          (add-cxn fcg-cxn fcg-construction-set))))))

(defun solution-p (node)
  "returns true if a node is a solution (succeeded)"
  (when (find 'succeeded (statuses node) :test 'equalp)
    t))

