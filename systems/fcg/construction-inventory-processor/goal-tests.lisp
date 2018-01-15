(in-package :fcg)

(defmethod cip-goal-test ((node cip-node) (mode symbol))
  (error "Please implement cip-goal-test for mode ~w" mode))
     
(defmethod cip-goal-test ((node cip-node) (mode (eql :no-applicable-cxns)))
  "Checks whether there are no more applicable constructions when a node is
fully expanded and no constructions could apply to its children
nodes."
  (and (or (not (children node))
	   (loop for child in (children node)
                 never (cxn-applied child)))
       (fully-expanded? node)))

(defmethod cip-goal-test ((node cip-node) (mode (eql :always-fail)))
  "Never yield a solution."
  nil)

(defmethod cip-goal-test ((node cip-node) (mode (eql :at-least-one-cxn-applied)))
  "Goal is reached when the list of applicable constructions of the
current node is non-empty."
  (when (applied-constructions node) t))

(defun connected-syntactic-structure (left-pole &key (grammar-hierarchy-features
                                                      (hierarchy-features *fcg-constructions*)))

  (let ((unit-names (remove 'root (mapcar #'unit-name left-pole)))
        (units-that-have-parents nil))

    (dolist (unit left-pole)
      (let ((unit-hierarchy-features
             (loop for feature in grammar-hierarchy-features
                  when (unit-feature unit feature)
                  append (unit-feature-value unit feature))))
        (when unit-hierarchy-features
          (setf units-that-have-parents (append units-that-have-parents unit-hierarchy-features)))))
    
    (loop with ancestor-units = nil ;;highest order unit ("root")
          for unit-name in unit-names
          unless (find unit-name units-that-have-parents)
          do (setf ancestor-units (push unit-name ancestor-units))
          finally (return (values (= (length ancestor-units) 1) (length ancestor-units))))))
      

(defmethod cip-goal-test ((node cip-node) (mode (eql :connected-structure)))
  "Checks whether all units of the left-pole-structure are connected.
This means that all feature names, except for the root unit name and
the highest parent unit can be found in the value of the hierarchy
features (subunits, constituents, dependents) of the units in the
transient structure."
  (let* ((left-pole (left-pole-structure (car-resulting-cfs (cipn-car node))))
         (connected? (connected-syntactic-structure left-pole :grammar-hierarchy-features (hierarchy-features (construction-inventory node)))))
    (or connected?
        (progn
          (set-data (goal-test-data node) 'dependencies-realized  left-pole)
          nil))))
        
    

(defmethod cip-goal-test ((node cip-node) (mode (eql :no-meaning-in-root)))
  "The node is a valid solution when there is no meaning predicate
left in the root unit (formulation only)."
  (let ((meaning-in-root-unit (extract-meaning (find 'root
                                                     (left-pole-structure
                                                      (car-resulting-cfs (cipn-car node))) :key 'first))))
    (set-data (goal-test-data node) 'meaning-in-root-unit meaning-in-root-unit)
    (not meaning-in-root-unit)))

(defmethod cip-goal-test ((node cip-node) (mode (eql :no-strings-in-root)))
  "The node is a valid solution when there is are no string features
left in the root unit's form predicates (comprehension only)."
  (let ((strings-in-root (get-strings (assoc 'root
                                       (left-pole-structure
                                        (car-resulting-cfs (cipn-car node)))))))
    (set-data (goal-test-data node) 'strings-in-root strings-in-root)
    (not strings-in-root)))

(defmethod cip-goal-test ((node cip-node) (mode (eql :fcg-light-no-strings-in-root)))
  "Calls :no-strings-in-root goal test (use this instead)"
  (warn "Please use the :no-strings-in-root goal test now. :fcg-light-no-strings-in-root is no longer supported.")
  (cip-goal-test node :no-strings-in-root))

(export 'connected-semantic-network)

(defun connected-semantic-network (semantic-network)
  "Checks whether an irl program is connected. Returns t if so and the
   number of sub graphs as a second value"
  (loop with classes = nil
     for x in semantic-network
     for variables = (find-all-if #'variable-p x)
     for cs = (loop for class in classes
                 when (intersection variables class)
                 collect class)
     if cs
     do (loop for class in cs do (setf classes (remove class classes)))
       (push (remove-duplicates (reduce #'append (cons variables cs)))
             classes)
     else
     do (push variables classes)
     finally (return (values (= (length classes) 1) (length classes)))))

(defmethod cip-goal-test ((node cip-node) (mode (eql :connected-semantic-network)))
  "Checks whether the resulting meaning network is fully integrated
(consists of a single connected chunk)."
  (let* ((meaning 
          (extract-meanings (left-pole-structure 
                             (car-resulting-cfs (cipn-car node)))))
         (connected? (connected-semantic-network meaning)))
    (or connected?
        (progn
          (set-data (goal-test-data node) 'unconnected-network  meaning)
          nil))))

(defgeneric equivalent-meaning? (interpreted-meaning intended-meaning mode)
  (:documentation "Compares the meaning returned by re-entrance with
                    the intended meaning"))

(defmethod equivalent-meaning? ((interpreted-meaning list) (intended-meaning list) 
 				(mode (eql :unify)))
  (unify (cons '== intended-meaning) interpreted-meaning))

(defmethod equivalent-meaning? ((interpreted-meaning list) (intended-meaning list) 
 				(mode (eql :unify-with-instantiated-variables)))
  "Checks whether the interpreted-meaning is equivalent to the intended meaning
    (equivalent: predicates or permutation, same 'bindings-network')"
  (unify (cons '==p intended-meaning) (instantiate-variables interpreted-meaning)))

(defmethod equivalent-meaning? ((interpreted-meaning list) (intended-meaning list) 
 				(mode (eql :unify-no-equalities)))
  (delete-if #'(lambda (bs)
 		 (equalities? 
 		  (delete-if #'(lambda (binding) (null (binding-val binding)))
 			     bs)))
 	     (unify (cons '== intended-meaning) interpreted-meaning)))


(require-configuration :equivalent-meaning-mode)

(defmethod cip-goal-test ((node cip-node) (mode (eql :re-enter-utterance)))
  "This is a goal test that can uses the old FCG notation (before 2015)."
  (warn "Please check this goal test before using it. It is not guaranteed to work (old FCG notation).")
  (let* ((construction-inventory (construction-inventory (cip node)))
         (utterance (render 
                     (right-pole-structure (car-resulting-cfs (cipn-car node)))
                     (get-configuration construction-inventory :render-mode)
                     :node node))
         (cfs (de-render utterance (get-configuration construction-inventory
                                                      :de-render-mode)))
         (solution+cip (multiple-value-list
                        (with-disabled-monitor-notifications
                          (fcg-apply (construction-inventory (cip node))
                                     cfs '<-
                                     :configuration (configuration (construction-inventory node))))))
         (solution (car solution+cip))
         (cip (cadr solution+cip))
         (intended-meaning 
          (extract-meanings
           (left-pole-structure
            (car-resulting-cfs (cipn-car (top-node (cip node)))))))
         (interpreted-meaning 
          (when solution
            (extract-meanings (left-pole-structure 
                               (car-resulting-cfs (cipn-car solution))))))
         (equivalent-meaning? 
          (equivalent-meaning?
           interpreted-meaning intended-meaning
           (get-configuration construction-inventory :equivalent-meaning-mode))))
    (set-data (goal-test-data node) 'utterance utterance)
    (set-data (goal-test-data node) 'parse-process cip)
    (set-data (goal-test-data node) 'solution solution)
    (set-data (goal-test-data node) 'intended-meaning intended-meaning)
    (set-data (goal-test-data node) 'interpreted-meaning interpreted-meaning)
    (set-data (goal-test-data node) 'equivalent-meaning? equivalent-meaning?)
    equivalent-meaning?))

(defmethod cip-goal-test ((node cip-node) (mode (eql :re-enter-utterance-until-found)))
  "This is a goal test that can uses the old FCG notation (before 2015)."
  (warn "Please check this goal test before using it. It is not guaranteed to work (old FCG notation).")
  (loop 
   with construction-inventory = (construction-inventory (cip node))
   with utterance = (render (right-pole-structure (car-resulting-cfs (cipn-car node)))
                            (get-configuration construction-inventory :render-mode)
                            :node node)
   with cfs = (de-render utterance (get-configuration construction-inventory :de-render-mode))
   for solution+cip = (multiple-value-list
                       (with-disabled-monitor-notifications
                         (fcg-apply (construction-inventory (cip node)) cfs '<-
                                    :configuration (configuration (construction-inventory (cip node))))))
   then (multiple-value-list
         (with-disabled-monitor-notifications
           (next-cip-solution cip)))
   for solution = (car solution+cip) 
   for cip = (cadr solution+cip)
   for intended-meaning = (extract-meanings
                           (left-pole-structure
                            (car-resulting-cfs (cipn-car (top-node (cip node))))))
   for interpreted-meaning = (when solution
                               (extract-meanings (left-pole-structure 
                                                  (car-resulting-cfs (cipn-car solution)))))
   for equivalent-meaning? = (equivalent-meaning?
                              interpreted-meaning intended-meaning
                              (get-configuration construction-inventory :equivalent-meaning-mode))
   until (or equivalent-meaning?
             (null solution))
   finally 
   (set-data (goal-test-data node) 'utterance utterance)
   (set-data (goal-test-data node) 'parse-process cip)
   (set-data (goal-test-data node) 'solution solution)
   (set-data (goal-test-data node) 'intended-meaning intended-meaning)
   (set-data (goal-test-data node) 'interpreted-meaning interpreted-meaning)
   (set-data (goal-test-data node) 'equivalent-meaning? equivalent-meaning?)
   (return equivalent-meaning?)))
