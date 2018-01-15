(in-package :irl)

;; #############################################
;; evaluate primitive in program
;; #############################################

(defun evaluate-primitive-in-program (primitive-in-program
                                      bindings
                                      &optional (ontology (make-blackboard)))
  "primitive-in-program -- primitive from irl program e.g.
                              (pick-apples ?var-1 ?var3 ?var-123)
   bindings -- ((?var-1 1.0 value-1)(?var-2 nil nil)...)
   ontology -- blackboard
   Returns new sets of bindings -- (((?var-1 1.0 value-1)(?var-2 1.0 value-2))
                                    ((?var-1 1.0 value-1)(?var-2 1.0 value-3))...)"
  (multiple-value-bind (primitive-bindings-indices
                        primitive-bindings)
      (loop
       for var in (cdr primitive-in-program)
       for index = (position var bindings :key #'var)
       collect index into indices
       collect (nth index bindings) into primitive-bindings
       finally (return (values indices primitive-bindings)))
    ;; (assert (= (length assert (new-primitive-bindings) (length primitive-bindings))))
    (let* ((primitive-type (first primitive-in-program))
           (eval-primitive-result (evaluate-primitive primitive-type
                                                      primitive-bindings
                                                      ontology))
           (new-bindings-list 
            (when (and (listp eval-primitive-result)
                       eval-primitive-result)
              (loop for new-primitive-bindings in eval-primitive-result
                                      
                    do (assert (= (length new-primitive-bindings)
                                  (length primitive-bindings-indices)))
                    collect
                    (loop
                     with new-bindings = (copy-list bindings)
                     for new-primitive-binding in new-primitive-bindings
                     for index in primitive-bindings-indices
                     do (setf (nth index  new-bindings)
                              new-primitive-binding)
                     finally (return new-bindings))))))
      (if new-bindings-list
        new-bindings-list
        eval-primitive-result))))

;; #############################################
;; check IRL program 
;; #############################################

(defun check-irl-program (irl-program &optional (ontology (make-blackboard)))
  "Checks irl-program for mistakes"
  ;;(declare (type list irl-program))
  (let* ((variables (remove-duplicates (find-all-anywhere-if
                                        #'variable-p
                                        irl-program)))
         (bindings (loop for var in variables
                         collect (make-instance 'binding :var var)))
         (bind-statements  (find-all 'bind irl-program :key #'first))
         (primitives-in-program (set-difference irl-program bind-statements)))
    ;; first check, everything should be a non-empty list
    (loop for expr in irl-program
          unless (and (listp expr) expr)
          do (error "The expression should be a non-empty list,~%got: ~a." expr))

    ;; next we check all bind statements
    (loop
     for bind-statement in bind-statements
     do
     (if (not (= (length bind-statement) 4))
       (error "Expected four element bind statement, ~%got: ~a" bind-statement)
       (destructuring-bind (bind type variable value-expr) bind-statement
         (declare (ignore bind))
         (when (not (variable-p variable))
           (error "Expected variable identifier in ~a,~%got: ~a."
                  bind-statement variable))
         (let ((value 
                (or (when (typep value-expr 'entity) value-expr)
                    (when (symbolp value-expr)
                      (or (when ontology
                            (find-entity-by-id ontology value-expr))
                          (error 
                           "Could not find an entity with id ~a in  ontology" value-expr)))
                    (error
                     "Expected symbol or entity as value in ~a:~%got: ~a"
                     bind-statement value-expr))))
           (unless (typep value 'entity)
             (if (symbolp value-expr)
               (error "Value ~a returned by find-entity-by-id is not an entity"
                      value)
               (error "Value ~a is not an entity"
                      value)))
           (unless (typep value type)
             (if (symbolp value-expr)
               (error "Value ~a returned by find-entity-by-id for ~a is not of type ~a"
                      value value-expr type))
             (error "Value ~a is not of type ~a"
                    value type))
           (when (null value)
             (error "Could not read the value expression~%  ~a." value-expr))
           (when (not (typep value type))
             (error "The type of the value does not match the ~
                       defined type:~%- value: ~a (~a)~%- defined type: ~a"
                    value (type-of value) type))
           (let ((binding (find variable bindings :key #'var) ))
             (when (or (score binding)
                       (value binding))
               (error "~a is bound multiple times in bind-statements."
                      variable))
             (setf (score binding) 1.0)
             (setf (value binding) value))))))
    
    ;; lastly we check all primitives
    (loop for expr in primitives-in-program
          for variables = (cdr expr)
          unless (= (length variables) (length (remove-duplicates variables)))
          do (error "In ~a variables appear at least twice as argument." expr)
          ;; primitive must be bound
          unless (boundp (first expr))
          do (error "Primitive ~a is not defined "
                    (car expr))
          do
          (let  ((primitive-type (get-primitive (car expr))))
            (assert primitive-type) ;; this should never happen
            
            ;; check that the number of variables matches the
            ;; number of slot-specs:
            (unless (= (length (slot-specs primitive-type))
                       (length variables))
              (error "Error while reading primitive expression~%  ~a.~
                      ~%The number of given variables does not match ~
                      the number of slots."
                     expr))
            ;; check that the given cvar-id are proper variable identifiers:
            (loop for var in variables
                  unless (variable-p var)
                  do (error "Error while reading primitive expression~%  ~a.~
                             ~%Expected variable identifier, got ~a."
                            expr var))
            ;; instantiate the primitive:
            (loop
             with slot-specs = (slot-specs primitive-type)
             for slot-spec in slot-specs
             for var in variables
             for value = (value (find var bindings :key #'var))
             for expected-type = (slot-spec-type slot-spec)
             unless (or (null value)
                        (subtypep (type-of value) expected-type))
             do (error "Expected value of type ~a in ~a for ~a,~%got: ~a"
                       expected-type expr var value))))
    t))
 
;; #############################################
;; evaluate IRL program
;; #############################################

(export '(status
          children
          bindings
          primitives-evaluated
          primitives-remaining
          primitives-evaluated-w/o-result
          ontology nodes queue
          check-node-fn check-irl-program-fn choose-next-primitive-fn
          evaluate-irl-program evaluate-irl-program-started
          evaluate-irl-program-finished))

(defclass irl-program-evaluation-node ()
  ((status 
    :accessor status :initarg :status :initform 'initial
    :documentation "Status of this node (initial primitives-remaining
                    inconsistent no-primitives-remaining solution
                    bad-node)")
   (children :accessor children :initarg :children :initform nil)
   (bindings :accessor bindings :initarg :bindings)
   (primitives-evaluated 
    :accessor primitives-evaluated
    :initarg :primitives-evaluated :initform nil)
   (primitives-remaining 
    :accessor primitives-remaining
    :initarg :primitives-remaining :initform nil)
   (primitives-evaluated-w/o-result 
    :accessor primitives-evaluated-w/o-result
    :initarg :primitives-evaluated-w/o-result :initform nil)))


(defun check-node-no-duplicate-solutions (node nodes solutions &rest rest)
  (declare (ignore rest nodes))
  (if (primitives-remaining node)
    node
    (loop for solution in solutions
          never
          (loop for value in (mapcar #'value solution)
                for node-binding-value in (mapcar #'value (bindings node))
                always (or (and (null node-binding-value)
                                (null value))
                           (and node-binding-value
                                value
                                (equal-entity node-binding-value value)))))))


(define-event evaluate-irl-program-started (irl-program list))

(define-event evaluate-irl-program-finished
  (solutions list) (evaluation-tree irl-program-evaluation-node))

(defun evaluate-irl-program
       (irl-program
        ontology
        &key
        configuration
        (check-node-fn 'check-node-no-duplicate-solutions)
        (check-irl-program-fn 'check-irl-program)
        (choose-next-primitive-fn 'random-elt)
        (evaluate-bind-statements-fn 'evaluate-bind-statements))
  
  (let* (;; evaluate-bind-statements-fn default
         (evaluate-bind-statements-fn
          (get-configuration-or-default configuration 'evaluate-bind-statements-fn
                                        evaluate-bind-statements-fn))
         ;; check-irl-program-fn
         (check-irl-program-fn (get-configuration-or-default configuration 'check-irl-program-fn
                                                             check-irl-program-fn))
         ;; replace all non variables in the program with variables
         (irl-program
          ;; introduce bind statements
          (loop for item in irl-program
                if (eq (first item) 'bind)
                collect item ;; bind statemets
                else if (length= (find-all-if #'variable-p
                                              (cdr item))
                                 (cdr item))
                collect item ;; no non variables
                else ;; 
                append 
                (loop ;; collect bind statements and new-item
                      with new-item = (list (car item))
                      with bind-statements 
                      for parameter in (cdr item)
                      if (variable-p parameter)
                      do (pushend parameter new-item)
                      else
                      do (let ((var (make-id '?var))
                               (value (if (symbolp parameter)
                                        (find-entity-by-id ontology parameter)
                                        parameter)))
                           (pushend var new-item)
                           (push `(bind ,(type-of value) ,var ,value)
                                 bind-statements))
                      finally (return (cons new-item bind-statements))))))
    (when check-irl-program-fn
      (funcall check-irl-program-fn irl-program ontology))
    (let* ((queue nil)
           (nodes nil)
           (solutions nil)
           ;; initializing bindings
           (variables 
            (remove-duplicates (find-all-anywhere-if #'variable-p irl-program)))
           (bind-statements (find-all 'bind irl-program :key #'first)) 
           (irl-program-w/o-bind-statements 
            (set-difference irl-program bind-statements))
           (bindings-through-bind-statements
            (funcall evaluate-bind-statements-fn
                     bind-statements ontology))
           (bindings-for-unbound-variables 
            (loop for var in (set-difference 
                              variables 
                              (mapcar #'var bindings-through-bind-statements))
                  collect (make-instance 'binding :var var)))
           (bindings (append bindings-for-unbound-variables
                             bindings-through-bind-statements))
           (initial-node  (make-instance
                           'irl-program-evaluation-node
                           :status 'initial
                           :bindings bindings
                           :primitives-evaluated nil
                           :primitives-remaining irl-program-w/o-bind-statements
                           :primitives-evaluated-w/o-result nil)))
      
      (notify evaluate-irl-program-started irl-program)
      
      ;; we keep the initial node (for visualization)
      (push initial-node nodes)
      (cond
       ((not (funcall check-node-fn initial-node nodes solutions))
        (setf (status initial-node) 'bad-node))
       ((primitives-remaining initial-node) (push initial-node queue))
       ((null (position nil (bindings initial-node) :key #'value))
        (setf (status initial-node) 'solution)
        (push (bindings initial-node) solutions))
       (t (setf (status initial-node) 'no-primitives-remaining)))
      
      ;; run the queue
      (when queue
        (loop

         for current-node = (pop queue)
         for current-primitive = (funcall choose-next-primitive-fn
                                          (primitives-remaining current-node))
         for result = (evaluate-primitive-in-program current-primitive
                                                     (bindings current-node)
                                                     ontology)
         do
         (cond
          ((eq result 'inconsistent) ;; if inconsistent
           (let ((new-node (make-instance
                            'irl-program-evaluation-node
                            :status 'inconsistent
                            :bindings (bindings current-node)
                            :primitives-evaluated 
                            (cons current-primitive
                                  (primitives-evaluated current-node))
                            :primitives-remaining
                            (append
                             (remove current-primitive
                                     (primitives-remaining current-node))
                             (primitives-evaluated-w/o-result current-node))
                            :primitives-evaluated-w/o-result nil)))
             (push new-node nodes)
             (push new-node (children current-node))))

          ((null result) ;; if nothing happens, requeue the node
           (let ((remaining-primitives (remove current-primitive
                                               (primitives-remaining current-node))))
             (cond
              (remaining-primitives
               (when (not (eq (status current-node) 'initial))
                 (setf (status current-node) 'primitives-remaining))
               (setf (primitives-remaining current-node) remaining-primitives)
               (setf (primitives-evaluated-w/o-result current-node)
                     (push current-primitive 
                           (primitives-evaluated-w/o-result current-node)))
               (setf queue (append queue (list current-node))))
              (t (setf (status current-node) 'no-primitives-remaining)
                 (push current-primitive 
                       (primitives-evaluated-w/o-result current-node))
                 (setf (primitives-remaining current-node) nil)))))
          ((and (listp result) result) ;; we have results
           (let ((remaining-primitives 
                  (append
                   (remove current-primitive
                           (primitives-remaining current-node))
                   (primitives-evaluated-w/o-result current-node)))
                 (evaluated-primitives (cons current-primitive
                                             (primitives-evaluated current-node))))
             (loop for res in result
                   for new-node = (make-instance
                                   'irl-program-evaluation-node
                                   :status 'no-primitives-remaining
                                   :bindings res
                                   :primitives-evaluated evaluated-primitives
                                   :primitives-remaining remaining-primitives
                                   :primitives-evaluated-w/o-result nil)
                   do
                   (push new-node (children current-node))
                   (cond
                    ((null (funcall check-node-fn new-node nodes solutions))
                     (setf (status new-node) 'bad-node))
                    (remaining-primitives   
                     (setf (status new-node) 'primitives-remaining)
                     (push new-node queue))
                    ((null (position nil res :key #'value))
                     (setf (status new-node) 'solution)
                     (push res solutions)))
                   (push new-node nodes) ;; add node to nodes
                   ))))
         while queue)) ;; queue while

      ;; clean solutions
      (setf solutions (loop for solution in solutions
                            if solution
                            collect solution))
    
      (notify evaluate-irl-program-finished solutions (car (last nodes)))
     
      ;; return solutions
      (values solutions (car (last nodes))))))

;; #############################################
;; IRL program connected?
;; #############################################

(export 'irl-program-connected?)

(defun irl-program-connected? (irl-program)
  "Checks whether an irl program is connected. Returns t if so, the
   number of sub graphs and the sub graphs themselves"
  (loop with classes = nil
        with sub-networks = nil
        for x in irl-program
        for variables = (find-all-if #'variable-p x)
        for (cs subs) = (multiple-value-list
                         (loop for class in classes
                               for sub-network in sub-networks
                               when (loop for var in variables
                                          thereis (member var class))
                               collect class into cs
                               and
                               collect sub-network into subs
                               finally (return (values cs subs))))
        if cs
        do
        (loop for class in cs do (setf classes (remove class classes)))
        (push (remove-duplicates (reduce #'append (cons variables cs)))
              classes)
        (loop for sub in subs do (setf sub-networks (remove sub sub-networks)))
        (push (cons x (reduce #'append subs)) sub-networks)
        else
        do
        (push variables classes)
        (push (list x) sub-networks)
        finally
        (return (values
                 (< (length classes) 2)
                 (length classes)
                 sub-networks))))

;; #############################################
;; irl-program
;; #############################################

(export '(irl-program-p))

(defun irl-program-p (thing)
  "returns t if thing conforms to the basic syntax of irl-programs
   list of bind statements (bind ...)  and irl-primitives (primitive ..)"
  (and (listp thing)
       (loop
        for s in thing
        always (and (listp s)
                    (or (eq (first s) 'bind)
                        (get-primitive (first s)))))))




