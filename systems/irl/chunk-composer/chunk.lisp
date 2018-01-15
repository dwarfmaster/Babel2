
(in-package :irl)


;; ############################################################################
;; chunk
;; ----------------------------------------------------------------------------

(export '(chunk make-chunk id irl-program target-var open-vars score
          create-chunk-from-primitive create-chunks-from-primitives
          create-chunk-from-irl-program
          irl-program->title
          irl-program->id
          get-unconnected-vars
          get-target-var
          get-open-vars))

(defclass chunk ()
  ((id
    :initarg :id :initform (make-id 'chunk) :accessor id :type symbol)
   (irl-program 
    :initarg :irl-program :initform nil :accessor irl-program :type list
    :documentation "the s-expression representation of the irl program")
   (target-var 
    :initarg :target-var :accessor target-var :type list
    :documentation "A (?variable . type) cons for the target for the chunk"
    :initform nil)
   (open-vars
    :initarg :open-vars :accessor open-vars :type list :initform nil
    :documentation "A list of (?variable . type) conses for the open variables")
   (score :initarg :score :accessor score :type single-float :initform 1.0
          :documentation "A score for the chunk"))
  (:documentation "A chunk is an irl program with an explicit target
                   and open variables"))

(defun create-chunk-from-primitive (primitive-id 
                                    &key (score 0.5) (chunk-class 'chunk)
                                    chunk-id
                                    target-var-is-open-var)
  "Creates a chunk from a primitive by making the variable of the first
   slot the target variable and all other variables open variables"
  (let* ((vars (loop for slot-spec in (slot-specs (get-primitive primitive-id))
                     collect (cons (intern (mkstr "?" (slot-spec-name slot-spec)))
                                   (slot-spec-type slot-spec))))
         (irl-program (list (cons primitive-id (mapcar #'car vars)))))
    (make-instance chunk-class
                   :id (or chunk-id primitive-id)
                   :target-var (car vars)
                   :open-vars (if target-var-is-open-var
                                vars
                                (cdr vars))
                   :irl-program irl-program
                   :score score)))

(defun create-chunks-from-primitives (primitive-ids 
                                      &key (score 0.5) (chunk-class 'chunk))
  "Creates chunks from primitives"
  (loop for primitive-id in primitive-ids
     collect (create-chunk-from-primitive primitive-id :score score
                                          :chunk-class chunk-class)))

(defun get-unconnected-vars (irl-program)
  ;; find all unconnected variables, in other words, all those that
  ;; appear once in the irl-program
  ;; notice that this also includes the target-var
  (loop
   with variables = (find-all-anywhere-if #'variable-p irl-program)
   for var in (remove-duplicates variables)
   if (= (count var variables)  1)
   collect var))

(defun get-target-var (irl-program)
  ;; returns the one unconnected open-var that appears as first argument
  ;; of a primitive, nil otherwise
  (let* ((open-vars (get-unconnected-vars irl-program))
         (target-vars (intersection
                       open-vars
                       (append
                        (mapcar #'second irl-program))))
         (target-var (when (= 1 (length target-vars))
                       (first target-vars)))
         (target-var-type
          (when target-var
            (let* ((primitive (find target-var irl-program
                                    :test #'member))
                   (slot-spec (nth
                               (- (position target-var primitive) 1)
                               (slot-specs (get-primitive (first primitive))))))
              (slot-spec-type slot-spec)))))
    (values target-var target-var-type)))

(defmethod target-var ((irl-program list))
  ;; returns the one unconnected open-var that appears as first argument
  ;; of a primitive, nil otherwise
  (unless (length= 0 irl-program)
    (if (and (length= 1 irl-program)(eq (car (first irl-program)) 'bind))
      (cons (third (first irl-program))
            (second (first irl-program)))
      (let* ((open-vars (get-unconnected-vars irl-program))
             (target-vars (intersection
                           open-vars
                           (append
                            (mapcar #'second irl-program))))
             (target-var (when (= 1 (length target-vars))
                           (first target-vars)))
             (target-var-type
              (let* ((primitive (find target-var irl-program
                                      :test #'member))
                     (slot-spec (nth
                                 (- (position target-var primitive) 1)
                                 (slot-specs (get-primitive (first primitive))))))
                (slot-spec-type slot-spec))))
        (cons target-var target-var-type)))))

(defun get-open-vars (irl-program)
  ;; find all unconnected variables which are not target
  (loop
   with open-vars =
   (set-difference
    (get-unconnected-vars irl-program)
    (mapcar #'second irl-program))
   for open-var in open-vars
   for primitive = (find-if #'(lambda(x)(member open-var x))
                            irl-program)
   if (eq (first primitive) 'bind) ;; it really is a bind statement
   collect (second primitive) into types
   else ;; normal primitive
   collect
   (let ((slot-spec (nth (position open-var (cdr primitive))
                         (slot-specs (get-primitive (first primitive))))))
     (slot-spec-type slot-spec))
   into types
   finally (return 
            (values open-vars types))))

(defmethod open-vars ((irl-program list))
  ;; find all unconnected variables which are not target
  (loop
   with open-vars =
   (set-difference
    (get-unconnected-vars irl-program)
    (mapcar #'second irl-program))
   for open-var in open-vars
   for primitive = (find-if #'(lambda(x)(member open-var x))
                            irl-program)
   if (eq (first primitive) 'bind) ;; it really is a bind statement
   collect (second primitive) into types
   else ;; normal primitive
   collect
   (let ((slot-spec (nth (position open-var (cdr primitive))
                         (slot-specs (get-primitive (first primitive))))))
     (slot-spec-type slot-spec))
   into types
   finally (return (apply #'mapcar #'cons (list open-vars types)))))
    
(defun create-chunk-from-irl-program (irl-program
                                      &key (chunk-class 'chunk)
                                      (id (irl-program->id irl-program))
                                      (score 0.5)
                                      (target-var nil))
  (let* ((variables (find-all-anywhere-if #'variable-p irl-program))
         (unconnected-variables (loop for var in variables
                                      if (= (count var variables)  1)
                                      collect var))
         (target-vars (intersection unconnected-variables (append
                                                           (mapcar #'second irl-program)
                                                           (mapcar #'third (find-all-if #'(lambda (x) (eq (car x) 'bind)) irl-program)))))
         (target-var (if target-var
                       (if (find target-var target-vars)
                         target-var
                         (error "specified target-var not available"))
                       (if (= (length target-vars) 1)
                         (first target-vars)
                         (error "zero or more than one possible target var found in irl-program"))))
         (target-var-primitive (find target-var irl-program :test #'member))
         (target-var-type (if (eq (first target-var-primitive) 'bind) (second target-var-primitive)
                            (slot-spec-type
                             (nth
                              (- (position target-var target-var-primitive) 1)
                              (slot-specs (get-primitive (first target-var-primitive)))))))
         (open-v (set-difference unconnected-variables target-vars))
         (open-vars
          (sort open-v
                #'<
                :key #'(lambda (x)
                         (position x (find x irl-program
                                           :test #'member)))))
         (primitives-with-open-vars (loop for open-var in open-vars
                                          collect
                                          (find open-var irl-program :test #'member)))
         (open-var-types (loop for open-var in open-vars
                               for primitive-with-open-vars in primitives-with-open-vars
                               collect
                               (slot-spec-type
                                (nth
                                 (- (position open-var primitive-with-open-vars) 1)
                                 (slot-specs
                                  (get-primitive (first primitive-with-open-vars))))))))
    (make-instance chunk-class
                   :id id
                   :irl-program irl-program
                   :target-var (cons target-var target-var-type)
                   :open-vars (loop for open-var in open-vars
                                    for open-var-type in open-var-types
                                    collect (cons open-var open-var-type))
                   :score score)))

;; ############################################################################
;; substitute-variables
;; ----------------------------------------------------------------------------

(export '(substitute-variables))

(defgeneric substitute-variables (thing substitutions)
  (:documentation "Substitutes variables in thing. Substitutions is an
                   association list. The result is a new structure
                   with unsubstituted symbols eq to those in thing"))

(defmethod substitute-variables ((thing t) (substitutions list))
  thing)

(defmethod substitute-variables ((symbol symbol) (substitutions list))
  (or (assqv symbol substitutions) symbol))

(defmethod substitute-variables ((cons cons) (substitutions list))
  (cons (substitute-variables (car cons) substitutions)
        (when (cdr cons) (substitute-variables (cdr cons) substitutions))))

(defmethod substitute-variables ((chunk chunk) (substitutions list))
  (make-instance 
   'chunk
   :irl-program (substitute-variables (irl-program chunk) substitutions)
   :target-var (substitute-variables (target-var chunk) substitutions)
   :open-vars (substitute-variables (open-vars chunk) substitutions)
   :score (score chunk)))

;; ############################################################################
;; expand-chunks
;; ----------------------------------------------------------------------------

(defun expand-chunks (irl-program ontology 
                      &optional already-expanded-symbols)
  (let* ((chunks (find-data ontology 'chunks)))
    (loop for c in irl-program
       for chunk = (find (first c) chunks :key #'id)
       if (and chunk (not (find (id chunk) already-expanded-symbols)))
       append (substitute-variables 
               (expand-chunks (irl-program chunk) ontology
                              (cons (id chunk) already-expanded-symbols))
               ;; call-pattern
               (append
                (list (cons
                       (car (target-var chunk)) (second c)))
                (loop for open-var in (open-vars chunk)
                   for call-p-var in (cdr (cdr c))
                   collect (cons (car open-var) call-p-var))
                 ))
       else 
       append (list c))))


;; ############################################################################
;; functions for making new chunks from others
;; ----------------------------------------------------------------------------

(export '(combine-chunk-program combine-chunk-call-pattern
          recombine-open-variables link-open-variables))

(defgeneric combine-chunk-program (chunk other-chunk))

(defmethod combine-chunk-program ((chunk chunk)(other-chunk chunk))
  "Adds the network of another chunk when the target variable of that
   chunk is compatble with an open variable"
  (loop for open-var in (open-vars chunk)
     for (open-var-id . open-var-type) = open-var
     when (or (subtypep open-var-type (cdr (target-var other-chunk)))
              (subtypep (cdr (target-var other-chunk)) open-var-type))
     collect
     (let* ((variables-in-both-chunks 
             (intersection 
              (find-all-anywhere-if #'variable-p (irl-program chunk))
              (find-all-anywhere-if #'variable-p (irl-program other-chunk))))
            (substitutions 
             (cons (cons (car (target-var other-chunk)) open-var-id)
                   (loop for var in variables-in-both-chunks
                      collect (cons var (make-id var)))))
            (other-chunk-2 (substitute-variables other-chunk substitutions)))
       (make-instance
        'chunk
        :target-var
        (if (irl-program chunk)
            (target-var chunk)
            (target-var other-chunk-2))
        :open-vars (append (remove open-var (open-vars chunk))
                           (open-vars other-chunk-2))
        :irl-program (append (irl-program chunk)
                             (irl-program other-chunk-2))
        :score (average (list (score chunk) (score other-chunk)))))))

(defun combine-chunk-call-pattern (chunk other-chunk)
  "Adds the call pattern of another chunk when the target variable of
   that chunk is compatble with an open variable"
  (loop for open-var in (open-vars chunk)
     for (open-var-id . open-var-type) = open-var
     when (or (subtypep open-var-type (cdr (target-var other-chunk)))
              (subtypep (cdr (target-var other-chunk)) open-var-type))
     collect
     (let* ((variables-in-both-chunks 
             (intersection
              (find-all-anywhere-if #'variable-p (irl-program chunk))
              (find-all-anywhere-if #'variable-p (irl-program other-chunk))))
            (substitutions 
             (cons (cons (car (target-var other-chunk)) open-var-id)
                   (loop for var in variables-in-both-chunks
                      collect (cons var (make-id var))))))
       (make-instance
        'chunk
        :irl-program
        `(,@(irl-program chunk)
            ,(substitute-variables
              `(,(id other-chunk) ,(car (target-var other-chunk))
                 ,@(mapcar #'car (open-vars other-chunk)))
              substitutions))
        :target-var (target-var chunk)
        :open-vars (append (remove open-var-id (open-vars chunk)
                                   :key #'car)
                           (substitute-variables
                            (open-vars other-chunk)
                            substitutions))
        :score (average (list (score chunk) (score other-chunk)))))))

(defun recombine-open-variables (chunk)
  "Tries to account for open variables of 'chunk' by using primitives
   that are already in the irl program. Returns all possible
   solutions."
  (loop with irl-program = (irl-program chunk)
     for (open-var-id . open-var-type) in (open-vars chunk)
     append (loop for predicate in irl-program
               for target-var-id = (cadr predicate)
               for target-var-type 
               = (slot-spec-type 
                  (first (slot-specs (get-primitive (car predicate)))))
               when (and (not (eq target-var-id (car (target-var chunk))))
                         (not (find open-var-id (cdr predicate)))
                         (or (subtypep open-var-type target-var-type)
                             (subtypep target-var-type open-var-type)))
               collect (let ((new-chunk 
                              (substitute-variables 
                               chunk (list (cons open-var-id target-var-id)))))
                         (setf (open-vars new-chunk)
                               (delete (find target-var-id (open-vars new-chunk)
                                             :key #'car)
                                       (open-vars new-chunk)))
                         new-chunk))))


(defun link-open-variables (chunk)
  "Tries to make two open variables of compatible type equal and when
   it does, reduces the number of open variables of the chunk by one."
  (loop for (open-var . other-open-vars) on (open-vars chunk)
     for open-var-id = (car open-var)
     for open-var-type = (cdr open-var)
     append (loop for other-open-var in other-open-vars
               for other-open-var-id = (car other-open-var)
               for other-open-var-type = (cdr other-open-var)
               when (or (subtypep open-var-type other-open-var-type)
                        (subtypep other-open-var-type open-var-type))
               collect (let ((new-chunk 
                              (substitute-variables 
                               chunk (list (cons open-var-id other-open-var-id)))))
                         (setf (open-vars new-chunk)
                               (delete (find other-open-var-id (open-vars new-chunk)
                                             :key #'car)
                                       (open-vars new-chunk)))
                         new-chunk))))


;; ############################################################################
;; add-bind-statement-for-open-var-to-chunk
;; ----------------------------------------------------------------------------

(export 'add-bind-statement-for-open-var-to-chunk)

(defun add-bind-statement-for-open-var-to-chunk (chunk bind-statement)
  "Adds a bind statement to a chunk and removes the variable from
   the open vars. Returns nil when this was not possible"
  (declare (type chunk chunk)
           (type list bind-statement))
  (when (assoc (third bind-statement) (open-vars chunk))
    (push bind-statement (irl-program chunk))
    (setf (open-vars chunk) (remove (assoc (third bind-statement) 
                                           (open-vars chunk))
                                    (open-vars chunk)))
    t))



;; ############################################################################
;; printing, copying, etc
;; ----------------------------------------------------------------------------

(defun irl-program->title (program &key (for-html t))
  "creates flat string description of the primitives used in an irl program"
  (if program
    (reduce 
     #'string-append
     (loop with primitive-ids = (mapcar #'car program)
           with unique-primitive-ids = (stable-sort (copy-list
                                                     (remove-duplicates primitive-ids))
                                                    #'string<)
           for id in unique-primitive-ids
           for i from 1
           for number-of-primitives-with-id = (length (find-all id primitive-ids))
           append (list 
                   (if (> number-of-primitives-with-id 1)
                     (format nil (if for-html "~a&#160;*&#160;" "~a * ")
                             number-of-primitives-with-id) "")
                   (format nil "~(~a~)" id) 
                   (if (< i (length unique-primitive-ids)) ", " ""))))
    "initial"))

(defun irl-program->id (program &optional (max-number-of-letters 10 ))
  "creates a symbol representing the primitives used"
  (let* ((bind-statements (find-all 'bind program :key #'first))
        (primitives (set-difference program bind-statements))
        (id-parts 
         (append
          (mapcar #'fourth bind-statements)
          (when (and (< 1 (length bind-statements))
                     (< 1 (length primitives)))
            '(--))
          (loop with primitive-ids = (remove 'bind (mapcar #'car program))
                with unique-primitive-ids =
                (stable-sort (copy-list
                              (remove-duplicates primitive-ids)) #'string<)
                for id in unique-primitive-ids
                for number-of-primitives-with-id =
                (length (find-all id primitive-ids))
                when (> number-of-primitives-with-id 1)
                collect (symb number-of-primitives-with-id '-)
                collect (symb id))))
        (id-string
         (string-append (format nil "~a" (first id-parts))
                        (format nil "~{--~a~}" (cdr id-parts)))))
    (if (and max-number-of-letters
             (< max-number-of-letters (length id-string)))
      (symb (subseq id-string 0 max-number-of-letters))
      (symb id-string))))
                
        

(defmethod print-object ((chunk chunk) stream)
  (format stream "<chunk: ~a>" (id chunk)))

(defmethod copy-object ((chunk chunk))
  (make-instance 'chunk :irl-program (copy-list (irl-program chunk))
                 :id (id chunk)
                 :target-var (target-var chunk)
                 :open-vars (copy-list (open-vars chunk))
                 :score (score chunk)))




