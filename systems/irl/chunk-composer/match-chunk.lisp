
(in-package :irl)

(export '(map-frame-bindings map-frame source-chunk match-chunk match-chunk-started match-chunk-finished
          unify-irl-programs equivalent-irl-programs? *equivalent-irl-programs?-timer*))

(defparameter *equivalent-irl-programs?-timer* 0)

;; #####################################################################
;; type checking
;; ---------------------------------------------------------------------

(defstruct typed-var name type)

(defmethod print-object ((var typed-var) stream)
  (format stream "[~a: ~(~a~)]" (typed-var-name var) (typed-var-type var)))

(defgeneric compatible-types (type-1 type-2)
  (:documentation "When one type is is a subtype of the other, returns
                   the subtype. E.g. compatible-types(integer,number)=integer"))
 
(defmethod compatible-types ((type-1 symbol) (type-2 symbol))
  (cond ((subtypep type-1 type-2) type-1)
        ((subtypep type-2 type-1) type-2)
        (t nil)))

(defmethod compatible-types ((type-1 typed-var) (type-2 typed-var))
  (compatible-types (typed-var-type type-1) (typed-var-type type-2)))



;; #####################################################################
;; map frames
;; ---------------------------------------------------------------------

(defstruct map-frame bindings)

(defmethod print-object ((frame map-frame) stream)
  (format stream "{~{~a~^ ~}}" (map-frame-bindings frame)))

(defun extend-frame (frame new-binding &key (extension-test #'function-frame))
  "Test if a new binding (i.e a (?var-1 . ?var-2) cons) can be added
   to a frame without violating any compatibility constraints. If so,
   the new frame is returned. If not, nil is returned.
   The compatibility constraint for map is that the frame is a function."
  (declare (type map-frame frame)
           (type cons new-binding))
  (when (funcall extension-test frame new-binding)
    ;; If F+ is a function, return it. Otherwise, return NIL
    (make-map-frame 
     :bindings (remove-duplicates
                (cons new-binding (map-frame-bindings frame))
                :test #'equal))))

(defun function-frame (frame new-binding)
  "frame = F, new-bindings = (a, b)
   F+ = F + {(a, b)}
   Suppose F is a function then F+ is a function iff for all (a', b') in F"
  (loop for binding in (map-frame-bindings frame)
        ;; if a' = a
        always (if (equal (car binding) (car new-binding))
                 ;; then b' = b
                 (equal (cdr binding) (cdr new-binding))
                 t)))

(defun surjective-function-frame (frame new-binding)
  "frame = F, new-bindings = (a, b)
   F+ = F + {(a, b)}
   Suppose F is a surjective function then F+ is a surjective function
   iff:"
  (and (function-frame frame new-binding) ;; F+ is a function
       ;; and for all (a' b'):
       (loop for binding in (map-frame-bindings frame)
             ;; if b' = b
             always (if (equal (cdr binding) (cdr new-binding))
                      ;; then a' = a
                      (equal (car binding) (car new-binding))
                      t))))
;; #####################################################################
;; Find-map-function: only unification of the predicates.
;; the mapping is one way (the right side does not gain equalities)


(defgeneric find-map-function (a b &optional frame &key)
  (:documentation "A unification with type checking that does not add
                   equalities to the right side"))

(defmethod find-map-function ((v1 t) (v2 t) &optional frame &key extension-test)
  (declare (ignore frame) (ignore extension-test))
  nil)

(defmethod find-map-function ((v1 null) (v2 null) 
                        &optional (frame (make-map-frame))
                        &key extension-test)
  (declare (ignore extension-test))
  frame)

(defmethod find-map-function ((p1 list) (p2 list) 
                        &optional (frame (make-map-frame))
                        &key (extension-test #'function-frame))
  (when (length= p1 p2)
    (let ((new-frame (find-map-function (car p1) (car p2) frame :extension-test extension-test)))
      (when new-frame
        (find-map-function (cdr p1) (cdr p2) new-frame :extension-test extension-test)))))
    
(defmethod find-map-function ((var1 typed-var) (var2 typed-var)
                        &optional (frame (make-map-frame))
                        &key (extension-test #'function-frame))
  (when (compatible-types var1 var2)
    (find-map-function (typed-var-name var1) (typed-var-name var2) frame :extension-test extension-test)))

(defmethod find-map-function ((v1 symbol) (v2 typed-var)
                        &optional (frame (make-map-frame))
                        &key (extension-test #'function-frame))
  (find-map-function v1 (typed-var-name v2) frame :extension-test extension-test))
  
(defmethod find-map-function ((v1 typed-var) (v2 symbol) 
                        &optional (frame (make-map-frame))
                        &key (extension-test #'function-frame))
  (find-map-function (typed-var-name v1) v2 frame :extension-test extension-test))

(defmethod find-map-function ((v1 symbol) (v2 symbol) 
                        &optional (frame (make-map-frame))
                        &key (extension-test #'function-frame))
  (if (variable-p v1)
      (extend-frame frame (cons v1 v2) :extension-test extension-test)
      (when (string= v1 v2) ;;before (eq v1 v2)
        frame)))


;; ############################################################################
;; find-intersections
;; ----------------------------------------------------------------------------

(defstruct intersection-tuple left-irl-network right-irl-network intersection trash frame)

;; expand one tuple.
;; 1. throw the first predicate of the left-nw away directle and
;; 2. find all the matches for the first pred of the left-nw in the right nw
(defun expand-intersection-tuple (tuple &key (extension-test #'surjective-function-frame))
  (let ((left (intersection-tuple-left-irl-network tuple))
        (right (intersection-tuple-right-irl-network tuple))
        (frame (intersection-tuple-frame tuple))
        (intersection (intersection-tuple-intersection tuple))
        (trash (intersection-tuple-trash tuple)))
    (when left ;; if left nw is empty, tuple can not be expanded anymore
      
      ;; throw head of left away, it might block others to match
      (cons (make-intersection-tuple :left-irl-network (cdr left)
                                     :right-irl-network right
                                     :intersection intersection
                                     :trash (cons (car left) trash)
                                     :frame frame)
            (loop for match-candidate in right
                  for rest-right = (remove match-candidate right)
                  for new-frame = (find-map-function (car left) match-candidate frame :extension-test extension-test)
                  if new-frame
                  
                  ;; find matches with head of left in right
                  collect (make-intersection-tuple :left-irl-network (cdr left)
                                                   :right-irl-network rest-right
                                                   :intersection (cons match-candidate intersection)
                                                   :trash trash
                                                   :frame new-frame))))))

;; can be more efficiently implemented by replacing breath first by best first algorithm (bit of a hassle though)
(defun find-intersection (left-irl-network right-irl-network &key (extension-test #'surjective-function-frame))
  (let ((finished-tuples nil)
        (tuple-list (list
                     (make-intersection-tuple :left-irl-network left-irl-network
                                              :right-irl-network right-irl-network
                                              :intersection nil
                                              :trash nil
                                              :frame (make-map-frame)))))
    ;; iteratively expand tuples, start with one non expanded tuple
    (loop while tuple-list do

          ;; epxand once.
          (setq tuple-list
                (loop for tuple in tuple-list
                      for expansion = (expand-intersection-tuple tuple :extension-test extension-test)
                      if expansion
                      append expansion
                      else

                      ;; if tuple is maximally expanded throw save it and throw it out of the tuple list
                      do (progn (push tuple finished-tuples) nil))))

    ;; find best tuples
    (when finished-tuples
      (let ((max-i 0))
        (loop for tuple in finished-tuples
              for i = (length (intersection-tuple-intersection tuple))
              if (> i max-i)
              do (setq max-i i))
        (find-all-if #'(lambda (tuple)
                         (eq (length (intersection-tuple-intersection tuple)) max-i)) finished-tuples)))))



;; #####################################################################
;; make-s-expression-with-typed-vars
;; ---------------------------------------------------------------------

(defgeneric make-s-expression-with-typed-vars (thing)
  (:documentation "Renders an irl program or a chunk into an
                   s-expression where bind statements are replaced by
                   typed variables"))

(defmethod make-s-expression-with-typed-vars ((meaning list))
  (loop for x in meaning
     if (eq (car x) 'bind) 
     collect (make-typed-var :name (third x) :type (second x))
     else collect x))

(defmethod make-s-expression-with-typed-vars ((chunk chunk))
  (append (make-s-expression-with-typed-vars (irl-program chunk))
          (loop for open-var in (open-vars chunk)
             collect (make-typed-var :name (car open-var) :type (cdr open-var)))))



;; #####################################################################
;; embedding 
;; ---------------------------------------------------------------------

(defgeneric embedding (sub-program super-program &optional frame)
  (:documentation "finds all map-frames for all possible
                   mappings from sub-program to super-program"))

(defmethod embedding ((meaning list) (chunk chunk) &optional frame)
  (declare (ignore frame))
  (embedding (make-s-expression-with-typed-vars meaning)
             (make-s-expression-with-typed-vars chunk)))

(defmethod embedding ((sub-net null) (super-net list) 
                      &optional (frame (make-map-frame)))
  (list frame))

(defmethod embedding ((sub-net list) (super-net list)
                      &optional (frame (make-map-frame)))
  (loop for elt-super in super-net
     for rest-super = (remove elt-super super-net)
        
     ;; try unify
     for elt-frame = (find-map-function (car sub-net) elt-super frame)
        
     ;; if unifies try rest
     if elt-frame
     append (embedding (cdr sub-net) rest-super elt-frame)))

;; ############################################################################
;; match-chunk
;; ----------------------------------------------------------------------------

(define-event match-chunk-started (chunk chunk) (meaning list))
(define-event match-chunk-finished (matched-chunks list))

(export '(matched-chunk meaning-map-frame))
(defclass matched-chunk (chunk)
  ((meaning-map-frame
    :initarg :meaning-map-frame :initform nil
    :accessor meaning-map-frame
    :documentation "The map frame for the meaning returned by match-chunk")
   (source-chunk
    :initarg :source-chunk :initform nil
    :accessor source-chunk
    :documentation "Pointer to the chunk used in the matching")))

(defun match-chunk (chunk meaning &key (notify t))
  "Match a chunk with a meaning. Returns a list of chunks.
   If no chunks are returned, the meaning does not match the chunk."
  (declare (type chunk chunk)
           (type list meaning))
  (let ((monitors::*monitor-notifications-disabled* (not notify)))
    (notify match-chunk-started chunk meaning)
    (let* ((u-frames (embedding meaning chunk))
           (chunks 
            (loop for frame in u-frames
                  ;; compute new bind statements
                  for new-bind-statements = (substitute-variables
                                             (find-all 'bind meaning :key #'car)
                                             (map-frame-bindings frame))
                  for irl-program =
                  (when
                      ;; check if bind statements are either not existent in the chunk or
                      ;; are "equal" to bind statements present in the chunk
                      (loop
                       with bind-statements-in-chunk = (find-all 'bind
                                                                 (irl-program chunk)
                                                                 :key #'first)
                       for bind-statement in new-bind-statements
                       for bind-statement-in-chunk =
                       (find (third bind-statement) bind-statements-in-chunk :key #'third)
                       always
                       (or (not bind-statement-in-chunk)
                           (eq (fourth bind-statement-in-chunk)
                               (fourth bind-statement))))
                          
                     
                    (append (irl-program chunk)
                            (loop for bind-statement in new-bind-statements
                                  when (not (find
                                             bind-statement
                                             (irl-program chunk)
                                             :test #'equalp))
                                  collect bind-statement)))
                  for open-vars = (loop
                                   with new-bound-vars = (mapcar #'third new-bind-statements)
                                   for open-var in (open-vars chunk)
                                   when (not (member (first open-var) new-bound-vars))
                                   collect open-var)
                  when irl-program
                  collect (make-instance 
                           'matched-chunk
                           :meaning-map-frame frame                                
                           :irl-program irl-program
                           :target-var (target-var chunk)
                           :open-vars open-vars
                           :source-chunk chunk
                           :score (score chunk)))))
      (notify match-chunk-finished chunks)
      (values chunks u-frames))))


;; ############################################################################
;; unify-irl-programs
;; ----------------------------------------------------------------------------


(defun unify-irl-programs (irl-program-1 irl-program-2)
  "checks whether irl-program-1 is a sub-program of irl-program-2 and
   returns the bindings"
  (let ((embedding (embedding irl-program-1 irl-program-2)))
    (when embedding
      (mapcar #'map-frame-bindings embedding))))



;; ############################################################################
;; equivalent-irl-programs?
;; ----------------------------------------------------------------------------

(defgeneric equivalent-irl-programs? (irl-program-1 irl-program-2)
  (:documentation "Checks whether two irl programs are equivalent,
                   that is they contain the same predicates and the
                   variable links between them are the same"))

(defmethod equivalent-irl-programs? ((irl-program-1 list) (irl-program-2 list))
  (and (length= irl-program-1 irl-program-2) ;; efficiency 
       (same-primitives irl-program-1 irl-program-2) ;; efficiency 
       (embedding irl-program-1 irl-program-2)
       (embedding irl-program-2 irl-program-1)))

(defun same-primitives (irl-program-1 irl-program-2)
  (equal-sets irl-program-1 irl-program-2 :test (lambda (x y) (eq (first x) (first y)))))
