(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;
;; Anti-unification ;;
;;;;;;;;;;;;;;;;;;;;;;

(defgeneric anti-unify (pattern source mode  &optional pattern-bindings source-bindings &key cost-params)
  (:documentation "Anti-unifies pattern with source, according to mode, optionally providing bindings-lists
    and parameters for cost calculation"))

;;;;;;;;;;;;;;;;;;;;;
;; Base Algorithms ;;
;;;;;;;;;;;;;;;;;;;;;

(defmethod anti-unify (pattern source (mode (eql :equal-feature-name-and-arity))
                       &optional (pattern-bindings +no-bindings+) (source-bindings +no-bindings+)
                       &key (cost-params nil))
  "anti-unifies (= searches the least general generalisation (lgg) for pattern and source)
   returns as values: resulting-pattern, bindings for pattern, bindings for source.
   Feature name and arity (number of arguments) should be equal, otherwise the whole feature gets replace by a
   variable (it's easy to adapt, what do we want?)"
  (declare (ignore cost-params))
  (cond
   ;; Case: Pattern equals Source, return pattern and all bindings for both pattern and source
   ((equalp pattern source)
    (values pattern
            pattern-bindings
            source-bindings))
   ;; Case: pattern and source are already substitued by the same binding
   ;;       Return this binding and all bindings for pattern and source
   ((subs-lookup pattern-bindings source-bindings pattern source)
    (values (subs-lookup pattern-bindings source-bindings pattern source)
            pattern-bindings
            source-bindings))
   ;; Case: pattern and source have same feature-name and arity (number of arguments)
   ;;       anti-unify the arguments, return resulting pattern and all bindings for source and pattern
   ((and (not (variable-p pattern))
         (not (variable-p source))
         (listp pattern)
         (listp source)
         (= (length pattern) (length source))
         (equalp (feature-name source) (feature-name pattern)) ;; restricting anti-unification for same feature
         (anti-unify-sequence pattern source :equal-feature-name-and-arity '() pattern-bindings source-bindings)) ;'() is for the accumulator
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings)
        (anti-unify-sequence pattern source mode '() pattern-bindings source-bindings)
      (values resulting-pattern
              resulting-pattern-bindings
              resulting-source-bindings)))
   ;; Case: non of the above: replace both by a new binding
   (t
    (let ((var (make-var)))
      (values var
              (extend-bindings pattern var pattern-bindings)
              (extend-bindings source var source-bindings))))))

(defmethod anti-unify (pattern source (mode (eql :no-restrictions))
                       &optional (pattern-bindings +no-bindings+) (source-bindings +no-bindings+)
                       &key (cost-params nil))
  "anti-unifies (= searches the least general generalisation (lgg) for pattern and source)
   returns as values: resulting-pattern, bindings for pattern, bindings for source
 Feature name should not be equal, arity should."
  (declare (ignore cost-params))
  (cond
   ;; Case: Pattern equals Source, return pattern and all bindings for both pattern and source
   ((equalp pattern source)
    (values pattern
            pattern-bindings
            source-bindings))
   ;; Case: pattern and source are already substitued by the same binding
   ;;       Return this binding and all bindings for pattern and source
   ((subs-lookup pattern-bindings source-bindings pattern source)
    (values (subs-lookup pattern-bindings source-bindings pattern source)
            pattern-bindings
            source-bindings))
   ;; Case: pattern and source have same feature-name and arity (number of arguments)
   ;;       anti-unify the arguments, return resulting pattern and all bindings for source and pattern
   ((and (not (variable-p pattern))
         (not (variable-p source))
         (listp pattern)
         (listp source)
         (= (length pattern) (length source))
         (anti-unify-sequence pattern source :equal-feature-name-and-arity '() pattern-bindings source-bindings)) ;'() is for the accumulator
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings)
        (anti-unify-sequence pattern source mode '() pattern-bindings source-bindings)
      (values resulting-pattern
              resulting-pattern-bindings
              resulting-source-bindings)))
   ;; Case: non of the above: replace both by a new binding
   (t
    (let ((var (make-var)))
      (values var
              (extend-bindings pattern var pattern-bindings)
              (extend-bindings source var source-bindings))))))

(defun anti-unify-sequence (pattern
                            source
                            mode
                            accumulator
                            pattern-bindings
                            source-bindings)
  "anti-unify the elements of a feature"
  (cond
   ;; Case: no elements anymore, return accumulator and bindings-lists
   ((and (null pattern) (null source))
    (values accumulator
            pattern-bindings
            source-bindings))
   ;; Case: still elements, anti-unify first and then rest, every time with new bindings
   (t
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings)
        (anti-unify (first pattern) (first source) mode pattern-bindings source-bindings)
      (anti-unify-sequence (rest pattern)
                           (rest source)
                           mode
                           (pushend resulting-pattern accumulator)
                           resulting-pattern-bindings
                           resulting-source-bindings)))))



