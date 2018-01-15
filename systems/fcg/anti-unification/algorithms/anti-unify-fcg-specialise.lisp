(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method for anti-unifying FCG constructions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod anti-unify (pattern source (mode (eql :fcg-specialise)) &optional
                               (pattern-bindings +no-bindings+)
                               (source-bindings +no-bindings+)
                               &key (cost-params '((equality 0) ;; don't punish when source is equal to pattern 
                                                   (non-matching-unit 10) ;; Punish badly non-matching units
                                                   (subst-from-bindingslist 0)
                                                   (source-variable-pattern-in-bindingslist 1)
                                                   (replace-by-new-var depth-of-replaced-pattern 1)
                                                   (discarded-feature 5)
                                                   (discarded-negated-feature 4))))
  "anti-unifies an fcg-pattern, including special operators and a source. Returns the resulting
   least general generalisation as well as the binding lists for pattern and source and the cost
   of the anti-unification (calculated based on cost-params)"
  ;; Source should contain more units than pattern, then call helper function, fail otherwise
  (when (<= (length pattern) (length source))
    ;; Get units from source to anti-unify pattern against (ordered)
    (let ((source-unit-reorderings (reorder-source-units pattern source cost-params))
          anti-unification-results)
      (dolist (s-u-r source-unit-reorderings)
        (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-discarded-features a-u-cost)
            (anti-unify-fcg-specialise pattern (first s-u-r) pattern-bindings source-bindings 'unit-level cost-params)
          (let ((total-cost (+ (second s-u-r) a-u-cost)))
            (push (list resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-discarded-features total-cost) anti-unification-results))))
      (sort anti-unification-results '< :key 'fifth))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main recursive anti-unification function for FCG patterns ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anti-unify-fcg-specialise (pattern source pattern-bindings source-bindings level cost-params)
  ;; Case: equality of pattern and source
  (cond
   ((equalp pattern source)
    (values pattern
            pattern-bindings
            source-bindings
            '() ;; discarded features
            (get-anti-unification-cost 'equality cost-params pattern source)))
   ;; Substitution is already in bindingslist
   ((subs-lookup pattern-bindings source-bindings pattern source)
    (values (subs-lookup pattern-bindings source-bindings pattern source)
            pattern-bindings
            source-bindings
            '()
            (get-anti-unification-cost 'subst-from-bindingslist cost-params pattern source)))
   ;; Case: unit level: unit-name can be different
   ((and (equalp level 'unit-level)
         (anti-unify-fcg-sequence-specialise pattern source '() pattern-bindings source-bindings '() 'unit-level cost-params 0)) ;'() is for the accumulator
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-discarded-features resulting-cost)
        (anti-unify-fcg-sequence-specialise pattern source '() pattern-bindings source-bindings '() 'unit-level cost-params 0)
      (values resulting-pattern
              resulting-pattern-bindings
              resulting-source-bindings
              resulting-discarded-features
              resulting-cost)))
   ;; Case: top-feature level (eg: syn-cat, sem-cat, args, subunits,...): no special operator, but still subset;; feature name should be exact
   ((and (equalp level 'top-feature-level)
         (anti-unify-fcg-set-specialise (rest pattern) (rest source) '() pattern-bindings source-bindings '() 'top-feature-level cost-params 0)) ;'() is for the accumulator
    (multiple-value-bind (resulting-pattern-1 resulting-pattern-bindings-1 resulting-source-bindings-1 resulting-discarded-features-1 resulting-cost-1)
        (anti-unify-fcg-specialise (first pattern) (first source) pattern-bindings source-bindings nil cost-params)
      (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-discarded-features resulting-cost)
          (anti-unify-fcg-set-specialise (rest pattern) (rest source) '() resulting-pattern-bindings-1 resulting-source-bindings-1 '() 'top-feature-level cost-params resulting-cost-1)
        (values (append (list resulting-pattern-1) resulting-pattern)
                resulting-pattern-bindings
                resulting-source-bindings
                (if resulting-discarded-features-1
                  (push resulting-discarded-features-1 resulting-discarded-features)
                  resulting-discarded-features)
                resulting-cost))))
   ;; Case: subset with special operator ==1 or ==1
   ((and (listp pattern)
         (or (equalp (first pattern) '==1)
             (equalp (first pattern) '==))
         (anti-unify-fcg-set-specialise (rest pattern) source '() pattern-bindings source-bindings '() nil cost-params 0)) ;'() is for the accumulator
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-discarded-features resulting-cost)
        (anti-unify-fcg-set-specialise (rest pattern) source '() pattern-bindings source-bindings '() nil cost-params 0)
      (values (append (list (first pattern)) resulting-pattern)
              resulting-pattern-bindings
              resulting-source-bindings
              resulting-discarded-features
              resulting-cost)))
   ;; Case: ==0
   ((and (listp pattern)
         (equalp (first pattern) '==0)
         (anti-unify-fcg-excludes-specialise (rest pattern) source '() pattern-bindings source-bindings '() cost-params 0)) ;'() is for the accumulator
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-discarded-features resulting-cost)
        (anti-unify-fcg-excludes-specialise (rest pattern) source '() pattern-bindings source-bindings '() cost-params 0)
      (values (append (list (first pattern)) resulting-pattern)
              resulting-pattern-bindings
              resulting-source-bindings
              resulting-discarded-features
              resulting-cost)))
   ;; Case: pattern and source have same feature-name and arity (number of arguments)
   ;;       anti-unify the arguments, return resulting pattern and all bindings for source and pattern
   ((and (not (variable-p pattern))
         (not (variable-p source))
         (listp pattern)
         (listp source)
         (not (get-so (first pattern)))
         (= (length pattern) (length source))
          ;(equalp (feature-name source) (feature-name pattern)) ;; restricting anti-unification for same feature
         (anti-unify-fcg-sequence-specialise pattern source '() pattern-bindings source-bindings '() nil cost-params 0)) ;'() is for the accumulator
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-discarded-features resulting-cost)
        (anti-unify-fcg-sequence-specialise pattern source '() pattern-bindings source-bindings '() nil cost-params 0)
      (values resulting-pattern
              resulting-pattern-bindings
              resulting-source-bindings
              resulting-discarded-features
              resulting-cost)))
   ;; Source is variable, pattern is already in bindingslist, then return its binding
   ((and (variable-p source)
         (assoc pattern pattern-bindings))
    (values (cdr (assoc pattern pattern-bindings))
            pattern-bindings
            source-bindings
            nil
            (get-anti-unification-cost 'source-variable-pattern-in-bindingslist cost-params pattern source)))
   ;; pattern is not a variable and does not unify with source: return source (specification)
   ((not (variable-p pattern))
      (values source
              pattern-bindings
              source-bindings
              nil
              (get-anti-unification-cost 'replace-by-new-var cost-params pattern source)))
   ;; pattern is bound variable and source is not its binding: return source
   ((and (variable-p pattern)
         (not (variable-p source))
         (source-substitution-of-pattern-p pattern source pattern-bindings source-bindings))
      (values source
              pattern-bindings
              source-bindings
              nil
              (get-anti-unification-cost 'replace-by-new-var cost-params pattern source)))
   ;; Else case: generalise to variable
   (t
    (let ((var (make-var (if (atom pattern)
                           pattern
                           nil))))
      (values var
              (extend-bindings pattern var pattern-bindings)
              (extend-bindings source var source-bindings)
              nil
              (get-anti-unification-cost 'replace-by-new-var cost-params pattern source))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive helper function for anti-unifying sequences ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anti-unify-fcg-sequence-specialise (pattern
                                source
                                accumulator
                                pattern-bindings
                                source-bindings
                                discarded-features
                                level
                                cost-params
                                cost)
  "anti-unify the elements of a feature"
  (let ((new-level))
   (if (equalp level 'unit-level)
    (setf new-level 'top-feature-level)
    (setf new-level nil))
  (cond
   ;; Case: no elements anymore, return accumulator and bindings-lists
   ((and (null pattern) (null source))
    (values accumulator
            pattern-bindings
            source-bindings
            discarded-features
            cost))
   ;; Case: still elements, anti-unify first and then rest, every time with new bindings
   (t
    (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-discarded-features resulting-cost)
        (anti-unify-fcg-specialise (first pattern) (first source)  pattern-bindings source-bindings new-level cost-params)
      (anti-unify-fcg-sequence-specialise (rest pattern)
                               (rest source)
                               (pushend resulting-pattern accumulator)
                               resulting-pattern-bindings
                               resulting-source-bindings
                               (if resulting-discarded-features
                                 (append resulting-discarded-features discarded-features)
                                 discarded-features)
                               level
                               cost-params
                               (+ resulting-cost cost)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive helper function for anti-unifying sets ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anti-unify-fcg-set-specialise (pattern
                           source
                           accumulator
                           pattern-bindings
                           source-bindings
                           discarded-features
                           level
                           cost-params
                           cost)
  "anti-unify the elements of a feature"
  (let ((new-level))
    (when (equalp level 'top-feature-level)
      (setf new-level 'nil))
    (cond
     ;; Case: no elements in pattern anymore, return accumulator and bindings-lists
     ((null pattern)
      (values accumulator
              pattern-bindings
              source-bindings
              discarded-features
              cost))
     ;; Case: first element of pattern has binding with some variable in bindingslist: return binding
     ((assoc (first pattern) pattern-bindings :test 'equalp)
      (anti-unify-fcg-set-specialise (rest pattern)
                          (remove (cdr (assoc
                                        (cdr (assoc (first pattern) pattern-bindings :test 'equalp))
                                        (reverse-bindings source-bindings) :test 'equalp)) source :test 'equalp)
                          (pushend (cdr (assoc (first pattern) pattern-bindings :test 'equalp)) accumulator)
                          pattern-bindings
                          source-bindings
                          discarded-features
                          level
                          cost-params
                          (get-anti-unification-cost 'subst-from-bindingslist cost-params pattern source)))
     ;; first of pattern is an atom that is findable in source: return it
     ((and
       (atom (first pattern))
       (find (first pattern) source :test 'equalp))
      (anti-unify-fcg-set-specialise (rest pattern) (remove pattern source :test 'equalp)
                          (pushend (first pattern) accumulator)
                          pattern-bindings
                          source-bindings
                          discarded-features
                          level
                          cost-params
                          cost))
     ;; Case: tag: continue with third
     ((and
       (listp (first pattern))
       (string= (first (first pattern)) "TAG")
       (find (feature-name (third (first pattern))) source :key 'car :test 'equalp))
      (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-discarded-features resulting-cost)
          (anti-unify-fcg-specialise (third (first pattern))
                          (find (feature-name (third (first pattern))) source :key 'car :test 'equalp)
                          pattern-bindings
                          source-bindings
                          new-level
                          cost-params)
        (anti-unify-fcg-set-specialise (rest pattern)
                            (remove (find (feature-name (third (first pattern))) source :key 'car :test 'equalp) source)
                            (pushend (append (list (first (first pattern)) (second (first pattern))) (list resulting-pattern)) accumulator)
                            resulting-pattern-bindings
                            resulting-source-bindings
                            (if resulting-discarded-features
                              (append resulting-discarded-features discarded-features)
                              discarded-features)
                            level
                            cost-params
                            (+ cost resulting-cost))))
     ;; first of pattern is list of which feature-name is findable in source: anti-unify it return it
     ((and
       (listp (first pattern))
       (find (feature-name (first pattern)) source :key 'car :test 'equalp))
      (multiple-value-bind (resulting-pattern resulting-pattern-bindings resulting-source-bindings resulting-discarded-features resulting-cost)
          (anti-unify-fcg-specialise (first pattern)
                          (find (feature-name (first pattern)) source :key 'car :test 'equalp)
                          pattern-bindings
                          source-bindings
                          new-level
                          cost-params)
        (anti-unify-fcg-set-specialise (rest pattern)
                            (remove (find (feature-name (first pattern)) source :key 'car :test 'equalp) source)
                            (pushend resulting-pattern accumulator)
                            resulting-pattern-bindings
                            resulting-source-bindings
                            (if resulting-discarded-features
                              (append resulting-discarded-features discarded-features)
                              discarded-features)
                            level
                            cost-params
                            (+ cost resulting-cost))))
     ;; Case top-level-feature: feature-name of first of pattern is not found in source but unifies with (feature-name nil), append feature to accumulator and continue processing
     ((and (equalp level 'top-feature-level)
           (unify (first pattern) `(,(feature-name (first pattern)) nil)))
      (anti-unify-fcg-set-specialise (rest pattern)
                          source
                          (pushend (first pattern) accumulator)
                          pattern-bindings
                          source-bindings
                          discarded-features
                          level
                          cost-params
                          cost))
     ;; Case: no matching feature-name: anti-unify-fcg-set-specialise rest + append pattern to descarded features
     (t
      (anti-unify-fcg-set-specialise (rest pattern)
                          source
                          accumulator
                          pattern-bindings
                          source-bindings
                          (push (first pattern) discarded-features)
                          level
                          cost-params
                          (+ cost (get-anti-unification-cost 'discarded-feature cost-params pattern source)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recursive helper function for anti-unifying excluded (NOT or  ==0) featuers ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anti-unify-fcg-excludes-specialise (pattern
                                source
                                accumulator
                                pattern-bindings
                                source-bindings
                                discarded-features
                                cost-params
                                cost)
  "anti-unify the elements of a feature"
  (cond
   ;; Case: no elements in pattern anymore, return accumulator and bindings-lists
   ((null pattern)
    (values accumulator
            pattern-bindings
            source-bindings
            discarded-features
            cost))
   ;; first of pattern is an atom that is not findable in source: return it
   ((and
     (atom (first pattern))
     (not (find (first pattern) source :test 'equalp)))
    (anti-unify-fcg-excludes-specialise (rest pattern) source
                             (pushend (first pattern) accumulator)
                             pattern-bindings
                             source-bindings
                             discarded-features
                             cost-params
                             cost))
   ;; first of pattern is list which is not unifiable in source: return it
   ((and
     (listp (first pattern))
     (not (find (first pattern) source :test 'unify))
     (anti-unify-fcg-excludes-specialise (rest pattern) source
                              (pushend (first pattern) accumulator)
                              pattern-bindings
                              source-bindings
                              discarded-features
                              cost-params
                              cost))
    (anti-unify-fcg-excludes-specialise (rest pattern) source
                             (pushend (first pattern) accumulator)
                             pattern-bindings
                             source-bindings
                             discarded-features
                             cost-params
                             cost))
   ;; Case: non matching feature-name: anti-unify-fcg-set-specialise rest + append pattern to discarded features
   (t
    (anti-unify-fcg-excludes-specialise (rest pattern)
                             source
                             accumulator
                             pattern-bindings
                             source-bindings
                             (push (first pattern) discarded-features)
                             cost-params
                             (+ cost (get-anti-unification-cost 'discarded-negated-feature cost-params pattern source))))))









