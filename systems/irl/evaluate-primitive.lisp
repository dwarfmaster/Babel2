(in-package :irl)

(export '(evaluate-primitive))

(defun evaluate-primitive (primitive bindings-list &optional (ontology (make-blackboard)))
  "primitive -- primitive-type
   bindings -- list of bindings in the right order of the slots of the primitives
   Returns either a list of list of new bindings, nil or 'inconsistent "
  ;; sanity check: as many bindings as slots
  (assert (= (length bindings-list)
             (length (slot-specs (get-primitive primitive)))))
  (let* ((bindings (if (subtypep (type-of (first bindings-list))
                                 'binding)
                     bindings-list
                     (loop for b in bindings-list
                           collect
                           (make-instance
                            'binding
                            :var (first b)
                            :score (second b)
                            :value (third b)))))
         (primitive-type (get-primitive primitive))
         (bound-slots-pattern (loop for binding in bindings
                                    if (value binding) collect t
                                    else collect nil))
         (applicable-evaluation-spec (find bound-slots-pattern
                                           (evaluation-specs primitive-type)
                                           :key #'evaluation-spec-bound-slots-pattern
                                           :test #'equalp))
         (applicable-slot-spec (loop
                                for slot-spec in (slot-specs primitive-type)
                                for binding in bindings
                                always
                                (or (null (value binding))
                                    (subtypep
                                     (type-of (value binding))
                                     (slot-spec-type slot-spec)))))
         (primitive-is-applicable (and applicable-slot-spec applicable-evaluation-spec))
         (results (when primitive-is-applicable
                    (apply (evaluation-spec-function applicable-evaluation-spec)
                           ontology
                           (mapcar #'value bindings)))))
    ;;  given the bound-slots-pattern there are the following cases
    ;;  no bound-slots-pattern found
    ;;  bound-slots-pattern all t -> t (success), nil (failure)
    ;;  bound-slots-pattern \exists nil ->
    ;;          results - could bind,
    ;;          results == nil - could execute, but not bind
    ;;  errors during application -> not handled
    (if primitive-is-applicable
      (if (null results)
        'inconsistent
        (if (position nil bound-slots-pattern)
          ;; bound slot patterns exist
          (loop for result in results
                collect
                (loop
                 ;; with score = (car result)
                 for binding in bindings
                 for res in result
                 if (value binding) collect binding
                 else collect (make-instance 'binding
                                             :var (var binding)
                                             :score (first res)
                                             :value (second res))))
          ;; only checked the bindings -> return the bindings
          (list bindings)))
      nil)))
