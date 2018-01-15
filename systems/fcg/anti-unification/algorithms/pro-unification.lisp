(in-package :fcg)

(defun apply-pro-unification (cxn ts direction)
  "returns new construction which is the result of pro-unification of cxn with ts in direction,"
  (let* ((matching-pattern (matching-pattern cxn direction))
         (source (left-pole-structure ts))
         (couplings (pro-unify matching-pattern source))
         (new-cxn (copy-object cxn)))
    (setf (name new-cxn) (make-id (string-append "pro-unified-" (symbol-name  (name cxn)))))
    (setf (pole-structure (left-pole new-cxn)) (substitute-bindings couplings (left-pole-structure cxn)))
    (setf (pole-structure (right-pole new-cxn)) (substitute-bindings couplings (right-pole-structure cxn)))
    new-cxn))

(defun pro-unify (pattern source &optional (reduced-bindings nil) (renamings nil))
  "if different variables occuring in pattern are under
   matching with source consistently bound to the same value,
   they are replaced with multiple occurences of the same variable.
   returns nil if pattern and source don't unify.
   returns the renamings that were performed in the form of (var-to-be-renamed . var-to-be-renamed-in)
   e.g. (pro-unify '((?a (p ((n ?x) (q ?t))))
                     (?b (p ((m ?y) (f ?g)))))
                   '((unit-1 (p ((n l) (q p))))
                     (unit-2 (p ((m l) (f p))))))
        =>  ((FCG::?G . FCG::?T) (FCG::?Y . FCG::?X))"
  (let ((bindings (reverse (reverse-bindings (first (match-structures pattern source)))))) ;; is only first matching solution enough??
    (loop for (binding . var) in bindings
          do
          (if (assoc binding reduced-bindings :test 'equalp)
            ;; (variable-to-rename . renaming-variable) to renamings
            (push (cons var (cdr (assoc binding reduced-bindings :test 'equalp))) renamings)
            ;; otherwise, push (binding . var) to reduced bindings
            (push (cons binding var) reduced-bindings)))
    (or renamings
        +no-bindings+)))


