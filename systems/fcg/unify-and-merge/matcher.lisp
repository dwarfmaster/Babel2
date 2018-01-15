(in-package :fcg)


;;; General extensible matcher and merger.

;;; This code is an extension to Norvig's unifier code in "Paradigms
;;; of AI, Case studies in CL."

;;; Unification in this book is what is called matching in the FCG
;;; formalism: It looks for variable bindings to make two expressions
;;; equal.

;;; The extensions are:

;;; 1) The matcher (unifier) is extensible by setting a symbol's S
;;; unify-fn property to a function (unify-S x y bindings-list) of three
;;; variables x y and bindings-list. Whenever the unifier is called as
;;; (unify (S ...) y bindings-list) or (unify y (S ...) bindings-list) the
;;; unify-S funcion will be called with variables (S ...) y and
;;; bindings-list.

;;; _ is a wildcard (matches anything):
;; (unify '_ '(hello 2 you)) => (+no-bindings+)
;;; Also the tail of a list:
;; (unify '(a b _) '(a b c d e)) => (+no-bindings+)

;;; The global variable *occurs-check* controls if circular bindings
;;; are allowed (and makes the code slower if turned on).

;;; 2) Instead of returning only one unifier (set of bindings) the unify
;;; function returns a set of unifiers. The more simple unification returning
;;; only one set of bindings is called unify-simple.

;;; 3) A function (fcg-merge pattern source &key (bindings +no-bindings+))
;;; defined is that tries to extend the source such that it unifies with the
;;; pattern. It returns a list of pairs of which the car is the extended source
;;; and the cdr is the result of unifying this extended source with the
;;; pattern.

;; ############################################################################
;; O. Utilities
 

(defun bucket-difference (l1 l2 &key (test #'eql))
  (let ((result (copy-seq l1)))
    (dolist (e2 l2 result)
      (setq result (delete e2 result
                           :count 1
                           :test #'(lambda (e2 e1)
                                     (funcall test e1 e2)))))))

;; ############################################################################
;; I. Basic abstractions

(export '(? +fail+ fail? +no-bindings+ no-bindings))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +fail+ nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun fail? (x)
    "Test whether x is eq to the constant +fail+"
    (eq +fail+ x)))

(defparameter +no-bindings+ '((t . t)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun no-bindings? (x)
    "Test whether x is eq to the constant +no-bindings+"
    (eq +no-bindings+ x)))

;; 'variable' ADT definition

(deftype var ()
  `(and symbol (satisfies variable-p)))

(defun variables-in (x)
  "Returns a list of all variables contained in x. X can be anything, but only
if it is a variable or a tree of which one of the leafs is a varibale something
is returned, i.e. for example the contents of a struct is not checked."
  (cond ((variable-p x) (list x))
	((consp x)
	 (append (variables-in (first x))
		 (variables-in (rest x))))
	(t nil)))

;; 'binding' ADT definition

(deftype binding ()
  `(cons))

(defun make-binding (var val)
  (cons var val))

(defun binding-val (binding)
  "Returns the value of binding (the cdr)"
  (declare (type binding binding))
  (cdr binding))

(defsetf binding-val (binding) (val)
  `(setf (cdr ,binding) ,val))

(defun binding-var (binding)
  "Returns the variable of binding (the car)"
  (declare (type binding binding))
  (car binding))

;; 'bindings' ADT definition

(deftype bindings ()
  `(or (satisfies fail?)
       (satisfies no-bindings?)
       cons))

;; utilities for bindings:

(export '(get-binding lookup safe-extend-bindings extend-bindings assert-binding
          bindings->alist))

(defun get-binding (var bindings)
  "Returns a binding (a cons cell of the form (var . value)) for the variable
   var as specified in bindings or NIL if var is unbound in bindings."
  (declare (type symbol var) (type bindings bindings))
  (assoc var bindings))

(defun lookup (var bindings &optional prev-bindings)
  "Returns the value to which var is bound in bindings or NIL if it is unbound."
  (declare (type symbol var) (type bindings bindings))
  (let* ((binding (get-binding var bindings))
         (binding-val (when binding (binding-val binding))))
    (when binding-val
      (if (or (not (variable-p binding-val))
              (find binding-val prev-bindings))
        binding-val
        (lookup binding-val bindings (cons var prev-bindings))))))


(defun extend-bindings (var val bindings)
  "Adds the binding of var to val to bindings."
  (declare (type bindings bindings))
  #+dbg
  (assert (not (get-binding var bindings)))
  (cons (make-binding var val)
	(if (eq bindings +no-bindings+)
	    nil
	    bindings)))

(defun safe-extend-bindings (var val bindings)
  "Adds the binding of var to val to bindings but checks whether there
is not already a different binding."
  (declare (type symbol var) (type bindings bindings))
  (let ((prev (lookup var bindings)))
    #+dbg
    (assert (or (not prev)
		(eq prev val)))
    (if prev 
	bindings
	(extend-bindings var val bindings))))

(defun assert-binding (var val bindings)
  "Assures that var is bound to val in binding, even if it was bount
to something else (in which case the binding is changed."
  (declare (type symbol var) (type bindings bindings))
  (let ((prev (get-binding var bindings)))
    (if prev 
	(progn (setf (binding-val prev) val)
	       bindings)
	(extend-bindings var val bindings))))

(defun reverse-bindings (bs)
  (declare (type bindings bs))
  (loop for b in bs collect (make-binding (binding-val b) (binding-var b))))

(defun bindings->alist (bs)
  "Does nothing for now because bindings are actually implemented as alist, but
is here in case we would like to change this."
  (declare (type bindings bs))
  bs)

(defun merge-bindings (bs1 bs2 &key cxn-inventory)
  "Merges the bindings bs1 and bs2. If there are conflicting bindings than
+fail+ is returned."
  (declare (type bindings bs1) (type bindings bs2))
  (cond ((or (fail? bs1) (fail? bs2))
	 +fail+)
	((no-bindings? bs1) bs2)
	((no-bindings? bs2) bs1)
	(t (loop for b in bs2 until (fail? bs1) do
		 (setq bs1 (unify-simple (binding-var b) (binding-val b) bs1 :cxn-inventory cxn-inventory)))
	   bs1)))

;; 'bindings-list' ADT definition

(export '(merge-bindings-lists))

(deftype bindings-list ()
  `(or null
       cons))

(defun merge-bindings-lists (bsl1 bsl2 &key cxn-inventory)
  "Returns a bindings-list (a list of bindings) which is the union of all
possible successful pairwise merges of bindings in bsl1 and bsl2"
  (declare (type bindings-list bsl1)
	   (type bindings-list bsl2))
  (when (and bsl1 bsl2)
    (let ((result))
      (dolist (bs1 bsl1 result)
	(dolist (bs2 bsl2)
	  (let ((merge (merge-bindings bs1 bs2 :cxn-inventory cxn-inventory)))
	    (unless (fail? merge)
	      (push merge result))))))))

;; utilities for introducing variables, substituting variables, detecting
;; equalities, etc ...

(export '(equalities? substitute-bindings create-vars
          introduce-variables expression-introduce-variables
          get-equalities))

(defun equalities? (bindings)
  "Returns a list of lists of variables that are equal according to
bindings"
  (declare (type bindings bindings))
  (loop with values = (remove-duplicates (mapcar #'cdr bindings))
     with variables-per-value = (mapcar #'(lambda (v) (list v)) values)
     for binding in bindings
     do (pushnew (binding-var binding)
		 (cdr (assoc (binding-val binding)
                             variables-per-value :test #'eql)))
     finally (return (loop for candidate in variables-per-value
			when (cddr candidate) collect (cdr candidate)))))

(defun substitute-bindings (bindings x)
  "Substitute all variables in x with their binding as specified in bindings."
  (declare (type bindings bindings))
  ;; Remi 20/10: Quick solution to remove illegal bindings... but we need to
  ;;             understand where they come-from
  (setf bindings (loop for binding in bindings
                       when (rest binding) collect binding))
  ;; -------------------------------------------------------
  (labels ((aux (x)
             (cond ((variable-p x)
                    (let ((y (assoc x bindings :test #'eq)))
                      (if (and y (not (eq (cdr y) x))) (aux (cdr y)) x)))
                   ((atom x) x)
                   (t (cons (aux (car x)) (aux (cdr x)))))))
    (cond ((eq bindings +fail+) +fail+)
          ((eq bindings +no-bindings+) x)
          (t (aux x)))))

(defun create-vars (list &optional (bindings +no-bindings+) (rename-variables t))
  "Extend bindings with variables for every element in list."
  (dolist (e list)
    (unless (or (lookup e bindings)
		(and (not rename-variables)
		     (variable-p e)))
      (setq bindings (extend-bindings e (make-var e) bindings))))
  bindings)

(defun introduce-variables (fact &optional bindings (rename-variables t))
"Introduces variables for each but the first element of fact."
  (setq bindings (create-vars (rest fact) bindings rename-variables))
  (values (sublis bindings fact) bindings))

(defun expression-introduce-variables (facts &optional bindings (rename-variables t))
  (values
   (loop for fact in facts 
	 do (multiple-value-setq (fact bindings)
	      (introduce-variables fact bindings rename-variables))
	 collect fact)
   bindings))

(defun get-equalities (bs)
  "Return a list of all equalities, i.e. bindings of variables to other
   variables."
  (declare (type bindings bs))
  (let ((copy (copy-list bs)) result)
    (loop while copy do
	  (let ((val (binding-val (first copy)))
		(var (binding-var (first copy))))
	    (dolist (binding (rest copy))
	      (when (eq val (binding-val binding))
		(setf result (push (cons var (binding-var binding)) result))))
	    (setf copy (delete val (the list copy) :key #'binding-val))))
    result))

(defgeneric rename-variables (object &optional renamings)
  (:documentation "Rename all variables that occur in the object to
   new unique variables and return the result. The renamings used (an
   alist) is returned as second value."))

(defmethod rename-variables ((x list) &optional renamings)
  "Rename all variables that occur in x to new unique variables and return the
   result. The renamings used (an alist) is returned as second value."
  (labels ((recurse (c)
	     (cond ((variable-p c) 
		    (or (assqv (the var c) renamings)
			(let ((new (make-var c)))
			  (setf renamings (push (cons c new) renamings))
			  new)))
		   ((consp c)
		    (cons (recurse (car c)) (recurse (cdr c))))
		   (t c))))
    (values (recurse x) renamings)))

(defgeneric instantiate-variables (object &optional renamings)
  (:documentation "Rename all variables that occur in the object to
   new unique constants and return the result. The renamings used (an
   alist) is returned as second value."))
(defmethod instantiate-variables ((x list) &optional renamings)
  "Instantiate all variables that occur in x to new unique constants
   and return the result. The renamings used (an alist) is returned as
   second value."
  (labels ((recurse (c)
	     (cond ((variable-p c) 
		    (or (assqv (the var c) renamings)
			(let ((new (make-const c)))
			  (setf renamings (push (cons c new) renamings))
			  new)))
		   ((consp c)
		    (cons (recurse (car c)) (recurse (cdr c))))
		   (t c))))
    (values (recurse x) renamings)))

(defgeneric instantiate-expression (e &optional instantiations))

(defmethod instantiate-expression ((e t) &optional instantiations)
  (cond ((variable-p e)
	 (let ((prev (assoc e instantiations)))
	   (if prev 
	       (progn
		 ;; (format t "~%found previous: ~a" prev)
		 (values (cdr prev) instantiations))
	       (let ((new (make-id "ID")))
		 (values new (cons (cons e new) instantiations))))))
	((consp e) (multiple-value-bind (new-car car-instantiations)
		       (instantiate-expression (car e) instantiations)
		     (multiple-value-bind (new-cdr final-instantiations)
			 (instantiate-expression (cdr e) car-instantiations)
		       (values (cons new-car new-cdr)
			       final-instantiations))))
	(t (values e instantiations))))

(defun reverse-renamings (in renamings)
  (values (sublis (loop for (first . second) in renamings
		      collect (cons second first))
		  in)
	  renamings))

;; ############################################################################
;;; II. Checking for circular bindings

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *occurs-check* t))

(defun occurs-check (var x bindings)
  "Check whether the variable var or its value according to bindings
occurs in x."
  (declare (type var var) (type bindings bindings))
  (cond ((eq var x) t)
	((and (variable-p x) (get-binding x bindings))
	 (occurs-check var (lookup x bindings) bindings))
	((consp x) (or (occurs-check var (first x) bindings)
		       (occurs-check var (rest x) bindings)))
	(t nil)))


;; ############################################################################
;;; III. Simple but extensible unify

(export '(unify-variable unify-simple))

(defun unify-variable (var x bindings &key cxn-inventory)
  (declare (type var var) (type bindings bindings))
  (multiple-value-bind (clean-x new-bindings)
      (remove-special-operators x bindings)
    (let ((binding (get-binding var new-bindings)))
      (cond ((fail? new-bindings) +fail+)
	    (binding
	     (unify-simple (binding-val binding) clean-x new-bindings :cxn-inventory cxn-inventory))
	    ((and (variable-p x) (get-binding clean-x new-bindings))
	     (unify-simple var (lookup clean-x bindings) new-bindings :cxn-inventory cxn-inventory))
	    ((and *occurs-check* (occurs-check var clean-x new-bindings))
	     +fail+)
	    (t (extend-bindings var clean-x new-bindings))))))

(defun simple-unify-special (x y bindings &key cxn-inventory)
  (first (funcall (unify-fn (first x)) x y (list bindings) :cxn-inventory cxn-inventory)))

(defun unify-atom (x y bindings &key cxn-inventory)
  (declare (ignore cxn-inventory))
  (cond ((eq bindings +fail+) +fail+)
	;; handle strings and interned symbols
	((equal x y) bindings)
	;; unify uninterned symbols 
	((and (symbolp x) (symbolp y)
	      (equal (symbol-name x) (symbol-name y)) bindings))
	;; unify variables
	((variable-p x) (unify-variable x y bindings))
	((variable-p y) (unify-variable y x bindings))
	((unify-equal x y) bindings)
	(t (values +fail+ x y))))

(defun unify-simple (x y &optional (bindings +no-bindings+) &key cxn-inventory)
  (cond ((and (consp x) (unify-fn (first x)))
	 (simple-unify-special x y bindings :cxn-inventory cxn-inventory))
	((and (consp y) (unify-fn (first y)))
	 (simple-unify-special y x bindings :cxn-inventory cxn-inventory))
	((and (consp x) (consp y))
	 (unify-simple (rest x) (rest y)
		       (unify-simple (first x) (first y) bindings)
                       :cxn-inventory cxn-inventory))
	(t (unify-atom x y bindings :cxn-inventory cxn-inventory))))


;; ############################################################################
;;; IV. Extensible unify returning multiple hypotheses

(export '(unify))

(defun unify-special (x y bindings-list &key cxn-inventory)
  (if (and (consp y) (unify-fn (first y)) (not (eq 'OR (first x))))
      (progn +fail+)
      (funcall (unify-fn (first x)) x y bindings-list :cxn-inventory cxn-inventory)))

(defun unify (x y &optional (bindings-list (list +no-bindings+)) &key cxn-inventory)
  (when bindings-list
    (cond (;; Added by Remi: unify symbol preceded by "!"
           (and (symbolp x) (string= "!" (subseq (symbol-name x) 0 1)))
           (if (or (variable-p y) (null y))
             +fail+
             (unify (make-symbol (subseq (symbol-name x) 1)) y bindings-list :cxn-inventory cxn-inventory)))
          ((and (consp x) (unify-fn (first x)) (not (variable-p y)))
	   (unify-special x y bindings-list :cxn-inventory cxn-inventory))
	  ((and (consp y) (unify-fn (first y)) (not (variable-p x)))
	   (unify-special y x bindings-list :cxn-inventory cxn-inventory))
	  ((and (consp x) (consp y))
	   (loop for bindings in bindings-list append 
                 (unify (rest x) (rest y) 
                        (unify (first x) (first y) (list bindings) :cxn-inventory cxn-inventory)
                        :cxn-inventory cxn-inventory)))
	  (t (loop for bindings in bindings-list
                for try = (unify-atom x y bindings :cxn-inventory cxn-inventory)
                unless (fail? try) 
                collect try)))))

;; ############################################################################
;; V. Merging

(defstruct (merge-result (:conc-name mr-) (:constructor make-mr))
  (expr nil)
  (bsl (list +no-bindings+))
  added
  moved)

(defun make-merge-result (expr bsl &optional (added nil))
  (unless (fail? bsl)
    (make-mr :expr expr :bsl bsl :added added)))

(defmethod make-load-form ((r merge-result) &optional env)
  ;; for avoiding errors when compiling expressions such as
  ;; #S(MERGE-RESULT :EXPR (C B A) :BSL (((T . T))) :ADDED (A))
  (declare (ignore env))
  (make-load-form-saving-slots r))

(export '(fcg-merge remove-special-operators))

(defun merge-special (pattern source bindings cutoff merge-fn &key (remove-special-operators t) cxn-inventory)
  (let ((mrs (funcall (merge-fn (first pattern)) pattern source bindings :cutoff cutoff :merge-fn merge-fn
		      :remove-special-operators remove-special-operators
                      :cxn-inventory cxn-inventory)))
    #+dbg
    (assert (or (not mrs)
		(every #'(lambda (mr) (= (length (mr-added mr))
					 (length (mr-added (first mrs)))))
		       mrs)))
    #+dbg
    (assert (or (not cutoff)
		(not mrs)
		(<= (length (mr-added (first mrs))) cutoff)))
    mrs))

(defun remove-special-operators (e bindings)
  (cond ((atom e) (values e bindings))
	((merge-fn (first e))
	 (if (clean-fn (first e))
	     (funcall (clean-fn (first e)) e bindings)
	     (remove-special-operators (cdr e) bindings)))
	(t
         (multiple-value-bind (new-car new-bindings)
             (remove-special-operators (car e) bindings)
           (multiple-value-bind (new-cdr final-bindings)
               (remove-special-operators (cdr e) new-bindings)
             (values (cons new-car new-cdr) final-bindings))))))

(defun fcg-length (l)
  "Quick hack to solve a problem..."
  (let ((r (rest l)))
    (cond ((null r) 1)
          ((atom r) 2)
          (t
           (1+ (fcg-length (rest l)))))))

(defun dotted-list? (list)
  "Checks whether the list is a dotted list."
  (not (null (rest (last list)))))

(defun make-dotted-list (list &optional (last-cell (make-var 'rest)))
  "Creates a dotted-list: (make-dotted-list '(a b)) -> (a b . ?rest-1)"
  (unless (dotted-list? list)
    (append list last-cell)))

(defun remove-initial-exclamation-marks (lst)
 (let ((symbols (flatten lst))
       (alist nil))
   (dolist (symbol symbols)
     (when (string= "!" (subseq (stringify symbol) 0 1))
       (push (cons symbol (make-symbol (subseq (stringify symbol) 1))) alist)))
   (if alist
     (sublis alist lst)
     lst)))

(defun fcg-merge (pattern source bindings &key (cutoff nil) (destructive t) (merge-fn #'fcg-merge) cxn-inventory)
  "The merger will find the minimal expansion of source so that it
unifies with the given pattern."
  (setf pattern (remove-initial-exclamation-marks pattern))
  (cond ((and (null pattern) (listp source))
	 (list (make-merge-result source
				  (list bindings))))
	((and (null source)
	      (listp pattern))
	 (when (and destructive
		    (or (null cutoff)
			(>= cutoff (length pattern))))
	   ;; when source is empty just return as maximally
	   ;; instantiated pattern without special operators
	   (multiple-value-bind (r new-bindings)
	       (remove-special-operators
                (substitute-bindings bindings pattern) bindings)
	     (list (make-merge-result r
				      (list new-bindings)
				      r)))))
	((or (variable-p pattern) (variable-p source))
	 (let ((bsl (unify pattern source (list bindings) :cxn-inventory cxn-inventory)))
	   (when bsl 
	     (list (make-merge-result source bsl)))))
        ;; Does this case ever occur??
	((and (consp source) (symbolp (first source)) (merge-fn (first source)))
	 (merge-special source pattern bindings cutoff merge-fn :cxn-inventory cxn-inventory))
	((and (consp pattern) (symbolp (first pattern)) (merge-fn (first pattern)))
	 ;; when there is a special operator let them handle it
	 (merge-special pattern source bindings cutoff merge-fn :cxn-inventory cxn-inventory))
	((or (atom pattern) (atom source))
	 (let ((bsl (unify pattern source (list bindings) :cxn-inventory cxn-inventory)))
	   (when bsl 
	     (list (make-merge-result source bsl)))))
	((and (consp pattern) (consp source))
	 (let ((first-tries
		(if destructive 
                  (funcall merge-fn (first pattern) (first source) bindings :cutoff cutoff :cxn-inventory cxn-inventory)
		    (let ((bsl (unify (first source) (first pattern) (list bindings) :cxn-inventory cxn-inventory)))
		      (when bsl (list (make-merge-result (first source) bsl)))))))
	   (cond (first-tries
		  (let ((res nil))
		    (dolist (ft first-tries)
		      (dolist (bs (mr-bsl ft))
			(let ((mrs (funcall merge-fn (rest pattern) (rest source) bs 
					    :cutoff (when cutoff (- cutoff (length (mr-added ft))))
                                            :cxn-inventory cxn-inventory)))
			  (dolist (mr mrs)
			    (setf (mr-expr mr) (cons (mr-expr ft) (mr-expr mr))
				  (mr-added mr) (append (mr-added ft) (mr-added mr)))
			    (let ((prev (find (mr-expr mr) res :key #'mr-expr :test #'equal)))
			      (if prev 
				  (setf (mr-bsl prev) (append (mr-bsl mr)
							      (mr-bsl prev)))
				  (push mr res)))))))
		    res)) ;; This (rest (last pattern)) stuff really
                          ;; has to go (this is the second bug that it
                          ;; has caused)
		 ((and destructive (or (and (rest (last pattern))
                                            (symbolp (rest (last pattern))))                                  
                                       (> (length pattern) (fcg-length source)))
		       (or (not cutoff)
			   (> cutoff 0)))
		  (let ((mrs (funcall merge-fn (rest pattern) source bindings
                                      :cutoff (when cutoff (- cutoff 1))
                                      :cxn-inventory cxn-inventory))
			result)
;;		  (format t "~%mrs=~A" mrs)
		    (dolist (mr mrs)
		      (dolist (bs (mr-bsl mr))
			(multiple-value-bind (first new-bs)
			    (remove-special-operators (substitute-bindings bs (first pattern)) bs)
			  (let ((previous (find-if #'(lambda (mr)
						       (and (consp (mr-expr mr))
							    (equalp first (first (mr-expr mr)))))
						   result)))
			    (if previous 
				(push new-bs (mr-bsl previous))
				(push (make-merge-result (cons first (mr-expr mr)) 
							 (list new-bs)
							 (cons first (mr-added mr)))
				      result))))))
		    result))
		 (t nil))))	
	(t nil)))

(defmacro test-unify (pattern source bsl)
  `(test-assert (permutation-of? (unify ',pattern
					',source)
				 ',bsl
				 :test #'equal)))

