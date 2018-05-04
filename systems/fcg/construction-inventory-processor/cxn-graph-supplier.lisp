(in-package :fcg)

;; #########################################################
;; can-cause
;; ---------------------------------------------------------

(defun variable? (a)
  "Test if a is a symbol starting by ?"
  (if (symbolp a)
      (char= (char (symbol-name a) 0) #\?)
      nil
      )
  )
    
(defun eq-elem (a b)
  "Heuristic that returns t if a and b can be unified"
  (cond ((and (listp a) (listp b))     (eq-list a b)                            )
        ((variable? a)                 t                                        )
        ((variable? b)                 t                                        )
        ((and (symbolp a) (symbolp b)) (string= (symbol-name a) (symbol-name b)))
        ( t                            nil                                      )
  ))

(defun eq-list (a b)
  "Heuristic that returns t if a and b can be unified, where a and b are lists"
  (cond ((and a b)               (and (eq-elem (car a) (car b))
                                      (eq-elem (cdr a) (cdr b))))
        ((and (null a) (null b)) t                              )
        ( t                      nil                            )
        )
  )

(defun imply? (a b)
  "Heuristic that returns t if an element of a can be unified with an element of b"
  (some (lambda (x) (member x b :test #'eq-elem)) a)
  )

(defun get-unit-name (a)
  "Returns the name of a unit"
  (if (listp (car a)) (car (cdr (car a))) (car a))
  )

(defun get-unit-features (a)
  "Returns the features of a unit"
  (cdr a)
  )

(defun unitJ? (a)
  "Tests if a is a J-unit"
  (if (listp (car a)) (eql (car (car a)) 'J) nil)
  )

(defun unitRoot? (a)
  "Tests if a is a ROOT unit"
  (if (consp a) (eql (car a) 'ROOT) nil)
  )

(defun unit-unroot (a hashtable)
  "If the unit is a root one, add to the hashtable a bind from the tag variable to the feature inside"
  (if (and (consp a) (eql (car a) 'ROOT))
      (setf (gethash (car (cdr (car (cdr a)))) hashtable)
            (car (cdr (cdr (car (cdr a)))))
            )
      nil
      )
  )

(defun feature-can-cause (a b)
  "Heuristic that returns t if merging the feature a allows the feature b to match"
  (if (and (listp a) (listp b) (not (unitRoot? b)))
      (let ((aname (symbol-name (car a)))
            (bname (symbol-name (car b))))
        (cond
          ((not (string= aname bname))
               nil)
          ((and (not (listp (car (cdr a)))) (not (listp (car (cdr b)))))
               (imply? (cdr a) (cdr b)))
          ((and (listp (car (cdr a))) (listp (car (cdr b))))
               (let ((aoper (car (car (cdr a))))
                     (boper (car (car (cdr b)))))
                 (cond
                   ((eql boper '==0)        nil)
                   ((not (eql aoper boper)) t)
                   ((eql aoper '==)         (imply? (cdr (car (cdr a)))
                                                    (cdr (car (cdr b)))))
                   ((eql aoper '==p)        (imply? (cdr (car (cdr a)))
                                                    (cdr (car (cdr b)))))
                   ((eql aoper '==1)        (imply? (cdr (car (cdr a)))
                                                    (cdr (car (cdr b)))))
                   (t                       t)
                   )
                 ))
          (t
               t)
          )
        )
      nil
      )
  )

(defun unit-can-cause (a b)
  "Heuristic that returns t if merging the unit a allows the unit b to match"
  (if (unitJ? b)
        nil
        (if (not (eq-elem (get-unit-name a) (get-unit-name b))) nil
            (some (lambda (x) (some (lambda (y) (feature-can-cause x y))
                               (get-unit-features b)))
                  (get-unit-features a))
            )
        )
  )

(defun feature-structure-can-cause (a b)
  "Heuristic that returns t if merging the feature structure a allows the feature structure b to match"
  (some (lambda (x) (some (lambda (y) (unit-can-cause x y)) b)) a)
  )

(defun replace-tag (unit hashtable)
  "If an element of the unit is a key of the hashtable, replace it by the value associated"
  (map 'list (lambda (x) (if (gethash x hashtable) (gethash x hashtable) x)) unit)
  )

(defun feature-structure-unroot (a)
  "Move root features to their respective J-units"
  ;; Create a hash-table binding the tag variable to the feature of the root units
  (defparameter tag-hash-table (make-hash-table))
  (map 'list (lambda (x) (unit-unroot x tag-hash-table)) a)
  ;; Replace tag variable by their content
  (map 'list (lambda (unit) (replace-tag unit tag-hash-table))
      ;; Remove root units
       (remove-if #'unitRoot? a)
       )
  )

(defun feature-structure-root? (a)
  (some #'unitRoot? a)
  )

(defun can-cause (a b dir)
  "Heuristic that returns t if merging the construction a allows the match pole (according to dir) of the construction b to match"
  (consp
    (or (feature-structure-can-cause (feature-structure-unroot (left-pole-structure a))
                                     (pole-structure (match-pole b dir)))
        (feature-structure-can-cause (feature-structure-unroot (right-pole-structure a))
                                     (pole-structure (match-pole b dir)))
        )
    )
  )

;; #########################################################
;; cxn-dependency-graph
;; ---------------------------------------------------------

(defclass cxn-dependency-graph ()
  ((cxn-dependency-graph-constructions
     :type hash-table :initarg :cxns :accessor cxn-graph-cxns
     :documentation "A hashtable binding constructions names to the actual constructions"
     ) 
   (cxn-dependency-graph-edges
     :type hash-table :initarg :edges :accessor cxn-graph-edges
     :documentation "A hashtable bing construction names to the ordered list of construction names it is connected to"
     )
   (cxn-dependency-graph-actives
     :type list :initarg :actives :accessor cxn-graph-act
     :documentation "A list of the active construction names"
     )
  ))

(defun make-cxns-hash-table (cxns)
  "Create a hashtable binding construction names to the actual construction from a list of constructions"
  (defvar cxns-hash-table (make-hash-table))
  (loop for cxn in cxns
     do (setf (gethash (name cxn) cxns-hash-table) cxn)
     )
  cxns-hash-table
  )

(defun insert-in-sorted-list (a l)
  "Insert a string a in a sorted list of string l such that the result is still sorted"
  (if (consp l)
      (if (string<= a (car l))
          (cons a l)
          (cons (car l) (insert-in-sorted-list a (cdr l)))
          )
      (cons a nil)
      )
  )

(defun make-cxns-edges (cxns dir)
  "From a list of constructions, returns a hashtable binding a construction name to the ordered list of construction names it can cause"
  (defparameter cxns-edges (make-hash-table))
  (loop for cxn1 in cxns
     do (loop for cxn2 in cxns
              unless (string= (name cxn1) (name cxn2))
              when   (can-cause cxn1 cxn2 dir)
              do (setf (gethash (name cxn1) cxns-edges)
                       (insert-in-sorted-list (name cxn2)
                                              (gethash (name cxn1) cxns-edges)
                                              )) 
         )
     )
  cxns-edges
  )

(defun feature-neg? (f)
  "Test if a feature uses the ==0 operator"
  (if (consp (car (cdr f)))
      (eql (car (car (cdr f))) '==0)
      nil
      )
  )

(defun unit-start? (a)
  "Test if a unit can be immediately applied (either empty or only negation lock)"
  (every #'feature-neg? (cdr a))
  )

(defun feature-structure-start? (a)
  "Test if a feature structure can be applied from start (all units are J-units or empty or only negations)"
  (every (lambda (u) (or (unitJ? u) (unit-start? u))) a)
  )

(defun make-cxns-actives (cxns dir)
  "List all constructions (by names) that have a root clause in their match pole (according to dir)"
  (sort
    (loop for cxn in cxns
       when (or
               (feature-structure-root?  (pole-structure (match-pole cxn dir)))
               (feature-structure-start? (pole-structure (match-pole cxn dir)))
              )
       collect (name cxn)
       )
    #'string<
    )
  )

(defun make-cxn-dependency-graph (construction-inventory dir)
  "Make a cxn-dependency-graph from a construction inventory"
  (let ((cxns (constructions construction-inventory)))
    (make-instance 'cxn-dependency-graph
                   :cxns    (make-cxns-hash-table cxns    )
                   :edges   (make-cxns-edges      cxns dir)
                   :actives (make-cxns-actives    cxns dir)
                   )
  ))

(defun cxn-dependency-graph->graphviz (graph)
  "Returns a description of the graph in the dot language"
  (setq fstr (make-array '(0) :element-type 'base-char
                              :fill-pointer 0 :adjustable t
                         ))
  (let ((make-id (lambda (s) (map 'string (lambda (c) (if (find c "-." :test #'char=)
                                                          #\_
                                                          c))
                                          (symbol-name s)))))
    (with-output-to-string (s fstr)
        (format s "digraph cxn_graph {~%")
        (loop for cxn-name being the hash-keys of (cxn-graph-cxns graph)
           do (format s "    ~A [label=\"~A\"];~%" (funcall make-id cxn-name)
                                                   (string-downcase cxn-name)
                      )
          )
        (format s "~%")
        (loop for cxn-name in (cxn-graph-act graph)
           do (format s "    ~A [shape = doubleoctagon];" (funcall make-id cxn-name))
          )
        (format s "~%")
        (loop for cxn-name being the hash-keys of (cxn-graph-edges graph)
           do (loop for nxt-name in (gethash cxn-name (cxn-graph-edges graph))
                  do (format s "    ~A -> ~A;~%" (funcall make-id cxn-name)
                                                 (funcall make-id nxt-name))
                 )
          )
        (format s "}~%")
      )
    )
  fstr
  )




