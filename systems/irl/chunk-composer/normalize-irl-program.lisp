(in-package :irl)

;; ############################################################################
;; equivalent-irl-programs-normal-form?
;; ----------------------------------------------------------------------------

(defun equivalent-irl-programs-normal-form? (irl-program1 irl-program2)
  "tests if two irl-programs are isomorph
   by comparing their normal forms"
  (and (eq (length irl-program1) (length irl-program2))
       (let ((normalized-irl-program1 (normalize-irl-program irl-program1))
             (normalized-irl-program2 (normalize-irl-program irl-program2)))
         (loop for p1 in normalized-irl-program1
               for p2 in normalized-irl-program2
               always (equal p1 p2)))))

;; (equivalent-irl-program prog1 prog2) <=> (equal (normalize-irl-program prog1) (normalize-irl-program prog2))
(defun normalize-irl-program (irl-program)
  "creates a unique (up to isomorphism) represtentation or an irl-program"
  (when irl-program
    (let ((copy (loop for pred in irl-program
                      collect (copy-list pred))))
      (enumerate-variables (remove-rank (sort-irl-program copy))))))

(defun remove-rank (ranked-irl-program)
  "removes the cluster index at the end of each primitive"
  (loop for pred in ranked-irl-program
        collect (let ((pred-index pred))
                  (loop while (listp pred-index)
                        collect (let ((value (car pred-index)))
                                  (setf pred-index (cdr pred-index))
                                  value)))))

(defun enumerate-variables (sorted-irl-program)
  "replaces variables with numbers"
  (let ((alist nil)
        (i 0))
    (loop for prim in sorted-irl-program
          collect
          (cons
           (car prim)
           (loop for var in (cdr prim)
                 if (variable-p var)
                 collect (let ((j (assq var alist)))
                           (if j
                             (cdr j)
                             (progn
                               (setf alist (cons (cons var (incf i)) alist))
                               i)))
                 else
                 collect var)))))


;; ############################################################################
;; sort function
;; ----------------------------------------------------------------------------

(defun sort-irl-program (irl-program)
  "creates an ordering over the irl-program that is total up to isomorphism
   (i.e if not prim1 < prim2 and not prim2 < prim1 then prim1 and prim2 are isomorph)"
  (let* ((graph (irl-graph-conversion irl-program))
         (clusters (create-initial-clusters graph)))
    
    ;; phase 2 :update clusters
    (loop while (update-clusters clusters))
    
    ;; phase 3: flatten
    (flatten-clusters clusters)))

(defun irl-graph-conversion (irl-program)
  "creates a graph presentation
   graph = (node1, node2, ....)
   node = (prim edge1, edge2, ....)
   edge = (prim . connectivity-number)
   connectivity-number: a unique number representing the way two prims are connected"
  (loop for prim1 in irl-program
        collect (cons
                 prim1
                 (loop for prim2 in irl-program
                       for connectivity-number = (connectivity-number prim1 prim2)
                       if (and connectivity-number (not (eq prim1 prim2)))
                       collect (cons prim2 connectivity-number)))))


;; ############################################################################
;; clustering functions
;; ----------------------------------------------------------------------------

(defun create-initial-clusters (graph)
  "Creates a ordering of clusters. Every cluster contains all primitives with
   the same name. The primitive names specify an ordering over the clusters."
  (let ((clusters
         (sort-by-partial-order
          graph
          #'(lambda (x y) (or
                           (< (symbol->number (car (car x))) (symbol->number (car (car y))))
                           (and (eq 'bind (car (car x)))
                                (eq 'bind (car (car y)))
                                (< (symbol->number (fourth (car x))) (symbol->number (fourth (car y))))))))))
    (enumerate-cluster-nodes clusters)
    (sort-node-edges clusters)
    clusters))
  
(defun update-clusters (clusters)
  "Does one iteration of refining the clusters
   1) For every prim1 it looks at the smallest cluster that contains a prim2
      such that prim2 is connected to prim2
   2) if the smallest connected cluster is equal for both clusters it looks
      at the connection number
   3) otheriwse it looks at the next biggest clusters, etc..."
  (let ((initial-length (length clusters))
        (cluster-pointer nil))
    (loop for cluster in (copy-list clusters)
          do (loop for new-cluster in (sort-by-partial-order cluster #'direct-node-partial-order)
                   if cluster-pointer do
                   (setf (cdr cluster-pointer) (cons new-cluster nil))
                   (setf cluster-pointer (cdr cluster-pointer))
                   else do
                   (setf (car clusters) new-cluster)
                   (setf cluster-pointer clusters)))
    (enumerate-cluster-nodes clusters)
    (sort-node-edges clusters)
    ;; return nil if clusters could not be updated (i.e. done)
    (not (eq (length clusters) initial-length))))

(defun flatten-clusters (clusters)
  "When the ordering algorithm is done, there can be clusters that still
   contain more then one element. Thes elements are isomorph.
   But we can not just pick any random ordering of the undecided clusters.
   Randomly splitting a cluster propagates through the rest of the undicided
   clusters.
   (i.e. if we are left with (((a ?x) (a ?y)) (((b ?x) (b ?y)))) then (a ?x) and (a ?y) are isomorph
         and (b ?x) and (b ?y) are also isomorph. But if we choose to order (a ?x), (a ?y) s.th.
         (a ?x) < (a ?y) we are forced to make (b ?x) < (b ?y))"
  (let ((non-singular-cluster-pointer (find-at-if #'(lambda (x) (> (length x) 1)) clusters)))
    (loop while non-singular-cluster-pointer
          do
          (let ((non-singular-cluster (car non-singular-cluster-pointer))
                (next-pointer (cdr non-singular-cluster-pointer)))
          ;; crate new cluster containing car of orignal cluster
          (setf (cdr non-singular-cluster-pointer)
                (cons (list (car non-singular-cluster))
                      next-pointer))
          ;; reduce original cluster by one
          (setf (car non-singular-cluster-pointer) (cdr non-singular-cluster))

          ;; correct meta info
          (enumerate-cluster-nodes clusters)
          (sort-node-edges clusters)

          ;;propagate consequences
          (update-clusters clusters)
          
          ;; next
          (setf non-singular-cluster-pointer (find-at-if #'(lambda (x) (> (length x) 1)) clusters))))
    (loop for cluster in clusters
          collect (car (car cluster)))))

(defun enumerate-cluster-nodes (clusters)
  "adds the index of the cluster to every primitive, so that we can determine the cluster of every prim
   in constant time. if prim = (get-context ?ctx) is in the first cluster it will set the car s.th.
   prim = (get-context ?ctx . 0).
   If the prim has already an index (from earlier clustering iterartions) it is overwritten"
  (loop for c in clusters
        for i from 0
        do (loop for node in c
                 do (setf (cdr (last (car node))) i)))
  clusters)

(defun sort-node-edges (clusters)
  "sorts the edges of every node such that the edge to the node of the smallest cluster is the first"
  (loop for cluster in clusters
        do (loop for node in cluster
                 do (setf (cdr node) (sort (cdr node) #'< :key #'(lambda (x) (cdr (last (car x))))))))
  clusters)


;; ############################################################################
;; ordering definitions
;; ----------------------------------------------------------------------------

(defun direct-node-partial-order (node1 node2)
  "Defines an ordering over nodes. Presupposes that the edges of the nodes
   are ordered (done by sort-edge-nodes)."
    (or
     (loop for edge1 in (cdr node1)
           for edge2 in (cdr node2)
           thereis (or
                    ;; cluster of edge smaller
                    (< (cdr (last (car edge1)))
                       (cdr (last (car edge2))))
                    ;; connectivity lower
                    (and
                     (= (cdr (last (car edge1)))
                        (cdr (last (car edge2))))
                     (< (cdr edge1) (cdr edge2)))))
     
     ;; edges match, test if they're equal and node1
     ;; contains less edges then node2
     (and
      (loop for edge1 in (cdr node1)
            for edge2 in (cdr node2)
            always (and
                     ;; cluster of edge smaller
                     (= (cdr (last (car edge1)))
                        (cdr (last (car edge2))))
                     ;; connectivity lower
                     (= (cdr edge1) (cdr edge2))))
      (< (length node1) (length node2)))))

(defun connectivity-number (prim1 prim2)
  "Defines a unique number resperesenting the way two primitives are connected.
   (i.e. which slots of prim1 are connected to wich slots of prim2).
   The numbers are garanteed to be unique within one lisp session. 
   It doesn't look at the primitive names. Presupposes that a primitive always has
   the same amount of arguments"
  (let ((alist nil)
        (i 0)
        (nvars1 (if (eq (car prim1) 'bind) 1 (length (cdr prim1))))
        (nvars2 (if (eq (car prim2) 'bind) 1 (length (cdr prim2)))))
    (let ((connection-signature-list
           (append
            (if (eq (car prim1) 'bind)
              (progn
                (setf alist (cons (cons (third prim1) (incf i)) alist))
                (list i))
              (loop for var in (cdr prim1)
                    for j = (assq var alist)
                    if j
                    collect (cdr j)
                    else
                    collect (progn
                              (setf alist (cons (cons var (incf i)) alist))
                              i)))
            (if (eq (car prim2) 'bind)
              (let ((j (assq (third prim2) alist)))
                (if j (list (cdr j))
                  (progn
                    (setf alist (cons (cons (third prim2) (incf i)) alist))
                    (list i))))
              (loop for var in (cdr prim2)
                    for j = (assq var alist)
                    if j
                    collect (cdr j)
                    else
                    collect (progn
                              (setf alist (cons (cons var (incf i)) alist))
                              i))))))
      (if (< (length alist) (+ nvars1 nvars2))
        (list->number connection-signature-list)
        nil))))
  

;; ############################################################################
;; more general helper functions
;; ----------------------------------------------------------------------------

(defun sort-by-partial-order (list predicate)
  "General function that generates ordered clusters. The elements within one cluster
   are not comparable by the order. (E.g. (sort-by-partial-order '(1 2 1 3) #'<) gives
   '((1 1) (2) (3)))"
  (let* ((sorted (sort list predicate))
         (first sorted)
         (second (cdr sorted))
         (result (list (list (car first))))
         (result-current-cluster result))
    (loop while second
          do
          (if (funcall predicate (car first) (car second))
            (progn
              (setf (cdr result-current-cluster) (list (list (car second))))
              (setf result-current-cluster (cdr result-current-cluster)))
            (setf (car result-current-cluster) (cons (car second) (car result-current-cluster))))
          (setf first second)
          (setf second (cdr second)))
    result))

(defun find-at-if (predicate list)
  (let ((pointer list))
    (loop while (and pointer (not (funcall predicate (car pointer))))
          do (setf pointer (cdr pointer)))
    pointer))
          

;; ############################################################################
;; unique numbers for symbols and lists
;; ----------------------------------------------------------------------------

;; find a unique number for symbols (to do lexical oredering on primitive names)
(defparameter *symbol-hash* (make-hash-table))
(defparameter *symbol-number* 0)
(defun symbol->number (symbol)
  "Looks up the unique number representing the symbol. If it doesn't exist yet,
   a new number is generated"
  (let ((value (gethash symbol *symbol-hash*)))
    (if value value
      (setf (gethash symbol *symbol-hash*) (incf *symbol-number*)))))

;; find a unique number for lists (to do lexical oredering on primitive connections)
(defparameter *list-number* 0)
(defparameter *list-tree* (list nil 0))

(defun list->number (list)
  "Looks up the unique number representing the list. If it doesn't exist yet,
   a new number is generated"
  (list->number-aux list *list-tree*))

(defun list->number-aux (list tree)
  (if list
    (let ((node (find (car list) (cdr (cdr tree)) :key #'car)))
      (if node
        (list->number-aux (cdr list) node)
        (let ((value (incf *list-number*)))
          (setf (cddr tree) (cons (list->branch list value) (cddr tree)))
          value)))
    (if (second tree) (second tree) (setf (second tree) (incf *list-number*)))))
       
(defun list->branch (list value)
  (if list
    (if (eq (length list) 1)
      (list (car list) value)
      (list (car list) nil (list->branch (cdr list) value)))
    (list nil value)))


(defun make-list-container ()
  (list nil t))

(defun find-value-for-list (list container)
  (if list
    (let ((node (find (car list) (cdr (cdr container)) :key #'car :test #'equal)))
      (if node
        (find-value-for-list (cdr list) node)
        nil))
    (second container)))

(defun store-value-for-list (list value container)
  (if list
    (let ((node (find (car list) (cdr (cdr container)) :key #'car :test #'equal)))
      (if node
        (store-value-for-list (cdr list) value node)
        (setf (cddr container) (cons (list->branch list value) (cddr container)))))
    (setf (second container) value)))

(defun make-irl-program-container ()
  (make-list-container))

(defun store-value-for-irl-program (irl-program value container)
  (let ((nf (normalize-irl-program irl-program)))
    (store-value-for-list nf value container)))

(defun store-value-for-chunk (chunk value container)
  (let ((nf (if (normalized-irl-program chunk) 
                (normalized-irl-program chunk)
                (setf (normalized-irl-program chunk)
                      (normalize-irl-program (irl-program chunk))))))
    (store-value-for-list nf value container)))

(defun find-value-for-chunk (chunk container)
  (let ((nf (if (normalized-irl-program chunk) 
                (normalized-irl-program chunk)
                (setf (normalized-irl-program chunk)
                      (normalize-irl-program (irl-program chunk))))))
    (find-value-for-list nf container)))
  
(defun find-value-for-irl-program (irl-program container)
  (let ((nf (normalize-irl-program irl-program)))
    (find-value-for-list nf container)))

;; further optimized
;; commented out, because for the test networks it doesn't give much of a speed boost
;; and since it's more complicated it's better not to use it.
;; (for more complex networks it is much faster)
;;(defun equivalent-irl-programs-normal-form? (irl-program1 irl-program2)
;;  (and
;;   ;; test 1: length ok?
;;   (eq (length irl-program1) (length irl-program2))
;;
;;   ;; match programs
;;   (let* ((copy1 (loop for pred in irl-program1
;;                       collect (copy-list pred)))
;;          (copy2 (loop for pred in irl-program2
;;                       collect (copy-list pred)))
;;          (graph1 (irl-graph-conversion copy1))
;;          (graph2 (irl-graph-conversion copy2))
;;          (clusters1 (create-initial-clusters graph1))
;;          (clusters2 (create-initial-clusters graph2)))
;;     (and
;;      ;; test 2: initial clusters ok?
;;      (eq (length clusters1) (length clusters2))
;;      
;;      ;; run paralel clustering
;;      (progn
;;        (loop while (and
;;                     (update-clusters clusters1)
;;                     (update-clusters clusters2)
;;                     ;; break as soon as irl-networks mismatch
;;                     (eq (length clusters1) (length clusters2))))
;;        
;;        ;; test 3: same amount of clusters?
;;        (eq (length clusters1) (length clusters2)))
;;    
;;      ;; test 4: normalized structures identical?
;;      (loop for p1 in (enumerate-variables (remove-rank (flatten-clusters clusters1)))
;;            for p2 in (enumerate-variables (remove-rank (flatten-clusters clusters2)))
;;            always (equal p1 p2))))))





