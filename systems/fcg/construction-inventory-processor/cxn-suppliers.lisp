(in-package :fcg)

(export '(constructions-for-application all-constructions-of-label))

(defun constructions-for-application (construction-inventory)
  (if (get-configuration construction-inventory :shuffle-cxns-before-application)
    (shuffle (copy-list (constructions construction-inventory)))
    (constructions construction-inventory)))

;; #########################################################
;; cxn-supplier-with-simple-queue
;; ---------------------------------------------------------

(export '(cxn-supplier-with-simple-queue))

(defclass cxn-supplier-with-simple-queue ()
  ((remaining-constructions 
    :type list :initarg :remaining-constructions :accessor remaining-constructions
    :documentation "A list of constructions that are still to try")))

(defmethod create-gen-cxn-supplier ((cip construction-inventory-processor) (mode (eql :simple-queue)))
  nil)

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :simple-queue)) (gen t))
  (declare (ignore gen))
  (make-instance 'cxn-supplier-with-simple-queue
                 :remaining-constructions
                 (constructions-for-application (construction-inventory (cip node)))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-with-simple-queue) (node cip-node))
  (pop (remaining-constructions cxn-supplier)))

;; #########################################################
;; cxn-supplier-with-ordered-labels
;; ---------------------------------------------------------

(export '(cxn-supplier-with-ordered-labels
          current-label remaining-labels
          all-constructions-of-current-label
          remaining-constructions
          all-constructions-of-label
          all-tried-constructions))

(defclass cxn-supplier-with-ordered-labels ()
  ((current-label 
    :initarg :current-label :accessor current-label
    :documentation "The current label that is tried")
   (remaining-labels
    :type list :initarg :remaining-labels :accessor remaining-labels
    :documentation "All labels that have not been tried yet")
   (all-constructions-of-current-label
    :type list :initarg :all-constructions-of-current-label
    :accessor all-constructions-of-current-label
    :documentation "All constructions that have the current label")
   (remaining-constructions
    :type list :initform nil :accessor remaining-constructions
    :documentation "A sublist of :all-constructions-of-current-label
                    that are still to try"))
  (:documentation "A construction pool that applies constructions of
                   different labels by a pre-specified order"))

(defmethod initialize-instance :after ((pool cxn-supplier-with-ordered-labels) &key)
  (setf (remaining-constructions pool) (all-constructions-of-current-label pool)))

(defun all-constructions-of-label (node label)
  "returns all constructions that of label 'label'"
  (loop for cxn in (constructions-for-application (construction-inventory (cip node)))
        for cxn-label = (attr-val cxn :label)
        when (or (and (symbolp cxn-label) (string= (symbol-name label) (symbol-name cxn-label)))
                 (and (listp cxn-label) (member label cxn-label)))
        collect cxn))

(defun all-tried-constructions (cxn-supplier-with-ordered-labels) 
  "returns all cxns before a particular cxn could apply"
  (set-difference (all-constructions-of-current-label
                   cxn-supplier-with-ordered-labels)
                  (remaining-constructions
                   cxn-supplier-with-ordered-labels)))
  
(require-configuration :production-order)

(require-configuration :parse-order)

(defmethod create-gen-cxn-supplier ((cip construction-inventory-processor) (mode (eql :ordered-by-label)))
  nil)

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :ordered-by-label)) (gen t))
  (declare (ignore gen))
  (let* ((parent (car (all-parents node))))
    (if parent
      ;; copy most of the stuff from the the pool of the parent
      (make-instance 
       'cxn-supplier-with-ordered-labels
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :all-constructions-of-current-label 
       (all-constructions-of-current-label (cxn-supplier parent)))
      ;; there is no parent, start from first label
      (let ((labels (get-configuration (construction-inventory (cip node))
                                       (if (eq (direction (cip node)) '->)
                                         :production-order :parse-order))))
        (make-instance 
         'cxn-supplier-with-ordered-labels
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :all-constructions-of-current-label
         (all-constructions-of-label node (car labels)))))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-with-ordered-labels) (node cip-node))
  (cond ((remaining-constructions cxn-supplier)
         ;; there are remaining constructions. just return the next one
         (pop (remaining-constructions cxn-supplier)))
        ((loop for child in (children node)
               thereis (cxn-applied child))
         ;; when the node already has children where cxn application succeeded,
         ;;  then we don't move to the next label
         nil)
        ((remaining-labels cxn-supplier)
         ;; go to the next label
         (setf (current-label cxn-supplier) (car (remaining-labels cxn-supplier)))
         (setf (remaining-labels cxn-supplier) (cdr (remaining-labels cxn-supplier)))
         (setf (all-constructions-of-current-label cxn-supplier)
               (all-constructions-of-label node (current-label cxn-supplier)))
         (setf (remaining-constructions cxn-supplier)
               (all-constructions-of-current-label cxn-supplier))
         (next-cxn cxn-supplier node))))

(defclass cxn-supplier-ordered-by-label-and-score (cxn-supplier-with-ordered-labels)
  ((current-label 
    :initarg :current-label :accessor current-label
    :documentation "The current label that is tried")
   (remaining-labels
    :type list :initarg :remaining-labels :accessor remaining-labels
    :documentation "All labels that have not been tried yet")
   (all-constructions-of-current-label
    :type list :initarg :all-constructions-of-current-label
    :accessor all-constructions-of-current-label
    :documentation "All constructions that have the current label")
   (remaining-constructions
    :type list :initform nil :accessor remaining-constructions
    :documentation "A sublist of :all-constructions-of-current-label
                    that are still to try"))
  (:documentation "A construction pool that applies constructions of
                   different labels by a pre-specified order"))

(defmethod create-gen-cxn-supplier ((cip construction-inventory-processor) (mode (eql :ordered-by-label-and-score)))
  nil)

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :ordered-by-label-and-score)) (gen t))
  (declare (ignore gen))
  (let* ((parent (car (all-parents node))))
    (if parent
      ;; copy most of the stuff from the the pool of the parent
      (make-instance 
       'cxn-supplier-ordered-by-label-and-score
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :all-constructions-of-current-label (all-constructions-of-current-label (cxn-supplier parent)))
      ;; there is no parent, start from first label
      (let ((labels (get-configuration (construction-inventory (cip node))
                                       (if (eq (direction (cip node)) '->)
                                         :production-order :parse-order))))
        (make-instance 
         'cxn-supplier-ordered-by-label-and-score
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :all-constructions-of-current-label
         (all-constructions-of-label-by-score node (car labels)))))))

(defun all-constructions-of-label-by-score (node label)
  "returns all constructions that of label 'label'"
  (let ((list (copy-object
               (loop for cxn in (constructions-for-application (construction-inventory (cip node)))
                     for cxn-label = (attr-val cxn :label)
                     when (or (and (symbolp cxn-label) (equalp (symbol-name label) (symbol-name cxn-label)))
                              (and (listp cxn-label) (member label cxn-label))) ;;TODO can be in different packages!!!
                     collect cxn))))
    (if (eq (class-name (class-of (first (constructions-for-application (construction-inventory (cip node)))))) 'scored-construction)
      (sort list #'> :key #'score)
      (sort list #'> :key #'(lambda (x) (attr-val x :score))))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-ordered-by-label-and-score) (node cip-node))
  (cond ((remaining-constructions cxn-supplier)
         ;; there are remaining constructions. just return the next one
         (pop (remaining-constructions cxn-supplier)))
        ((loop for child in (children node)
               thereis (cxn-applied child))
         ;; when the node already has children where cxn application succeeded,
         ;;  then we don't move to the next label
         nil)
        ((remaining-labels cxn-supplier)
         ;; go to the next label
         (setf (current-label cxn-supplier) (car (remaining-labels cxn-supplier)))
         (setf (remaining-labels cxn-supplier) (cdr (remaining-labels cxn-supplier)))
         (setf (all-constructions-of-current-label cxn-supplier)
               (all-constructions-of-label-by-score node (current-label cxn-supplier)))
         (setf (remaining-constructions cxn-supplier)
               (all-constructions-of-current-label cxn-supplier))
         (next-cxn cxn-supplier node))))
             


;; #########################################################
;; cxn-supplier-with-scores
;; ---------------------------------------------------------

(export '(cxn-supplier-with-scores))

(defclass cxn-supplier-with-scores ()
  ((remaining-constructions 
    :type list :initarg :remaining-constructions :accessor remaining-constructions
    :documentation "A list of constructions that are still to try")))

(defmethod create-gen-cxn-supplier ((cip construction-inventory-processor) (mode (eql :scores)))
  nil)

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :scores)) (gen t))
  (declare (ignore gen))
  (if (eq (class-name (class-of (first (constructions-for-application (construction-inventory (cip node)))))) 'scored-construction)
    (make-instance 'cxn-supplier-with-scores
                   :remaining-constructions
                   (sort (constructions-for-application (construction-inventory (cip node)))
                         #'> 
                         :key #'score))
    (make-instance 'cxn-supplier-with-scores
                   :remaining-constructions
                   (sort (constructions-for-application (construction-inventory (cip node)))
                         #'> 
                         :key #'(lambda (x) (attr-val x :score))))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-with-scores) (node cip-node))
  (pop (remaining-constructions cxn-supplier)))

;; #########################################################
;; cxn-supplier-with-hashed-simple-queue
;; ---------------------------------------------------------

(export '(cxn-supplier-with-hashed-simple-queue))

(defun constructions-for-application-hashed (node)
  "computes all constructions that could be applied for this node
   plus nil hashed constructions"
  (let ((constructions
         ;; get all constructions compatible
         ;; with the hashes of the node
         ;; append nil hashed constructions
         (remove-duplicates
          (append
           (loop
            for hash in (hash node (get-configuration node :hash-mode))
            append (gethash hash (constructions-hash-table (construction-inventory node))))
           (gethash nil (constructions-hash-table (construction-inventory node)))))))
    ;; shuffle if requested
    (when (get-configuration node :shuffle-cxns-before-application)
      (setq constructions 
            (shuffle constructions)))
    ;; sort if requested
    (when (get-configuration node :sort-cxns-before-application)
      (setq constructions
            (funcall (get-configuration node :sort-cxns-before-application)
                     constructions :node node)))
    ;; return constructions
    constructions))

(defclass cxn-supplier-with-hashed-simple-queue ()
  ((remaining-constructions
    :type list :initarg :remaining-constructions
    :accessor remaining-constructions
    :documentation "A list of constructions that are still to try")))

(defmethod create-gen-cxn-supplier ((cip construction-inventory-processor) (mode (eql :hashed-simple-queue)))
  nil)

(defmethod create-cxn-supplier ((node cip-node)
                                (mode (eql :hashed-simple-queue))
                                (gen t))
  (declare (ignore gen))
  (make-instance
   'cxn-supplier-with-simple-queue
   :remaining-constructions (constructions-for-application-hashed node)))

(defmethod next-cxn ((cxn-supplier cxn-supplier-with-hashed-simple-queue)
                     (node cip-node))
  (pop (remaining-constructions cxn-supplier)))


#|

(defmethod hash ((construction construction)
                 (mode (eql :hash-word-entity-root))
                 &key &allow-other-keys)
  "Returns the string and meaning from the attributes of the construction"
  (when (or (attr-val construction :string)
            (attr-val construction :meaning))
    (list (attr-val construction :string)
          (attr-val construction :meaning))))

(defmethod hash ((node cip-node)
                 (mode (eql :hash-word-entity-root)) ;; For using hashed construction sets in the root.
                 &key &allow-other-keys)
  "Checks the root and returns entities (for IRL meanings) or predicates."
  (let ((transient-structure (car-resulting-cfs (cipn-car node))))
    (if (eq '<- (direction (cip node)))
      (let ((strings (extract-string (get-root (right-pole-structure transient-structure)))))
        (mapcar #'third strings))
      ;; In production return the meanings.
      (loop with meanings = (extract-meaning (get-root (left-pole-structure transient-structure)))
            for m in meanings
            ;; collect the "entity" or "predicate":
            collect (if (and (eq (first m) 'bind)
                             (fourth m))
                      (fourth m)
                      (first m))))))

|#

(defmethod hash ((construction construction)
                 (mode (eql :hash-word-entity-root-one-pole))
                 &key &allow-other-keys)
  "Returns the string and meaning from the attributes of the construction"
  (when (or (attr-val construction :string)
            (attr-val construction :meaning))
    (list (attr-val construction :string)
          (attr-val construction :meaning))))

(defmethod hash ((node cip-node)
                 (mode (eql :hash-word-entity-root-one-pole)) ;; For using hashed construction sets in the root.
                 &key &allow-other-keys)
  "Checks the root and returns entities (for IRL meanings) or predicates."
  (let ((transient-structure (car-resulting-cfs (cipn-car node))))
    (if (eq '<- (direction (cip node)))
      (let ((strings (extract-string (get-root (left-pole-structure transient-structure)))))
        (mapcar #'third strings))
      ;; In production return the meanings.
      (loop with meanings = (extract-meaning (get-root (left-pole-structure transient-structure)))
            for m in meanings
            ;; collect the "entity" or "predicate":
            collect (if (and (eq (first m) 'bind)
                             (fourth m))
                      (fourth m)
                      (first m))))))

;; #########################################################
;; cxn-supplier-with-ordered-labels
;; ---------------------------------------------------------

(export '(cxn-supplier-with-hash+ordered-labels))

(defclass cxn-supplier-with-hash+ordered-labels
          (cxn-supplier-with-ordered-labels)
  ()
  (:documentation
   "A construction pool that applies constructions of
    different labels by a pre-specified order and supports hashing"))

(defun all-constructions-of-label-hashed (node label)
  "returns all constructions of label 'label'"
  (loop for cxn in (constructions-for-application-hashed node)
        for cxn-label = (attr-val cxn :label)
        when (or (eq label cxn-label)
                 (and (listp cxn-label) (member label cxn-label)))
        collect cxn))

(defmethod create-gen-cxn-supplier ((cip construction-inventory-processor) (mode (eql :hashed-ordered-by-label)))
  nil)

(defmethod create-cxn-supplier ((node cip-node) (mode (eql :hashed-ordered-by-label)) (gen t))
  (declare (ignore gen))
  (let* ((parent (car (all-parents node))))
    (if parent
      ;; copy most of the stuff from the the pool of the parent
      (make-instance 
       'cxn-supplier-with-hash+ordered-labels
       :current-label (current-label (cxn-supplier parent))
       :remaining-labels (remaining-labels (cxn-supplier parent))
       :all-constructions-of-current-label 
       (all-constructions-of-current-label (cxn-supplier parent)))
      ;; there is no parent, start from first label
      (let ((labels (get-configuration (construction-inventory (cip node))
                                       (if (eq (direction (cip node)) '->)
                                         :production-order :parse-order))))
        (make-instance 
         'cxn-supplier-with-hash+ordered-labels
         :current-label (car labels)
         :remaining-labels (cdr labels)
         :all-constructions-of-current-label
         (all-constructions-of-label-hashed node (car labels)))))))

(defmethod next-cxn ((cxn-supplier cxn-supplier-with-hash+ordered-labels)
                     (node cip-node))
  (cond ((remaining-constructions cxn-supplier)
         ;; there are remaining constructions. just return the next one
         (pop (remaining-constructions cxn-supplier)))
        ((loop for child in (children node)
               thereis (cxn-applied child))
         ;; when the node already has children where cxn application succeeded,
         ;;  then we don't move to the next label
         nil)
        ((remaining-labels cxn-supplier)
         ;; go to the next label
         (setf (current-label cxn-supplier) (car (remaining-labels cxn-supplier)))
         (setf (remaining-labels cxn-supplier) (cdr (remaining-labels cxn-supplier)))
         (setf (all-constructions-of-current-label cxn-supplier)
               (all-constructions-of-label-hashed node (current-label cxn-supplier)))
         (setf (remaining-constructions cxn-supplier)
               (all-constructions-of-current-label cxn-supplier))
         (next-cxn cxn-supplier node))))
