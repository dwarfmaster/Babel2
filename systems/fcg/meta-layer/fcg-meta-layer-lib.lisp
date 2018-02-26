
(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file contains a library of problem-diagnostic-repair-fix tuples ;;
;; that are often used in Fluid Construction Grammar                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass cxn-fix (fix)
  ()
  (:documentation "A fix class for fixes that apply a construction and return the cxn-application-result"))

(defmethod handle-fix ((fix cxn-fix) (repair repair) (problem problem) (node cip-node) &key &allow-other-keys)
  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
  (call-next-method)
  (push fix (fixes (problem fix))) ;;we add the current fix to the fixes slot of the problem
  (with-disabled-monitor-notifications
    (set-data fix 'fixed-cars
              (fcg-apply (get-processing-cxn (restart-data fix)) (car-resulting-cfs (cipn-car node)) (direction (cip node))))))


;; Unknown Words ;;
;;;;;;;;;;;;;;;;;;;

;; Classes
;; --------

(defclass unknown-words (problem)
  ())

(defclass diagnose-unknown-words (diagnostic)
  ((trigger :initform 'new-node)))

(defclass add-lexical-cxn (repair)
  ((trigger :initform 'new-node)))

;; Methods
;;--------

(defmethod diagnose ((diagnostic diagnose-unknown-words) (node cip-node)
                     &key &allow-other-keys)
  "Diagnose that the fully expanded structure contains untreated strings"
  (when (fully-expanded? node)
    (let ((strings-in-root (get-strings (assoc 'root
                                               (left-pole-structure
                                                (car-resulting-cfs (cipn-car node)))))))
      (when strings-in-root
        (let ((problem (make-instance 'unknown-words)))
          (set-data problem 'strings strings-in-root)
          problem)))))

(defmethod repair ((repair add-lexical-cxn)
                   (problem unknown-words)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new lexical construction for the first untreated string"
  (let ((uw (first (get-data problem 'strings))))
    (multiple-value-bind (cxn-set lex-cxn)
        (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append uw "-cxn")))
                            ((?word-unit
                              (args (?ref))
                              (parent ?parent-unit)
                              (syn-cat (lex-class ?lex-class))
                              (sem-cat (sem-class ?sem-class)))
                             <-
                             (?word-unit
                              (HASH meaning ((,(intern (upcase uw)) ?ref)))
                              --
                              (HASH form ((string ?word-unit ,uw)))))
                            :cxn-inventory ,(copy-object (original-cxn-set (construction-inventory node)))
                            :cxn-set lex))
      (declare (ignore cxn-set))
      (make-instance 'cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data lex-cxn))))
                    


;; Unknown meanings ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Classes
;; --------

(defclass unknown-meaning-predicates (problem)
  ())

(defclass diagnose-unknown-meaning-predicates (diagnostic)
  ((trigger :initform 'new-node)))

;; Methods
;;--------

(defmethod diagnose ((diagnostic diagnose-unknown-meaning-predicates) (node cip-node)
                     &key &allow-other-keys)
  "Diagnose that the fully expanded structure contains untreated meaning predicates"
  (when (fully-expanded? node)
    (let ((meanings-in-root (extract-meaning (assoc 'root
                                               (left-pole-structure
                                                (car-resulting-cfs (cipn-car node)))))))
      (when meanings-in-root
        (let ((problem (make-instance 'unknown-meaning-predicates)))
          (set-data problem 'meaning-predicates meanings-in-root)
          problem)))))

(defmethod repair ((repair add-lexical-cxn)
                   (problem unknown-meaning-predicates)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new lexical construction for the first untreated string"
  (let* ((pred (first (get-data problem 'meaning-predicates)))
         (cxn-name (first pred))
         (ref (make-var (second pred))))
    (multiple-value-bind (cxn-set lex-cxn)
        (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append cxn-name "-cxn")))
                            ((?word-unit
                              (args (,ref))
                              (parent ?parent-unit)
                              (syn-cat (lex-class ?lex-class))
                              (sem-cat (sem-class ?sem-class)))
                             <-
                             (?word-unit
                              (HASH meaning ((,cxn-name ,ref)))
                              --
                              (HASH form ((string ?word-unit ,(make-new-word))))))
                            :cxn-inventory ,(copy-object (original-cxn-set (construction-inventory node)))
                            :cxn-set lex))
      (declare (ignore cxn-set))
      (make-instance 'cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data lex-cxn))))


;; Unconnected meaning ;;
;;;;;;;;;;;;;;;;;;;

;; Classes
;; --------

(defclass unconnected-meaning (problem)
  ())

(defclass diagnose-unconnected-meaning (diagnostic)
  ((trigger :initform 'new-node)))

(defclass add-phrasal-cxn (repair)
  ((trigger :initform 'new-node)))


;; Methods
;;--------

(defmethod diagnose ((diagnostic diagnose-unconnected-meaning) (node cip-node)
                     &key &allow-other-keys)
  "Diagnose that the parsed meaning is unconnected when a solution is found."
  (when (and (eq (direction (cip node)) '<-)
             (fully-expanded? node)) 
    (let ((parsed-meaning
           (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car node))))))
      (unless (connected-semantic-network parsed-meaning)
        (make-instance 'unconnected-meaning)))))

(defmethod repair ((repair add-phrasal-cxn)
                   (problem unconnected-meaning)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new phrasal construction for the unconnected meaning"
  (let ((hierarchy-feature (first (hierarchy-features (construction-inventory node)))))
    (multiple-value-bind (cxn-set phrasal-cxn)
        (eval `(def-fcg-cxn phrasal-cxn
                            ((?parent-unit
                              (args (?ref))
                              (,hierarchy-feature (?lexical-unit ?phrasal-head)))
                             (?phrasal-head
                              (footprints (phrasal-cxn)))
                             <-
                             (?lexical-unit
                              (args (?ref))
                              (parent ?parent-unit)
                              (sem-cat (sem-class ?sem-class-1))
                              --
                              (parent ?parent-unit)
                              (syn-cat (lex-class ?lex-class-1)))
                             (?phrasal-head
                              (footprints (not phrasal-cxn))
                              (parent ?parent-unit)
                              (args (?ref))
                              (sem-cat (sem-class ?sem-class-2))
                              --
                              (footprints (not phrasal-cxn))
                              (parent ?parent-unit)
                              (syn-cat (lex-class ?lex-class-2)))
                             (?parent-unit
                              --
                              (HASH form ((meets ?lexical-unit ?phrasal-head ?parent-unit)))))
                            :disable-automatic-footprints t
                            :cxn-inventory ,(copy-object (original-cxn-set (construction-inventory node)))
                            :cxn-set cxn))
      (declare (ignore cxn-set))
      (make-instance 'cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data phrasal-cxn))))


;; Unknown meanings ;;
;;;;;;;;;;;;;;;;;;;;;;

;; Generalising and specifying constructions
;; -----------------------------------------

(defclass no-match (problem)
  ())

(defclass diagnose-no-match (diagnostic)
  ((trigger :initform 'new-node)))

(defclass anti-unify (repair)
  ((trigger :initform 'new-node)))

(defclass anti-unify-pro-unify (repair)
  ((trigger :initform 'new-node)))

;; Methods
;;--------

(defmethod diagnose ((diagnostic diagnose-no-match) (node cip-node)
                     &key &allow-other-keys)
  "diagnose that meaning network in final transient structure is unconnected"
  (when (and (eq (direction (cip node)) '<-)
             (fully-expanded? node))
    (let ((parsed-meaning
           (extract-meanings (left-pole-structure (car-resulting-cfs (cipn-car node))))))
      (unless (connected-semantic-network parsed-meaning)
        (make-instance 'no-match)))))

(defmethod repair ((repair anti-unify)
                   (problem no-match)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by anti-unifying the non-lexical and non-morphological cxns of the cxn-inventory,
   and applying the anti-unified cxn with the lowest cost to the transient structure (ts)."
  (let* ((ts (car-resulting-cfs (cipn-car node)))
         (cxn-inventory (original-cxn-set (construction-inventory node)))
         (direction (direction (cip node)))
         (a-u-cxns nil)
         (a-u-cxn-lowest-cost nil))
    ;; Anti-unify all non-lexical and non-morphological constructions
    (loop for cxn in (constructions cxn-inventory)
          unless (or (member "MORPH" (listify (rest (assoc :label (attributes cxn)))) :test #'string=)
                     (member "LEX" (listify (rest (assoc :label (attributes cxn)))) :test #'string=)
                     (> (length (matching-pattern (get-processing-cxn cxn) direction)) (+ 1 (length (left-pole-structure ts)))))
          do (multiple-value-bind (a-u-cxn cost)
                 (match-robust (get-processing-cxn cxn) ts direction)
               (push (list a-u-cxn cost) a-u-cxns)))
    ;; Sort anti-unification-results according to cost
    (setf a-u-cxn-lowest-cost (first (first (sort a-u-cxns #'< :key #'second))))
    (let ((anti-unified-fcg-cxn (processing-cxn->fcg-cxn a-u-cxn-lowest-cost :cxn-inv cxn-inventory :include-footprints? t)))
      (add-cxn anti-unified-fcg-cxn (copy-object cxn-inventory))
      (make-instance 'cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data anti-unified-fcg-cxn))))

(defmethod repair ((repair anti-unify-pro-unify)
                   (problem no-match)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by anti-unifying the non-lexical and non-morphological cxns of the cxn-inventory,
   and applying the anti-unified cxn with the lowest cost to the transient structure (ts)."
  (let* ((ts (car-resulting-cfs (cipn-car node)))
         (cxn-inventory (original-cxn-set (construction-inventory node)))
         (direction (direction (cip node)))
         (a-u-cxns nil)
         (a-u-cxn-lowest-cost nil))
    ;; Anti-unify all non-lexical and non-morphological constructions
    (loop for cxn in (constructions cxn-inventory)
          unless (or (member "MORPH" (listify (rest (assoc :label (attributes cxn)))) :test #'string=)
                     (member "LEX" (listify (rest (assoc :label (attributes cxn)))) :test #'string=)
                     (> (length (matching-pattern (get-processing-cxn cxn) direction)) (+ 1 (length (left-pole-structure ts)))))
          do (multiple-value-bind (a-u-cxn cost)
                 (match-robust (get-processing-cxn cxn) ts direction)
               (push (list a-u-cxn cost) a-u-cxns)))
    ;; Sort anti-unification-results according to cost
    (setf a-u-cxn-lowest-cost (first (first (sort a-u-cxns #'< :key #'second))))
    (let* ((pro-unified-cxn (apply-pro-unification a-u-cxn-lowest-cost ts direction))
           (pro-unified-fcg-cxn (processing-cxn->fcg-cxn pro-unified-cxn :cxn-inv cxn-inventory :include-footprints? t)))
      (add-cxn pro-unified-fcg-cxn (copy-object cxn-inventory))
      (make-instance 'cxn-fix
                     :repair repair
                     :problem problem
                     :restart-data pro-unified-fcg-cxn))))




