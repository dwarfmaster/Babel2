(in-package :fcg)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to convert processing-cxns to fcg-cxns ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO does not handle negation except for footprints
;;      This should be trivial, I (Paul) think it's just replacing
;;       ==0 by not...


;; You should have created an fcg-construction-set with the correct feature types already!!

(defun processing-cxn->fcg-cxn (p-cxn &key cxn-inv (include-footprints? nil))
  "translate processing-cxn into fcg-cxn"
  (let* ((cxn-name (name p-cxn))
        (cxn-inventory (or cxn-inv (get-original-cxn-inventory p-cxn)))
        (feature-types (feature-types cxn-inventory))
        (attributes (attributes p-cxn))
        (left-pole (left-pole-structure p-cxn))
        (tags-and-vars-left-pole nil)
        (right-pole (right-pole-structure p-cxn))
        (tags-and-vars-right-pole nil)
        (contributing-part-hash (make-hash-table))
        (conditional-part-hash (make-hash-table))
        (contributing-part-units nil)
        (conditional-part-units nil))
    
    ;; Make lists of tags and their variables
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; --> For left-pole
    (dolist (unit left-pole)
      (dolist (feature (rest unit))
        (when (and (listp feature) (string= (first feature) 'tag))
          (push (list (second feature) (third feature)) tags-and-vars-left-pole))))

    ;; --> For right-pole
    (dolist (unit right-pole)
      (dolist (feature (rest unit))
        (when (and (listp feature) (string= (first feature) 'tag))
          (push (list (second feature) (third feature)) tags-and-vars-right-pole))))
    
    ;; Add non-j-units to conditional-part-hash and j-units to contributing-part-hash
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; --> For left-pole
    (dolist (unit left-pole)
      (if (not (j-unit-p unit))

        ;; For non-j-units on left-pole
        (dolist (feature (rest unit))
          (unless (string= (first feature) 'tag) ;; Don't copy tagged features
            (let ((converted-feature (processing-feature->fcg-feature feature feature-types include-footprints?)))
              (when converted-feature
                (push (cons 'production-lock converted-feature) (gethash (unit-name unit) conditional-part-hash))))))

        ;; For j-units on left-pole
        (dolist (feature (rest unit))
          ;; For variables found in tag list, push tagged feature to the hash table for the conditional part
          (if (find feature tags-and-vars-left-pole :test #'equal-car)
            (push (cons 'production-lock (convert-tag-feature
                                          feature
                                          tags-and-vars-left-pole
                                          feature-types
                                          include-footprints?))
                  (gethash (second (first unit)) conditional-part-hash))
            ;; For normal features, convert and add to hash table for the contributing part
            (let ((converted-feature (processing-feature->fcg-feature feature feature-types include-footprints?)))
              (when converted-feature
                (pushnew converted-feature (gethash (second (first unit)) contributing-part-hash) :test 'equal)))))))

    ;; --> For right pole
    (dolist (unit right-pole)
      (if (not (j-unit-p unit))

      ;; For non-j-units on right pole
      (dolist (feature (rest unit))
        (unless (string= (first feature) 'tag) ;; Don't copy tagged features
          (let ((converted-feature (processing-feature->fcg-feature feature feature-types include-footprints?)))
            (when converted-feature
              (push (cons 'comprehension-lock converted-feature) (gethash (unit-name unit) conditional-part-hash))))))

      ;; For j-units on right pole
      (dolist (feature (rest unit))
        ;; For variables found in tag list, push tagged feature to the hash table for the conditional part
        (if (find feature tags-and-vars-right-pole :test #'equal-car)
          (push (cons 'comprehension-lock (convert-tag-feature
                                           feature
                                           tags-and-vars-right-pole
                                           feature-types
                                           include-footprints?))
                (gethash (second (first unit)) conditional-part-hash))
          ;; For normal features, convert and add to hash table for the contributing part
          (let ((converted-feature (processing-feature->fcg-feature feature feature-types include-footprints?)))
            (when converted-feature
              (pushnew converted-feature (gethash (second (first unit)) contributing-part-hash) :test 'equal)))))))
    
    ;; Create units for contributing part
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (loop for unit-name being the hash-keys of contributing-part-hash
          do
          (let ((unit-features (gethash unit-name contributing-part-hash))
                (unit nil))
            (dolist (feature unit-features)
              ;; If feature is already in contributing part: append values and push to unit otherwise push to unit
              ;; This is mainly to handle footprints which were on different poles in the processing-cxn
              (let ((feature-same-fname (find (feature-name feature) unit :test 'string= :key 'first)))
                (if feature-same-fname
                  (nsubstitute ;; is nsubstitute safe? we count on the side-effects here, but it seems to work...
                   (cons (feature-name feature)
                         (list (append (second feature) (second feature-same-fname))))
                   feature-same-fname unit)
                  (push feature unit))))
            ;; Make contributing-unit
            (push (make-instance 'contributing-unit
                           :name unit-name
                           :unit-structure unit)
                  contributing-part-units)))

    ;; Create units for conditional part
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (loop for unit-name being the hash-keys of conditional-part-hash
          do
          (let ((unit-features (gethash unit-name conditional-part-hash))
                (comprehension-lock nil)
                (formulation-lock nil))
            (dolist (feature unit-features)
              (if (string= (first feature) 'production-lock)
                (push (rest feature) formulation-lock)
                (push (rest feature) comprehension-lock)))
            ;; Make conditional-unit
            (push (make-instance 'conditional-unit
                                  :name unit-name
                                  :comprehension-lock comprehension-lock
                                  :formulation-lock formulation-lock)
                  conditional-part-units)))
    
    ;; Create FCG-Construction
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ;; And we're DONE ! I hope you enjoyed the tour,
    ;;                  please don't forget to tip the tourguide
    (make-instance 'fcg-construction
                   :name cxn-name
                   :attributes attributes
                   :contributing-part contributing-part-units
                   :conditional-part conditional-part-units
                   :cxn-inventory cxn-inventory
                   :feature-types feature-types)))


(defun processing-feature->fcg-feature (feature feature-types include-footprints?)
  "Translates a processing feature, with special operators to
   an FCG feature"
  (cond ((string= (feature-name feature) 'footprints)
         (if include-footprints?
           (if (string= (first (cadr feature)) '==0)
             (cons 'footprints (list (substitute 'NOT '==0 (cadr feature) :test 'string=)))
             (cons 'footprints (list (remove '== (cadr feature) :test 'string=))))
           nil))
        (t
         (convert-feature-to-fcg feature feature-types))))

(defun convert-set-of-predicates (feature)
  (let ((feature-to-return nil))
    (dolist (predicate (reverse (second feature)))
      (unless (and (atom predicate) (or (string= '== predicate) (string= '==1 predicate)))
        (push predicate feature-to-return)))
    (list (feature-name feature) feature-to-return)))

(defun convert-feature-to-fcg (feature feature-types)
  (cond
   ;; For set-of-predicates: remove operator
   ((string= (second (assoc (feature-name feature) feature-types)) 'set-of-predicates)
    (convert-set-of-predicates feature))
   ;; For sets
   ((string= (second (assoc (feature-name feature) feature-types)) 'set)
    (cons (feature-name feature) (list (rest (second feature)))))
   ;; for other feature types
   (t
    (let ((feature-to-return nil))
      (cond ((atom feature)
             (push feature feature-to-return))
            (t
             (push (first feature) feature-to-return)
             (if (not (and (equal (length (cdr feature)) 1) (atom (cadr feature))))
               (progn
                 (dolist (element (cdr feature))
                   (cond
                    ((atom element)
                     (push element feature-to-return))
                    ((not (or (equal (first element) '==1) (equal (first element) '==)))
                     (push (convert-feature-to-fcg element feature-types) feature-to-return))
                    (t
                     (dolist (el2 (cdr element))
                       (push (convert-feature-to-fcg el2 feature-types) feature-to-return))))))
               (push (cadr feature) feature-to-return))
             (reverse feature-to-return)))))))

(defun convert-tag-feature (tag-name tag-list feature-types include-footprints?)
  (let ((tag-feature nil))
    (dolist (tag tag-list)
      (when (equalp (car tag) tag-name)
        (setf tag-feature (cons 'HASH (processing-feature->fcg-feature (second tag) feature-types include-footprints?)))))
    tag-feature))

(defun equal-car (item list)
  (let ((return-value nil))
    (when (equalp item (car list))
      (setf return-value t))
    return-value))


