(in-package :fcg)

;; After some helper functions and macros, this file contains functions for 
;; converting from FCG Light constructions to FCG 2 constructions.
;; Structure:
;;     1. Main Functions
;;     2. Unit and feature conversion
;;     3. Helper functions

;; Remi: Helper functions and macro to simplify/shorten the function fcg-light-cxn->fcg-2-cxn.
;; These macros are based on original code from the functions by Paul and avoid
;; code duplication.
;; ---------------------------------------------------------------------------------------------
(defun cons-or-extend-fcg-tag (tag prior-tags)
  "Pushes a new tag or merges the tag with a previous one in the list of tags."
  (let* ((tagged-feature-name (tag-feature-name tag))
         (prior-tag (find tagged-feature-name prior-tags :key #'tag-feature-name)))
    (if prior-tag
      (substitute (append tag (rest prior-tag))
                  prior-tag prior-tags :test #'equal)
      (cons tag prior-tags))))

(defun fcg-handle-tags-for-root (root-features units)
  "Handles tags in the ROOT unit."
  (let ((old-root (get-root units)))
    (if old-root
      (let ((extended-root (root-append old-root root-features))) ;; TODO: if feature exists, tag it!!
        (substitute extended-root old-root units :test #'equal))
      (cons (cons 'root root-features) units))))

(defun fcg-place-repeat-tag-in-j-units (tags-for-j-units j-units)
  "Put the repeat tags in the correct j-units or add a new j-unit."
  (dolist (tag tags-for-j-units j-units)
    (let ((unit (find (first tag) j-units :key #'j-unit-name)))
      (if unit
        (push (second tag) (rest unit)) ;; Add the tag.
        (push `((J ,(first tag))
                ,@(rest tag)) j-units))))) ;; Create new unit.

(defun fcg-process-footprints (units j-units sem-or-syn cxn-name)
  "Adds footprints to J-units."
  (dolist (unit units j-units) ;; Return the j-units at the end.
     (unless (eql (unit-name unit) 'root) ;; no footprints in root
       ;; search for corresponding j-unit
       (let ((j-unit (find (unit-name unit) j-units :key #'j-unit-name)))
         (if j-unit
           ;; If a corresponding j-unit is found
           (let ((existing-footprints (find-footprints j-unit))) ;; find footprints already present
             (if existing-footprints
               (pushend (make-symbol (string-append sem-or-syn '- cxn-name)) (second existing-footprints))
               ;; otherwise create new footprints feature
               (push `(footprints (==1 ,(make-symbol (string-append sem-or-syn '- cxn-name)))) (cdr j-unit))))
           ;; If no corresponding j-unit in the cxn, create one with the footprints feature
           (push (cons `(j ,(first unit)) `((footprints (==1 ,(make-symbol (string-append sem-or-syn '- cxn-name))))))  j-units))))))

(defmacro fcg-convert-lock->fcg-2-unit (cxn feature-types fcg-unit units tags-for-j-units tags-for-root sem-or-syn &key lock-fn)
  ;; This macro is mainly used for its side effects: it setfs the value of the macro-expansion of units, tags-for-j-units and
  ;; tags-for-root. Do not use this macro outside of the translation function.
  `(let ((unit-name (name ,fcg-unit))
         (lock (funcall ,lock-fn ,fcg-unit)))
     (multiple-value-bind (unit add-to-j-units add-to-root)
       (convert-unit-to-fcg-2 (cons unit-name lock) (name ,cxn) ,cxn ,sem-or-syn :feature-types ,feature-types :j-unit nil)
       ;; units with only hashed features do not occur in the processing-cxn
       (when unit
         (push unit ,units))
       ;; Store variables that are tagged for j-unit
       (when add-to-j-units
         (push add-to-j-units ,tags-for-j-units))
       ;; Store tagged features for root
       (dolist (tag add-to-root)
         (setf ,tags-for-root (cons-or-extend-fcg-tag tag ,tags-for-root))))))

;;;;;;;;;;;;;;;;;;;;;;;
;; 1. Main Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;

(export '(>>))

(defun fcg-light-cxn->fcg-2-cxn (cxn &key (feature-types (or (feature-types cxn)
                                                             (feature-types *fcg-constructions*)
                                                             '((args sequence)
                                                               (form set-of-predicates)
                                                               (meaning set-of-predicates))))
                                     processing-cxn-inventory)
  "takes an FCG-cxn as input, and returns a processing-cxn"
  (let ((cxn-name (name cxn))
        (contributing-part (contributing-part cxn))
        (conditional-part (conditional-part cxn))
        (sem-pole-units nil)
        (syn-pole-units nil)
        (j-units nil)
        (j-units-syn-pole nil)
        (tags-for-j-units-sem-pole nil)
        (tags-for-j-units-syn-pole nil)
        (tags-for-root-sem-pole nil)
        (tags-for-root-syn-pole nil)
        (attributes (attributes cxn)))
    
    ;; Process conditional units ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Loop through conditional units of cxn
    (dolist (conditional-unit conditional-part)
        ;; --> Convert formulation-lock
        ;;     Get: unit for sem-pole, tagged variables for j-unit and tagged features for root
        (fcg-convert-lock->fcg-2-unit cxn feature-types conditional-unit
                                      sem-pole-units tags-for-j-units-sem-pole tags-for-root-sem-pole 'sem
                                      :lock-fn #'formulation-lock)
        ;; --> Convert comprehension-lock
        ;;     Get: unit for sem-pole, tagged variables for j-unit and tagged features for root
        (fcg-convert-lock->fcg-2-unit cxn feature-types conditional-unit
                                      syn-pole-units tags-for-j-units-syn-pole tags-for-root-syn-pole 'syn
                                      :lock-fn #'comprehension-lock))
    
    ;; Process contributing-units ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; Loop through contributing units of cxn
    (dolist (contributing-unit contributing-part)
      (let ((unit-name (name contributing-unit))
            (fs (unit-structure contributing-unit)))
        ;; Store converted-units
        (push (convert-unit-to-fcg-2 (cons unit-name fs) cxn-name cxn 'j :feature-types feature-types :j-unit t) j-units)))

    ;; Process Hashed (Tagged) features ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; --> add hashed features of formulation lock as tagged features to root of sem-pole
    (when tags-for-root-sem-pole
      (setf sem-pole-units (fcg-handle-tags-for-root tags-for-root-sem-pole sem-pole-units)))
    ;; --> add hashed features of comprehension lock as tagged features to root of syn-pole
    (when tags-for-root-syn-pole
      (setf syn-pole-units (fcg-handle-tags-for-root tags-for-root-syn-pole syn-pole-units)))
    ;; --> Add variables of tags for 'pasting' in j-units of sem-pole and syn-pole
    (setf j-units (fcg-place-repeat-tag-in-j-units tags-for-j-units-sem-pole j-units)
          j-units-syn-pole (fcg-place-repeat-tag-in-j-units tags-for-j-units-syn-pole j-units-syn-pole))

    ;; Process Footprints ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Skip if disable-automatic-footprints is true for either the construction or the construction-inventory
    (unless (or (disable-automatic-footprints (cxn-inventory cxn)) (disable-automatic-footprints cxn))
      ;; --> For units in sem-pole and syn-pole
      (setf j-units (fcg-process-footprints sem-pole-units j-units 'sem cxn-name)
            ;; --> For units in syn-pole
            j-units-syn-pole (fcg-process-footprints syn-pole-units j-units-syn-pole 'syn cxn-name)))
    
    ;; Create the processing construction ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (let ((sem-pole (append (reverse sem-pole-units) j-units))
          (syn-pole (append (reverse syn-pole-units) j-units-syn-pole)))
      (make-instance 'processing-construction
                     :name cxn-name
                     :domain 'sem
                     :attributes attributes
                     :left-pole (or sem-pole '(()))
                     :right-pole (or syn-pole '(()))
                     :cxn-inventory processing-cxn-inventory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2. Unit and feature conversion ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun convert-unit-to-fcg-2 (unit-fs cxn-name cxn pole &key feature-types j-unit)
  "takes a feature structure for an FCG unit as input and returns:
      - the unit converted into a processing-unit,
      - the tags to be added in J-units and
      - the tagged features to be added in the root"

  (let ((fs (copy-list unit-fs))
        (unit-fs-to-return nil)
        (tag-conditional-to-return nil)
        (tag-root-to-return))

    ;; --> For j-units append, make unit-name in form of '(J unit-name)'
    (when j-unit
      (setf fs (cons (list 'J (first fs)) (cdr fs))))

    ;; --> Process each feature of the unit, collect:
    ;;     - converted feature
    ;;     - tagged variable for conditional-unit
    ;;     - tagged feature for root
    (dolist (feature (cdr fs))
      (multiple-value-bind (converted-feature tag-for-conditional-unit tag-for-root)
          (convert-feature-to-fcg-2 feature feature-types)
        (when converted-feature
          (push converted-feature unit-fs-to-return))
        (when tag-for-conditional-unit
          (push tag-for-conditional-unit tag-conditional-to-return))
        (when tag-for-root
          (push tag-for-root tag-root-to-return))))

    ;; --> If a non j-unit contains not only hashed (tagged) features, add an ==0 footprint to it,
    ;;     unless automatic-footprints are disabled, of course
    (when unit-fs-to-return
      (unless (or (disable-automatic-footprints (cxn-inventory cxn)) (disable-automatic-footprints cxn))
        (unless j-unit
          (let ((footprints-feature (find 'footprints unit-fs-to-return :test 'string= :key 'first)))
            (if footprints-feature
              (pushend (make-symbol (string-append pole '- cxn-name)) (second footprints-feature))
              (push `(footprints (==0 ,(make-symbol (string-append pole '- cxn-name)))) unit-fs-to-return))))))
    
    ;; Construct values to return
    (cond
     ;; Case 1: no tags, but unit: return unit, nil and nil
     ((and unit-fs-to-return (not tag-conditional-to-return))
      (values (cons (first fs) (reverse unit-fs-to-return)) nil nil))
     ;; Case 2: tags
     (tag-conditional-to-return
      (if unit-fs-to-return
        ;; if unit, return unit, tagged variable for j-unit and tagged feature for root
        (values (cons (first fs) (reverse unit-fs-to-return)) (cons (first fs) tag-conditional-to-return) tag-root-to-return)
        ;; if no unit, return nil, tagged variable for j-unit and tagged feature for root
        (values nil (cons (first fs) tag-conditional-to-return) tag-root-to-return))))))

(defun check-for-invalid-not-operator (feature-name feature-value feature-type)
  "Does a deep search for the occurrence of the NOT operator and throws an error when found.
   The function allows dotted lists to be used."
  (cond ((atom feature-value) nil)
        ((and (atom (first feature-value))
              (string= "NOT" (write-to-string (first feature-value)))) (error "You cannot use negation in ~a: ~a" feature-type feature-name))
        (t
         (check-for-invalid-not-operator feature-name (first feature-value) feature-type)
         (check-for-invalid-not-operator feature-name (rest feature-value) feature-type))))

(defun convert-feature-to-fcg-2 (feature feature-types)
  "converts an fcg-feature to a processing-feature"
 (cond
  
  ;; Case 1: feature-value = atomic e.g. (head ?verb)
  ;; --> Return feature
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ((and (null (cddr feature)) (atom (second feature)))
   feature)

  ;; Case 2: The feature starts with a hash: e.g. (hash form ((string ?x "x")))
  ;; --> Return nil, a new variable and a the feature tagged with this variable (for the root later)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ((eql (feature-name feature) 'hash)
   (let ((converted-feature (convert-feature-to-fcg-2 (cdr feature) feature-types)))
     (if (string= (first converted-feature) 'tag)
       (values nil (second converted-feature) converted-feature)
       (let ((tag (make-var "tag")))
         (values nil tag `(tag ,tag ,(convert-feature-to-fcg-2 (cdr feature) feature-types)))))))
  
  ;; Case 3: ++ operator. A feature is typed as special, e.g.: (form set-of-predicates :word-order)
  ;; --> Return feature expanded to ++ operator
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ((third (assoc (feature-name feature) feature-types))
   `(,(feature-name feature) (++ ,(third (assoc (feature-name feature) feature-types)) ,(second feature))))

 ;; Case 4: >> operator (overwrites)
 ;; --> Return feature with prefix special operator ->
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ((find '>> feature)
  (let* ((operator-index (position '>> feature))
         (old (nth (- operator-index 1) feature))
         (new (nth (+ operator-index 1) feature)))
    (cond ((string= (second (assoc (feature-name feature) feature-types)) "SET")
          `(,(feature-name feature) (-> ,(cons '== old) ,new)))
          (t
           `(,(feature-name feature) (-> ,old ,new))))))
 
 ;; Case 5: feature is a set of atoms
 ;; --> cons == or ==0 to the set
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
 ((string= (second (assoc (feature-name feature) feature-types)) "SET")
  (let ((set-elements nil)
        (first-element? t))
    (dolist (value (second feature))
      (if (string= "NOT" value)
        (progn
          (setf set-elements (cons '==0 set-elements))
          (when first-element?
            (setf first-element? nil)))
        (progn
          (when first-element?
            (setf set-elements (cons '== set-elements))
            (setf first-element? nil))
          (setf set-elements (cons value set-elements)))))
    (list (feature-name feature) (reverse set-elements))))
 
  ;; Case 5: feature is a set-of-predicates
  ;; --> cons == or ==0 to value
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ((string= (second (assoc (feature-name feature) feature-types)) "SET-OF-PREDICATES")
   (let ((feature-values nil)
         (feature-values-with-negation nil))
     ;; For overwriting in set-of-predicates (NOT compatible with negation)
     (if (find '>> (second feature))
       (let* ((operator-index (position '>> (second feature)))
              (old (nth (- operator-index 1) (second feature)))
              (new (nth (+ operator-index 1) (second feature))))
         `(,(feature-name feature) (-> (,old) (,new))))
       ;; For non-overwriting (default) handling
       (progn
         (loop for value in (reverse (second feature))
               do (if (and (listp value) (string= "NOT" (first value)))
                    (if feature-values-with-negation
                      (error "You can only use negation once in sets.")
                      (setf feature-values-with-negation (cons '==0 (rest value))))
                    (push value feature-values)))
         (when feature-values (push '== feature-values))
         (if (and feature-values feature-values-with-negation)
           (let ((tag1 (make-var "tag-include"))
                 (tag2 (make-var "tag-exclude")))
             `(tag ,tag1 (,(feature-name feature) ,feature-values)
                   ,tag2 (,(feature-name feature) ,feature-values-with-negation)))
           (list (feature-name feature) (or feature-values feature-values-with-negation)))))))


  ;; Case 6: sequence-of-predicates
  ;; --> Return the feature as is
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ((string= (second (assoc (feature-name feature) feature-types)) "SEQUENCE-OF-PREDICATES")
   (check-for-invalid-not-operator (feature-name feature)
                                   (feature-value feature) "sequence-of-predicates")
   feature)
  
  ;; Case 7: Feature is a sequence
  ;; --> Return feature as is
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ((equalp (symbol-name (second (assoc (feature-name feature) feature-types))) "sequence")
   (check-for-invalid-not-operator (feature-name feature)
                                   (feature-value feature) "sequence")
   feature)
  
  ;; Case 8: First element is NOT:
  ;; --> convert rest of feature and put between (==0 feature)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ((equalp (symbol-name (feature-name feature)) "NOT")
   `(==0 ,@(convert-feature-to-fcg-2 (rest feature) feature-types)))

   ;; Case 9: Set of feature-value pairs
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ((string= (second (assoc (feature-name feature) feature-types)) "SET-OF-FEATURES")
   (let ((pos-path nil)
         (neg-paths nil))
     (dolist (value (reverse (rest feature)))
       (let ((translated-feature (convert-feature-to-fcg-2 value feature-types)))
         (cond
          ((eq 'tag (first translated-feature))
           (push (third translated-feature) pos-path)
           (push (fifth translated-feature) neg-paths))
          ((or (and (listp (second translated-feature))
                    (eq (first (second translated-feature)) '==0))
               (eq (first translated-feature) '==0))
           (push translated-feature neg-paths))
          (t
           (push translated-feature pos-path)))))
     (cond
      ((null neg-paths)
       `(,(feature-name feature) (== ,@pos-path)))
      ((and (null pos-path) (null (rest neg-paths)))
       `(,(feature-name feature)
         ,(if (eql '==0 (caar neg-paths))
            (car neg-paths)
            (list '== (first neg-paths)))))
      (t
       `(tag ,(make-var "tag") (,(feature-name feature) (== ,@pos-path))
             ,@(loop for neg-path in (reverse neg-paths)
                     append (list (make-var "tag")
                                  (list (feature-name feature)
                                        (if (eql '==0 (first neg-path))
                                          neg-path
                                          (list '== neg-path))))))))))

  ;; Case 10: Default: Value is a feature-value pair:
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (t
   (let ((pos-path nil)
         (neg-paths nil))
     (dolist (value (reverse (rest feature)))
       (let ((translated-feature (convert-feature-to-fcg-2 value feature-types)))
         (cond
          ((eq 'tag (first translated-feature))
           (push (third translated-feature) pos-path)
           (push (fifth translated-feature) neg-paths))
          ((or (and (listp (second translated-feature))
                    (eq (first (second translated-feature)) '==0))
               (eq (first translated-feature) '==0))
           (push translated-feature neg-paths))
          (t
           (push translated-feature pos-path)))))
     (cond
      ((null neg-paths)
       `(,(feature-name feature) (==1 ,@pos-path)))
      ((and (null pos-path) (null (rest neg-paths)))
       `(,(feature-name feature)
         ,(if (eql '==0 (caar neg-paths))
            (car neg-paths)
            (list '==1 (first neg-paths)))))
      (t
       `(tag ,(make-var "tag") (,(feature-name feature) (==1 ,@pos-path))
             ,@(loop for neg-path in (reverse neg-paths)
                     append (list (make-var "tag")
                                  (list (feature-name feature)
                                        (if (eql '==0 (first neg-path))
                                          neg-path
                                          (list '==1 neg-path))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; 3. Helper Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-footprints (j-unit)
  "returns the footprints feature of the unit"
  (let ((value-to-return nil))
    (dolist (feature (cdr j-unit))
      (when (listp feature)
        (when (eql (car feature) 'footprints)
          (setf value-to-return feature))))
    value-to-return))

(defun inherit-feature-types (inventory-types cxn-types)
  "returns inventory types with cxn-types added to them
   when in conflict, cxn-types win"
  (cond ((eq cxn-types nil)
         inventory-types)
        ((find (first (first cxn-types)) inventory-types :key #'first :test #'string=)
         (inherit-feature-types
          (substitute (first cxn-types) (find (first (first cxn-types)) inventory-types :key #'first :test #'string=) inventory-types)
          (rest cxn-types)))
        (t
         (inherit-feature-types
          (append (list (first cxn-types)) inventory-types)
          (rest cxn-types)))))

(defun root-append (conditional-root-unit hashed-root-features)
  "This function checks the existing root (specified as a unit on the
conditional part for features and tags those that also occur in the
list of new-root-features. Double tagging is needed because FCG-2
unification cannot handle two features with the same feature name."
  (let ((conditional-root-features (rest conditional-root-unit))
        (hashed-root-feature-names
         (mapcar #'feature-name (remove-special-operators hashed-root-features
                                                          +no-bindings+))))
    (setf conditional-root-features
          (loop for conditional-feature in conditional-root-features
            if (find (feature-name conditional-feature) hashed-root-feature-names)
            collect (list 'tag (make-var 'tag) conditional-feature)
            else collect conditional-feature))
              
    (cons 'root (append conditional-root-features hashed-root-features))))
