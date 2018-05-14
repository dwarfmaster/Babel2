
(ql:quickload :irl)

(in-package :irl)

;;                 IRL Tutorial
;;
;; This file is intended as a stand-alone tutorial
;; that explains step-by-step how to use all the
;; functionality of IRL.
;;
;; Content:                                   | Method/Class/Function discussed
;; -------------------------------------------+-------------------------------------------
;; Part 1: irl-objects                        | (class: entity, method: equal-entity)
;; Part 2: Creating primitives                | (macro: defprimitive)
;;   Part 2a: Filter-by-circle                |
;;   Part 2b: A more general primitive:       |
;;            filter-by-shape                 |
;;     Part 2b-I:   First case                |
;;     Part 2b-II:  Multidirectionality       |
;;     Part 2b-III: Hypotheses                |
;;     Part 2b-IV:  Consistency check         |
;; Part 3: Ontologies                         |
;; Part 4: Continuous features                |
;; Part 5: Networks                           |
;; Part 6. Chunks                             | (class: chunk)
;;   Part 6a: Evaluation chunk                | (function: evaluate-chunk)
;;   Part 6b: Create chunk from primitive     | (function: create-chunk-from-primitive)
;;   Part 6c: Create chunks from irl program  | (function: create-chunk-from-irl-program)
;;   Part 6d: Combine chunks                  | (function: combine-chunk-program)
;; Part 7: Using the composer                 | 
;;   Part 7a: Define composer class           | (class: composer, method: handle-node)
;;   Part 7b: Create/initialize composer      |
;;   Part 7c: Confine the search              |
;; Part 8: Matching/Flexible interpretation   | 
;;   Part 8a: Matching examples               | (function: match-chunk)
;;   Part 8b: Flexible interpretation         |



;; ##########################################################
;; Part 1. irl-objects
;; ##########################################################


;; Creating entities for IRL
;; They _have to_ inherit from the class entity

(defclass object (entity)
  ((shape :type symbol :initarg :shape :accessor shape)
   (size :type float :initarg :size :accessor size)))

(defclass object-set (entity)
  ((objects :type list :initarg :objects :accessor objects)))

(defmethod equal-entity ((set1 object-set) (set2 object-set))
  (permutation-of? (objects set1) (objects set2) :test #'equal-entity))


;; Drawing entities in IRL
;; Overload the method make-html-for-entity-details

(defmethod make-html-for-entity-details ((object object) &key)
  `(((div :class "entity-detail" :style "text-align:center")
     ((span :style ,(format nil "font-size:~,2fpx;" (+ 20 (* 30 (size object)))))
      ,(ecase (shape object) (rectangle "&#x25ad;") (triangle "&#x25b3;") (circle "&#x25ef;"))))
    ((div :class "entity-detail") ,(format nil "size: ~,2f" (size object)))))

(defmethod make-html-for-entity-details ((set object-set) &key)
  `(((div :class "entity-detail") 
     ,@(loop for object in (objects set)
             collect (make-html object :expand-initially t)))))


;; Show in browser (http://localhost:8000/)

(progn
  (clear-page)
  (add-element (make-html (make-instance 'object
                                         :shape 'triangle
                                         :size 0.9
                                         :id 'a-triangle)
                          :expand-initially t)))


;; Create a context

(defparameter *context* 
  (make-instance 
   'object-set
   :id 'my-context
   :objects (list (make-instance 'object :size 0.8 :shape 'circle)
                  (make-instance 'object :size 0.6 :shape 'rectangle)
                  (make-instance 'object :size 0.4 :shape 'circle)
                  (make-instance 'object :size 0.2 :shape 'triangle)
                  (make-instance 'object :size 0.9 :shape 'rectangle))))


(add-element (make-html *context* :expand-initially t))



;; ##########################################################
;; Part 2: Creating primitives
;; ##########################################################


;;   ######################################################
;;   Part 2a: Filter-by-circle
;;   ######################################################

;;     #################################################
;;     A primitive that computes a new set, containing
;;     only the circles from the input-set

(defprimitive filter-circles ((target-set object-set) (source-set object-set))
  ((source-set => target-set)
   (let ((set-of-circles
          (make-instance 'object-set
                         :objects (loop for object in (objects source-set)
                                        if (eq (shape object) 'circle)
                                        collect object))))
         (bind (target-set 1.0 set-of-circles)))))

(activate-monitor trace-irl-in-web-browser)

(clear-page)
(evaluate-irl-program 
 `((filter-circles ?target-set ?source-set)
   (bind object-set ?source-set ,*context*))
 nil)


;;   ######################################################
;;   Part 2b: A more general primitive: filter-by-shape
;;   ######################################################

;; Create a shape-category object
(defclass shape-category (entity)
  ((shape :type symbol :initarg :shape :reader shape)))

;; Drawing the category
(defmethod make-html-for-entity-details ((category shape-category) &key)
  `(((div :class "entity-detail" :style "text-align:center")
     ((span :style "font-size:50px;")
      ,(ecase (shape category) (rectangle "&#x25ad;")
         (triangle "&#x25b3;") (circle "&#x25ef;"))))))

;; A list of all the categories
(defparameter *shapes*
  (list
   (make-instance 'shape-category :id 'rectangle :shape 'rectangle)
   (make-instance 'shape-category :id 'circle :shape 'circle)
   (make-instance 'shape-category :id 'triangle :shape 'triangle)))

;; show categories in browser
(progn
  (clear-page)
  (loop for shape in *shapes*
        do (add-element (make-html shape :expand-initially t))))


;;     #################################################
;;     Part 2b-I: First case
;;     #################################################

(defun filter-by-shape (object-set shape-category)
  (let ((filtered-objects (loop for object in (objects object-set)
                                if (eq (shape object) (shape shape-category))
                                collect object)))
    (when filtered-objects
      (make-instance 'object-set :objects filtered-objects))))

(defprimitive filter-by-shape ((filtered-set object-set)
                               (source-set object-set)
                               (shape-category shape-category))
  ;; if given source-set and shape, compute filtered-set
  ((source-set shape-category => filtered-set)
   (let ((computed-set (filter-by-shape source-set shape-category)))
     (when computed-set
       (bind (filtered-set 1.0 computed-set))))))


;; 1 source-set shape => filtered-set (rectangle)
(evaluate-irl-program 
 `((filter-by-shape ?filtered-set ?source-set ?shape)
   (bind shape-category ?shape ,(first *shapes*))
   (bind object-set ?source-set ,*context*))
 nil)

;; 2 source-set shape => filtered-set (circle)
(evaluate-irl-program 
 `((filter-by-shape ?filtered-set ?source-set ?shape)
   (bind shape-category ?shape ,(second *shapes*))
   (bind object-set ?source-set ,*context*))
 nil)

;; 3 source-set filtered-set => shape (does not work)
(evaluate-irl-program 
 `((filter-by-shape ?filtered-set ?source-set ?shape)
   (bind object-set ?source-set ,*context*)
   (bind object-set ?filtered-set 
         ,(make-instance 'object-set 
                         :objects (list (fourth (objects *context*))))))
 nil)


;;     #################################################
;;     Part 2b-II: Multidirectionality
;;     #################################################

(defprimitive filter-by-shape ((filtered-set object-set) (source-set object-set)
                               (shape-category shape-category))
  ;; previous case: if given source-set and shape, compute filtered-set
  ((source-set shape-category => filtered-set)
   (let ((computed-set (filter-by-shape source-set shape-category)))
     (when computed-set
       (bind (filtered-set 1.0 computed-set)))))
  
  ;; new case: if given source-set and filtered-set, compute shape-category
  ((source-set filtered-set => shape-category)
   (let ((computed-category
          (find-if #'(lambda (shape) (equal-entity
                                      filtered-set
                                      (filter-by-shape source-set shape)))
                   *shapes*)))
     (when computed-category
       (bind (shape-category 1.0 computed-category))))))

;; 1 source-set filtered-set => shape (works now)
(evaluate-irl-program 
 `((filter-by-shape ?filtered-set ?source-set ?shape)
   (bind object-set ?source-set ,*context*)
   (bind object-set ?filtered-set 
         ,(make-instance 'object-set 
                         :objects (list (fourth (objects *context*))))))
 nil)

;; 2 source-set => filtered-set shape (doesn't work)
(evaluate-irl-program 
 `((filter-by-shape ?filtered-set ?source-set ?shape)
   (bind object-set ?source-set ,*context*))
 nil)


;;     #################################################
;;     Part 2b-III: Hypotheses
;;     #################################################

(defprimitive filter-by-shape ((filtered-set object-set) (source-set object-set)
                               (shape-category shape-category))
  ;; first case: if given source-set and shape, compute filtered-set
  ((source-set shape-category => filtered-set)
   (let ((computed-set (filter-by-shape source-set shape-category)))
     (when computed-set
       (bind (filtered-set 1.0 computed-set)))))
  
  ;; previous case: if given source-set and filtered-set, compute shape-category
  ((source-set filtered-set => shape-category)
   (let ((computed-category
          (find-if #'(lambda (shape) (equal-entity
                                      filtered-set
                                      (filter-by-shape source-set shape)))
                   *shapes*)))
     (when computed-category
       (bind (shape-category 1.0 computed-category)))))

  ;; new case: if given source-set, compute pairs of filtered-set and shape
  ((source-set => filtered-set shape-category)
   (loop for shape in *shapes*
         for computed-set = (filter-by-shape source-set shape)
         if computed-set
         do (bind (shape-category 1.0 shape)
                  (filtered-set 1.0 computed-set)))))

;; 1 source-set => filtered-set shape (works now)
(evaluate-irl-program 
 `((filter-by-shape ?filtered-set ?source-set ?shape)
   (bind object-set ?source-set ,*context*))
 nil)

;; 2 filtered-set source-set shape => (doesn't work)
(evaluate-irl-program 
 `((filter-by-shape ?filtered-set ?source-set ?shape)
   (bind object-set ?source-set ,*context*)
   (bind shape-category ?shape ,(third *shapes*))
   (bind object-set ?filtered-set 
         ,(make-instance 'object-set 
                         :objects (list (fourth (objects *context*))))))
 nil)


;;     #################################################
;;     Part 2b-IV: Consistency check
;;     #################################################

(defprimitive filter-by-shape ((filtered-set object-set) (source-set object-set)
                               (shape-category shape-category))
  ;; first case: if given source-set and shape, compute filtered-set
  ((source-set shape-category => filtered-set)
   (let ((computed-set (filter-by-shape source-set shape-category)))
     (when computed-set
       (bind (filtered-set 1.0 computed-set)))))
  
  ;; second case: if given source-set and filtered-set, compute shape-category
  ((source-set filtered-set => shape-category)
   (let ((computed-category
          (find-if #'(lambda (shape) (equal-entity
                                      filtered-set
                                      (filter-by-shape source-set shape)))
                   *shapes*)))
     (when computed-category
       (bind (shape-category 1.0 computed-category)))))

  ;; previous case: if given source-set, compute pairs of filtered-set and shape
  ((source-set => filtered-set shape-category)
   (loop for shape in *shapes*
         for computed-set = (filter-by-shape source-set shape)
         if computed-set
         do (bind (shape-category 1.0 shape)
                  (filtered-set 1.0 computed-set))))

  ;; final case: if given source-set, filtered-set and shape, check for consitency
  ((source-set filtered-set shape-category =>)
   (equal-entity filtered-set (filter-by-shape source-set shape-category))))

;; 1 filtered-set source-set shape => (works now)
(evaluate-irl-program 
 `((filter-by-shape ?filtered-set ?source-set ?shape)
   (bind object-set ?source-set ,*context*)
   (bind shape-category ?shape ,(third *shapes*))
   (bind object-set ?filtered-set 
         ,(make-instance 'object-set 
                         :objects (list (fourth (objects *context*))))))
 nil)



;; ####################################################################
;; Part 3: Ontologies
;; ####################################################################

(progn
  (defparameter *ontology* (make-blackboard))
  (set-data *ontology* 'shapes *shapes*)
  (set-data *ontology* 'context *context*))

;; The ontology is used in two ways:

;; 1. If we pass a symbol in the bind-statement, IRL will automatically look
;; for an entity with that id.
(evaluate-irl-program '((bind object-set ?context my-context)) *ontology*)

;; 2. In the primitive definition, ontology is a keyword that always refers
;; to the ontology that is passed in (evaluate-irl-program ...) 
(defprimitive get-context ((context object-set))
  ((context =>)
   (equal-entity (get-data ontology 'context) context))
  ((=> context)
   (bind (context 1.0 (get-data ontology 'context)))))

(evaluate-irl-program '((get-context ?context)) *ontology*)

;; Use the ontology rather than global variables:
(defprimitive filter-by-shape ((filtered-set object-set) (source-set object-set)
                               (shape-category shape-category))
  ;; case 1
  ((source-set shape-category => filtered-set)
   (let ((computed-set (filter-by-shape source-set shape-category)))
     (when computed-set (bind (filtered-set 1.0 computed-set)))))
  
  ;; case 2
  ((source-set filtered-set => shape-category)
   (let ((computed-category
          (find-if #'(lambda (shape) (equal-entity filtered-set (filter-by-shape source-set shape)))
                   ;;*shapes*)
                   (get-data ontology 'shapes))
          ))
     (when computed-category (bind (shape-category 1.0 computed-category)))))

  ;; case 3
  ((source-set => filtered-set shape-category)
   (loop for shape in (get-data ontology 'shapes) ;;*shapes*
         for computed-set = (filter-by-shape source-set shape)
         if computed-set
         do (bind (shape-category 1.0 shape) (filtered-set 1.0 computed-set))))

  ;; case 4
  ((source-set filtered-set shape-category =>)
   (equal-entity filtered-set (filter-by-shape source-set shape-category))))

;; Test: source-set shape => filtered-set
(evaluate-irl-program 
 `((filter-by-shape ?filtered-set ?source-set ?shape)
   (bind shape-category ?shape rectangle)
   (get-context ?source-set))
 *ontology*)



;; ####################################################################
;; Part 4: Continuous features
;; ####################################################################

;; The size of the objects are continuous. Here we look at an example
;; how to categorize with such continuous features.

;; First we need a category class
(defclass size-category (entity)
  ((size :type number :initarg :size :reader size)))

;; Drawing the category
(defmethod make-html-for-entity-details ((category size-category) &key)
  `(((div :class "entity-detail") ,(format nil "size: ~,2f" (size category)))))

;; A list of all the size categories
(set-data *ontology*
          'sizes
          (list
           (make-instance 'size-category :id 'small :size 0.3)
           (make-instance 'size-category :id 'big :size 0.7)))
(progn
  (clear-page)
  (loop for shape in (get-data *ontology* 'sizes)
        do (add-element (make-html shape :expand-initially t))))

;; Normally we first define a similarity measure between categories and
;; entities.
(defgeneric similarity (x y)
  (:documentation "A similarity measure between two irl entities"))
(defmethod similarity ((x entity) (y entity))
  0)
(defmethod similarity ((object object) (category size-category))
  ;; 1 - the size difference
  (- 1 (abs (- (size object) (size category)))))

;; Voronoi: Find the best category for an object 
(defun find-best-category (object categories)
  (reduce #'(lambda (cat1 cat2)
              (if (> (similarity object cat1)
                     (similarity object cat2))
                     cat1 cat2))
          categories))

;; Filter by size find every object for which the given category
;; is the best category.
(defun filter-by-size (object-set size-category all-size-categories)
  (let ((filtered-objects (loop for object in (objects object-set)
                                if (equal-entity
                                    size-category
                                    (find-best-category object all-size-categories))
                                collect object)))
    (when filtered-objects
      (make-instance 'object-set :objects filtered-objects))))               

;; Use the ontology rather than global variables:
(defprimitive filter-by-size ((filtered-set object-set) (source-set object-set)
                              (size-category size-category))
  ;; case 1
  ((source-set size-category => filtered-set)
   (let ((computed-set (filter-by-size source-set size-category (get-data ontology 'sizes))))
     (when computed-set (bind (filtered-set 1.0 computed-set)))))
  
  ;; case 2
  ((source-set filtered-set => size-category)
   (let ((computed-category
          (find-if #'(lambda (size)
                       (equal-entity
                        filtered-set
                        (filter-by-size source-set size (get-data ontology 'sizes))))
                   (get-data ontology 'sizes))))
     (when computed-category (bind (size-category 1.0 computed-category)))))

  ;; case 3
  ((source-set => filtered-set size-category)
   (loop for size in (get-data ontology 'sizes)
         for computed-set = (filter-by-size source-set size (get-data ontology 'sizes))
         if computed-set
         do (bind (size-category 1.0 size) (filtered-set 1.0 computed-set))))

  ;; case 4
  ((source-set filtered-set size-category =>)
   (equal-entity filtered-set (filter-by-size source-set size-category (get-data ontology 'sizes)))))

;; source-set size => filtered-set
(evaluate-irl-program 
 `((filter-by-size ?target-set ?source-set ?size)
   (get-context ?source-set)
   (bind size-category ?size big))
 *ontology*)



;; ####################################################################
;; Part 5: Networks
;; ####################################################################

(defprimitive unique-entity ((unique-entity object) (source-set object-set))
  ((unique-entity source-set =>)
   (and (= 1 (length (objects source-set)))
        (eq (id (first (objects source-set))) (id unique-entity))))
  
  ((source-set => unique-entity)
   (when (= 1 (length (objects source-set)))
     (bind (unique-entity 1.0 (first (objects source-set)))))))

;; We can discriminate the big circle
(evaluate-irl-program
 `((unique-entity ?topic ?set2)
   (filter-by-size ?set2 ?set1 ?size-category)
   (filter-by-shape ?set1 ?source-set ?shape-category)
   (get-context ?source-set)
   (bind size-category ?size-category big)
   (bind shape-category ?shape-category circle))
 *ontology*)

;; And we can let IRL figure that out:
(evaluate-irl-program
 `((bind object ?topic ,(first (objects (get-data *ontology* 'context))))
   (unique-entity ?topic ?set2)
   (filter-by-size ?set2 ?set1 ?size-category)
   (filter-by-shape ?set1 ?source-set ?shape-category)
   (get-context ?source-set))
 *ontology*)



;; ##########################################################
;; Part 6. Chunks
;; ##########################################################

;; Chunks are a way to package irl-networks. They are the main
;; building blocks for the composition process.

;; A chunk contains (1) a network, (2) open variables
;; and (3) a target variable. A chunk can be interpeted as a
;; complex operation that if we provide it with values for
;; the open variables, it can compute the target variable.

(defparameter *chunk-1*
  (make-instance 
   'chunk
   :id 'filter-by-shape+get-context
   :irl-program `((filter-by-shape ?target-set ?source-set ?shape)
                  (get-context ?source-set)) ;; 1) the irl network
   :open-vars '((?shape . shape-category))   ;; 2) the open variables
   :target-var '(?target-set . object-set))) ;; 3) the target variable 

(progn
  (clear-page)
  (add-element (make-html *chunk-1* :expand-initially t)))


;;   ######################################################
;;   Part 6a: Evaluation chunk

(evaluate-chunk *chunk-1* *ontology*)


;;   ######################################################
;;   Part 6b: Create chunk from primitive

(defparameter *chunk-2*
  (create-chunk-from-primitive 'filter-by-size))

(progn
  (clear-page)
  (add-element (make-html *chunk-2* :expand-initially t)))


;;   ######################################################
;;   Part 6c: Create chunks from irl program

(defparameter *chunk-3*
  (create-chunk-from-irl-program `((filter-by-shape ?internal-set ?source-set ?shape)
                                   (filter-by-size ?output-set ?internal-set ?size))))

(progn
  (clear-page)
  (add-element (make-html *chunk-3* :expand-initially t)))


;;   ######################################################
;;   Part 6d: Combine chunks

(defparameter *chunk-4* (first (combine-chunk-program *chunk-2* *chunk-1*)))

(progn
  (clear-page)
  (add-element `((div)
                 ((div :style "font-size:20px;") "Combining ")
                 ,(make-html *chunk-2* :expand-initially t)
                 ((div :style "font-size:20px;") "with")
                 ,(make-html *chunk-1* :expand-initially t)
                 ((div :style "font-size:20px;") " gives: ")
                 ,(make-html *chunk-4* :expand-initially t))))



;; ##########################################################
;; Part 7. Using the composer
;; ##########################################################

;; The composer uses the combine-chunk-program function to
;; gradually build up increasingly complex programs
;; It starts with an initial (empty) chunk and tries to
;; combine it with the list of available chunks until it
;; finds a chunk that when evaluated achieves the communicative
;; goal


;;   ######################################################
;;   Part 7a: Define composer class

;; Make a new composer class (the communicative goal is to
;; discriminate a topic oject, so we create a topic slot)
(defclass my-composer (chunk-composer)
  ((topic :initarg :topic :accessor topic :initform nil)))

;; Overload handle-node to meet our specific needs
;; Return values:
;; 1) nil: No results, search process continues.
;; 2) a list containing chunk-composer-node-solutions:
;     the method (get-next-solution ...) returns this list

(defmethod handle-node ((node chunk-composer-node)
                        (handler (eql 'evaluate))
                        (composer my-composer))
  (let ((evaluation-results (call-next-method)))
    (values (loop for result in evaluation-results
                  ;; check if the evaluation results indeed discriminates
                  ;; the topic
                  if (equal-entity (target-entity result) (topic composer))
                  collect result)
            nil)))


;;   ######################################################
;;   Part 7b: Create/initialize composer

(setf *my-composer* (make-instance 'my-composer
                                   :topic (first (objects (get-data *ontology* 'context)))
                                   :initial-chunk (make-instance 'chunk
                                                                 :id 'initial
                                                                 :target-var '(?topic . object)
                                                                 :open-vars '((?topic . object)))
                                   :chunks (list
                                            (create-chunk-from-primitive 'get-context)
                                            (create-chunk-from-primitive 'unique-entity)
                                            (create-chunk-from-primitive 'filter-by-shape)
                                            (create-chunk-from-primitive 'filter-by-size))
                                   :max-search-depth 4
                                   :ontology *ontology*))
;; Find first solution
(get-next-solutions *my-composer*)

;; Find second solution
(get-next-solutions *my-composer*)

;; No solutions left
(get-next-solutions *my-composer*)


;;   ######################################################
;;   Part 7c: Confine the search

;; We have to be careful. Since the filter-operations can re-apply
;; till infinity, we find a lot of useless solutions if we increase
;; the search depth.

(setf *my-composer-2* (make-instance 'my-composer
                                     :topic (first (objects (get-data *ontology* 'context)))
                                     :initial-chunk (make-instance 'chunk
                                                                   :id 'initial
                                                                   :target-var '(?topic . object)
                                                                   :open-vars '((?topic . object)))
                                     :chunks (list
                                              (create-chunk-from-primitive 'get-context)
                                              (create-chunk-from-primitive 'unique-entity)
                                              (create-chunk-from-primitive 'filter-by-shape)
                                              (create-chunk-from-primitive 'filter-by-size))
                                     :max-search-depth 6
                                     :ontology *ontology*))

(get-all-solutions *my-composer-2*) ;; this will return a lot of unwanted results

;; One way to solve it is to make sure that the composer only creates
;; nodes that contain at most one instance of the same operation
(defmethod handle-node ((node chunk-composer-node)
                        (handler (eql 'expand))
                        (composer my-composer))
  (multiple-value-bind (ignore nodes) (call-next-method)
    (values
     nil
     (loop for node in nodes
           for irl-program = (irl-program (chunk node))
           if (not (duplicates? irl-program :key #'first))
           collect node))))

(setf *my-composer-2* (make-instance 'my-composer
                                     :topic (first (objects (get-data *ontology* 'context)))
                                     :initial-chunk (make-instance 'chunk
                                                                   :id 'initial
                                                                   :target-var '(?topic . object)
                                                                   :open-vars '((?topic . object)))
                                     :chunks (list
                                              (create-chunk-from-primitive 'get-context)
                                              (create-chunk-from-primitive 'unique-entity)
                                              (create-chunk-from-primitive 'filter-by-shape)
                                              (create-chunk-from-primitive 'filter-by-size))
                                     :max-search-depth 6
                                     :ontology *ontology*))

(get-all-solutions *my-composer-2*) ;; now it only returns relevant results



;; ##########################################################
;; Part 8. Matching
;; ##########################################################

;; IRL is made to provide an interpretation/composition
;; mechanism for _natural language_. NL is inherently vague.
;; E.g.  It is possible that the hearer only partly understands the
;; utterance "big circle". It knows that the prototypes for big
;; and circle are involved (i.e. it gets the partial program
;; '((bind size-category ?size big) (bind shape-category ?shape circle))
;; but doesn't know how they are related)

;; We can let the composer look for a network that discriminates
;; a topic _and_ matches the meaning. The matching is done
;; with the function match-chunk that tests the following:


;; 1) All the primitves in the meaning must be in the chunk (not the other way around)
;; 2) All the bind statements in the meaning must match an open variable or a bind statement in the chunk.
;; 3) All the links in the meaning must be in the chunk (chunks can add links meanings not)


;;   ######################################################
;;   Part 8a: Matching examples

(setf *chunk-5*
      (make-instance 
       'chunk 
       :irl-program '((get-context ?ctx)
                      (filter-by-size ?set-1 ?ctx ?size)
                      (filter-by-shape ?set-2 ?set-1 ?shape)
                      (unique-entity ?target ?set-2))
       :target-var '(?target . object)
       :open-vars '((?size . size-category) (?shape . shape-category))))

;; Only bind statements (with typing)
(clear-page)
(match-chunk *chunk-5* '((bind size-category ?size big)
                         (bind shape-category ?shape circle)))

;; Complete meaning
(clear-page)      
(match-chunk *chunk-5* '((get-context ?ctx)
                         (filter-by-size ?set-1 ?ctx ?size)
                         (filter-by-shape ?set-2 ?set-1 ?shape)
                         (unique-entity ?target ?set-2)
                         (bind size-category ?size big)
                         (bind shape-category ?shape circle)))


;; All the primitives in the meaning should match one in the chunk
;; So, this doesn't match:
(clear-page)
(match-chunk *chunk-5* '((get-context ?ctx)
                         (filter-by-size ?set-1 ?ctx ?size)
                         (filter-by-bla ?set-2 ?set-1 ?shape)
                         (unique-entity ?target ?set-2)
                         (bind size-category ?size big)
                         (bind shape-category ?shape circle)))

;; The meaning can contain less primitives
(clear-page) 
(match-chunk *chunk-5* '((get-context ?ctx)
                         (filter-by-size ?set-1 ?ctx ?size)
                         (unique-entity ?target ?set-2)
                         (bind size-category ?size big)
                         (bind shape-category ?shape circle)))

;; Chunks can add a link to the result
;; The following will create a link between set-2 and set-3
(clear-page) 
(match-chunk *chunk-5* '((get-context ?ctx)
                         (filter-by-size ?set-1 ?ctx ?size)
                         (filter-by-shape ?set-2 ?set-1 ?shape)
                         (unique-entity ?target ?set-3)
                         (bind size-category ?size big)
                         (bind shape-category ?shape circle)))

;; Even if the meaning is completely disconnected
(clear-page) 
(match-chunk *chunk-5* '((get-context ?a)
                         (filter-by-size ?c ?b ?foo)
                         (filter-by-shape ?e ?d ?bar)
                         (unique-entity ?g ?f)
                         (bind size-category ?size big)
                         (bind shape-category ?shape circle)))

;; But aditional links in the meaning can not overrule the chunk
;; So, this doesn't match:
(clear-page) 
(match-chunk *chunk-5* '((get-context ?ctx)
                         (filter-by-size ?set-1 ?ctx ?size)
                         (filter-by-shape ?set-2 ?set-1 ?shape)
                         (unique-entity ?ctx ?set-2)
                         (bind size-category ?size big)
                         (bind shape-category ?shape circle)))


;;   ######################################################
;;   Part 8b: Flexible interpretation

(defparameter *partial-meaning-1* '((bind shape-category ?shape-category circle)
                                  (bind size-category ?size-category big)))

(setf *my-composer* (make-instance 'chunk-composer
                                   :meaning *partial-meaning-1*
                                   :initial-chunk (make-instance 'chunk
                                                                 :id 'initial
                                                                 :target-var '(?topic . object)
                                                                 :open-vars '((?topic . object)))
                                   :chunks (list
                                            (create-chunk-from-primitive 'get-context)
                                            (create-chunk-from-primitive 'unique-entity)
                                            (create-chunk-from-primitive 'filter-by-shape)
                                            (create-chunk-from-primitive 'filter-by-size))
                                   :max-search-depth 4
                                   :ontology *ontology*))

(get-all-solutions *my-composer*)
;; This returns two (semantically equivalent) solutions. E.g.:
;; ((unique-entity ?topic ?source-set)
;;  (filter-by-size ?source-set ?source-set-38 ?size-category)
;;  (filter-by-shape ?source-set-38 ?source-set-39 ?shape-category)
;;  (get-context ?source-set-39)
;;  (bind shape-category ?shape-category circle)
;;  (bind size-category ?size-category big))


;; End: This is all you need to know!