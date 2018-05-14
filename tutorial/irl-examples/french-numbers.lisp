;;;;;
;;;;; This file demonstrates the main functionalities of irl. It
;;;;; shows how to write primitives, how to test them, how to compose
;;;;; meanings for specific goals and how to interpret meanings coming
;;;;; from fcg.
;;;;;
;;;;; The example that we're going to walk through in this demo is the
;;;;; counting of apples in French. 
;;;;;
;;;;; In order to enjoy this example, the web interface should be
;;;;; supported by your lisp.
;;;;;

(ql:quickload :irl)

(ql:quickload :fcg)

(defpackage :french-numbers
  (:use :common-lisp :utils :monitors :web-interface :irl :fcg))

(in-package :french-numbers)

;;;;; #########################################################################
;;;;; 1. Basic Types

;; First we define the basic types that we need for counting apples:
;; sets of apples and quantities

;; an apple-set contains a certain number of apples
(defclass apple-set (entity)
  ((apples :type list :initarg :apples :accessor apples)))

;; this is a helper function that makes an apple set and adds
;; 'number-of-apples' times the symbol 'apple to the set
(defun make-apple-set (number-of-apples)
  (make-instance 'apple-set :id (make-id (mkstr number-of-apples '-apples))
                 :apples (loop for n from 1 to number-of-apples collect 'apple)))

;; for nice output of apple-set entities in the web browser we
;; specialize this method to visualize the number of apples
(defmethod make-html-for-entity-details ((set apple-set)  &key &allow-other-keys)
  `(((div :class "entity-detail")
     ,@(loop for i from 1 to (length (apples set))
          append `("&#x274d; " ,(if (= (mod i 10) 0) '((br)) ""))))
    ((div :class "entity-detail") "number of apples: " ,(length (apples set)))))

;; a quantity is a means to pick a number of elements from a set, for
;; example by comparing the elements in the set with a certain number of
;; fingers
(defclass quantity (entity)
  ((n :type integer :initarg :n :accessor n)))

;; this is again for more informative html output
(defmethod make-html-for-entity-details ((quantity quantity)  &key &allow-other-keys)
  `(((div :class "entity-detail") "n = " ,(n quantity))))

;; this makes a quantity and gives it an id corresponding to n, e.g.
;; 'seven for 7
(defun make-quantity (n)
  (make-instance 'quantity :id (intern (format nil "~:@(~r~)" n)) :n n))

;;;;; #########################################################################
;;;;; 2. Ontology

;; An ontology is a blackboard that contains all external data,
;; concepts, prototypes, etc. that are needed by the conceptualization
;; machinery.
(defparameter *apples-ontology* (make-blackboard))

;; let's assume that in French there are the 'basic numbers' 1 to 10
;; (each corresponding to a specific finger) and 20 (two times all
;; fingers on both hands). We add these quantities to the ontology as
;; 'basic concepts of quantity':
(set-data *apples-ontology* 'quantities 
          (append 
           (loop for i from 1 to 10 collect (make-quantity i))
           (list (make-quantity 20))))

;;;;; #########################################################################
;;;;; 3. Primitive Definition 1

;; We define a mental operation for picking quantities of apples from a set.
(defprimitive pick-apples ((set apple-set) (amount quantity))

  ;; when both the set and the amount are bound, then we check
  ;; whether the number of apples in the set corresponds to the
  ;; quantitiy
  ((set amount =>)
   (= (length (apples set)) (n amount)))

  ;; when the set is bound, we look up our ontology for a quantity
  ;; that is equivalent to the number of apples in the set. Note that
  ;; this can can fail when for example the number of apples is 37
  ;; (because there is no basic quantity concept of 37 in the
  ;; ontology).
  ((set => amount)
   (let ((amount (find (length (apples set))
                       (get-data ontology 'quantities)
                       :key #'n :test #'=)))
     (when amount (bind (amount 1.0 amount)))))

  ;; when the quantity is known, we just pick that number of apples
  ((amount => set)
   (bind (set 1.0 (make-apple-set (n amount))))))

;;;;; #########################################################################
;;;;; 4. IRL Program Evaluation 1

;; We can already try out this primitive

;; this will produce traces of the main irl functions in the web
;; browser at http://localhost:8000
(activate-monitor trace-irl-in-web-browser)
(clear-page)


;; this counts the number of apples in a set of 7 (look at
;; http://localhost:8000 to see the result)
(evaluate-irl-program `((pick-apples ?set ?amount)
                        (bind apple-set ?set ,(make-apple-set 7)))
                      *apples-ontology*)

;; -> We see that it comes up with a binding between ?amount and the
;; entity 'seven


;; and this picks seven apples (from a heap of plenty of them):
(evaluate-irl-program `((pick-apples ?set ?amount)
                        (bind quantity ?amount seven))
                      *apples-ontology*)


;; however, counting will fail when the number of apples is greater
;; than can be quantified by the basic number concepts (e.g. 32):
(evaluate-irl-program `((pick-apples ?set ?amount)
                        (bind apple-set ?set ,(make-apple-set 32)))
                      *apples-ontology*)

;;;;; #########################################################################
;;;;; 5. Primitive Definition 2

;; In order to deal with bigger heaps of apples that can not be
;; quantified by the basic number concepts, we need operations for
;; combining and for combining multiples of quantifyable apple sets.

;; this combines the number of apples from 'set' with a number of
;; apples quantified by 'amount' into 'sum'
(defprimitive add-apples ((sum apple-set) (amount quantity) (set apple-set))
  ;; all slots are bound, so we check whether the addition
  ;; holds
  ((sum amount set =>)
   (= (length (apples sum)) (+ (n amount) (length (apples set)))))
  
  ;; when we know 'set' and 'amount', we pick the 'sum' set
  ((set amount => sum)
   (bind (sum 1.0 (make-apple-set (+ (n amount) (length (apples set)))))))
  
  ;; when we know the 'sum', we take the biggest quantity that we have
  ;; a concept of and put the rest in 'set'
  ((sum => amount set)
   (loop for amount 
      in (reverse (get-data ontology 'quantities))
      for rest = (- (length (apples sum)) (n amount))
      when (> rest 0)
      do (bind (amount 1.0 amount) (set 1.0 (make-apple-set rest)))
      (return))))

;; this combines the number of apples in 'set' with multiples
;; ('factor') of apples quantified by 'amount' into 'sum'
(defprimitive multiply-apples ((sum apple-set) (factor quantity) 
                               (amount quantity) (set apple-set))
  ;; all slots bound, we check
  ((sum factor amount set =>)
   (= (length (apples sum)) (+ (* (n factor) (n amount))
                               (length (apples set)))))

  ;; all except 'sum' known, we pick the 'sum'
  ((set factor amount => sum)
   (bind (sum 1.0 (make-apple-set (+ (* (n factor) (n amount))
                                     (length (apples set)))))))

  ;; the 'sum' is know, we take the biggest 'quantity' and 'factor'
  ;; that we have a concept of and put the rest in 'set'
  ((sum => factor amount set)
   (loop with quantities
        = (reverse (get-data ontology 'quantities))
        for amount in quantities
        thereis (loop for factor in quantities
                   for rest = (- (length (apples sum)) (* (n factor) (n amount)))
                   when (> rest 1)
                   do (bind (factor 1.0 factor) (amount 1.0 amount)
                            (set 1.0 (make-apple-set rest)))
                   (return t)))))

;;;;; #########################################################################
;;;;; 6. IRL Program Evaluation 2

;; Now we can already conceptualize bigger sets of apples

;; this picks 97 apples. As a result, the variable ?apples is bound to
;; a set consisting of 97 apples (click on the entities in the web
;; browser to see details):
(evaluate-irl-program `((multiply-apples ?apples ?factor ?amount-1 ?apples-2)
                        (bind quantity ?factor four)
                        (bind quantity ?amount-1 twenty)
                        (add-apples ?apples-2 ?amount-2 ?apples-3)
                        (bind quantity ?amount-2 ten)
                        (pick-apples ?apples-3 ?amount-3)
                        (bind quantity ?amount-3 seven))
                      *apples-ontology*)

;; and this counts the number of apples in a set of 97 apples. 
(evaluate-irl-program `((bind apple-set ?apples ,(make-apple-set 97))
                        (multiply-apples ?apples ?factor ?amount-1 ?apples-2)
                        (add-apples ?apples-2 ?amount-2 ?apples-3)
                        (pick-apples ?apples-3 ?amount-3))
                      *apples-ontology*)

;;;;; #########################################################################
;;;;; 7. Composition of IRL Programs


;; Next we see how we can automatically combine the operations for
;; counting into irl programs. 

;; first we add the primitives we want to use as chunks to our ontology
(set-data *apples-ontology* 'chunks
          (create-chunks-from-primitives '(pick-apples add-apples multiply-apples)))

;; this defines the chunk to start the composition process with 
(defparameter *initial-chunk* 
  (make-instance 'chunk :target-var `(?apples . apple-set)
                 :open-vars `((?apples . apple-set))))

;; before we try to evaluate a chunk, we add a bind statment for the
;; apple set that we want to count
(defun make-chunk-wrapper-with-apple-set (number-of-apples)
  (lambda (chunk)
    (let ((copy (copy-object chunk)))
      (push `(bind apple-set ?apples ,(make-apple-set number-of-apples))
            (irl-program copy))
      copy)))

;; since we can only express basic quantities in language, we are not
;; interested in chunks that have apple-sets in their open variables
(defun exclude-solutions-with-apple-sets-in-open-vars (evaluation-result &rest rest)
  (declare (ignore rest))
  (loop for var in (open-vars (chunk evaluation-result))
     never (eq (cdr var) 'apple-set)))

;; finally we create the composer
(defparameter *apples-composer*
  (make-instance 
   'single-topic-composer
   :ontology *apples-ontology*
   :initial-chunk *initial-chunk*
   :chunk-wrapper-fn (make-chunk-wrapper-with-apple-set 97)
   :check-evaluation-result-fn #'exclude-solutions-with-apple-sets-in-open-vars))

;; this runs the composer to find one solution. 
(get-next-solutions *apples-composer*)

;; -> We get a result with the quantities 'four twenty ten seven',
;; which is a good conceptualization of a quantity of 97 apples in
;; French. Click on the solution and on the nodes to see the composed
;; irl programs.

;;;;; #########################################################################
;;;;; 8. FCG Constructions


;; In order to express the composed meaning in language, we need to
;; define some constructions for the basic quantities and for the
;; operations that combine them


;; a set that will contain all constructions
(defparameter *apple-constructions* (def-fcg-constructions apple-constructions
                                      :feature-types ((args sequence)
                                                      (ref sequence)
                                                      (form set-of-predicates)
                                                      (meaning set-of-predicates)
                                                      (subunits sequence))
                                      :fcg-configurations ((:parse-order lex phrasal)
                                                           (:production-order lex phrasal)
                                                           (:render-mode . :render-with-scope)
                                                           (:create-initial-structure-mode . :replace-variables-with-symbols)
                                                           (:production-goal-tests :no-applicable-cxns :connected-structure)
                                                           (:parse-goal-tests :no-applicable-cxns :connected-semantic-network))))

  
;; lexical entries for the basic number concepts
(loop for (n name) in `((1 un) (2 deux) (3 trois) (4 quatre) (5 cinq)
                        (6 six) (7 sept) (8 huit) (9 neuf) (10 dix) 
                        (20 vingt))
   for unit-name = (symb '? name '-unit)
   do (add-cxn (make-instance 'fcg-construction
                              :name name
                              :contributing-part (list (make-instance 'contributing-unit
                                                                      :name unit-name
                                                                      :unit-structure `((sem-cat sem-quantity)
                                                                                        (ref (?quantity))
                                                                                        (syn-cat syn-quantity)
                                                                                        (lb ,unit-name)
                                                                                        (rb ,unit-name))))
                              :conditional-part (list 
                                                 (make-instance 'conditional-unit
                                                                :name unit-name
                                                                :formulation-lock `((HASH meaning ((bind quantity ?quantity 
                                                                                                         ,(id (make-quantity n))))))
                                                                :comprehension-lock `((HASH form ((string ,unit-name ,(format nil "~(~s~)"
                                                                                                                              name)))))))
                              :attributes '((:label . lex) (:score . 0.5))
                              :feature-types '((args sequence)
                                               (ref sequence)
                                               (form set-of-predicates)
                                               (meaning set-of-predicates)
                                               (subunits sequence))
                              :cxn-inventory *apple-constructions*)
               *apple-constructions*))

;; grammatical constructions for the operations on quantities
(add-cxn (make-instance 'fcg-construction
                        :name 'pick-apples
                        :contributing-part (list (make-instance 'contributing-unit
                                                                :name '?pick-apples-unit
                                                                :unit-structure '((sem-cat sem-apple-set)
                                                                                  (ref (?set))
                                                                                  (syn-cat syn-apple-set)
                                                                                  (subunits (?amount-unit))
                                                                                  (lb ?lb)
                                                                                  (rb ?rb)))
                                                 (make-instance 'contributing-unit
                                                                :name '?amount-unit
                                                                :unit-structure '((checks (used +)))))
                        :conditional-part (list 
                                           (make-instance 'conditional-unit
                                                          :name '?amount-unit
                                                          :formulation-lock '((sem-cat sem-quantity)
                                                                              (ref (?amount))
                                                                              (checks (not (used +)))
                                                                              (lb ?lb)
                                                                              (rb ?rb))
                                                          :comprehension-lock '((syn-cat syn-quantity)
                                                                                (checks (not (used +)))
                                                                                (lb ?lb)
                                                                                (rb ?rb)))
                                           (make-instance 'conditional-unit
                                                          :name '?pick-apples-unit
                                                          :formulation-lock '((HASH meaning ((pick-apples ?set ?amount))))
                                                          :comprehension-lock nil))
                        :attributes '((:label . phrasal) (:score . 0.5))
                        :feature-types '((args sequence)
                                         (ref sequence)
                                         (form set-of-predicates)
                                         (meaning set-of-predicates)
                                         (subunits sequence))
                        :cxn-inventory *apple-constructions*)
         *apple-constructions*)

(add-cxn (make-instance 'fcg-construction
                        :name 'add-apples
                        :contributing-part (list (make-instance 'contributing-unit
                                                                :name '?add-apples-unit
                                                                :unit-structure '((sem-cat sem-apple-set)
                                                                                  (ref (?sum))
                                                                                  (syn-cat syn-apple-set)
                                                                                  (subunits (?amount-unit ?set-unit))
                                                                                  (lb ?a-lb)
                                                                                  (rb ?s-rb)))
                                                 (make-instance 'contributing-unit
                                                                :name '?amount-unit
                                                                :unit-structure '((checks (used +))))
                                                 (make-instance 'contributing-unit
                                                                :name '?set-unit
                                                                :unit-structure '((checks (used +)))))
                        :conditional-part (list 
                                           (make-instance 'conditional-unit
                                                          :name '?amount-unit
                                                          :formulation-lock '((sem-cat sem-quantity)
                                                                              (ref (?amount))
                                                                              (checks (not (used +)))
                                                                              (lb ?a-lb)
                                                                              (rb ?a-rb))
                                                          :comprehension-lock '((syn-cat syn-quantity)
                                                                                (checks (not (used +)))
                                                                                (lb ?a-lb)
                                                                                (rb ?a-rb)))
                                           (make-instance 'conditional-unit
                                                          :name '?set-unit
                                                          :formulation-lock '((sem-cat sem-apple-set)
                                                                              (ref (?set))
                                                                              (checks (not (used +)))
                                                                              (lb ?s-lb)
                                                                              (rb ?s-rb))
                                                          :comprehension-lock '((syn-cat syn-apple-set)
                                                                                (checks (not (used +)))
                                                                                (lb ?s-lb)
                                                                                (rb ?s-rb)))
                                           (make-instance 'conditional-unit
                                                          :name '?add-apples-unit
                                                          :formulation-lock '((HASH meaning ((add-apples ?sum ?amount ?set))))
                                                          :comprehension-lock '((HASH form ((meets ?a-rb ?s-lb))))))
                        :attributes '((:label . phrasal) (:score . 0.6))
                        :feature-types '((args sequence)
                                         (ref sequence)
                                         (form set-of-predicates)
                                         (meaning set-of-predicates)
                                         (subunits sequence))
                        :cxn-inventory *apple-constructions*)
         *apple-constructions*)

(add-cxn (make-instance 'fcg-construction
                        :name 'multiply-apples
                        :contributing-part (list (make-instance 'contributing-unit
                                                                :name '?multiply-apples-unit
                                                                :unit-structure '((sem-cat sem-apple-set)
                                                                                  (ref (?sum))
                                                                                  (syn-cat syn-apple-set)
                                                                                  (subunits (?factor-unit ?amount-unit ?set-unit))
                                                                                  (lb ?f-lb)
                                                                                  (rb ?s-rb)))
                                                 (make-instance 'contributing-unit
                                                                 :name '?factor-unit
                                                                 :unit-structure '((checks (used +))))
                                                  (make-instance 'contributing-unit
                                                                 :name '?amount-unit
                                                                 :unit-structure '((checks (used +))))
                                                  (make-instance 'contributing-unit
                                                                 :name '?set-unit
                                                                 :unit-structure '((checks (used +)))))
                        :conditional-part (list
                                           (make-instance 'conditional-unit
                                                          :name '?factor-unit
                                                          :formulation-lock '((sem-cat sem-quantity)
                                                                              (ref (?factor))
                                                                              (checks (not (used +)))
                                                                              (lb ?f-lb)
                                                                              (rb ?f-rb))
                                                          :comprehension-lock '((syn-cat syn-quantity)
                                                                                (checks (not (used +)))
                                                                                (lb ?f-lb)
                                                                                (rb ?f-rb)))
                                           (make-instance 'conditional-unit
                                                          :name '?amount-unit
                                                          :formulation-lock '((sem-cat sem-quantity)
                                                                              (ref (?amount))
                                                                              (checks (not (used +)))
                                                                              (lb ?a-lb)
                                                                              (rb ?a-rb))
                                                          :comprehension-lock '((syn-cat syn-quantity)
                                                                                (checks (not (used +)))
                                                                                (lb ?a-lb)
                                                                                (rb ?a-rb)))
                                           (make-instance 'conditional-unit
                                                          :name '?set-unit
                                                          :formulation-lock '((sem-cat sem-apple-set)
                                                                              (ref (?set))
                                                                              (checks (not (used +)))
                                                                              (lb ?s-lb)
                                                                              (rb ?s-rb))
                                                          :comprehension-lock '((syn-cat syn-apple-set)
                                                                                (checks (not (used +)))
                                                                                (lb ?s-lb)
                                                                                (rb ?s-rb)))
                                           (make-instance 'conditional-unit
                                                          :name '?multiply-apples-unit
                                                          :formulation-lock '((HASH meaning ((multiply-apples ?sum ?factor ?amount ?set))))
                                                          :comprehension-lock '((HASH form ((meets ?f-rb ?a-lb)
                                                                                            (meets ?a-rb ?s-lb))))))
                        :attributes '((:label . phrasal) (:score . 0.7))
                        :feature-types '((args sequence)
                                         (ref sequence)
                                         (form set-of-predicates)
                                         (meaning set-of-predicates)
                                         (subunits sequence))
                        :cxn-inventory *apple-constructions*)
         *apple-constructions*)

;;;;; #########################################################################
;;;;; 9. Production

;; we apply the constructions to the meaning created by the composer

;; we activate this monitor to see the details of the application process
(activate-monitor trace-fcg-light)

;; production. The meaning that is given to FCG is the irl program of
;; the composed chunk plus bind statements its open variables:
(formulate (create-meaning (first (solutions *apples-composer*)))
           :cxn-inventory *apple-constructions*)

;; -> the utterance is "quatre vingt dix sept", which is indeed kind
;; of French for the amount of 97 

;;;;; #########################################################################
;;;;; 10. Interpretation 

;; Now we test the interpretation of French numbers

;; we apply the construcions to an utterance. The result is an irl program.
(defparameter *irl-program* 
  (comprehend '("quatre" "vingt" "dix" "sept")
              :cxn-inventory *apple-constructions*))

;;;;; #########################################################################
;;;;; 11. Semantic interpretation

;; when the irl program returned by linguistic interpretation is complete,
;; we can just evaluate it directly
(evaluate-irl-program *irl-program* *apples-ontology*)

;; -> The value that is bound to the first slot of the multiply-apples
;; primitive is a set of 97 apples.

;;;;; #########################################################################
;;;;; 12. Semantic Interpretation with Partial IRL Networks

;; sometimes (or most of the times in an actual experiment), the irl
;; program interpreted by FCG is not complete.

;; to simulate this, we remove the add-apples primitive from the irl network
(defparameter *partial-irl-program*
  (remove 'add-apples *irl-program* :key #'car))

;; but we can still interpret the utterance by using the composer to
;; find and evalute a chunk that matches the partial meaning
(defparameter *apples-composer*
  (make-instance 
   'single-topic-composer
   :ontology *apples-ontology*
   :initial-chunk *initial-chunk*
   :meaning *partial-irl-program*))

;; we get one solution
(get-next-solutions *apples-composer*)

;; -> The result is an apple set consisting of 97 apples.  





