
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;; How to Use the Integrated Meta-Layer of Fluid Construction Grammar ;;
;; August 2016 - Katrien & Paul                                       ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :fcg)
(in-package :fcg)
(activate-monitor trace-fcg)

;; 1. Introduction
;; ---------------

;; The meta-layer-learning package is now compeletely integrated in FCG, providing
;; hooks for problem solving and learning at the right spots (we hope). This demo
;; walks through an example and aims to explain everything you need to know for writing
;; and using your own diagnostic and repairs in FCG

;; 2. Terminology and high-level description
;; -----------------------------------------

;; -> Routine processing <-
;; This is the normal workflow of FCG. Starting with the initial transient structure, the
;; constructions from the construction inventory can apply and at each point create a new transient structure.
;;
;; -> Meta layer processing <-
;; After each construction application, routine processing can be interrupted and FCG can jump to its meta-layer. At this meta-
;; layer, (almost) anything can be achieved. Then FCG jumps back to routine processing and the normal
;; workflow continues.
;;
;; -> Diagnostics <-
;; Diagnostics are a kind of 'tests' that are run after every construction application (or more accurately
;; after the creation of any new node in the search tree). Diagnostics can trigger problems, which are stored
;; in the node.
;;
;; -> Repairs <-
;; are strategies to solve problems. They are specialised towards a specific (class of) problems. They return
;; 'fixes'
;;
;; -> Fixes <-
;; Fixes are created by repairs and contain a solution tailored towards the specific problem. The fixes are
;; automatically put into effect by a handle-fix method
;;
;; -> Consolidation <-
;; When a branch of a search tree leads to a solution, the constructions that were created on the fly by a repair are added
;; to the construction-inventory. If not, then they are not added.
;;
;; Routine Processing -> Diagnostic            Routine-Processing ->  Solution
;;                            I                         I                  I
;;                         Problem    ->  Repair   ->  Fix             Consolidation
;;
;; 3. Example for unknown words
;; ----------------------------
;;
;; Say, we have a grammar but our input text contains out-of-vocabulary words. For these words, we want to make lexical
;; constructions on the fly using the meta-layer. What do we need to do?

;; 3.1. Make specialised subclasses for the problem, a diagnostic and a repair strategy.
;; --------------------------------------------------------------------------------------

;; Problem class: demo-unknown-words inherits from problem
;; Make a new class for each problem
(defclass demo-unknown-words (problem)
  ())

;; Diagnostic class: demo-diagnose-unknown-words inherits from diagnostic
;; Make a new class for each diagnostic. Don't change the trigger.
(defclass demo-diagnose-unknown-words (diagnostic)
  ((trigger :initform 'new-node)))

;; Repair class: demo-add-lexical-cxn inherits from repair
;; Make a new class for each repair strategy. Don't change the trigger.
(defclass demo-add-lexical-cxn (repair) 
  ((trigger :initform 'new-node)))

;; 3.2. Write a diagnose method for your problem
;; --------------------------------------------------------------------------------------

;; The diagnose method is run on every node and should detect a problem and return it.
;; In this case, it checks wheter there are any strings left in the root after all
;; other constructions applied. If so, it returns a problem with the strings in its 'strings data-field.

(defmethod diagnose ((diagnostic demo-diagnose-unknown-words) (node cip-node)
                     &key &allow-other-keys)
  "Diagnose that the fully expanded structure contains untreated strings"
  (when (fully-expanded? node) ;; Check whether the node is fully expanded, i.e. no other construction can apply anymore
    (let ((strings-in-root (get-strings (assoc 'root
                                               (left-pole-structure
                                                (car-resulting-cfs (cipn-car node))))))) ;; Collect all strings that are still in the root
      (when strings-in-root
        (let ((problem (make-instance 'demo-unknown-words)))
          (set-data problem 'strings strings-in-root)
          problem))))) ;; Return a problem with the left-over strings in its 'strings'-data field


;; 3.3. Write a repair method for your problem
;; --------------------------------------------------------------------------------------

;; The repair method is run on every problem and should return a fix. This repair method
;; returns a fix of the type cxn-fix, which means that the fix has a construction in its
;; :restart-data slot.
;; This repair introduces a new lexical construction for the first unknown word.
;; The construction will automatically be applied by a handle-fix method (see below).

(defmethod repair ((repair demo-add-lexical-cxn)
                   (problem demo-unknown-words)
                   (node cip-node)
                   &key &allow-other-keys)
  "Repair by making a new lexical construction for the first untreated string"
  (let ((uw (first (get-data problem 'strings)))) 
    (multiple-value-bind (cxn-set lex-cxn)
        (eval `(def-fcg-cxn ,(make-symbol (upcase (string-append uw "-cxn")))
                            ((?word-unit
                              (args (?ref))
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

;; 3.4. Handle-fix (you can reuse the default one for cxn-fix!)
;; --------------------------------------------------------------------------------------

;; The handle fix method determines what will happen with the fix. The one for cxn-fixes apply the construction
;; to the transient structure so far and returns the new cxn-application-result.
;; Whatever you do, always return an object from the class cxn-application result, otherwise you should have
;; a look at next-cip-solution and make our implementation better.

;; (defmethod handle-fix ((fix cxn-fix) (repair repair) (problem problem) (node cip-node) &key &allow-other-keys)
;;  "Apply the construction provided by fix tot the result of the node and return the construction-application-result"
;;  (call-next-method)
;;  (with-disabled-monitor-notifications
;;    (set-data fix 'fixed-cars
;;              (fcg-apply (get-processing-cxn (restart-data fix)) (car-resulting-cfs (cipn-car node)) (direction (cip node))))))

;; 4. Demo of the Example
;; --------------------------------------------------------------------------------------

;; 4.1 A small grammar
;; -------------------

;; This is a very small grammar (NP,VP,transitive clause) and lexical entries for mouse, linguist and likes only.
;; Note that we have added the names of the diagnostics and repairs that we have written above to their respective
;; slots in the def-fcg-construction macro:

(def-fcg-constructions meta-layer-demo-grammar
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (constituents sequence)
                  (dependents sequence))
  :hierarchy-features (constituents dependents)
  :diagnostics (demo-diagnose-unknown-words)
  :repairs (demo-add-lexical-cxn)
  :fcg-configurations ((:consolidate-repairs . t))
  
  ;; Lexical constructions
  (def-fcg-cxn the-cxn
               ((?the-word
                 (args (?x))
                 (sem-cat (sem-class referent))
                 (syn-cat (lex-class article)))
                <-
                (?the-word
                 (HASH meaning ((unique ?x)))                     
                 --
                 (HASH form ((string ?the-word  "the"))))))

  (def-fcg-cxn mouse-cxn
               ((?mouse-word
                 (args (?x))
                 (sem-cat (sem-class physical-entity))
                 (syn-cat (lex-class noun)))
                <-
                (?mouse-word
                 (HASH meaning ((mouse ?x)))                     
                 --
                 (HASH form ((string ?mouse-word  "mouse"))))))
  
  (def-fcg-cxn likes-cxn
               ((?likes-word
                 (args (?x ?y))
                 (sem-cat (sem-class relation))
                 (syn-cat (lex-class verb)
                          (type transitive)))
                <-
                (?likes-word
                 (HASH meaning ((deep-affection ?x ?y)))                     
                 --
                 (HASH form ((string ?likes-word  "likes"))))))
  
  (def-fcg-cxn linguist-cxn
               ((?linguist-word
                 (args (?x))
                 (sem-cat (sem-class physical-entity))
                 (syn-cat (lex-class noun)))
                <-
                (?linguist-word
                 (HASH meaning ((linguist ?x)))                     
                 --
                 (HASH form ((string ?linguist-word  "linguist"))))))
  
  ;;Grammatical Constructions
  ;; NP -> ART NOUN
  (def-fcg-cxn noun-phrase-cxn
               ((?noun-phrase
                 (args (?x))
                 (sem-cat (sem-class referring-expression))
                 (syn-cat (lex-class noun-phrase))
                 (constituents (?article ?noun)))
                (?noun
                 (dependents (?article)))
                <-
                (?article
                 (args (?x))
                 (sem-cat (sem-class referent))
                 --
                 (syn-cat (lex-class article)))
                (?noun
                 (args (?x))
                 (sem-cat (sem-class physical-entity))                   
                 --
                 (syn-cat (lex-class noun)))
                (?noun-phrase
                 --
                 (HASH form ((meets ?article ?noun))))))
  
  ;; VP -> V
  (def-fcg-cxn verb-phrase-cxn
               ((?verb-phrase
                 (args (?x ?y))
                 (sem-cat (sem-class relational-expression))
                 (syn-cat (lex-class verb-phrase)
                          (type transitive))
                 (constituents (?verb)))
                <-
                (?verb
                 (args (?x ?y))
                 (sem-cat (sem-class relation))
                 --
                 (syn-cat (lex-class verb)
                          (type transitive)))))
  
  ;; Transitive-clause -> NP VP NP
  (def-fcg-cxn transitive-clause-cxn
               ((?transitive-clause
                 (args (?x ?y))
                 (sem-cat (sem-class predicating-expression))
                 (syn-cat (lex-class transitive-clause))
                 (constituents (?subject-noun-phrase ?verb-phrase ?object-noun-phrase)))
                (?verb
                 (dependents (?subject-noun ?object-noun)))
                <-
                (?subject-noun-phrase
                 (args (?x))
                 (sem-cat (sem-class referring-expression))
                 (constituents (?subject-article ?subject-noun))
                 --
                 (syn-cat (lex-class noun-phrase))
                 (constituents (?subject-article ?subject-noun)))
                (?verb-phrase
                 (args (?x ?y))
                 (sem-cat (sem-class relational-expression))
                 (constituents (?verb))
                 --
                 (syn-cat (lex-class verb-phrase)
                          (type transitive))
                 (constituents (?verb)))
                (?object-noun-phrase
                 (args (?y))
                 (sem-cat (sem-class referring-expression))
                 (constituents (?object-article ?object-noun))
                 --
                 (syn-cat (lex-class noun-phrase))
                 (constituents (?object-article ?object-noun)))
                (?transitive-clause
                 --
                 (HASH form ((meets ?subject-noun ?verb)
                             (meets ?verb ?object-article)))))))

;; 4.2. Routine Processing
;; ----------------------

;; The following utterance can be comprehended without any problems, all words are known.
;; In the web-interface, you can see that all nodes are green, and the final node (solution)
;; is dark green
(comprehend '("the" "mouse" "likes" "the" "linguist"))

;; 4.3. Meta-layer processing
;; -------------------------

;; The following utterance cannot be comprehended in routine processing: both bird and cat are unknown words. Therefore, we
;; diagnose two times an unknown-word problem (orange nodes) en repair it two times with a new lexical construction (yellow nodes).
;; Observe that the tree leads to a solution (dark green node).
(comprehend '("the" "bird" "likes" "the" "cat"))


;; 4.4. Consolidation
;; -------------------

;; As the previous use of the meta-layer led to a solution, the freshly made constructions were consolidated (added to the cxn-inventory).
;; Now, these construction can be reused in routine processing. Observe the green nodes for the same sentence now:
(comprehend '("the" "bird" "likes" "the" "cat"))

;; The learned constructions can now also be used in formulation, in routine processing
(formulate '((unique o-1)(bird o-1)(unique o-2)(cat o-2)(deep-affection o-1 o-2)))


;; 5. Configuration Options
;; ------------------------

;; Handling diagnostics and repairs, as well as consolidating repair constructions is done by default by FCG. you can however add
;; options to turn off consolidation of repairs (this means problems will be solved, but the solutions will not be learned/stored):

(set-configuration *fcg-constructions* :consolidate-repairs nil)

;; Or you can turn off the meta-layer completely. FCG will never stop routine processing.

(set-configuration *fcg-constructions* :use-meta-layer nil)
 