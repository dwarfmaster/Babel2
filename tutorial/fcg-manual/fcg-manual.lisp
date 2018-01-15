;; ###############
;; # FCG MANUAL #
;; ##############

;; -----------
;; # OUTLINE #
;; -----------

;; 1. Introduction
;; 2. Download, Installation and Start-up
;; 3. The basics of FCG
;;     3.1. Defining a construction inventory
;;     3.2. Defining a construction
;;     3.3. The three faces of FCG
;;     3.4. Using a grammar in comprehension and formulation
;; 4. Feature Types
;; 5. Construction sets
;; 6. Configurations for processing
;; 7. Non-default construction inventories and 'add-cxn'
;; 8. For FCG Developpers

;; -------------------
;; # 1. Introduction #
;; -------------------

;; This technical document describes the notational conventions for the current implementation of
;; Fluid Construction Grammar (FCG). The notation is based on the formalisation of FCG described
;; in Steels (2015). It focusses on the implementation and therefore assumes you have read this
;; paper. For any questions, do not hesitate to contact us at fcg-mailing@ai.vub.ac.be
;; (visit https://ai.vub.ac.be/mailman/listinfo/fcg-mailing} to subscribe to the mailing list!.

;; ------------------------------------------
;; # 2. Download, Installation and Start-up #
;; ------------------------------------------

;; Fluid Construction Grammar is released as a part of the Babel 2 framework and runs on Mac OSX,
;; Linux and Windows. Instructions for downloading and installing Babel 2 can be found at
;; emergent-languages.org/Babel2.

;; After having successfully installed Babel 2, you can use this manual in an interactive way.
;; Evaluate the following line to load the FCG package and to start the web-interface. Then, open
;; a web browser (preferrably Safari or Firefox) at http://localhost:8000. 

;; You have to evaluate these two lines every time you use FCG.
(asdf:make :fcg)
(in-package :fcg)
(activate-monitor trace-fcg)

;; ------------------------
;; # 3. The basics of FCG #
;; ------------------------

;; 3.1. Defining a construction inventory
;; --------------------------------------

;; Before you can start to write your own FCG grammar, you will have to define a construction-
;; inventory. This is the place where all constructions will be stored. A new construction- 
;; inventory can be defined in its simplest form by executing the following macro. You will see
;; that it is initially empty (0 constructions). The construction-inventory is stored by default
;; in the local variable *fcg-constructions*.

(def-fcg-constructions my-first-grammar
  :fcg-configurations ((:node-tests :update-references :check-duplicate :restrict-nr-of-nodes)
                       (:de-render-mode . :de-render-with-scope)
                       (:render-mode . :render-with-scope)))

;; 3.2. Defining a construction
;; ----------------------------

;; Now that we have defined a construction-inventory, we can define our first construction. The
;; easiest way to do this is by using the def-fcg-cxn macro. This macro takes the construction
;; name as first argument ('mouse-cxn' in the example below) and a representation of the construction
;; as a second argument. The representation of the construction consists of two parts. In the upper
;; part, the contributing units (= Left-hand-side of a construction in Steels (2015)) of the construction
;; are defined. In the following 'mouse-cxn', there is only one contributing unit, namely '?mouse-unit'.

;; Then, the arrow seperating the contributing part and the conditional part of the construction is
;; given by the symbol '<-'. Under the arrow, the conditional part of the construction (= Right-hand-side
;; of the construction in Steels (2015)) is declared. The conditional part can at its turn consists of
;; different conditional units (but our example construction has only one). Every conditional unit consists
;; of the unit name and then two parts, the formulation-lock and the comprehension-lock. The formulation lock
;; is given immediately after the unit-name and is followed by the symbol '--'. Then, the comprehension lock
;; is declared and the unit is closed.

;; For requiring a feature to be present in the input and for relocating it to an other unit, you can put
;; 'HASH' in front of the feature name. This correponds to the '#' symbol used in Steels (2015).

;; Now you can evaluate the following 'mouse-cxn'. It will be added to the construction-inventory that we
;; have defined above.

(def-fcg-cxn mouse-cxn
             ((?mouse-unit
               (args ?x)
               (syn-cat (class noun))
               (sem-cat (sem-type physical-entity)))
              <-
              (?mouse-unit
               (HASH meaning ((mouse ?x)))
               --
               (HASH form ((string ?mouse-unit "mouse"))))))

;; 3.3. The three faces of FCG
;; ---------------------------

;; There exists three representation forms for FCG constructions. First of all, there is the 'Lisp notation'
;; used in this manual. This is the 'base notation' that actually runs. Second, there is the interactive
;; web-interface representation, which is automatically created based on the 'Lisp notation'. Third, there
;; is the LaTeX notation used in FCG papers and slideshows. The LaTeX notation is also directly translated
;; from the Lisp notation.

;; Let's inspect the 'mouse-cxn' that we have just defined in the other representations. If you go to your
;; web browser (which should still be at https://localhost:8000), you will see that it displays the fact
;; that you added 1 construction to the construction inventory. Click on the blue box with 'mouse-cxn' in
;; order to expand it. You will see the the contributing part on the left side, the arrow, and the conditional
;; part on the right side, showing all the units in the construction. Now click on 'expand' (or on the units one
;; by one). You will see all the features in the units nicely formatted, and the two locks in the conditional
;; units.

;; If you hover over the name of a construction, some symbols appear above it. If you click the 'L' symbol,
;; a file with the LaTeX source code for this construction will automatically download. Paste this code into
;; an existing LaTeX document, compile, and you will see the construction in its LaTeX notation.


;; 3.4. Using a grammar in comprehension and formulation
;; -----------------------------------------------------

;; Now that we have defined a construction inventory and 1 construction, it is time to add some more constructions
;; and use them for the very thing FCG was implemented for: processing. Evaluate the following to simple
;; constructions. They will be added to the same construction inventory which you defined above.

(def-fcg-cxn the-cxn
             ((?the-unit
               (args ?x)
               (syn-cat (class quantifier))
               (sem-cat (sem-type referent)))
              <-
              (?the-unit
               (HASH meaning ((unique ?x)))
               --
               (HASH form ((string ?the-unit "the"))))))

(def-fcg-cxn np-cxn
             ((?np-unit
               (args ?x)
               (syn-cat (class referring-expression))
               (subunits (?mouse-unit ?the-unit)))
              <-
              (?the-unit
               (sem-cat (sem-type referent))
               (args ?x)
               --
               (syn-cat (class quantifier)))
              (?mouse-unit
               (sem-cat (sem-type physical-entity))
               (args ?x)
               --
               (syn-cat (class noun)))
              (?np-unit
               --
               (HASH form ((meets ?the-unit ?mouse-unit ?np-unit))))))

;; We can now use the 'mouse-cxn' 'the-cxn' and 'np-cxn in processing'. FCG is a bidirectional formalism
;; in the sense that it uses the same constructions and processing mechanisms for both directions. Evaluate
;; the following lines for testing the processing. Look at the web interface for seeing the construction
;; applicatation process and results. For clarity reasons, it might be a good idea to refresh your web
;; browser every between two construction applications.

;; In comprehension
(comprehend '("the" "mouse") :cxn-inventory *fcg-constructions*)
(comprehend '("mouse" "the"))

;; In formulation
(formulate '((mouse obj) (unique obj)))

;; Apart from 'comprehend' and 'formulate' which return the first solution they found, there exist also
;; 'comprehend-all' and 'formulate-all', which examine the whole search tree and return all solutions.
;; For the tiny grammar above, these will make no difference, as there is only 1 solution.

;; It can be very informative to inspect the construction application process in the web-interface. Click
;; two times (don't 'double click') on one of the nodes in the construction application path (green). Now,
;; you can see the transient structure before the application of this construction, the applied construction,
;; and the transient structure after the application. Expand/minimize units and nodes by clicking on them.


;; --------------------
;; # 4. Feature Types #
;; --------------------

;; Linguistic information is represented using feature-value pairs. Such
;; pairs consist of a FEATURE-NAME (or attribute) and a VALUE for that feature.
;; In bracketed notation, this looks as follows:
;;
;;          (feature-name value)
;;
;; The FCG-system is an open-ended system that lets you decide yourself which
;; features are relevant for the language that you're describing and which
;; values they can take. That is, there are no linguistic type restrictions on
;; feature value pairs. Moreover, the feature-names and values are 'arbitrary'
;; in the sense that they do not have any meaning outside the grammar. In oter terms, the way
;; in which they are used inside one grammar is their only meaning.
;;
;; However there are formal type restrictions on feature-values that affect
;; the way that feature behaves during processing (the way that features are matched and merged).
;; The value types that are implemented are:
;; - Sequence
;; - Set
;; - Set of predicates
;; - All the rest that does not fit in the above categories
;;
;; 4.1 Sequence
;; ------------
;; Sequences are ORDERED LISTS. For example, you can imagine a feature called
;; WORD-FORM whose value is a list of ordered strings. For instance:
;;
;;          (word-form ("song" "-s"))
;;          (word-form ("song" "zero-marking"))
;;
;; When such a feature-value pair occurs in a formulation or comprehension lock,
;; the construction specifies that it can only contribute its information if an
;; "exact pattern match" can be found in the transient structure. We can manually
;; test this using the function TEST-MATCH, which takes the value from a construction's
;; lock as its first argument, and a potential match from the transient structure as its
;; second value:

(test-match ("song" "-s") ("song" "-s") :type sequence)

;; When evaluating this, the result is a list of HYPOTHESES (see further below). In our example,
;; the return value is (((T . T))), which is a list that contains one solution: ((T . T)), which
;; means that FCG's unification algorithm found an exact match. The following test fails because
;; the two values do not match:

(test-match ("song" "-s") ("song" "zero-marking") :type sequence)

;; In other words, if a construction contains the following information in one of its locks:
;;              (word-form ("song" "-s"))
;; It can only be unlocked if a match for that value is found in the transient structure.
;;
;; +++++++++++++++++++++++++++++++++++++++
;; 4.2 Variables and variable substitution
;; +++++++++++++++++++++++++++++++++++++++
;;
;; Often, a construction will UNDERSPECIFY information. For example, the plural-s construction
;; may not care about the first string in the value of the feature word-form. In this case, we
;; can use VARIABLES to underspecify this information. Variables are symbols that start with a
;; question mark. So the plural-s form could contain the following feature-value pair:
;;               (word-form (?stem "-s"))
;;
;; Let's try to match value that against several other values:

(test-match (?stem "-s") ("song" "-s") :type sequence)
(test-match (?stem "-s") ("car" "-s") :type sequence)
(test-match (?stem "-s") ("song" "zero-marking") :type sequence)

;; The result value of the first test was (((?stem . "song"))). This means that the unification algorithm
;; found one solution: ((?stem . "song")). This result means that if we SUBSTITUTE the variable ?stem for
;; the value "song", we would get an exact match with the value of the transient structure:
;;
;; (?stem "-s")  ----> substitute ?stem with "song" ----> ("song" "-s")
;;
;; Similarly, in the second example, we get an exact match between the two values if we substitute ?stem for
;; the value "car". The third example fails, however, because we cannot find any substitution for the variable
;; ?stem that would make the two values the same.
;;
;; ++++++++++++++++++++++++
;; 4.3 Variable equalities
;; ++++++++++++++++++++++++
;;
;; Variables can be assigned ANY value, but once a value has been assigned to them, this value needs to be
;; constistently used throughout the construction. That is, if the same VARIABLE NAME occurs more than once,
;; it must always be bound to the same value.
;;
;; Example: suppose that we want to describe Pingelapese, a Micronesian language that features duplication and
;;          triplication. For instance, the word "mejr" (to sleep) can be reduplicated into "mejrmejr" (sleeping)
;;          and triplicated into "mejrmejrmejr" (still sleeping). We can imagine a feature called duplicated form
;;          that looks as follows:
;;
;;          (duplicated-form ("mejr" "mejr"))
;;
;;          The duplication-construction, however, should abstract away from the specific form and be compatible
;;          with any word that allows duplication. This can be achieved through VARIABLE EQUALITIES:
;;
;;          (duplicated-form (?form ?form))

(test-match (?form ?form) ("mejr" "mejr") :type sequence) ;; succeeds
(test-match (?form ?form) ("ha" "ha") :type sequence) ;; succeeds
(test-match (?form ?form) ("song" "-s") :type sequence) ;; fails

;; +++++++++++++++++++++
;; 4.4 Variable bindings
;; +++++++++++++++++++++
;;
;; While processing, the FCG-system will often have already found a binding for a variable. Suppose that the duplication
;; construction contains the following two feature-value pairs:
;;             (base-form ?base-form)
;;             (duplicated-form (?base-form ?base-form))
;;
;; Then it is possible that the unification algorithm has already found the following bindings:
;;
;;             (?base-form . "mejr")
;;
;; We can now exploit these bindings for restricting the search for good matches:

(test-match (?base-form ?base-form) ("mejr" "mejr") :type sequence
            :bindings ((?base-form . "mejr"))) ;; Succeeds.
(test-match (?base-form ?base-form) ("mejr" "mejr") :type sequence
            :bindings ((?base-form . "song"))) ;; Fails because inconsistent.

;; A powerful usage of variables is that they can be bound to other variables, which allows information to remain
;; underspecified until a value is found:

(test-match (?stem ?suffix) ("song" ?morpheme) :type sequence)

;; ++++++++++++++++++++++++++++++++++++++
;; 4.5 Sequences in the contributing 
;; ++++++++++++++++++++++++++++++++++++++
;;
;; When sequences occur in the contributing part of a construction, the FCG-system will try to "merge" that
;; information with information already present in the transient structure. In the simplest case, this simply
;; means adding the information:

(test-merge ("song" "-s") () :type sequence)

;; A second simple case is that the transient structure already contains the information. In this case, merging
;; needs to be compatible with that information. Just like with the conditional part, variables may be used.

(test-merge ("song" "-s") ("song" "-s") :type sequence) ;; Succeeds because values are the same.
(test-merge (?stem "-s") ("song" "-s") :type sequence) ;; Succeeds because ?stem can be substituted.
(test-merge (?stem "-s") ("song" "zero-marking") :type sequence) ;; Fails because information conflicts.

;; Sometimes, however, the information in the transient structure may be incomplete. In this case, merging will
;; only succeed if the information in the transient structure can be changed in a way that makes it compatible with
;; the information supplied by the construction. In this way, the plural-s construction may add the "-s" string:
(test-merge (?stem "-s") ("song") :type sequence)
;; Or the duplication construction may duplicate the single form of a basic lexical entry:
(test-merge (?base-form ?base-form) ("mejr") :type sequence)
;; Conflicting information, e.g. when the order is violated, will lead to failure:
(test-merge (?stem "-s") ("-s" "song") :type sequence)
;; Also be careful with variables, because they can be bound to any value:
(test-merge (?stem "-s") ("-s") :type sequence)


;; 4.6. Set and set-of-predicates
;; ------------------------------
;; The behavior of the unification algorithm changes to some kind of "subset" operation
;; if a feature-value is typed as a set or set-of-predicates. For instance, suppose that
;; we want represent the meaning of verbs according to Adele Goldberg's 1995 book
;; "Constructions: A constructional approach to argument structure". Goldberg writes that
;; a verb comes with a participant structure. For instance, a "buy"-event involves a
;; "buyer", a "seller" and something that is being "bought". We can use a first order predicate
;; calculus for representing that semantic frame:
;;
;; buy(?ev), buyer(?ev, ?x), seller(?ev, ?y), bought(?ev, ?z)
;;
;; In bracketed notation, this becomes:
;; ((buy ?ev) (buyer ?ev ?x) (seller ?ev ?y) (bought ?ev ?z))
;;
;; The meaning of the utterance "Sally bought a book from Pat" could thus be represented
;; as follows:
;; ((sally a) (pat b) (book c)
;;  (buy ev-1) (buyer ev-1 a) (seller ev-1 b) (bought ev-1 c))
;;
;; A lexical construction, such as the BOOK-construction, only needs to cover its own meaning
;; and does not care about other meanings that the speaker may want to express. In this case,
;; we do not want the unification algorithm to search for an EXACT match in the transient
;; structure, but to find a SUBSET of the value in the transient structure that matches with
;; the conditional value of the construction:

(test-match ((book ?x)) ((sally a) (pat b) (book c)) :type set-of-predicates)

;; This succeeds because the unification finds that ((book ?x)) can be matched against the
;; subset ((book c)). The two values are equal if we substitute ?x for the value c. Also order
;; does not matter anymore:

(test-match ((book ?x) (sally ?y)) ((sally a) (pat b) (book c)) :type set-of-predicates)

;; Of course, variable equalities DO matter:

(test-match ((book ?x) (sally ?x)) ((sally a) (pat b) (book c)) :type set-of-predicates)

;; +++++++++++++++++++++++++++++++
;; 4.7 Beware of variables in sets
;; +++++++++++++++++++++++++++++++
;;
;; Sets and set-of-predicates change to unification algorithm to its most lenient behavior.
;; You should therefore be very careful with the use of variables!
;;
(test-match (?x) ((sally a) (pat b) (book c)) :type set-of-predicates) ;; 3 hypotheses
(test-match (?x ?y) ((sally a) (pat b) (book c)) :type set-of-predicates) ;; 6 hypotheses

;; +++++++++++++++++++++++++++++
;; 4.8 Contributable information
;; +++++++++++++++++++++++++++++

(test-merge ((book ?X)) () :type set-of-predicates) ;; Succeeds
(test-merge ((book ?X)) ((book x)) :type set-of-predicates) ;; Succeeds
(test-merge ((book ?x)) ((sally y)) :type set-of-predicates) ;; Succeeds

;; Does the following succeed? Why (not)?
(test-merge ((book ?x)) ((book a)) :type set-of-predicates :bindings ((?x . b)))

;; 4.9. All the rest...
;; --------------------
;; Most features, however, are best handled by the unification algorithm's default
;; behavior (subset). This also means that you do not explicitly have to type them. This is especially
;; useful for features whose values are complex, e.g. values that are feature-value
;; pairs themselves.
;;
;; For example, the value agreement consists of a list of feature-value pairs
;; themselves (e.g. number and person):
;;
;;          (agreement (number sg) (person 3))
;;

(test-match ((number sg) (person 3)) ((number sg) (person 3)))
(test-match ((number ?n) (person ?p)) ((number sg) (person 3)))
(test-match ?agreement ((number sg) (person 3))) ;; Succeeds!! Why??

;; The difference (and utility) of the default behavior with respect to sets becomes
;; clear in MERGING. For example, if the value of the agreement feature is considered as a set,
;; then the following unwanted behavior may occur:

(test-merge ((number sg) (person 3)) ((number pl) (person 3)) :type set) ;; succeeds (unwanted!)

;; In the default behavior, every element in a feature's value may only occur once in
;; the transient structure's feature-value pair:

(test-merge ((number sg) (person 3)) ((number pl) (person 3))) ;; fails (as wanted)


;; -------------------------
;; # 8. For FCG Developers #
;; -------------------------

(add-cxn (make-instance 'fcg-construction
                        :name 'noun-phrase-cxn
                        :contributing-part (list (make-instance 'contributing-unit
                                                                :name '?noun-phrase
                                                                :unit-structure '((args (?x))
                                                                                  (sem-cat (sem-class referring-expression))
                                                                                  (syn-cat (lex-class noun-phrase))
                                                                                  (subunits (?article ?noun)))))
                        :conditional-part (list (make-instance 'conditional-unit
                                                               :name '?article
                                                               :formulation-lock '((args (?x))
                                                                                   (sem-cat (sem-class referent)))
                                                               :comprehension-lock '((syn-cat (lex-class article))))
                                                (make-instance 'conditional-unit
                                                               :name '?noun
                                                               :formulation-lock '((args (?x))
                                                                                   (sem-cat (sem-class physical-entity)))      
                                                               :comprehension-lock '((syn-cat (lex-class noun))))
                                                (make-instance 'conditional-unit
                                                               :name '?noun-phrase
                                                               :formulation-lock nil
                                                               :comprehension-lock '((HASH form ((meets ?article ?noun))))))
                        :attributes '((:label . cxn) (:score . 0.5))
                        :description "A construction for the noun phrase"
                        :cxn-inventory *fcg-constructions*)
         *fcg-constructions*)


                            



