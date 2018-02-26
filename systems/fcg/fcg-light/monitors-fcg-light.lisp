(in-package :fcg)

(export '(trace-fcg))

;; ############################################################
;; trace-fcg
;; ############################################################

(define-event added-fcg-cxn-set)

(define-monitor trace-fcg 
    :documentation "Traces results of fcg processing in a web browser")

(define-event-handler (trace-fcg cxn-deleted)
  (add-element '((hr)))
  (add-element 
   `((h4) "Removed&#160;&#160; " 
     ,(make-html construction :expand-initially nil) " from&#160;&#160;"
     ,(make-html construction-inventory))))

(define-event-handler (trace-fcg cxn-added)
  (when (get-configuration (visualization-configuration construction-inventory) :show-upper-menu)
    (clear-page)
    (interactive-web-interface-header))
  (add-element '((hr)))
  (add-element 
   `((h4) "Added&#160;&#160; " 
     ,(make-html construction :expand-initially nil) " to&#160;&#160;"
     ,(make-html construction-inventory))))


;; ------------------------------------------------------------
;; construction application

(define-event-handler (trace-fcg cxn-application-started)
  (add-element `((hr)))
  (add-element 
   `((h3) "Applying "
     ,(make-html (get-original-cxn cxn) :direction direction)
     "in "
     ,(if (eq direction '->) "formulation" "comprehension"))))

(define-event-handler (trace-fcg cxn-application-finished)
  (let ((cars (append failed-cars resulting-cars)))
    (cond ((> (length cars) 1)
           (add-element `((p) ((b) ,(length cars) " results:")))
           (loop for car in cars 
                 do (add-element '((hr)))
                 (add-element (make-html-fcg-light car
                                                   :feature-types (feature-types
                                                                   (original-cxn-set (cxn-inventory (car-applied-cxn car))))))))
          (cars (add-element `((div)
                               ,(make-html-fcg-light (first cars)
                                                     :feature-types (feature-types
                                                                     (original-cxn-set (cxn-inventory (car-applied-cxn (first cars)))))))))
          (t (add-element `((div) ((b) "no match")))))))

;; ------------------------------------------------------------
;; construction set application

(define-event-handler (trace-fcg cip-started)
  (add-element `((hr)))
  (add-element 
   `((h3) ,(if (children (top-node cip))
             "Computing next solution for application of "
             "Applying ")
     ; because constantly rendering the full construction inventory
     ; gets very slow with a large number of constructions, turn off
     ; rendering once the inventory gets larger than:
     ,(if (> (size (construction-inventory cip)) (get-configuration (construction-inventory cip) 
                                                                    :max-size-for-html))
        (format nil "a large ~a (~d)"
                (get-construction-inventory-title-string (original-cxn-set (construction-inventory cip)))
                (size (original-cxn-set (construction-inventory cip))))
     (make-html (original-cxn-set (construction-inventory cip))))
     #+:type-hierarchies ,(if (type-hierarchies::get-type-hierarchy (original-cxn-set (construction-inventory cip)))
                            (make-html (type-hierarchies::get-type-hierarchy (original-cxn-set (construction-inventory cip)))
                                       :weights? t :render-program "circo")
                           "")
     " in "
     ,(if (eq (direction cip) '->) "formulation" "comprehension"))))

(define-event-handler (trace-fcg cip-finished)
  (add-element '((hr)))
  (add-element (make-html-fcg-light cip :solutions (when solution (list solution)))))

(define-event-handler (trace-fcg cip-restart-requested)
  (add-element '((hr)))
  (add-element `((h4) "Restart requested"))
  (add-element (make-html-fcg-light (cip cipn))))

(define-event-handler (trace-fcg fcg-apply-w-n-solutions-started)
  (add-element `((hr)))
  (add-element 
   `((h3) ,(if (typep n 'integer)
             (format nil "Computing max ~a solutions for application of " n)
             "Computing all solutions for application of ")
     ,(make-html (original-cxn-set construction-inventory))
     #+:type-hierarchies ,(if (type-hierarchies::get-type-hierarchy (original-cxn-set construction-inventory))
                            (make-html (type-hierarchies::get-type-hierarchy (original-cxn-set (construction-inventory cip)))
                                       :weights? t :render-program "circo")
                           "")
     " in "
     ,(if (eq direction '->) "formulation" "comprehension"))))

(define-event-handler (trace-fcg fcg-apply-w-n-solutions-finished)
  (add-element '((hr)))
  (add-element (make-html-fcg-light cip :solutions solutions)))

(define-event-handler (trace-fcg added-fcg-cxn-set))

;; ------------------------------------------------------------
;; produce/ parse

(define-event-handler (trace-fcg produce-started)
  (when (get-configuration (visualization-configuration construction-inventory) :show-upper-menu)
    (clear-page)
    (interactive-web-interface-header))
  (add-element `((hr)))
  (add-element `((h2) "Formulating&#160;"))
  (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                 (predicate-network->svg meaning :only-variables nil)
                 (html-pprint meaning))))

(define-event-handler (trace-fcg produce-finished)
  (add-element `((h3) ,(format nil "Utterance: &quot;~{~a~^ ~}&quot;" utterance)))
  (add-element `((p) " ")))

(define-event-handler (trace-fcg produce-all-started)
  (when (get-configuration (visualization-configuration construction-inventory) :show-upper-menu)
    (clear-page)
    (interactive-web-interface-header))
  (add-element `((hr)))
  (add-element `((h2) ,(format
                        nil
                        "Formulating (~a)&#160;"
                        (if (typep n 'number)
                          (format nil "max ~a solutions" n)
                          "all solutions"))))
  (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                 (if (get-configuration (visualization-configuration construction-inventory) :show-wiki-links-in-predicate-networks )
                   (predicate-network-with-wiki-links->svg meaning :only-variables nil)
                   (predicate-network->svg meaning :only-variables nil))
                 (html-pprint meaning))))
     
(define-event-handler (trace-fcg produce-all-finished)
  (add-element `((h3) ,(format nil "Utterances: &quot;~{~{~a~^ ~}~^, ~}&quot;" utterances)))
  (add-element `((p) " ")))

(define-event-handler (trace-fcg parse-started)
  (when (get-configuration *default-visualization-configuration* :show-upper-menu)
    (clear-page)
    (interactive-web-interface-header))
  (add-element `((hr)))
  (add-element `((h2) ,(format nil "Comprehending &quot;~{~a~^ ~}&quot;" utterance))))

(define-event-handler (trace-fcg parse-finished)
  (add-element `((h3 :style "margin-bottom:3px;") "Meaning:"))
  (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                 (if (get-configuration (visualization-configuration construction-inventory) :show-wiki-links-in-predicate-networks )
                   (predicate-network-with-wiki-links->svg meaning)
                   (predicate-network->svg meaning))
                 (html-pprint meaning)))
  (add-element `((p) " ")))

(define-event-handler (trace-fcg parse-all-started)
  (when (get-configuration *default-visualization-configuration* :show-upper-menu)
    (clear-page)
    (interactive-web-interface-header))
  (add-element `((hr)))
  (add-element `((h2) ,(format nil "Comprehending &quot;~{~a~^ ~}&quot; (~a)"
                               utterance
                               (if (typep n 'number)
                                 (format nil "max ~a solutions" n)
                                 "all solutions")))))

(define-event-handler (trace-fcg parse-all-finished)
  (add-element `((h3 :style "margin-bottom:3px;") "Parsed meanings"))
  (loop for meaning in meanings
        for i from 1
        do
        (add-element `((h4) ,(format nil "meaning ~a " i)))
        (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                       (if (get-configuration (visualization-configuration construction-inventory) :show-wiki-links-in-predicate-networks )
                         (predicate-network-with-wiki-links->svg meaning)
                         (predicate-network->svg meaning))
                       (html-pprint meaning))))
  (add-element `((p) " ")))

;; ############################################################
;; trace-fcg-search-process
;; ############################################################

(export '(trace-fcg-search-process))

(define-monitor trace-fcg-search-process
    :documentation "Traces the fcg light search process in a web browser")

(define-event-handler (trace-fcg-search-process cip-next-node)
  (add-element '((hr)))
  (add-element 
   `((table :class "two-col")
     ((tbody) ,(make-tr-for-cip-tree-fcg-light cipn "next node" 
                                               :hide-subtree-with-duplicates nil)))))

(define-event-handler (trace-fcg-search-process cip-node-expanded)
  (add-element `((table :class "two-col")
                 ((tbody)
                  ,(make-tr-for-cip-tree-fcg-light cipn "expansion" 
                                     :hide-subtree-with-duplicates nil)
                  ,(make-tr-for-cip-tree-fcg-light (top-node (cip cipn)) "new tree" )
                  ,(make-tr-for-cip-queue-fcg-light (cip cipn) "new queue" )))))

;; ############################################################
;; trace-fcg-debugging-mode
;; ############################################################
;; runs evaluation functions

(export '(trace-fcg-debugging))

(define-monitor trace-fcg-debugging
    :documentation "Traces results of fcg in a web browser, with the evaluation package activated")

(define-event-handler (trace-fcg-debugging cxn-deleted)
  (add-element '((hr)))
  (add-element 
   `((h4) "Removed&#160;&#160; " 
     ,(make-html construction :expand-initially nil) " from&#160;&#160;"
     ,(make-html construction-inventory))))

(define-event-handler (trace-fcg-debugging cxn-added)
  (when (get-configuration (visualization-configuration construction-inventory) :show-upper-menu)
    (clear-page)
    (interactive-web-interface-header))
  (add-element '((hr)))
  (add-element 
   `((h4) "Added&#160;&#160; " 
     ,(make-html construction :expand-initially nil) " to&#160;&#160;"
     ,(make-html construction-inventory))))


;; ------------------------------------------------------------
;; construction application

(define-event-handler (trace-fcg-debugging cxn-application-started)
  (add-element `((hr)))
  (add-element 
   `((h3) "Applying "
     ,(make-html (get-original-cxn cxn) :direction direction)
     "in "
     ,(if (eq direction '->) "formulation" "comprehension"))))

(define-event-handler (trace-fcg-debugging cxn-application-finished)
  (let ((cars (append failed-cars resulting-cars)))
    (cond ((> (length cars) 1)
           (add-element `((p) ((b) ,(length cars) " results:")))
           (loop for car in cars 
                 do (add-element '((hr)))
                 (add-element (make-html-fcg-light car
                                                   :feature-types (feature-types
                                                                   (original-cxn-set (cxn-inventory (car-applied-cxn car))))))))
          (cars (add-element `((div)
                               ,(make-html-fcg-light (first cars)
                                                     :feature-types (feature-types
                                                                     (original-cxn-set (cxn-inventory (car-applied-cxn (first cars)))))))))
          (t (add-element `((div) ((b) "no match")))))))

;; ------------------------------------------------------------
;; construction set application

(define-event-handler (trace-fcg-debugging cip-started)
  (add-element `((hr)))
  (add-element 
   `((h3) ,(if (children (top-node cip))
             "Computing next solution for application of "
             "Applying ")
     ; because constantly rendering the full construction inventory
     ; gets very slow with a large number of constructions, turn off
     ; rendering once the inventory gets larger than:
     ,(if (> (size (construction-inventory cip)) (get-configuration (construction-inventory cip) 
                                                                    :max-size-for-html))
        (format nil "a large ~a (~d)"
                (get-construction-inventory-title-string (original-cxn-set (construction-inventory cip)))
                (size (original-cxn-set (construction-inventory cip))))
     (make-html (original-cxn-set (construction-inventory cip))))
     " in "
     ,(if (eq (direction cip) '->) "formulation" "comprehension"))))

(defun make-html-fcg-light-for-debugging (cip &key
                                              (feature-types (feature-types (original-cxn-set (construction-inventory cip))))
                                              (solutions nil)
                                              (show-queue nil)
                                              (configuration nil))
  (set-configuration configuration :labeled-paths t)
  (set-configuration configuration :with-search-debug-data t)
  
  (let* ((solution (first solutions)) ;;to do: extend to multiple solutions
         (sentence (when (field? (car-resulting-cfs (cipn-car solution)) :utterance)
                     (first (get-data (car-resulting-cfs (cipn-car solution)) :utterance))))
         (meaning (when (field? (car-resulting-cfs (cipn-car solution)) :meaning)
                    (get-data (car-resulting-cfs (cipn-car solution)) :meaning)))
         (direction (if (eq (direction cip) '<-) "comprehension" "production"))
         (grammar (original-cxn-set (construction-inventory solution)))
         (processing-result (first (if (eq (direction cip) '<-)
                                     (evaluate-sentence-for-comprehension sentence grammar 
                                                                          :series 5 :bi-directional? t)
                                     (evaluate-sentence-for-production meaning grammar
                                                                       :series  5 :bi-directional? t))))
         (evaluation (first (create-sentence-profiles-for-comprehension (list (list processing-result)))))
         (reverted-solution (if (eq (direction cip) '<-)
                              (pr-production-final-node processing-result)
                              (pr-parsing-final-node processing-result)))
         (constructions-first-round
          (get-applied-fcg-light-constructions
           (applied-constructions (if (eq (direction cip) '<-)
                                    (pr-parsing-final-node processing-result)
                                    (pr-production-final-node processing-result)))))
         (constructions-second-round
          (get-applied-fcg-light-constructions
           (applied-constructions (if (eq (direction cip) '<-)
                                    (pr-production-final-node processing-result)
                                    (pr-parsing-final-node processing-result))))))
    

    `((div)
      ((table :class "three-col")
       ((tbody)
        ((tr) ((th) "")
         ((th) ,(format nil "~a" (if (eq (direction cip) '<-)
                                   "Comprehension" "Formulation")))
         ((th) ,(format nil "~a" (if (eq (direction cip) '<-)
                                   "Reformulation" "Reinterpretation"))))
        ((tr)
         ((td) "Initial structure")
         ((td) ,(make-html-fcg-light (initial-cfs cip)
                                     :configuration configuration :feature-types feature-types))
         ((td) ,(make-html-fcg-light (car-source-cfs (cipn-car (top-node (cip reverted-solution))))
                                     :configuration configuration :feature-types feature-types)))

     
        ,@(loop for solution in solutions 
                for n from 1
                append 
                `(,(if (> (length solutions) 1)
                     `((tr) ((td :colspan "2") ((b) "solution " ,n)))
                     "")
                  ((tr)
                   ((td) "Resulting structure")
                   ((td) ,(make-html-fcg-light (car-resulting-cfs (cipn-car solution))
                                               :configuration configuration :feature-types feature-types))
                   ((td) ,(make-html-fcg-light (car-resulting-cfs (cipn-car reverted-solution))
                                               :configuration configuration :feature-types feature-types)))))
        ((tr)
         ((td) "Applied cxns")
         ((td) ,@(html-hide-rest-of-long-list 
                  (sort constructions-first-round #'string-lessp :key #'name) 10
                  #'(lambda (construction) 
                      (make-html construction :expand-initially nil
                                 :configuration configuration
                                 :wrap-in-paragraph nil))))
         ((td) ,@(html-hide-rest-of-long-list 
                  (sort constructions-second-round #'string-lessp :key #'name) 10
                  #'(lambda (construction) 
                      (make-html construction :expand-initially nil
                                 :configuration configuration
                                 :wrap-in-paragraph nil)))))
        ((tr) ((td) "Utterances")
         ((td :style "font-size: 16px;color: green")
          ,(if (stringp (sp-sentence evaluation))
             (format nil "~s" (sp-sentence evaluation))
             (format nil "~{~s~}" (sp-sentence evaluation))))
         ,(if (sp-bi-directionalp evaluation)
            `((td :style "font-size: 16px;color: green") ,(format nil "~s" (sp-formulated-utterance evaluation)))
            `((td :style "font-size: 16px;color: red") ,(format nil "~s" (sp-formulated-utterance evaluation)))))
        ((tr) ((td) "Longest common substring")
         ((td) ,(format nil "~1$ (sd: ~1$)" (first (sp-longest-common-substring evaluation))
                        (second (sp-longest-common-substring evaluation)))) ((td)))
        ((tr) ((td) "Average branching factor")
         ((td) ,(format nil "~1$ (sd: ~1$)" (first (sp-average-branching-factor-comprehension evaluation))
                        (second (sp-average-branching-factor-comprehension evaluation))))
         ((td) ,(format nil "~1$ (sd: ~1$)" (first (sp-average-branching-factor-formulation evaluation))
                        (second (sp-average-branching-factor-formulation evaluation)))))

        ((tr) ((td) ,(format nil "Non-bidirectional cxns"))
         ((td) ,@(html-hide-rest-of-long-list
                  (set-difference constructions-first-round constructions-second-round :key #'name)
                  10
                  #'(lambda (construction) 
                      (make-html construction :expand-initially t
                                 :configuration (visualization-configuration grammar)
                                 :wrap-in-paragraph nil))))
         ((td) ,@(html-hide-rest-of-long-list
                  (set-difference constructions-second-round constructions-first-round :key #'name)
                  10
                  #'(lambda (construction) 
                      (make-html construction :expand-initially t
                                 :configuration (visualization-configuration grammar)
                                 :wrap-in-paragraph nil)))))
        ((tr) ((td) "Number of meaning chunks")
         ((td) ,(format nil "~1$ (sd: ~1$)" (first (sp-nr-of-meaning-chunks evaluation))
                        (second (sp-nr-of-meaning-chunks evaluation)))) ((td)))))
      ((table :class "two-col")
       ((tbody)
        ,(make-tr-for-cip-tree-fcg-light (top-node cip) (format nil "Search tree (~a)" direction)
                                         :configuration configuration)
        ((tr)
         ((td) ,(format nil "Constructional dependencies (~a)" direction))
         ((td) ,(make-html (analyse-solution (first solutions) (direction cip))
                           :configuration configuration))))))))
  
(define-event-handler (trace-fcg-debugging cip-finished)  
  ;;RUN EVALUATION HERE AND PRINT RESULTS TO WEB INTERFACE
    (add-element '((hr)))
    (add-element
     (make-html-fcg-light-for-debugging cip :solutions (when solution (list solution))
                                        :configuration (visualization-configuration (construction-inventory solution))))
    )

(define-event-handler (trace-fcg-debugging cip-restart-requested)
  (add-element '((hr)))
  (add-element `((h4) "Restart requested"))
  (add-element (make-html-fcg-light (cip cipn))))

(define-event-handler (trace-fcg-debugging fcg-apply-w-n-solutions-started)
  (add-element `((hr)))
  (add-element 
   `((h3) ,(if (typep n 'integer)
             (format nil "Computing max ~a solutions for application of " n)
             "Computing all solutions for application of ")
     ,(make-html (original-cxn-set construction-inventory)) " in "
     ,(if (eq direction '->) "formulation" "comprehension"))))

(define-event-handler (trace-fcg-debugging fcg-apply-w-n-solutions-finished)
  (add-element '((hr)))
  (add-element (make-html-fcg-light cip :solutions solutions)))

(define-event-handler (trace-fcg-debugging added-fcg-cxn-set))

;; ------------------------------------------------------------
;; produce/ parse

(define-event-handler (trace-fcg-debugging produce-started)
  (when (get-configuration (visualization-configuration construction-inventory) :show-upper-menu)
    (clear-page)
    (interactive-web-interface-header))
  (add-element `((hr)))
  (add-element `((h2) "Formulating&#160;"))
  (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                 (predicate-network->svg meaning :only-variables nil)
                 (html-pprint meaning))))

(define-event-handler (trace-fcg-debugging produce-finished)
  (add-element `((h3) ,(format nil "Utterance: &quot;~{~a~^ ~}&quot;" utterance)))
  (add-element `((p) " ")))

(define-event-handler (trace-fcg-debugging produce-all-started)
  (when (get-configuration (visualization-configuration construction-inventory) :show-upper-menu)
    (clear-page)
    (interactive-web-interface-header))
  (add-element `((hr)))
  (add-element `((h2) ,(format
                        nil
                        "Formulating (~a)&#160;"
                        (if (typep n 'number)
                          (format nil "max ~a solutions" n)
                          "all solutions"))))
  (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                 (predicate-network->svg meaning :only-variables nil)
                 (html-pprint meaning))))
     
(define-event-handler (trace-fcg-debugging produce-all-finished)
  (add-element `((h3) ,(format nil "Utterances: &quot;~{~{~a~^ ~}~^, ~}&quot;" utterances)))
  (add-element `((p) " ")))

(define-event-handler (trace-fcg-debugging parse-started)
  (when (get-configuration *default-visualization-configuration* :show-upper-menu)
    (clear-page)
    (interactive-web-interface-header))
  (add-element `((hr)))
  (add-element `((h2) ,(format nil "Debugging &quot;~{~a~^ ~}&quot;" utterance))))

(define-event-handler (trace-fcg-debugging parse-finished)
  (add-element `((h3 :style "margin-bottom:3px;") "Meaning:"))
  (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                 (predicate-network->svg meaning)
                 (html-pprint meaning)))
  
  
  )

(define-event-handler (trace-fcg-debugging parse-all-started)
  (when (get-configuration *default-visualization-configuration* :show-upper-menu)
    (clear-page)
    (interactive-web-interface-header))
  (add-element `((hr)))
  (add-element `((h2) ,(format nil "Debugging &quot;~{~a~^ ~}&quot; (~a)"
                               utterance
                               (if (typep n 'number)
                                 (format nil "max ~a solutions" n)
                                 "all solutions")))))

(define-event-handler (trace-fcg-debugging parse-all-finished)
  (add-element `((h3 :style "margin-bottom:3px;") "Parsed meanings"))
  (loop for meaning in meanings
        for i from 1
        do
        (add-element `((h4) ,(format nil "meaning ~a " i)))
        (add-element (if (get-configuration construction-inventory :draw-meaning-as-network)
                       (predicate-network->svg meaning)
                       (html-pprint meaning))))
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; INTERACTIVE WEB INTERFACE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;auxiliar function
(defun split-by-one-space (string)
  "Returns a list of substrings of string
divided by ONE space each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
  (loop for i = 0 then (1+ j)
        as j = (position #\Space string :start i)
        collect (subseq string i j)
        while j))


(ht-simple-ajax:defun-ajax comprehend-sentence-fcg-light (input-sentence) (wi::*ajax-processor*)
  "Function to comprehend an input utterance"
  (parse (if (equal input-sentence "")
           '()
           (split-by-one-space input-sentence)) *fcg-constructions*)
  nil)

(ht-simple-ajax:defun-ajax formulate-meaning-fcg-light (input-meaning) (wi::*ajax-processor*)
  "Function to formulate an input meaning"
  (let* ((input-meaning-in-list (if (search "((" input-meaning)
                                  input-meaning
                                  (concatenate 'string "(" input-meaning ")")))
         (meaning (with-input-from-string (s input-meaning-in-list) (read s))))
    (if (listp (car meaning))
      (produce (with-input-from-string (s input-meaning-in-list) (read s)) *fcg-constructions*)
      (progn
        (clear-page)
        (interactive-web-interface-header)
        (add-element `((hr)))
        (add-element `((h2) ,(format nil "Error: Meaning should be a list of meaning predicates")))
        (add-element `((h2) ,(format nil " Input meaning given: &#123;~(~a~)&#125;" input-meaning))))))
  nil)

;;;; ;;Function called when the there is a text area to write and load the grammar
;;;; (ht-simple-ajax:defun-ajax load-grammar-fcg-light (grammar) (wi::*ajax-processor*)
;;;;   "Function to replace the current grammar"
;;;;   (let ((*package* (find-package :fcg-light)))
;;;;     (eval (with-input-from-string (s grammar) (read s))))
;;;;   nil)

(ht-simple-ajax:defun-ajax load-grammar-fcg-light-from-file (grammar-file) (wi::*ajax-processor*)
  "Function to replace the current grammar"
  (let ((*package* (find-package :fcg-light)))
    (eval (with-input-from-string (s grammar-file) (read s))))
  nil)

(define-js 'readInputText "
function loadGrammar() {

    var file = document.getElementById('grammar-file').files[0];

    if(!file)
    {
    alert('No file to load!');
    }

    var reader = new FileReader();
    reader.onload = function(e) {
    var readedTXT = e.target.result;
    ajax_load_grammar_fcg_light_from_file(readedTXT);
    };

    reader.readAsText(file);
    return false;
}
")

(defun interactive-web-interface-header ()
  (add-element `((div :class "table")
                 ((table :style "width:100%;")
                  ((tbody)
                   ((tr)
                     ((div) ((h4) ,(format nil "Load your grammar: "))))
                    ((tr)
                     ((td)
                      ((label :for "grammar-file") "Select a grammar")
                      ((tr))
                     ((input :type "file" :accept "text/plain" :id "grammar-file" )))
                     ((td)
                     ((a :style "border-radius: 4px; font-family: Courier New;font-size: 13px;color: #000000;background: #d6d6d6;padding:4px;"
                         :href "javascript:loadGrammar();")
                      "Load Grammar")))))))
  ;;Text area to past the grammar
;;;;   (add-element `((div :class "table":style "width:100%;border-spacing: 10px;")
;;;;                  ((table :width "100%")
;;;;                   ((tbody)
;;;;                    ((td :width "85%")
;;;;                     ((form :name "load_grammar")
;;;;                      ((div) ((h4) ,(format nil "Load your grammar: ")))
;;;;                      ((textarea :style "width:100%;font-family:Courier,Courier New;font-size:9pt;"
;;;;                                 :rows "4" :name "grammar")
;;;;                       ,(format nil ""))))
;;;;                    ((td)
;;;;                     ((a :href "javascript:ajax_load_grammar_fcg_light(document.load_grammar.grammar.value);"
;;;;                         :style "border-radius: 4px; font-family: Courier New;font-size: 13px;color: #000000;background: #d6d6d6;padding:4px;")
;;;;                      "Load grammar"))))))
  (add-element `((div :class "table" :style "width:100%;border-spacing: 10px;")
                 ((table :style "width:100%;")
                  ((tbody)
                   ((tr)
                    ((td :style "width:70%;")
                     ((form :name "input_form")
                      ((div) ((h4) ,(format nil "Input utterance/meaning")))
                      ((textarea :style "width:100%;font-family:Courier,Courier New;font-size:9pt;"
                                 :rows "1" :name "input" :title "input"))))
                    ((td)
                     ((tr)
                     ((a :style "border-radius: 4px; font-family: Courier New;font-size: 13px;color: #000000;background: #d6d6d6;padding:4px;"
                         :href "javascript:ajax_comprehend_sentence_fcg_light(document.input_form.input.value);")
                      "Comprehend utterance"))
                     ((tr))
                     ((tr))
                     ((tr))
                     ((tr))
                     ((tr)
                     ((a :style "border-radius: 4px; font-family: Courier New;font-size: 13px;color: #000000;background: #d6d6d6;padding:4px;"
                         :href "javascript:ajax_formulate_meaning_fcg_light(document.input_form.input.value);")
                      "Formulate meaning"))))))))
  (add-element `((div :class "table")
                 ((h3)
                  ,(make-html *fcg-constructions*);;(make-html cxn-inventory)
                  ))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated but still there for backwards compatibility ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(trace-fcg-light))

(define-monitor trace-fcg-light
    :documentation "Backwards compatibility macro used for warning to be removed later")

(define-event-handler (trace-fcg-light cip-started)
  (warn "Trace-fcg-light is a monitor that dates from the time the current FCG still had an inferiority complex. Please don't use it anymore but activate the monitor TRACE-FCG !"))
