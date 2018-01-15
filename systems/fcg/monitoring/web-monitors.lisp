
(in-package :fcg)


(export '(trace-fcg-processing-level trace-fcg-processing-level-search-process))

;; ############################################################
;; trace-fcg-processing-level
;; ############################################################

(define-monitor trace-fcg-processing-level 
    :documentation "Traces results of fcg processing in a web browser")

(define-event-handler (trace-fcg-processing-level cxn-deleted)
  (add-element '((hr)))
  (add-element 
   `((h4) "Removed&#160;&#160; " 
     ,(make-html construction :expand-initially nil) " from&#160;&#160;"
     ,(make-html construction-inventory))))

(define-event-handler (trace-fcg-processing-level cxn-added)
  (add-element '((hr)))
  (add-element 
   `((h4) "Added&#160;&#160; " 
     ,(make-html construction :expand-initially nil) " to&#160;&#160;"
     ,(make-html construction-inventory))))


;; ------------------------------------------------------------
;; construction application

(define-event-handler (trace-fcg-processing-level cxn-application-started)
  (add-element `((hr)))
  (add-element 
   `((h3) "Applying "
     ,(make-html cxn :expand-initially nil)
     "in direction "
     ,(if (eq direction '->) "&#x2192; " "&#x2190; "))))

(define-event-handler (trace-fcg-processing-level cxn-application-finished)
  (let ((cars (append failed-cars resulting-cars)))
    (cond ((> (length cars) 1)
           (add-element `((p) ((b) ,(length cars) " results:")))
           (loop for car in cars 
              do (add-element '((hr)))
                (add-element (make-html car))))
          (cars (add-element `((div) ,(make-html (first cars)))))
          (t (add-element `((div) ((b) "no match")))))))


;; ------------------------------------------------------------
;; construction set application

(define-configuration-default-value :max-size-for-html 500)

(define-event-handler (trace-fcg-processing-level cip-started)
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
                  (get-construction-inventory-title-string (construction-inventory cip))
                  (size (construction-inventory cip)))
          (make-html (construction-inventory cip)))
     " in direction "
     ,(if (eq (direction cip) '->) "&#x2192;" "&#x2190;"))))

(define-event-handler (trace-fcg-processing-level cip-finished)
  (add-element '((hr)))
  (add-element (make-html cip :solutions (when solution (list solution)))))

(define-event-handler (trace-fcg-processing-level cip-restart-requested)
  (add-element '((hr)))
  (add-element `((h4) "Restart requested"))
  (add-element (make-html (cip cipn))))

(define-event-handler (trace-fcg-processing-level fcg-apply-w-n-solutions-started)
  (add-element `((hr)))
  (add-element 
   `((h3) ,(if (typep n 'integer)
             (format nil "Computing max ~a solutions for application of " n)
             "Computing all solutions for application of ")
     ,(make-html construction-inventory) " in direction "
     ,(if (eq direction '->) "&#x2192;" "&#x2190;"))))

(define-event-handler (trace-fcg-processing-level fcg-apply-w-n-solutions-finished)
  (add-element '((hr)))
  (add-element (make-html cip :solutions solutions)))

;; ------------------------------------------------------------
;; produce/ parse

(define-event-handler (trace-fcg-processing-level produce-started)
  (add-element `((hr)))
  (add-element `((h2) "Producing&#160;"))
  (add-element
   (if (get-configuration construction-inventory :draw-meaning-as-network)
       (predicate-network->svg meaning)
       (html-pprint meaning))))

(define-event-handler (trace-fcg-processing-level produce-finished)
  (add-element `((h3) ,(format nil "Utterance: &quot;~{~a~^ ~}&quot;" utterance)))
  (add-element `((p) " ")))

(define-event-handler (trace-fcg-processing-level produce-all-started)
  (add-element `((hr)))
  (add-element `((h2) ,(format
                        nil
                        "Producing (~a)&#160;"
                        (if (typep n 'number)
                                 (format nil "max ~a solutions" n)
                                 "all solutions"))))
  (add-element
   (if (get-configuration construction-inventory :draw-meaning-as-network)
       (predicate-network->svg meaning)
       (html-pprint meaning))))
     
(define-event-handler (trace-fcg-processing-level produce-all-finished)
  (add-element `((h3) ,(format nil "Utterances: &quot;~{~{~a~^ ~}~^, ~}&quot;" utterances)))
  (add-element `((p) " ")))

(define-event-handler (trace-fcg-processing-level parse-started)
  (add-element `((hr)))
  (add-element `((h2) ,(format nil "Parsing &quot;~{~a~^ ~}&quot;" utterance))))

(define-event-handler (trace-fcg-processing-level parse-finished)
  (add-element `((h3 :style "margin-bottom:3px;") "Meaning:"))
  (add-element
   (if (get-configuration construction-inventory :draw-meaning-as-network)
       (predicate-network->svg meaning)
       (html-pprint meaning)))
  (add-element `((p) " ")))

(define-event-handler (trace-fcg-processing-level parse-all-started)
  (add-element `((hr)))
  (add-element `((h2) ,(format nil "Parsing &quot;~{~a~^ ~}&quot; (~a)"
                               utterance
                               (if (typep n 'number)
                                 (format nil "max ~a solutions" n)
                                 "all solutions")))))

(define-event-handler (trace-fcg-processing-level parse-all-finished)
  (add-element `((h3 :style "margin-bottom:3px;") "Parsed meanings"))
  (loop for meaning in meanings
        for i from 1
        do
        (add-element `((h4) ,(format nil "meaning ~a " i)))
        (add-element
         (if (get-configuration construction-inventory :draw-meaning-as-network)
             (predicate-network->svg meaning)
             (html-pprint meaning))))
  (add-element `((p) " ")))


;; ############################################################
;; trace-fcg-processing-level-search-process
;; ############################################################

(define-monitor trace-fcg-processing-level-search-process
    :documentation "Traces the fcg search process in a web browser")

(define-event-handler (trace-fcg-processing-level-search-process cip-next-node)
  (add-element '((hr)))
  (add-element 
   `((table :class "two-col")
     ((tbody) ,(make-tr-for-cip-tree cipn "next node" 
                                     :hide-subtree-with-duplicates nil)))))

(define-event-handler (trace-fcg-processing-level-search-process cip-node-expanded)
  (add-element `((table :class "two-col")
                 ((tbody)
                  ,(make-tr-for-cip-tree cipn "expansion" 
                                     :hide-subtree-with-duplicates nil)
                  ,(make-tr-for-cip-tree (top-node (cip cipn)) "new tree" )
                  ,(make-tr-for-cip-queue (cip cipn) "new queue" )))))







