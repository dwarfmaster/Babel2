;;; This file contains a small sample to test the details of construction
;;; -checking and to demonstrate the available checks.

; load fcg first 
; (asdf:operate 'asdf:load-op 'fcg)

(in-package :fcg)

(defparameter *constructions* (make-instance 'construction-set))
(set-configuration *constructions* :production-goal-tests '(:no-applicable-cxns))
(set-configuration *constructions* :parse-goal-tests '(:no-applicable-cxns))
;; this configuration can be used to define the specific check-cxn-tests
;; defaults to *check-cxn-tests* ...
;; e.g.
;: (set-configuration *constructions* :check-cxn-tests '(j-unit-test))
(set-configuration *constructions* :check-cxn-tests *check-cxn-tests*)

;; Activate this monitor to automatically run check-cxns
;; when initializing constructions...
(activate-monitor check-cxns)

;; The browser-tracings depend on this general monitor...
(activate-monitor trace-fcg-processing-level)

;;Without mistakes
(add-cxn
 (make-cxn sample-cxn (:apply-sequentially t)
           ((root
             (TAG ?meaning (meaning (== (sample ?sample-set ?base-set)
                                        (sample ?adasd ?ads))))
             (footprints (==0 sample-cxn)))
            ((J ?word-sample)
             ?meaning
             (args (?sample-set ?base-set))
             (sem-cat (==1 (is-animate nil) (is-countable nil) (class object)))
             (footprints (==1 sample-cxn))))
           <-->
           ((root
             (footprints (==0 sample-cxn))
             (TAG ?form (form (== (string ?word-sample "sample")))))
            ((J ?word-sample)
             ?form
             (footprints (==1 sample-cxn))
             (syn-cat (==1 (number singular) (lex-cat noun)))))
           )
 *constructions*)

(parse '("sample") *constructions*)

;;With mistakes (but inits)
(add-cxn
   (make-cxn sample-cxn (:apply-sequentially t)
           ((root
             (TAG ?meaning (meaning (== (sample ?sample-set ?base-set)
                                        (sample ?adasd ?ads)))
                  ?blubb (bla)
                  ?bling)
             (TAG)
             (footprints (==0 sample-cxn))
             (?blubb))
            ((J word-sample)
             ?meaning ?wrong
             (args (?sample-set ?base-set))
             (args (?sample-set ?base-set))
             (subunits (?mango ?tango))
             (sem-cat (==1 (is-animate nil) (is-animate nil) (is-countable nil) (class object)))
             (footprints (==1 sample-cxn))))
           <-->
           ((root
             (footprints (==0 sample-cxn))
             (TAG ?form (form (== (string ?word-sample "sample"))) foam)))
   )
   *constructions*)

(parse '("sample") *constructions*)

;; FCG-LIGHT
#|
;; The browser-tracings depend on this general monitor...
(activate-monitor trace-fcg)
;;Without mistakes
(def-fcg-constructions simple-english-grammar
  :feature-types ((args sequence)
                  (form set-of-predicates)
                  (meaning set-of-predicates)
                  (constituents sequence)
                  (dependents sequence))
  :fcg-configurations ((:check-cxn-tests *check-cxn-tests*))
  (def-fcg-cxn sample-cxn 
               ((?word-sample
                 (args (?sample-set ?base-set))
                 (sem-cat (is-animate nil)
                          (is-countable nil)
                          (class object))
                 (syn-cat (number singular)
                          (lex-cat noun)))
                <-
                (?word-sample
                 (HASH meaning ((sample ?sample-set ?base-set)
                                (sample ?adasd ?ads)))                     
                 --
                 (HASH form ((string ?word-sample "sample")))))))

(comprehend '("sample"))               

;;With mistakes (but inits)
(def-fcg-cxn sample-cxn 
             ((?word-sample
               (args (?sample-set ?base-set))
               (args (?sample-set ?base-set))
               (sem-cat (is-animate nil)
                        (is-countable nil)
                        (class object))
               (subunits (?mango ?tango)))
              <-
              (?word-sample
               (HASH meaning ((sample ?sample-set ?base-set)
                              (sample ?adasd ?ads)))
               (HASH bla )                    
               --
               (HASH form ((string ?word-sample "sample")))
               (HASH foam ()))))

(comprehend '("sample"))
|#