
(asdf:operate 'asdf:load-op :regular-expressions)

(in-package :fcg)

(activate-monitor trace-fcg)

;;; --------------------------------------------------------------------------
;;; The constructions below are some ad-hoc examples to show the
;;; usage of the regular expressions. More detailed explanations
;;; will be offerred in the upcoming FCG tutorials in 2014.
;;; --------------------------------------------------------------------------

(progn
  (setf *constructions* (make-instance 'construction-set))
  (set-configuration *constructions* :parse-goal-tests '(:no-applicable-cxns))
  (set-configuration *constructions* :production-goal-tests '(:no-applicable-cxns))
  
  (def-syn-cxn rang-cxn ()
               ((?top-unit
                 (syn-subunits (== ?unit)))
                (?unit
                 (footprints (==0 rang))
                 (phon-cat ((onset "r")
                            (nucleus "i")
                            (coda "ng"))))
                ((J ?unit)
                 (footprints (==1 rang))))
               <-->
               ((?top-unit
                 (tag ?form (form (== (string ?unit "r{i|a}ng"))))
                 (footprints (==0 rang)))
                ((J ?unit ?top-unit)
                 ?form)))

  (add-cxn (make-cxn test-cxn ()
                     ((?top-unit
                       (tag ?meaning (meaning (== (test ?ref))))
                       (footprints (==0 lex)))
                      ((J ?unit ?top-unit)
                       ?meaning
                       (footprints (== lex))))
                     <-->
                     ((?top-unit
                       (tag ?form (form (== (string ?unit "test"))))
                       (footprints (==0 lex)))
                      ((J ?unit ?top-unit)
                       ?form
                       (footprints (== lex))))) *constructions*)

  (add-cxn (make-cxn sing-stem-cxn ()
                     ((?top-unit
                       (tag ?meaning (meaning (== (sing ?ev))))
                       (footprints (==0 lex)))
                      ((J ?unit ?top-unit)
                       ?meaning
                       (args (?ev))
                       (footprints (== lex))))
                     <-->
                     ((?top-unit
                       (tag ?form (form (== (string ?unit "sing.*"))))
                       (footprints (==0 lex)))
                      ((J ?unit ?top-unit)
                       ?form
                       (phon-cat ((onset "s")
                                  (nucleus "i")
                                  (coda "ng")))
                       (footprints (== lex))))) *constructions*)

  (add-cxn (make-cxn swim-stem-cxn ()
                     ((?top-unit
                       (tag ?meaning (meaning (== (swim ?ev))))
                       (footprints (==0 lex)))
                      ((J ?unit ?top-unit)
                       ?meaning
                       (args (?ev))
                       (footprints (== lex))))
                     <-->
                     ((?top-unit
                       (syn-subunits (== ?unit)))
                      (?unit
                       (form (== (string ?unit "swim.*")))
                       (footprints (==0 lex))
                       (tense past)
                       (phon-cat (==1 (onset "sw")
                                      (nucleus "i")
                                      (coda "m"))))
                      ((J ?unit)
                       (footprints (==1 lex))))) *constructions*)
    
  (def-syn-cxn swam-past-cxn ()
               ((?top-unit
                 (syn-subunits (== ?unit)))
                (?unit
                 (footprints (==0 past))
                 (phon-cat (==1 (onset "sw")
                                (nucleus "i")
                                (coda "m")))
                 (tense past))
                ((J ?unit)
                 (footprints (==1 past))))
               <-->
               ((?top-unit
                 (tag ?form (form (== (string ?unit "sw{i|a}m")))))
                ((J ?unit ?top-unit)
                 ?form)))

  (add-cxn (make-cxn leef-stem-cxn ()
                     ((?top-unit
                       (tag ?meaning (meaning (== (live ?ev))))
                       (footprints (==0 lex)))
                      ((J ?unit ?top-unit)
                       ?meaning
                       (args (?ev))
                       (footprints (== lex))))
                     <-->
                     ((?top-unit
                       (tag ?form (form (== (string ?unit ".*leef.*"))))
                       (footprints (==0 lex)))
                      ((J ?unit ?top-unit)
                       ?form
                       (footprints (== lex))))) *constructions*)

  (add-cxn (make-cxn geleefd-cxn ()
                     ((?top-unit
                       (sem-subunits (== ?unit))
                       (tag ?meaning (meaning (== (past-part ?ev)))))
                      (?unit
                       (args (?ev)))
                      ((J ?unit)
                       ?meaning))
                     <-->
                     ((?top-unit
                       (syn-subunits (== ?unit)))
                      (?unit
                       (form (== (string ?unit "(ge).+(d)")))
                       (footprints (==0 past-part-cxn)))
                      ((J ?unit)
                       (footprints (== past-part-cxn))))) *constructions*)

  (add-cxn (make-cxn ing-form-cxn-3 ()
                     ((?top-unit
                       (sem-subunits (== ?unit))
                       (tag ?meaning (meaning (== (ongoing ?ev)))))
                      (?unit
                       (args (?ev)))
                      ((J ?unit)
                       ?meaning))
                     <-->
                     ((?top-unit
                       (syn-subunits (== ?unit)))
                      (?unit
                       (footprints (==0 ing-form-cxn))
                       (phon-cat (==1 (onset ?onset)
                                      (nucleus ?nucleus)
                                      (coda ?coda)))
                       (form (== (string ?unit "<?onset><?nucleus><?coda>(ing)"))))
                      ((J ?unit)
                       (footprints (==1 ing-form-cxn))))) *constructions*)

  (add-cxn (make-cxn ing-form-cxn-2 ()
                     ((?top-unit
                       (sem-subunits (== ?unit))
                       (tag ?meaning (meaning (== (ongoing ?ev)))))
                      (?unit
                       (args (?ev)))
                      ((J ?unit)
                       ?meaning))
                     <-->
                     ((?top-unit
                       (syn-subunits (== ?unit)))
                      (?unit
                       (footprints (==0 ing-form-cxn))
                       (phon-cat (==1 (onset ?onset)))
                       (form (== (string ?unit "sing(ing)"))))
                      ((J ?unit)
                       (footprints (==1 ing-form-cxn))))) *constructions*)

  (add-cxn (make-cxn stationschef-cxn ()
                     ((?top-unit
                       (tag ?meaning (meaning (== (stationschef ?ref))))
                       (footprints (==0 lex)))
                      ((J ?unit ?top-unit)
                       (args (?ref))
                       (footprints (==1 lex))
                       ?meaning))
                     <-->
                     ((?top-unit
                       (tag ?form (form (== (string ?unit "station.*chef"))))
                       (footprints (==0 lex)))
                      ((J ?unit ?top-unit)
                       ?form
                       (footprints (==1 lex))
                       (form (== (part-1 "station")
                                 (part-2 "chef")))))) *constructions*)

  (add-cxn (make-cxn s-infix ()
                     ((?top-unit
                       (tag ?meaning (meaning (== (combined-ref ?ref))))
                       (sem-subunits (== ?unit)))
                      (?unit
                       (args (?ref)))              
                      ((J ?unit)
                       ?meaning))
                     <-->
                     ((?top-unit
                       (syn-subunits (== ?unit)))
                      (?unit
                       (footprints (==0 infix))
               
                       (form (== (part-1 ?noun1)
                                 (part-2 ?noun2))))
                      ((J ?unit)
                       (form (== (string ?unit "<?noun1>(s)<?noun2>")))
                       (footprints (==1 infix))))) *constructions*)

  (add-cxn (make-cxn ing-form-cxn-1 ()
                     ((?top-unit
                       (sem-subunits (== ?unit))
                       (tag ?meaning (meaning (== (ongoing ?ev)))))
                      (?unit
                       (args (?ev)))
                      ((J ?unit)
                       ?meaning))
                     <-->
                     ((?top-unit
                       (syn-subunits (== ?unit)))
                      (?unit
                       (footprints (==0 ing-form-cxn))
                       (phon-cat (==1 (onset ?onset)))
                       (form (== (string ?unit "..+(ing)"))))
                      ((J ?unit)
                       (footprints (==1 ing-form-cxn))))) *constructions*))

;; Exact match:
(produce '((test ref)) *constructions*)
(parse '("test") *constructions*)
;; Partial match:
(parse '("singing" "singer") *constructions*)
;; Competing constructions:
(parse-all '("singing") *constructions*)
(produce-all '((sing ev-1) (ongoing ev-1)) *constructions*)
;; Pre- and suffix:
(parse '("geleefd") *constructions*)
(produce '((live ev-2) (past-part ev-2)) *constructions*)
;; Infix:
(parse '("stationschef") *constructions*)
(produce '((stationschef ref) (combined-ref ref)) *constructions*)
;; Vowel change:
(parse '("swam") *constructions*)
(produce '((swim ev-1)) *constructions*)
