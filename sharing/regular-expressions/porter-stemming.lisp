
(in-package :fcg)

;; (activate-monitor trace-fcg)

(defparameter *constructions* nil)
  
(defmacro def-syn-cxn (name attributes left-pole arrow right-pole)
  (declare (ignore arrow))
  `(add-cxn (make-instance 'construction
                           :name ',name
                           :domain 'syn
                           :attributes `,(parse-attributes ',attributes)
                           :left-pole ',left-pole
                           :right-pole ',right-pole)
            *constructions*))

(progn
  (setf *constructions* (make-instance 'construction-set))
  (set-configuration *constructions* :cxn-supplier-mode :ordered-by-label)
  (set-configuration *constructions* :parse-order '(1a 1b 1c two three four 5a 5b))
  (set-configuration *constructions* :root-mode t)
  (set-configuration *constructions* :parse-goal-tests '(:no-applicable-cxns))
  (set-configuration *constructions* :create-initial-structure-mode :root-mode)
  (set-configuration *constructions* :de-render-mode :de-render-in-root-mode)

  ;; Step 5.
  (def-syn-cxn ll->l (:label 5b)
               ((root
                 (footprints (==1 step5b))))
               <-->
               ((root
                 (footprints (==0 step5b))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+.*{l|ll}"))))))
  
  (def-syn-cxn m=1not*o+e-> (:label 5a) ;; second c is not w x or y
               ((root
                 (footprints (==1 step5a))))
               <-->
               ((root
                 (footprints (==0 step5a))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+{|e}"))))))

  (def-syn-cxn not*o (:label 5a)
               ((root
                 (footprints (==1 step5a))))
               <-->
               ((root
                 (footprints (==0 step5a))
                 (form (== (string ?string ".*[^\aeiou]+[aeiouy][^\aeiouwxy]e"))))))
  
;  (def-syn-cxn m=1*o+e-> (:label 5a) ;; second c is not w x or y
;               ((root
;                 (footprints (==1 step5a))))
;               <-->
;               ((root
;                 (footprints (==0 step5a))
;                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiouwxy]+.*{|e}"))))))
  
  (def-syn-cxn m>e-> (:label 5a)
               ((root
                 (footprints (==1 step5a))))
               <-->
               ((root
                 (footprints (==0 step5a))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou]*.*{|e}"))))))
  

  ;; Step 4.
  (def-syn-cxn ize-> (:label four)
               ((root
                 (footprints (==1 step4))))
               <-->
               ((root
                 (footprints (==0 step4))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|ize}"))))))
  
  (def-syn-cxn ive-> (:label four)
               ((root
                 (footprints (==1 step4))))
               <-->
               ((root
                 (footprints (==0 step4))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|ive}"))))))
  (def-syn-cxn ous-> (:label four)
               ((root
                 (footprints (==1 step4))))
               <-->
               ((root
                 (footprints (==0 step4))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|ous}"))))))
    
  (def-syn-cxn iti-> (:label four)
               ((root
                 (footprints (==1 step4))))
               <-->
               ((root
                 (footprints (==0 step4))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|iti}"))))))
  
  (def-syn-cxn ate-> (:label four)
               ((root
                 (footprints (==1 step4))))
               <-->
               ((root
                 (footprints (==0 step4))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|ate}"))))))
  
  (def-syn-cxn ism-> (:label four)
               ((root
                 (footprints (==1 step4))))
               <-->
               ((root
                 (footprints (==0 step4))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|ism}"))))))

  (def-syn-cxn ou-> (:label four)
               ((root
                 (footprints (==1 step4))))
               <-->
               ((root
                 (footprints (==0 step4))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|ou}"))))))

    
  (def-syn-cxn [st]ion-> (:label four)
               ((root
                 (footprints (==1 step4))))
               <-->
               ((root
                 (footprints (==0 step4))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou]*.*[ts]{|ion}"))))))

  (def-syn-cxn ent-> (:label four)
               ((root
                 (footprints (==1 step4))))
               <-->
               ((root
                 (footprints (==0 step4 block-ment-or-ent))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|ent}"))))))

  (def-syn-cxn ment-blocker (:label four)
               ((root
                 (footprints (==1 block-ment-or-ent))))
               <-->
               ((root
                 (footprints (==0 block-ment-or-ent))
                 (form (== (string ?string ".+ment"))))))
    
  (def-syn-cxn ment-> (:label four)
               ((root
                 (footprints (==1 step4 block-ment-or-ent))))
               <-->
               ((root
                 (footprints (==0 step4 block-ment-or-ent))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|ment}"))))))

  (def-syn-cxn ement-blocker (:label four)
               ((root
                 (footprints (==1 block-ment-or-ent))))
               <-->
               ((root
                 (footprints (==0 block-ment-or-ent))
                 (form (== (string ?string ".+ement"))))))
    
  (def-syn-cxn ement-> (:label four)
               ((root
                 (footprints (==1 step4 block-ment-or-ent))))
               <-->
               ((root
                 (footprints (==0 step4))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|ement}"))))))
  
  (def-syn-cxn ant-> (:label four)
               ((root
                 (footprints (==1 step4))))
               <-->
               ((root
                 (footprints (==0 step4))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|ant}"))))))
  
  (def-syn-cxn ible-> (:label four)
               ((root
                 (footprints (==1 step4))))
               <-->
               ((root
                 (footprints (==0 step4))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|ible}"))))))
  
  (def-syn-cxn able-> (:label four)
               ((root
                 (footprints (==1 step4))))
               <-->
               ((root
                 (footprints (==0 step4))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|able}"))))))
    
  (def-syn-cxn ic-> (:label four)
               ((root
                 (footprints (==1 step4))))
               <-->
               ((root
                 (footprints (==0 step4))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|ic}"))))))
  
  (def-syn-cxn er-> (:label four)
               ((root
                 (footprints (==1 step4))))
               <-->
               ((root
                 (footprints (==0 step4))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|er}"))))))

  (def-syn-cxn ence-> (:label four)
               ((root
                 (footprints (==1 step4))))
               <-->
               ((root
                 (footprints (==0 step4))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|ence}"))))))
    
  (def-syn-cxn ance-> (:label four)
               ((root
                 (footprints (==1 step4))))
               <-->
               ((root
                 (footprints (==0 step4))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|ance}"))))))
  
  (def-syn-cxn al-> (:label four)
               ((root
                 (footprints (==1 step4))))
               <-->
               ((root
                 (footprints (==0 step4))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+[aeiouy]+[^\aeiou].*{|al}"))))))
  
  ;; Step 3.
  (def-syn-cxn ness-> (:label three)
               ((root
                 (footprints (==1 step3))))
               <-->
               ((root
                 (footprints (==0 step3))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{|ness}"))))))
    
  (def-syn-cxn ful-> (:label three)
               ((root
                 (footprints (==1 step3))))
               <-->
               ((root
                 (footprints (==0 step3))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{|ful}"))))))
    
  (def-syn-cxn ical->ic (:label three)
               ((root
                 (footprints (==1 step3))))
               <-->
               ((root
                 (footprints (==0 step3))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ic|ical}"))))))
    
  (def-syn-cxn icity->ic (:label three)
               ((root
                 (footprints (==1 step3))))
               <-->
               ((root
                 (footprints (==0 step3))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ic|iciti}"))))))

  (def-syn-cxn alize->al (:label three)
               ((root
                 (footprints (==1 step3))))
               <-->
               ((root
                 (footprints (==0 step3))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{al|alize}"))))))

  (def-syn-cxn ative-> (:label three)
               ((root
                 (footprints (==1 step3))))
               <-->
               ((root
                 (footprints (==0 step3))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{|ative}"))))))

  (def-syn-cxn icate->ic (:label three)
               ((root
                 (footprints (==1 step3))))
               <-->
               ((root
                 (footprints (==0 step3))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ic|icate}"))))))
  
  ;; Step 2.
  (def-syn-cxn logi->log (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{log|logi}"))))))
    
  (def-syn-cxn biliti->ble (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ble|biliti}"))))))
    
  (def-syn-cxn iviti->ive (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ive|iviti}"))))))
  
  (def-syn-cxn aliti->al (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{al|aliti}"))))))
  
  (def-syn-cxn ousness->ous (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ous|ousness}"))))))
  
  (def-syn-cxn fulness->ful (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ful|fulness}"))))))
    
  (def-syn-cxn iveness->ive (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ive|iveness}"))))))

  (def-syn-cxn alism->al (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{al|alism}"))))))

  (def-syn-cxn ator->ate (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ate|ator}"))))))

  (def-syn-cxn ation->ate (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ate|ation}"))))))
    
  (def-syn-cxn ization->ize (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ize|ization}"))))))

  (def-syn-cxn ousli->ous (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ous|ousli}"))))))
  
  (def-syn-cxn eli->e (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^aeiou]*[aeiouy]+[^aeiou].*{e|eli}"))))))
  
  (def-syn-cxn entli->ent (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ent|entli}"))))))

  (def-syn-cxn alli->al (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{al|alli}"))))))

  (def-syn-cxn bli->ble (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ble|bli}"))))))

  (def-syn-cxn izer->ize (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ize|izer}"))))))
  
  (def-syn-cxn anci->ance (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ance|anci}"))))))

  (def-syn-cxn enci->ence (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ence|enci}"))))))
    
  (def-syn-cxn tional->tion (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{tion|tional}"))))))

  (def-syn-cxn ational->ate (:label two)
               ((root
                 (footprints (==1 step2))))
               <-->
               ((root
                 (footprints (==0 step2))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou].*{ate|ational}"))))))

  ;; Step 1c.
  (def-syn-cxn m0Y->I (:label 1c)
               ((root
                 (footprints (==1 step1c))))
               <-->
               ((root
                 (footprints (==0 step1c))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+.*{i|y}"))))))

;;                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]*.*{i|y}"))))))
  ;; Step 1b.  
  (def-syn-cxn *o+->e (:label 1b) ;; second c is not w x or y
               ((root
                 (footprints (==1 *0-> step1b2-3 step5a))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 *0-> d*->d dd->d)))
                 (form (== (string ?string "[^\aeiou]+[aeiouy][^\aeiouwxy]{e|}"))))))
  
  (def-syn-cxn yy->y (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{y|yy}"))))))

  (def-syn-cxn xx->x (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{x|xx}"))))))

  (def-syn-cxn ww->w (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{w|ww}"))))))

  (def-syn-cxn vv->v (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{v|vv}"))))))

  
  (def-syn-cxn tt->t (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{t|tt}"))))))
    
  (def-syn-cxn rr->r (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{r|rr}"))))))
    
  (def-syn-cxn qq->q (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{q|qq}"))))))
    
  (def-syn-cxn pp->p (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{p|pp}"))))))

  (def-syn-cxn nn->n (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{n|nn}"))))))
  
  (def-syn-cxn mm->m (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{m|mm}"))))))
  
  (def-syn-cxn kk->k (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{k|kk}"))))))

  (def-syn-cxn jj->j (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{j|jj}"))))))


  (def-syn-cxn hh->h (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{h|hh}"))))))
  
  (def-syn-cxn gg->g (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{g|gg}"))))))
    
  (def-syn-cxn ff->f (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{f|ff}"))))))

  (def-syn-cxn d*->d (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{d|dd}"))))))

  (def-syn-cxn cc->c (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{c|cc}"))))))

  (def-syn-cxn bb->b (:label 1b)
               ((root
                 (footprints (==1 d*->d))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 d*->d)))
                 (form (== (string ?string ".+{b|bb}"))))))

  (def-syn-cxn iz->ize (:label 1b)
               ((root
                 (footprints (==1 iz->ize))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 iz->ize d*->d)))
                 (form (== (string ?string ".+{ize|iz}"))))))

  (def-syn-cxn bl->ble (:label 1b)
               ((root
                 (footprints (==1 bl->ble))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 bl->ble d*->d)))
                 (form (== (string ?string ".+{ble|bl}"))))))
  
  (def-syn-cxn at->ate (:label 1b)
               ((root
                 (footprints (==1 at->ate))))
               <-->
               ((root
                 (tag ?required (footprints (==1 step1b2-3))
                      ?forbidden (footprints (==0 at->ate d*->d)))
                 (form (== (string ?string ".+{ate|at}"))))))
  
  (def-syn-cxn *v*+ing-> (:label 1b)
               ((root
                 (footprints (==1 step1b step1b2-3))))
               <-->
               ((root
                 (footprints (==0 step1b d*->d))
                 (form (== (string ?string "[^\aeiou]*[aeiouy].*{|ing}"))))))
  
  (def-syn-cxn *v*+ed-> (:label 1b)
               ((root
                 (footprints (==1 step1b step1b2-3))))
               <-->
               ((root
                 (footprints (==0 step1beed step1b d*->d))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+.*{|ed}"))))))

    (def-syn-cxn block-for-step1b (:label 1b)
               ((root
                 (footprints (==1 step1beed))))
               <-->
               ((root
                 (footprints (==0 step1beed d*->d))
                 (form (== (string ?string ".*eed"))))))
    
  (def-syn-cxn >m0+eed->ee (:label 1b)
               ((root
                 (footprints (==1 step1beed))))
               <-->
               ((root
                 (footprints (==0 step1beed d*->d step1b))
                 (form (== (string ?string "[^\aeiou]*[aeiouy]+[^\aeiou]+.*{ee|eed}"))))))
  
  ;; Step 1a.
  (def-syn-cxn s-> (:label 1a)
               ((root
                 (footprints (==1 step1a))))
               <-->
               ((root
                 (footprints (==0 step1a))
                 (form (== (string ?string "..+{|s}"))))))

  (def-syn-cxn ss->ss (:label 1a)
                 ((root
                   (footprints (==1 step1a))))
                 <-->
                 ((root
                   (footprints (==0 step1a))
                   (form (== (string ?string ".+ss"))))))

  (def-syn-cxn ies->s (:label 1a)
               ((root
                 (footprints (==1 step1a))))
               <-->
               ((root
                 (footprints (==0 step1a))
                 (form (== (string ?string ".+{i|ies}"))))))
  
  (def-syn-cxn sses->ss (:label 1a)
               ((root
                 (footprints (==1 step1a))))
               <-->
               ((root
                 (footprints (==0 step1a))
                 (form (== (string ?string ".+{ss|sses}"))))))

  (def-syn-cxn ignore-two-letters (:label 1a)
               ((root
                 (footprints (==1 step1a step1b step1c step2 step3 step4 step5a step5b))))
               <-->
               ((root
                 (footprints (==0 step1a))
                 (form (== (string ?string ".."))))))

  (def-syn-cxn ignore-one-letter (:label 1a)
               ((root
                 (footprints (==1 step1a step1b step1c step2 step3 step4 step5a step5b))))
               <-->
               ((root
                 (footprints (==0 step1a))
                 (form (== (string ?string ".")))))))

(defun data-file-path (name &optional (type "txt"))
  (merge-pathnames (make-pathname :directory '(:relative "sharing" "regular-expressions")
                                  :name name
                                  :type type)
                   (babel-pathname)))

(defun test-porter-stemming ()
  (deactivate-all-monitors)
  (load (data-file-path "porter-stemming" "lisp")) 
  (let ((input (open (data-file-path "porter-stemmer-input")))
        (checklist (open (data-file-path "porter-stemmer-checklist")))
        (i 0))
    (loop for input-line = (read-line input nil)
          for check = (read-line checklist nil)
          while input-line
          do (multiple-value-bind (meaning cipn)
                 (parse (list input-line) *constructions*)
               (declare (ignore meaning))
               (incf i)
               (let ((parsed-string (if cipn
                                      (first (render (car-resulting-cfs (cipn-car cipn)) t))
                                      input-line)))
                 (unless (string= parsed-string check)
                   (format t "~%~a parsed into: ~a and not ~a" (pp input-line) (pp parsed-string) (pp check))))))
    (close input)
    (close checklist)
    (print "+++++++++++++++++++++++++++Finished+++++++++++++++++++++++++++")
    i))

(defun porter-stem (string)
  (deactivate-all-monitors)
  (load (data-file-path "porter-stemming" "lisp"))
  (multiple-value-bind (meaning cipn)
      (parse (list string) *constructions*)
    (declare (ignore meaning))
    (if cipn
      (first (render (car-resulting-cfs (cipn-car cipn)) t))
      string)))

;; (test-porter-stemming)
