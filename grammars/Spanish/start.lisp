
;(asdf:operate 'asdf:load-op :spanish-grammar)
(in-package :fcg)

(defparameter *test-sentences* (babel-pathname :directory '("grammars" "Spanish")
                                               :name "spanish-test-suite3" :type "txt"))

(evaluate-grammar-for-comprehension *test-sentences*
                                    *spanish-verb-conjugation*
                                    :series 1 :bi-directional? t 
                                    :exclude-sentences-with-single-word nil)
(comprehend-and-formulate '("piens" "o") :cxn-inventory *spanish-verb-conjugation*)
(comprehend-and-formulate '("pens" "a" "mos") :cxn-inventory *spanish-verb-conjugation*)

(formulate-and-comprehend '((time-point present-point point-1)
                            (simultaneous point-1 dinner-event-1)
                            (cognitive-action think dinner-event-1)
                            (thinker dinner-event-1 me-1)
                            (person me  me-1)) :cxn-inventory *spanish-verb-conjugation*)

(comprehend-and-formulate '("cuez" "o") :cxn-inventory *spanish-verb-conjugation*)
(comprehend-and-formulate '("cuec" "e" "s") :cxn-inventory *spanish-verb-conjugation*)
(comprehend-and-formulate '("coz" "a" "mos") :cxn-inventory *spanish-verb-conjugation*)


(comprehend-and-formulate "coc í" :cxn-inventory *spanish-verb-conjugation*)
(read-from-string "cocí")

(clear-page)
(activate-monitor trace-fcg)
(comprehend '("cen" "o") :cxn-inventory *spanish-verb-conjugation*)
(comprehend-all '("cen" "aba") :cxn-inventory *spanish-verb-conjugation*)
;;ceno
(formulate-and-comprehend '((time-point present-point point-1)
                            (simultaneous point-1 dinner-event-1)
                            (activity eat dinner-event-1)
                            (eater dinner-event-1 me-1)
                            (person me me-1)) :cxn-inventory *spanish-verb-conjugation*)

;;cené
(formulate-and-comprehend '((time-point recalled-point point-1)
                            (simultaneous point-1 dinner-event-1)
                            (event-perspective bound dinner-event-1)
                            (activity eat dinner-event-1)
                            (eater dinner-event-1 me-1)
                            (person me me-1)) :cxn-inventory *spanish-verb-conjugation*)

(comprehend '("llegu" "é") :cxn-inventory *spanish-verb-conjugation*)
(formulate '((time-point recalled-point point-1)
             (simultaneous point-1 arrival-1)
             (event-perspective bound arrival-1)
             (motion arrive arrival-1)
             (arriver arrival-1 me-1)
             (person me  me-1)) :cxn-inventory *spanish-verb-conjugation*)


;;cenaba
(formulate-and-comprehend '((time-point recalled-point point-1)
                            (simultaneous point-1 dinner-event-1)
                            (event-perspective unbound dinner-event-1)
                            (activity eat dinner-event-1)
                            (eater dinner-event-1 me-1)
                            (person me me-1)) :cxn-inventory *spanish-verb-conjugation*)

;;habré cenado
(formulate '((time-point present-point point-1)
             (anticipating point-1 point-2)
             (time-point anticipated-point point-2)
             (anterior point-2 dinner-event-1)
             (activity eat dinner-event-1)
             (eater dinner-event-1 me-1)
             (person me me-1)) :cxn-inventory *spanish-verb-conjugation*)

;;habria cenado
(formulate '((time-point recalled-point point-1)
             (anticipating point-1 point-2)
             (time-point anticipated-point point-2)
             (anterior point-2 dinner-event-1)
             (activity eat dinner-event-1)
             (eater dinner-event-1 me-1)
             (person me me-1)) :cxn-inventory *spanish-verb-conjugation*)


(comprehend-and-formulate '("lleg" "o") :cxn-inventory *spanish-verb-conjugation*)
(comprehend-and-formulate '("lleg" "a" "s") :cxn-inventory *spanish-verb-conjugation*)
(comprehend-and-formulate '("lleg" "a") :cxn-inventory *spanish-verb-conjugation*)
(comprehend-and-formulate '("lleg" "a" "mos") :cxn-inventory *spanish-verb-conjugation*)
(comprehend-all '("lleg" "a" "mos") :cxn-inventory *spanish-verb-conjugation*)

;;add stressed cxn + thematic "á"
(comprehend-and-formulate '("lleg" "a" "is") :cxn-inventory *spanish-verb-conjugation*)

(comprehend-and-formulate '("lleg" "a" "n") :cxn-inventory *spanish-verb-conjugation*)


(comprehend-and-formulate '("lleg" "aste") :cxn-inventory *spanish-verb-conjugation*)
(comprehend-and-formulate '("lleg" "ó") :cxn-inventory *spanish-verb-conjugation*)
(comprehend-and-formulate '("lleg" "a" "mos") :cxn-inventory *spanish-verb-conjugation*)

(comprehend-and-formulate '("lleg" "aba" "s") :cxn-inventory *spanish-verb-conjugation*)

(comprehend-and-formulate '("cuez" "o") :cxn-inventory *spanish-verb-conjugation*)

(comprehend-all '("lleg" "aba")  :cxn-inventory *spanish-verb-conjugation*)
(comprehend-and-formulate '("llegu" "é") :cxn-inventory *spanish-verb-conjugation*)
(comprehend-and-formulate '("cuez" "o") :cxn-inventory *spanish-verb-conjugation*)

(comprehend-all '("present" "o") :cxn-inventory *spanish-verb-conjugation*) 

;;TO DO:
;;+ Stress constructions >> This could be done by the de-render/render function
;; Now cuezo cannot be produced correctly
;;+Test multiple expansions
