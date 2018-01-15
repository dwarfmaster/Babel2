
;; ######################################################################################################### ;;
;;                                                                                                           ;;
;; Demo of a grammar for Dutch VP and of Robust Comprehension through                                        ;;
;; the integration of production in comprehension (re-entrance).                                             ;;
;;                                                                                                           ;;
;; See the following publications:                                                                           ;;
;;                                                                                                           ;;
;; 1.  Van Eecke, P. Robust Processing of the Dutch Verb Phrase.                                     ;;
;;     Constructions and Frames (submitted).                                                                 ;;
;;                                                                                                           ;;
;; 2. Van Eecke, P. (2015). Achieving robustness through the integration of production in comprehension.     ;;
;;    In Proceedings of the EuroAsianPacific Joint Conference on Cognitive Science, pages 187-192.           ;;
;;                                                                                                           ;;
;; ######################################################################################################### ;;

;; Grammar file:               trunk/grammars/dutch-vp/dutch-vp-grammar.lisp
;; Robust parsing functions:   trunk/grammars/dutch-vp/robust-parsing.lisp

;; Evaluate these lines to load:
(asdf:make :dutch-vp)
(in-package :dutch-vp)
(activate-monitor trace-fcg)

;; Examples of comprehension/formulation

(comprehend '("zal" "zingen"))
(comprehend '("kan" "gezongen" "hebben"))
(comprehend '("heeft" "kunnen" "zingen"))
(comprehend '("zal" "moeten" "kunnen" "zingen"))
(comprehend '("zal" "moeten" "kunnen" "laten" "zingen"))
(comprehend '("zal" "gezongen" "moeten" "kunnen" "laten" "hebben"))

;; zal hebben gezongen
(formulate '((event sing ev) (aspect perfect ev ev-2) (focus action - ev-2) (modality hypothesis ev-2 ev-3) (time-point deictic now) (time-relation overlaps ev-3 now)))
;; zal gezongen hebben
(formulate-all ' ((sing ev) (perfect ev-2 ev) (action-focus ev-2 +) (modality ev-3 ev-2 hypothesis) (deictic-time-point now)(overlaps ev-3 now)))
;; zal moeten hebben gezongen
(formulate ' ((event sing ev) (aspect perfect ev ev-2) (focus action + ev-2) (modality hypothesis ev-2 ev-3) (time-point deictic now) (time-relation overlaps ev-3 now)))
;; heeft moeten zingen
(formulate-all ' ((event sing ev) (aspect perfect ev-2 ev-3) (focus action - ev-3) (modality obligation ev ev-2) (time-point deictic now) (time-relation overlaps ev-3 now)))
;; heeft moeten kunnen zingen
(formulate-all ' ((event sing ev) (aspect perfect ev-3 ev-4) (focus action - ev-4) (modality obligation ev-2 ev-3) (modality ability ev ev-2) (time-point deictic now) (time-relation overlaps ev-4 now)))


;; Examples of robust parsing (integration of production in comprehension) with different ranking metrics

(parse-robust '("moeten" "zingen" "hebben") :ranking-metric 'word-order)
(parse-robust '("moeten" "zingen" "hebben") :ranking-metric 'number-of-errors)
(parse-robust '("moeten" "zingen" "hebben") :ranking-metric 'levenshtein-distance)

(parse-robust '("moeten" "heeft" "zingen" ) :ranking-metric 'word-order)
(parse-robust '("moeten" "heeft" "zingen") :ranking-metric 'number-of-errors)
(parse-robust '("moeten" "heeft" "zingen" ) :ranking-metric 'levenshtein-distance)

;; Very complex examples of robust parsing (takes about 10-12 seconds for maximum preciesion (default))
;; (speedup with keyword :search-beam specifying the number of hypotheses that are generated))

(parse-robust '("zal" "moeten" "kunnen" "gezongen" "heeft") :ranking-metric 'levenshtein-distance)
(parse-robust '("zal" "zingt" "moeten" "kunnen" "hebben") :ranking-metric 'levenshtein-distance)
(parse-robust '("zal" "moeten" "hebben" "kunnen" "zingt") :ranking-metric 'levenshtein-distance)

(parse-robust '("zal" "moeten" "kunnen" "gezongen" "heeft") :ranking-metric 'levenshtein-distance :search-beam 5)
(parse-robust '("zal" "zingt" "moeten" "kunnen" "hebben") :ranking-metric 'levenshtein-distance :search-beam 10)
(parse-robust '("zal" "moeten" "hebben" "kunnen" "zingt") :ranking-metric 'levenshtein-distance :search-beam 10)


;; Examples of many forms with modals and perfect, from short to long

(full-grammar-evaluation *fcg-constructions*
                         :test-sentences-file *test-file-comprehension*
                         :test-meanings-file *test-file-production*
                         :gold-standard-sentences-file *gold-standard-utterances-file*
                         :gold-standard-meanings-file *gold-standard-meanings-file*)

;; no modals
(parse-robust '("zingt"))
(parse-robust '("heeft" "gezongen"))

;; 1 modal
(parse-robust '("kan" "zingen" ))
(parse-robust '("kan" "gezongen" "hebben"))
(parse-robust '("kan" "hebben" "gezongen"))
(parse-robust '("had" "kunnen" "zingen"))

;; 2 modals
(parse-robust '("moet" "kunnen" "zingen"))
(parse-robust '("moet" "kunnen" "gezongen" "hebben"))
(parse-robust '("moet" "gezongen" "kunnen" "hebben"))
(parse-robust '("moet" "kunnen" "hebben" "gezongen"))
(parse-robust '("moet" "hebben" "kunnen" "zingen"))
(parse-robust '("heeft" "moeten" "kunnen" "zingen"))

;; 3 modals
(parse-robust '("zal" "moeten" "kunnen" "zingen"))
(parse-robust '("zal" "moeten" "kunnen" "gezongen" "hebben"))
(parse-robust '("zal" "gezongen" "moeten" "kunnen" "hebben"))
(parse-robust '("zal" "moeten" "kunnen" "hebben" "gezongen"))
(parse-robust '("zal" "hebben" "moeten" "kunnen" "zingen"))
(parse-robust '("zal" "moeten" "hebben" "kunnen" "zingen"))
(parse-robust '("heeft" "moeten" "kunnen" "laten" "zingen"))

;; 4 modals
(parse-robust '("zal" "moeten"  "kunnen" "laten" "zingen"))
(parse-robust '("zal" "moeten" "kunnen" "laten" "gezongen" "hebben"))
(parse-robust '("zal" "gezongen" "moeten" "kunnen" "laten" "hebben" ))
(parse-robust '("zal" "moeten" "kunnen" "laten" "hebben" "gezongen"))
(parse-robust '("zal" "moeten" "kunnen" "hebben" "laten" "zingen"))
(parse-robust '("zal" "moeten" "hebben"  "kunnen" "laten" "zingen"))
(parse-robust '("zal" "hebben" "moeten"  "kunnen" "laten" "zingen"))


(fcg::make-gold-standard-meaning-file  (babel-pathname
                                          :directory '("grammars" "Dutch" "dutch-vp" "evaluation")
                                          :name "evaluation-sentences" :type "txt")
                                        *fcg-constructions*
                                        (babel-pathname
                                          :directory '("grammars" "Dutch" "dutch-vp" "evaluation")
                                          :name "evaluation-sentences-meanings" :type "csv")
                                        )

(fcg::evaluate-grammar-for-comprehension (babel-pathname
                                          :directory '("grammars" "Dutch" "dutch-vp" "evaluation")
                                          :name "evaluation-sentences" :type "txt")
                                         *fcg-constructions*
                                         :gold-standard-meanings-file
                                         (babel-pathname
                                          :directory '("grammars" "Dutch" "dutch-vp" "evaluation")
                                          :name "evaluation-sentences-meanings" :type "csv")
                                         :bi-directional? t)

