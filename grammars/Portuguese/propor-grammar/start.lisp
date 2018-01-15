(asdf:operate 'asdf:load-op 'propor-grammar)

(in-package :fcg)

;;Example sentences for PROPOR grammar
;; >>see also test-sentences.lisp
(comprehend-and-formulate '("ele" "nao" "o" "leio") :cxn-inventory *propor-grammar*)


(activate-monitor trace-fcg-light)
(clear-page)
(set-configuration *fcg-visualization-configuration* :with-search-debug-data t)

(comprehend-and-formulate '("eu"  "dei" "te" "este" "livro") :cxn-inventory *propor-grammar*)
(comprehend-and-formulate '("tu"  "querias" "o" "livro") :cxn-inventory *propor-grammar*)

(comprehend-and-formulate '("este" "livro" "te" "dei" "eu") :cxn-inventory *propor-grammar*)

(comprehend-all '("quem" "te" "deu" "este" "livro" ) :cxn-inventory *propor-grammar*)
(comprehend-and-formulate-all '("a_quem" "o" "deste" "tu" ) :cxn-inventory *propor-grammar*)
(comprehend-and-formulate-all '("tu" "deste" "o" "a_quem") :cxn-inventory *propor-grammar*)

(comprehend-and-formulate '("ele" "nao" "te" "deu" "este_livro") :cxn-inventory *propor-grammar*)

(comprehend-and-formulate-all '("eu" "leio" "o"  "raramente") :cxn-inventory *propor-grammar*)
(comprehend-and-formulate-all '("eu" "raramente" "o" "leio" ) :cxn-inventory *propor-grammar*)

(comprehend-and-formulate-all '("poucas" "pessoas" "o" "leem") :cxn-inventory *propor-grammar*)

(comprehend-and-formulate-all '("eu" "ontem" "vi" "te") :cxn-inventory *propor-grammar*)
(comprehend-and-formulate '("eu" "dei" "te" "este" "livro" "porque" "tu" "o" "querias") :cxn-inventory *propor-grammar*)









