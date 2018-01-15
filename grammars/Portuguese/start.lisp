;; Portuguese grammar for clitics
;; Tânia Marques and Katrien Beuls (2016)

;(asdf:operate 'asdf:load-op 'portuguese-grammar)
(in-package :fcg)


;; EVALUATING THE GRAMMAR DIRECTLY
;; -------------------------------

(defparameter *portuguese-corpus-grammar* (create-grammar))

(defparameter *sentences-with-gold-standard-meanings*
  (babel-pathname :directory '("grammars" "Portuguese")
                  :name "sentences-with-gold-standard-meanings" :type "txt"))

(evaluate-grammar-for-comprehension *sentences-with-gold-standard-meanings*
                                    *portuguese-corpus-grammar*
                                    :write-bidirectional-sentences t)

(evaluate-grammar-for-production *sentences-with-gold-standard-meanings*
                                 *portuguese-corpus-grammar*
                                 :write-bidirectional-meanings t)


;; PROCESSING INDIVIDUAL SENTENCES
;; -------------------------------
;;
;;TO DO
;; boundary feature kan terug verwijderd worden uit units zelf (wordt geupdatet in de root)

(progn
(defparameter *portuguese-corpus-grammar* (create-grammar))
(deactivate-all-monitors)
;(set-configuration *fcg-visualization-configuration* :hierarchy-features '(dependents))
;(set-configuration *fcg-visualization-configuration* :selected-hierarchy 'dependents)

(set-configuration (visualization-configuration *portuguese-corpus-grammar*) :with-search-debug-data t)
;(add-element (make-html *portuguese-corpus-grammar*))
(activate-monitor trace-fcg-light)
(clear-page))



(activate-monitor trace-fcg-search-process)
(comprehend '("onde" "a" "encontrou" "o" "pedro") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend '("a" "maria" "nao" "o" "viu") :cxn-inventory *portuguese-corpus-grammar*)

;; EVALUATE SENTENCES FROM TEST SUITE:
(evaluate-grammar
 (babel-pathname :directory '("grammars" "Portuguese" "Mini-Corpus")
                 :name "test-suite-sentences-all" :type "txt")
 (create-grammar) :length 7
 :output (babel-pathname :directory '("grammars" "Portuguese") :name "results-length-7" :type "csv")
 :correct-sentences (babel-pathname :directory '("grammars" "Portuguese") :name "correct-sentences" :type "txt")
 :correct-meanings (babel-pathname :directory '("grammars" "Portuguese") :name "correct-meanings" :type "txt"))

(comprehend '("todas" "as"  "criancas" "nos" "disseram" "a" "verdade")
            :cxn-inventory *portuguese-corpus-grammar*)

(comprehend-and-formulate '("de" "ti" "se" "sabe" "pouco" ) :cxn-inventory *portuguese-corpus-grammar*)
(comprehend '("nao" "sei" "como" "disser" "lhe" ) :cxn-inventory *portuguese-corpus-grammar*)

(comprehend-and-formulate '("telefonou" "lhe" "o" "paulo") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend-and-formulate '("tu" "compras" "o" "bolo") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend-and-formulate '("nada" "me" "deram") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend-and-formulate '("disse" "me" "o" ) :cxn-inventory *portuguese-corpus-grammar*)

;;WORK BENCH
(comprehend-and-formulate '("deu" "lhe" "o") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend-and-formulate '("e" "possivel" "convidar" "a") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("eu" "penso" "convidar" "a") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("eu" "penso" "nao" "o" "convidar") :cxn-inventory *portuguese-corpus-grammar*)


;(comprehend-and-formulate '( "se" "sabe" "pouco" "de" "ti") :cxn-inventory *portuguese-corpus-grammar*)


;;no meaning difference!! both can be produced
(comprehend-and-formulate '("o" "joao" "raramente" "me" "ve") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend '("o" "joao" "ve" "me" "raramente") :cxn-inventory *portuguese-corpus-grammar*)


;(web-interface::predicate-network-formulation-fcg-light->svg meaning)

(comprehend-and-formulate '("nao" "sei" "como" "disser" "lhe") :cxn-inventory *portuguese-corpus-grammar*) ;;enclisis but subordination??
(comprehend-and-formulate '("sempre" "me" "perguntaram" "por" "ti") :cxn-inventory *portuguese-corpus-grammar*) ;;sempre not produced in right place
(comprehend '("os" "alunos" "telefonaram" "lhe" "todos") :cxn-inventory *portuguese-corpus-grammar*) ;;todos not parsed (determiner)
(comprehend '("de" "este" "livro" "me" "lembro" "eu") :cxn-inventory *portuguese-corpus-grammar*)
;;sempre me perguntaram por ti


de este livro me lembro eu
eu sei que ele o encontrara
de este livro me lembro bem 
o miguel encontro a em o cinema
o pedro encontrou a em o cinema
eles vieram ca para me visitar
so o pedro me deu uma prenda 


;; New constructions needed for final evaluation of corpus
(comprehend '("contaste" "lhes" "o_que") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend '("todos" "os" "rapazes" "me" "ajudaram") :cxn-inventory *portuguese-corpus-grammar*) 

(comprehend-and-formulate '("e" "possivel" "convidar" "a") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend-and-formulate '("e" "possivel" "nao" "a" "convidar") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend '("eu" "penso" "convidar" "a") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("eu" "penso" "convidar" "a") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend '("eu" "penso" "nao" "o" "convidar") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend-and-formulate '("eu" "penso" "nao" "o" "convidar") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend-and-formulate '( "poe" "os" "em" "a" "rua") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("ele" "poe" "os" "em" "a" "rua") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend-and-formulate '("se" "sabe" "pouco") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("de" "ti" "se" "sabe" "pouco" ) :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '( "se" "sabe" "pouco" "de" "ti") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("de" "ele" "se" "sabe" "pouco" ) :cxn-inventory *portuguese-corpus-grammar*)

nao sei como disser lhe 

;;split in search due to syncretic "o"
;;>> make determiner cxn phrasal?
(comprehend '("eu" "nao" "o" "leio") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("todos" "os" "rapazes" "me" "ajudaram") :cxn-inventory *portuguese-corpus-grammar*)


;;TEST SENTENCES FOR COLING
(comprehend-and-formulate '("disse" "me" "o") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("alguem" "me" "ajudou") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend-and-formulate '("eu" "disse") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend '("isso"  "disse" "lhe" "eu") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend '("eu" "penso" "convidar" "a") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("o_que" "lhes" "contaste") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("tu" "compras" "o" "bolo") :cxn-inventory *portuguese-corpus-grammar*)
;;split in search due to syncretic "os" (same as above)
(comprehend-and-formulate '("a_quem" "os" "entregaste") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("quem" "te" "deu" "este" "livro" ) :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("nada" "me" "deram") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("alguns" "alunos" "lhe" "telefonaram") :cxn-inventory *portuguese-corpus-grammar*)
;; IS THIS SENTENCE ACTUALLY CORRECT?


(comprehend-and-formulate '("o" "pedro" "ajudou" "me") :cxn-inventory *portuguese-corpus-grammar*)
;; why proclisis? (indefinite pronoun = quantified subject) I had to introduce a new trigger cxn
(comprehend-and-formulate '("alguem" "me" "ajudou") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend-and-formulate '("a_quem" "o" "deste" "tu" ) :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("eu" "dei" "te" "este" "livro" "porque" "tu" "o" "querias") :cxn-inventory *portuguese-corpus-grammar*)
;(comprehend-and-formulate '("algumas" "pessoas" "o" "leem")  :cxn-inventory *portuguese-corpus-grammar*)
;(comprehend-and-formulate '("algumas" "pessoas"  "leem" "o")  :cxn-inventory *portuguese-corpus-grammar*)
;; proclisis (fronted focus):
(comprehend-and-formulate '("isso" "lhe" "disse" "eu") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("este" "livro" "te" "dei" "eu") :cxn-inventory *portuguese-corpus-grammar*)

(defun get-cip-leaves (cip)
  "Helper function: get all leaves (final nodes) from the cip search
tree."
  (remove nil
          (traverse-depth-first cip
                                :collect-fn #'(lambda (node) 
                                                (when (fully-expanded? node) node)))))

(defun get-last-cip-node (cip) 
  "Helper function: extract last node that is consulted from a
construction inventory processor (cip). Useful when there is no
solution."
  (let ((last-node
	 (loop for node in (get-cip-leaves cip)
	    when (find 'succeeded (statuses node))
	    return node)))
    (if last-node
	last-node
	(first (last (get-cip-leaves cip))))))


(comprehend-and-formulate '("deu" "lhe" "o") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("deu" "nos" "o") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("deu"  "o") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend '("de_ti" "se" "sabe" "pouco") :cxn-inventory *portuguese-corpus-grammar*)
;;TO ADD: reflexive cxn, prepositional phrase, argument structure cxn


(comprehend-and-formulate '("em" "este" "livro") :cxn-inventory *portuguese-corpus-grammar*)
;;cannot parse sentences without a subject yet
(comprehend-and-formulate '("ele" "poe" "os" "em" "a" "rua") :cxn-inventory *portuguese-corpus-grammar*)


(comprehend '("eu" "sei" "que" "ele" "o" "encontrara") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend-and-formulate '("o" "policia" "e" "meu" "tio") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("o" "policia" "que" "te" "viu") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend '("o" "policia" "que" "te" "viu" "e" "meu" "tio") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("o" "policia" "que" "te" "viu" "e" "meu" "tio")
                          :cxn-inventory *portuguese-corpus-grammar*)


;; PROPOR SENTENCES
;;-----------------------------
(defparameter *portuguese-corpus-grammar* (create-grammar))
(activate-monitor trace-fcg-light)
;;default
(comprehend-and-formulate '("eu"  "dei" "te" "este" "livro") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("tu"  "querias" "o" "livro") :cxn-inventory *portuguese-corpus-grammar*)
;;fronted focus
(comprehend-and-formulate '("este" "livro" "te" "dei" "eu") :cxn-inventory *portuguese-corpus-grammar*)
;;wh-question words
(comprehend-and-formulate '("quem" "te" "deu" "este" "livro" ) :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("a_quem" "o" "deste" "tu" ) :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("tu" "deste" "o" "a_quem") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend-and-formulate '("ele" "nao" "te" "deu" "este" "livro") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend-and-formulate '("eu" "leio" "o"  "raramente") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("eu" "raramente" "o" "leio" ) :cxn-inventory *portuguese-corpus-grammar*)

(comprehend-and-formulate '("poucas" "pessoas" "o" "leem") :cxn-inventory *portuguese-corpus-grammar*)

(comprehend-and-formulate '("eu" "ontem" "vi" "te") :cxn-inventory *portuguese-corpus-grammar*)
(comprehend-and-formulate '("eu" "dei" "te" "este" "livro" "porque" "tu" "o" "querias") :cxn-inventory *portuguese-corpus-grammar*)








