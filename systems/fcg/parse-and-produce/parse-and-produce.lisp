
(in-package :fcg)

(export '(produce produce-started produce-finished 
          produce-all produce-all-started produce-all-finished        
	  parse parse-started parse-finished
          parse-all parse-all-started parse-all-finished
          render de-render create-initial-structure))

(defgeneric create-initial-structure (meaning mode)
  (:documentation "Makes a new coupled feature structure that
                   contains the meaning"))

(defgeneric render (object mode &key &allow-other-keys)
  (:documentation "Renders a structure into an utterance"))

(defgeneric de-render (utterance mode &key &allow-other-keys)
  (:documentation "De-renders an utterance into a coupled feature structure."))

(defgeneric produce (meaning construction-inventory &optional silent)
  (:documentation "produces an utterance for meaning"))

(defgeneric produce-all (meaning construction-inventory &key silent n)
  (:documentation "produces all or max n utterances meaning"))

(defgeneric parse (utterance construction-inventory &optional silent)
  (:documentation "parses an utterance"))

(defgeneric parse-all (utterance construction-inventory &key silent n)
  (:documentation "parses an utterance until n or all solutions/meanings are found"))

(define-event produce-started (meaning list)
              (construction-inventory construction-inventory)
              (initial-cfs coupled-feature-structure))

(define-event produce-finished (utterance list))

(define-event produce-all-started (n t) (meaning list)
              (construction-inventory construction-inventory))

(define-event produce-all-finished (utterances list))

(define-event parse-started (utterance list) (initial-cfs coupled-feature-structure))

(define-event parse-finished (meaning list)
              (construction-inventory construction-inventory))

(define-event parse-all-started (n t) (utterance list))

(define-event parse-all-finished (meanings list)
              (construction-inventory construction-inventory))

