;; file called fron.lisp
;; this file contains a minimal system for defining an ontology
;; to be used for generating world models in a cognitive linguistics style 
;; created by steels 24 august 2008 

;; extended by steels 9 may 2011 to make facts also structures 
;; revised again by steels april 2012 adding new functionalities such as a matcher and events 

(in-package :fron)

(export '(def-class do-def-class def-object do-def-object world-model-facts
           an def-event def-world-model do-def-world-model print-world-model instantiate
           do-instantiate def-ontology do-def-ontology compare do-compare instantiate-event
           do-instantiate-event world-model-individuals fact-present 
           *world-model* *frontology* make-item *item-counter* *world-models*
           html-ontology do-html-ontology))


;;###########################################################################
;; Helper functions
;;###########################################################################

;; general auxiliary functions for manipulating a-lists
;; presumably something like this exists??

(defmacro get-value (element a-list)
  `(cdr (assoc ,element ,a-list)))

(defmacro add-change-value (element value a-list)
  `(let ((the-element ,element)
         (the-value ,value))
    (let ((existing-pair (assoc the-element ,a-list)))
      (if existing-pair
        (if (not (equal (cdr existing-pair) the-value)) 
          (rplacd existing-pair the-value))
        (setf ,a-list
              (cons (cons the-element the-value)
                    ,a-list)))
      the-value)))


;;###########################################################################
;; Global variables for creating new symbols and ontologies/world-models
;;###########################################################################

;; some functions for creating new symbols
;; should use the ones in Babel when this is linked into Babel

(defvar *fact-counter* 0)
(setf *fact-counter* 0)
(defvar *item-counter* 0)
(setf *item-counter* 0)
(defvar *var-counter* 0) ; when variables are created as part definitions
(setf *var-counter* 0)

(defun init-counter ()
  (setf *item-counter* 0)
  (setf *fact-counter* 0)
  (setf *var-counter* 0))

;(defclass entity ()
; ((id :type symbol :initarg :id :accessor id))) ; should come from IRL 

;(defclass individual (entity) 
  ; the facts in which this individual is involved
  ; could possibly be a slot in entity because will be needed for all FRON entities 
;  ((facts :type list :initarg :facts :accessor facts)))

;(defclass action (individual) nil)


;;; some global variables for ontologies and world models 

(defvar *frontology*) ; for the time being there is a global ontology
; this could be made local to each agent later 
(setf *frontology* nil)
(defvar *frontologies*) ; all the ontologies
(setf *frontologies* nil)

(defvar *world-models*) ; the set of all world models
; this will be equal to an a-list ((label . <world-model>) ...)
(setf *world-models* nil)

(defun def-init-world-models ()
  (setf *world-models* nil))

(defparameter *world-model* nil) ; current world model is global could be made local to agent

;;###########################################################################
;; Create, continue and reset world model and ontology
;;###########################################################################

(defstruct ontology
  ; an ontology has
  ; a label 
  ; frames for classes stored as ((label . <definition>) ...)
  ; objects stored as ((label . <definition>) ...)
  ; events stored as ((label . <definition>) ...)
  label classes objects events)

(defstruct world-model
  ; a world model has a label
  ; a set of objects stored as ((label . <item>) ...) 
  ; a set of facts about the objects stores as a straight list most recent first ((pred arg1 arg2) ...)
  ; and a set of individuals occuring in these facts  ((label . <item>) ...) 
  label objects facts individuals)

(defstruct fact 
  ; a fact is now also a structure
  number ; just a way to refer back to facts 
  content ; a (<pred> <arg> ...) list
  time ; can be anything at this point 
  truth ; a value between -1 (false) and 1 (true) 
  source) ; how the fact was derived 

;; some ways to make it easy to define world models
;; assuming global variables for *WORLD-MODEL* and *ONTOLOGY* 

(defmacro def-world-model (&body args)
  ; begins the definition of a new world model
  ; syntax: (def-world-model <label>)
  `(progn
     (setf *world-model* (make-world-model :label ',(car args))) ; start a new world-model
     (add-change-value ',(car args) *world-model* ; store the world-model 
                        *world-models*)           
     (setf ,(car args) *world-model*))) ; set it to the label that is given to make later access easier

(defmacro continue-world-model (&body args)
  ; this is useful when you want to set the global variable *world-model* again 
  `(let
     ((existing-world-model
       (get-value ',(car args) *world-models*)))
     (if existing-world-model
       (setf *world-model* existing-world-model)
       (format t "~%World-model ~a does not exist, create it first"))))

(defmacro end-world-model () ; end the definition of the current world model 
  (setf *world-model* nil))

(defmacro print-world-model (&body args)
  ; prints a world model 
  `(do-print-world-model ',(car args)))

(defun do-print-world-model (label)
  ; prints all the facts in the world model 
  (let
   ((world-model (if (null label) *world-model* ; default
                   (get-value label *world-models*))))
    (if world-model
      (progn 
        (format t "~%Facts in ~a:" (world-model-label world-model))
        (dolist (fact (world-model-facts *world-model*))
          (format t "~%~a. ~a" (fact-number fact) (fact-content fact))
          (let* ((time-stamp (fact-time fact))
                (begin (if time-stamp (time-stamp-begin time-stamp)))
                (end (if time-stamp (time-stamp-end time-stamp))))
            (cond
             (begin 
              (if end ; begin and end are known 
                (format t " @[~a,~a]" (time-point-point begin) (time-point-point end))
                (format t " @[~a,-]" (time-point-point begin)))) ; only begin 
             (end ; only end
              (format t " @[-~a]"  (time-point-point end)))
             (t nil)))))
      (format t "~%Unknown world model: ~a" label))))

(defmacro html-world-model (&body args)
  ; prints a world model 
  `(do-html-world-model ',(car args)))

(defun do-html-world-model (label)
  ; prints all the facts in the world model
  ; on the current web page 
  (let
   ((world-model (if (null label) *world-model* ; default
                   (get-value label *world-models*))))
    (if world-model
      (progn 
        (add-element '((br)))
        (add-element (format nil "~%Facts in ~a:" (world-model-label world-model)))
        (dolist (fact (world-model-facts *world-model*))
          (add-element '((br)))
          (add-element (format nil "~%~a. ~a" (fact-number fact) (fact-content fact)))
          (let* ((time-stamp (fact-time fact))
                (begin (if time-stamp (time-stamp-begin time-stamp)))
                (end (if time-stamp (time-stamp-end time-stamp))))
            (cond
             (begin 
              (if end ; begin and end are known
                  (add-element
                   (format nil " @[~a,~a]" (time-point-point begin) (time-point-point end)))
                  (add-element
                   (format nil " @[~a,-]" (time-point-point begin))))) ; only begin
             (end ; only end
              (add-element (format nil " @[-~a]"  (time-point-point end))))
             (t nil)))))
      (add-element (format nil "~%Unknown world model: ~a" label)))))



(defmacro print-o-world-model (&body args)
  ; prints a world model with facts grouped per object 
  `(do-print-o-world-model ',(car args)))

(defun do-print-o-world-model (label)
  ; prints all the facts in a world model with the given label 
  ; assumes that each individual has a property list with all facts in
  ; which it appears AS FIRST ARGUMENT
  ; is just a way to look at contents of the world model 
  (let*
      ((world-model (if (null label) *world-model* ; default
                      (get-value label *world-models*)))
       (individuals (world-model-individuals (get-value label world-model))))
    (dolist (individual individuals)
      (if (get individual 'facts)
        (progn
          (format t "~%== ~a == " individual)
          (dolist (fact-structure (get individual 'facts))
            (let ((fact (fact-content fact-structure)))
              (if (cddr fact) ; there are args 
                (format t "~%  ~a:" (car fact))
                (format t "~% a ~a" (car fact)))
              (dolist (arg (cddr fact))
                (format t " ~a" arg)))))))))

(defun print-world-objects (label)
  ; prints the objects in the world model
  (let
   ((world-model  (get-value label *world-models*)))
    (if world-model
      (progn 
        (format t "~%Objects: ~a" (world-model-label world-model))
        (dolist (object (world-model-objects *world-model*))
          (format t "~% ~a: ~a" (car object) (cdr object)))))))

(defmacro def-ontology (&body args)
  `(do-def-ontology ',(car args)))

(defun add-ontology (name ontology)
  ;; adds a new ontology to the existing list of ontologies 
  (setf *frontologies* (cons (cons name ontology) *frontologies*))
  )
(defun get-ontology (name)
  ; gets the ontology with the given name 
  `(cdr (assoc ',name *frontologies*)))

(defun do-def-ontology (ontology-name)
  ; creates a new ontology and sets it to the current ontology
  (let
      ((new-ontology (make-ontology :label ontology-name)))
    (add-ontology ontology-name new-ontology) ; adds it to global list of ontologies 
    (setf *frontology* new-ontology))) ; make it the current one 
; do we still want this?     (setf (car args) *frontology*)))
    
(defmacro continue-ontology (&body args)
  ; sets the current ontology to a given ontology and creates one if not yet existing
  `(do-continue-ontology ',(car args)))

(defun do-continue-ontology (ontology-name)
  ; get the ontology and set it to the current ontology
  (let ((ontology (cdr (assoc ontology-name *frontologies*))))
    (if (null ontology) 
       (format t "~%Ontology ~a does not exist, create it first" ontology-name)
       (setf *frontology* ontology))))

;; stuff for printing out an ontology
;; this needs to be revised 

(defun print-ontology ()
  (if *frontology*
    (progn (format t "~%")
      (format t "~%~%(DEF-ONTOLOGY ~a)" (ontology-label *frontology*))
      (do-print-ontology *frontology*))
    (format t "~%No ontology defined yet")))

(defun do-print-ontology (ontology)
  (dolist (frame (reverse (ontology-classes ontology)))
    (format t "~%~%(DEF-CLASS ~a" (car frame))
    (do-print-frame (cdr frame)))
  (dolist (frame (reverse (ontology-objects ontology)))
    (format t "~%~%(DEF-OBJECT ~a" (car frame))
    (do-print-frame (cdr frame))))

(defun html-ontology ()
  (if *frontology*
    (progn (add-element '((br)))
      (add-element (format nil "ONTOLOGY: ~a" (ontology-label *frontology*)))
      (do-html-ontology *frontology*))
    (format t "~%No ontology defined yet")))

(defun do-html-ontology (ontology)
  (if (ontology-classes ontology)
    (progn (add-element '((br)))
      (add-element 
       (format nil "CLASSES:"))))
  (dolist (frame (reverse (ontology-classes ontology)))
    (add-element (format nil " ~a " (car frame)))

;    (do-html-frame (cdr frame)))
       )
  (if (ontology-objects ontology)
        (progn (add-element '((br)))
          (add-element 
           (format nil "OBJECTS:"))))
  (dolist (frame (reverse (ontology-objects ontology)))
    (add-element (format nil "  ~a " (car frame)))
;    (do-print-frame (cdr frame))
    ))

(defun make-item (name type world-model)
  (let ((new-individual
         (make-symbol
          (format nil "~a-~a" name (setf *item-counter* (+ 1 *item-counter*))))
;         (make-instance type
;                        :facts nil 
;          :id 
;          (format nil "~a-~a" name (setf *item-counter* (+ 1 *item-counter*))))
         ))
    ; add this individual to the current world model
    (setf (world-model-individuals world-model)
          (cons new-individual (world-model-individuals world-model)))
    new-individual))

(defun make-var (name)
  (let ((new-var   (make-symbol
                    (format nil "?~a-~a" name (setf *var-counter* (+ 1 *var-counter*))))))
    ; we specify on the p-list of this new symbol that it is a variable 
    (setf (get new-var 'var-p) t)
    new-var))

(defun var-p (symbol)
  ; so there is a quick cheap test to find out
  (get symbol 'var-p))

;;###########################################################################
;; Handling time 
;;###########################################################################

;; each fact has a period during which it is valid (= (begin end)) 
;; every time point is relative to a reference frame
;; a reference frame has a beginning and an end

(defstruct time-frame
  begin end)

(defstruct time-point
  frame point)

(defstruct time-stamp
  begin end)

(defun new-time-frame ()
  (make-time-frame :begin 0 :end 0))

(defun compute-new-begin (current level)
  ; this cuts off all digits beyond a certain level
  ; and then increments at that level
  ; there is some residue due to use of floor -- should be fixed one day
  ; e.g. 1.23 => 1.2 if level = 1 => outcome = 1.3  
  ; e.g. 1.523412123 => 1.0 if level = 0 
  (let*
      ((the-fixnum (floor (* current (expt 10 level))))
       (new-num (* the-fixnum (* (expt 10 (- level))) 1.0)))
      (+ new-num
         (expt 10 (- level)))))

;(trace compute-new-begin)

(defun new-time-point (frame level)
  ; make a new time point and update the frame 
  (format t "~%~a " (time-frame-end frame))
  (setf (time-frame-end frame)
        (compute-new-begin (time-frame-end frame) level))
  (format t "~%~a " (time-frame-end frame))
  (make-time-point
   :frame frame
   :point (time-frame-end frame)))

;;###########################################################################
;; Manipulating world model and ontology
;;###########################################################################

(defun add-object (object item world-model)
  (let ((existing-pair (assoc object (world-model-objects world-model))))
    (if existing-pair
      ; object with this label exists already but is now replaced
      (rplacd existing-pair item)
      (setf (world-model-objects world-model)
            (cons (cons object item) (world-model-objects world-model))))))

(defun fact-present (fact-content all-facts) 
  ; rewritten to take facts as objects into account
  ; returns the fact 
  (let ((found-fact nil))
    (do ((remaining-facts all-facts (cdr remaining-facts)))
        ((null remaining-facts))
      (if (equal (fact-content (car remaining-facts)) fact-content)
        (return (setf found-fact (car remaining-facts)))))
    found-fact))

(defun add-fact (content world-model &optional (time nil))
  (if (fact-present content (world-model-facts  world-model))
    'already-there ; but maybe not with the same truth-value or time stamp?? check this later
    (let ((fact (make-fact :number (setf *fact-counter* (+ 1 *fact-counter*))
                           :content content :truth T :source 'defined :time time)))
      ; store all facts for a given object for efficiency in retrieval later 
      ;(setf (facts (second content))
      ;      (cons fact (facts (second content))))
      (setf (world-model-facts world-model)
          (setf (world-model-facts world-model)
                (cons fact (world-model-facts world-model))))
      fact)))

;; (defun add-constraint (fact world-model)
;;   ; is this still needed?? 
;;   (let ((wm-constraints (world-model-constraints world-model)))
;;     (if (member fact wm-constraints)
;; 	'already-there 
;; 	(setf wm-constraints
;; 	      (cons fact (world-model-constraints world-model))))))

(defun existing-fact-p (fact)
  (member fact (world-model-facts *world-model*)))

;; some access functions defined by Katrien? 

(defun retrieve-frame-type-from-onto (label)
  ; retrieve the definition of a frame or a type based on its label 
  (or 
   (cdr (assoc label (ontology-classes *frontology*)))))

(defun retrieve-frame-from-onto (label)
  ; retrieve the definition of a frame or a type based on its label 
  (cdr (assoc label (ontology-classes *frontology*))))

(defun retrieve-object-from-onto (label)
  ; retrieve the definition of an object based on its label 
  (cdr (assoc label (ontology-objects *frontology*))))

(defun retrieve-object-from-wm (label)
   ; retrieve the definition of an object based on its label 
  (cdr (assoc label (world-model-objects *world-model*))))

;;###########################################################################
;; Frames and objects
;;###########################################################################

(defstruct frame
  label
  definition
  evoking
  arguments
  self-variable
  states
  actions)

(defmacro print-frame (&body body)
 `(let ((frame
          (get-value ',(car body) (ontology-classes *frontology*))))
    (if frame
      (progn 
        (format t "~%(DEF-FRAME ~a" (frame-label frame))
        (do-print-frame frame))
      (let ((object 
             (get-value ',(car body) (ontology-objects *frontology*))))
        (if (null object)
          (format t "~%No frame for ~a" ',(car body))
          (progn
            (format t "~%(DEF-OBJECT ~a" (frame-label frame))
            (do-print-frame frame)))))))

(defun do-print-frame (frame)
  (if (frame-arguments frame)
    (progn 
      (format t "~%  (") ; begin arguments 
;  (if (frame-self-variable frame)
;    (format t "(self ~a)"  (frame-self-variable frame)))
      (format t "~a" (car (frame-arguments frame))) ; first one 
      (mapcar #'(lambda (slot)
                  (format t "~%   ~a" slot))
              (cdr (frame-arguments frame)))
      (format t ")")))
  (mapcar #'(lambda (def)
              (format t "~%  ~a" def))
          (frame-definition frame))
  (format t ")"))

(defun do-define-class (label arguments definition ontology)
  ; defines a class by creating a frame for it and adds it to the current ontology 
   (let ((new-frame (define-frame label arguments definition)))
    (if (null *frontology*)
	(format t "~%Define first an ontology using def-ontology")
	(let ((existing-pair (assoc label (ontology-classes ontology))))
	  (if existing-pair ; change through side effect if already there !!!!
            (progn
              (format t "~%Warning: Redefining class ~a" new-frame)
	      (rplacd existing-pair new-frame))
            (setf (ontology-classes ontology) ; otherwise add it 
                  (cons (cons label new-frame)
                        (ontology-classes ontology))))
          new-frame))))

(defmacro def-frame (&body body)
  ; variant of def-class to be backward compatible 
  `(let* ((label ',(car body))
          (remainder (transform-definition ',(cdr body))) ; is for handling syntactic shorthands 
          (args (car remainder))
          (evoking (cdr remainder)))
     (do-define-class label args evoking)))

(defmacro def-class (&body body)
  ; way for defining the frame for a class with a label, a set of roles, and frames that will be evoked
  ; example:   
  ; (def-class tube (tube-size)
  ;     (a container ?self))
  ; work is done by do-define-class
  `(let* ((label ',(car body))
          (the-args ',(second body)))
     (if  (and the-args ; there are arguments
               (member (car the-args) '(a an))) ; but it is already a description
       (do-define-class label  nil ',(cdr body) *frontology*) ; no args 
       (do-define-class label the-args ',(cddr body) *frontology*))))

(defun define-frame (label old-arguments evokes)
  ; can be further simplified
  (let
      ((arguments
        (mapcar #'(lambda (argument)
                    (if (symbolp argument) (list argument) ; in case only one argument given 
                      argument))
                old-arguments)))
    (make-frame :label label
                    :self-variable '?self
                    :arguments arguments 
                    :definition evokes)))

(defmacro def-object (&body body)
  ; for defining individual objects
  ; syntax (def-object label (a this ?self) ... (a that))
  `(let* ((label ',(car body))
         (evoking ',(cdr body)))
      (do-def-object label evoking *frontology*)))


(defun do-def-object (label evoking ontology) 
  ; for defining individual objects
  ; syntax (def-object label (a this ?self) ... (a that))
  ; returns the new object 
  (let
      ((new-object (define-object label (transform-definition evoking))))
     (if (null ontology)
       (format t "~%Define first an ontology using def-ontology")
       (let ((existing-pair (assoc label (ontology-objects ontology))))
         (if existing-pair ; change through side effect if already there
           (progn 
             (format t "~%Warning: object ~a redefined" label)
             (rplacd existing-pair new-object))
           (setf (ontology-objects ontology)
                 (cons (cons label new-object)
                   (ontology-objects ontology))))
         new-object))))

(defun define-object (label evokes)
  ; object definition is like frame definition except no arguments 
  (make-frame :label label
              :self-variable '?self
              :arguments nil 
              :definition evokes))

(defmacro def-event (label &key roles evokes states actions)
  `(do-define-event ',label ',roles ',evokes ',states ',actions *frontology*))

(defun do-define-event (label arguments evokes states actions ontology)
   (let ((new-frame (define-frame label arguments evokes)))
     (setf (frame-states new-frame) states)
     (setf (frame-actions new-frame) actions)
    (if (null *frontology*)
	(format t "~%Define first an ontology using def-ontology")
	(let ((existing-pair (assoc label (ontology-events ontology))))
	  (if existing-pair ; change through side effect if already there !!!!
            (progn
              (format t "~%Warning: Redefining class ~a" new-frame)
	      (rplacd existing-pair new-frame))
            (setf (ontology-events ontology) ; otherwise add it 
                  (cons (cons label new-frame)
                        (ontology-events ontology))))
          new-frame))))

(defun transform-definition (expression)
  ; handles syntax short-hand  
  ; changes ((slot (a type)) ... ) to ((slot ?var)) (a type ?var)))
  ; for example: 
  ; (def-frame state-change ?s
  ;   (change-object (an object)))
  ; =>
  ; (def-frame state-change ?s
  ;   (change-object ?object-1)
  ;   (an object ?object-1))
  (let*
    ((new-concepts nil)
     (arguments 
      (mapcar
       #'(lambda (element)
;           (format t "~%=++ ~a" element)
         (cond
          ((symbolp element) element) ; name of argument 
          ((symbolp (second element)) element) ; (label + var) 
          ((member (car (second element)) '(a an))
           (let
               ((new-var (make-var (second (second element)))))
             (setf new-concepts
                   (cons `(a 
                           ,(second (second element))
                           ,new-var ,@(cddr (second element)))
                         new-concepts))
             (list (car element) new-var)))
          (t 
           element)))
       (car expression))))
    (cons arguments
          (append new-concepts (cdr expression)))))

; should become macros 

(defun get-frame (label)
  ; retrieve the definition of a frame or a type based on its label 
  (cdr (assoc label (ontology-classes *frontology*))))

(defun get-object (label)
  ; retrieve the definition of an object based on its label 
  (cdr (assoc label (ontology-objects *frontology*))))

(defun get-event (label)
  ; retrieve the definition of an event based on its label 
  (cdr (assoc label (ontology-events *frontology*))))

(defmacro instantiate (&body args)
  ; instantiates
  `(do-instantiate ',(car args) ',(cdr args) *world-model*))
                     
(defun do-instantiate (concept args world-model)
  ; takes a concept-name a list of arguments and instantiates all the facts
  ; always returns a list of bindings
  ; the ?self is bound to the referent of the concept 
  ; 1. check whether frame exists as object
;  (format t "~%do-instantiate: ~a ~a ~a" concept  (get-object concept)
;          (get-value concept (world-model-objects world-model)))
  (let ((current-object-instance
           (and (get-object concept) ; the concept refers to an object
                (get-value concept (world-model-objects world-model))))) ; and it already exists
    ; in that case return it does not need to be re-instantiated again 
    (if current-object-instance
      (list (cons '?self current-object-instance))
      (let
      ; 3. if not, deal with arguments first, i.e. generate facts for them 
          ((filler-arguments
            (create-filler-arguments concept args world-model)))
      ; and now generate all the facts for this concept 
        (generate-facts concept nil filler-arguments nil world-model)
        ))))

(defun do-instantiate-event (concept args world-model)
  ; variant of do-instantiate that will also instantiate a state machine 
  ; takes a concept-name a list of arguments and instantiates all the facts
  ; always returns a list of bindings
  ; the ?self is bound to the referent of the concept 
  ; first instantiate the concept itself
  (let ((bindings (do-instantiate concept args world-model)))
    (do-instantiate-state-machine bindings
                                  (frame-states (get-event concept))
                                  (frame-actions (get-event concept)) world-model)))

;(trace do-instantiate-state-machine)

(defun do-instantiate-state-machine (bindings states actions world-model)
  (let
      ((self (get-value '?self bindings)) ; the event as a whole 
       (time-frame (new-time-frame))
       ;(bindings nil)
       (state-bindings nil)
       (added-facts nil)) ; create new time frame
    (dolist (state states) ; no info on time yet 
      (multiple-value-bind (not-used added)
          (handle-evoked nil (second state) nil nil world-model)
        (declare (ignore not-used))
        (setf state-bindings (cons (cons (car state) added) state-bindings))
        (setf added-facts (append added added-facts))))
    (dolist (action-spec actions)
      (let* ((new-action (make-item (car action-spec) 'action world-model))
            (time-point
             (new-time-point time-frame 0))
            (time-stamp
             (make-time-stamp :begin time-point)))
        ; assert that the step 
        (setf added-facts
            (cons (add-fact (list (car action-spec) new-action self)
                            world-model time-stamp)
                  added-facts))
      (let ((actions (cdr (assoc :action (cdr action-spec)))))
        ; assert that the action took place at the new time point
        (dolist (action actions)
              (multiple-value-bind (not-used added)
                  (handle-evoked new-action action nil nil world-model time-stamp)
                (declare (ignore not-used))
                (setf added-facts (append added added-facts)))))
      (let ((results (cdr (assoc :begin (cdr action-spec)))))
        ; assert that the result of the action took place
        (dolist (result results)
          (let ((facts (get-value result state-bindings)))
            (format t "~%Begin:~a" facts)
            (dolist (fact facts)
              (setf (fact-time fact)
                    (make-time-stamp :begin time-point))))))
      (let ((results (cdr (assoc :end (cdr action-spec)))))
        ; assert facts that are no longer valid 
        (dolist (result results)
          (let ((facts (get-value result state-bindings)))
            (format t "~%End:~a" facts)
            (dolist (fact facts)
              (setf (time-stamp-end (fact-time fact)) time-point))))))
      )
    added-facts))

; inefficient needs to be reprogrammed and done only once (at read time) 
(defun do-state-description (description)
  (let ((do-part nil)
        (do-found nil))
    (dolist (element description)
      (cond
       ((and do-found (equal element :post)) (return do-part))
       (do-found
        (setf do-part (cons element do-part)))
       ((equal element :do) (setf do-found t))))
    do-part))

(defun post-state-description (description)
  (let ((post-part nil)
        (post-found nil))
    (dolist (element description)
      (cond
       (post-found
        (setf post-part (cons element post-part)))
       ((equal element :post) (setf post-found t))))
    post-part))


;(untrace)
;(trace do-instantiate-event)
;(trace do-instantiate)
;(trace create-filler-arguments)
;(trace do-instantiate)
;(untrace)

; (trace get-value)

(defun create-filler-arguments (frame args world-model)
  (declare (ignore frame))
  (mapcar #'(lambda (arg) ; loop over given arg pairs and look up their definitions
              (let ((filler (get-value (second arg)
                                       (world-model-objects world-model))))
                (if filler ; is already instantiated 
                  arg
                  (let*
                      ((existing-object ;otherwise get definition of this object 
                        (get-object (second arg)))
                       (new-filler 
                        (if (null existing-object) 
                          (get-value '?self ; instantiate returns a list of bindings one of them is the object itself
                                     (do-instantiate
                                      (second arg) nil world-model)))))
                    (cond
                     (new-filler
                      ; we have created a new object so it needs to be stored in the world model 
                      (add-object concept-label self world-model)
                      (list (car arg) new-filler))
                     (t ; we keep as is
                      arg))))))
          args))


;(trace create-filler-arguments)

;(trace generate-facts)
;(trace do-instantiate)
;(untrace)

; (trace generate-facts)
; (trace handle-evoked)
; (untrace)
;(trace handle-argument)

(defun handle-argument (self arg-spec args bindings world-model &optional time)
  (let*
      ((arg (car arg-spec))
       (constraints (find-constraints (cdr arg-spec))) ; these are extra descriptions attached here
       (variable (find-variable (cdr arg-spec)))
       (filled-arg (assoc arg args))
       (item (if filled-arg ; there is already a binding 
               (second filled-arg) 
               (make-item arg 'individual world-model)))
       (added-facts nil)) ; else create new one
;    (format t "~%Filled: ~a ~a" ARG ITEM)
;    (if (null filled-arg) (add-object item)) ; new object is anonymous 
    (setf added-facts
          (list (add-fact ; now add fact 
                 (list arg self item) world-model time)))
    (if variable
      (setf bindings ; add variable if not there yet 
            (cons (cons variable item) bindings)))
    (mapcar #'(lambda (constraint) ; add constraints about this item 
                (setf added-facts
                      (cons (add-fact (list constraint item) world-model time)
                            added-facts)))
            constraints)
    (values bindings added-facts)))

(defmacro def-instance (&body descriptions)
  `(let ((bindings nil))
     (dolist (evoke-spec ',descriptions)
       (setf bindings (handle-evoked nil evoke-spec nil bindings *world-model*)))
;       (do-instantiate (second evoke-spec) (cddr evoke-spec) *world-model*))
     bindings))

; (trace handle-evoked)
; (trace add-fact)
; (trace do-instantiate)

(defun handle-evoked (self evoke-spec args bindings world-model &optional time)
  (declare (ignore self args))
  ; returns the set of bindings 
  ; example (a left-of ?r (source-left-of ?s))
  ; self is object being described by global frame
  ; my-self is when object is different from self
  (let*
      ((type (second evoke-spec))
       (variable (find-variable (cddr evoke-spec))) 
       (bound-variable (cdr (assoc variable bindings)))
       (my-self
        (if bound-variable bound-variable
          (make-item type 'individual world-model)))
       (new-args nil)
       (added-facts nil))
;    (if (null bound-variable) (add-object type my-self))
;    (add-fact (list type my-self))
    (if variable 
      (setf bindings ; add variable if not there yet
            (add-binding variable my-self bindings)))
;    (format t "~%Handle bindings in evoked for ~a" (cddr evoke-spec))
    (dolist (arg (cddr evoke-spec))
      (if (varp arg) ; ignore is the spec of the variable
        t
        (cond
         ((varp (second arg)) ; the filler is a variable
          (format t "~%!! variable ~a" (second arg))
          (let 
            ((filler (cdr (assoc (second arg) bindings))))
            ; is it already bound?
            (if filler ; there is a filler 
              (setf new-args ; so use it in the new arg list 
                    (cons (list (car arg) filler) new-args))
              ; otherwise generate a warning but ignore this arg 
              (and (format t "~%Warning: Unbound variable ~a in ~a" (second arg) evoke-spec)
                   t))))
         ((symbolp (second arg)) ; the filler is the name of an object
          (multiple-value-bind
              (new-bindings added) 
              (do-instantiate (second arg) nil world-model)
            (setf added-facts (append added added-facts))
            (let 
              ((referent (if new-bindings (get-value '?self new-bindings))))
              (if referent ; add the binding 
                (setf new-args
                      (cons (list (car arg) referent) new-args))
                (progn 
                  (format t "~%Warning: Unknown object ~a" (second arg))
                  (setf new-args (cons arg new-args)))))))
         (t ;; could be a description perhaps
            (format t "~%Warning: Description not implemented yet: ~a" (second arg))))))
    (multiple-value-bind
        (not-used added)
        (generate-facts type my-self new-args nil world-model time)
      (declare (ignore not-used))
      (values bindings (append added added-facts)))))

(defun generate-facts (concept-label self args bindings world-model &optional time)
  ; generates all the facts for a given concept and produces a set of bindings  + list of facts 
  ; concept-label = the name of the concept
  ; self = the object that is already known to be the referent of the concept
  ; args = set of arguments ((arg filler) .... )
  ; world-model = current world-model 
  (let* ; first get the definition 
      ((object-def ; are we dealing with an object? 
        (get-object concept-label)) ; use that as definition 
       (frame (if object-def object-def ; if not 
                (get-frame concept-label))) ; get the class definition
       (frame (if frame frame ; it can also be an event
                (get-event concept-label))) 
       (self (if self self ; is there already a known referent? use it
               (make-item concept-label 'individual world-model))) ; otherwise it is a random new name 
       (new-fact-content (list concept-label self))
;       (existing-fact (fact-present new-fact-content  (world-model-facts  world-model)))
       (added-facts nil)
       )
    (if object-def ; we are creating a new object so that is now stored
      (add-object concept-label self world-model))
    (cond
     ((null frame) ; treated as constant?
      (format t "~%Warning: Undefined frame ~a" concept-label)
      nil)
     ((fact-present new-fact-content  (world-model-facts  world-model)) ; is already there
      ; could be with another truth value take care of this later.
      ; need concrete examples first 
      (format t "~%Warning: Already existing ~a" new-fact-content)
      (values bindings (list (fact-present new-fact-content  (world-model-facts  world-model))))) ; clean this up

     (t
      (setf bindings (cons (cons '?self self) bindings))
      (setf added-facts
            (cons 
             (add-fact new-fact-content world-model time) ; now add the basic fact
             added-facts))
      ; sweep through all arguments to create new ones if necessary
      ; and collect the bindings of variables on the 
      (dolist (arg-spec (frame-arguments frame))
        (if arg-spec ; if there are arguments 
          (multiple-value-bind
              (not-used added)
              (handle-argument ; instantiate them in turn 
               self arg-spec args bindings world-model time)
            (declare (ignore not-used))
            (setf added-facts (append added added-facts)))))
      (dolist (evoke-spec (frame-definition frame))
        (if evoke-spec ; if there are evocations 
          (multiple-value-bind
              (not-used added)
              (handle-evoked
               self evoke-spec args bindings world-model time)
            (declare (ignore not-used))
            (setf added-facts (append added added-facts)))))
      (values bindings added-facts)
      ))))

;(untrace)
          
(defun add-binding (variable my-self bindings)
  (cond ((null bindings) (list (cons variable my-self)))
        ((assoc variable bindings) ; already there
         bindings)
        (t (cons (cons variable my-self) bindings))))

(defun resolve-bindings (args bindings)
  (cond ((null args) Nil)
        (t
         (let* ((var (find-variable (car args)))
                (binding (if var (cdr (assoc var bindings)))))
           (if binding
             (cons (list (caar args) binding)
                   (resolve-bindings (cdr args) bindings)))))))

(defun find-variable (arg-spec)
  (cond ((null arg-spec) nil)
        ((varp (car arg-spec)) (car arg-spec))
        (t (find-variable (cdr arg-spec)))))

(defun find-constraints (arg-spec)
  ;; looks out where the list of type specifications starts 
  (cond ((null arg-spec) nil)
        ((equal (car arg-spec) '<)
         (cdr arg-spec))
        (t 
         (find-constraints (cdr arg-spec)))))
        
(defun varp (symbol)
  (if (symbolp symbol)
    (string= "?" (char (symbol-name symbol) 0))))                

(defun append-if-not-there (list other-list)
  (cond ((null list) other-list)
        ((member (car list) other-list)
         (append-if-not-there (cdr list) other-list))
        (t (append-if-not-there (cdr list) (cons (car list) other-list)))))

(defun find-type-arg (specs)
  (cond ((null specs) nil)
        ((equal (car specs) '=) (second specs))
        (t (find-type-arg (cdr specs)))))

(defun fetch-constraints (frame type arg)
  (let*
      ((frame-def (get-frame frame)) ; does not matter whether type or frame
       (specs (if frame-def (frame-arguments frame-def)))
       (specs (if (varp (car specs)) (cdr specs) specs))
       (specific-arg (assoc arg specs))
       (own-constraints  (find-constraints specific-arg))
       (type-arg (find-type-arg specific-arg)))
    (if type
      (append-if-not-there 
       own-constraints
       (fetch-constraints type nil type-arg))
      own-constraints)))

;;; additions in 2012

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Creating templates from frames
;;;; templates are like instances except that they are filled with variables
;;;; and there are no new facts added to the world model
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro def-template (&body args)
  ; creates a template with variables for a given frame
  ; except for objects 
  `(do-def-template ',(car args) ',(cdr args) ',*world-model*))

(defun do-def-template (frame args world-model) 
  ; 1. check whether frame exists as object
  (if (and (get-object frame) ; the frame refers to an object
           (get-value frame (world-model-objects world-model)))
    (format t "~%!! Object ~a already exists as *-~a*" frame frame)
      (let
          ; 3. if not, create arguments to generate facts from
          ((filler-arguments
            (create-filler-arguments-template args world-model)))
        (generate-template frame nil filler-arguments nil world-model nil))))

(defun create-filler-arguments-template (args world-model)
  ; unclear why this is needed, could be same as create-filler-arguments
  (mapcar #'(lambda (arg) ; loop over given arg pairs and look up their definitions
              (let ((filler (get-value (second arg)
                                       (world-model-objects world-model))))
                (if filler ; is already bound 
                  (list (car arg) filler)
                  (let*
                      ((definition ;otherwise get definition of this object 
                        (get-object (second arg)))
                       (new-filler
                        (if definition ; and instantiate it
                          (get-value '?self ; instantiate returns a list of bindings 
                                     (do-def-template (second arg) nil world-model)))))
                    (if new-filler      
                      (list (car arg) new-filler)
                      (if (numberp (second arg)) ;; or it can be a number, e.g. time stamp 
                        (list (car arg) (make-symbol (format nil "time-point-~a" (second arg))))
                        (progn
                          (format t "~%!! Undefined object ignored: ~a as ~a in ~a"
                                  (second arg) (car arg) frame)
                          (list (car arg)))))))))
          args))

(defun generate-template (label self args bindings world-model template)
  (let*
    ((object-def ; are we dealing with an object? 
        (get-object label))
     (frame (if object-def object-def
              (get-frame label)))) ; otherwise take the definition of the frame 
    (cond
     ((null frame) 
      (format t "~%Undefined frame ~a" label) nil) ; no definition so terminate immediately
     (t
      (let*
          ((new-self (if (null self) (make-var label))) ; if self not given
           (self (if self self new-self)))
        (setf template (cons (list label self)
                             template))
        (setf bindings (cons (cons (frame-self-variable frame) self) bindings))
;        (format t "~%Generate template: ~a, bindings ~a" template bindings)
      ; sweep through all arguments to create new ones if necessary
        (dolist (arg-spec (frame-arguments frame))
          (multiple-value-bind (new-template new-bindings)
              (handle-argument-template
               self arg-spec args bindings world-model)
            (setf template (append new-template template))
            (setf bindings new-bindings)))
        (dolist (evoke-spec (frame-definition frame))
          (multiple-value-bind (new-template new-bindings) 
                    (handle-evoked-template self evoke-spec args bindings world-model)
;            (format t "~%evoked template: ~a, new-template: ~a" template new-template)
            (setf template (append template new-template))
            (setf bindings new-bindings)))
        (values template bindings))))))

(defun handle-argument-template (self arg-spec args bindings world-model)
  (declare (ignore world-model))
  (let*
      ((arg (car arg-spec))
       (constraints (find-constraints (cdr arg-spec)))
       (variable (find-variable (cdr arg-spec)))
       (filled-arg (assoc arg args))
       (item (if filled-arg ; there is already a binding 
               (second filled-arg)
               (make-var arg)))) ; else create variable at this point
    (format t "~%Filled: ~a ~a" ARG ITEM)
;    (if (null filled-arg) (add-object item world-model)) ; new object is anonymous 
    (let ((new-template (list (list arg self item))))
      (if variable
        (setf bindings ; add variable if not there yet 
              (cons (cons variable item) bindings)))
      (mapcar #'(lambda (constraint) ; add constraints about this item 
                  (setf new-template (cons (list constraint item) new-template)))
              constraints)
      (values new-template bindings))))

(defun handle-evoked-template (self evoke-spec args bindings world-model)
  (declare (ignore args self))
  ; example (a left-of ?r (source-left-of ?s))
  ; self is object being described by global frame
  ; my-self is when object is different from self
  (let*
      ((type (second evoke-spec))
       (variable (find-variable (cddr evoke-spec))) 
       (bound-variable (cdr (assoc variable bindings)))
       (my-self
        (if bound-variable bound-variable
          (make-var type)))
       (new-args nil))
;    (if (null bound-variable) (add-object type my-self))
;    (add-fact (list type my-self))
    (if variable 
      (setf bindings ; add variable if not there yet
            (add-binding variable my-self bindings)))
    (dolist (arg (cddr evoke-spec))
      (if (varp arg) ; ignore is the spec of the variable
        t 
        (let* ((filler (cdr (assoc (second arg) bindings))))
 ;         (format t "~%Look for filler ~a ~a ~a" (second arg) filler bindings)
          (if filler
            (setf new-args
                  (cons (list (car arg) filler) new-args))
            (let ((new-filler (make-item (car arg) 'individual world-model)))
              (setf bindings 
                    (add-binding (second arg) new-filler bindings))
              (setf new-args
                    (cons (list (car arg) new-filler) new-args)))))))
    (generate-template type my-self new-args bindings world-model nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Matching templates against a world model 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *trace-matcher* t) ; to see what is going on 

(defmacro do-compare (&body args) 
  `(find-bindings ,(car args)
                  (if ,(second args)
                    (world-model-facts ,(second args))
                    (world-model-facts *world-model*))
                  nil))

(defun find-bindings (template facts binding-hypotheses)
  ; goes through the world-model to find possible bindings
  (if *trace-matcher* (format t "~%+>> Template: ~a" template))
  (dolist (pattern template) ; go through all patterns in a template
    (if *trace-matcher* (format t "~%++ Pattern: ~a with ~a" pattern binding-hypotheses))
      (let ((match-first-pattern 
             (find-bindings-pattern pattern facts binding-hypotheses)))
        (if match-first-pattern ; there are new bindings
          (setf binding-hypotheses (append-new match-first-pattern binding-hypotheses))
          (return nil)) ; no matches found
             ))
    binding-hypotheses)

(defun append-new (list-of-elements target-set)
  ; add only if not there yet
  (cond ((null list-of-elements) target-set)
        ((already-present (car list-of-elements) target-set)
         (append-new (cdr list-of-elements) target-set))
        (t (cons (car list-of-elements)
                 (append-new (cdr list-of-elements) target-set)))))

(defun already-present (list-of-bindings target-set)
  (if (null target-set) nil ; none of them was equal
    (let ((one-target (car target-set)))
      (if (null (equal (length list-of-bindings) (length one-target)))
      ; they are not equal so check the next one 
        (already-present list-of-bindings (cdr target-set))
        (if (deep-already-present list-of-bindings (car target-set)) t ; they are equal
          (already-present list-of-bindings (cdr target-set)))))))

(defun deep-already-present (list-of-bindings target-bindings)
  ; they are assumed to be of equal length 
  (cond
   ((null list-of-bindings) t) ; we checked all so they are equal 
   ((equal (cdr (car list-of-bindings)) ; look whether binding is the same 
           (cdr (assoc (caar list-of-bindings) target-bindings)))
    (deep-already-present (cdr list-of-bindings) target-bindings))
   (t nil))) ; either binding not there or was a different binding 

(defun find-bindings-pattern (pattern facts hypotheses)
  ; pattern is a fact with variables in a template
  ; facts is the set of possible facts
  ; hypotheses is a list of possible binding-lists (((<var> . <value>) ...) .... ((<var> . <value>)))
  (let ((new-hypotheses nil))
    (dolist (fact-structure facts)
      (let ((fact (fact-content fact-structure)))
    ; go through these facts
        (if *trace-matcher* (format t "~%+++ Fact: ~a" fact))
      (cond
         ((equal (car pattern)  (car fact)) ; pattern and fact have the same predicate
           ; now test all the arguments
           (if (null (equal (length (cdr pattern)) (length (cdr fact))))
             (progn (format t "~%Warning: unequal length of ~a and ~a" args-pattern args-fact)
               nil)) ; ignore this fact 
           (let 
               ((outcome (find-bindings-arguments (cdr pattern) (cdr fact) hypotheses)))
             (if *trace-matcher* (format t "~%+++ outcome: ~a" outcome))
             (if (equal outcome 'fail) ; means this fact did not match
               nil ; but another one could
               ; otherwise there is a match and this is a valid hypothesis
               (setf new-hypotheses (append outcome new-hypotheses)))))
         (t nil)) ; this fact did not match
    ))
    (if *trace-matcher* (format t "~%+++ new-hypotheses: ~a" new-hypotheses))
  new-hypotheses))

; little test for this 
;(setf ?that (make-var 'that))
;(setf test (list (list (cons ?this 'bla))))
;(setf ?now (make-var 'now))
;(find-bindings  (list (List 'pred ?this ?that) (list 'what ?now ))  (list '(pred this that) '(pred these tho) '(what now)) nil)

(defun find-bindings-arguments (args-pattern args-fact hypotheses)
  ; returns 'fail if there was a mismatch
  ; otherwise the set of possible hypotheses so far
  (do ((args-pattern args-pattern (cdr args-pattern)) ; arguments of pattern
       (args-fact args-fact (cdr args-fact)) ; arguments of fact
       (result nil))
      ((null args-pattern) result)
      (let ((arg-pattern (car args-pattern))
            (arg-fact (car args-fact)))
        (if (var-p arg-pattern) ; arg is a variable
          (if (null hypotheses) ; no binding list yet so create a new binding list 
            (setf hypotheses (list (list (cons arg-pattern arg-fact))))
            ; otherwise compute new hypothesis list 
            (let ((new-bindings (transform-bindings arg-pattern arg-fact hypotheses)))
              (if new-bindings ; there is a result 
                (setf hypotheses new-bindings) ; new set of bindings
                (return (setf result 'fail))))) ; otherwise failure for this fact
          ; arg-pattern is not a variable, so it must be an individual objecdt in which case a direct match is needed
          (if (equal arg-pattern arg-fact)
            hypotheses ; so we keep going
            (return (setf result 'fail)))))
      (setf result hypotheses)))

(defun transform-bindings (arg-pattern arg-fact hypotheses)
  (let ((new-hypotheses nil))
  (dolist (hypothesis hypotheses)
    (let ((existing-binding (cdr (assoc arg-pattern hypothesis))))
      (cond (existing-binding 
              (if (equal existing-binding arg-fact) ; it was already in the hypothesis list and is equal
                (setf new-hypotheses (cons hypothesis new-hypotheses))
              ; it was there but not equal then this hypothesis is no longer viable
                nil)) ; hypothesis is not retained
            (t ; there was no binding so we can add it
             (setf new-hypotheses (cons (cons (cons arg-pattern arg-fact)
                                              hypothesis)
                                        new-hypotheses))))))
  new-hypotheses))


  
  
  
