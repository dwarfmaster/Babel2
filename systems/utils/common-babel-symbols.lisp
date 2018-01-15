;;;;
;;;; File: common-babel-symbols.lisp
;;;;
;;;; This file is there to avoid package conflicts for often used symbols 
;;;; by exporting them from the :utils package
;;;;

(in-package :utils)

(export 
 '(topic utterance meaning partial-meaning sensory-context 
   name robot box region robot-p box-p region-p a b 
   x y orientation width height
   image-schema prototype
   value
   score
   status
   cyclic
   top-node
   label))

