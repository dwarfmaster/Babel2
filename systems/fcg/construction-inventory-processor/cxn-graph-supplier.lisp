(in-package :fcg)

;; #########################################################
;; can-cause
;; ---------------------------------------------------------

(defun variable? (a)
  (documentation "Test if a is a symbol starting by ?")
  (if (symbolp a)
      (char= (char (symbol-name a) 0) #\?)
      nil
      )
  )
    
(defun eq-elem (a b)
  (documentation "Heuristic that returns t if a and b can be unified")
  (cond ((and (listp a) (listp b))     (eq-list a b)                            )
        ((variable? a)                 t                                        )
        ((variable? b)                 t                                        )
        ((and (symbolp a) (symbolp b)) (string= (symbol-name a) (symbol-name b)))
        ( t                            nil                                      )
  ))

(defun eq-list (a b)
  (documentation "Heuristic that returns t if a and b can be unified, where a and b are lists")
  (cond ((and a b)               (and (eq-elem (car a) (car b))
                                      (eq-elem (cdr a) (cdr b))))
        ((and (null a) (null b)) t                              )
        ( t                      nil                            )
        )
  )

(defun imply? (a b)
  (documentation "Heuristic that returns t if an element of a can be unified with an element of b")
  (consp (some (lambda (x) (member x b :test #'eq-elem)) a))
  )

(defun get-unit-name (a)
  (documentation "Returns the name of a unit")
  (if (listp (car a)) (car (cdr (car a))) (car a))
  )

(defun unitJ? (a)
  (documentation "Tests if a is a J-unit")
  (if (listp (car a)) (eql (car (car a)) 'J) nil)
  )

(defun unitRoot? (a)
  (documentation "Tests if a is a ROOT unit")
  (if (listp a) (eql (car a) 'ROOT) nil)
  )

(defun feature-can-cause (a b)
  (documentation "Heuristic that returns t if merging the feature a allows the feature b to match")
  (if (and (listp a) (listp b) (not (unitRoot? a)) (not (unitRoot? b)))
      (let ((aname (symbol-name (car a)))
            (bname (symbol-name (car b)))
            (aoper (car (cdr a)))
            (boper (car (cdr b))))
        (cond
          ((not (string= aname bname)) nil)
          ((not (eql aoper boper))     t)
          ((eql aoper '==)             (imply? (cdr (cdr a)) (cdr (cdr b))))
          ((eql aoper '==p)            (imply? (cdr (cdr a)) (cdr (cdr b))))
          ((eql aoper '==1)            (imply? (cdr (cdr a)) (cdr (cdr b))))
          (t                           t)
          )
        )
      nil
      )
  )

(defun unit-can-cause (a b)
  (documentation "Heuristic that returns t if merging the unit a allows the unit b to match")
  (if (unitJ? b)
        nil
        (if (not (eq-elem (get-unit-name a) (get-unit-name b))) nil
            (some (lambda (x) (some (lambda (y) (feature-can-cause x y)) b)) a)
            )
        )
  )

(defun feature-structure-can-cause (a b)
  (documentation "Heuristic that returns t if merging the feature structure a allows the feature structure b to match")
  (some (lambda (x) (some (lambda (y) (unit-can-cause x y)) b)) a)
  )

(defun can-cause (a b dir)
  (documentation "Heuristic that returns t if merging the construction a allows the match pole (according to dir) of the construction b to match")
  (or (feature-structure-can-cause (left-pole-structure a)
                                   (pole-structure (match-pole b dir)))
      (feature-structure-can-cause (right-pole-structure a)
                                   (pole-structure (match-pole b dir)))
      )
  )



