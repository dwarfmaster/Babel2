
(in-package :fcg)

;; (activate-monitor trace-fcg)


;;;; Case 1: Exact matches
;;;; -------------------------------------
;;;; FCG> (unify "sing" "singing")
;;;; NIL
;;;; FCG> (unify "sing" "sing")
;;;; ((T . T))
;;;;
;;;; Case 2: Partial matches
;;;; --------------------------------------
;;;; FCG> (unify "sing.+" "singing")
;;;; (((#:?G95058-1 "sing.+" "singing")))
;;;; FCG> (unify "sing.+" "ringing")
;;;; NIL
;;;; FCG> (unify "sing.+" "sing")
;;;; NIL
;;;; FCG> (unify "sing.*" "sing")
;;;; (((#:?G95076-1 "sing.*" "sing")))
;;;; FCG> (unify "sing.*" "sings")
;;;; (((#:?G95077-1 "sing.*" "sing")))

(defun test-regular-expressions ()

  ;; Case-1: Exact match:
  ;; ------------------------------------------------------------------------------
  (cond
   ((not (equal (produce '((test ref)) *constructions*) '("test")))
    (error "Exact string match fails in production."))
   ((not (unify '((test ref)) (parse '("test") *constructions*)))
    (error "Exact string match fails in parsing."))

   ;; Case-2: Partial match:
   ;; ------------------------------------------------------------------------------
   ((not (equal (produce '((sing ev-1)) *constructions*) '("sing")))
    (error "Partial match fails in production."))
   ((not (unify '((sing ev-1)) (parse '("sing") *constructions*)))
    (error "Partial match fails in parsing."))))

;; (test-regular-expressions)
