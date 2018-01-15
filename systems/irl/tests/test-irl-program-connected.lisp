
(in-package :irl)

(defmacro testt-irl-program-connected? (irl-program connected number-of-classes)
  `(multiple-value-bind (ret-connected ret-number-of-classes)
       (irl-program-connected? ,irl-program)
     (test-assert (and (eq ,connected ret-connected)
                       (eq ,number-of-classes ret-number-of-classes))
                  "connected ~a, expected ~a and returned ~a classes, expected ~a"
                  ret-connected ,connected
                  ret-number-of-classes ,number-of-classes)))

(deftest test-irl-program-connected? ()
  (testt-irl-program-connected? '() t 0)
  (testt-irl-program-connected? '((bind t ?t t)) t 1)
  (testt-irl-program-connected? '((bind t ?t t)
                                  (bind t ?t1 t)) nil 2)
  (testt-irl-program-connected? '((bind t ?t t)
                                  (test ?t ?t1)
                                  (bind t ?t1 t)) t 1)
  (testt-irl-program-connected? '((bind t ?t t)
                                  (test ?t ?t1)
                                  (bind t ?t1 t)
                                  (test ?t1 ?t2)) t 1)
  (testt-irl-program-connected? '((bind t ?t t)
                                  (test ?t5 ?t3)
                                  (bind t ?t1 t)
                                  (test ?t1 ?t4)) nil 3))

;; (test-irl-program-connected?)
  