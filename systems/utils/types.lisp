
(in-package :utils)

(export '(subtypep2 typep2))

(defun subtypep2 (sup sub &optional environment)
  (subtypep sub sup environment))

(defun typep2 (type object &optional environment)
  (subtypep type object environment))