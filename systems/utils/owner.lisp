
(in-package :utils)

(defgeneric owner (object))

(defmethod owner (object)
  (declare (ignore object))
  nil)
