
(in-package :robot-interface)

(export '(head-touch-front head-touch-middle head-touch-rear head-yes-no))

(defmethod head-touch-front ((robot robot) &key)
  (detect-head-touch robot "Front"))

(defmethod head-touch-middle ((robot robot) &key)
  (detect-head-touch robot "Middle"))

(defmethod head-touch-rear ((robot robot) &key)
  (detect-head-touch robot "Rear"))

(defmethod head-yes-no ((robot robot) &key)
  (head-yes-or-no robot))