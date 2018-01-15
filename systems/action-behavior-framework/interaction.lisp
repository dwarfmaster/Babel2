
(in-package :action-behavior-framework)

(export '(begin-interaction))

;; ----------------------------------------------------------------------------
;; begin-interaction

(export '(begin-interaction))

(defgeneric begin-interaction (thing &key)
  (:documentation "initializes thing for interaction"))

(defmethod begin-interaction (thing &key &allow-other-keys)
  (declare (ignore thing)))

;; ----------------------------------------------------------------------------
;; finish-interaction

(export '(finish-interaction))

(defgeneric finish-interaction (thing &key)
  (:documentation "finish thing for interaction"))

(defmethod finish-interaction (thing &key &allow-other-keys)
  (declare (ignore thing)))

