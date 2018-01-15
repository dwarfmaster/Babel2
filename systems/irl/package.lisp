
(in-package :cl-user)

(defpackage :irl
  (:documentation "Incremental Recruitment Language (IRL)")
  (:use :common-lisp :test-framework :utils :monitors
        #+:hunchentoot-available-on-this-platform :web-interface)
  (:shadow :context))


