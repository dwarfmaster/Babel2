
(in-package :common-lisp)

#-:hunchentoot-available-on-this-platform
(error "Hunchentoot is not available on this platform. The web-interface does not work.")

(defpackage #:web-interface
  (:nicknames #:wi)
  (:use :common-lisp :cl-user :hunchentoot :ht-simple-ajax :utils)
  (:documentation "A package for the web interface"))
