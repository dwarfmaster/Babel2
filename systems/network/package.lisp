;;; defines the network package

(in-package #:common-lisp-user)

(defpackage :network
  (:use #:common-lisp #:test-framework #:utils #:monitors 
   #+:hunchentoot-available-on-this-platform #:web-interface)
  (:documentation "general support for the datastructure network.")
  (:import-from #:cl-user))