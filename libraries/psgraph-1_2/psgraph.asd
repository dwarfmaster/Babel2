;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(in-package #:cl-user)
(defpackage #:psgraph-system (:use #:asdf #:cl))
(in-package #:psgraph-system)

(defsystem psgraph
  :name "psgraph"
  :author "Joseph Bates, Carnegie Mellon University"
  :version ""
  :maintainer ""
  :licence "Public Domain"
  :description "PostScript DAG Grapher."
  :long-description "The PSGrapher is a set of Lisp routines that can
be called to produce PostScript commands that display a directed
acyclic graph."
  :components ((:file "psgraph")))
