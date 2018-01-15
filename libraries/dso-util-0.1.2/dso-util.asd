;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp -*-

#|
Copyright (C) 2007  David Owen <dsowen@fugue88.ws>

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
|#



(asdf:defsystem #:dso-util
  :description "David Owen's utilities."
  :version "0.1"
  :author "David Owen"
  :licence "LGPL-2.1"
  :depends-on (#:cl-ppcre)
  :components ((:file "file" :depends-on ("package"))
               (:file "impl" :depends-on ("package"))
               (:file "package")
               (:file "time" :depends-on ("package"))))
