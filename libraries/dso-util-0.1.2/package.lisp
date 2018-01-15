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

(defpackage #:dso-util
  (:use #:cl #:cl-ppcre)
  (:export #:parse-iso-date
           #:format-iso-date
           #:positive
           #:negative
           #:range
           #:subvect
           #:zip-alist
           #:zip-glist
           #:zip-plist
           #:with-gensyms
           #:compose
           #:peval
           #:inflist
           #:bound
           #:boundf
           #:nothing
           #:true-fn
           #:nil-fn
           #:gulp-input
           #:string+
           #:string-initcase
           #:only-if))
