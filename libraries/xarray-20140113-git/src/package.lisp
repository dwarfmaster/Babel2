(in-package #:xarray-asd)

(defpackage #:xarray
  (:use :common-lisp :iterate :bind :cl-utilities :anaphora)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export

   ;; utilities
   
   rm-index rm-subscripts cm-index cm-subscripts
   
   ;; interface

   xelttype #| xtype |#
   xrank xdims xdim xsize 

   xref-subscript-out-of-bounds
   xref-wrong-number-of-subscripts 
   xref-setting-readonly
   xref-incompatible-type
   xdim-invalid-axis-number

   xsetf xref 
   xsimilar xcreate xcreate-similar as* as copy-as

   ;; array

   cvector carray cvector* carray*

   ;; view

   xview original-ancestor
   permutation permutation-xview
   xslice xslice-xview drop column-major-projection
   column-major-projection-view column-major-projection-flat-view
   flat flat-xview
   #|transpose|#

   ;; operations
   
   xdim= x+ x- x* x/ x= xorder xsort xsum xprod xmin xmax xmean xmap
   xop xcollect xdot xconcat

   ;; sequences

   int-seq

   ;; atoms -- nothing needs to be exported
   
   ))
