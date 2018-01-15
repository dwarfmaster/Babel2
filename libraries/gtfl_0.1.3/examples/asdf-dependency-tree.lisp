;;;;; Copyright (c) 2009-2012, Martin Loetzsch
;;;;; All rights reserved.

;;;;; Redistribution and use in source and binary forms, with or
;;;;; without modification, are permitted provided that the following
;;;;; conditions are met:

;;;;;  Redistributions of source code must retain the above copyright
;;;;;  notice, this list of conditions and the following disclaimer.

;;;;;  Redistributions in binary form must reproduce the above
;;;;;  copyright notice, this list of conditions and the following
;;;;;  disclaimer in the documentation and/or other materials provided
;;;;;  with the distribution.

;;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
;;;;; CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
;;;;; INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
;;;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;;;;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
;;;;; CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
;;;;; USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
;;;;; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
;;;;; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
;;;;; IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
;;;;; THE POSSIBILITY OF SUCH DAMAGE.


;; this is an example that uses both gtfl's tree drawing and expandable
;; elements functionality. It draws a tree of a system's asdf dependencies.
;; click on any system nodes to expand/collapse them or on "expand-all" 
;; to expand/collapse them all.

(asdf:operate 'asdf:load-op :gtfl)

(in-package :gtfl)

(define-css 'asdf "
div.asdf { margin-top:5px;margin-bottom:5px; }
div.asdf-duplicate { padding:3px;font-style:italic;}
div.asdf div.title { background-color:#000; padding:5px; padding-top:3px; padding-bottom:2px; }
div.asdf div.title > a { color:#fff;font-weight:bold; }
table.asdf-system { border-collapse:collapse;border:1px solid black; width:100%;}
table.asdf-system > tbody > tr > td { vertical-align:top; padding:4px;padding-bottom:2px; padding-top:2px; }
")

(defparameter *systems-already-drawn* nil
  "since asdf dependencies are not really a tree, we remember which
   systems have been drawn already and don's show them a second time.")

(defmacro system-slot (slot)
  "retuns the slot-value or nil when slot is unbound"
  `(when (slot-boundp system ,slot) (slot-value system ,slot)))

(defun draw-system-details (system)
  (who
   (:table 
    :class "asdf-system"
    (:tbody
     (let ((slots '(asdf::description asdf::long-description asdf::author
                    asdf::maintainer asdf::licence asdf::version)))
       (if (loop for slot in slots never (system-slot slot))
           (htm (:tr (:td (:i "no details available"))))
           (loop for slot in slots
              when (system-slot slot)
              do (htm 
                  (:tr 
                   (:td (format t "~(~a~):" slot))
                   (:td (esc (system-slot slot))))))))))))

(defun draw-asdf-dependency-tree* (system expand/collapse-all-id)
  "a helper function for recursively drawing asdf dependencies"
  (if (find system *systems-already-drawn*)
      (who (:div :class "asdf-duplicate" 
                 (format t "~(~a~)" (asdf:component-name system))))
      (progn
        (push system *systems-already-drawn*)
        (draw-node-with-children 
         (who-lambda 
          (let ((element-id (make-id-string "asdf")))
            (htm 
             (:div 
              :class "asdf"
              (make-expandable/collapsable-element
               element-id expand/collapse-all-id
               (who2s 
                (:div 
                 :class "title"
                 (make-expand/collapse-link 
                  element-id t "show details" 
                  (princ (asdf:component-name system)))))
               (who-lambda 
                (:div 
                 :class "title"
                 (make-expand/collapse-link 
                  element-id nil "hide details" 
                  (princ (asdf:component-name system))))
                (draw-system-details system)))))))
         (mapcar 
          #'(lambda (system-name)
              (who-lambda (draw-asdf-dependency-tree*
                           (asdf:find-system system-name)
                           expand/collapse-all-id)))
          (reverse 
           (when (slot-boundp system 'asdf::in-order-to)
             (cdadr (assoc 'asdf::load-op 
                           (slot-value system 'asdf::in-order-to))))))
         :color "black"))))

(defun draw-asdf-dependency-tree (system-name)
  (let ((id (make-id-string "asdf"))
        (*systems-already-drawn* nil))
    (who
     (:h2 "dependencies for asdf system " (princ system-name))
     (:div (make-expandable/collapsable-element 
            (make-id-string) id
            (who2s (make-expand/collapse-all-link id t nil  "expand all"))
            (who2s (make-expand/collapse-all-link id nil nil "collapse all"))))
     (:div (draw-asdf-dependency-tree* (asdf:find-system system-name) id)))))

;;(start-gtfl)

;;(gtfl-out (draw-asdf-dependency-tree :gtfl))

