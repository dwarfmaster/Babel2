
(in-package :meta-layer-learning)

(defparameter *web-colors*
 `((diagnostic . "#212121")
   (repair . "#212121")
   (fix . "#4CAF50")
   (duplicate-fix . "#800080")
   (fix-w-problems . "#6495ED")
   (problem-w/o-fixes . "#FF5252")
   (problem-w-fixes . "#388E3C")))

;; ##################################################################
;; table rows for object with learning
;; ##################################################################

(export '(make-table-rows-for-object-w-learning))

(defun make-table-rows-for-object-w-learning (o)
  `(,@(when (not (null (diagnostics o)))
        `(((tr)
           ((td) "diagnostics ")
           ((td)
            ,@(loop for d in (diagnostics o)
                 for label = (or 
                              (and (slot-exists-p d 'id)
                                      (slot-value d 'id))
                              (and (slot-exists-p d 'label)
                                      (slot-value d 'label))
                              (type-of d))
                    for type = (type-of d)
                    for i from 1
                    append `(((span :style "white-space:nowrap")
                              ((div :style "display:inline-block")
                               ,(html-pprint label))
                              ,(format nil " (~(~a~))" type)
                              ,(if (< i (length
                                         (diagnostics o)))
                                 "," ""))
                             " "))))))
    ,@(when (not (null (repairs o)))
        `(((tr)
           ((td) "repairs ")
           ((td)
            ,@(loop
               for r in (repairs o)
               for label = (or 
                              (and (slot-exists-p r 'id)
                                      (slot-value r 'id))
                              (and (slot-exists-p r 'label)
                                      (slot-value r 'label))
                              (type-of r))
               for type = (type-of r)
               for i from 1
               append `(((span :style "white-space:nowrap")
                         ((div :style "display:inline-block")
                          ,(html-pprint label))
                         ,(format nil " (~(~a~))" type)
                         ,(if (< i (length (repairs o)))
                            "," ""))
                        " "))))))
    ,@(when (not (null (problems o)))
        `(((tr)
           ((td) "problems ")
           ((td)
            ((p) ,@(html-hide-rest-of-long-list
                    (problems o) 5 #'(lambda (p) (make-html p))))))))))

;; ##################################################################
;; make-html problem
;; ##################################################################

(defmethod html-color ((p problem))
  (if (fixes p)
    "#62e200" "#ff4c00"))

(define-css 'problem "
 div.problem { display:inline-block;text-align:left;margin-right:5px;margin-top:5px;margin-bottom:5px;}
 div.problem div.problem-title { padding-left:5px; padding-right:5px; padding-top:3px; padding-bottom:3px; }
 div.problem div.problem-title a { color:white; }
 ")

(defmethod make-html ((p problem) &key expand-initially)
  (let ((element-id (make-id 'tp))
        (link-title `((tt) ,(format nil "~(~a~)" 
                                    (or 
                                     (and (slot-exists-p p 'id)
                                          (slot-value p 'id))
                                     (and (slot-exists-p p 'label)
                                          (slot-value p 'label))
                                     (type-of p))))))
    `((div :class "problem"
           :style ,(format nil "border:1px solid ~a;" (if (fixes p)
                                                        (assqv 'problem-w-fixes *web-colors*)
                                                        (assqv 'problem-w/o-fixes *web-colors*))))
      ,(make-expandable/collapsable-element
 	element-id (make-id)
        `((div :class "problem-title"
               :style ,(format nil "background-color:~a;" (if (fixes p)
                                                           (assqv 'problem-w-fixes *web-colors*)
                                                           (assqv 'problem-w/o-fixes *web-colors*))))
 	  ((a ,@(make-expand/collapse-link-parameters element-id t))
           ,link-title))
        `((span)
          ((div :class "problem-title"
                :style ,(format nil "background-color:~a;" (if (fixes p)
                                                             (assqv 'problem-w-fixes *web-colors*)
                                                             (assqv 'problem-w/o-fixes *web-colors*))))
           ((a ,@(make-expand/collapse-link-parameters element-id nil))
            ,link-title))
          ((table :class "two-col")
           ((tbody)
            ((tr)
             ((td) "issued-by:")
             ((td) ,(let ((issuer (issued-by p)))
                      (cond 
                       ((symbolp issuer)
                        (downcase (symbol-name issuer)))
                       ((slot-exists-p issuer 'id)
                        (downcase (symbol-name (id issuer))))
                       ((slot-exists-p issuer 'label)
                        (downcase (symbol-name (slot-value issuer 'label))))
                       (t (make-html issuer))))))
            ,@(when (fixes p)
                `(((tr)
                   ((td) "fixes:")
                   ((td) 
                    ((p) ,@(loop for f in (fixes p)
                              for r = (and (slot-exists-p f 'issued-by)
                                           (slot-value f 'issued-by))
                              for i from 0
                              if (symbolp f)
                              collect (downcase (symbol-name r))
                              else if r
                              collect 
                                (downcase
                                 (symbol-name (or
                                               (and (slot-exists-p r 'id)
                                                    (slot-value r 'id))
                                               (type-of r))))
                              else collect (make-html r) 
                              collect " ")))))))))
        :expand-initially expand-initially))))

;; ##################################################################
;; make-html repair
;; ##################################################################

(define-css 'repair "
 div.repair { display:inline-block;text-align:left;margin-right:5px;margin-top:5px;margin-bottom:5px;}
 div.repair div.repair-title { padding-left:5px; padding-right:5px; padding-top:3px; padding-bottom:3px;}
 div.repair div.repair-title a { color:white; }
 div.repair td.data-field { padding:0px;padding-right:4px;padding-left:2px;vertical-align:top;border-spacing:0px}
 div.repair td.data { padding:0px;vertical-align:center;border-spacing:0px }
 ")

(defmethod make-html ((r repair) &key expand-initially)
  (let ((element-id (make-id 'repair))
        (link-title `((tt) ,(format nil "~(~a~) ~@[(~a)~]"
                                    (or 
                                     (and (slot-exists-p r 'id)
                                          (slot-value r 'id))
                                     (and (slot-exists-p r 'label)
                                          (slot-value r 'label))
                                     (type-of r))
                                    (score r)))))
    `((div :class "repair"
           :style ,(format nil "border:1px solid ~a;" (assqv 'repair *web-colors*)))
      ,(make-expandable/collapsable-element
 	element-id (make-id)
        `((div :class "repair-title"
               :style ,(format nil "background-color:~a;" (assqv 'repair *web-colors*)))
 	  ((a  ,@(make-expand/collapse-link-parameters element-id t))
           ,link-title))
        `((span)
          ((div :class "repair-title"
                :style ,(format nil "background-color:~a;" (assqv 'repair *web-colors*)))
           ((a ,@(make-expand/collapse-link-parameters element-id nil))
            ,link-title))
          ((table :class "two-col")
           ((tbody)
            ((tr)
             ((td) "trigger:")
             ((td) ,(make-html (trigger r))))
             ,@(when (data r)
                 `(((tr) ((td :class "data-field") "data:")
                    ((td :class "data")
                     ((td :class "data-field")
                      ,(call-next-method)))))))))
        :expand-initially expand-initially))))

;; ##################################################################
;; make-html diagnostic
;; ##################################################################

(define-css 'diagnostic "
 div.diagnostic { display:inline-block;text-align:left;margin-right:5px;margin-top:5px;margin-bottom:5px;}
 div.diagnostic div.diagnostic-title { padding-left:5px; padding-right:5px; padding-top:3px; padding-bottom:3px;}
 div.diagnostic div.diagnostic-title a { color:white; }
 div.diagnostic td.data-field { padding:0px;padding-right:4px;padding-left:2px;vertical-align:top;border-spacing:0px}
 div.diagnostic td.data { padding:0px;vertical-align:center;border-spacing:0px }
 ")

(defmethod make-html ((d diagnostic) &key expand-initially)
  (let ((element-id (make-id 'diag))
        (link-title `((tt) ,(format nil "~(~a~)" 
                                    (or 
                                     (and (slot-exists-p d 'id)
                                          (slot-value d 'id))
                                     (and (slot-exists-p d 'label)
                                          (slot-value d 'label))
                                     (type-of d))))))
    `((div :class "diagnostic" :style ,(format nil "border:1px solid ~a;" (assqv 'diagnostic *web-colors*)))
      ,(make-expandable/collapsable-element
 	element-id (make-id)
        `((div :class "diagnostic-title"
               :style ,(format nil "background-color:~a;" (assqv 'diagnostic *web-colors*)))
 	  ((a ,@(make-expand/collapse-link-parameters element-id t))
           ,link-title))
        `((span)
          ((div :class "diagnostic-title"
                :style ,(format nil "background-color:~a;" (assqv 'diagnostic *web-colors*)))
           ((a :style "color:white;"
               ,@(make-expand/collapse-link-parameters element-id nil))
            ,link-title))
          ((table :class "two-col")
           ((tbody)

            ((tr)
             ((td) "trigger:")
             ((td) ,(let ((trigger (trigger d)))
                      (cond 
                       ((symbolp trigger)
                        (downcase (symbol-name trigger)))
                       (t (html-pprint trigger)))))))))
        :expand-initially expand-initially))))

;; ##################################################################
;; make-html fix
;; ##################################################################

(define-css 'fix "
 div.fix { display:inline-block;text-align:left; margin-right:5px; margin-top:5px; margin-bottom:5px; }
 div.fix div.fix-title { padding-left:5px; padding-right:5px; padding-top:3px; padding-bottom:3px;}
 div.fix div.fix-title a { color:white; }
 div.fix > table.two-col {border-spacing:0px}
 div.fix td.data-field { padding:0px;padding-right:4px;padding-left:2px;vertical-align:top;border-spacing:0px}
 div.fix td.data { padding:0px;vertical-align:center;border-spacing:0px }
 ")

(defmethod make-html ((fix fix)
                      &key expand-initially)
  (let ((element-id (make-id 'fix))
        (link-title `((tt) ,(format nil "~(~a~)" 
                                    (or
                                     (and (slot-exists-p fix 'id)
                                          (slot-value fix 'id))
                                     (and (slot-exists-p fix 'label)
                                          (slot-value fix 'label))
                                     (type-of fix)))))
        (expand-collapse-all-id (make-id)))
    `((div :class "fix" :style ,(format nil "border:1px solid ~a;" (assqv 'fix *web-colors*)))
      ,(make-expandable/collapsable-element
 	element-id expand-collapse-all-id
        `((div :class "fix-title"
               :style ,(format nil "background-color:~a;" (assqv 'fix *web-colors*)))
 	  ((a ,@(make-expand/collapse-link-parameters element-id t))
           ,link-title))
        `((span)
          ((div :class "fix-title"
                :style ,(format nil "background-color:~a;" (assqv 'fix *web-colors*)))
           ((a ,@(make-expand/collapse-link-parameters element-id nil))
            ,link-title))
          ((table :class "two-col")
           ((tr) ((td :class "data-field") "issued-by:")
            ((td :class "data")
             ,(make-html (issued-by fix))))
           ((tr) ((td :class "data-field") "problem:")
            ((td :class "data")
             ((td :class "data-field")
              ,(make-html (problem fix)))))
           ,@(when (restart-data fix)
               `(((tr) ((td :class "data-field") "restart-data:")
                  ((td :class "data")
                   ((td :class "data-field")
                    ,(make-html (restart-data fix)))))))
           ,@(when (data fix)
               `(((tr) ((td :class "data-field") "data:")
                  ((td :class "data")
                   ((td :class "data-field")
                    ,(call-next-method))))))))
        :expand-initially expand-initially))))
