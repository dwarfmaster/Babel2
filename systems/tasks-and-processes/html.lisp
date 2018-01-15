
(in-package :tasks-and-processes)

;; ##################################################################
;; make table and table rows for blackboard and results
;; ##################################################################

(define-css 'process-results "
div.process-result { border-top:0px dashed #ccc;border-spacing:0px}
div.process-result > div { padding:0px;border-spacing:0px}
process-result > table.two-col {border-spacing:0px}
process-result td.data-field { padding:0px;padding-right:4px;vertical-align:top;border-spacing:0px}
process-result td.data { padding:0px;vertical-align:top;border-spacing:0px }
")

(defun make-table-rows-for-blackboard (bb
                                       &key expand/collapse-all-id)
  (loop
   for field in (fields bb)
   for value = (get-data bb field)
   collect
   `((tr)
     ((td :class "data-field")
      ,(format nil "~(~a~):" field))
     ((td :class "data")
      ,(if (listp value)
         (html-pprint value)
         (make-html value :expand/collapse-all-id expand/collapse-all-id))))))

(defun make-table-for-blackboard (bb)
  `((table :class "two-col")
    ,@(make-table-rows-for-blackboard bb)))

(defun make-table-rows-for-results (results)
  (loop
   with expand/collapse-all-id = (make-id 'results)
   for result in results
   for i from 1
   when (= 4 i)
   collect
   `((tr :name ,(downcase (mkstr expand/collapse-all-id))
         :id ,(downcase (make-id 'result))
         :style "display:table-row")
     ((td)
      ((a ,@(make-toggle-display-style-all-link-parameters
             expand/collapse-all-id "none" "table-row"
             (mkstr "results " i "-" (length results))))
       ,(mkstr "results " i "-" (length results))))
     ((td)))
   if (> 4 i )
   collect `((tr)
             ((td) ,(format nil "result ~a (~,2f)" i (score result)))
             ((td) ,(make-table-for-blackboard (data result))))
   else
   collect
   `((tr :name ,(downcase (mkstr expand/collapse-all-id))
         :id ,(downcase (make-id 'result))
         :style "display:none")
     ((td) ,(format nil "result ~a (~,2f)" i (score result)))
     ((td) ,(make-table-for-blackboard (data result))))))

(defun make-html-for-results (results)
  `((div :class "process-results")
    ,(if results
       `((table :class "two-col")
         ,@(make-table-rows-for-results results))
       `(((p"no results"))))))



;; ##################################################################
;; process-result (for visualization inside a task see above)
;; ##################################################################

;; process-result
(define-css 'process-result "
div.process-result { display:inline-block;text-align:left;margin-right:5px;margin-top:5px;margin-bottom:5px;}
div.process-result div.title { padding-left:5px; padding-right:5px; padding-top:3px; padding-bottom:3px; }
div.process-result div.title a { color:white; }
")

(defmethod make-html ((p process-result) &key result-nr expand-initially)
  (let ((element-id (make-id 'pr))
        (link-title `((tt) ,(format nil "~(result~@[~a~] (~,2f)~)"
                                    result-nr (score p)))))
    `((div :class "process-result"
           :style "border:1px solid black;")
      ,(make-expandable/collapsable-element
	element-id (make-id)
        `((div :class "title"
               :style "background-color:black;")
	  ((a :style "color:white;" ,@(make-expand/collapse-link-parameters element-id t))
           ,link-title))
        `((span)
          ((div :class "title"
                :style "background-color:black;")
           ((a :style "color:white;"
               ,@(make-expand/collapse-link-parameters element-id nil))
            ,link-title))
          ((table :class "two-col")
           ((tbody)
            ,@(make-table-rows-for-blackboard (data p))
            ,@(when (typep p 'object-w-learning)
                (make-table-rows-for-object-w-learning p)))))
        :expand-initially expand-initially))))

;; ##################################################################
;; process
;; ##################################################################

(define-css 'process "
div.process-float { display:inline-block;margin-right:10px;
                margin-top:-6px;margin-bottom:8px; }
div.process .process-title { color:white; position:relative; }
div.process .process-title > a { color:white; }
div.process .process-title { padding:1px;padding-left:3px;padding-right:3px;}
div.process .process-details { padding:0px; }
div.process .process-details > table.two-col { margin-bottom:-5px;}
div.process div.process-details div.process-detail { 
  padding-left:4px; padding-right:4px;padding-bottom:0px;padding-top:0px;
  border-top:1px dashed #563;text-align:left;  }
div.process div.process-details div.process-detail:first-child { border-top:none;}")

(defparameter *process-colors*
  '((create . "#79a") (run . "#8a8") (continue . "#8a8")
    (restart . "#8a9")(restarted . "#8a9")))

(defmethod html-color ((process process))
    (assqv (first (status process)) *process-colors*))

(defmethod make-html ((process process) 
                      &key (expand/collapse-all-id (make-id 'process))
                      (draw-as-tree t)
                      (expand-initially nil))
  (let* ((element-id (make-id 'process))
         (process-color (html-color process))
         (title-div
          `((div :class "process-title" 
                 :style ,(mkstr "background-color:" process-color ";"))))
         (title (downcase (symbol-name (label process))))
         (div 
          (make-expandable/collapsable-element
           element-id expand/collapse-all-id
           `((div :class "process"
                  :style ,(mkstr
                           "margin-top:5px;margin-bottom:5px;"
                           "border-width:1px;border-style:solid;border-color:"
                           process-color ";"))
             (,@title-div
              ((a ,@(make-expand/collapse-link-parameters element-id t)) 
               ,title)))
           `((div :class "process "
                  :style
                  ,(mkstr "margin-top:5px;margin-bottom:5px;"
                          "border-width:1px;border-style:solid;border-color:"
                          process-color ";"))
             (,@title-div
              ((a ,@(make-expand/collapse-link-parameters element-id nil))
               ,title))
             ((div :class "process-details")
              ((div :class "process-detail")
               ((table :class "two-col")
                ((tr)
                 ((td) "status ")
                 ((td) ,(html-pprint (status process))))
                ,@(when (typep process 'object-w-learning)
                    (make-table-rows-for-object-w-learning process))))
              ,@(when (input process) ;; the input result of the process
                  `(((div :class "process-detail")
                     ,(make-table-for-blackboard (data (input process))))))
              ,@(when (fields (data process)) ;; the data of the proces
                  `(((div :class "process-detail")
                     ,(make-table-for-blackboard (data process)))))
              ,@(when (results process)
                  `(((div :class "process-detail")
                     ,(make-html-for-results (results process)))))))
           :expand-initially expand-initially)))
    (if draw-as-tree 
      (draw-node-with-children 
       div
       (loop for child in (children process)
             collect (make-html child 
                                :expand/collapse-all-id expand/collapse-all-id)))
      `((div :class "process-float")
        ,div))))

;; ##################################################################
;; task
;; ##################################################################

(defun make-table-row-for-queue (caption queue)
  `((tr)
    ((td) ,caption)
    ((td) 
     ,@(html-hide-rest-of-long-list
        queue 5 #'(lambda (node) (make-html node :draw-as-tree nil))))))

(defun make-table-row-for-tree (caption top-node)
  (let ((expand/collapse-all-id (make-id 'tree)))
    `((tr)
      ((td) ,(make-expand/collapse-all-link expand/collapse-all-id caption))
      ((td) ,(make-html top-node
                        :expand/collapse-all-id expand/collapse-all-id)))))

(define-css 'task "
div.task { display:inline-block;margin-bottom:10px;margin-right:5px;margin-top:5px;border:1px solid #000; }
.task div.task-title { padding:0px;}
.task div.task-title { padding-left:5px; padding-right:5px; padding-top:3px; padding-bottom:3px; background-color:#000; }
.task div.task-title a { color:#fff; }
")

(defmethod make-html ((task task) &key expand-initially)
  (let ((element-id (make-id 'ta))
        (link-title `((tt)
                      ,(format nil "~(~a~) ~(~a~)"
                               (type-of task) (label task)))))
    `((div :class "task")
      ,(make-expandable/collapsable-element
	element-id (make-id)
        `((div :class "task-title")
	  ((a ,@(make-expand/collapse-link-parameters element-id t)) ,link-title))
        `((span)
          ((div :class "task-title")
           ((a ,@(make-expand/collapse-link-parameters element-id nil)) ,link-title))
          ((table :class "two-col")
           ((tbody)
            ((tr)
             ((td) "processes")
             ((td) ,(html-pprint (processes task))))
            ,@(when (typep task 'object-w-learning)
                    (make-table-rows-for-object-w-learning task))
            ,(make-table-row-for-tree "process-tree" (top task))
            ,@(when (unfinished-processes task)
                (list
                 (make-table-row-for-queue "queue" (unfinished-processes task))))
            ,@(when (results task)
                (make-table-rows-for-results (results task))))))
        :expand-initially expand-initially))))

;;;; (let ((task
;;;;        (make-instance 'task :processes '(test-process-1 test-process-2))))
;;;;   (multiple-value-bind (results processes)
;;;;       (run-task task)
;;;;     (add-element (make-html (top task)))
;;;;     ;; (add-element )
;;;;     ;; (add-element (make-html-for-results (results task)))
;;;;     ))



