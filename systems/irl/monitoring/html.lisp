
(in-package :irl)

(export '(make-tr-for-evaluation-results
          make-tr-for-irl-evaluation-search-process
          make-tr-for-irl-program
          dead-node?))

(defgeneric content->html-table-rows (node &key))

;; #########################################################
;; draw-solutions
;; ---------------------------------------------------------

(defun draw-solutions (solutions)
  "Draws lists of bindings, initially hiding all from the fifth on"
  (if solutions
      (loop with expand/collapse-all-solutions-id = (make-id 'solutions)
         for solution in solutions
         for i from 1
         for solution-id = (make-id 'solution)
         for p = `((p) ,(make-expand/collapse-all-link 
                         solution-id (mkstr "solution " i " "))
                   ,@(loop for binding in solution
                           collect (make-html
                                    binding
                                    :expand/collapse-all-id solution-id
                                    :expand-initially t)))
         do
         (when (< i 4) (add-element p))
         (when (= i 4) 
           (add-element
            (make-expandable/collapsable-element
             (make-id) expand/collapse-all-solutions-id
             `((p) ((a ,@(make-expand/collapse-all-link-parameters
                          expand/collapse-all-solutions-id t))
                    "solutions " ,i "-" ,(length solutions)))
             `((div)))))
         (when (>= i 4)
           (add-element 
            (make-expandable/collapsable-element
             (make-id) expand/collapse-all-solutions-id
             '((div)) p))))
      (add-element `((p) ((b) "no solutions")))))

;; #########################################################
;; make-tr-for-irl-program... (makes table rows)
;; ---------------------------------------------------------

(defun make-tr-for-irl-program (caption irl-program)
  "Makes a table row with caption as the first cell and the s-expression
   and a graph of irl-program as the second cell"
  `((tr)
    ((td) ,caption)
    ((td) ,(html-pprint irl-program :max-width 100)
     ,(if (loop for x in irl-program
             if (eq (car x) 'bind) count x into number-of-bind-statements
             else count x into number-of-primitives
             finally (return (or (> number-of-primitives 1)
                                 (and (= number-of-primitives 1)
                                      (> number-of-bind-statements 0)))))
          (irl-program->svg irl-program)
          ""))))

(defun make-tr-for-queue (caption queue)
  `((tr)
    ((td) ,caption)
    ((td) 
     ,@(html-hide-rest-of-long-list
        queue 5 #'(lambda (node) (make-html node :draw-as-tree nil))))))

(defun make-tr-for-evaluation-results (caption evaluation-results)
  (if evaluation-results
      (let ((expand/collapse-all-id (make-id 'results)))
        `((tr)
          ((td) ,(make-expand/collapse-all-link expand/collapse-all-id caption))
          ((td) 
           ,@(html-hide-rest-of-long-list
              evaluation-results 10
              #'(lambda (r)
                  (make-html r :expand/collapse-all-id expand/collapse-all-id))))))
      ""))

(defun make-tr-for-tree (caption top-node)
  (let ((expand/collapse-all-id (make-id 'tree)))
    `((tr)
      ((td) ,(make-expand/collapse-all-link expand/collapse-all-id caption))
      ((td) ,(make-html top-node
                        :expand/collapse-all-id expand/collapse-all-id)))))

(defun make-tr-for-irl-evaluation-search-process (caption top-node)
  (labels ((all-leaves (node)
             (if (children node)
                 (mappend #'all-leaves (children node))
                 (list node))))
    (let ((tree-id-1 (make-id 'evaluation-tree))
          (tree-id-2 (make-id 'evaluation-tree))
          (best-leaf
           (the-highest (all-leaves top-node)
                        #'(lambda (x) (or (length (primitives-evaluated x)) 0)))))
      `((tr)
        ((td) ,(make-expandable/collapsable-element 
                (make-id) tree-id-1
                `((a ,@(make-expand/collapse-all-link-parameters
                        tree-id-1 "show evaluation process"))
                  ,caption)
                (make-expand/collapse-all-link tree-id-2 caption)))
        ((td) ,(make-expandable/collapsable-element
                (make-id) tree-id-1
                `((a ,@(make-expand/collapse-all-link-parameters
                        tree-id-1 "show evaluation process"))
                  ((i) ,(format 
                         nil "initial~{ - ~(~a~)~}" 
                         (reverse 
                          (mapcar 
                           #'car  
                           (append 
                            (primitives-evaluated best-leaf)
                            (primitives-evaluated-w/o-result best-leaf)))))))
                (lambda ()
                  `((div :style "margin-top:-6px")
                    ,(make-html top-node :expand/collapse-all-id tree-id-2)))))))))

;; #########################################################
;; entity - make-html
;; ---------------------------------------------------------

(defgeneric make-html-for-entity-details (entity &key)
  (:documentation "Creates a list of divs for the expanded version of an entity"))

(export 'make-html-for-entity-details)

(defmethod make-html-for-entity-details ((e entity) &key &allow-other-keys)
  '(""))

(define-css 'entity "
div.entity { display:inline-block;margin-right:10px;margin-top:4px;
             margin-bottom:4px;padding:0px; }
div.entity-box { border:1px solid #562; display:inline-block;}
div.entity div.entity-title  { 
  padding:0px;padding-left:3px;padding-right:3px;
  white-space:nowrap; background-color:#562; }
div.entity div.entity-title > a {color:#fff;}
div.entity div.entity-title > span {color:#fff;}
table.entity { border-collapse:collapse; }
table.entity td.entity-type { font-style:italic;padding:0px;padding-left:4px;}
table.entity td.entity-details div.entity-detail { 
  padding-left:4px; padding-right:4px;padding-bottom:1px;padding-top:1px;
  border-top:1px dashed #563;text-align:left;  }
table.entity td.entity-details > div { overflow:hidden; }
table.entity td.entity-details div.entity-detail:first-child { border-top:none;} 
")

(defmethod make-html ((e entity)
                      &rest parameters
                      &key (expand/collapse-all-id (make-id 'entity))
                      (expand-initially nil))
  `((div :class "entity")
    ,(let ((element-id (make-id (id e))))
          (make-expandable/collapsable-element 
           element-id expand/collapse-all-id
           `((div :class "entity-box")
             ((div :class "entity-title")
              ((a ,@(make-expand/collapse-link-parameters 
                     element-id t "expand entity")
                  :name ,(mkstr (id e)))
               ,(format nil "~(~a~)" (id e)))))
           (lambda ()
             `((div :class "entity-box")
               ((div :class "entity-title")
                ((a ,@(make-expand/collapse-link-parameters element-id nil 
                                                            "collapse entity")
                    :name ,(mkstr (id e)))
                 ,(format nil "~(~a~)" (id e))))
               ((table :class "entity" :cellpadding "0" :cellspacing "0") 
                ((tr)
                 ((td :class "entity-details")
                  ,@(apply 'make-html-for-entity-details e parameters))))))
           :expand-initially expand-initially))
    ((table :class "entity")
     ((tr) ((td :class "entity-type") 
            ,(format nil "~(~a~)" (type-of e)))))))

;; #########################################################
;; binding - make-html
;; ---------------------------------------------------------

(define-css 'binding "
div.binding { display:inline-block;margin-right:20px;
               margin-bottom:15px; border-collapse:collapse; }
div.binding > div.pprint { margin-left:3px;}
div.binding > table { border-collapse:collapse; }
div.binding > table > tbody > tr > td > div.entity { 
    margin-top:3px;margin-bottom:1px;}
div.binding div.unbound-value {
    color:#562;padding:0px;padding-right:3px;padding-left:3px;
    margin-top:3px;margin-bottom:1px;border:1px dotted #562; }
div.binding td.score { padding-left:4px; }
")

(defmethod make-html ((binding binding) &key (expand/collapse-all-id (make-id 'b))
                      (expand-initially nil))
  `((div :class "binding")
    ,(html-pprint (var binding))
    ((table)
     ((tbody)
      ((tr) 
       ((td) 
        ,(if (value binding)
             (make-html (value binding)
                        :expand-initially expand-initially
                        :expand/collapse-all-id expand/collapse-all-id)
             '((div :class "unbound-value") "unbound"))))
      ,@(when (score binding)
              `(((tr)
                 ((td :class "score")
                  ,(format nil "~,3f" (score binding))))))))))

;; #########################################################
;; irl-program-evaluation-node - make-html
;; ---------------------------------------------------------

(define-css 'ipen "
div.ipen { margin-top:5px;margin-bottom:5px; }
div.ipen .ipen-title { color:white; position:relative;padding:3px;
                       padding-bottom:2px;padding-top:2px;}
div.ipen div.ipen-details { padding:3px; }
div.ipen > div > div.ipen-details:last-child { margin-bottom:-15px; }
")

(defparameter *irl-program-evaluation-node-status-colors*
  '((initial . "#242") 
    (primitives-remaining . "#337")
    (inconsistent . "#822")
    (no-primitives-remaining . "#445")
    (bad-node . "#500")
    (solution . "#050")))


(defun irl-program-evaluation-node->title (node)
  (cond
    ((eq (status node) 'initial)
     "initial")
    ((member (status node) '(primitives-remaining
                             no-primitives-remaining
                             solution
                             inconsistent
                             bad-node))
     (if (primitives-evaluated node)
         (format nil "~(~a~)" (caar (primitives-evaluated node)))
         (symbol-name (status node))))
    (t "UNKNOWN STATUS")
    ))
  
(defmethod make-html ((node irl-program-evaluation-node) 
                      &key (expand/collapse-all-id (make-id 'ipen))
                      (expand-initially nil))
  (let* ((element-id (make-id 'ipen))
         (bindings-id (make-id 'bindings)) 
         (status (status node))
         (node-color
          (or (assqv status *irl-program-evaluation-node-status-colors*)
              (error "no status color defined for status ~a"  status)))
         (title-div          
          `((div :class "ipen-title"
                 :style ,(mkstr "color:" node-color ";"
                                (cond ((member status 
                                               '(initial inconsistent bad-node
                                                 no-primitives-remaining))
                                       "font-style:italic")
                                      ((member status '(solution))
                                       "font-weight:bold")
                                      (t ""))))))
         (title 
          `((span :style ,(mkstr "color:" node-color)) 
            ,(irl-program-evaluation-node->title node))))
    (draw-node-with-children 
     (make-expandable/collapsable-element 
      element-id expand/collapse-all-id
      ;; collapsed element
      `((div :class "ipen")
        (,@title-div
         ((a ,@(make-expand/collapse-link-parameters element-id t)) ,title)))
      ;; expanded element
      (lambda ()
        (flet ((show-primitives (list title)
                 (if list
                     `((div :class "ipen-details") ,title ":&#160;&#160;"
                       ((div :style "display:inline-table;") ,(html-pprint list)))
                     "")))
          (let ((status `((span :style ,(mkstr "color:" node-color))
                          ,(format nil "status: ~(~a~)" (status node))))
                (id (make-id 'ipen)))
            `((div :class "ipen" :style ,(mkstr "border:1px dotted " node-color))
              (,@title-div
               ((a ,@(make-expand/collapse-link-parameters element-id nil))
                ,title))
              ((div :style ,(mkstr "border-top:1px dotted " node-color))
               ((div :class "ipen-details") 
                ,(make-expand/collapse-all-link 
                  id status "show primitives" "hide primitives"))
               ,(make-expandable/collapsable-element
                 (make-id) id
                 `((div))
                 `((div)
                   ,(show-primitives (primitives-evaluated node)
                                     "evaluated primitives")
                   ,(show-primitives (primitives-remaining node)
                                     "remaining primitives")
                   ,(show-primitives (primitives-evaluated-w/o-result node)
                                     "primitives evaluated w/o result")))
               ((div :class "ipen-details")
                ,(make-expand/collapse-all-link bindings-id "bindings:&#160;&#160;")
                ,@(loop for b in (bindings node)
                     collect (make-html 
                              b 
                              :expand/collapse-all-id bindings-id))))))))
      :expand-initially expand-initially)
     (loop for child in (children node)
        collect (make-html child :expand/collapse-all-id expand/collapse-all-id))
     :color "#aaa")))

;; #########################################################
;; chunk - make-html
;; ---------------------------------------------------------

(define-css 'chnk "
div.chnk { border:1px solid #543; display:inline-block; margin-right:5px; margin-bottom:5px;}
div.chnk-title { position:relative;background-color:#543;padding:1px;
                 padding-left:3px;padding-right:3px; }
div.chnk-title > a { color:white; }
div.chnk-title > div.save-button { position:absolute;right:5px;top:1px;display:none;}
div.chnk-title > div.save-button > a { color:#fff; }
div.chnk table.chnk-details { margin:3px;margin-bottom:-5px;}
div.chnk .chnk-details img { margin:-15px;margin-top:0px;margin-right:0px; }
")

;; keeps each chunk that is drawn in a hash table for potential saving
(defvar *saveable-chunks* (make-hash-table :test #'equal))

;; empty the hash table when the 'reset' button is pushed
(defun reset-saveable-chunks ()
  (setf *saveable-chunks* (make-hash-table :test #'equal)))

(pushnew #'reset-saveable-chunks wi::*reset-functions*)

;; this is the variable that the chunk is saved to
(defvar *saved-chunk* nil)

(export '*saved-chunk*)

(ht-simple-ajax:defun-ajax save-chunk (id) (wi::*ajax-processor*)
  "Called from the html page to save a chunk to a global variable"
  (setf *saved-chunk* (gethash id *saveable-chunks*))
  (add-element `((hr)))
  (add-element `((p) "Saved chunk " ((div) ,(make-html *saved-chunk*))
		 " to global variable " ((tt) ((b) "*saved-chunk*"))))
  (render-xml nil))

(defmethod make-html ((chunk chunk) 
                      &key (expand/collapse-all-id (make-id 'chunk))
                      (expand-initially nil))
  (let ((chunk-div-id (make-id 'chunk))
        (title (format nil "~(~a~) (~,2f)" (id chunk) (score chunk)))
        (save-button-id (make-id 'chunk)))
    (make-expandable/collapsable-element 
     chunk-div-id expand/collapse-all-id
     `((div :class "chnk")
       ((div :class "chnk-title")
        ((a ,@(make-expand/collapse-link-parameters chunk-div-id t)) ,title)))
     (lambda ()
       `((div :class "chnk"
              ,@(unless wi::*static-html* 
                        `(:onmouseover 
                          ,(mkstr "document.getElementById('" save-button-id
                                  "').style.display = 'inline';")
                          :onmouseout 
                          ,(mkstr "document.getElementById('" save-button-id 
                                  "').style.display = 'none';"))))
         ((div :class "chnk-title")
          ((a ,@(make-expand/collapse-link-parameters chunk-div-id nil)) ,title)
          ,@(unless 
             wi::*static-html*
             (setf (gethash (format nil "~(~a~)" save-button-id) *saveable-chunks*)
                   chunk)
             `(((div :class "save-button" :id ,(mkstr save-button-id)) 
                ((a :href ,(format nil "javascript:ajax_save_chunk('~(~a~)');" 
                                   save-button-id)
                    :title "save chunk") "save")))))
         ((table :class "chnk-details two-col")
          ((tbody)
           ,(if (target-var chunk)
                `((tr)
                  ((td) "target&#160;var")
                  ((td) ((div :style "display:inline-block")
                         ,(html-pprint (car (target-var chunk))))
                   ,(format nil "&#160;(~(~a~))" (cdr (target-var chunk)))))
                "")
           ,(if (open-vars chunk)
                `((tr)
                  ((td) "open&#160;vars")
                  ((td) 
                   ,@(loop for (id . type) in (open-vars chunk)
                        for i from 1
                        append `(((span :style "white-space:nowrap")
                                  ((div :style "display:inline-block")
                                   ,(html-pprint id))
                                  ,(format nil " (~(~a~))" type)
                                  ,(if (< i (length (open-vars chunk)))
                                       "," ""))
                                 " "))))
                "")
           ,(make-tr-for-irl-program "irl&#160;program" (irl-program chunk))
           ((tr)
            ((td) "score")
            ((td) ,(format nil "~,2f" (score chunk))))))))
     :expand-initially expand-initially)))

;; #########################################################
;; chunk-evaluation-result - make-html
;; ---------------------------------------------------------

(define-css 'cer "
div.cer { border:1px solid #465; display:inline-block; margin-right:7px; margin-bottom:7px;}
div.cer .cer-title { background-color:#465;padding:1px;
                     padding-left:3px;padding-right:3px; }
div.cer .cer-title > a { color:white; }
div.cer table.cer-details  { margin:3px;margin-bottom:-5px;}
div.cer table.cer-details img { margin:-15px;margin-top:0px;margin-right:0px; }
")

(defmethod make-html ((result chunk-evaluation-result)
                      &key (expand/collapse-all-id (make-id 'cer))
                      (expand-initially nil))
  (let ((cer-div-id (make-id 'cer))
        (bindings-id (make-id 'bindings))
        (title 
         (append 
          (loop for value in (mapcar #'fourth (bind-statements result))
             for i from 1
             collect (format nil "~(~a~)~:[~;,&#160;~]" value
                             (< i (length (bind-statements result)))))
          (list (format nil " (~,2f)" (score result))))))
    (make-expandable/collapsable-element 
     cer-div-id expand/collapse-all-id
     `((div :class "cer")
       ((div :class "cer-title")
        ((a ,@(make-expand/collapse-link-parameters cer-div-id t)) ,@title)))
     (lambda ()
       `((div :class "cer")
         ((div :class "cer-title")
          ((a ,@(make-expand/collapse-link-parameters cer-div-id nil)) ,@title))
         ((table :class "cer-details two-col")
          ((tbody)
           ((tr)
            ((td) "chunk")
            ((td) ,(make-html (chunk result)
                              :expand-initially t)))
           ,@(when (evaluation-tree result)
                   `(,(make-tr-for-irl-evaluation-search-process 
                       "evaluation process"
                       (evaluation-tree result))))
           ((tr)
            ((td) "target entity")
            ((td) ,(make-html (target-entity result))))
           ((tr)
            ((td) ,(make-expand/collapse-all-link bindings-id "bindings"))
            ((td) 
             ,@(loop for b in (bindings result)
                  collect (make-html b :expand/collapse-all-id bindings-id))))
           ((tr)
            ((td) "bind statements")
            ((td) ,(html-pprint (bind-statements result) :max-width 100)))))))
     :expand-initially expand-initially)))

(defmethod make-html ((result chunk-composer-node-solution)
                      &key (expand/collapse-all-id (make-id 'cer))
                      (expand-initially nil))
  (let ((cer-div-id (make-id 'cer))
        (bindings-id (make-id 'bindings))
        (title 
         (append 
          (loop for value in (mapcar #'fourth (bind-statements result))
             for i from 1
             collect (format nil "~(~a~)~:[~;,&#160;~]" value
                             (< i (length (bind-statements result)))))
          (list (format nil " (~,2f)" (score result))))))
    (make-expandable/collapsable-element 
     cer-div-id expand/collapse-all-id
     `((div :class "cer")
       ((div :class "cer-title")
        ((a ,@(make-expand/collapse-link-parameters cer-div-id t)) ,@title)))
     (lambda ()
       `((div :class "cer")
         ((div :class "cer-title")
          ((a ,@(make-expand/collapse-link-parameters cer-div-id nil)) ,@title))
         ((table :class "cer-details two-col")
          ((tbody)
           ((tr)
            ((td) "node")
            ((td) ,(make-html (node result) :draw-as-tree nil)))
           ((tr)
            ((td) "chunk")
            ((td) ,(make-html (chunk result)
                              :expand-initially t)))
           ,@(when (evaluation-tree result)
                   `(,(make-tr-for-irl-evaluation-search-process 
                       "evaluation process"
                       (evaluation-tree result))))
           ((tr)
            ((td) "target entity")
            ((td) ,(make-html (target-entity result))))
           ((tr)
            ((td) ,(make-expand/collapse-all-link bindings-id "bindings"))
            ((td) 
             ,@(loop for b in (bindings result)
                  collect (make-html b :expand/collapse-all-id bindings-id))))
           ((tr)
            ((td) "bind statements")
            ((td) ,(html-pprint (bind-statements result) :max-width 100)))))))
     :expand-initially expand-initially)))

;; #########################################################
;; dead-node?
;; #########################################################

(defgeneric dead-node? (node))

(defmethod dead-node? ((node chunk-composer-node))
  nil)

(defmethod dead-node? ((node single-topic-composer-node))
  "Determines wheter a subtree is fully expanded and does not contain solutions"
  (labels ((dead-node-aux (node)
             (if (children node)
                 (loop for child in (children node)
                    always (dead-node-aux child))
                 (member (status node) 
                         '(match-chunk-failed
                           chunk-wrapper-failed
                           duplicate-chunk
                           bad-chunk
                           max-search-depth-reached
                           no-further-combination-possible
                           no-evaluation-results
                           combined-program)))))
    (and (> (depth node) 1) (children node) (dead-node-aux node))))

;; #########################################################
;; chunk-composer-node - make-html & content->htm-table-rows
;; ---------------------------------------------------------

(define-css 'ccn "
div.ccn { margin-top:5px;margin-bottom:5px; }
div.ccn-light { margin-top:5px;margin-bottom:5px;padding-left:1px;
                font-style:italic; }
div.ccn-float { display:inline-block;margin-right:10px;
                margin-top:-6px;margin-bottom:8px; }
div.ccn .ccn-title { color:white; position:relative; }
div.ccn .ccn-title > a { color:white; }
div.ccn .ccn-title > div.save-button { 
   position:absolute;right:5px;top:1px;display:none;}
div.ccn .ccn-title > div.save-button > a { color:#fff; }
div.ccn .ccn-title { padding:1px;padding-left:3px;padding-right:3px;}
div.ccn .ccn-details { padding:2px; }
div.ccn .ccn-details > table.two-col { margin-bottom:-5px;}
div.ccn-dead-node { padding-left:2px;
                    padding-right:1px;padding-bottom:1px;}
")

(export '*saveable-ccns*)

;; keeps each ccn that is drawn in a hash table for potential saving
(defvar *saveable-ccns* (make-hash-table :test #'equal))

;; empty the hash table when the 'reset' button is pushed
(defun reset-saveable-ccns ()
  (setf *saveable-ccns* (make-hash-table :test #'equal)))

(pushnew #'reset-saveable-ccns wi::*reset-functions*)

;; this is the variable that the node is saved to
(defvar *saved-ccn* nil)

(export '(*saved-ccn* *chunk-composer-node-status-colors*))

(ht-simple-ajax:defun-ajax save-ccn (id) (wi::*ajax-processor*)
  "Called from the html page to save a ccn to a global variable"
  (setf *saved-ccn* (gethash id *saveable-ccns*))
  (add-element `((hr)))
  (add-element `((p) "Saved chunk composer node " ((div) ,(make-html *saved-ccn*))
		 " to global variable " ((tt) ((b) "*saved-ccn*"))))
  (render-xml nil))

;; #* added
(defparameter *chunk-composer-node-handler-colors*
  '((initial . "#8a8") (processed . "#79a") (evaluate . "#7aa")
    (expand . "#268") (evaluation-succeeded . "#273")))

;; #* added 
(defmethod html-color ((node chunk-composer-node))
  (let ((next-handler (cond ((solutions node) 'evaluation-succeeded)
                            ((next-handler node) (next-handler node))
                            (t 'processed))))
    (assqv next-handler *chunk-composer-node-handler-colors*)))

(defmethod make-html ((node chunk-composer-node) 
                      &key (expand/collapse-all-id (make-id 'ccn))
                      (draw-as-tree t)
                      (hide-dead-subtree t)
                      (expand-initially nil)
                      (initial-node nil))
  (let* ((node (copy-object node))
         (element-id (make-id 'ccn))
         (save-button-id (make-id 'ccn))
         (node-color (html-color node))
         (title-div
          `((div :class "ccn-title" 
                 :style ,(mkstr "background-color:" node-color ";"))))
         (title 
          (list (format nil "~a&#160;" (sequence-number node))
                (if (and (not (irl-program->title (irl-program (chunk node))))
                         initial-node)
                  "initial"
                  (irl-program->title (irl-program (chunk node))))))
                  
         (div 
          (make-expandable/collapsable-element 
           element-id expand/collapse-all-id
               `((div :class "ccn" :style ,(mkstr "border:1px solid " node-color))
                 (,@title-div
                  ((a ,@(make-expand/collapse-link-parameters element-id t)) 
                   ,@title)))
           (lambda ()
             `((div :class "ccn" :style ,(mkstr "border:1px solid " node-color)
                    ,@(unless wi::*static-html*
                              `(:onmouseover 
                                ,(mkstr "document.getElementById('" save-button-id
                                        "').style.display = 'inline';")
                                :onmouseout 
                                ,(mkstr "document.getElementById('" save-button-id 
                                        "').style.display = 'none';"))))
               (,@title-div
                ((a ,@(make-expand/collapse-link-parameters element-id nil))
                 ,@title)
                ,@(unless 
                   wi::*static-html* 
                   (setf (gethash (format nil "~(~a~)" save-button-id) 
                                  *saveable-ccns*)
                         node)
                   `(((div :class "save-button" :id ,(mkstr save-button-id)) 
                      ((a :href ,(format nil "javascript:ajax_save_ccn('~(~a~)');" 
                                         save-button-id)
                          :title "save node") "save")))))
               ((div :class "ccn-details")
                ((table :class "two-col")
                 ((tbody)
                  ,@(content->html-table-rows node)
                  )))))
           :expand-initially expand-initially)))
    (if draw-as-tree 
        (draw-node-with-children 
         div
         (if (and hide-dead-subtree (dead-node? node))
             (let ((id (make-id 'sub-tree)))
               (list (make-expandable/collapsable-element 
                      id (make-id)
                      `((div :class "ccn-dead-node")
                        ((a ,@(make-expand/collapse-link-parameters 
                               id t "show dead subtree")) "+"))
                      (draw-node-with-children 
                       `((div :class "ccn-dead-node")
                         ((a ,@(make-expand/collapse-link-parameters 
                                id nil "hide dead subtree")) "-"))
                       (loop for child in (children node)
                          collect (make-html child :hide-dead-subtree nil))))))
             (loop for child in (children node)
                collect (make-html child :hide-dead-subtree hide-dead-subtree
                                   :expand/collapse-all-id expand/collapse-all-id))))
         `((div :class "ccn-float")
           ,div))))

(defmethod content->html-table-rows ((node chunk-composer-node) &key)
  (let ((node-color (html-color node)))
    `(((tr)
       ((td) "next-handler")
       ((td)
        ((span :style ,(mkstr "color:" node-color)) 
                 ,(format nil "~(~a~)" (next-handler node))
                 ,@(if 
                       (handler-history node)
                     `(" (" 
                       ,@(loop for s on (handler-history node)
                               collect 
                               `((span 
                                  :style 
                                  ,(mkstr 
                                    "color:" 
                                    (assqv (car s) 
                                           *chunk-composer-node-handler-colors*)))
                                 ,(format nil "~(~a~)~:[~;, ~]" (car s) (cdr s))))
                       ")")))))
      ((tr)
       ((td) "source-chunks")                   
       ((td) ,@(loop for c in (source-chunks node)
                     collect
                     (make-html c))))
      ((tr)
       ((td) "chunk")
       ((td) ,(make-html (chunk node) :expand-initially t)))
      ,@(if (matched-chunks node)
          (loop with new-chunks 
                = (remove (chunk node) (matched-chunks node))
                with l>1 = (> (length new-chunks) 1)
                for c in new-chunks  for i from 1
                collect `((tr) 
                          ((td) ,(mkstr "matched chunk " (if l>1 i "")))
                          ((td) ,(make-html c :expand-initially t))))
          '(""))
      ,(make-tr-for-evaluation-results 
        "evaluation results" (solutions node)))))

;; ##########################################################
;; multi-topic-composer-node - content->html-table-rows
;; ----------------------------------------------------------

(defmethod content->html-table-rows ((node multi-topic-composer-node) &key)
  (append (call-next-method node)
          (list (make-tr-for-evaluation-results 
                 "evaluation results before goal test" (results-before-goal-test node)))))

;; ##########################################################
;; single-topic-composer-node - content->html-table-rows
;; ----------------------------------------------------------

(defparameter *chunk-composer-node-status-colors*
  '((initial . "#8a8") (combined-program . "#79a") (combined-call-pattern . "#7aa")
    (recombined-open-variables . "#7a9") (linked-open-variables . "#799")
    (match-chunk-failed . "#876") (chunk-wrapper-failed . "#867")
    (duplicate-chunk . "#765") (bad-chunk . "#744")
    (no-evaluation-results . "#268") (no-further-combination-possible . "#678") 
    (max-search-depth-reached . "#556") 
    (bad-evaluation-results . "#845") (evaluation-succeeded . "#273")))

(defmethod html-color ((node single-topic-composer-node))
  (or (assqv (status node) *chunk-composer-node-status-colors*)
      (error "no status color defined for status ~a" (status node))))

(defmethod content->html-table-rows ((node single-topic-composer-node) &key)
  (let ((default-trs (call-next-method node))
        (node-color (html-color node)))
    `(;;handler
      ,(first default-trs)
      ;;status
      ((tr)
       ((td) "status")
       ((td) 
        ((span :style ,(mkstr "color:" node-color)) 
         ,(format nil "~(~a~)" (status node))
         ,@(if 
               (status-history node)
             `(" (" 
               ,@(loop for s on (status-history node)
                       collect 
                       `((span 
                          :style 
                          ,(mkstr 
                            "color:" 
                            (assqv (car s) 
                                   *chunk-composer-node-status-colors*)))
                         ,(format nil "~(~a~)~:[~;, ~]" (car s) (cdr s))))
               ")")))))
      ;;source-chunks
      ,(second default-trs)
      ;;chunk
      ,(third default-trs)
      ;;matched
      ,@(subseq default-trs 3 (1- (length default-trs)))
      ;;wrapped
      ,@(if (wrapped-chunks node)
          (loop with new-chunks 
                = (loop for chunk in (wrapped-chunks node)
                        unless (or (find chunk (matched-chunks node))
                                   (eq chunk (chunk node)))
                        collect chunk)
                with l>1 = (> (length new-chunks) 1)
                for c in new-chunks  for i from 1
                collect `((tr) 
                          ((td) ,(mkstr "wrapped chunk " (if l>1 i "")))
                          ((td) ,(make-html c :expand-initially t))))
          '(""))
      ;;evaluation trees
      ,@(let ((evaluation-trees (or (loop for sol in (solutions node)
                                          when (evaluation-tree sol)
                                          collect (evaluation-tree sol))
                                    (and (evaluation-tree node)
                                         (list (evaluation-tree node))))))
          (when evaluation-trees
            (loop for tree in evaluation-trees
                  for i from 1
                  collect (make-tr-for-irl-evaluation-search-process
                           (mkstr "evaluation process" 
                                  (if (cdr evaluation-trees)
                                    i ""))
                           tree))))
      ;;bad-results
      ,(make-tr-for-evaluation-results 
        "bad evaluation results" (bad-evaluation-results node))
      ;;actual results
      ,@(last default-trs))))

;; #########################################################
;; chunk-composer - make-html & content->html-table-rows
;; ---------------------------------------------------------

(defmethod make-html ((composer chunk-composer) &key (verbose nil))
  `((table :class "two-col")
    ((tbody)
     ,@(content->html-table-rows composer :verbose verbose))))

(defmethod content->html-table-rows ((composer chunk-composer) &key (verbose nil))
  `(,@(if verbose
        `(((tr) 
           ((td) "max-search-depth")
           ((td) ,(max-search-depth composer))))
        '(""))
    ((tr)
     ((td) "chunks")
     ((td) ,@(mapcar #'make-html (chunks composer))))
    ,(if (meaning composer)
       (make-tr-for-irl-program "meaning" (meaning composer))
       "")
    ,(make-tr-for-tree "composition-tree" (top-node composer))
    ,(make-tr-for-queue "queue" (queue composer))
    ,(make-tr-for-evaluation-results 
      "evaluation results" (solutions composer))))
  
;; #########################################################
;; single-topic-composer
;; ---------------------------------------------------------

(defmethod make-html ((composer single-topic-composer) &key (verbose nil))
   `((table :class "two-col")
     ((tbody)
      ,@(if verbose
           `(((tr) 
              ((td) "max-search-depth")
              ((td) ,(max-search-depth composer)))
             ((tr) 
              ((td) "check-chunk-fns")
              ((td) ,(html-pprint (check-chunk-fns composer))))
             ((tr) 
              ((td) "node-rating-fn")
              ((td) ,(html-pprint (node-rating-fn composer))))
             ((tr) 
              ((td) "initial-chunk-score-fn")
              ((td) ,(html-pprint (initial-chunk-score-fn composer))))
             ((tr) 
              ((td) "chunk-wrapper-fn")
              ((td) ,(html-pprint (chunk-wrapper-fn composer))))
             ((tr) 
              ((td) "check-evaluation-result-fn")
              ((td) ,(html-pprint (check-evaluation-result-fn composer))))
             ((tr) 
              ((td) "evaluation-result-scoring-fn")
              ((td) ,(html-pprint (evaluation-result-scoring-fn composer)))))
           '(""))
      ((tr)
       ((td) "chunks")
       ((td) ,@(mapcar #'make-html (chunks composer))))
      ,(if (meaning composer)
           (make-tr-for-irl-program "meaning" (meaning composer))
           "")
      ,(make-tr-for-tree "composition-tree" (top-node composer))
      ,(make-tr-for-queue "queue" (queue composer))
      ,(make-tr-for-evaluation-results 
        "evaluation results" (solutions composer)))))
