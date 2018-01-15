(in-package :fcg)

(export '(specified-hierarchy-features?))

;;;;;;;;;;;;; WARNING!!!!!!!!!! ;;;;;;;;;;;;;;;;;;;;;;;;
;;This visualization configuration is only used as a last resort when
;;no construction inventory is used. Typically to draw units or
;;transient structures manually.
(defvar *default-visualization-configuration*
  (make-instance 'configuration
                 :configuration '(;; Show cfs and cxn as coupled feature structure?
                                  (:coupled-mode . t)
                                  ;; show expanded nodes in fcg search tree or below
                                  (:expand-nodes-in-search-tree . t)
                                  ;; Represent subfeatures in a different way?
                                  (:subfeatures . nil)
                                  ;; Specify which features should be hidden in the web interface (only works on top-level features). 
                                  (:hide-features . (footprints))
                                  ;; Specify which features names should be checked when drawing the hierarchy of units and subunits in the wi
                                  (:hierarchy-features . (subunits))
                                  ;; Select the feature that will be use to draw the hierarchy of units and subunits in the wi
                                  ;; if nil (default), it will use all the feature names specified in :hierarchy-features
                                  ;; otherwise, it will only use the feature name specified
                                  (:selected-hierarchy . nil)
                                  ;; Specify which subfeatures should be shown in the web interface.
                                  ;; if value is'(), all subfeatures will be shown
                                  (:select-subfeatures . nil)
                                  ;; Show the search debug data
                                  (:with-search-debug-data . nil)
                                  (:latex-visualization . nil)
                                  ;; Visualize a menu in the upper part of the web interface
                                  (:show-upper-menu . nil)
                                  ;; Hide units which are empty or only contain footprints
                                  (:remove-empty-units . nil)
                                  ;; Draw meaning and form in car (tree)
                                  (:add-form-and-meaning-to-car . t)
                                  ;; Constructional dependencies:
                                  (:show-constructional-dependencies . t)
                                  (:labeled-paths . nil) ;;nil/no-bindings/full
                                  (:colored-paths . nil)
                                  )))

;;hash table where all the nodes of the application process
;;and if they are expanded or not are stored
(defvar *expanded-node* (make-hash-table :test 'equal))

(defun reset-expanded-node ()
  (setf *expanded-node* (make-hash-table :test 'equal)))

(pushnew #'reset-expanded-node web-interface::*reset-functions*)

(defun store-expanded-node (id expanded?)
  (setf (gethash (mkstr id) *expanded-node*) (if expanded? t nil)))

(defun get-expanded-node ()
  "Returns an association list containing the keys of hash table TABLE"
  (let ((expanded-nodes-list nil))
    (maphash (lambda (k v)
               (if v
                 (push k expanded-nodes-list)))
             *expanded-node*)
    expanded-nodes-list))

;;hash table that stores the cipn that corresponds to each
;;node of the application process
(defvar *current-cipn* (make-hash-table :test 'equal))

(defun reset-current-cipn ()
  (setf *current-cipn* (make-hash-table :test 'equal)))

(pushnew #'reset-current-cipn web-interface::*reset-functions*)

(defun store-current-cipn (object id)
  (setf (gethash (mkstr id) *current-cipn*) object))

(defun get-current-cipn (id)
  "Returns an association list containing the keys of hash table TABLE"
  (let ((value nil))
    (maphash (lambda (k v)
               (if (equal k id)
                 (setf value v)))
             *current-cipn*)
    value))

;;in the compacted visualization, the last node of the application
;;process appears as the selected node. This variable is keeps track
;;of the last node in order to print it 
(defvar *last-cipn* nil)

;;list of functions called from the html used in the new web interface
(web-interface::defun-ajax update_node_to_collapse_func (node_id) (web-interface::*ajax-processor*)
  "Called from the html page to collapse all the previous expanded elements and store the current node_id as expanded"
  (let ((expanded-nodes (get-expanded-node)))
    (loop for ex-node in expanded-nodes do
          (expand-or-collapse-node ex-node nil)
          (store-expanded-node ex-node nil))
    (store-expanded-node node_id t)
    nil))

(web-interface::defun-ajax reset_expanded_nodes_func () (web-interface::*ajax-processor*)
  "Called from the html page to reset all the previous expanded elements "
  (let ((expanded-nodes (get-expanded-node)))
    (loop for ex-node in expanded-nodes do
          (expand-or-collapse-node ex-node nil)
          (store-expanded-node ex-node nil))
    nil))

(web-interface::defun-ajax update_selected_structure (info_struct_id cipn_id) (web-interface::*ajax-processor*)
  "Called from the html page to replace the content of one element"
  (replace-element-content (string-downcase info_struct_id) (get-replacing-content (string-downcase info_struct_id) (get-current-cipn (mkstr (string-upcase cipn_id)))))
  nil)

;;modified versions of make-expand/collapse-link-parameters
;;the main difference is that they call several js functions from one click
(defun make-expand/collapse-link-parameters-of-subtrees (element-id expand? &optional (title nil))
  "to be used when creating a <a >click</a> link for expanding/ collapsing nodes"
  `(:href ,(format nil "javascript:ajax_reset_expanded_nodes_func();javascript:~a('~(~a~)');" 
		   (if expand? "expand" "collapse") element-id)
    :title ,(or title (if expand? "expand" "collapse"))))

(defun extended-make-expand/collapse-link-parameters (element-id info-struct-id &key (title nil))
  "to be used when creating a <a >click</a> link for expanding/ collapsing nodes"
  `(:href ,(format nil "javascript:ajax_update_node_to_collapse_func('~(~a~)'); expand('~(~a~)');ajax_update_selected_structure(~('~a', '~a'~)); "
		   element-id element-id info-struct-id element-id)
    :title ,(if title title "expand")))

;; #########################################################
;; helper accessor functions
;; ---------------------------------------------------------

(defun <units> (structure)
  (remove-J-units structure))
  
(defun <j-units> (structure)
  (get-J-units structure))
  
(defun <unit-name> (unit)
  (car unit))
  
(defun <features> (unit)
  (cdr unit))

(defun <sorted-features> (unit)
  (sort (cdr unit) (lambda (f1 f2) (string-lessp (symbol-name f1) (symbol-name f2))) :key 'first))
    
(defun <feature-tag> (feature)
  (when (or (eq 'TAG-ONLY (first feature)) 
	    (eq 'TAG-WITHOUT (first feature))
	    (eq 'TAG (first feature))
	    (eq 'TAG-PARTS (first feature)))
    (first feature)))

(defun <feature-name> (feature)
  (if (<feature-tag> feature) 
    (second feature) (first feature)))

(defun <subfeature-name> (subfeature)
  (car subfeature))

(defun <subfeature-values> (subfeature)
  (cdr subfeature))

(defun <sorted-subfeatures> (subfeature)
  (sort subfeature (lambda (f1 f2) (string-lessp (if (symbolp f1) (symbol-name f1) (write-to-string f1))
                                                 (if (symbolp f2) (symbol-name f2) (write-to-string f2))))
        :key 'first))

(defun <feature-value> (feature)
  (if (<feature-tag> feature)
    (third feature)
    (second feature)))

(defun <get-feature> (unit name)
  (loop for feature in (<features> unit)
        when (string= (<feature-name> feature) name)
        return feature))

(defun specified-hierarchy-features? (&key (configuration *default-visualization-configuration*))
  "This function checks if the user uses different hierarchy-features in the configuration than the default ones"
  (set-difference (get-configuration configuration :hierarchy-features) (list 'subunits)))

(defun <get-feature-names-that-specify-hierarchy> (&key (configuration *default-visualization-configuration*))
  "this function returns the list of feature names that will be used to draw the unit hierarchies"
  (let ((feature-for-hierarchy (get-configuration configuration :selected-hierarchy))) 
    (if ;; a feature-for-hierarchy has been selected
        (and feature-for-hierarchy
             ;; it is a symbol
             (symbolp feature-for-hierarchy)
             ;; and it is one of possible the feature names for a hierarchy
             (find feature-for-hierarchy (get-configuration configuration :hierarchy-features) :test #'string= ))
      ;;return the selected feature-for-hierarchy
      (values (list feature-for-hierarchy) t)
      ;;otherwise return the list of hierarchy-features
      (get-configuration configuration :hierarchy-features))))

(defun <subunit-names> (unit &key (configuration *default-visualization-configuration*))
  "This function returns the hierarchical features specified in the configuration
e.g.:(subunits (unit-1 unit-2))"
  (let ((feature-hierarchy (<get-feature-names-that-specify-hierarchy> :configuration configuration))
         subunits-feature)
    (dolist (fh feature-hierarchy)
          (let ((val (<get-feature> unit fh)))
            (when val
              (setf subunits-feature val)
              (return t))))
    (when subunits-feature
      (feature-value subunits-feature))))

(defun <level-0-units> (structure &key (configuration *default-visualization-configuration*))
  (loop with all-subunit-names = (loop for unit in (<units> structure)
                                       append (<subunit-names> unit :configuration configuration))
        for unit in (<units> structure)
        unless (find (<unit-name> unit) all-subunit-names)
        collect unit))

(defun <subunits> (unit structure &key (configuration *default-visualization-configuration*))
  (loop with subunit-names = (<subunit-names> unit :configuration configuration)
        for unit in (<units> structure)
        when (find (<unit-name> unit) subunit-names)
        collect unit))

(defun feature-to-hide-p (feature &key (configuration *default-visualization-configuration*))
  "Is the feature in the list of :hide-features, given in the visualization configuration of a cxn-inventory."
  (if (get-configuration configuration :hide-features)
    (member (string (<feature-name> feature))
          (get-configuration configuration :hide-features)
          :test #'string-equal)
    nil))

(defun is-subfeature-in-subfeatures-to-show (subfeature-name &key (configuration *default-visualization-configuration*))
  (or (not (get-configuration configuration :select-subfeatures))
      (member (if (numberp subfeature-name)
                (write-to-string subfeature-name)
                (string subfeature-name))
              (get-configuration configuration :select-subfeatures)
              :test #'string-equal)))

;; #########################################################
;; unit
;; ---------------------------------------------------------

(define-css 'unit "
div.unit { border:1px solid #888; margin-bottom:6px; margin-top:6px; }
div.unit-name { padding-top:2px; padding-bottom:2px; padding-left:5px; padding-right:5px; color:#009; }
div.unit-feature { padding-left:5px; padding-right:2px;padding-top:3px;padding-bottom:3px;border-top:1px dashed #bbb;background-color:#fff;}
div.unit-feature-name { float:left;color:#060;white-space:nowrap;line-height:13px; margin-bottom:0px; }
div.unit-feature-name span.tagged { color:#700; } 
div.unit-feature-value { margin-top:1px;margin-bottom:0px;margin-left:0px;margin-right:5px;display:inline-table; }
div.unit-subfeature { margin-left:0px;margin-right:2px;margin-top:0px;margin-bottom:0px; background-color:#fff;}
div.unit-subfeature-name { float:left;color:#105799;white-space:nowrap;line-height:13px; margin-top:1px;margin-bottom:0px;margin-right:-10px; }
div.unit-subfeature-name span.tagged { color:#700; }
div.unit-subfeature-value {  margin-left:15px;margin-right:5px;margin-bottom:1px;display:inline-table; }
")

(define-css 'unit-with-direction "
div.unit-with-direction { border:1px solid #888; margin-bottom:6px; margin-top:6px;background-color:#CEE4FF; }
div.unit-with-direction-name { padding-top:2px; padding-bottom:2px; padding-left:5px; padding-right:5px; color:#009; }
div.unit-with-direction-feature { padding-left:5px; padding-right:2px;padding-top:3px;padding-bottom:3px;border-top: 1px dashed #bbb;background-color:#fff;}
")

(define-css 'modified-unit "
div.modified-unit { border:1px solid #888; margin-bottom:6px; margin-top:6px;background-color:#FFFFCC; }
div.modified-unit-name { padding-top:2px; padding-bottom:2px; padding-left:5px; padding-right:5px; color:#009; }
div.modified-unit-feature { padding-left:5px; padding-right:2px;padding-top:3px;padding-bottom:3px;border-top: 1px dashed #bbb;background-color:#fff;}
")

(define-css 'added-unit "
div.added-unit { border:1px solid #888; margin-bottom:6px; margin-top:6px;background-color:#C2E0D1; }
div.added-unit-name { padding-top:2px; padding-bottom:2px; padding-left:5px; padding-right:5px; color:#009; }
div.added-unit-feature { padding-left:5px; padding-right:2px;padding-top:3px;padding-bottom:3px;border-top: 1px dashed #bbb;background-color:#fff;}
")

(defmethod wi::html-pprint-list ((value list) (first-el (eql 'fcg:++)) &optional (append ""))
  "This method is used to print lists where the first element is (eql 'fcg:++)"
  (declare (ignore append))
  (let ((id-1 (make-id 'expansion-1)))
    `((span :class "table")
      ((div) 
       ,(make-expandable/collapsable-element 
         id-1 (make-id)
         ;; collapsed element
         `((div) 
           ((a ,@(make-expand/collapse-link-parameters id-1 t "show expansion"))
            "++")
           " " ,(wi::html-pprint-aux (second value)) " " ,(wi::html-pprint-aux (third value)) ")"
           )
         ;; expanded element
         `((div) 
           ((a ,@(make-expand/collapse-link-parameters id-1 nil "hide expansion"))
            "--")
           " " ,(wi::html-pprint-aux (fcg:fcg-expand (second value) :value (third value) :source nil)) ")" ))))))

;;feature-value->html has been modified to draw sub-features.
;;if the value of a feature is a list and this feature is allowed to have the sub-feature representation,
;; it will draw the sub-feature name and value.
;;otherwise, it will draw the feature-value without making the distinction between sub-feature name and value
(defun feature-value->html (value &key (is-tagged? nil) (configuration *default-visualization-configuration*))
  (if is-tagged?
    `((div)
      ((br))
      ((div :class "unit-feature-name") ,(html-pprint (car value) :max-width 40))
      ((tr))
      ((div :class "unit-feature-value") ,(html-pprint (car (cdr value)) :max-width 40)))
    (if (and (listp value)
             (listp (car value))
             (get-configuration configuration :subfeatures))
      `((div :class "unit-subfeature")
        ((br))
        ,@(loop for sub-feature in (<sorted-subfeatures> value)
                append
                (if (listp sub-feature)
                  (if (is-subfeature-in-subfeatures-to-show (<subfeature-name> sub-feature)
                                                            :configuration configuration)
                    ;;if subfeature values is a list of lists with one element, represent each element of the list
                    ;;of lists in a different row
                    (if (= (length (<subfeature-values> sub-feature)) 1)
                      (if (listp (car (<subfeature-values> sub-feature)))
                        `(((tr))
                          ((div :class "unit-subfeature-name")
                           ,(html-pprint (<subfeature-name> sub-feature) :max-width 40))
                          ((div :class "unit-subfeature-value")
                           ,@(loop for element in (car (<subfeature-values> sub-feature))
                                   collect 
                                   `,(html-pprint element :max-width 40))))
                        `(((tr))
                          ((div :class "unit-subfeature-name")
                           ,(html-pprint (<subfeature-name> sub-feature) :max-width 40))
                          ((div :class "unit-subfeature-value")
                           ,(html-pprint (car (<subfeature-values> sub-feature)) :max-width 40))))
                      `(((tr))
                        ((div :class "unit-feature-value")
                         ,(html-pprint sub-feature :max-width 40))
                        ))
                    `())
                  `(((div :class "unit-feature-value")
                     ,(html-pprint sub-feature :max-width 40))))))
      `((div :class "unit-feature-value")
        ,(html-pprint value :max-width 40)))))

(defun feature->html (feature &key (configuration *default-visualization-configuration*))
  "function that draws features. It calls the feature-value->html function"
  (if (<feature-value> feature)
    (let ((feature-name-string (format nil "~(~a~)&#160;&#160;" (<feature-name> feature)))
          (tag (<feature-tag> feature)))
      `((div :class "unit-feature")
        ,@(if tag
            (loop for variable in (cdr feature) by #'cddr
                  for value in (cddr feature) by #'cddr
                  for i from 1
                  collect `((span)
                            ((div :class "unit-feature-name")
                             ,(if (= i 1) 
                                `((span :class "tagged") 
                                  ,(format nil "~(~a~) " (first feature)))
                                "")
                             ((span :class "tagged") 
                              ,(format nil "~(~a~)&#160;" variable)))
                            ((span)
                             ,(feature-value->html
                               value :is-tagged? t
                               :configuration configuration))))
            `(((span)
               ((div :class "unit-feature-name") ,feature-name-string)
               ,(feature-value->html
                 (<feature-value> feature)
                 :configuration configuration))))))
    ""))

(defun unit->html (unit expand/collapse-all-id &key
                        (direction nil)
                        (modified-units nil)
                        (added-units nil)
                        (configuration *default-visualization-configuration*))
  "function to draw units in html"
  (let ((element-id (make-id 'unit))
	(unit-name-lcase (format nil "~(~a~)" (<unit-name> unit)))
	(unit-name-ucase (mkstr (<unit-name> unit)))
        ;;determine the style for this unit
        (unit-style (cond (direction "unit-with-direction")
                          ((find (<unit-name> unit) modified-units) "modified-unit")
                          ((find (<unit-name> unit) added-units) "added-unit")
                          (t "unit"))))
    `((div :class ,unit-style)
      ,(if (<features> unit)
         (make-expandable/collapsable-element 
          element-id expand/collapse-all-id
          `((div :class "unit-name")
            ((a ,@(make-expand/collapse-link-parameters element-id t "expand unit")
                :name ,unit-name-ucase) ,unit-name-lcase))
          (lambda ()
            `((span)
              ((div :class "unit-name")
               ((a ,@(make-expand/collapse-link-parameters element-id nil "collapse unit")
                   :name ,unit-name-ucase)
                ,unit-name-lcase))
              ,@(loop for feature in (<sorted-features> unit);;before <features>
                      collect (if (feature-to-hide-p feature :configuration configuration)
                                ""
                                (feature->html feature :configuration configuration))))))
         `((div :class "unit-name" :name ,unit-name-ucase) ,unit-name-lcase)))))

(defun unit-with-subunits->html (unit structure right-to-left expand/collapse-all-id
                                      &key (depth 1) (direction nil)
                                      (modified-units nil) (added-units nil)
                                      (configuration *default-visualization-configuration*))
  "returns a table with a unit, lines to its subunits and the subunits themselves"
  (if (> depth 20)
    `((div :style "color:#f00") "giving up")
    (draw-node-with-children 
     (unit->html unit expand/collapse-all-id
                 :direction direction
                 :modified-units modified-units
                 :added-units added-units
                 :configuration configuration)
     (loop for unit in (<subunits> unit structure :configuration configuration)
           collect (unit-with-subunits->html unit structure right-to-left expand/collapse-all-id
                                             :depth (+ depth 1)
                                             :direction direction
                                             :modified-units modified-units
                                             :added-units added-units
                                             :configuration configuration))
     :right-to-left right-to-left)))

;; #########################################################
;; coupled-feature-structure
;; ---------------------------------------------------------

(export '(*saved-cfs*))

(defvar *saved-cfs* nil)

(defvar *make-html-cfs-construction-inventory* nil
  "This variable will be bound to the construction set that was
   responsible for creating a cfs during html generation, needed
   for ajax interactivity")

(defun ajax-save-cfs-aux (cfs ci &optional node)
  (setf *saved-cfs* cfs)
  (add-element `((hr)))
  (add-element 
   `((p) "Saved structure"  ,(make-html *saved-cfs* :construction-inventory ci)
     ,@(when node `("of node&#160;&#160; " ,(make-html node :draw-children nil)))
     " to global variable " ((tt) ((b) "*saved-cfs*"))))
  (render-xml nil))
  
(ht-simple-ajax:defun-ajax save-cfs (id) (wi::*ajax-processor*)
  (let ((cfs.ci (get-wi-object id)))
    (ajax-save-cfs-aux (car cfs.ci) (cdr cfs.ci))))

(defun ajax-print-cfs-aux (cfs ci &optional node)
  (add-element `((hr)))
  (add-element 
   `((p) "Printing structure" ,(make-html cfs :construction-inventory ci)
     ,@(when node `("of node&#160;&#160; " ,(make-html node :draw-children nil)))
     "&#x2192;"))
  (add-element `((pre) ,(let (;;(*print-escape* nil)
                              (*print-right-margin* 100))
                          (cl-who:escape-string (format nil "~:w" cfs))))))
  
(ht-simple-ajax:defun-ajax print-cfs (id) (wi::*ajax-processor*)
  (let* ((cfs.ci (get-wi-object id)))
    (ajax-print-cfs-aux (car cfs.ci) (cdr cfs.ci))))

(defun ajax-render-cfs-aux (cfs ci &key (node nil)
                                (fcg-light-style nil))
  (let* ((render-mode (get-configuration ci :render-mode))
         (utterance (render cfs render-mode :node node)))
    (add-element `((hr)))
    (add-element 
     `((p) "Rendering structure" ,(if fcg-light-style
                                    (make-html-fcg-light cfs
                                                         :construction-inventory ci
                                                         :feature-types (feature-types (original-cxn-set ci)))
                                    (make-html cfs :construction-inventory ci))
       ,@(when node `("of node&#160;&#160; " ,(if fcg-light-style
                                                (make-html-fcg-light node :draw-children nil)
                                                (make-html node :draw-children nil))))
       "with mode " ((tt) ,(format nil "~(~a~)" render-mode)) " &#x2192; "
       "&quot;" ((b) ,(format nil "~{~a~^ ~}" utterance)) "&quot;"))
    (render-xml nil)))
  
(ht-simple-ajax:defun-ajax render-cfs (id fcg-light-style?) (wi::*ajax-processor*)
  (let* ((cfs.ci (get-wi-object id)))
    (ajax-render-cfs-aux (car cfs.ci) (cdr cfs.ci)
                         :fcg-light-style fcg-light-style?)))

(defun ajax-extract-meaning-cfs-aux (cfs ci &optional node)
  (let ((meaning (extract-meanings (left-pole-structure cfs))))
    (add-element `((hr)))
    (add-element 
     `((p) "Extracting meaning of structure" ,(make-html cfs :construction-inventory ci)
       ,@(when node `("of node&#160;&#160; " ,(make-html node :draw-children nil)))
       " &#x2192; " ,(html-pprint meaning)))
    (render-xml nil)))

(ht-simple-ajax:defun-ajax extract-meaning-cfs (id) (wi::*ajax-processor*)
  (let* ((cfs.ci (get-wi-object id)))
    (ajax-extract-meaning-cfs-aux (car cfs.ci) (cdr cfs.ci))))

(ht-simple-ajax:defun-ajax cfs-apply-all (id production?) (wi::*ajax-processor*)
  (let* ((cfs.ci (get-wi-object id))
         (cfs (car cfs.ci)) (ci (cdr cfs.ci))
         (direction (if (read-from-string production?) '-> '<-))
         (*make-html-cfs-construction-inventory* ci))
    (add-element '((hr)))
    (add-element `((p) "Applying all constructions of " 
                   ,(make-html ci) " to " ,(make-html cfs) " in direction " 
                   ,(if (eq direction '->) "&#x2192; " "&#x2190; ")))
    (loop with cars = nil
          for cxn in (constructions ci)
          do (multiple-value-bind (cars-1 cars-2)
                 (fcg-apply cxn cfs direction :notify nil
                            :configuration (configuration ci))
               (setf cars (nconc cars cars-1 cars-2)))
          finally 
          (if cars
            (loop for car in cars 
                  do (add-element '((hr)))
                  (add-element (make-html car)))
            (add-element '((p) "No construction could be applied")))))
  (render-xml nil))               

(defmethod make-html ((pole pole) &key (right-to-left nil) expand/collapse-all-id
                      (direction nil) (modified-units nil) (added-units nil)
                      (configuration *default-visualization-configuration*))
  `((div)
    ,@(loop for unit in (<level-0-units> (pole-structure pole))
            collect `((div) ,(unit-with-subunits->html unit (pole-structure pole)
                                                       right-to-left expand/collapse-all-id
                                                       :direction direction
                                                       :modified-units modified-units
                                                       :added-units added-units
                                                       :configuration configuration)))))

(define-css 'pole-connector "
div.pole-connector { height:32px;width:55px; }
div.pole-connector > div.domain { position:absolute;top:-4px;padding-left:4px; padding-right:4px;}
div.pole-connector > svg { position:absolute;top:11px; }
div.pole-connector > div.save-button { position:absolute;left:15px;top:20px;color:#006;display:none; }
div.pole-connector > span a { position:absolute;top:11px;left:10px;height:10px;width:35px; }
div.pole-connector > span a:hover { border-bottom:1px solid #006; }
")

(defun make-pole-connector (cfs expand/collapse-all-id-left
                                expand/collapse-all-id-right)
  `((div :style "position:relative;" :class "pole-connector")
    ((div :class "domain" :style "left:0px;") 
     ,(make-expandable/collapsable-element 
       (make-id 'left-pole) expand/collapse-all-id-left
       `((a ,@(make-expand/collapse-all-link-parameters 
               expand/collapse-all-id-left t "expand left pole"))
         ,(format nil "~(~a~)" (left-pole-domain cfs)))
       `((a ,@(make-expand/collapse-all-link-parameters 
               expand/collapse-all-id-left nil "collapse left pole"))
         ,(format nil "~(~a~)" (left-pole-domain cfs)))))
    ((div :class "domain" :style "right:0px;") 
     ,(make-expandable/collapsable-element 
       (make-id 'right-pole) expand/collapse-all-id-right
       `((a ,@(make-expand/collapse-all-link-parameters 
               expand/collapse-all-id-right t "expand right pole"))
         ,(format nil "~(~a~)" (right-pole-domain cfs)))
       `((a ,@(make-expand/collapse-all-link-parameters 
               expand/collapse-all-id-right nil "collapse right pole"))
         ,(format nil "~(~a~)" (right-pole-domain cfs)))))
    ((svg :xmlns "http://www.w3.org/2000/svg" :width "55" :height "10")
     ((line :stroke "#000060" :stroke-width "0.9px" 
            :x1 "2" :y1 "5" :x2 "53" :y2 "5"))
     ((polygon :points "0,5 8,0 8,10" :fill "#000060"))
     ((polygon :points "55,5 47,0 47,10" :fill "#000060")))
    ,(let ((element-id (make-id 'cfs)))
       (make-expandable/collapsable-element 
        element-id (make-id 'cfs)
        `((a :href ,(format nil "javascript:expandAll('~(~a~)');expandAll('~(~a~)');expand('~(~a~)');" expand/collapse-all-id-left expand/collapse-all-id-right element-id) :title "expand all"))
        `((a :href ,(format nil "javascript:collapseAll('~(~a~)');collapseAll('~(~a~)');collapse('~(~a~)');" expand/collapse-all-id-left expand/collapse-all-id-right element-id) :title "collapse all"))))))

(define-css 'cxn_cfs "
div.cxn_cfs { padding:0px; display:inline-block; margin-right: 7px; margin-bottom:3px;margin-top:3px;position:relative; }
div.cxn_cfs > div.title { color:#fff;padding-left:2px;padding-right:2px;padding-top:0px;padding-bottom:1px; }
div.cxn_cfs > div.title > a { color:#fff; font-size:9pt; font-weight:normal;}
div.cxn_cfs > table > tbody > tr > td.attributes { padding:4px; border-bottom:1px dashed #008;}
div.cxn_cfs table.cxn_cfs { border-collapse:collapse;margin-top:4px;margin-bottom:-2px; }
div.cxn_cfs table.cxn_cfs > tbody > tr > td { padding:0px; }
div.cxn_cfs table.cxn_cfs > tbody > tr:first-child > td { padding-top:3px;padding-bottom:2px;} 
div.cxn_cfs table.cxn_cfs > tbody > tr.j-units > td { border-top:1px dashed #008; padding-top:3px;padding-bottom:2px;}
div.cxn_cfs table.cxn_cfs > tbody > tr > td:first-child { padding-left:7px;}
div.cxn_cfs table.cxn_cfs > tbody > tr > td:last-child { padding-right:7px;}
div.cxn_cfs table.cxn_cfs > tbody > tr > td:first-child > div { float:right; }
div.cxn_cfs table.cxn_cfs > tbody > tr > td:first-child > table > tbody > tr > td { padding:0px; }
div.cxn_cfs table.cxn_cfs > tbody > tr > td:first-child > table > tbody > tr > td > div { float:right; }
")

(define-css 'cfs "
div.cfs {  border:1px solid #669999; }
div.cfs > div.title { border:1px solid #669999;background-color: #669999; }
")

(defun compare-units-after-cxn-applies (cxn-pole-result cxn-pole-source)
  "used to add color information in application result"
  (let ((unit-found nil))
    (loop for unit in cxn-pole-result do
          (progn
            (setf unit-found nil)
            (loop for result-unit in cxn-pole-source do
                  (when (equal-sets unit result-unit)
                    (setf unit-found t)
                    (return unit-found))))
          when (not unit-found)
          collect (car unit))))

(defun get-list-of-changes-at-transient-structure (source-cfs resulting-cfs)
  "this function compares the original and the resulting transient structures
and identifies the units that have been added or modified for left and right
poles, and also the units added or modified for the combined ts visualization"
  (let ((lp-source (left-pole-structure source-cfs))
        (lp-source-units (loop for unit in (left-pole-structure source-cfs) 
                               collect (car unit)))
        (rp-source (right-pole-structure source-cfs))
        (rp-source-units (loop for unit in (right-pole-structure source-cfs) 
                               collect (car unit)))
        (lp-result (left-pole-structure resulting-cfs))
        (rp-result (right-pole-structure resulting-cfs))
        changes-lp
        changes-rp
        (modified-units-lp '())
        (added-units-lp '())
        (modified-units-rp '())
        (added-units-rp '())
        (modified-units-combined '())
        (added-units-combined '()))
    (setf changes-lp (compare-units-after-cxn-applies lp-result lp-source))
    (setf changes-rp (compare-units-after-cxn-applies rp-result rp-source))
    (loop for unit in changes-lp do
          (if (find unit lp-source-units)
            (setf modified-units-lp (cons unit modified-units-lp))
            (setf added-units-lp (cons unit added-units-lp))))
    (loop for unit in changes-rp do
          (if (find unit rp-source-units)
            (setf modified-units-rp (cons unit modified-units-rp))
            (setf added-units-rp (cons unit added-units-rp))))
    (setf modified-units-combined (union modified-units-lp modified-units-rp))
    (setf added-units-combined (union added-units-lp added-units-rp))
    (list modified-units-lp added-units-lp modified-units-rp added-units-rp modified-units-combined added-units-combined)))
 
(defmethod make-html ((cfs coupled-feature-structure) 
                      &key (construction-inventory *make-html-cfs-construction-inventory*)
                      (expand/collapse-all-id (make-id 'cfs))
                      (configuration *default-visualization-configuration*)
                      (source-cfs nil))
  ;; replacing methods is bad, but right now it's difficult otherwise
  (let ((cfs-id (make-id 'cfs))
        changes-at-transient-structure)
    (when source-cfs
      (setf changes-at-transient-structure (get-list-of-changes-at-transient-structure source-cfs cfs)))
    (store-wi-object (cons cfs construction-inventory) cfs-id)
    (let* ((element-id-1 (make-id 'cfs))
           (expand/collapse-all-id-left (make-id 'cfs))
           (expand/collapse-all-id-right (make-id 'cfs))
           (expand/collapse-all-id-combined (make-id 'cfs))
           (outer-div (lambda (children)
                        (make-div-with-menu 
                         :menu-items 
                         `(((a :href ,(format nil "javascript:ajax_save_cfs('~a');" cfs-id)
                               :title "save cfs")
                            "&#x22ba;") 
                           ((a :href ,(format nil "javascript:ajax_print_cfs('~a');" cfs-id)
                               :title "print cfs as text")
                            "&#x22ee;")
                           ,@(when 
                                 construction-inventory
                               `(((a :href ,(format nil "javascript:ajax_render_cfs('~a','nil');" 
                                                    cfs-id)
                                     :title "render cfs")
                                  "&#x22b3;")  
                                 ((a :href ,(format nil 
                                                    "javascript:ajax_extract_meaning_cfs('~a');"
                                                    cfs-id)
                                     :title "extract meaning of cfs")
                                  "&#x22b2;")
                                 ((a :href ,(format nil 
                                                    "javascript:ajax_cfs_apply_all('~a','t');"
                                                    cfs-id)
                                     :title "apply all constructions in production")
                                  "&#x226b;")
                                 ((a :href ,(format nil 
                                                    "javascript:ajax_cfs_apply_all('~a','nil');"
                                                    cfs-id)
                                     :title "apply all constructions in parsing")
                                  "&#x226a;"))))
                         :div-children children)))
           (coupled-version 
            (lambda ()
              (funcall
               outer-div
               `(((div :class "cxn_cfs cfs")
                  ((div :class "title") 
                   ((a ,@(make-expand/collapse-link-parameters element-id-1 t "combine cfs"))
                    ((span) "transient structure")))
                  ((table :class "cxn_cfs cfs")
                   ((tbody)
                    ((tr)
                     ((td)
                      ,(if source-cfs
                         (make-html (left-pole cfs) :right-to-left t 
                                    :expand/collapse-all-id expand/collapse-all-id-left
                                    :modified-units (car changes-at-transient-structure)
                                    :added-units (car (cdr changes-at-transient-structure)))
                         (make-html (left-pole cfs) :right-to-left t 
                                    :expand/collapse-all-id expand/collapse-all-id-left)))
                     ((td) 
                      ,(make-pole-connector 
                        cfs expand/collapse-all-id-left expand/collapse-all-id-right))
                     ((td)
                      ,(if source-cfs
                         (make-html (right-pole cfs) 
                                    :expand/collapse-all-id expand/collapse-all-id-right
                                    :modified-units (car (cddr changes-at-transient-structure))
                                    :added-units (car (cdddr changes-at-transient-structure))
                                    :configuration configuration)
                         (make-html (right-pole cfs) 
                                    :expand/collapse-all-id expand/collapse-all-id-right
                                    :configuration configuration)))))))))))
           (combined-version 
            (lambda ()
              (funcall
               outer-div
               `(((div :class "cxn_cfs cfs")
                  ((div :class "title") 
                   ((a ,@(make-expand/collapse-link-parameters element-id-1 nil "decouple cfs")) 
                    ((span) "combined transient structure")))
                  ((table :class "cxn_cfs cfs")
                   ((tbody)
                    ((tr)
                     ((td)
                      ((div :class "domain" :style "left:0px;") 
                       ,(make-expandable/collapsable-element 
                         (make-id 'pole) expand/collapse-all-id-combined
                         `((a ,@(make-expand/collapse-all-link-parameters 
                                 expand/collapse-all-id-combined t "expand"))
                           ,(format nil "~(~a~)" "expand"))
                         `((a ,@(make-expand/collapse-all-link-parameters 
                                 expand/collapse-all-id-combined nil "collapse"))
                           ,(format nil "~(~a~)" "collapse")))))
                     ((td)
                      ,(if source-cfs
                         (make-html (combined-pole cfs) 
                                    :expand/collapse-all-id expand/collapse-all-id-combined
                                    :modified-units (car (cddddr changes-at-transient-structure))
                                    :added-units (car (last changes-at-transient-structure))
                                    :configuration configuration)
                         (make-html (combined-pole cfs) 
                                    :expand/collapse-all-id expand/collapse-all-id-combined
                                    :configuration configuration))))))))))))
      (if (get-configuration configuration :coupled-mode)
        ;;if we select to show the coupled-version first
        (make-expandable/collapsable-element 
         element-id-1 expand/collapse-all-id
         coupled-version combined-version
         :expand-initially nil)
        ;;if we select to show the combined-version first
        (make-expandable/collapsable-element 
         element-id-1 expand/collapse-all-id
         combined-version coupled-version
         :expand-initially nil)))))

;; #########################################################
;; j-unit
;; ---------------------------------------------------------

(define-css 'j-unit "
div.j-unit { padding:0px; margin-bottom:6px; margin-top:6px; }
div.j-unit div.existing { border:1px dotted #888; background-color:#ddd;}
div.j-unit div.new { border:1px solid #888;}
div.j-unit div.unit-name { padding-top:2px; padding-bottom:2px; padding-left:5px; padding-right:5px; color:#444; }
div.j-unit div.tag-ref { padding-left:5px; padding-right:2px;padding-top:3px;padding-bottom:3px;border-top: 1px dashed #bbb;color:#700;white-space:nowrap;}
")

(defun j-unit->html (j-unit units &key (right-to-left nil) 
                            (expand/collapse-all-id (make-id 'j-unit))
                            (configuration *default-visualization-configuration*))
  (let* ((unit-name (second (car j-unit)))
	 (unit-name-string (format nil "~(~a~)" unit-name))
	 (parent (third (car j-unit)))
	 (children (fourth (car j-unit)))
	 (element-id (make-id 'j-unit))
	 (unit-box 
	  `((div :class "j-unit")
	    ((div :class ,(if (find unit-name units :key #'<unit-name>) "existing" "new"))
	     ,(if (cdr j-unit)
                (make-expandable/collapsable-element 
                 element-id expand/collapse-all-id 
                 `((div :class "unit-name")
                   ((a ,@(make-expand/collapse-link-parameters element-id t "expand unit")
                       :name ,(mkstr unit-name)),unit-name-string))
                 (lambda ()
                   `((span)
                     ((div :class "unit-name")
                      ((a ,@(make-expand/collapse-link-parameters element-id nil "collapse unit")
                          :name ,(mkstr unit-name)) ,unit-name-string))
                     ,@(loop for x in (cdr j-unit)
                             if (symbolp x)
                             collect `((div :class "tag-ref") ,(format nil "&#x2192; ~(~a~)" x))
                             else collect (if (feature-to-hide-p x :configuration configuration)
                                            ""
                                            (feature->html x :configuration configuration))))))
                `((div :class "unit-name") 
                  ((span :name ,(mkstr unit-name)) ,unit-name-string))))))
	 (unit-with-children
	  (draw-node-with-children 
	   unit-box 
	   (loop for child in children
                 collect `((div :class "j-unit")
                           ((div :class "existing")
                            ((div :class "unit-name")
                             ((span :name ,(mkstr child))
                              ,(format nil "~(~a~)" child))))))
	   :right-to-left right-to-left :style "dotted")))
    (if parent
      (draw-node-with-children 
       `((div :class "j-unit")
         ((div :class "existing")
          ((div :class "unit-name")
           ((span :name ,(mkstr parent))
            ,(format nil "~(~a~)" parent)))))
       (list unit-with-children)
       :right-to-left right-to-left :style "dotted")
      unit-with-children)))

;; #########################################################
;; construction
;; ---------------------------------------------------------

(defvar *saved-cxn* nil)

(export '*saved-cxn*)

(ht-simple-ajax:defun-ajax save-construction (id) (wi::*ajax-processor*)
  (setf *saved-cxn* (get-wi-object id))
  (add-element `((hr)))
  (add-element `((p) "Saved construction&#160;&#160; " 
                 ,(make-html *saved-cxn* :expand-initially nil)
		 " to global variable " ((tt) ((b) "*saved-cxn*"))))
  (render-xml nil))

(ht-simple-ajax:defun-ajax print-construction (id) (wi::*ajax-processor*)
  (let ((construction (get-wi-object id)))
    (add-element `((hr)))
    (add-element 
     `((p) "Printing construction&#160;&#160; " 
       ,(make-html construction :expand-initially nil) "&#x2192;"))
    (add-element 
     `((pre) ,(let ((*print-right-margin* 100))
                (cl-who:escape-string (format nil "~:w" construction)))))))

(defun add-cxn-and-print-as-list (cxn)
  (let* ((toggle? (if wi::*static-html* t))
         (wi::*static-html* nil)
         (lst (make-html cxn))
         (str (third (first (third (second (second lst))))))
         (print-id (subseq str 36 (- (length str) 3))))
    (let ((wi::*static-html* (if toggle? t)))
      (print-construction print-id))))

(defun ajax-apply-construction-aux (construction production?)
  (if (not (and *saved-cfs* (typep *saved-cfs* 'coupled-feature-structure)))
    (progn
      (add-element '((hr)))
      (add-element '((p) "Variable " ((tt) ((b) "*saved-cfs*")) 
                     " is not bound to a coupled feature structure")))
    (progn
      (add-element '((hr)))
      (add-element `((p) ((tt) ((b) "*saved-cfs*")) ":" ,(make-html *saved-cfs*)))
      (fcg-apply construction *saved-cfs* 
                 (if (read-from-string production?) '-> '<-)
                 :configuration (if (get-root (left-pole-structure *saved-cfs*))
                                  (let ((config (make-instance 'configuration)))
                                    (set-configuration config :root-mode t))
                                  nil))))
  (render-xml nil))

(ht-simple-ajax:defun-ajax apply-construction (id production?) (wi::*ajax-processor*)
  (let ((construction (get-wi-object id)))
    (ajax-apply-construction-aux construction production?)))

(ht-simple-ajax:defun-ajax merge-construction (id production?) (wi::*ajax-processor*)
  (let* ((construction (copy-object (get-wi-object id))))
    (setf (attr-val construction :merge-always) t)
    (setf (name construction) (symb 'temp- (name construction)))
    (add-element '((hr)))
    (add-element `((p) "Created temporary construction " 
                   ,(make-html construction :expand-initially nil)
                   " with attribute " ((tt) "(:merge-always . t)")))
    (ajax-apply-construction-aux construction production?)))

(define-css 'construction "
div.construction {  border:1px solid #008; }
div.construction > div.title { border:1px solid #008; background-color: #008;}
")

(defun cxn-structure->html (construction expand/collapse-all-id-left
                                         expand/collapse-all-id-right &key (direction nil)
                                         (configuration *default-visualization-configuration*))
  `((table :class "cxn_cfs construction")
    ((tbody)
     ((tr)
      ((td)
       ((div)
	,(make-html (left-pole construction)
                    :right-to-left t
		    :expand/collapse-all-id expand/collapse-all-id-left
                    :direction (if (eq direction '->) direction nil)
                    :configuration configuration)))
      ((td) 
       ,(make-pole-connector construction expand/collapse-all-id-left 
			     expand/collapse-all-id-right))
      ((td) 
       ,(make-html (right-pole construction) 
		   :expand/collapse-all-id expand/collapse-all-id-right
                   :direction (if (eq direction '<-) direction nil)
                   :configuration configuration)))
     ,(if (or (<j-units> (left-pole-structure construction))
	      (<j-units> (right-pole-structure construction)))
        `((tr :class "j-units")
          ((td)
           ,@(loop for j-unit in (<j-units> (left-pole-structure construction))
                   collect 
                   `((tr)
                     ((td)
                      ,(j-unit->html 
                        j-unit (<units> (left-pole-structure construction))
                        :expand/collapse-all-id expand/collapse-all-id-left
                        :right-to-left t
                        :configuration configuration)))))
          ((td))
          ((td)
           ,@(loop for j-unit in (<j-units> (right-pole-structure construction))
                   collect (j-unit->html 
                            j-unit (<units> (right-pole-structure construction))
                            :expand/collapse-all-id expand/collapse-all-id-right
                            :configuration configuration))))
        ""))))

(defgeneric make-html-construction-title (construction)
  (:documentation "returns some html for displaying the title of a construction"))

(defmethod make-html-construction-title ((construction construction))
  `((span) 
    ,(format nil "~(~a~)" (name construction)) 
    ,@(when (attributes construction)
        `(" "
          ((span :style "white-space:nowrap")
           ,(format nil "(~{~(~a~)~^ ~})" 
                    (loop for x in (attributes construction)
                          when (cdr x)
                          if (floatp (cdr x))
                          collect (format nil "~,2f" (cdr x))
                          else collect (mkstr (cdr x)))))))))

(defmethod make-html ((construction construction) &key (expand-initially t)
		      (expand/collapse-all-id (make-id 'construction))
                      (direction nil)
                      (configuration *default-visualization-configuration*))
  (let* ((element-id-1 (make-id 'construction))
	 (element-id-2 (make-id 'construction))
	 (expand/collapse-all-id-left (make-id 'left-pole))
	 (expand/collapse-all-id-right (make-id 'right-pole))
	 (construction-title (make-html-construction-title construction))
         (construction-id (make-id (name construction)))
         (outer-div
          (lambda (children)
            (make-div-with-menu 
             :div-attributes '(:class "cxn_cfs construction")
             :menu-items 
             `(((a :href ,(format nil "javascript:ajax_save_construction('~a');" 
                                  construction-id)
                   :title "save construction")
                "&#x22a4;") 
               ((a :href ,(format nil "javascript:ajax_print_construction('~a');" 
                                  construction-id)
                   :title "print construction as text")
                "&#x22ee;") 
               ((a :href 
                   ,(format 
                     nil "javascript:ajax_apply_construction('~a','t');" 
                     construction-id)
                   :title "apply to *saved-cfs* in production")
                "&#x22a2;") 
               ((a :href 
                   ,(format 
                     nil "javascript:ajax_apply_construction('~a','nil');" 
                     construction-id)
                   :title "apply to *saved-cfs* in parsing")
                "&#x22a3;") 
               ((a :href 
                   ,(format 
                     nil "javascript:ajax_merge_construction('~a','t');" 
                     construction-id)
                   :title "merge with *saved-cfs* in production")
                "&#x22cc;") 
               ((a :href 
                   ,(format 
                     nil "javascript:ajax_merge_construction('~a','nil');"
                     construction-id)
                   :title "merge with *saved-cfs* in parsing")
                "&#x22cb;"))
             :div-children children)))
	 (collapsed-version 
	  (lambda ()
	    (funcall 
             outer-div 
             `(((div :class "title")
                ((a ,@(make-expand/collapse-link-parameters 
                       element-id-1 t "expand construction"))
                 ,construction-title))))))
	 (expanded-version 
	  (lambda ()
	    (funcall
             outer-div
             `(((div :class "title") 
                ((a ,@(make-expand/collapse-link-parameters 
                       element-id-1 nil "collapse construction"))
                 ,construction-title)
                ,(if (attributes construction)
                   `((a ,@(make-expand/collapse-link-parameters 
                           element-id-2 t "show attributes"))
                     " show attributes")
                   ""))
               ,(cxn-structure->html 
                 construction expand/collapse-all-id-left 
                 expand/collapse-all-id-right :direction direction
                 :configuration configuration)))))
	 (expanded-version-with-attributes
	  (when (attributes construction)
	    (lambda ()
              (funcall 
               outer-div
               `(((div :class "title") 
                  ((a ,@(make-expand/collapse-link-parameters 
                         element-id-1 nil "collapse construction"))
                   ,construction-title))
                 ((table :style "border-collapse:collapse")
                  ((tbody)
                   ((tr)
                    ((td :class "attributes")
                     ,(html-pprint (attributes construction))))
                   ((tr)
                    ,(cxn-structure->html 
                      construction expand/collapse-all-id-left 
                      expand/collapse-all-id-right :direction direction
                      :configuration configuration))))))))))
    (store-wi-object construction construction-id)
    (make-expandable/collapsable-element 
     element-id-1 expand/collapse-all-id
     collapsed-version
     (if (attributes construction)
       (make-expandable/collapsable-element 
        element-id-2 expand/collapse-all-id
        expanded-version
        expanded-version-with-attributes)
       expanded-version)
     :expand-initially expand-initially)))

;; #########################################################
;; cxn-application-result
;; ---------------------------------------------------------

(defparameter *status-colors*
  ;; A list of mappings from various fcg statuses to colors. Use
  ;; pushnew to get new statuses in here
  '(((initial) . "#444;")
    ((cxn-matched) . "#480;")
    ((cxn-applied succeeded expanded-sequentially) . "#050;")
    ((cxn-applied goal-test-failed) . "#339966;")
    ((restart problem-found) .  "#8028E0;")
    ((first-merge-failed second-merge-failed merge-without-match-failed failed)
     . "#703;")
    ((bad-hierarchy-after-first-merge bad-hierarchy-after-second-merge invalid-boundaries non-conform) . "#c0c;")
    ((structure-not-changed) . "#80f;")
    ((cxn-applied-without-match) . "#046;")
    ((duplicate) . "#520;")
    ((unconnected-before-morphs dangling-dependencies) . "#500;")
    ((rank-upgrade) . "#444;")
    ((max-search-depth-reached max-expansions-reached max-nr-of-nodes-reached) . "#888")
    ((diagnosed-problem) . "#eb9500;")
    ((repaired-problem) . "#D0ABDB;")
    ((reused-from-chart) . "#e580ff;")
    ((added-by-repair) . "#999900;")
    ((diagnostic-triggered) . "#E65C00;")))

(defun status-color (status)
  (or (cdr (assoc status *status-colors* :test #'member))
      "#000000"))
      ;(error "no status color defined for status ~a" status)))

(define-css 'car "
table.car { border-collapse:collapse; }
table.car > tbody > tr > td { vertical-align:top;padding-bottom:10px; }
table.car > tbody > tr > td:first-child { padding-right:15px; }
 ")

(defmethod make-html ((car cxn-application-result) &key
                      (configuration *default-visualization-configuration*))
  `((table :class "car")
    ((tbody)
     ((tr)
      ((td) "status")
      ((td :style ,(format nil "color:~a" (status-color (car-status car))))
       ((tt) ,(format nil "~(~a~)" (car-status car)))))
     ,(if (not (eq (car-status car) 'initial))
        `((tr)
          ((td) "source structure")
          ((td) ,(make-html (car-source-cfs car) :configuration configuration)))
        "")
     ,(if (car-applied-cxn car) 
        `((tr)
          ((td) "applied construction")
          ((td) ,(make-html (car-applied-cxn car) :direction (car-direction car) :expand-initially t :wrap-in-paragraph nil
                            :configuration configuration)))
        "")
     ,(if (and (not (member (car-status car) '(cxn-applied cxn-matched)))
	       (car-match-bindings car))
        `((tr) 
          ((td) "bindings after matching")
          ((td) ,(html-pprint (car-match-bindings car))))
        "")
     ,(if (and (not (member (car-status car) '(cxn-applied cxn-matched)))
	       (car-first-merge-structure car))
        (let ((expand/collapse-all-id (make-id 'fs)))
          `((tr)
            ((td)
             ,(make-expandable/collapsable-element 
               (make-id 'first-merge) expand/collapse-all-id 
               `((a ,@(make-expand/collapse-all-link-parameters expand/collapse-all-id t))
                 "structure after first merge")
               `((a ,@(make-expand/collapse-all-link-parameters expand/collapse-all-id nil))
                 "structure after first merge")))
            ((td) 
             ((div :style "float:left;")
              ,(make-html 
                (make-instance 'pole :structure (car-first-merge-structure car))
                :configuration configuration
                :expand/collapse-all-id expand/collapse-all-id
                :right-to-left (eq (pole-domain 
                                    (merging-pole (car-applied-cxn car) (car-direction car)))
                                   'sem))))))
        "")
     ,(if (eq (car-status car) 'bad-hierarchy-after-first-merge)
        `((tr)
          ((td) ((b) "bad structure after first merge"))
          ((td) ,(html-pprint (car-first-merge-structure car) :max-width 60)))
        "")
     ,(if (and (not (member (car-status car) '(cxn-applied cxn-matched)))
	       (car-first-merge-bindings car))
        `((tr)
          ((td) "bindings after first merge")
          ((td) ,(html-pprint (car-first-merge-bindings car))))
        "")
     ,(if (and (not (member (car-status car) '(cxn-applied cxn-matched)))
	       (car-second-merge-structure car))
        (let ((expand/collapse-all-id (make-id 'fs)))
          `((tr)
            ((td) 
             ,(make-expandable/collapsable-element 
               (make-id 'second-merge) expand/collapse-all-id 
               `((a ,@(make-expand/collapse-all-link-parameters expand/collapse-all-id t))
                 "structure after second merge")
               `((a ,@(make-expand/collapse-all-link-parameters expand/collapse-all-id nil))
                 "structure after second merge")))
            ((td)
             ((div :style "float:left;")
              ,(make-html 
                (make-instance 'pole :structure (car-second-merge-structure car))
                :configuration configuration
                :expand/collapse-all-id expand/collapse-all-id
                :right-to-left  (eq (pole-domain 
                                     (merging-pole (car-applied-cxn car) 
                                                   (opposite (car-direction car))))
                                    'sem))))))
        "")
     ,(if (eq (car-status car) 'bad-hierarchy-after-second-merge)
        `((tr)
          ((td) ((b) "bad structure after second merge"))
          ((td) ,(html-pprint (car-second-merge-structure car) :max-width 60)))
        "")
     ,(if (car-resulting-cfs car)
        `((tr)
          ((td) "resulting structure")
          ((td) ,(make-html (car-resulting-cfs car) :configuration configuration
                            :source-cfs (car-source-cfs car))))
        "")
     ,(if (car-second-merge-bindings car)
        `((tr)
          ((td) "resulting bindings")
          ((td) ,(html-pprint (car-second-merge-bindings car))))
        "")
     ,(if (and (car-first-merge-added car) (not (car-resulting-cfs car)))
        `((tr)
          ((td) "added in first merge")
          ((td) ,(make-html (make-instance 'pole :structure (car-first-merge-added car))
                            :configuration configuration)))
        "")
     ,(if (and (car-second-merge-added car) (not (car-resulting-cfs car)))
        `((tr)
          ((td) "added in second merge")
          ((td) ,(make-html (make-instance 'pole :structure (car-second-merge-added car))
                            :configuration configuration))) 
        ""))))

;; #########################################################
;; construction-inventory
;; ---------------------------------------------------------

(defparameter *construction-inventory-title-strings* 
  '((construction-set . "construction set")
    (construction-network . "construction-network")
    (hashed-construction-set . "hashed construction set")
    (scored-hashed-construction-set . "scored hashed construction set")))

(defun get-construction-inventory-title-string (construction-inventory)
  (substitute #\space #\-
              (symbol-name (type-of construction-inventory))))

(defgeneric make-html-construction-inventory-title (construction-inventory &key)
  (:documentation "returns some html for displaying the title of a construction set"))

(defmethod make-html-construction-inventory-title ((ci construction-inventory) &key &allow-other-keys)
  `((span) ,(get-construction-inventory-title-string ci)
    ,(format nil " (~a)" (size ci))))

(defgeneric make-html-construction-inventory-body (cs &key)
  (:documentation "returns some html for the body of a construction set"))

(defmethod make-html-construction-inventory-body ((ci construction-inventory) &key (configuration nil))
  (let ((configuration (or configuration (visualization-configuration ci))))
    (html-hide-rest-of-long-list 
     (copy-list (constructions ci)) 50
     #'(lambda (construction)
         (make-html construction
                    :expand-initially nil
                    :wrap-in-paragraph nil
                    :configuration configuration)))))

(define-css 'construction-inventory "
div.construction-inventory { padding:0px; display:inline-block; margin:3px; }
div.construction-inventory > div.title a { color:#40241A; }
div.construction-inventory-expanded { border:1px solid #40241A; }
div.construction-inventory-expanded > div.title { padding-left:5px;padding-right:5px;padding-top:1px;padding-bottom:2px; }
div.construction-inventory > table > tbody > tr { border-top:1px dashed #40241A;}
div.construction-inventory > table > tbody > tr > td { padding-left:5px; padding-bottom:2px;padding-top:2px;}
")

(defmethod make-html ((ci construction-inventory) &key (expand-initially nil) (configuration nil))
  (let* ((configuration (or configuration (visualization-configuration ci)))
         (element-id-1 (make-id 'construction-inventory))
	 (element-id-2 (make-id 'construction-inventory))
	 (title (make-html-construction-inventory-title ci))
	 (collapsed-version 
	  (lambda ()
	    `((div :class "construction-inventory")
	      ((div :class "title") 
	       ((a ,@(make-expand/collapse-link-parameters 
		      element-id-1 t "show constructions")) 
		,title)))))
	 (expanded-version-1
	  (lambda ()
	    `((div :class "construction-inventory construction-inventory-expanded")
	      ((div :class "title") 
	       ((a ,@(make-expand/collapse-link-parameters 
                      element-id-2 t "show more details"))
		,title))
              ((table :class "two-col")
               ((tbody)
                ((tr) 
                 ((td :colspan "2") ,@(make-html-construction-inventory-body ci
                                                                             :configuration configuration))))))))
	 (expanded-version-2
          (lambda ()
            `((div :class "construction-inventory construction-inventory-expanded")
	      ((div :class "title") 
               ((a ,@(make-expand/collapse-link-parameters 
                      element-id-1 nil "hide details"))
                ,title))
              ((table :class "two-col")
               ((tbody)
                ((tr) 
                 ((td :colspan "2") ,@(make-html-construction-inventory-body ci
                                                                             :configuration configuration)))
                ((tr) 
                 ((td) "configuration")
                 ((td) ,(html-pprint (entries (configuration ci)))))))))))
    (make-expandable/collapsable-element 
     element-id-1 (make-id 'cs) collapsed-version
     (make-expandable/collapsable-element 
      element-id-2 (make-id 'cs) expanded-version-1 expanded-version-2)
     :expand-initially expand-initially)))

;; #########################################################
;; construction-inventory-processor
;; ---------------------------------------------------------

(defmethod make-html ((cip construction-inventory-processor) 
                      &key (solutions nil solutions-provided-p)
                      (show-queue nil)
                      (configuration nil))
  (let ((configuration (or configuration (visualization-configuration (construction-inventory cip))))
        (*make-html-cfs-construction-inventory* (construction-inventory cip))
        (info-struct-id-name (make-id 'info-struct)))
    (reset-expanded-node)
    (if (get-configuration configuration :expand-nodes-in-search-tree)
      `((div) 
        ,@(when solutions-provided-p
            `(((h4) ,(case (length solutions)
                       (0 "No solution found")
                       (1 "Found a solution")
                       (t (format nil "Found ~a solutions"
                                  (length solutions)))))))
        ((table :class "two-col")
         ((tbody)
          ((tr)
           ((td) "initial structure")
           ((td) ,(make-html (initial-cfs cip)
                             :configuration configuration)))
          ,(make-tr-for-cip-tree (top-node cip) "application process"
                                 :configuration configuration)
          ,(if (and show-queue (queue cip))
             (make-tr-for-cip-queue cip "queue"
                                    :configuration configuration)
             "")
          ,@(loop for solution in solutions 
                  for n from 1
                  append 
                  `(,(if (> (length solutions) 1)
                       `((tr) ((td :colspan "2") ((b) "solution " ,n))) "")
                    ((tr)
                     ((td) "applied constructions")
                     ((td) ,@(html-hide-rest-of-long-list 
                              (applied-constructions solution) 10
                              #'(lambda (construction) 
                                  (make-html construction :expand-initially nil
                                             :configuration configuration
                                             :wrap-in-paragraph nil)))))
                    ((tr)
                     ((td) "resulting structure")
                     ((td) ,(make-html (car-resulting-cfs (cipn-car solution))
                                       :configuration configuration))))))))
      `((div) 
        ,@(when solutions-provided-p
            `(((h4) ,(case (length solutions)
                       (0 "No solution found")
                       (1 "Found a solution")
                       (t (format nil "Found ~a solutions"
                                  (length solutions)))))))
        ((table :class "two-col")
         ((tbody)
          ((tr)
           ((td) "application process")
           ((td) ,(make-html (top-node cip) :struct-id info-struct-id-name
                             :configuration configuration)))
          ((tr)
           ((td) "selected node")
           ((td) ,(get-replacing-content info-struct-id-name (get-current-cipn *last-cipn*)
                                         :configuration configuration)))
          ,(if (and show-queue (queue cip)) (make-tr-for-cip-queue cip "queue") "")))))))

(defun get-replacing-content (struct-id-name cipn &key (tree-id (make-id 'cipn)) 
                                             (configuration nil))
  "this function returns the content of a node, and it's used to replace the selected node in the compacted visualization"
  (let* ((node-id (symb tree-id '- (created-at cipn)))
         (cip (cip cipn))
         (construction-inventory (construction-inventory cip))
         (configuration (or configuration (visualization-configuration construction-inventory)))
         (with-search-debug-data (get-configuration configuration :with-search-debug-data))
         (node-color (cipn-node-color cipn cip))
         (status-tr
          `((tr) 
            ((td ,@(when (duplicate cipn)
                     `(:onMouseOver 
                       ,(mkstr "var n = document.getElementById('" 
                               tree-id '- (created-at (duplicate cipn))
                               "'); if (n) n.style.border= '3px solid #dd5';")
                       :onMouseOut 
                       ,(mkstr "var n = document.getElementById('" 
                               tree-id '- (created-at (duplicate cipn))
                               "'); if (n) n.style.border = 'none';"))))
             ,@(loop for status on (statuses cipn)
                     collect `((span :style ,(mkstr "color:" (status-color 
                                                              (car status))))
                               ,(format nil "~(~a~)" (car status))
                               ,@(when (cdr status) '(", ")))))))
         (td-with-background-style 
          `(:style ,(mkstr "background-color:" node-color ";color:#fff;")))
         (title-div
          (lambda (link-parameters)
            `((div :class "cipn-title" 
                   :style ,(mkstr "background-color:"  node-color ";"))
              ((a ,@link-parameters)
               ,(if with-search-debug-data
                  (format nil "~a,&#160;~,2f: " (created-at cipn) (priority cipn)) 
                  "")
               ,(make-html-cipn-title cipn construction-inventory)))))
         ;;
         ;;Generates the menu that appears above the constructions (void)
         (outer-div-with-menu
          (lambda (div-children)
            `((div :class "cipn" :id ,(mkstr node-id))
              ,(make-div-with-menu 
                :div-attributes `(:style ,(mkstr "border:1px solid " node-color))
                :menu-items 
                `()
                :div-children div-children))))
         (collapsed
          (lambda ()
            (funcall 
             outer-div-with-menu 
             (list 
              (funcall 
               title-div 
               (make-expand/collapse-link-parameters struct-id-name t
                                                     "show details from new visualization"))
              `((table)
                ((tbody)
                 ,status-tr
                 ,@(when (car-resulting-cfs (cipn-car cipn))
                     (let ((*make-html-cfs-construction-inventory* 
                            construction-inventory))
                       `(((tr) 
                          ((td)
                           ,(make-html (car-resulting-cfs 
                                        (cipn-car cipn))
                                       :configuration configuration))))))))))))
         (extended
          (lambda ()
            (funcall 
             outer-div-with-menu 
             (list 
              (funcall 
               title-div 
               (make-expand/collapse-link-parameters struct-id-name nil
                                                     "hide details from new visualization"))
              (let ((*make-html-cfs-construction-inventory* construction-inventory))
                `((table)
                  ((tbody)
                   ,status-tr
                   ((tr) 
                    ((td ,@td-with-background-style) "application result"))
                   ((tr) ((td) ,(make-html (cipn-car cipn)
                                           :configuration configuration)))
                   ((tr)
                    ((td ,@td-with-background-style) 
                     "cxn supplier "
                     ((tt)
                      ,(format nil "~(~:w~)" 
                               (get-configuration construction-inventory
                                                  :cxn-supplier-mode)))))
                   ((tr) ((td) ,(make-html (cxn-supplier cipn)
                                           :configuration configuration)))
                   ,@(when 
                         (fields (goal-test-data cipn))
                       `(((tr)
                          ((td ,@td-with-background-style)
                           ,(format nil "goal tests: ~{~(~a~)~^, ~}"
                                    (get-configuration 
                                     construction-inventory
                                     (if (eq (direction cip) '->)
                                       'production-goal-tests
                                       'parse-goal-tests)))))
                         ((tr) 
                          ((td) 
                           ,(make-html (goal-test-data cipn)
                                       :configuration configuration)))))
                   ,@(when (get-configuration cipn :show-meaning/utterance)
                       (if (eq (direction (cip cipn)) '<-)
                         `(((tr)
                            ((td ,@td-with-background-style)
                             "meaning"))
                           ((tr) 
                            ((td) 
                             ,(let* ((meaning 
                                      (extract-meanings (left-pole-structure 
                                                         (car-resulting-cfs
                                                          (cipn-car cipn))))))
                                (if (get-configuration cipn :draw-meaning-as-network)
                                  (if (get-configuration configuration :show-wiki-links-in-predicate-networks)
                                    (predicate-network-with-wiki-links->svg meaning)
                                    (predicate-network->svg meaning))
                                  (make-html meaning))))))
                         `(((tr)
                            ((td ,@td-with-background-style)
                             "utterance"))
                           ((tr) 
                            ((td) 
                             ,(let* ((utterance
                                      (render
                                       (car-resulting-cfs (cipn-car cipn))
                                       (get-configuration (cip cipn)
                                                          :render-mode))))
                                (make-html utterance))))))))))))))
         (div
          (lambda ()
            (make-expandable/collapsable-element
             struct-id-name (make-id)
             ;; show some details
             (funcall collapsed)
             ;; show all details
             (funcall extended)))))
    (funcall div)))

;; #########################################################
;; cip-node
;; ---------------------------------------------------------

(defvar *saved-cipn* nil)

(export '*saved-cipn*)

(ht-simple-ajax:defun-ajax save-cipn (id) (wi::*ajax-processor*)
  (setf *saved-cipn* (get-wi-object id))
  (add-element `((hr)))
  (add-element `((p) "Saved node&#160;&#160; " 
                 ,(make-html *saved-cipn* :draw-children nil)
		 " to global variable " ((tt) ((b) "*saved-cipn*"))))
  (render-xml nil))

(ht-simple-ajax:defun-ajax save-cipn-cfs (id) (wi::*ajax-processor*)
  (let ((cipn (get-wi-object id)))
    (ajax-save-cfs-aux 
     (car-resulting-cfs (cipn-car cipn)) (construction-inventory (cip cipn)) cipn)))

(ht-simple-ajax:defun-ajax print-cipn-cfs (id) (wi::*ajax-processor*)
  (let ((cipn (get-wi-object id)))
    (ajax-print-cfs-aux 
     (car-resulting-cfs (cipn-car cipn)) (construction-inventory (cip cipn)) cipn)))

(ht-simple-ajax:defun-ajax render-cipn (id fcg-light-style?) (wi::*ajax-processor*)
  (let ((cipn (get-wi-object id)))
    (ajax-render-cfs-aux 
     (car-resulting-cfs (cipn-car cipn)) (construction-inventory (cip cipn)) :node cipn :fcg-light-style fcg-light-style?)))

(ht-simple-ajax:defun-ajax extract-meaning-cipn (id) (wi::*ajax-processor*)
  (let ((cipn (get-wi-object id)))
    (ajax-extract-meaning-cfs-aux 
     (car-resulting-cfs (cipn-car cipn)) (construction-inventory (cip cipn)) cipn)))

(defun ajax-cipn-next-solution-aux (id &key all-solutions (fcg-light-style nil))
  (let* ((cipn (get-wi-object id))
         (nodes-to-requeue))
    (loop for node in (queue (cip cipn))
          if (or (eq node cipn) (find cipn (all-parents node)))
          collect node into nodes-below-cipn
          else collect node into other-nodes
          finally (setf nodes-to-requeue other-nodes)
          (setf (queue (cip cipn)) nodes-below-cipn))
    (add-element `((hr)))
    (add-element `((p) ,(if all-solutions
                          "Computing all solutions below&#160;&#160; "
                          "Computing next solution below&#160;&#160; ")
                   ,(if fcg-light-style
                      (make-html-fcg-light cipn :draw-children nil)
                      (make-html cipn :draw-children nil))))
    (if (queue (cip cipn))
      (let ((solutions 
             (if all-solutions 
               (loop for solution = (next-cip-solution (cip cipn) :notify nil)
                     while solution collect solution)
               (remove nil (list (next-cip-solution (cip cipn) :notify nil))))))
        (loop for node in nodes-to-requeue
              do (cip-enqueue node (cip cipn) 
                              (get-configuration (construction-inventory (cip cipn))
                                                 :queue-mode)))
        (add-element (if fcg-light-style
                       (make-html-fcg-light (cip cipn) :solutions solutions)
                       (make-html (cip cipn) :solutions solutions))))
      (add-element '((p) "queue exhausted!"))))
  (render-xml nil))

(ht-simple-ajax:defun-ajax cipn-next-solution (id fcg-light-style?) (wi::*ajax-processor*)
  (ajax-cipn-next-solution-aux id :all-solutions nil
                               :fcg-light-style fcg-light-style?))

(ht-simple-ajax:defun-ajax cipn-all-solutions (id  fcg-light-style?) (wi::*ajax-processor*)
  (ajax-cipn-next-solution-aux id :all-solutions t
                               :fcg-light-style fcg-light-style?))

(define-css 'cipn "
div.cipn { margin-bottom:5px; margin-top:5px; }
div.cipn-float { display:inline-block;margin-right:10px;
                 margin-top:-6px;margin-bottom:8px; }
div.cipn-title { padding-top:2px; padding-left:5px;padding-right:5px;padding-bottom:2px;  }
div.cipn-title { position:relative; color:#fff; }
div.cipn-title > a { color:#fff; }
div.cipn-linear-chain { padding-top:2px; padding-left:5px;padding-right:5px;padding-bottom:2px;background-color:#ddd;border-left:1px solid #888;border-right:1px solid #888; }
div.cipn > div > table { border-collapse:collapse; }
div.cipn > div > table > tbody > tr > td { padding-top:3px; padding-left:5px;padding-right:5px;padding-bottom:3px; vertical-align:top; }
div.cipn > div > table > tbody > tr > td > table { margin-bottom:-3px;}
div.cipn-hidden-subtree { padding:0px;margin:0px;padding:0px;margin-bottom:2px; }
")

(defgeneric make-html-cipn-title (cipn construction-inventory))

(defmethod make-html-cipn-title ((cipn cip-node)
                                 (construction-inventory construction-inventory))
  `((span :style ,(mkstr (cond ((or (eq (car (statuses cipn)) 'succeeded)
                                    (and (fully-expanded? cipn)
                                         (not (children cipn))))
                                "font-weight:bold;")
                               ((not (fully-expanded? cipn)) "font-style:italic;")
                               (t ""))))
    ,(if (car-applied-cxn (cipn-car cipn))
       (make-html-construction-title (car-applied-cxn (cipn-car cipn)))
       "initial")))

(defgeneric cipn-node-color (cipn cip)
  (:documentation "Computes the html color (e.g. '#ffaa33' for a node"))

;;;;;; FUNCTION TO SPECIFY THE COLOR OF THE NODES
(defmethod cipn-node-color ((cipn cip-node) (cip construction-inventory-processor))
  (cond ((eq (car (statuses cipn)) 'restart) "#8028E0;")
        ((eq (car (statuses cipn)) 'problem-found) "#8028E0;")
        ((eq (car (statuses cipn)) 'succeeded) "#040;")
        ((eq (car (statuses cipn)) 'cxn-applied) "#060;")
        ((and (eq (car (statuses cipn)) 'cxn-applied)
              (not (children cipn)) (fully-expanded? cipn)) "#a10;")
        ((and (eq (car (statuses cipn)) 'cxn-applied)
              (fully-expanded? cipn)) "#253;")
        ((and (eq (car (statuses cipn)) 'cxn-applied)
              (not (fully-expanded? cipn))) "#352;")
        (t (status-color (car (statuses cipn))))))

(defparameter *linear-chain-color* "#363;")

(defmethod make-html ((cipn cip-node)
		      &key (struct-id nil)
                      (draw-children t)
                      (hide-subtrees-with-duplicates t)
		      (tree-id (make-id 'cipn)) 
                      (within-linear-chain? nil)
                      (linear-chain-id nil)
                      (last-node-of-linear-chain nil)
                      (configuration nil))
  (let* ((node-id-0 (make-id 'cipn))
	 (node-id-1 (make-id 'chain-element-id))
         (node-id-2 (make-id 'chain-id))
         (cip (cip cipn))
         (construction-inventory (construction-inventory cip))
         (configuration (or configuration (visualization-configuration construction-inventory)))
	 (node-color (cipn-node-color cipn cip))
         (with-search-debug-data (get-configuration configuration :with-search-debug-data))
         (node-id (symb tree-id '- (created-at cipn)))
         ;;function for the title
         (title-div
          (lambda (bol_color link-parameters)
            `((div :class "cipn-title" 
                   :style ,(mkstr "background-color:" (if bol_color "#A8930D;" node-color ) ";"))
              ((a ,@link-parameters)
               ,(if with-search-debug-data
                  (format nil "~a,&#160;~,2f: " (created-at cipn) (priority cipn)) 
                  "")
               ,(make-html-cipn-title cipn construction-inventory)))))
         ;;
         (td-with-background-style 
          `(:style ,(mkstr "background-color:" node-color ";color:#fff;")))
         ;;
         (status-tr
          `((tr) 
            ((td ,@(when (duplicate cipn)
                     `(:onMouseOver 
                       ,(mkstr "var n = document.getElementById('" 
                               tree-id '- (created-at (duplicate cipn))
                               "'); if (n) n.style.border= '3px solid #dd5';")
                       :onMouseOut 
                       ,(mkstr "var n = document.getElementById('" 
                               tree-id '- (created-at (duplicate cipn))
                               "'); if (n) n.style.border = 'none';"))))
             ,@(loop for status on (statuses cipn)
                     collect `((span :style ,(mkstr "color:" (status-color 
                                                              (car status))))
                               ,(format nil "~(~a~)" (car status))
                               ,@(when (cdr status) '(", ")))))))
         ;;check if the subtree is duplicated
	 (complete-sub-tree-is-duplicate 
          (labels ((duplicate-sub-tree? (node)
                     (cond ((not (fully-expanded? node)) nil)
                           ((children node)
                            (loop for child in (children node)
                                  always (duplicate-sub-tree? child)))
                           (t (duplicate node)))))
            (and hide-subtrees-with-duplicates (duplicate-sub-tree? cipn))))
         ;;
         (linear-chain
          (unless within-linear-chain?
            (labels ((traverse-linear-chain (node &optional label)
                       (when (and (all-parents node) (children node)
                                  (car-resulting-cfs (cipn-car node))
                                  (or (not label)
                                      (eq (attr-val
                                           (car-applied-cxn (cipn-car node)) :label)
                                          label))
                                  (not (member (car (statuses node))
                                               '(succeeded goal-test-failed))))
                         (if (= 1 (length (children node)))
                           (cons node (traverse-linear-chain 
                                       (first (children node))
                                       (attr-val
                                        (car-applied-cxn (cipn-car node)) :label)))
                           (cons node nil)))))
              (let ((chain (traverse-linear-chain cipn)))
                (when (> (length chain) 1) chain)))))
         ;;
         (within-linear-chain (or within-linear-chain? linear-chain))
         ;;
         (last-node-of-linear-chain (or last-node-of-linear-chain
                                        (car (last linear-chain))))
         ;;
         (linear-chain-id (or linear-chain-id (when linear-chain (make-id 'chain))))
         ;;
         (outer-div-with-menu
          (lambda (div-children)
            `((div :class "cipn" :id ,(mkstr node-id))
              ,(make-div-with-menu 
                :div-attributes `(:style ,(mkstr "border:1px solid " node-color))
                :menu-items 
                `(,@(when (and within-linear-chain draw-children)
                      `(((a ,@(make-expand/collapse-link-parameters
                               linear-chain-id nil 
                               "compact linear chain of nodes"))
                         "&#x2217;") ))
                  ((a :href ,(format nil "javascript:ajax_save_cipn('~a');" 
                                     node-id)
                      :title "save node")
                   "&#x22a4;")
                  ,@(when 
                        (car-resulting-cfs (cipn-car cipn))
                      `(((a :href 
                            ,(format nil "javascript:ajax_save_cipn_cfs('~a');" 
                                     node-id)
                            :title "save cfs")
                         "&#x22ba;")
                        ((a :href 
                            ,(format nil "javascript:ajax_print_cipn_cfs('~a');" 
                                     node-id)
                            :title "print cfs as text")
                         "&#x22ee;")
                        ,@(when 
                              (loop for node in (queue cip)
                                    thereis (or (eq node cipn)
                                                (find cipn (all-parents node))))
                            `(((a :href 
                                  ,(format 
                                    nil 
                                    "javascript:ajax_cipn_next_solution('~a','nil');"
                                    node-id)
                                  :title "get next solution below this node")
                               "&#x227b;") 
                              ((a :href 
                                  ,(format 
                                    nil 
                                    "javascript:ajax_cipn_all_solutions('~a','nil');"
                                    node-id)
                                  :title "get all solutions below this node")
                               "&#x226b;")))
                        ((a :href ,(format nil "javascript:ajax_render_cipn('~a','nil');"
                                           node-id)
                            :title "render cfs to utterance")
                         "&#x22b3;") 
                        ((a :href ,(format 
                                    nil 
                                    "javascript:ajax_extract_meaning_cipn('~a');"
                                    node-id)
                            :title "extract meaning of cfs")
                         "&#x22b2;"))))
                :div-children div-children))))
         ;;generates the different versions of the node
         (div
          (if (get-configuration configuration :expand-nodes-in-search-tree)
            (lambda ()
              (make-expandable/collapsable-element 
               node-id-1 (make-id)
               ;; the unexpanded version
               (lambda ()
                 (funcall 
                  outer-div-with-menu
                  (list 
                   (funcall 
                    title-div nil
                    (make-expand/collapse-link-parameters node-id-1 t)))))
               (lambda ()
                 (make-expandable/collapsable-element
                  node-id-2 (make-id)
                  ;; show some details
                  (lambda ()
                    (funcall 
                     outer-div-with-menu 
                     (list 
                      (funcall 
                       title-div nil
                       (make-expand/collapse-link-parameters node-id-2 t "show more details"))
                      `((table)
                        ((tbody)
                         ,status-tr
                         ,@(when (car-resulting-cfs (cipn-car cipn))
                             (let ((*make-html-cfs-construction-inventory* 
                                    construction-inventory))
                               `(((tr) 
                                  ((td :style ,(mkstr "border-top:1px solid "
                                                      node-color))
                                   ,(make-html (car-resulting-cfs (cipn-car cipn))
                                               :configuration configuration)))))))))))
                  ;; show lots of details
                  (lambda ()
                    (funcall 
                     outer-div-with-menu 
                     (list 
                      (funcall 
                       title-div nil
                       (make-expand/collapse-link-parameters node-id-1 nil "hide details"))
                      (let ((*make-html-cfs-construction-inventory* construction-inventory))
                        `((table)
                          ((tbody)
                           ,status-tr
                           ((tr) 
                            ((td ,@td-with-background-style) "application result"))
                           ((tr) ((td) ,(make-html (cipn-car cipn)
                                                   :configuration configuration)))
                           ((tr)
                            ((td ,@td-with-background-style) 
                             "cxn supplier "
                             ((tt)
                              ,(format nil "~(~:w~)" 
                                       (get-configuration construction-inventory
                                                          :cxn-supplier-mode)))))
                           ((tr) ((td) ,(make-html (cxn-supplier cipn))))
                           ,@(when 
                                 (fields (goal-test-data cipn))
                               `(((tr)
                                  ((td ,@td-with-background-style)
                                   ,(format nil "goal tests: ~{~(~a~)~^, ~}"
                                            (get-configuration 
                                             construction-inventory
                                             (if (eq (direction cip) '->)
                                               'production-goal-tests
                                               'parse-goal-tests)))))
                                 ((tr) 
                                  ((td) 
                                   ,(make-html (goal-test-data cipn))))))
                           ,@(when (get-configuration cipn :show-meaning/utterance)
                               (if (eq (direction (cip cipn)) '<-)
                                 `(((tr)
                                    ((td ,@td-with-background-style)
                                     "meaning"))
                                   ((tr) 
                                    ((td) 
                                     ,(let* ((meaning 
                                              (extract-meanings (left-pole-structure 
                                                                 (car-resulting-cfs
                                                                  (cipn-car cipn))))))
                                        (if (get-configuration cipn :draw-meaning-as-network)
                                          (if (get-configuration configuration :show-wiki-links-in-predicate-networks)
                                            (predicate-network-with-wiki-links->svg meaning)
                                            (predicate-network->svg meaning))
                                          (make-html meaning))))))
                                 `(((tr)
                                    ((td ,@td-with-background-style)
                                     "utterance"))
                                   ((tr) 
                                    ((td) 
                                     ,(let* ((utterance
                                              (render
                                               (car-resulting-cfs (cipn-car cipn))
                                               (get-configuration (cip cipn)
                                                                  :render-mode))))
                                        (make-html utterance)))))))))))))))))
            (lambda ()
              (make-expandable/collapsable-element 
               node-id-1 node-id-2
               ;; the unexpanded version
               (lambda ()
                 (store-expanded-node (mkstr node-id-1) nil)
                 (store-current-cipn cipn (mkstr node-id-1))
                 (setf *last-cipn* (mkstr node-id-1))
                 (funcall title-div nil
                          (extended-make-expand/collapse-link-parameters
                           node-id-1 struct-id :title "select structure to show")))
               (lambda ()
                 (funcall title-div t
                          (make-expand/collapse-link-parameters node-id-1 nil "unselect")))))))         
         (tree
          (lambda ()
            (draw-node-with-children
             (funcall div)
             (unless (eq cipn last-node-of-linear-chain)
               (reverse 
                (loop for child in (children cipn)
                      collect (if (get-configuration configuration :expand-nodes-in-search-tree)
                                (make-html 
                                 child 
                                 :tree-id tree-id 
                                 :last-node-of-linear-chain last-node-of-linear-chain
                                 :within-linear-chain? within-linear-chain
                                 :linear-chain-id linear-chain-id
                                 :hide-subtrees-with-duplicates 
                                 (and hide-subtrees-with-duplicates
                                      (not complete-sub-tree-is-duplicate)))
                                (make-html
                                 child 
                                 :struct-id struct-id
                                 :tree-id tree-id 
                                 :last-node-of-linear-chain last-node-of-linear-chain
                                 :within-linear-chain? within-linear-chain
                                 :linear-chain-id linear-chain-id
                                 :hide-subtrees-with-duplicates 
                                 (and hide-subtrees-with-duplicates
                                      (not complete-sub-tree-is-duplicate)))))))
             :line-width "1px" :style "solid" :color "#888" :width "10px"))))
    (when (get-configuration configuration :expand-nodes-in-search-tree)
      (store-wi-object cipn node-id))
    (cond 
     ((not draw-children)
      `((div :class "cipn-float") ,(funcall div)))
     (complete-sub-tree-is-duplicate
      (make-expandable/collapsable-element 
       node-id-0 (make-id 'cipn)
       `((div :class "cipn-hidden-subtree") 
         ((a ,@(make-expand/collapse-link-parameters node-id-0 t "expand subtree"))
          "+"))
       (draw-node-with-children 
        `((div :class "cipn-hidden-subtree") 
          ((a ,@(if (get-configuration configuration :expand-nodes-in-search-tree)
                  (make-expand/collapse-link-parameters node-id-0 nil "collapse subtree")
                  (make-expand/collapse-link-parameters-of-subtrees node-id-0 nil "collapse subtree")))
           "-"))
        (list (funcall tree)))))      
     (linear-chain
      (draw-node-with-children
       (make-expandable/collapsable-element 
        linear-chain-id (make-id)
        `((div :class "cipn cipn-title" 
               :style ,(mkstr "background-color:" *linear-chain-color*))
          ((a ,@(make-expand/collapse-link-parameters 
                 linear-chain-id t "show nodes of linear chain"))
           "* "
           ,@(loop for node on linear-chain 
                   append `(,(make-html-cipn-title (car node) construction-inventory)
                            ,@(when (cdr node) '(", "))))))
        (funcall tree))
       (loop for child in (children (car (last linear-chain)))
             collect (if (get-configuration configuration :expand-nodes-in-search-tree)
                       (make-html 
                        child 
                        :tree-id tree-id 
                        :hide-subtrees-with-duplicates hide-subtrees-with-duplicates)
                       (make-html 
                        child
                        :struct-id struct-id
                        :tree-id tree-id 
                        :hide-subtrees-with-duplicates hide-subtrees-with-duplicates)))
       :line-width "1px" :style "solid" :color "#888" :width "10px"))      
     (t (funcall tree)))))

(defun make-tr-for-cip-queue (cip title &key (configuration nil))
  (let* ((*make-html-cfs-construction-inventory* (construction-inventory cip))
         (configuration (or configuration (visualization-configuration *make-html-cfs-construction-inventory*))))
    `((tr)
      ((td) ,title)
      ((td) ,@(html-hide-rest-of-long-list 
               (queue cip) 5
               #'(lambda (node)
                   (make-html 
                    node :draw-children nil 
                    :configuration configuration)))))))

(defun make-tr-for-cip-tree (cipn title &key (hide-subtree-with-duplicates t)
                                  (configuration nil))
  (let* ((*make-html-cfs-construction-inventory* (construction-inventory (cip cipn)))
         (configuration (or configuration (visualization-configuration *make-html-cfs-construction-inventory*))))
    `((tr)
      ((td) ,title)
      ((td) ,(make-html 
              cipn 
              :hide-subtrees-with-duplicates hide-subtree-with-duplicates
              :configuration configuration)))))

;; #########################################################
;; cxn-suppliers
;; ---------------------------------------------------------

(defmethod make-html ((cxn-supplier cxn-supplier-with-ordered-labels) &key)
  `((table :class "two-col")
    ((tbody)
     ((tr)
      ((td) "remaining labels")
      ((td) ,(html-pprint (remaining-labels cxn-supplier))))
     ((tr)
      ((td) "remaining cxns")
      ((td) ,(html-pprint (mapcar #'name (remaining-constructions cxn-supplier))))))))

(defmethod make-html ((cxn-supplier cxn-supplier-with-simple-queue) &key)
  `((table :class "two-col")
    ((tbody)
     ((tr)
      ((td) "remaining cxns")
      ((td) ,(html-pprint (mapcar #'name (remaining-constructions cxn-supplier))))))))
