(in-package :fcg)

;; #########################################################
;; make-html
;; ---------------------------------------------------------

(export '(make-html-fcg-light))

(defgeneric make-html-fcg-light (object &key &allow-other-keys)
  (:documentation "Makes html code from object in a FCG way. The result is an
                   s-expression representation of an html element
                   representing the object. You can turn this
                   representation into actual xhtml by
                   calling (render-xml result) on it."))

(define-css 'arrow "
div.pole-connector { height:32px;width:55px; }
div.pole-connector > div.domain { position:absolute;top:-4px;padding-left:4px; padding-right:4px;}
div.pole-connector > svg { position:absolute;top:11px; }
div.pole-connector > div.save-button { position:absolute;left:15px;top:20px;color:#006;display:none; }
div.pole-connector > span a { position:absolute;top:11px;left:10px;height:10px;width:35px; }
div.pole-connector > span a:hover { border-bottom:1px solid #006; }
")

(define-css 'fcg-light-unit "
div.fcg-light-unit { border:1px solid #888; margin-bottom:6px; margin-top:6px; }
div.fcg-light-added-unit { border:1px solid #888; margin-bottom:6px; margin-top:6px;background-color:#C2E0D1; }
div.fcg-light-modified-unit { border:1px solid #888; margin-bottom:6px; margin-top:6px;background-color:#FFFFCC; }
div.fcg-light-unit-name { padding-top:2px; padding-bottom:2px; padding-left:5px; padding-right:5px;font-weight:bold; color:#009; }
div.fcg-light-unit-feature { padding-left:5px; padding-right:2px;padding-top:0px;padding-bottom:1px; background-color:#fff;}
div.fcg-light-unit-feature-matched { padding-left:5px; padding-right:2px;padding-top:0px;padding-bottom:1px; background-color:#CEE4FF;}
div.fcg-light-unit-feature-name { float:left;color:#000;white-space:nowrap; }
div.fcg-light-unit-feature-not {  float:left;color:#DF0101;white-space:nowrap; }
div.fcg-light-unit-feature-name span.tagged { color:#700; }
div.fcg-light-unit-feature-value { margin-top:0px;margin-bottom:0px;margin-left:0px;margin-right:5px;display:inline-table; }
div.fcg-light-unit-default-feature-value { color:#444;margin-top:0px;margin-bottom:0px;margin-left:5px;margin-right:5px;display:inline-table; }
div.fcg-light-unit-name-sep { padding-left:0px; padding-right:0px;padding-top:0px;padding-bottom:0px;border-top:1px solid #888;}
div.fcg-light-unit-lock-sep { padding-left:0px; padding-right:0px;padding-top:0px;padding-bottom:0px;border-top:1px solid #000;}
div.empty-contributing-part { padding-left:10px; padding-right:10px;font-size:200%; }
")

(defun make-arrow (expand/collapse-all &key (expanded-units nil))
  (let ((expand-symbol (format nil "&#160;&#160;&#160;&#160;&#160; &#10753; &#160;&#160;&#160;&#160;&#160;"))
        (expand-text "expand construction")
        (collapse-symbol (format nil "&#160;&#160;&#160;&#160;&#160; &#10752; &#160;&#160;&#160;&#160;&#160;"))
        (collapse-text "collapse construction"))
    `((div :style "position:relative;" :class "arrow")
      ((div :class "domain" :style "left:0px;")
       ,(make-expandable/collapsable-element 
         (make-id 'left-pole) expand/collapse-all
         `((a ,@(make-expand/collapse-all-link-parameters 
                 expand/collapse-all t (if expanded-units collapse-text expand-text)))
           ,(if expanded-units collapse-symbol expand-symbol))
         `((a ,@(make-expand/collapse-all-link-parameters 
                 expand/collapse-all nil (if expanded-units expand-text collapse-text)))
           ,(if expanded-units expand-symbol collapse-symbol)))
       ((svg :xmlns "http://www.w3.org/2000/svg" :width "55" :height "10")
        ((line :stroke "#000060" :stroke-width "0.9px" 
               :x1 "2" :y1 "5" :x2 "53" :y2 "5"))
        ((polygon :points "0,5 8,0 8,10" :fill "#000060")))))))

;; #########################################################
;; construction
;; ---------------------------------------------------------

(defun get-feature-type (feature-name
                         feature-types)
  "function that returns the feature type of a feature, if it is specified"
  (let ((feature-type nil))
    (loop for ft in feature-types
          do
          (when (equal feature-name (first ft))
            (setf feature-type (second ft))))
    feature-type))

(defun get-expansion-operator (feature-name
                               feature-types)
  "function that returns the name of the expansion operator of a feature
(corresponds to the third element of the list"
  (let ((expansion-op nil))
    (loop for ft in feature-types
          do
          (when (equal feature-name (first ft))
            (setf expansion-op (third ft))))
    expansion-op))

(defun draw-expansion-operator (expansion-op)
  "function to draw the expansion operator"
  `((b)
    ,(format nil (concatenate 'string " +" (remove #\: (write-to-string expansion-op :case :downcase) :end 1) "+"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FUNCTION "REPLACE-ALL" TO ADD IN "UTILS"
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun replace-all (string
                    part
                    replacement
                    &key
                    (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
          while pos)))

(defun get-format-set-of-predicates (feature-value)
  "returns the format of a feature value when it has been labeled as set of predicates"
  (let ((string-to-return (concatenate 'string  (write-to-string (first feature-value)) "&#40;")))
    (loop for value in (cdr feature-value)
          do
          (setf string-to-return (concatenate 'string string-to-return (write-to-string value) "&#44;&#32;")))
    (setf string-to-return (concatenate 'string string-to-return "&#41;"))
    (setf string-to-return (concatenate 'string (subseq string-to-return 0 (search "&#44;&#32;&#41;" string-to-return)) "&#41;"))
    (format nil "~(~a~)"  (replace-all (replace-all string-to-return "FCG:" "") "#:" ""))))

(defun get-format-default-feature-value (feature-value
                                         &key
                                         (tab 0))
  "function that returns the html code for the default feature value"
  (let ((string-tab ""))
    (loop for i from 1 to tab 
          do (setf string-tab (string-append string-tab "&#160;&#160;&#160;")))
    (if (not (listp feature-value))
      `((tr)
        ,(get-highlighted-element (first feature-value) :string-tab string-tab)
        ,(format nil  "&#160;&#160;"))
      (if (not (listp (first feature-value)))
        (if (not (listp (second feature-value)))
          `((tr)
            ,(get-highlighted-element (first feature-value) :string-tab string-tab)
            ,(format nil  ": ")
            ,(get-highlighted-element (second feature-value))
            ,(format nil  "&#160;&#160;"))
          `((tr)
            ,(get-highlighted-element (first feature-value) :string-tab string-tab)
            ,(format nil  ":&#160;&#160;")        
            ,@(loop for fv in (cdr feature-value)
                    collect (get-format-default-feature-value fv :tab (+ tab 1)))))
        `((tr)
          ,@(loop for fv in feature-value
                  collect (get-format-default-feature-value fv :tab tab)))))))

(defun get-format-default-feature-value-for-units (feature-values)
  "function that returns the html code for the default feature value"
  `((tr)
    ,@(loop for feature-value in feature-values
            collect (get-format-default-feature-value feature-value))))

(defun fcg-light-feature-in-lists->html (feature
                                         feature-types)
  "function that draws features. It calls the feature-value->html function"
  (declare (ignore feature-types))
  (if (equal feature 'emptyset)
    `((div :class "unit-feature-value") "&#x2205;")
    (if (second feature)
      (let ((feature-name-string (if (equal (first feature) 'hash)
                                   (format nil "&#35;&#32;~(~a~):&#160;&#160;" (second feature)) ;;#+ +name 
                                   (format nil "~(~a~):&#160;&#160;" (first feature))))
            (feature-values (if (equal (first feature) 'hash)
                              (cddr feature)
                              (cdr feature))))
        `((div :class "fcg-light-unit-feature")
          ,(if (and (= (length feature-values) 1)
                    (not (listp (car feature-values))))
             `((span)
               ((div :class "fcg-light-unit-feature-name") ,feature-name-string)
               ((br))
               ((div :class "fcg-light-unit-default-feature-value") ,(format nil "~(~a~)&#160;&#160;" (car feature-values))))
             `((span)
               ((div :class "fcg-light-unit-feature-name") ,feature-name-string)
               ((br))
               ((div :class "fcg-light-unit-default-feature-value")
                ,@(loop for feature-value in feature-values
                        collect
                        `((tr) ,(format nil  "~(~a~)&#160;&#160;"  feature-value))))))))
      "")))

(defun collect-highlighted-elements (feature-values &optional result)
  "Helper function that allows dotted list."
  (if (atom feature-values)
    (reverse (cons-if feature-values result))
    (collect-highlighted-elements (rest feature-values)
                                  (cons (get-highlighted-element (first feature-values)) result))))

(defun fcg-light-sequence-feature-type (feature-values)
  "returns the html for the feature-values of type sequence"
  (let ((elements-to-add (collect-highlighted-elements feature-values)))
    `((tr)
      ,(format nil " &#91;")
      ,@(loop for (element . rest) on elements-to-add
              collect element
              when rest collect (format nil ", "))
      ,(format nil "&#93;"))))

(defun fcg-light-set-feature-type (feature-values)
  "returns the html for the feature-values of type set"
  (let (not-to-add)
    `((tr)
      ,(format nil " &#123;")
      ,@(loop for (element . rest) on feature-values
              collect (if (string= element 'NOT)
                        (progn
                          (setf not-to-add t)
                          `((span :style "color:#DF0101;")
                            ,(format nil "&#172; ")))
                        (if not-to-add
                          (get-highlighted-element element :negated? t)
                          (get-highlighted-element element)))
              when (and rest (not (string= element 'NOT)))
              collect (if not-to-add
                        `((span :style "color:#DF0101;")
                            ,(format nil ", "))
                        (format nil ", ")))
      ,(format nil "&#125;"))))

(defun fcg-light-set-of-predicates-feature-type (feature-values &key rest)
  "returns the html for the feature-values of type set-of-predicates"
  (if (consp feature-values)
    (let ((elements-to-add (loop for element in feature-values
                                 collect
                                 (get-highlighted-element element))))
      `((span)
        ,(car elements-to-add)
        ,(format nil "&#40;")
        ,@(loop for (element . rest) on (cdr elements-to-add)
                collect element
                when rest collect (format nil ", "))
        ,(format nil "&#41;")
        ,(if rest
           (format nil ",")
           (format nil ""))))
    feature-values))

(defun fcg-light-set-of-predicates-feature-type-for-root (feature-values &key rest)
  "returns the html for the feature-values of type set-of-predicates"
  (let ((elements-to-add (loop for element in feature-values
                               collect
                               (get-highlighted-element element))))
    `((tr)
      ,(car elements-to-add)
      ,(format nil " &#40;")
      ,@(loop for (element . rest) on (cdr elements-to-add)
              collect element
              when rest collect (format nil ", "))
      ,(format nil "&#41;")
      ,(if rest
         (format nil ",")
         (format nil "")))))

(defun fcg-light-feature->html (feature
                                feature-types
                                &key
                                (matching-background nil)
                                (configuration *default-visualization-configuration*))
  "function that draws features. It calls the feature-value->html function"
  (cond ((equal feature 'emptyset)
         `((div :class "fcg-light-unit-feature-value") "&#x2205;"))
        (t
         ;;feature types
         (if (second feature)
           (let ((hash-to-add? (if (string= (first feature) 'hash)
                                 t
                                 nil))
                 (not-to-add? (if (string= (first feature) 'NOT)
                                t
                                nil)))
             (let ((symbol-to-add (cond (hash-to-add? (format nil "# "))
                                        (not-to-add? (format nil "&#172; "))
                                        (t (format nil ""))))
                   (feature-name-string (cond (hash-to-add? (get-highlighted-element (second feature)))
                                              (not-to-add? (get-highlighted-element (car (second feature))))
                                              (t (get-highlighted-element (first feature)))))
                   (feature-values (cond (hash-to-add? (cddr feature))
                                         (not-to-add? (cdr (second feature)))
                                         (t (cdr feature))))
                   (feature-type (get-feature-type (cond (hash-to-add? (second feature))
                                                         (not-to-add? (car (second feature)))
                                                         (t (first feature)))
                                                   feature-types))
                   (expansion-operator (get-expansion-operator (cond (hash-to-add? (second feature))
                                                                     (not-to-add? (car (second feature)))
                                                                     (t (first feature)))
                                                               feature-types)))
               (cond
                ((equalp (symbol-name feature-type) (symbol-name 'sequence))
                 `( ,(if matching-background
                       `(div :class "fcg-light-unit-feature-matched")
                       `(div :class "fcg-light-unit-feature"))
                    ((span)
                     (,(if not-to-add?
                         `(div :class "fcg-light-unit-feature-not")
                         `(div :class "fcg-light-unit-feature-name")) ,symbol-to-add ,feature-name-string
                                                                      ,(if expansion-operator
                                                                         (draw-expansion-operator expansion-operator)
                                                                         (format nil ":")))
                     ,(if (atom (car feature-values)) ;; For the case you want to use a variable to match a sequence
                        `((div :class "fcg-light-unit-default-feature-value")
                          ,(get-highlighted-element (car feature-values)))
                        (progn
                          `((br))
                          `((div :class "fcg-light-unit-default-feature-value")
                            ,(fcg-light-sequence-feature-type (car feature-values))))))))
                ((equalp (symbol-name feature-type) (symbol-name 'sequence-of-predicates))
                 `( ,(if matching-background
                       `(div :class "fcg-light-unit-feature-matched")
                       `(div :class "fcg-light-unit-feature"))
                    ((span)
                     (,(if not-to-add?
                         `(div :class "fcg-light-unit-feature-not")
                         `(div :class "fcg-light-unit-feature-name"))
                      ,symbol-to-add ,feature-name-string ,(if expansion-operator
                                                             (draw-expansion-operator expansion-operator)
                                                             (format nil ":")))
                     ,(if (atom (car feature-values)) ;; For the case you want to use a variable to match a sequence-of-predicates
                        `((div :class "fcg-light-unit-default-feature-value")
                          ,(get-highlighted-element (car feature-values)))
                        (progn
                          `((br))
                          `((div :class "fcg-light-unit-default-feature-value")
                            ((tr)
                             ,(format nil "[")
                             ,@(loop for (element . rest) on (car feature-values)
                                     collect (fcg-light-set-of-predicates-feature-type element :rest rest)
                                     when rest collect `((br)))
                             ,(format nil "]"))))))))
                ((equalp (symbol-name feature-type) (symbol-name 'set-of-predicates))
                 `(,(if matching-background
                      `(div :class "fcg-light-unit-feature-matched")
                      `(div :class "fcg-light-unit-feature"))
                   ((span)
                    (,(if not-to-add?
                        `(div :class "fcg-light-unit-feature-not")
                        `(div :class "fcg-light-unit-feature-name")) ,symbol-to-add ,feature-name-string
                                                                     ,(if expansion-operator
                                                                        (draw-expansion-operator expansion-operator)
                                                                        (format nil ":")))
                    ,(if (atom (car feature-values)) ;; For the case you want to use a variable to match a set of predicates
                       `((div :class "fcg-light-unit-default-feature-value")
                         ,(get-highlighted-element (car feature-values)))
                       (progn
                         `((br))
                         `((div :class "fcg-light-unit-default-feature-value")
                           ((tr)
                            ,(format nil "{")
                            ,@(loop for (element . rest) on (car feature-values)
                                    collect (fcg-light-set-of-predicates-feature-type element :rest rest)
                                    when rest collect `((br)))
                            ,(format nil "}"))))))))
                ((equalp (symbol-name feature-type) (symbol-name 'set))
                 `(,(if matching-background
                      `(div :class "fcg-light-unit-feature-matched")
                      `(div :class "fcg-light-unit-feature"))
                   ((span)
                    (,(if not-to-add?
                        `(div :class "fcg-light-unit-feature-not")
                        `(div :class "fcg-light-unit-feature-name")) ,symbol-to-add ,feature-name-string
                                                                     ,(if expansion-operator
                                                                        (draw-expansion-operator expansion-operator)
                                                                        (format nil ":")))
                    ,(if (atom (car feature-values)) ;; For the case you want to use a variable to match a set
                       `((div :class "fcg-light-unit-default-feature-value")
                         ,(get-highlighted-element (car feature-values)))
                       (progn
                         `((br))
                         `((div :class "fcg-light-unit-default-feature-value")   
                           ,(fcg-light-set-feature-type (car feature-values))))))))
                (t ;;default case
                   `(,(if matching-background
                        `(div :class "fcg-light-unit-feature-matched")
                        `(div :class "fcg-light-unit-feature"))
                     ,(if (and (= (length feature-values) 1)
                               (not (listp (car feature-values))))
                        `((span)
                          (,(if not-to-add?
                              `(div :class "fcg-light-unit-feature-not")
                              `(div :class "fcg-light-unit-feature-name")) ,symbol-to-add ,feature-name-string
                                                                           ,(if expansion-operator
                                                                              (draw-expansion-operator expansion-operator)
                                                                              (format nil ":")))
                          (,(if not-to-add?
                              `(div :class "fcg-light-unit-feature-not")
                              `(div :class "fcg-light-unit-default-feature-value"))
                           ,(get-highlighted-element (first feature-values))
                           ,(format nil "&#160;&#160;")))
                        `((span)
                          (,(if not-to-add?
                              `(div :class "fcg-light-unit-feature-not")
                              `(div :class "fcg-light-unit-feature-name")) ,symbol-to-add ,feature-name-string
                                                                           ,(if expansion-operator
                                                                              (draw-expansion-operator expansion-operator)
                                                                              (format nil ":")))
                          ((br))
                          ((div :class "fcg-light-unit-default-feature-value")
                           ,@(loop for feature-value in feature-values
                                   collect
                                   (fcg-light-feature->html feature-value
                                                            feature-types
                                                            :matching-background matching-background
                                                            :configuration configuration))))))))))
           ""))))

(defun fcg-light-unit-feature->html (feature
                                     feature-types
                                     &key
                                     (configuration *default-visualization-configuration*))
  "function that draws features of units in the transient structure. It calls the feature-value->html function"
  (if (equal feature 'emptyset)
    `((div :class "fcg-light-unit-feature-value") "&#x2205;")
    ;;feature types
    (if (second feature)
      (let ((feature-name-string (get-highlighted-element (first feature)))
            (feature-values (cdr feature))
            (feature-type (get-feature-type (first feature) feature-types)))
        (cond
         ((equalp (symbol-name feature-type) (symbol-name 'sequence))
          `((div :class "fcg-light-unit-feature")
            ((span)
             ((div :class "fcg-light-unit-feature-name") ,feature-name-string ,(format nil ":"))
             ,(if (atom (car feature-values)) ;; For the case you want to use a variable to match a sequence
                `((div :class "fcg-light-unit-default-feature-value")
                  ,(get-highlighted-element (car feature-values)))
                (progn
                  `((br))
                  `((div :class "fcg-light-unit-default-feature-value")
                    ,(fcg-light-sequence-feature-type (car feature-values))))))))
         ((equalp (symbol-name feature-type) (symbol-name 'sequence-of-predicates))
          `((div :class "fcg-light-unit-feature")
            ((span)
             ((div :class "fcg-light-unit-feature-name") ,feature-name-string ,(format nil ":"))
             ,(if  (or (atom (car feature-values))
                       (atom (caar feature-values))) ;; For the case you want to use a variable to match a sequence-of-predicates
                `((div :class "fcg-light-unit-default-feature-value")
                  ,(get-highlighted-element (caar feature-values)))
                (progn
                  `((br))
                  `((div :class "fcg-light-unit-default-feature-value")
                    ((tr)
                     ,(format nil "[")
                     ,@(loop for (element . rest) on (caar feature-values)
                             collect (fcg-light-set-of-predicates-feature-type element :rest rest)
                             when rest collect `((br)))
                     ,(format nil "]"))))))))
         ((equalp (symbol-name feature-type) (symbol-name 'set-of-predicates))
          `((div :class "fcg-light-unit-feature")
            ((span)
             ((div :class "fcg-light-unit-feature-name") ,feature-name-string ,(format nil ":"))
             ,(if (atom (car feature-values)) ;; For the case you want to use a variable to match a set-of-predicates
                `((div :class "fcg-light-unit-default-feature-value")
                  ,(get-highlighted-element (car feature-values)))
                (progn
                  `((br))
                  `((div :class "fcg-light-unit-default-feature-value")
                    ((tr)
                     ,(format nil "{")
                     ,@(loop for (element . rest) on (car feature-values)
                             collect (fcg-light-set-of-predicates-feature-type element :rest rest)
                             when rest collect `((br)))
                     ,(format nil "}"))))))))
         ((equalp (symbol-name feature-type) (symbol-name 'set))
          `((div :class "fcg-light-unit-feature")
            ((span)
             ((div :class "fcg-light-unit-feature-name") ,feature-name-string ,(format nil ":"))
             ,(if (atom (car feature-values)) ;; For the case you want to use a variable to match a set
                `((div :class "fcg-light-unit-default-feature-value")
                  ,(get-highlighted-element (car feature-values)))
                (progn
                  `((br))
                  `((div :class "fcg-light-unit-default-feature-value")
                    ,(fcg-light-set-feature-type (car feature-values))))))))
         (t ;;default case
            `((div :class "fcg-light-unit-feature")
              ,(if (and (= (length feature-values) 1)
                        (not (listp (car feature-values))))
                 `((span)
                   ((div :class "fcg-light-unit-feature-name") ,feature-name-string ,(format nil ":"))
                   ((div :class "fcg-light-unit-default-feature-value")
                    ,(get-highlighted-element (first feature-values))
                    ,(format nil "&#160;&#160;")))
                 `((span)
                   ((div :class "fcg-light-unit-feature-name") ,feature-name-string ,(format nil ":"))
                   ((br))
                   ((div :class "fcg-light-unit-default-feature-value")
                    ,@(loop for feature-value in feature-values
                            collect
                            (get-format-feature-value-fcg-light-unit-feature feature-value feature-types
                                                                             :configuration configuration)))))))))
      "")))

(defun get-format-feature-value-fcg-light-unit-feature (feature-value
                                                        feature-types
                                                        &key
                                                        (configuration *default-visualization-configuration*))
  "function that returns the html code for the feature value of transient structures"
  (if (not (listp feature-value))
    `((tr)
      ,(get-highlighted-element (first feature-value)))
    (if (not (listp (first feature-value)))
      `((tr)
        ,(get-highlighted-element (first feature-value))
        ,@(loop for fv in (cdr feature-value)
                collect (fcg-light-unit-feature->html fv feature-types :configuration configuration)))
      `((tr)
        ,@(loop for fv in feature-value
                collect (fcg-light-unit-feature->html fv feature-types :configuration configuration))))))

(defun transient-structure-unit->html (contributing-unit
                                       feature-types
                                       expand/collapse-all-id
                                       &key
                                       (expand-units nil)
                                       (modified-units nil)
                                       (added-units nil)
                                       (configuration *default-visualization-configuration*))
  "function to draw contributing-units in html"
  (let* ((element-id (make-id 'contr-unit))
         (unit-name-lcase (format nil "~(~a~)" (car contributing-unit)))
         (unit-name-ucase (mkstr (car contributing-unit)))
         (unit-style (cond ((find (car contributing-unit) modified-units) "fcg-light-modified-unit")
                           ((find (car contributing-unit) added-units) "fcg-light-added-unit")
                           (t "fcg-light-unit")))
         (collapsed-unit `((div :class "fcg-light-unit-name")
                           ((a ,@(make-expand/collapse-link-parameters element-id t "expand unit")
                               :name ,unit-name-ucase) ,unit-name-lcase)))
         (expanded-unit (lambda ()
                          `((span)
                            ((div :class "fcg-light-unit-name")
                             ((a ,@(make-expand/collapse-link-parameters element-id nil "collapse unit")
                                 :name ,unit-name-ucase)
                              ,unit-name-lcase))
                            ((div :class "fcg-light-unit-name-sep"))
                            ,@(loop for feature in (cdr contributing-unit)
                                    collect (if (feature-to-hide-p feature :configuration configuration)
                                              ""
                                              (if (get-configuration configuration :latex-visualization)
                                                (fcg-light-unit-feature->html feature feature-types
                                                                              :configuration configuration)
                                                (fcg-light-feature-in-lists->html feature feature-types))))))))
    `((div :class ,unit-style)
      ,(if (cdr contributing-unit)
         (make-expandable/collapsable-element 
          element-id expand/collapse-all-id
          (if expand-units expanded-unit collapsed-unit)
          (if expand-units collapsed-unit expanded-unit))
         `((div :class "fcg-light-unit-name" :name ,unit-name-ucase) ,unit-name-lcase)))))

(defmethod make-html-for-transient-structure-unit (units
                                                   feature-types
                                                   &key
                                                   (expanded-units nil)
                                                   expand/collapse-all-id
                                                   (configuration *default-visualization-configuration*))
  (if units
    `((div)
      ,@(loop for unit in units
              collect `((div) ,(transient-structure-unit->html unit feature-types expand/collapse-all-id
                                                               :expand-units expanded-units
                                                               :configuration configuration))))
    `((div :class "empty-contributing-part") "&#x2205;")))

(defun contributing-unit->html (contributing-unit
                                feature-types
                                expand/collapse-all-id
                                &key
                                (expanded-units nil)
                                (configuration *default-visualization-configuration*))
  "function to draw contributing-units in html"
  (let* ((element-id (make-id 'contr-unit))
         (unit-name-lcase (format nil "~(~a~)" (name contributing-unit)))
         (unit-name-ucase (mkstr (name contributing-unit)))
         (collapsed-unit `((div :class "fcg-light-unit-name")
                           ((a ,@(make-expand/collapse-link-parameters element-id t "expand unit")
                               :name ,unit-name-ucase) ,unit-name-lcase)))
         (expanded-unit (lambda ()
                          `((span)
                            ((div :class "fcg-light-unit-name")
                             ((a ,@(make-expand/collapse-link-parameters element-id nil "collapse unit")
                                 :name ,unit-name-ucase)
                              ,unit-name-lcase))
                            ((div :class "fcg-light-unit-name-sep"))
                            ,@(loop for feature in (unit-structure contributing-unit)
                                    collect (if (feature-to-hide-p feature :configuration configuration)
                                              ""
                                              (if (get-configuration configuration :latex-visualization)
                                                (fcg-light-feature->html feature feature-types
                                                                         :configuration configuration)
                                                (fcg-light-feature-in-lists->html feature feature-types))))))))
    `((div :class "fcg-light-unit")
      ,(if (unit-structure contributing-unit)
         (make-expandable/collapsable-element 
          element-id expand/collapse-all-id
          (if expanded-units expanded-unit collapsed-unit)
          (if expanded-units collapsed-unit expanded-unit))
         `((div :class "fcg-light-unit-name" :name ,unit-name-ucase) ,unit-name-lcase)))))

(defmethod make-html-for-contributing-part (contributing-part
                                            feature-types
                                            &key
                                            expand/collapse-all-id
                                            (expanded-units nil)
                                            (configuration *default-visualization-configuration*))
  (if contributing-part
    `((div)
      ,@(loop for contributing-unit in contributing-part
              collect `((div) ,(contributing-unit->html contributing-unit feature-types expand/collapse-all-id
                                                        :expanded-units expanded-units
                                                        :configuration configuration))))
    `((div :class "empty-contributing-part") "&#x2205;")))

(defun conditional-unit->html (conditional-unit
                               feature-types
                               expand/collapse-all-id
                               direction
                               &key
                               (expanded-units nil)
                               (configuration *default-visualization-configuration*))
  "function to draw conditional-units in html"
  (let* ((element-id (make-id 'contr-unit))
         (unit-name-lcase (format nil "~(~a~)" (name conditional-unit)))
         (unit-name-ucase (mkstr (name conditional-unit)))
         (collapsed-unit `((div :class "fcg-light-unit-name")
                           ((a ,@(make-expand/collapse-link-parameters element-id t "expand unit")
                               :name ,unit-name-ucase) ,unit-name-lcase)))
         (expanded-unit (lambda ()
                          `((span)
                            ((div :class "fcg-light-unit-name")
                             ((a ,@(make-expand/collapse-link-parameters element-id nil "collapse unit")
                                 :name ,unit-name-ucase)
                              ,unit-name-lcase))
                            ((div :class "fcg-light-unit-name-sep"))
                            ,@(loop for feature in (formulation-lock conditional-unit)
                                    collect (if (feature-to-hide-p feature :configuration configuration)
                                              ""
                                              (if (get-configuration configuration :latex-visualization)
                                                (fcg-light-feature->html feature feature-types
                                                                         :matching-background (if direction (if (eql direction '->)
                                                                                                              t nil)
                                                                                                nil)
                                                                         :configuration configuration)
                                                (fcg-light-feature-in-lists->html feature feature-types))))
                            ,(if (not (formulation-lock conditional-unit))
                               `((div :class "fcg-light-unit-feature-value") "&#x2205;") "")
                            ((div :class "fcg-light-unit-lock-sep"))
                            ,@(loop for feature in (comprehension-lock conditional-unit)
                                    collect (if (feature-to-hide-p feature :configuration configuration)
                                              ""
                                              (if (get-configuration configuration :latex-visualization)
                                                (fcg-light-feature->html feature feature-types
                                                                         :matching-background (if direction (if (eql direction '->)
                                                                                                              nil t)
                                                                                                nil)
                                                                         :configuration configuration)
                                                (fcg-light-feature-in-lists->html feature feature-types))))
                            ,(if (not (comprehension-lock conditional-unit))
                               `((div :class "fcg-light-unit-feature-value") "&#x2205;")
                               "")))))
    `((div :class "fcg-light-unit")
      ,(make-expandable/collapsable-element 
        element-id expand/collapse-all-id
        (if expanded-units expanded-unit collapsed-unit)
        (if expanded-units collapsed-unit expanded-unit)))))

(defmethod make-html-for-conditional-part (conditional-part feature-types &key expand/collapse-all-id direction (expanded-units nil)
                                                            (configuration *default-visualization-configuration*))
  `((div)
    ,@(loop for conditional-unit in conditional-part
            collect `((div) ,(conditional-unit->html conditional-unit feature-types expand/collapse-all-id direction
                                                     :expanded-units expanded-units
                                                     :configuration configuration)))))

(defun fcg-light-cxn-structure->html (construction
                                      feature-types
                                      expand/collapse-all
                                      &key
                                      (expanded-units nil)
                                      (direction nil)
                                      (configuration nil))
  (let ((configuration (or configuration (visualization-configuration (cxn-inventory construction)))))
  `((table :class "cxn_cfs construction")
    ((tbody)
     ((tr)
      ((td)
       ((div)
        ,(make-html-for-contributing-part (contributing-part construction) feature-types
                                          :expanded-units expanded-units
                                          :expand/collapse-all-id expand/collapse-all
                                          :configuration configuration)))
      ((td) 
       ,(make-arrow expand/collapse-all :expanded-units expanded-units))
      ((td) 
       ,(make-html-for-conditional-part (conditional-part construction) feature-types
                                        :expanded-units expanded-units
                                        :expand/collapse-all-id expand/collapse-all
                                        :direction direction
                                        :configuration configuration)))))))

(ht-simple-ajax:defun-ajax print-fcg-light-construction (id) (wi::*ajax-processor*)
  (let ((construction (get-wi-object id)))
    (add-element `((hr)))
    (add-element 
     `((p) "Printing construction&#160;&#160; " 
       ,(make-html construction :expand-initially nil) "&#x2192;"))
    (add-element 
     `((pre) ,(let ((*print-right-margin* 100))
                (cl-who:escape-string (format nil "~:w" construction))))))
  (render-xml nil))

(defun get-duplicate-elements-and-values (lst)
  (cond ((null lst) '())
        ((not (variable-p (car lst)))
         (cons (car lst) (get-duplicate-elements-and-values (cdr lst))))
        ((member (car lst) (cdr lst))
         (cons (car lst) (get-duplicate-elements-and-values (cdr lst))))
        (t (get-duplicate-elements-and-values (cdr lst)))))

(defun ajax-apply-fcg-light-construction-aux (construction formulation?)
  (if (not (and *saved-cfs* (typep *saved-cfs* 'coupled-feature-structure)))
    (progn
      (add-element '((hr)))
      (add-element '((p) "Variable " ((tt) ((b) "*saved-cfs*")) 
                     " is not bound to a coupled feature structure")))
    (progn
      *saved-cfs*
      (add-element '((hr)))
      (add-element `((p) ((tt) ((b) "*saved-cfs*")) ":"
                     ,(make-html-fcg-light *saved-cfs*
                                           :feature-types (feature-types construction)
                                           :construction-inventory (cxn-inventory construction))))
      (multiple-value-bind (succeeded-cars failed-cars)
          (fcg-apply (get-processing-cxn construction)
                     *saved-cfs* 
                     (if (read-from-string formulation?) '-> '<-)
                     :configuration (if (get-root (left-pole-structure *saved-cfs*))
                                      (let ((config (make-instance 'configuration)))
                                        (set-configuration config :root-mode t))
                                      nil))
        (when (and (not succeeded-cars)
                   (not failed-cars))
          (let* ((matching-pattern (matching-pattern (get-processing-cxn construction)
                                                     (if (read-from-string formulation?)
                                                       '-> '<-)))
                 (anti-unify-result (anti-unify matching-pattern (left-pole-structure *saved-cfs*) :fcg))
                 (bindings (loop for elem in (cadar anti-unify-result)
                                 for el in (caddar anti-unify-result)
                                 collect `(,(car elem) . ,(car el))))
                 (conflicting-values (get-duplicate-elements-and-values (mapcar #'car bindings)))
                 (discarded-features (fourth (first anti-unify-result))))
            ;; Present conflicting values
            (when conflicting-values
              (add-element `((h4) "The following elements in the cxn give conflicts in matching: "))
              (loop for (element . rest) on conflicting-values
                    do (add-element (get-highlighted-element (get-properly-binding-name element) :if-string-print-as-string nil))
                    when rest do (add-element `((br))))
              (add-element `((h4) "with the following elements in the transient structure: "))
              (loop for element in bindings
                    when (find (car element) conflicting-values)
                    do (progn (add-element (get-highlighted-element (cdr element) :if-string-print-as-string nil))
                         (add-element `((br))))))
            ;; Present discarded features
            (when discarded-features
              (add-element `((h4) "The following features from the cxn were required but not found in the transient structure: "))
              (loop for element in discarded-features
                    do (progn
                         (add-element
                          (make-html element))
                         (add-element `((br))))))
            ;; Print Transient Structure
            (when (or conflicting-values discarded-features)
              (add-element `((br)))
              (add-element (make-html-fcg-light *saved-cfs*
                                                :construction-inventory (cxn-inventory construction)
                                                :feature-types (feature-types construction))))
            (cond ((eq nil anti-unify-result)
                   (add-element `((h4) "Unfortunately, Anti-Unification could not determine why the construction does not match")))
                  ((eq (length anti-unify-result) 1)
                   (add-element '((br)))
                   (add-element `((a :href ,(format nil "javascript:ajax_show_anti_unification_result('~a','~a');" 
                                                    anti-unify-result (read-from-string formulation?))
                                     :title "apply to *saved-cfs* in formulation") "Show entire anti-unification anlysis")))
                  (t
                   (add-element `((h4) "Anti-Unification yielded multiple analyses, only the most probable one is shown."))
                   (add-element `((a :href ,(format nil "javascript:ajax_show_anti_unification_result('~a','~a');" 
                                                    anti-unify-result (read-from-string formulation?))
                                     :title "apply to *saved-cfs* in formulation") "Show entire anti-unification analysis")))))))))
  (render-xml nil))

(ht-simple-ajax:defun-ajax show-anti-unification-result (id formulation?) (wi::*ajax-processor*)
  ;; Print result of anti-unification
  (let ((anti-unify-result (read-from-string id)))
    (if (eq (length anti-unify-result) 1)
      ;; One anti-unification result
      (progn
        (let ((solution (first anti-unify-result)))
          (add-element `((hr)))
          (add-element `((h3) ,(string-append "*** Anti-Unification Result ***")))
          (add-element `((h4) "Resulting Mathching Pattern: "))
          (add-element (make-html (make-instance 'processing-construction
                                                 :name 'resulting-pattern
                                                 :domain 'sem-syn
                                                 :left-pole (if (read-from-string formulation?)
                                                              (make-instance 'pole
                                                                             :pole-structure (first solution)
                                                                             :domain 'sem)
                                                              '((root)))
                                                 :right-pole (if (not (read-from-string formulation?))
                                                               (make-instance 'pole
                                                                              :pole-structure (first solution)
                                                                              :domain 'syn)
                                                               '((root))))))
          (add-element `((h4) "Pattern Bindings: "))
          (add-element (make-html (second  solution)))
          (add-element `((h4) "Source Bindings: "))
          (add-element (make-html (third solution)))
          (add-element `((h4) "Discarded Features: "))
          (if (fourth solution)
            (dolist (disc-feat (fourth solution))
              (add-element (make-html disc-feat)))
            (add-element `((p) "No features were discarded")))))
      ;; Multiple anti-unification results
      (loop for solution in anti-unify-result
            for s from 1 upto (length anti-unify-result) 
            do
            (add-element `((hr)))
            (add-element `((h3) ,(string-append "*** Anti-Unification Result " (write-to-string s) " ***")))
            (add-element `((h4) "Resulting Matching Pattern: "))
            (add-element (make-html (make-instance 'processing-construction
                                                   :name 'matching-pattern
                                                   :domain 'sem-syn
                                                   :left-pole (if (read-from-string formulation?)
                                                                (make-instance 'pole
                                                                               :pole-structure (first solution)
                                                                               :domain 'sem)
                                                                '((root)))
                                                   :right-pole (if (not (read-from-string formulation?))
                                                                 (make-instance 'pole
                                                                                :pole-structure (first solution)
                                                                                :domain 'syn)
                                                                 '((root))))))
            (add-element `((h4) "Pattern Bindings: "))
            (add-element (make-html (second solution)))
            (add-element `((h4) "Source Bindings: "))
            (add-element (make-html (third solution)))
            (add-element `((h4) "Discarded Features: "))
            (if (fourth solution)
              (dolist (disc-feat (fourth solution))
                (add-element (make-html disc-feat)))
              (add-element `((p) "No features were discarded"))))))
  (render-xml nil))

(ht-simple-ajax:defun-ajax apply-fcg-light-construction (id formulation?) (wi::*ajax-processor*)
  (let ((construction (get-wi-object id)))
    (ajax-apply-fcg-light-construction-aux construction formulation?)))

(defun ajax-obtain-cxn-in-latex-aux (construction)
  "saves a text file with the latex representation of the selected construction"
  ;; creating a file with the cxn name in ./tmp folder
  (let ((file-name (concatenate 'string ".tmp/" (format nil "~(~a~)" (name construction)) ".tex"))
        (path-string (subseq (namestring (asdf:system-source-directory :fcg)) 0
                             (search "systems" (namestring (asdf:system-source-directory :fcg))))))
    (with-open-file (outputstream file-name
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
      (fcg-light->latex construction :stream outputstream)
      (close outputstream)
      (open-file-in-os (concatenate 'string "file:///" path-string file-name)))))

(ht-simple-ajax:defun-ajax obtain-cxn-in-latex (id) (wi::*ajax-processor*)
  "Function that retrieves the latex form of the input construction"
  (let ((construction (get-wi-object id)))
    (ajax-obtain-cxn-in-latex-aux construction))
  nil)

(ht-simple-ajax:defun-ajax show-cxn-in-fcg-2 (id) (wi::*ajax-processor*)
  "Function that retrieves the fcg-2 form of the input construction"
  (let ((construction (get-wi-object id)))
    (add-element `((hr)))
    (add-element `((p),(make-html (get-processing-cxn construction))))
    nil))

(defmethod make-html-construction-title ((construction fcg-construction))
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

(defmethod make-html ((construction fcg-construction)
                      &key
                      (expand-initially t)
                      (expanded-units nil)
		      (expand/collapse-all-id (make-id 'construction))
                      (direction nil)
                      (configuration nil))
  (let* ((configuration (or configuration (visualization-configuration (cxn-inventory construction))))
         (element-id-1 (make-id 'construction))
	 (element-id-2 (make-id 'construction))
	 (expand/collapse-all (make-id 'construction))
	 (construction-title (make-html-construction-title construction))
         (feature-types (feature-types construction))
         (construction-id (make-id (name construction)))
         (outer-div
          (lambda (children)
            (make-div-with-menu 
             :div-attributes '(:class "cxn_cfs construction")
             :menu-items 
             `(((a :style "font-family: Courier New;font-size: 16px;"
                   :href ,(format nil "javascript:ajax_save_construction('~a');" 
                                  construction-id)
                   :title "save construction")
                "&#x22a4;") 
               ((a :style "font-family: Courier New;font-size: 16px;"
                   :href ,(format nil "javascript:ajax_print_fcg_light_construction('~a');" 
                                  construction-id)
                   :title "print construction as text")
                "&#x22ee;") 
               ((a :style "font-family: Courier New;font-size: 16px;"
                   :href ,(format nil "javascript:ajax_apply_fcg_light_construction('~a','t');" 
                                  construction-id)
                   :title "apply to *saved-cfs* in formulation")
                "&#x22a2;") 
               ((a :style "font-family: Courier New;font-size: 16px;"
                   :href ,(format nil "javascript:ajax_apply_fcg_light_construction('~a','nil');" 
                                  construction-id)
                   :title "apply to *saved-cfs* in comprehension")
                "&#x22a3;")
               ((a :style "font-family: Courier New;font-size: 16px;"
                   :href ,(format nil "javascript:ajax_obtain_cxn_in_latex('~a');" construction-id)
                   :title "obtain the construction in Latex")
                "L")
               ((a :style "font-family: Courier New;font-size: 16px;"
                   :href ,(format nil "javascript:ajax_show_cxn_in_fcg_2('~a');" construction-id)
                   :title "show the corresponding low-level construction")
                "FCG2"))
             :div-children children)))
	 (collapsed-version 
	  (lambda ()
	    (funcall 
             outer-div 
             `(((div :class "title" :draggable "true"
                     :ondragstart
                     ,(format nil "event.dataTransfer.setData('text/plain','~a');" construction-id))
                ((a ,@(make-expand/collapse-link-parameters 
                       element-id-1 t "expand construction"))
                 ,construction-title))))))
	 (expanded-version 
	  (lambda ()
	    (funcall
             outer-div
             `(((div :class "title" :draggable "true"
                     :ondragstart
                     ,(format nil "event.dataTransfer.setData('text/plain','~a');" construction-id)) 
                ((a ,@(make-expand/collapse-link-parameters 
                       element-id-1 nil "collapse construction"))
                 ,construction-title)
                ,(if (attributes construction)
                   `((a ,@(make-expand/collapse-link-parameters 
                           element-id-2 t (if (not (string= (description construction) nil))
                                            "show attributes and description"
                                            "show attributes")))
                     ,(if (not (string= (description construction) nil))
                        " show description"
                        " show attributes"))
                   ""))
               ,(if (get-configuration configuration :remove-empty-units)
                  (fcg-light-cxn-structure->html (remove-empty-units construction) feature-types expand/collapse-all
                                                 :expanded-units expanded-units
                                                 :direction direction
                                                 :configuration configuration)
                  (fcg-light-cxn-structure->html construction feature-types expand/collapse-all
                                                 :expanded-units expanded-units
                                                 :direction direction
                                                 :configuration configuration))))))
	 (expanded-version-with-attributes
	  (when (attributes construction)
	    (lambda ()
              (funcall 
               outer-div
               `(((div :class "title" :draggable "true"
                       :ondragstart
                       ,(format nil "event.dataTransfer.setData('text/plain','~a');" construction-id)) 
                  ((a ,@(make-expand/collapse-link-parameters 
                         element-id-1 nil "collapse construction"))
                   ,construction-title))
                 ((table :style "border-collapse:collapse")
                  ((tbody)
                   ,@(when (not (string= (description construction) nil))
                       `(((tr)
                          ((td :class "attributes")
                           ,(description construction)))))
                   ((tr)
                    ((td :class "attributes")
                     ,(html-pprint (attributes construction))))
                   ((tr)
                    ,(if (get-configuration configuration :remove-empty-units)
                       (fcg-light-cxn-structure->html (remove-empty-units construction) feature-types expand/collapse-all
                                                      :expanded-units expanded-units
                                                      :direction direction
                                                      :configuration configuration)
                       (fcg-light-cxn-structure->html construction feature-types expand/collapse-all
                                                      :expanded-units expanded-units
                                                      :direction direction
                                                      :configuration configuration)))))))))))
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
;; construction-inventory
;; ---------------------------------------------------------

(export '(*saved-ci*))

(defvar *saved-ci* nil)

(define-css 'fcg-light-construction-inventory "
div.fcg-light-construction-inventory { padding:0px; display:inline-block; margin:3px; }
div.fcg-light-construction-inventory > div.title a { color:#40241A; }
div.fcg-light-construction-inventory-expanded { border:1px solid #40241A; }
div.fcg-light-construction-inventory-expanded > div.title { padding-left:5px;padding-right:5px;padding-top:1px;padding-bottom:2px; }
div.fcg-light-construction-inventory > table > tbody > tr { border-top:1px  #40241A;}
div.fcg-light-construction-inventory > table > tbody > tr > td { padding-left:5px; padding-bottom:2px;padding-top:2px;}
div.fcg-light-construction-inventory-sep { padding-left:0px; padding-right:0px;padding-top:0px;padding-bottom:0px;border-top:1px dashed #40241A;}
")

(defun ajax-obtain-ci-in-latex-aux (ci)
  "saves a text file with the latex representation of the selected construction"
  ;; creating a file with the cxn name in ./tmp folder
  (let ((file-name (concatenate 'string ".tmp/construction-set.tex"))
        (path-string (subseq (namestring (asdf:system-source-directory :fcg)) 0
                             (search "systems" (namestring (asdf:system-source-directory :fcg))))))
    (with-open-file (outputstream file-name
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
      (fcg-light-construction-set->latex ci :stream outputstream)
      (close outputstream)
      (open-file-in-os (concatenate 'string "file:///" path-string file-name)))))

(ht-simple-ajax:defun-ajax obtain-ci-in-latex (ci) (wi::*ajax-processor*)
  "Function that retrieves the latex form of the input construction inventory"
  (ajax-obtain-ci-in-latex-aux (get-wi-object ci))
  nil)

(ht-simple-ajax:defun-ajax find-cxn-in-ci (cxn-name ci) (wi::*ajax-processor*)
  "Function that searches the cxn-name in the ci and prints the cxn if it is in the ci"
  (let ((fcg-cxn (find-cxn cxn-name (get-wi-object ci) :key #'name :test #'string-equal)))
    (if fcg-cxn
      (progn
        (add-element `((h4) ,(format nil (concatenate 'string "Construction " cxn-name":"))))
        (add-element (make-html fcg-cxn)))
      (add-element `((h4) ,(format nil (concatenate 'string "Construction " cxn-name " not found"))))))
  nil)

(defmethod make-html ((ci fcg-construction-set)
                      &key
                      (expand-initially nil))
  (let* ((ci-id (make-id (type-of ci)))
         (element-id-1 (make-id 'construction-inventory))
	 (element-id-2 (make-id 'construction-inventory))
         (cxn-name-1 (replace-all (string (make-id "cxn-name")) "-" ""))
         (cxn-name-2 (replace-all (string (make-id "cxn-name")) "-" ""))
         (form-name-1 (replace-all (string (make-id "form")) "-" ""))
         (form-name-2 (replace-all (string (make-id "form")) "-" ""))
         (datalist-id (replace-all (string (make-id "datalist")) "-" ""))
         (cxnlist (mapcar #'downcase (mapcar #'name (constructions ci))))
	 (title (make-html-construction-inventory-title ci))
         (outer-div
          (lambda (expanded? children)
            (make-div-with-menu 
             :div-attributes (if expanded?
                               '(:class "fcg-light-construction-inventory fcg-light-construction-inventory-expanded")
                               '(:class "fcg-light-construction-inventory"))
             :menu-items 
             `(((a :style "font-family: Courier New;font-size: 16px;"
                   :href ,(format nil "javascript:ajax_obtain_ci_in_latex('~a');" ci-id)
                   :title "obtain the construction inventory in Latex")
                "L"))
             :div-children children)))
         (datalist `((datalist :id ,datalist-id)
                     ,@(loop for elem in cxnlist
                             collect `((option :value ,elem)))))
         (collapsed-version
          (lambda ()
	    (funcall 
             outer-div nil
             `(((div :class "title") 
                ((a ,@(make-expand/collapse-link-parameters 
                       element-id-1 t "show constructions")) 
                 ,title))))))
         (expanded-version-1
          ;; static html
          (if wi::*static-html*
            (lambda ()
              (funcall 
               outer-div t
               `(((div :class "title") 
                  ((a ,@(make-expand/collapse-link-parameters 
                         element-id-2 t "show more details"))
                   ,title))
                 ((div :class "fcg-light-construction-inventory-sep")
                  ((table :class "two-col")
                   ((tbody)
                    ((tr)
                     ((td :colspan "2") ,@(make-html-construction-inventory-body ci)))))))))
            ;; non-static html
            (lambda ()
              (funcall 
               outer-div t
               `(((div :class "title") 
                  ((a ,@(make-expand/collapse-link-parameters 
                         element-id-2 t "show more details"))
                   ,title))
                 ((div :class "fcg-light-construction-inventory-sep")
                  ,datalist)
                 ((table)
                  ((tbody)
                   ((tr)
                    ((td) ((form  :name ,form-name-1 :style "padding-top:12px;" :onkeypress "return event.keyCode != 13;")
                           ((input :list ,datalist-id :style "width:100%;font-size:9pt;"
                                   :rows "1" :name ,cxn-name-1 :title "type a construction name" :placeholder "search..."))))
                    ((td) ((a :style "color:#fff;padding-left:5px;padding-right:5px;padding-top:2px;padding-bottom:2px; border:1px solid #008; background-color: #008;"
                              :href ,(format nil (concatenate 'string "javascript:ajax_find_cxn_in_ci(document."
                                                              form-name-1
                                                              "."
                                                              cxn-name-1 
                                                              ".value,'~a')") ci-id)
                              :title "if available, it prints the construction in the web interface")
                           "Search")))))
                 ((div :class "fcg-light-construction-inventory-sep")
                  ((table :class "two-col")
                   ((tbody)
                    ((tr)
                     ((td :colspan "2") ,@(make-html-construction-inventory-body ci)))))))))))
         (expanded-version-2
          (if wi::*static-html*
            (lambda ()
              (funcall 
               outer-div t
               `(((div :class "title")
                  ((a ,@(make-expand/collapse-link-parameters 
                         element-id-1 nil "hide details"))
                   ,title))
                 ((div :class "fcg-light-construction-inventory-sep")
                  ,datalist
                  ((table :class "two-col")
                   ((tbody)
                    ((tr)
                     ((td) "feature-types")
                     ((td) ,(html-pprint (feature-types ci))))))
                  ((div :class "fcg-light-construction-inventory-sep"))
                  ((table :class "two-col")
                   ((tbody)
                    ((tr) 
                     ((td :colspan "2") ,@(make-html-construction-inventory-body ci)))))))))
            (lambda ()
              (funcall 
               outer-div t
               `(((div :class "title")
                  ((a ,@(make-expand/collapse-link-parameters 
                         element-id-1 nil "hide details"))
                   ,title))
                 ((div :class "fcg-light-construction-inventory-sep")
                  ,datalist
                  ((table :class "two-col")
                   ((tbody)
                    ((tr)
                     ((td) "feature-types")
                     ((td) ,(html-pprint (feature-types ci))))))
                  ((div :class "fcg-light-construction-inventory-sep"))
                  ((table)
                   ((tbody)
                    ((tr)
                     ((td) ((form :name ,form-name-2  :style "padding-top:12px;" :onkeypress "return event.keyCode != 13;")
                            ((input :list ,datalist-id :style "width:100%;font-size:9pt;"
                                    :rows "1" :name ,cxn-name-2 :title "type a construction name" :placeholder "search..."))))
                     ((td) ((a :style "color:#fff;padding-left:5px;padding-right:5px;padding-top:2px;padding-bottom:2px; border:1px solid #008; background-color: #008;"
                               :href ,(format nil (concatenate 'string "javascript:ajax_find_cxn_in_ci(document."
                                                               form-name-2
                                                               "."
                                                               cxn-name-2
                                                               ".value,'~a')") ci-id)
                               :title "if available, it prints the construction in the web interface")
                            "Search")))))
                  ((div :class "fcg-light-construction-inventory-sep"))
                  ((table :class "two-col")
                   ((tbody)
                    ((tr) 
                     ((td :colspan "2") ,@(make-html-construction-inventory-body ci))))))))))))
    (store-wi-object ci ci-id)
    (make-expandable/collapsable-element 
     element-id-1 (make-id 'cs) collapsed-version
     (make-expandable/collapsable-element 
      element-id-2 (make-id 'cs) expanded-version-1 expanded-version-2)
     :expand-initially expand-initially)))

(defun fcg-light-unit-with-subunits->html (unit
                                           structure
                                           right-to-left
                                           expand/collapse-all-id
                                           &key
                                           feature-types
                                           (expand-units nil)
                                           (depth 1)
                                           (direction nil)
                                           (modified-units nil)
                                           (added-units nil)
                                           (configuration *default-visualization-configuration*))
  "returns a table with a unit, lines to its subunits and the subunits themselves"
  (if (> depth 20)
    `((div :style "color:#f00") "giving up")
    (draw-node-with-children
     (transient-structure-unit->html unit feature-types expand/collapse-all-id
                                     :expand-units expand-units
                                     :modified-units modified-units
                                     :added-units added-units
                                     :configuration configuration)
     (loop for unit in (<subunits> unit structure :configuration configuration)
           collect (fcg-light-unit-with-subunits->html unit structure right-to-left expand/collapse-all-id
                                                       :depth (+ depth 1)
                                                       :expand-units expand-units
                                                       :direction direction
                                                       :modified-units modified-units
                                                       :added-units added-units
                                                       :configuration configuration
                                                       :feature-types feature-types))
     :right-to-left right-to-left)))

(defmethod make-html-fcg-light ((pole pole)
                                &key
                                feature-types
                                (expand-units nil)
                                (right-to-left nil)
                                expand/collapse-all-id
                                (direction nil)
                                (modified-units nil)
                                (added-units nil)
                                (configuration *default-visualization-configuration*))
  `((div)
    ,@(loop for unit in (<level-0-units> (pole-structure pole) :configuration configuration)
            collect `((div) ,(fcg-light-unit-with-subunits->html unit (pole-structure pole)
                                                                 right-to-left expand/collapse-all-id :direction direction
                                                                 :modified-units modified-units :added-units added-units
                                                                 :expand-units expand-units
                                                                 :configuration configuration
                                                                 :feature-types feature-types)))))

(defun print-fcg-light-transient-structure (ts)
  (let ((transient-structure (left-pole-structure ts))
        (resulting-string ""))
    (dolist (unit transient-structure)
      (when (not (equal (car unit) 'fcg:root))
        (setf resulting-string (concatenate 'string resulting-string (print-fcg-unit unit) "
"))))
    resulting-string))

(defun print-fcg-unit (unit)
  (let ((string-to-return (write-to-string (car unit))))
    (dolist (feature (cdr unit)) 
      (when (not (equal (car feature) 'fcg:footprints))
        (setf string-to-return (concatenate 'string string-to-return "
    "  (write-to-string feature )))))
    string-to-return))

(defun ajax-print-fcg-light-cfs-aux (cfs ci &optional node) ;;to print text to web interface
  (add-element `((hr)))
  (add-element 
   `((p) "Printing structure" ,(make-html-fcg-light cfs :construction-inventory ci :feature-types (feature-types
                                                                                                   (original-cxn-set ci)))
     ,@(when node `("of node&#160;&#160; " ,(make-html-fcg-light node :draw-children nil)))
     "&#x2192;"))
  (add-element `((pre) ,(let ((*print-right-margin* 100))
                          (cl-who:escape-string (format nil "~:w" 
                                                        (print-fcg-light-transient-structure cfs))))))
  (render-xml nil))
  
(ht-simple-ajax:defun-ajax print-fcg-light-cfs (id) (wi::*ajax-processor*)
  (let* ((cfs.ci (get-wi-object id)))
    (ajax-print-fcg-light-cfs-aux (car cfs.ci) (cdr cfs.ci)))
  nil)

(ht-simple-ajax:defun-ajax cfs-fcg-light-apply-all (id production?) (wi::*ajax-processor*)
  (let* ((cfs.ci (get-wi-object id))
         (cfs (car cfs.ci)) (ci (cdr cfs.ci))
         (direction (if (read-from-string production?) '-> '<-))
         (*make-html-cfs-construction-inventory* ci))
    (add-element '((hr)))
    (add-element `((p) "Applying all constructions of " 
                   ,(make-html (original-cxn-set ci)) " to "
                   ,(make-html-fcg-light cfs
                                         :configuration (configuration ci)
                                         :feature-types (feature-types (original-cxn-set ci)))
                   ,(if (eq direction '->) " in formulation " " in comprehension ")))
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
                  (add-element (make-html-fcg-light car
                                                    :configuration (configuration ci)
                                                    :feature-types (feature-types (original-cxn-set ci)))))
            (add-element '((p) "No construction could be applied")))))
  (render-xml nil))

(defun ajax-obtain-ts-in-latex-aux (ts)
  "saves a text file with the latex representation of the selected construction"
  ;; creating a file with the cxn name in ./tmp folder
  (let ((file-name (concatenate 'string ".tmp/transient-structure.tex"))
        (path-string (subseq (namestring (asdf:system-source-directory :fcg)) 0
                             (search "systems" (namestring (asdf:system-source-directory :fcg))))))
    (with-open-file (outputstream file-name
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
      (convert-transient-structure-to-latex ts :stream outputstream)
      (close outputstream)
      (open-file-in-os (concatenate 'string "file:///" path-string file-name)))))

(ht-simple-ajax:defun-ajax obtain-ts-in-latex (id) (wi::*ajax-processor*)
  "Function that retrieves the latex form of the input construction"
  (let ((transient-structure (get-wi-object id)))
    (ajax-obtain-ts-in-latex-aux transient-structure))
  nil)

(defun ajax-extract-meaning-cfs-as-predicate-network-aux (cfs ci &optional node)
  (let ((meaning (extract-meanings (left-pole-structure cfs))))
    (add-element `((hr)))
    (add-element 
     `((p) "Extracting meaning of structure",(make-html-fcg-light cfs
                                                                  :construction-inventory ci
                                                                  :feature-types (feature-types (original-cxn-set ci)))
       ,@(when node `("of node&#160;&#160; " ,(make-html-fcg-light node :draw-children nil)))
       " &#x2192; " ,(if (get-configuration (visualization-configuration (construction-inventory node)) :show-wiki-links-in-predicate-networks)
                       (predicate-network-with-wiki-links->svg meaning)
                       (predicate-network->svg meaning))))
    (render-xml nil)))

(ht-simple-ajax:defun-ajax extract-meaning-cfs-as-predicate-network (id) (wi::*ajax-processor*)
  (let* ((cfs.ci (get-wi-object id)))
    (ajax-extract-meaning-cfs-as-predicate-network-aux (car cfs.ci) (cdr cfs.ci))))

(defun ajax-save-fcg-light-cfs-aux (cfs ci &optional node)
  "Function that saves the fcg light cfs"
  (setf *saved-cfs* cfs)
  (when ci
    (setf *saved-ci* ci)) ;; necessary because sometimes the ci value is nil, but we still need one for drawing it 
  (add-element `((hr)))
  (add-element 
   `((p) "Saved structure"  ,(make-html-fcg-light *saved-cfs*
                                                  :construction-inventory (if ci
                                                                            ci
                                                                            *saved-ci*)
                                                  :feature-types (if ci
                                                                   (feature-types (original-cxn-set ci))
                                                                   (feature-types (original-cxn-set *saved-ci*))))
     ,@(when node `("of node&#160;&#160; " ,(make-html-fcg-light node :draw-children nil)))
     " to global variable " ((tt) ((b) "*saved-cfs*"))))
  (render-xml nil))

(ht-simple-ajax:defun-ajax save-fcg-light-cfs (id) (wi::*ajax-processor*)
  (let ((cfs.ci (get-wi-object id)))
    (ajax-save-fcg-light-cfs-aux (car cfs.ci) (cdr cfs.ci))))

(ht-simple-ajax:defun-ajax show-ts-in-fcg-2 (id) (wi::*ajax-processor*)
  "Function that retrieves the transient structure in FCG-2 format"
  (let ((ts (get-wi-object id)))
    (add-element `((hr)))
    (add-element `((p),(make-html (car ts))))
    nil))

(ht-simple-ajax:defun-ajax draw-ts-in-selected-hierarchy (element-id id sel-hierarchy-string) (wi::*ajax-processor*)
  "Function that draws the transient structure in the hierarchy selected"
  (let ((ts (get-wi-object id))
        (hierarchy (if (string-not-equal sel-hierarchy-string "NIL")
                     (let ((symbol (find-symbol sel-hierarchy-string 'FCG)))
                       (if symbol
                         symbol
                         (make-symbol sel-hierarchy-string)))
                     nil)))
    (set-configuration (visualization-configuration (cdr ts)) :selected-hierarchy hierarchy)
    (replace-element-content (string-downcase element-id)
                             (get-replacing-transient-structure (car ts)
                                                                id
                                                                :feature-types (feature-types (original-cxn-set (cdr ts)))
                                                                :construction-inventory (cdr ts)))
    nil))

(defun get-replacing-transient-structure (cfs
                                          cfs-id 
                                          &key
                                          feature-types
                                          (construction-inventory *make-html-cfs-construction-inventory*)
                                          (expand/collapse-all-id (make-id 'cfs))
                                          (expand-units nil)
                                          (configuration nil)
                                          (source-cfs nil))
  "this function returns the content of a transient structure, and it's used to replace the selected transient structure"
  (let ((configuration (or nil (visualization-configuration construction-inventory)))
        changes-at-transient-structure)
    (when source-cfs
      (setf changes-at-transient-structure (get-list-of-changes-at-transient-structure source-cfs cfs)))
    (let* ((element-id-1 (make-id 'cfs))
           (expand/collapse-all-id-combined (make-id 'cfs))
           (outer-div (lambda (children)
                        (make-div-with-menu 
                         :menu-items 
                         `(((a :style "font-family: Courier New;font-size: 16px;"
                               :href ,(format nil "javascript:ajax_save_fcg_light_cfs('~a');" cfs-id)
                               :title "save cfs")
                            "&#x22ba;") 
                           ((a :style "font-family: Courier New;font-size: 16px;"
                               :href ,(format nil "javascript:ajax_print_fcg_light_cfs('~a');" cfs-id)
                               :title "print cfs as text")
                            "&#x22ee;")
                           ,@(when 
                                 construction-inventory
                               `(((a :style "font-family: Courier New;font-size: 16px;"
                                     :href ,(format nil "javascript:ajax_render_cfs('~a','t');" cfs-id)
                                     :title "render cfs")
                                  "&#x22b3;")  
                                 ((a :style "font-family: Courier New;font-size: 16px;"
                                     :href ,(format nil "javascript:ajax_extract_meaning_cfs_as_predicate_network('~a');" cfs-id)
                                     :title "extract meaning of cfs")
                                  "&#x22b2;")
                                 ((a :style "font-family: Courier New;font-size: 16px;"
                                     :href ,(format nil "javascript:ajax_cfs_fcg_light_apply_all('~a','t');" cfs-id)
                                     :title "apply all constructions in formulation")
                                  "&#x226b;")
                                 ((a :style "font-family: Courier New;font-size: 16px;"
                                     :href ,(format nil "javascript:ajax_cfs_fcg_light_apply_all('~a','nil');" cfs-id)
                                     :title "apply all constructions in comprehension")
                                  "&#x226a;")
                                 ((a :style "font-family: Courier New;font-size: 16px;"
                                     :href ,(format nil "javascript:ajax_obtain_ts_in_latex('~a');" cfs-id)
                                     :title "obtain the transient structure in Latex")
                                  "L")
                                 ((a :style "font-family: Courier New;font-size: 16px;"
                                     :href ,(format nil "javascript:ajax_show_ts_in_fcg_2('~a');" cfs-id)
                                     :title "show the corresponding low-level transient-structure")
                                  "FCG2")
                                 ,@(when (and (> (length (get-configuration configuration :hierarchy-features)) 1)
                                              (specified-hierarchy-features? :configuration configuration))
                                     (let ((n 0))
                                       (loop for hf in (get-configuration configuration :hierarchy-features)
                                             do (incf n)
                                             collect
                                             `((a :style "font-family: Courier New;font-size: 16px;"
                                                  :href ,(format nil "javascript:ajax_draw_ts_in_selected_hierarchy('~a', '~a', '~a');"
                                                                 element-id-1
                                                                 cfs-id
                                                                 hf)
                                                  :title ,(format nil "use the feature '~a' to draw hierarchy" hf))
                                               ,(format nil "h~a" n))))))))
                         :div-children children)))
           (coupled-version 
            (lambda ()
              ()))
           (combined-version 
            (lambda ()
              (funcall
               outer-div
               `(((div :class "cxn_cfs cfs")
                  ((div :class "title") 
                   ((a ,@(make-expand/collapse-link-parameters element-id-1 nil "decouple cfs")) 
                    ((span) "transient structure")))
                  ((table :class "cxn_cfs cfs")
                   ((tbody)
                    ((tr)
                     ((td)
                      ((div :class "domain" :style "left:0px;") 
                       ,(make-expandable/collapsable-element 
                         (make-id 'pole) expand/collapse-all-id-combined
                         `((a ,@(make-expand/collapse-all-link-parameters 
                                 expand/collapse-all-id-combined t
                                 (if expand-units "collapsed" "expand")))
                           ,(if expand-units
                              (format nil "&#10752;&#160;")
                              (format nil "&#10753;&#160;")))
                         `((a ,@(make-expand/collapse-all-link-parameters 
                                 expand/collapse-all-id-combined nil
                                 (if expand-units "expand" "collapsed")))
                           ,(if expand-units
                              (format nil "&#10753;&#160;")
                              (format nil "&#10752;&#160;"))))))
                     ((td)
                      ,(if source-cfs
                         (make-html-fcg-light (combined-pole cfs)
                                              :expand-units expand-units
                                              :feature-types feature-types
                                              :expand/collapse-all-id expand/collapse-all-id-combined
                                              :modified-units (car (cddddr changes-at-transient-structure))
                                              :added-units (car (last changes-at-transient-structure))
                                              :configuration configuration)
                         (make-html-fcg-light (combined-pole cfs)
                                              :expand-units expand-units
                                              :feature-types feature-types
                                              :expand/collapse-all-id expand/collapse-all-id-combined
                                              :configuration configuration)))))))))))
           (div
            (lambda ()
              (make-expandable/collapsable-element 
               element-id-1 expand/collapse-all-id
               combined-version coupled-version
               :expand-initially nil))))
      (funcall div))))

;; #########################################################
;; coupled-feature-structure
;; ---------------------------------------------------------

(defmethod make-html-fcg-light ((cfs coupled-feature-structure) 
                                &key
                                feature-types
                                (expand-units nil)
                                (construction-inventory *make-html-cfs-construction-inventory*)
                                (expand/collapse-all-id (make-id 'cfs))
                                (configuration nil)
                                (source-cfs nil))
  ;; replacing methods is bad, but right now it's difficult otherwise
  (let ((configuration (cond (configuration configuration)
                             (construction-inventory
                              (visualization-configuration construction-inventory))
                             (t *default-visualization-configuration*)))
        (cfs-id (make-id 'cfs))
        changes-at-transient-structure)
    (when source-cfs
      (setf changes-at-transient-structure (get-list-of-changes-at-transient-structure source-cfs cfs)))
    (store-wi-object (cons cfs construction-inventory) cfs-id)
    (let* ((element-id-1 (make-id 'cfs))
           (expand/collapse-all-id-combined (make-id 'cfs))
           (outer-div (lambda (children)
                        (make-div-with-menu 
                         :menu-items 
                         `(((a :style "font-family: Courier New;font-size: 16px;"
                               :href ,(format nil "javascript:ajax_save_fcg_light_cfs('~a');" cfs-id)
                               :title "save cfs")
                            "&#x22ba;") 
                           ((a :style "font-family: Courier New;font-size: 16px;"
                               :href ,(format nil "javascript:ajax_print_fcg_light_cfs('~a');" cfs-id)
                               :title "print cfs as text")
                            "&#x22ee;")
                           ,@(when 
                                 construction-inventory
                               `(((a :style "font-family: Courier New;font-size: 16px;"
                                     :href ,(format nil "javascript:ajax_render_cfs('~a', 't');" cfs-id)
                                     :title "render cfs")
                                  "&#x22b3;")  
                                 ((a :style "font-family: Courier New;font-size: 16px;"
                                     :href ,(format nil "javascript:ajax_extract_meaning_cfs_as_predicate_network('~a');" cfs-id)
                                     :title "extract meaning of cfs")
                                  "&#x22b2;")
                                 ((a :style "font-family: Courier New;font-size: 16px;"
                                     :href ,(format nil "javascript:ajax_cfs_fcg_light_apply_all('~a', 't');" cfs-id)
                                     :title "apply all constructions in formulation")
                                  "&#x226b;")
                                 ((a :style "font-family: Courier New;font-size: 16px;"
                                     :href ,(format nil "javascript:ajax_cfs_fcg_light_apply_all('~a','nil');" cfs-id)
                                     :title "apply all constructions in comprehension")
                                  "&#x226a;")
                                 ((a :style "font-family: Courier New;font-size: 16px;"
                                     :href ,(format nil "javascript:ajax_obtain_ts_in_latex('~a');" cfs-id)
                                     :title "obtain the transient structure in Latex")
                                  "L")
                                 ((a :style "font-family: Courier New;font-size: 16px;"
                                     :href ,(format nil "javascript:ajax_show_ts_in_fcg_2('~a');" cfs-id)
                                     :title "show the corresponding low-level transient-structure")
                                  "FCG2")
                                 ,@(when 
                                       (and (> (length (get-configuration configuration :hierarchy-features)) 1)
                                            (specified-hierarchy-features? :configuration configuration))
                                     (let ((n 0))
                                       (loop for hf in (get-configuration configuration :hierarchy-features)
                                             do (incf n)
                                             collect
                                             `((a :style "font-family: Courier New;font-size: 16px;"
                                                  :href ,(format nil "javascript:ajax_draw_ts_in_selected_hierarchy('~a', '~a', '~a');"
                                                                 element-id-1
                                                                 cfs-id
                                                                 hf)
                                                  :title ,(format nil "use the feature '~a' to draw hierarchy" hf))
                                               ,(format nil "h~a" n))))))))
                         :div-children children)))
           (coupled-version 
            (lambda ()
              ()))
           (combined-version 
            (lambda ()
              (funcall
               outer-div
               `(((div :class "cxn_cfs cfs")
                  ((div :class "title") 
                   ((a ,@(make-expand/collapse-link-parameters element-id-1 nil "decouple cfs")) 
                    ((span) "transient structure")))
                  ((table :class "cxn_cfs cfs")
                   ((tbody)
                    ((tr)
                     ((td)
                      ((div :class "domain" :style "left:0px;") 
                       ,(make-expandable/collapsable-element 
                         (make-id 'pole) expand/collapse-all-id-combined
                         `((a ,@(make-expand/collapse-all-link-parameters 
                                 expand/collapse-all-id-combined t
                                 (if expand-units "collapse" "expand")))
                           ,(if expand-units
                              (format nil "&#10752;&#160;")
                              (format nil "&#10753;&#160;")))
                         `((a ,@(make-expand/collapse-all-link-parameters 
                                 expand/collapse-all-id-combined nil
                                 (if expand-units "expand" "collapse")))
                           ,(if expand-units
                              (format nil "&#10753;&#160;")
                              (format nil "&#10752;&#160;"))))))
                     ((td)
                      ,(if source-cfs
                         (make-html-fcg-light (combined-pole cfs)
                                              :expand-units expand-units
                                              :feature-types feature-types
                                              :expand/collapse-all-id expand/collapse-all-id-combined
                                              :modified-units (car (cddddr changes-at-transient-structure))
                                              :added-units (car (last changes-at-transient-structure))
                                              :configuration configuration)
                         (make-html-fcg-light (combined-pole cfs)
                                              :expand-units expand-units
                                              :feature-types feature-types
                                              :expand/collapse-all-id expand/collapse-all-id-combined
                                              :configuration configuration))))))))))))
      (make-expandable/collapsable-element 
       element-id-1 expand/collapse-all-id
       combined-version coupled-version
       :expand-initially nil))))

;; #########################################################
;; cxn-application-result
;; ---------------------------------------------------------

(defun select-fcg-light-bindings (fcg-bindings source-ts)
  "function that selects the fcg-light bindings, subset of the fcg bindings"
  (let ((fcg-light-bindings '()))
    (loop for binding in fcg-bindings
          do
          (when (not (search "?TAG" (write-to-string (car binding)))) ;; when not tag
            (if (member-of-tree (first binding) (left-pole-structure source-ts)) ;; if first var in binding is from the ts
              (pushend binding fcg-light-bindings) ;; show the binding pair as it is
              (let ((first-elem (intern
                                 (get-properly-binding-name (first binding))))
                    (second-elem (rest binding)))
                ;; strip the sufix from the first binding in the binding pair and add the couple to the bindings
                (pushend (cons first-elem second-elem) fcg-light-bindings)))))
    fcg-light-bindings))

(defmethod make-html-fcg-light ((car cxn-application-result)
                                &key
                                (expand-units nil)
                                (feature-types (original-cxn-set *saved-ci*))
                                (configuration nil))
  (let ((configuration (or configuration (visualization-configuration (cxn-inventory (car-applied-cxn car))))))
    `((table :class "car")
      ((tbody)
       ((tr)
        ((td) "status")
        ((td :style ,(format nil "color:~a" (status-color (car-status car))))
         ((tt) ,(format nil "~(~a~)" (car-status car)))))
       ,(if (not (eq (car-status car) 'initial))
          `((tr)
            ((td) "source structure")
            ((td) ,(make-html-fcg-light (car-source-cfs car)
                                        :configuration configuration
                                        :feature-types feature-types
                                        :expand-units expand-units
                                        :construction-inventory (when (car-applied-cxn car)
                                                                  (cxn-inventory (car-applied-cxn car))))))
          "")
       ,(if (car-applied-cxn car) 
          `((tr)
            ((td) "applied construction")
            ((td) ,(make-html (get-original-cxn (car-applied-cxn car))
                              :direction (car-direction car)
                              :expand-initially t
                              :wrap-in-paragraph nil
                              :expand-units expand-units
                              :configuration configuration)))
          "")
       ,(if (and (not (member (car-status car) '(cxn-applied cxn-matched)))
                 (car-match-bindings car))
          `((tr) 
            ((td) "bindings after matching")
            ((td) ,(html-pprint (select-fcg-light-bindings (car-match-bindings car) (car-source-cfs car)))))
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
                ,(make-html-fcg-light (make-instance 'pole :structure (car-first-merge-structure car))
                                      :feature-types feature-types
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
            ((td) ,(html-pprint (select-fcg-light-bindings (car-first-merge-bindings car) (car-source-cfs car)))))
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
                ,(make-html-fcg-light (make-instance 'pole :structure (car-second-merge-structure car))
                                      :feature-types feature-types
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
       ,(if (and (not (eq (car-status car) 'initial))
                 (car-resulting-cfs car))
          `((tr)
            ((td) "resulting structure")
            ((td) ,(make-html-fcg-light (car-resulting-cfs car)
                                        :configuration configuration
                                        :feature-types feature-types
                                        :expand-units expand-units
                                        :construction-inventory (when (car-applied-cxn car )
                                                                  (cxn-inventory (car-applied-cxn car)))
                                        :source-cfs (car-source-cfs car))))
          "")
       ,(if (car-second-merge-bindings car)
          `((tr)
            ((td) "resulting bindings")
            ((td) ,(html-pprint (select-fcg-light-bindings (car-second-merge-bindings car) (car-source-cfs car)))))
          "")
       ,(if  (and
              (get-configuration configuration :add-form-and-meaning-to-car)
              (eq (car-direction car) '<-)
              (car-resulting-cfs car))
          `((tr)
            ((td) "meaning")
            ((td) ,(if (get-configuration configuration :show-wiki-links-in-predicate-networks)
                       (predicate-network-with-wiki-links->svg (extract-meanings (left-pole-structure (car-resulting-cfs car))))
                       (predicate-network->svg (extract-meanings (left-pole-structure (car-resulting-cfs car)))))))
          "")
       ,(if  (and
              (get-configuration configuration :add-form-and-meaning-to-car)
              (eq (car-direction car) '->)
              (car-resulting-cfs car))
          `((tr)
            ((td) "form")
            ((td) ,(make-html (extract-forms (left-pole-structure (car-resulting-cfs car))))))
          "")
       ,(if (and (car-first-merge-added car) (not (car-resulting-cfs car)))
          `((tr)
            ((td) "added in first merge")
            ((td) ,(make-html-fcg-light (make-instance 'pole :structure (car-first-merge-added car))
                                        :configuration configuration
                                        :feature-types feature-types)))
          "")
       ,(if (and (car-second-merge-added car) (not (car-resulting-cfs car)))
          `((tr)
            ((td) "added in second merge")
            ((td) ,(make-html-fcg-light (make-instance 'pole :structure (car-second-merge-added car))
                                        :configuration configuration
                                        :feature-types feature-types)))
       "")))))

;; #########################################################
;; cip-node
;; ---------------------------------------------------------

(ht-simple-ajax:defun-ajax save-cipn-fcg-light (id) (wi::*ajax-processor*)
  (setf *saved-cipn* (get-wi-object id))
  (add-element `((hr)))
  (add-element `((p) "Saved node&#160;&#160; " 
                 ,(make-html-fcg-light *saved-cipn* :draw-children nil)
		 " to global variable " ((tt) ((b) "*saved-cipn*"))))
  (render-xml nil))

(ht-simple-ajax:defun-ajax save-cipn-cfs-fcg-light (id) (wi::*ajax-processor*)
  (let ((cipn (get-wi-object id)))
    (ajax-save-fcg-light-cfs-aux
     (car-resulting-cfs (cipn-car cipn)) (construction-inventory (cip cipn)) cipn)))

(ht-simple-ajax:defun-ajax print-cipn-cfs-fcg-light (id) (wi::*ajax-processor*)
  (let ((cipn (get-wi-object id)))
    (ajax-print-fcg-light-cfs-aux 
     (car-resulting-cfs (cipn-car cipn)) (construction-inventory (cip cipn)) cipn)))

(ht-simple-ajax:defun-ajax extract-meaning-cipn-as-predicate-network (id) (wi::*ajax-processor*)
  (let ((cipn (get-wi-object id)))
    (ajax-extract-meaning-cfs-as-predicate-network-aux 
     (car-resulting-cfs (cipn-car cipn)) (construction-inventory (cip cipn)) cipn)))

(web-interface::defun-ajax update_selected_structure_fcg_light (info_struct_id cipn_id) (web-interface::*ajax-processor*)
  "Called from the html page to replace the content of one element"
  (replace-element-content (string-downcase info_struct_id) (get-replacing-content-fcg (string-downcase info_struct_id) (get-current-cipn (mkstr (string-upcase cipn_id)))))
  nil)

(defun check-if-children-already-present (children-name node)
  "returns true if the node has a child with the same name, nil otherwise"
  (loop for child in (children node)
        when (search children-name (write-to-string (cipn-car child)))
        return t))

(ht-simple-ajax:defun-ajax drop-cxn (id-cxn id-cipn subtree-id) (wi::*ajax-processor*)
  "applies the cxn-id on the resulting-cfs of the node with id-cipn"
  (let* ((dropped-fcg-cxn (get-wi-object id-cxn))
         (cip-node (get-wi-object id-cipn)))
    (when (and dropped-fcg-cxn cip-node)
      (let ((dropped-cxn (find-cxn (get-wi-object id-cxn) (processing-cxn-inventory (cxn-inventory dropped-fcg-cxn)) :test #'string=)))
        (multiple-value-bind (succeeded-cars failed-cars)
            (fcg-apply dropped-cxn (car-resulting-cfs (cipn-car cip-node))
                       (car-direction (cipn-car cip-node)) :notify nil)
          (cond ((and succeeded-cars
                      (not (check-if-children-already-present (write-to-string (first succeeded-cars)) cip-node)))
                 (cip-enqueue (cip-add-child cip-node (first succeeded-cars))
                              (cip cip-node) (get-configuration (cip cip-node) :queue-mode))
                 (replace-element-content subtree-id
                                          (make-html-fcg-light (top-node (cip cip-node))
                                                               :subtree-id subtree-id)))
                ((and failed-cars
                      (not (check-if-children-already-present (write-to-string (first failed-cars)) cip-node)))
                 (cip-enqueue (cip-add-child cip-node (first failed-cars))
                              (cip cip-node) (get-configuration (cip cip-node) :queue-mode))
                 (replace-element-content subtree-id
                                          (make-html-fcg-light (top-node (cip cip-node))
                                                               :subtree-id subtree-id)))
                (t
                 (let* ((matching-pattern (matching-pattern dropped-cxn
                                                            (car-direction (cipn-car cip-node))))
                        (anti-unify-result (anti-unify matching-pattern (left-pole-structure
                                                                         (car-resulting-cfs (cipn-car cip-node))) :fcg))
                        (bindings (loop for elem in (cadar anti-unify-result)
                                        for el in (caddar anti-unify-result)
                                        collect `(,(car elem) . ,(car el))))
                        (conflicting-values (get-duplicate-elements-and-values (mapcar #'car bindings)))
                        (discarded-features (fourth (first anti-unify-result))))
                   ;; Present conflicting values
                   (when conflicting-values
                     (add-element `((h4) "The following elements in the cxn give conflicts in matching: "))
                     (loop for (element . rest) on conflicting-values
                           do (add-element (get-highlighted-element (get-properly-binding-name element) :if-string-print-as-string nil))
                           when rest do (add-element `((br))))
                     (add-element `((h4) "with the following elements in the transient structure: "))
                     (loop for element in bindings
                           when (find (car element) conflicting-values)
                           do (progn (add-element (get-highlighted-element (cdr element) :if-string-print-as-string nil))
                                (add-element `((br))))))
                   ;; Present discarded features
                   (when discarded-features
                     (add-element `((h4) "The following features from the cxn were required but not found in the transient structure: "))
                     (loop for element in discarded-features
                           do (progn
                                (add-element
                                 (make-html element))
                                (add-element `((br))))))
                   ;; Print Transient Structure
                   (when (or conflicting-values discarded-features)
                     (add-element `((br)))
                     (add-element (make-html-fcg-light (car-resulting-cfs (cipn-car cip-node))
                                                       :construction-inventory (cxn-inventory (get-wi-object id-cxn))
                                                       :feature-types (feature-types (cxn-inventory (get-wi-object id-cxn))))))
                   (cond ((eq nil anti-unify-result)
                          (add-element `((h4) "Unfortunately, Anti-Unification could not determine why the construction does not match")))
                         ((eq (length anti-unify-result) 1)
                          (add-element '((br)))
                          (add-element `((a :href ,(format nil "javascript:ajax_show_anti_unification_result('~a','~a');" 
                                                           anti-unify-result  (if (equal (car-direction (cipn-car cip-node)) '->)
                                                                                "t"
                                                                                "nil"))
                                            :title "apply to *saved-cfs* in formulation") "Show entire anti-unification anlysis")))
                         (t
                          (add-element `((h4) "Anti-Unification yielded multiple analyses, only the most probable one is shown."))
                          (add-element `((a :href ,(format nil "javascript:ajax_show_anti_unification_result('~a','~a');" 
                                                           anti-unify-result  (if (equal (car-direction (cipn-car cip-node)) '->)
                                                                                "t"
                                                                                "nil"))
                                            :title "apply to *saved-cfs* in formulation") "Show entire anti-unification analysis")))))))))))
  nil)

(ht-simple-ajax:defun-ajax drop-cxn-and-replace (id-cxn id-cipn subtree-id info-struct-id) (wi::*ajax-processor*)
  "applies the cxn-id on the resulting-cfs of the node with id-cipn"
  (let* ((dropped-fcg-cxn (get-wi-object id-cxn))
         (node-id-1 (make-id 'chain-element-id))
         (cip-node (get-current-cipn (mkstr (string-upcase id-cipn)))))
    (when (and dropped-fcg-cxn cip-node)
      (let ((dropped-cxn (find-cxn (get-wi-object id-cxn) (processing-cxn-inventory (cxn-inventory dropped-fcg-cxn)) :test #'string=)))
        (multiple-value-bind (succeeded-cars failed-cars)
            (fcg-apply dropped-cxn (car-resulting-cfs (cipn-car cip-node))
                       (car-direction (cipn-car cip-node)) :notify nil)
          (cond ((and succeeded-cars
                      (not (check-if-children-already-present (write-to-string (first succeeded-cars)) cip-node)))
                 (let ((new-cipn-node (cip-add-child cip-node (first succeeded-cars))))
                   (cip-enqueue new-cipn-node
                                (cip cip-node) (get-configuration (cip cip-node) :queue-mode))
                   (replace-element-content subtree-id
                                            (make-html-fcg-light (top-node (cip cip-node))
                                                                 :struct-id info-struct-id
                                                                 :subtree-id subtree-id))
                   (store-expanded-node (mkstr node-id-1) nil)
                   (store-current-cipn new-cipn-node (mkstr node-id-1))
                   (setf *last-cipn* (mkstr node-id-1))
                   (update_selected_structure_fcg_light info-struct-id (mkstr node-id-1))))
                ((and failed-cars
                      (not (check-if-children-already-present (write-to-string (first failed-cars)) cip-node)))
                 (let ((new-cipn-node (cip-add-child cip-node (first failed-cars))))
                   (cip-enqueue new-cipn-node
                                (cip cip-node) (get-configuration (cip cip-node) :queue-mode))
                   (replace-element-content subtree-id
                                            (make-html-fcg-light (top-node (cip cip-node))
                                                                 :struct-id info-struct-id
                                                                 :subtree-id subtree-id))
                   (store-expanded-node (mkstr node-id-1) nil)
                   (store-current-cipn new-cipn-node (mkstr node-id-1))
                   (setf *last-cipn* (mkstr node-id-1))
                   (update_selected_structure_fcg_light info-struct-id (mkstr node-id-1))))
                (t
                 (let* ((matching-pattern (matching-pattern dropped-cxn
                                                            (car-direction (cipn-car cip-node))))
                        (anti-unify-result (anti-unify matching-pattern (left-pole-structure
                                                                         (car-resulting-cfs (cipn-car cip-node))) :fcg))
                        (bindings (loop for elem in (cadar anti-unify-result)
                                        for el in (caddar anti-unify-result)
                                        collect `(,(car elem) . ,(car el))))
                        (conflicting-values (get-duplicate-elements-and-values (mapcar #'car bindings)))
                        (discarded-features (fourth (first anti-unify-result))))
                   ;; Present conflicting values
                   (when conflicting-values
                     (add-element `((h4) "The following elements in the cxn give conflicts in matching: "))
                     (loop for (element . rest) on conflicting-values
                           do (add-element (get-highlighted-element (get-properly-binding-name element) :if-string-print-as-string nil))
                           when rest do (add-element `((br))))
                     (add-element `((h4) "with the following elements in the transient structure: "))
                     (loop for element in bindings
                           when (find (car element) conflicting-values)
                           do (progn (add-element (get-highlighted-element (cdr element) :if-string-print-as-string nil))
                                (add-element `((br))))))
                   ;; Present discarded features
                   (when discarded-features
                     (add-element `((h4) "The following features from the cxn were required but not found in the transient structure: "))
                     (loop for element in discarded-features
                           do (progn
                                (add-element
                                 (make-html element))
                                (add-element `((br))))))
                   ;; Print Transient Structure
                   (when (or conflicting-values discarded-features)
                     (add-element `((br)))
                     (add-element (make-html-fcg-light (car-resulting-cfs (cipn-car cip-node))
                                                       :construction-inventory (cxn-inventory (get-wi-object id-cxn))
                                                       :feature-types (feature-types (cxn-inventory (get-wi-object id-cxn))))))
                   (cond ((eq nil anti-unify-result)
                          (add-element `((h4) "Unfortunately, Anti-Unification could not determine why the construction does not match")))
                         ((eq (length anti-unify-result) 1)
                          (add-element '((br)))
                          (add-element `((a :href ,(format nil "javascript:ajax_show_anti_unification_result('~a','~a');" 
                                                           anti-unify-result  (if (equal (car-direction (cipn-car cip-node)) '->)
                                                                                "t"
                                                                                "nil"))
                                            :title "apply to *saved-cfs* in formulation") "Show entire anti-unification anlysis")))
                         (t
                          (add-element `((h4) "Anti-Unification yielded multiple analyses, only the most probable one is shown."))
                          (add-element `((a :href ,(format nil "javascript:ajax_show_anti_unification_result('~a','~a');" 
                                                           anti-unify-result  (if (equal (car-direction (cipn-car cip-node)) '->)
                                                                                "t"
                                                                                "nil"))
                                            :title "apply to *saved-cfs* in formulation") "Show entire anti-unification analysis")))))))))))
  nil)

(defun extended-make-expand/collapse-link-parameters-fcg-light (element-id
                                                                info-struct-id
                                                                &key
                                                                (title nil))
  "to be used when creating a <a >click</a> link for expanding/ collapsing nodes"
  `(:href ,(format nil "javascript:ajax_update_node_to_collapse_func('~(~a~)'); expand('~(~a~)');ajax_update_selected_structure_fcg_light (~('~a', '~a'~)); "
                   element-id element-id info-struct-id element-id)
    :title ,(if title title "expand")))

(defmethod make-html-cipn-title-fcg ((cipn cip-node)
                                     (construction-inventory construction-inventory))
  `((span :style ,(mkstr (cond ((or (eq (car (statuses cipn)) 'succeeded)
                                    (and (fully-expanded? cipn)
                                         (not (children cipn))))
                                "font-weight:bold;")
                               ((not (fully-expanded? cipn)) "font-style:italic;")
                               (t ""))))
    ,(if (car-applied-cxn (cipn-car cipn))
       (make-html-construction-title
        ;; I think it is safer to get it from the construction application result. Then it also works for constructions
        ;; that are not in the construction inventory, but for example added by the meta-layer
        (car-applied-cxn (cipn-car cipn))) ;(find-cxn (name (car-applied-cxn (cipn-car cipn))) construction-inventory)
       "initial")))

(defmethod make-html-fcg-light ((cipn cip-node)
                                &key
                                (struct-id nil)
                                (draw-children t)
                                (hide-subtrees-with-duplicates t)
                                (subtree-id nil)
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
         (with-search-debug-data (get-configuration configuration :with-search-debug-data))
         (feature-types (feature-types (original-cxn-set construction-inventory)))
	 (node-color (cipn-node-color cipn cip))
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
               ,(make-html-cipn-title-fcg cipn construction-inventory)))))
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
            `((div :class "cipn" :id ,(mkstr node-id)
                   :ondrop ,(format nil
                                    "javascript:ajax_drop_cxn(event.dataTransfer.getData('text/plain'),'~a','~a');"
                                    node-id
                                    (if subtree-id
                                      subtree-id
                                      (string-downcase (mkstr node-id-1))))
                   :ondragover "event.preventDefault()")
              ,(make-div-with-menu 
                :div-attributes `(:style ,(mkstr "border:1px solid " node-color))
                :menu-items 
                `(,@(when (and within-linear-chain draw-children)
                      `(((a :style "font-family: Courier New;font-size: 16px;"
                            ,@(make-expand/collapse-link-parameters
                               linear-chain-id nil 
                               "compact linear chain of nodes"))
                         "&#x2217;") ))
                  ((a :style "font-family: Courier New;font-size: 16px;"
                      :href ,(format nil "javascript:ajax_save_cipn_fcg_light('~a');" node-id)
                      :title "save node")
                   "&#x22a4;")
                  ,@(when 
                        (car-resulting-cfs (cipn-car cipn))
                      `(((a :style "font-family: Courier New;font-size: 16px;"
                            :href ,(format nil "javascript:ajax_save_cipn_cfs_fcg_light('~a');" node-id)
                            :title "save cfs")
                         "&#x22ba;")
                        ((a :style "font-family: Courier New;font-size: 16px;"
                            :href ,(format nil "javascript:ajax_print_cipn_cfs_fcg_light('~a');" node-id)
                            :title "print cfs as text")
                         "&#x22ee;")
                        ,@(when 
                              (loop for node in (queue cip)
                                    thereis (or (eq node cipn)
                                                (find cipn (all-parents node))))
                            `(((a :style "font-family: Courier New;font-size: 16px;"
                                  :href ,(format nil "javascript:ajax_cipn_next_solution('~a', 't');" node-id)
                                  :title "get next solution below this node")
                               "&#x227b;") 
                              ((a :style "font-family: Courier New;font-size: 16px;"
                                  :href ,(format nil "javascript:ajax_cipn_all_solutions('~a', 't');" node-id)
                                  :title "get all solutions below this node")
                               "&#x226b;")))
                        ((a :style "font-family: Courier New;font-size: 16px;"
                            :href ,(format nil "javascript:ajax_render_cipn('~a','t');" node-id)
                            :title "render cfs to utterance")
                         "&#x22b3;") 
                        ((a :style "font-family: Courier New;font-size: 16px;"
                            :href ,(format nil "javascript:ajax_extract_meaning_cipn_as_predicate_network('~a');" node-id)
                            :title "extract meaning of cfs")
                         "&#x22b2;"))))
                :div-children div-children))))
         (outer-div-with-menu-replace
          (lambda (div-children)
            `((div :class "cipn" :id ,(mkstr node-id)
                   :ondrop ,(format nil
                                    "javascript:ajax_drop_cxn_and_replace(event.dataTransfer.getData('text/plain'),'~a','~a','~a');"
                                    node-id-1
                                    (if subtree-id
                                      subtree-id
                                      (string-downcase (mkstr node-id-1)))
                                    struct-id)
                   :ondragover "event.preventDefault()")
              ,(make-div-with-menu 
                :div-attributes `(:style ,(mkstr "border:1px solid " node-color))
                :menu-items 
                `()
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
                                   ,(make-html-fcg-light (car-resulting-cfs (cipn-car cipn))
                                                         :feature-types feature-types
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
                           ((tr) ((td) ,(make-html-fcg-light (cipn-car cipn)
                                                             :feature-types feature-types
                                                             :configuration configuration)))
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
                                                                  :render-mode)
                                               :node cipn)))
                                        (make-html utterance)))))))))))))))))
            (lambda ()
              (make-expandable/collapsable-element 
               node-id-1 node-id-2
               ;; the unexpanded version
               (lambda ()
                 (store-expanded-node (mkstr node-id-1) nil)
                 (store-current-cipn cipn (mkstr node-id-1))
                 (setf *last-cipn* (mkstr node-id-1))
                 (funcall
                  outer-div-with-menu-replace
                  (list
                   (funcall title-div nil
                            (extended-make-expand/collapse-link-parameters-fcg-light
                             node-id-1 struct-id :title "select structure to show")))))
               (lambda ()
                 (funcall
                  outer-div-with-menu-replace
                  (list
                   (funcall title-div t
                            (extended-make-expand/collapse-link-parameters-fcg-light
                             node-id-1 struct-id :title "unselect")))))))))
         (tree
          (lambda ()
            (draw-node-with-children
             (funcall div)
             (unless (eq cipn last-node-of-linear-chain)
               (reverse 
                (loop for child in (children cipn)
                      collect (if (get-configuration configuration :expand-nodes-in-search-tree)
                                (make-html-fcg-light 
                                 child 
                                 :tree-id tree-id
                                 :subtree-id (if subtree-id
                                               subtree-id
                                               (string-downcase (mkstr node-id-1)))
                                 :last-node-of-linear-chain last-node-of-linear-chain
                                 :within-linear-chain? within-linear-chain
                                 :linear-chain-id linear-chain-id
                                 :hide-subtrees-with-duplicates (and hide-subtrees-with-duplicates
                                                                     (not complete-sub-tree-is-duplicate)))
                                (make-html-fcg-light 
                                 child 
                                 :struct-id struct-id
                                 :tree-id tree-id
                                 :subtree-id (if subtree-id
                                               subtree-id
                                               (string-downcase (mkstr node-id-1)))
                                 :last-node-of-linear-chain last-node-of-linear-chain
                                 :within-linear-chain? within-linear-chain
                                 :linear-chain-id linear-chain-id
                                 :hide-subtrees-with-duplicates (and hide-subtrees-with-duplicates
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
                   append `(,(make-html-cipn-title-fcg (car node) construction-inventory)
                            ,@(when (cdr node) '(", "))))))
        (funcall tree))
       (loop for child in (children (car (last linear-chain)))
             collect (if (get-configuration configuration :expand-nodes-in-search-tree)
                       (make-html-fcg-light 
                        child 
                        :tree-id tree-id
                        :subtree-id (if subtree-id
                                      subtree-id
                                      (string-downcase (mkstr node-id-1)))
                        :hide-subtrees-with-duplicates hide-subtrees-with-duplicates)
                       (make-html-fcg-light 
                        child
                        :struct-id struct-id
                        :tree-id tree-id
                        :subtree-id (if subtree-id
                                      subtree-id
                                      (string-downcase (mkstr node-id-1)))
                        :hide-subtrees-with-duplicates hide-subtrees-with-duplicates)))
       :line-width "1px" :style "solid" :color "#888" :width "10px"))      
     (t (funcall tree)))))

;; #########################################################
;; construction-inventory-processor
;; ---------------------------------------------------------

(defun get-applied-fcg-light-constructions (applied-cxns)
  "returns the list of applied cxns in its fcg-light format"
  (let ((fcg-light-applied-cxns '()))
    (loop for applied-cxn in applied-cxns
          do
          (push (get-original-cxn applied-cxn) fcg-light-applied-cxns))
    fcg-light-applied-cxns))

(defun make-tr-for-cip-queue-fcg-light (cip title &key (configuration nil))
  (let* ((*make-html-cfs-construction-inventory* (construction-inventory cip))
         (configuration (or configuration (visualization-configuration *make-html-cfs-construction-inventory*)))
         (subtree-id (mkstr (make-id 'subtree-id))))
    `((tr)
      ((td) ,title)
      ((td) ((div :id ,subtree-id)
             ,@(html-hide-rest-of-long-list 
                (queue cip) 5
                #'(lambda (node)
                    (make-html-fcg-light 
                     node
                     :subtree-id subtree-id
                     :draw-children nil 
                     :configuration configuration))))))))

(defun make-tr-for-cip-tree-fcg-light (cipn
                                       title
                                       &key
                                       (hide-subtree-with-duplicates t)
                                       (configuration nil))
  (let* ((*make-html-cfs-construction-inventory* (construction-inventory (cip cipn)))
         (configuration (or configuration (visualization-configuration *make-html-cfs-construction-inventory*)))
         (subtree-id (mkstr (make-id 'subtree-id))))
    `((tr)
      ((td) ,title)
      ((td) ((div :id ,subtree-id)
             ,(make-html-fcg-light 
               cipn
               :subtree-id subtree-id
               :hide-subtrees-with-duplicates hide-subtree-with-duplicates
               :configuration configuration))))))

(defmethod make-html-fcg-light ((cip construction-inventory-processor) 
                                &key
                                (feature-types (feature-types (original-cxn-set (construction-inventory cip))))
                                (solutions nil solutions-provided-p)
                                (show-queue nil)
                                (configuration nil))
  (declare (ignore solutions-provided-p))
  (let ((*make-html-cfs-construction-inventory* (construction-inventory cip))
        (subtree-id (mkstr (make-id 'subtree-id)))
        (info-struct-id-name (make-id 'info-struct))
        (configuration (or configuration (visualization-configuration (construction-inventory cip)))))
    (reset-expanded-node)
    (if (get-configuration configuration :expand-nodes-in-search-tree)
      `((div)
        ((table :class "two-col")
         ((tbody)
          ((tr)
           ((td) "initial structure")
           ((td) ,(make-html-fcg-light (initial-cfs cip)
                                       :configuration configuration :feature-types feature-types)))
          ,(make-tr-for-cip-tree-fcg-light (top-node cip) "application process"
                                           :configuration configuration)
          ,(if (get-configuration configuration :show-constructional-dependencies)
             `((tr)
               ((td) "constructional dependencies")
               ((td) ,(make-html (analyse-solution (first solutions) (direction cip))
                                 :configuration configuration)))
             "")
          ,(if (and show-queue (queue cip))
             (make-tr-for-cip-tree-fcg-light cip "queue"
                                             :configuration configuration)
             "")
          ,@(loop for solution in solutions 
                  for n from 1
                  append 
                  `(,(if (> (length solutions) 1)
                       `((tr) ((td :colspan "2") ((b) "solution " ,n)))
                       "")
                    ((tr)
                     ((td) "applied constructions")
                     ((td) ,@(html-hide-rest-of-long-list 
                              (get-applied-fcg-light-constructions (applied-constructions solution)) 10
                              #'(lambda (construction) 
                                  (make-html construction :expand-initially nil
                                             :configuration configuration
                                             :wrap-in-paragraph nil)))))
                    ((tr)
                     ((td) "resulting structure")
                     ((td) ,(make-html-fcg-light (car-resulting-cfs (cipn-car solution))
                                                 :configuration configuration :feature-types feature-types))))))))
      `((div)
        ((table :class "two-col")
         ((tbody)
          ((tr)
           ((td) "application process")
           ((td) ((div :id ,subtree-id)
                  ,(make-html-fcg-light (top-node cip)
                                        :subtree-id subtree-id
                                        :struct-id info-struct-id-name))))
          ((tr)
           ((td) "selected node")
           ((td) ,(get-replacing-content-fcg info-struct-id-name (get-current-cipn *last-cipn*)
                                             :configuration configuration)))
          ,(if (and show-queue (queue cip)) (make-tr-for-cip-tree-fcg-light cip "queue")
             "")))))))
 
(defun get-replacing-content-fcg (struct-id-name
                                  cipn
                                  &key
                                  (tree-id (make-id 'cipn)) 
                                  (configuration nil))
  "this function returns the content of a node, and it's used to replace the selected node in the compacted visualization"
  (let* ((node-id (symb tree-id '- (created-at cipn)))
         (with-search-debug-data (get-configuration configuration :with-search-debug-data))
         (cip (cip cipn))
         (construction-inventory (construction-inventory cip))
         (node-color (cipn-node-color cipn cip))
         (feature-types (feature-types (original-cxn-set construction-inventory)))
         (configuration (or configuration (visualization-configuration construction-inventory)))
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
               ,(make-html-cipn-title-fcg cipn construction-inventory)))))
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
               (make-expand/collapse-link-parameters struct-id-name t "show details from new visualization"))
              `((table)
                ((tbody)
                 ,status-tr
                 ,@(when (car-resulting-cfs (cipn-car cipn))
                     (let ((*make-html-cfs-construction-inventory* 
                            construction-inventory))
                       `(((tr) 
                          ((td)
                           ,(make-html-fcg-light (car-resulting-cfs (cipn-car cipn))
                                                 :feature-types feature-types
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
                   ((tr) ((td) ,(make-html-fcg-light (cipn-car cipn)
                                                     :feature-types feature-types
                                                     :configuration configuration)))
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
                                                          :render-mode)
                                       :node cipn)))
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

(defun remove-empty-units (fcg-construction)
  "removes empty units (footprints only units) from the contributing part of a construction"
  (let ((cxn-to-return (copy-object fcg-construction)))
    (dolist (c-unit (contributing-part fcg-construction))
      (when (and (eq 1 (length (unit-structure c-unit)))
                 (string= 'footprints (feature-name (first (unit-structure c-unit)))))
        (setf (contributing-part cxn-to-return) (remove (name c-unit) (contributing-part cxn-to-return) :key 'name :test 'string=))))
    cxn-to-return))
