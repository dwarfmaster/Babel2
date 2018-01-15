
(in-package :web-interface)

;; #########################################################
;; helpers
;; ---------------------------------------------------------

(defun get-super-types (type &key (ignore '(standard-object t)) level)
  "Computes a list of types (and supertypes) ignoring types in ignore
   and up to level"
  (declare (type symbol type))
  (unless (or (member type ignore)
              (and level (<= level 0)))
    (cons type
          (remove-duplicates 
           (loop for class in (closer-mop:class-direct-superclasses 
                               (find-class type))
                 append (get-super-types (class-name class)
                                         :ignore ignore
                                         :level (when level (- level 1))))))))

(defun get-sub-types (type
                      &key ignore
                      level)
  "Computes a list of types (and supertypes) ignoring types in ignore)"
  (declare (type symbol type))
  (unless (or (member type ignore)
              (and level (<= level 0)))
    (cons type
          (remove-duplicates 
           (loop for class in (closer-mop:class-direct-subclasses
                               (find-class type))
                 append (get-sub-types (class-name class)
                                       :ignore ignore
                                       :level (when level (- level 1))))))))

(defun class-graph-s-dot-envelope (graph)
  "takes a graph and envelopes it with the graph"
  (append
   '(s-dot::graph)
   '(((s-dot::ranksep "0.3")
      (s-dot::nodesep "0.5")
      (s-dot::rankdir "BT")
      (s-dot::margin "0")
      (s-dot::fontname "Arial")
      (s-dot::fontsize "10.0")))
   (if (listp (first graph))
     graph
     (list graph))))
  
(defgeneric class->s-dot (class &key finish &allow-other-keys)
  (:documentation "finish envelopes the behavior so it can be shown
                   otherwise, part of an s-dot graph is returned"))

(defmethod class->s-dot ((b symbol) &key (slots t) (methods t) arguments finish
                         (max-width 60))
  (class->s-dot (find-class b) :slots slots :methods methods :arguments arguments
                :max-width max-width
                :finish finish))

(defmethod class->s-dot ((c class) &key
                         (slots t)
                         (methods t)
                         arguments
                         (max-width 60)
                         finish)
  (let* ((slots-str
          (when slots
            (sort 
             (loop for m in (closer-mop:class-direct-slots c)
                   for name = (closer-mop:slot-definition-name m)
                   for type = (if (not (eq (closer-mop:slot-definition-type m) t))
                                (closer-mop:slot-definition-type m)
                                nil)
                   for string = (format nil "+~a~@[ : ~a~]" name type)
                   for string-split = (split string #\Space)
                   for format-str = (mkstr "~{~<~%   ~1," max-width ":;~A~>~^ ~}")
                   for string-formatted = (format nil "~{~a~^\\l~}"
                                                  (loop for s in (split (format nil format-str string-split)
                                                                        #\Newline)
                                                        unless (string-equal s "")
                                                        collect (string-right-trim " " s)))
                   collect string-formatted)
             #'string-lessp)))
         (methods-str
          (when methods
            (remove-duplicates 
            (sort 
             (loop for m in (closer-mop:specializer-direct-methods c)
                   for function = (closer-mop:method-generic-function m)
                   for name = (closer-mop:generic-function-name function)
                   for args 
                   = (when arguments
                       (loop for a in (closer-mop:method-specializers m)
                             for n in (closer-mop:method-lambda-list m)
                             if (and (typep a 'standard-object)
                                     (not (typep a 'closer-mop::eql-specializer)))
                             collect (format nil "~(~a: ~a~)" 
                                             (symbol-name n)
                                             (closer-mop::class-name a))
                             else collect (format nil "~(~a: ~a~)" (symbol-name n)
                                                  #-ccl
                                                  a
                                                  #+ccl
                                                  (ccl::name-of a))))
                   for string = (format nil "~(+~a~@[ (~{~a~^, ~})~]~)" name args)
                   for string-split = (split string #\Space)
                   for format-str = (mkstr "~{~<~%   ~1," max-width ":;~A~>~^ ~}")
                   for string-formatted
                   = (format nil "~{~a~^\\l~}"
                             (loop for s in (split
                                             (format nil format-str string-split)
                                             #\Newline)
                                   unless (string-equal s "")
                                   collect (string-right-trim " " s)))
                   when (and (not (typep m 'closer-mop:standard-reader-method))
                             (not (typep m 'closer-mop:standard-writer-method)))
                   collect string-formatted)
             #'string-lessp)
            :test 'string-equal)))
         (name (mkstr (closer-mop::class-name c)))
         (label (mkstr "{" (string-downcase name) 
                       (if slots-str
                         (format nil "|~(~{~a\\l~}~)" slots-str)
                         "")
                       (if methods-str
                         (string-replace 
                          (format nil "|~(~{~a\\l~}~)" methods-str)
                          "->" "--")
                         "")
                       "}"))
         (node-id (make-dot-id 0 (symb name)))
         (class-node `(s-dot::node
                       ((s-dot::shape "record")
                        (s-dot::fontname "Arial")
                        (s-dot::fontsize "10.0")
                        (s-dot::id ,node-id)
                        (s-dot::label ,label)))))
    (values
     (if finish
       (class-graph-s-dot-envelope class-node)
       class-node)
     node-id)))

(defun classes->s-dot (classes &key packages
                               (slots t)
                               (methods t)
                               arguments
                               (max-width 60)
                               finish)
  "takes a series, draws each and connects those which
   are subclasses"
  (let* ((classes-names-map)
         (packages
          (when packages
            (remove-duplicates (mapcar #'symbol-package classes))))
         (graph
          (append
           (if packages
             ;; get nodes (cluster - does not work yet)
             (loop for p in packages
                   for p-name = (package-name p)
                   collect
                   `(s-dot::cluster
                     ((s-dot::id ,(make-dot-id 0 (symb p-name)))
                      (s-dot::label ,p-name)
                      (s-dot::fontname "Arial")
                      (s-dot::fontsize "10.0"))
                     ,@(loop for c in (find-all p classes :key #'symbol-package)
                             for (node name)
                             = (multiple-value-list
                                (class->s-dot c
                                              :slots slots
                                              :methods methods
                                              :arguments arguments
                                              :max-width max-width))
                             collect node
                             do (push (cons c name) classes-names-map))))
             ;; get nodes no cluster
             (loop for c in classes
                   for (node name)
                   = (multiple-value-list (class->s-dot c
                                                        :slots slots
                                                        :methods methods
                                                        :arguments arguments
                                                        :max-width max-width))
                   collect node
                   do (push (cons c name) classes-names-map)))
           ;; transitions
           (loop
            for c in classes
            append (loop for sup in (intersection
                                     classes
                                     (mapcar #'closer-mop::class-name 
                                             (closer-mop:class-direct-superclasses
                                              (find-class c))))
                         for id1 =  (assqv c classes-names-map)
                         for id2 = (assqv sup classes-names-map)
                         when (and id1 id2)
                         collect
                         `(s-dot::edge ((s-dot::from ,id1)
                                       (s-dot::to ,id2))))))))
    (if finish
      (class-graph-s-dot-envelope graph)
      graph)))
   
(defun classes->svg (classes &key packages
                             (slots t)
                             (methods t)
                             arguments
                             (max-width 60))
  `((div :class "predicate-network-svg")
    ,(s-dot->svg (classes->s-dot classes
                                 :slots slots
                                 :methods methods
                                 :arguments arguments
                                 :max-width max-width
                                 :packages packages
                                 :finish t))))

(defun class->svg (c &key
                     (slots t)
                     (methods t)
                     arguments
                     (max-width 60))
  `((div :class "predicate-network-svg")
    ,(s-dot->svg (class->s-dot c :finish t
                               :slots slots
                               :methods methods
                               :arguments arguments
                               :max-width max-width))))

#+:hunchentoot-available-on-this-platform 
(define-css 'class-diagram-svg "
div.class-diagram-svg { overflow:svg;}
div.class-diagram-svg > svg { margin-left:8px;margin-right:8px;margin-top:3px;margin-bottom:8px; }
")

;; #########################################################
;; class-diagram (main function)
;; ---------------------------------------------------------

(export '(class-diagram))

(defun class-diagram (class &key super super-level sub sub-level
                            (slots t) (methods t)
                            (arguments t)
                            (max-width 50)
                            path (format "png") (open t))
  "shows class in web browser unless file name is given
   super - t/nil show super classes
   super-level - e.g.  1 - just the next super class
   sub - t/nil show sub classes
   sub-level - e.g.  1 - just the next super class
   slots - show slots of the class
   methods - show methods of the class
   arguments - show arguments of methods
   max-width - the maximum width of each class
   path - the filename for export (if nil we show it in the webinterface)
   format - format of the export
   open - if t tries to open the resulting file"
  (let* ((class-symb (if (symbolp class) class (closer-mop::class-name class)))
         (classes (remove-duplicates (append
                                      (list class-symb)
                                      (when super
                                        (get-super-types
                                         class-symb
                                         :level (unless (null super-level) (+ 1 super-level))))
                                      (when sub
                                        (get-sub-types
                                         class-symb
                                         :level (unless (null sub-level) (+ 1 sub-level)))))))
         (s-dot (classes->s-dot classes
                                :slots slots
                                :methods methods
                                :arguments arguments
                                :max-width max-width
                                :finish t)))
    (if path
      (s-dot->image s-dot
                    :path path :open open :format format)
      #+:hunchentoot-available-on-this-platform
      (add-element `((div :class "class-diagram-svg")
                     ,(s-dot->svg s-dot))))))

;; #########################################################
;; examples
;; ---------------------------------------------------------

(defun examples-class-diagram ()
  "generates some examples in the web browser and in Aiuto/.tmp"
  ;; web browser examples localhost:8000
  (class-diagram 'tree-node :arguments t)
  (class-diagram 'tree-node :slots t :methods t :arguments nil)
  (class-diagram 'tree-node :sub t :slots t :methods t)
  (class-diagram 'utils::ca :super t)
  ;; file export
  (class-diagram
   'relatable-class :sub t
   :path (babel-pathname :directory '(".tmp")
                         :name "entity"
                         :type "pdf")
   :format "pdf"))

;; run the examples
;; (examples-class-diagram)
