
(in-package :fcg)

(export '(application-path
          analyse-solution
          unit-bindings->graph
          make-html
          extract-unit-bindings))

(defun application-path (input &key (direction '<-) (write-data nil))
  "Draws an application path graph for
   comprehension or formulation (depending on the
   direction that has been specified, the default
   being comprehension) of the given input."
  (multiple-value-bind (output solution cip)
      (cond ((eq direction '<-)
             (comprehend input))
            ((eq direction '->)
             (formulate input)))
    (when output
      (multiple-value-bind (data cxn-ubs)
          (extract-unit-bindings solution direction)
        (gp-deps data cxn-ubs direction (construction-inventory cip))
        (when write-data
          (write-data data))
        (values data output)))))

(defun analyse-solution (solution direction &key (write-data nil))
  (multiple-value-bind (data cxn-ubs)
      (extract-unit-bindings solution direction)
    (gp-deps data cxn-ubs direction (construction-inventory (cip solution)))
    (when write-data
      (write-data data))
    data))

(defun all-cipn-cars-on-solution-path (solution)
  "Get all cipn-cars (cip node construction application results) of
the nodes above the solution."
  (mapcar #'cipn-car
          (cdr (reverse (cons solution (all-parents solution)))))) ;;why cdr?
  

(defun extract-unit-bindings (solution direction)
  "Returns the data for the application path
   graph, given the cip nodes solution. The data
   is currently represented as a list (for each
   construction) of (cxn-name, unit-names, unit-
   bindings) triples. The list is ordered
   according to the order in which the
   constructions have been applied."
  (let ((id -1)
        (cxn-unit-bindings (make-hash-table)))

    (values
     (make-instance 'gp-data
                    :cxns (mapcar #'(lambda (cipn-car)
                                      (incf id)
                                      (gp-cxn-data cipn-car id direction cxn-unit-bindings))
                                  (all-cipn-cars-on-solution-path solution)))
     cxn-unit-bindings)))

(defun resolve-value (value bindings)
  (let ((next-val (cdr (assoc value bindings :test #'eq))))
    (if next-val
      (resolve-value next-val bindings)
      value)))

(defun resolve-bindings (bindings)
  (remove-duplicates (mapcar (lambda (binding)
                               (cons (car binding)
                                     (resolve-value (cdr binding) bindings)))
                             bindings)
                     :test #'equalp))

(defun get-resolved-bindings (gp-cxn)
  "";;katrien: no idea what this does exactly...
  (remove-if-not #'car 
                 (mapcar #'(lambda (binding)
                             (cons (find (first binding) (units gp-cxn) :test #'eq :key #'name)
                                   (rest binding)))
                         (resolve-bindings (snd-mrg-binds gp-cxn)))))

(defun lock-lst (cxn direction)
  "Returns a list of units corresponding with the
   locks for the given direction. These correspond
   to the feature structures from the
   corresponding pole, excluding the root unit and
   J-units."
  (remove-if #'(lambda (unit)
                 (or (j-unit-p unit) (root-p unit)))
             (mapcar #'(lambda (unit)
                         (make-any-unit (unit-name unit)
                                        (filter-gp-feats (unit-features unit))))
                     (cond ((eq direction '<-) ; comprehension lock
                            (right-pole-structure cxn)) 
                           ((eq direction '->) ; formulation lock
                            (left-pole-structure cxn))))))

(defun gp-cxn-data (cipn-car id direction cxn-unit-bindings)
  "Given a car node, a unique identifier for the
   construction to extract, the direction of
   application process, and some hash maps,
   mapping the construction id to some remaining
   data, returns a corresponding gp-cxn instance
   and adds the remaining data to the hash maps."
  (let ((gp-cxn (make-instance 'gp-cxn
                               :name (name (car-applied-cxn cipn-car))
                               :id id
                               :snd-mrg-binds (car-second-merge-bindings cipn-car)
                               :snd-mrg-struc (car-second-merge-structure cipn-car))))
    (setf (units gp-cxn)
          (gp-units-data (car-applied-cxn cipn-car) gp-cxn)) ;;gp-cxn nodig!
    
    (setf (gethash id cxn-unit-bindings) ;;mss beter om naam te gebruiken?
          (mapcar #'(lambda (binding) ;;dit moet nog opgekuist worden!
                      (list (first binding)
                            (rest binding)
                            (rest (assoc (rest binding)
                                         (snd-mrg-struc gp-cxn)))
                            (rest (assoc (name (first binding))
                                         (lock-lst (car-applied-cxn cipn-car) direction)
                                         :test #'string=))))
                  (get-resolved-bindings gp-cxn)))
    gp-cxn))

(defun make-any-unit (name feats) ; can also make J-units
  (cons name feats))

(defun filter-gp-feats (feats)
  "Exclude tag variables and footprints from unit-
   features."
  (remove-if (lambda (feat-struc)
               (or
                ; no tag variables
                (not (listp feat-struc))
                ; no footprints
                (eq (car feat-struc) 'footprints)))
             feats))

(defun real-listp (value)
  "Like listp, but returns nil for non-list cons-cells."
  (or (null value)
      (and (consp value)
           (real-listp (cdr value)))))

;;; (defun all-unit-names (cxn &key (include-root? nil))
;;;   (let ((left-and-right-pole-structures (append (left-pole-structure cxn)
;;;                                                 (right-pole-structure cxn))))
;;;     (remove-duplicates
;;;      (loop for unit in (if include-root?
;;;                         left-and-right-pole-structures
;;;                         (remove-if #'root-p left-and-right-pole-structures))
;;;           collect (unit-name unit :maybe-j-unit t)))))

;;; (defun all-units (cxn &key (include-root? nil))
;;;   (let ((left-and-right-pole-structures (append (left-pole-structure cxn)
;;;                                                 (right-pole-structure cxn))))
;;;     (remove-duplicates
;;;      (loop for unit in (if include-root?
;;;                          left-and-right-pole-structures
;;;                          (remove-if #'root-p left-and-right-pole-structures))
;;;            collect unit) :test #'equal :key #'unit-name)))
  

;;; (defun make-gp-units (processing-cxn gp-cxn)
;;;   (let ((gp-units nil))
;;;     ;;processing construction has two poles:
;;;     ;;a) left-pole
;;;     (setf gp-units
;;;           (loop for unit in (remove-if #'root-p (left-pole-structure processing-cxn))
;;;                 if (j-unit-p unit)
;;;                 collect (make-instance 'gp-unit
;;;                                        :name (unit-name unit)
;;;                                        :cxn gp-cxn ;;volledige cxn nodig?
;;;                                        :cont-part (append (unit-features unit)
;;;                                                           (cont-part unit))) ;;zit hier al iets in??
;;;                 else collect (make-instance 'gp-unit
;;;                                             :name (unit-name unit)
;;;                                             :cxn gp-cxn
;;;                                             :prod-lock (unit-features unit))))
;;;     ;;b) right-pole                         
;;;     (setf gp-units
;;;           (append gp-units
;;;                   (loop for unit in (remove-if #'root-p (right-pole-structure processing-cxn))
;;;                         if (j-unit-p unit)
;;;                         collect (make-instance 'gp-unit
;;;                                                :name (unit-name unit)
;;;                                                :cxn gp-cxn
;;;                                                :cont-part (append (unit-features unit)
;;;                                                                   (cont-part gp-unit))) ;;zit hier al iets in??
;;;                         else collect (make-instance 'gp-unit
;;;                                                     :name (unit-name unit)
;;;                                                     :cxn gp-cxn
;;;                                                     :comp-lock (unit-features unit)))))
;;;     gp-units))


(defun gp-units-data (proc-cxn gp-cxn)
  (let ((gp-units (mapcar (lambda (uname)
                         (make-instance 'gp-unit
                                        :name uname
                                        :cxn gp-cxn))
                       (remove-duplicates
                        (mapcar (lambda (unit)
                                  (unit-name unit :maybe-j-unit T))
                                (remove-if #'root-p
                                           (append (left-pole-structure proc-cxn)
                                                   (right-pole-structure proc-cxn))))))))
    (mapc (lambda (unit)
            (add-unit-fs unit gp-units 'left))
          (left-pole-structure proc-cxn))
    (mapc (lambda (unit)
            (add-unit-fs unit gp-units 'right))
          (right-pole-structure proc-cxn))
    gp-units))

(defun add-unit-fs (unit gp-units pole)
  (let ((gp-unit (find (unit-name unit :maybe-j-unit T) gp-units
                       :key #'name)))
    (when gp-unit
      (if (j-unit-p unit)
        (setf (cont-part gp-unit)
              (append (unit-features unit) ; not really efficient but it's not a long list
                      (cont-part gp-unit)))
        (case pole
          (left (setf (prod-lock gp-unit)  ; there should be only one
                      (unit-features unit)))
          (right (setf (comp-lock gp-unit) ; there should be only one
                       (unit-features unit))))))))

(defun unit-paths (data cxn-ubs)
  ; trans-unit -> list of units in order
  ; add snd-mrg-str to units
  (let ((paths (make-hash-table)))
    (mapc (lambda (c)
            (let ((ubs (gethash (id c) cxn-ubs)))
              (mapc (lambda (ub)
                      (setf (snd-mrg-str (first ub)) (third ub))
                      (push (first ub) (gethash (second ub) paths)))
                    ubs)))
          (reverse (cxns data))) ; reverse because push reverses too
    paths))


(defun set= (s1 s2 &key (test #'eql))
  (not (set-exclusive-or s1 s2 :test test)))

(defun gp-deps (data cxn-ubs direction cxn-inv)
  (let ((upaths (unit-paths data cxn-ubs))
        (deps nil))
    (maphash (lambda (key upath)
               (let ((parents nil))
                 (mapc (lambda (unode)
                         (let ((lock-paths (reverse (fs-paths (cons '==1 (remove-tags (if (eq direction '<-) (comp-lock unode) (prod-lock unode)) :all-fs T)) cxn-inv)))
                               (lock-binds (make-hash-table :test #'equalp))
                               (parent-idx 0))
                           (setf (tunit unode) key)
                           (mapc (lambda (parent)
                                   (let ((d-bindings nil)
                                         (from-last-p (= parent-idx (- (length parents) 1))))
                                     (mapc (lambda (bd)
                                             (let* ((lpath (lock-path bd))
                                                    (binding (and (cdr lpath)
                                                                  (or (negating-bd-p bd) (unify lpath (snd-mrg-str parent)))
                                                                  (gp-merge-bsl lpath (snd-mrg-str parent) (snd-mrg-binds (cxn unode))))))
                                               (setf binding (order-bds binding (tree-leaves (lock-values bd))))
                                               (cond ((and from-last-p (negating-bd-p bd)) ; negations only on green edgesp 
                                                      (push (make-instance 'gp-binding 
                                                                           :lock-path lpath
                                                                           :features (features bd)
                                                                           :lock-values (lock-values bd)
                                                                           :bindings binding
                                                                           :set-of-predicates-p (set-of-predicates-p bd))
                                                            d-bindings))
                                                     ((and binding (not (negating-bd-p bd)) (not (find binding (gethash (features bd) lock-binds)
                                                                                                       :test (lambda (bd1 bd2)
                                                                                                               (set= bd1 bd2 :test #'equalp)))))
                                                      (push binding (gethash (features bd) lock-binds))
                                                           (push (make-instance 'gp-binding ;;;;
                                                                                :lock-path lpath
                                                                                :features (features bd)
                                                                                :lock-values (lock-values bd)
                                                                                :bindings binding
                                                                                :set-of-predicates-p (set-of-predicates-p bd)
                                                                                )
                                                                 d-bindings))
                                                     ((and from-last-p (not (gethash (features bd) lock-binds)))
                                                      ;; DEBUG (each lock path should have been added to an edge)
                                                      (push (make-instance 'gp-binding ;;;;
                                                                                :lock-path lpath
                                                                                :features (features bd)
                                                                                :lock-values (lock-values bd)
                                                                                :bindings '(((NIL . NIL)))
                                                                                :set-of-predicates-p (set-of-predicates-p bd)
                                                                                )
                                                            d-bindings)))))
                                                      
                                           lock-paths)
                                     (when (or d-bindings from-last-p)
                                       (push (make-instance 'gp-dependency
                                                            :from parent
                                                            :to unode
                                                            :from-last-p from-last-p
                                                            :bindings d-bindings
                                                            :tunit key)
                                             deps)))
                                   (incf parent-idx))
                                 parents))
                         (setf parents (append parents (list unode))))
                       upath)))
             upaths)
    (setf (dependencies data) deps)))


(defun order-bds (bdss to-vals)
  (mapcar (lambda (bds)
            (mapcar (lambda (bd)
                      (if (find (car bd) to-vals :test #'equalp)
                        (cons (cdr bd) (car bd))
                        bd))
                    bds))
          bdss))

(defun negating-bd-p (bd)
  (or (find '==0 (features bd))
      (find '==0 (tree-leaves (lock-values bd)))))

(defun tree-leaves (tree)
  (if (consp tree)
    (append (tree-leaves (car tree))
            (tree-leaves (cdr tree)))
    (list tree)))


(defun find-feat-type (feat cxn-inv)
  (car (cdr (assoc feat (feature-types (original-cxn-set cxn-inv))))))


(defun fs-paths (fss cxn-inv)
  (if (operator-p (car fss))
    (mapcar (lambda (path)
              (setf (lock-path path) (cons (car fss) (list (lock-path path))))
              (push (car fss) (features path))
              path)
            (fs-paths (cdr fss) cxn-inv))
    (apply #'append
           (mapcar (lambda (fs)
                     (cond ((every #'leafp (cdr fs))
                            (list (make-instance 'gp-binding
                                                 :lock-path fs
                                                 :features (if (operator-p (car fs))
                                                             nil
                                                             (list (car fs)))
                                                 :lock-values (if (operator-p (car fs))
                                                                (list fs)
                                                                (cdr fs)))))
                           ((string= (find-feat-type (car fs) cxn-inv) 'set-of-predicates)
                            (list (make-instance 'gp-binding
                                                 :lock-path fs
                                                 :features (list (car fs))
                                                 :lock-values (cdr fs)
                                                 :set-of-predicates-p T)))
                           (T (mapcar (lambda (path)
                                        (setf (lock-path path)
                                              (cons (car fs) (list (lock-path path))))
                                        (push (car fs) (features path))
                                        path)
                                      (apply #'append (mapcar (lambda (fss) (fs-paths fss cxn-inv)) (cdr fs)))))))
                   fss))))

(defun leafp (fs)
  (not (and (real-listp fs)
            (remove-if-not #'real-listp fs))))


(defun remove-tags (featstrucs &key (all-fs nil))
  (apply #'append
         (mapcar (lambda (featstruc)
                   (if (string= (car featstruc) 'tag)
                     (if all-fs (snd+2s (cdr featstruc)) (list (third featstruc)))
                     (list featstruc)))
                 featstrucs)))

(defun snd+2s (lst)
  (and lst
       (cons (second lst) (snd+2s (cddr lst)))))

(defun symbol= (x y)
  (if (and (symbolp x) (symbolp y))
    (string= x y)
    (equalp x y)))

(defun gp-merge-bsl (pattern source check-bindings)
  "Merges two feature structures and returns a list
   of valid binding lists."
  (let ((res (fcg::fcg-merge pattern source +no-bindings+)))
    (setf res (apply #'append (mapcar #'fcg::mr-bsl res)))
    (remove-if-not (lambda (mr-bs)
                     (merge-bindings-2 mr-bs check-bindings))
                   res)))


(defun merge-bindings-2 (pattern source)
  "Tries to merge two binding lists. Returns
   NIL if the bindings lists are not consistant
   with each other."
  (or (equalp pattern '((T . T)))
      (unify (mapcar #'car pattern)
             (mapcar #'cdr pattern)
             (list (remove-if-not (lambda (bd) ;;; preventing NIL NIL bindings
                                    (symbolp (cdr bd)))
                                  source)))))
    

;(defparameter *only-vp* T)

(defun unit-bindings->graph (&key (data nil) (debug-mode nil) ;(colored-paths t) (labeled 'full)
                                  (prefered-font "Open Sans Condensed")
                                  (construction-inventory *fcg-constructions*)
                                  (visualization-configuration nil)
                                  ; (trace-units nil)
                                  )
  "Draws a graph, analogeous to the one for the
   full grammar profiler.

   Labeled can be 'full, 'no-bindings or nil"
  (unless data
    (setf data (read-data)))
  (unless visualization-configuration
    (setf visualization-configuration (visualization-configuration construction-inventory)))
  (let* ((clusters nil)
         (edges (dependencies data))
         (not-new-units (remove-duplicates (mapcar #'unitstr (mapcar #'to edges))))
         (tu-colors (mk-tu-colors data))
         (colored-paths (get-configuration visualization-configuration :colored-paths))
         (labeled-paths (get-configuration visualization-configuration :labeled-paths))
         (trace-units (mapcar #'string-downcase (get-configuration visualization-configuration :trace-units))))
    (mapc (lambda (cxn)
            (push (append `(s-dot::cluster ((s-dot::id ,(string-append (make-dot-id2 (symbol-name (name cxn)))
                                                                       (write-to-string (id cxn))))
                                            (s-dot::label ,(string-append "<<TABLE BGCOLOR=\"#030e86\" COLOR=\"transparent\" WIDTH=\"80%\"><TR><TD ALIGN=\"CENTER\" WIDTH=\"80%\"> "
                                                                          (string-downcase (symbol-name (name cxn)))
                                                                          "</TD></TR></TABLE>>")) ; ,(string-downcase (symbol-name (name cxn))))
                                            (s-dot::color "#030e86")
                                            (s-dot::fontcolor "white")
                                            (s-dot::fontsize "9")
                                            (s-dot::fontname ,prefered-font)
                                            ;(s-dot::style "filled, setlinewidth(2.0)")
                                            (s-dot::style "setlinewidth(1.5)")
                                            (s-dot::tooltip ,(format nil "~A" (snd-mrg-binds cxn)))
                                            (s-dot::lwidth "0.0")
                                            ;(s-dot::margin "0.0")
                                            ;(s-dot::fillcolor "#030e86")
                                            ;(s-dot::bgcolor "green")
                                            
                                            ;(s-dot::gradientangle 270)
                                            ;(s-dot::fontcolor "white")
                                            ;(s-dot::fillcolor "#030e86;0.3:white")
                                            (s-dot::lheight "0.0")
                                            ))
                          (mapcar (lambda (unit)
                                    (let ((new-unit-p (not (find (unitstr unit)
                                                                 not-new-units :test #'string=))))
                                      `(s-dot::node ((s-dot::id ,(make-dot-id2 (unitstr unit)))
                                                     (s-dot::label ,(string-downcase (apply-if (not debug-mode) #'utils::remove-numeric-tail (symbol-name (name unit)))))
                                                     (s-dot::shape "box")
                                                     (s-dot::style "filled, setlinewidth(1.0)")
                                                     (s-dot::fillcolor ,(if new-unit-p "#C2E0D1"
                                                                          (if (and trace-units (find (string-downcase (utils::remove-numeric-tail (symbol-name (name unit)))) trace-units :test #'string=))
                                                                            "yellow"
                                                                            "white")))
                                                     (s-dot::color ; "#669999"
                                                                   ,(if colored-paths ; (string= (utils::remove-numeric-tail (symbol-name (name unit))) "?VP")
                                                                      (gethash (tunit unit) tu-colors)
                                                                      	"#808080"))
                                                     (s-dot::fontcolor ; "#669999"
                                                                       ,(if colored-paths ; (string= (utils::remove-numeric-tail (symbol-name (name unit))) "?VP")
                                                                          (gethash (tunit unit) tu-colors)
                                                                          "#030e86"))
                                                     (s-dot::fontsize "9")
                                                     (s-dot::fontname ,prefered-font) ;(string-append prefered-font " bold") ;;does not work well  with svg
                                                     (s-dot::height "0.25")
                                                     (s-dot::tooltip ,(symbol-name (tunit unit)))
                                                     ;(s-dot::width "0.0")
                                                     ;(s-dot::margin "0.0")
                                                     ))))
                                  (units cxn)))
                  clusters))
          (cxns data))
    (setf edges
          (mapcar (lambda (edge)
                    `(s-dot::edge ((s-dot::from ,(make-dot-id2 (unitstr (from edge))))
                                   (s-dot::to ,(make-dot-id2 (unitstr (to edge))))
                                   (s-dot::color ,(if (and (from-last-p edge) ;"#669999" "gray90"
                                                           colored-paths) ; (string= (utils::remove-numeric-tail (symbol-name (name (to edge)))) "?VP")
                                                      (gethash (tunit edge) tu-colors)
                                                      "gray30"))
                                   (s-dot::label ,(if (or labeled-paths
                                                          (and trace-units
                                                               (find (string-downcase (utils::remove-numeric-tail (symbol-name (name (to edge)))))  trace-units :test #'string=)))
                                                    (bindingstr (bindings edge) labeled-paths :debug-mode debug-mode)
                                                    ""))
                                   (s-dot::fontcolor ,(if (and (from-last-p edge)
                                                           colored-paths)
                                                      (gethash (tunit edge) tu-colors)
                                                      "gray30"))
                                   (s-dot::fontsize "9")
                                   (s-dot::fontname ,prefered-font)
                                   (s-dot::style ,(if (from-last-p edge) "" "dotted"))
                                   (s-dot::tooltip ,(format nil "~A" (remove-if (lambda (b) (equalp b '(T . T)))
                                                                                (apply #'append (apply #'append (mapcar #'bindings (bindings edge)))))))
                                   (s-dot::labeltooltip ,(format nil "~A" (remove-if (lambda (b) (equalp b '(T . T)))
                                                                                     (apply #'append (apply #'append (mapcar #'bindings (bindings edge)))))))
                                   )))
                  edges))
    (append '(s-dot::graph ((s-dot::rankdir "LR")
                            (s-dot::tooltip "constructional dependencies")))
            clusters
            edges)))


(defun make-dot-id2 (name)
  (substitute-chars name
                    (list #\- #\? #\! #\[ #\] #\{ #\} #\, #\; #\# #\=)
                    #\_))
  
(defun substitute-chars (str chars replacement)
  (if chars
    (substitute replacement (car chars)
                (substitute-chars str (cdr chars) replacement))
    str))

(defun circular (l)
  (let ((cl (copy-list l)))
    (setf (cdr (last cl)) cl)
    cl))

(defparameter *lp-colors* '("#3F2EBD" "#6E350E" "#4F0576" "#12477D" 
                            "#209AA0" "#89791F" "#669999" "#468845"
                            "#87115C"))

(defun mk-tu-colors (data)
  (let ((tus nil)
        (tuc (make-hash-table))
        (colors (circular *lp-colors*)))
    (mapc (lambda (cxn)
            (mapc (lambda (u)
                    (push (tunit u) tus))
                  (units cxn)))
          (cxns data))
    (setf tus (remove-duplicates tus))
    (mapc (lambda (key)
            (setf (gethash key tuc) (car colors))
            (setf colors (cdr colors)))
          tus)
    tuc))

(defun apply-if (condition f value)
  (if condition
    (apply f (list value))
    value))

(defun unitstr (gp-unit)
  (string-append (symbol-name (name (cxn gp-unit)))
                 (write-to-string (id (cxn gp-unit)))
                 (symbol-name (name gp-unit))))

(defun bindingstr (edge-bindings labeled &key (debug-mode nil))
  (let ((prev-feats nil)
        (xtra-tab nil))
    (setf edge-bindings
          (remove-if (lambda (binding)
                       (and (not debug-mode)
                            (find 'footprints (features binding))))
                     edge-bindings))
    (if edge-bindings
      (format nil "<<TABLE align=\"LEFT\" cellpadding=\"0\" cellspacing=\"0\" color=\"transparent\">~{~A~^~}</TABLE>>" ; "<<TABLE align=\"LEFT\">~{~A~^~}</TABLE>>"
              (mapcar (lambda (binding)
                        (let* ((cut-feats (rm-prefix (features binding) prev-feats))
                               (fake-val (equalp (lock-values binding) '((==1))))
                               (xtb (and cut-feats (or fake-val xtra-tab)))
                               (has-neg (find '==0 (features binding)))
                               (res (format nil "<TR><TD align=\"LEFT\" valign=\"TOP\">~A</TD> <TD align=\"LEFT\" valign=\"BOTTOM\">~A</TD></TR>"
                                            (if has-neg
                                              (featstr (features binding)
                                                       (lock-values binding)
                                                       debug-mode
                                                       (set-of-predicates-p binding)
                                                       0
                                                       T)
                                              (featstr (cons '==1 cut-feats) (if fake-val nil (lock-values binding)) debug-mode (set-of-predicates-p binding)
                                                       (- (count '==1 (features binding))
                                                          (count '==1 cut-feats)
                                                          (if xtra-tab
                                                            0
                                                            1)
                                                          )
                                                       T))
                                            (if (or fake-val (eq labeled 'no-bindings)) "" (bdstr (bindings binding) debug-mode)))))
                          (setf prev-feats (and (not has-neg) (features binding)))
                          (setf xtra-tab xtb)
                          res))
                      edge-bindings))
      "")))

(defun rm-prefix (l p)
  (if (and l p (eq (car l) (car p)))
    (rm-prefix (cdr l) (cdr p))
    l))

(defun featstr (feats vals debug-mode set-of-predicates-p &optional (cnt 0) (firstln nil))
  
  (cond ((not feats)
         (if set-of-predicates-p
           (setofpredstr vals debug-mode)
           (valuestr vals debug-mode)))
        ((eq (car feats) '==0)
         (format nil "<FONT color=\"#DF0101\">&#172;~A</FONT>"
                 (featstr (cdr feats) vals debug-mode set-of-predicates-p cnt)))
        ((eq (car feats) '==1)
         (string-append (cntbrkstr cnt firstln)
                        (featstr (cdr feats) vals debug-mode set-of-predicates-p (+ cnt 1))))
        (T (format nil "~A: ~A" (string-downcase (car feats))
                  ; (if (and (equalp feats '(meaning)) (eq (caar vals) '==)) ;;;
                   ;  (setofpredstr (cdar vals) debug-mode)
                     (featstr (cdr feats) vals debug-mode set-of-predicates-p cnt)))))

(defun cntbrkstr (cnt firstln)
  (if (> cnt 0)
    (apply #'string-append
           (cons (if firstln "" "<BR align=\"LEFT\" />")
                 (mapcar #'(lambda (x) ;"&nbsp;&nbsp;&nbsp;&nbsp;"
                             (declare (ignore x))
                           (format nil "~a ~a ~a" #\Space #\Space #\Space))
                         (make-list cnt))))
    ""))

(defun valuestr (vals debug-mode)
  (format nil "~{~A~^ ~}"
          (mapcar (lambda (val)
                    (if (and (consp val) (operator-p (car val)))
                      (opstr val debug-mode)
                      (valstr val debug-mode)))
                  vals)))

(defun valstr (val debug-mode)
  (cond ((real-listp val)
         (format nil "[~{~A~^, ~}]"
                 (mapcar (lambda (val)
                           (valstr val debug-mode))
                         val)))
        ((and (not debug-mode) (symbolp val) (< 2 (length (symbol-name val))))
         (utils::remove-numeric-tail (string-downcase (symbol-name val))))
        ((symbolp val)
         (string-downcase val))
        (T (format nil "~A" val))))

(defun opstr (val debug-mode)
  (case (car val)
    (==1 (valstr val debug-mode))
    (== (format nil "{~{~A~^, ~}}" (mapcar (lambda (v) (valstr v debug-mode)) (cdr val))))
    (==0 (format nil "<FONT color=\"#DF0101\">&not;~A</FONT>"
                 (valuestr (cdr val) debug-mode)))
    (otherwise (valstr val debug-mode))))

(defun setofpredstr (vals debug-mode)
  (format nil "{~{~A~^, ~}}"
          (mapcar (lambda (p) (predstr p debug-mode)) (if (eq (caar vals) '==)
                                                              (cdar vals)
                                                              vals))))

(defun predstr (pred debug-mode)
  (format nil "~A(~{~A~^, ~})"
          (valstr (car pred) debug-mode)
          (mapcar (lambda (v) (valstr v debug-mode))
                  (cdr pred))))

(defun bdstr (bdss debug-mode)
  (format nil "<FONT face=\"monaco\">~{~A~^<BR align=\"LEFT\" /><BR align=\"LEFT\" />~}</FONT>"
          (mapcar (lambda (bds)
                    (format nil "{~{~A~^,<BR align=\"LEFT\" /> ~}}   " ; &nbsp;
                            (mapcar (lambda (bd)
                                      (constr bd debug-mode))
                                    bds)))
                  bdss)))
  

(defun constr (c debug-mode)
  (when c
    (format nil "(~A . ~A)" (valstr (car c) debug-mode) (valstr (cdr c) debug-mode))))



(defun substitute-special-operators (feat-strucs cxn-inventory)
  (cons '==1
        (mapcar (lambda (fs)
                  (sub-spec-op-rec fs cxn-inventory))
                feat-strucs)))

(defun sub-spec-op-rec (fs ci)
  (cond ((or (not (real-listp fs))
             (not (real-listp (cadr fs))))
         fs)
        ((real-listp (car fs))
         (mapcar (lambda (inner-fs)
                   (sub-spec-op-rec inner-fs ci))
                 fs))
        (T
         (case (find-special-operator (car fs) ci)
           (sequence
            fs)
           (set-of-predicates
            (cons (car fs)
                  (mapcar (lambda (inner-fs)
                            (cons '== inner-fs))
                          fs)))
           (set
            (cons (car fs)
                  (mapcar (lambda (inner-fs)
                            (cons '== (sub-spec-op-rec inner-fs ci)))
                          (cdr fs))))
           (otherwise
            (cons (car fs)
                  (mapcar (lambda (inner-fs)
                            (cons '==1 (sub-spec-op-rec inner-fs ci)))
                            (cdr fs))))))))

(defun find-special-operator (feat cxn-inventory)
  (cadr (assoc feat (feature-types (original-cxn-set cxn-inventory))
               ; eq doesn't work (different packages)
               :test #'string=)))
