
(in-package :fcg)

(export '(gp-data
          gp-cxn
          gp-unit
          gp-dependency
          gp-binding
          binding=
          find-gp-unit
          read-data
          write-data))



(defclass gp-data ()
  ((cxns
    :accessor cxns
    :initarg :cxns
    :initform nil
    :documentation
    "A list of applied gp-cxns.")
   (dependencies
    :accessor dependencies
    :initarg :dependencies
    :initform nil
    :documentation
    "The dependencies between gp-units,
     corresponding with edges.")))

(defclass gp-cxn ()
  ((name :accessor name
         :initarg :name)
   (id
    :accessor id
    :initarg :id
    :initform -1
    :documentation
    "A unique identifier for the applied
     construction. The same construction may be
     applied multiple times.")
   (units :accessor units
          :initarg :units
          :initform nil)
   (snd-mrg-binds
    :accessor snd-mrg-binds
    :initarg :snd-mrg-binds
    :initform nil
    :documentation
    "The second merge bindings, corresponding with
     the car node of the applied construction.")
   (snd-mrg-struc
    :accessor snd-mrg-struc
    :initarg :snd-mrg-struc
    :initform nil
    :documentation
    "The second merge structure, corresponding with
     the car node of the applied construction.")))

(defclass gp-unit ()
  ((name :accessor name
         :initarg :name)
   (cxn :accessor cxn
        :initarg :cxn)
   (tunit :accessor tunit
          :initarg :tunit
          :initform nil)
   (prod-lock
    :accessor prod-lock
    :initarg :prod-lock
    :initform nil
    :documentation
    "The feature structures from the unit's
     production lock.")
   (comp-lock
    :accessor comp-lock
    :initarg :comp-lock
    :initform nil
    :documentation
    "The feature structures from the unit's
     comprehension lock.")
   (cont-part
    :accessor cont-part
    :initarg :cont-part
    :initform nil
    :documentation
    "The feature structures from the unit's
     J-units.")
   (snd-mrg-str
    :accessor snd-mrg-str
    :initarg :snd-mrg-str
    :initform nil)))

(defclass gp-dependency ()
  ((from :accessor from
         :initarg :from)
   (to :accessor to
       :initarg :to)
   (from-last-p :accessor from-last-p
                :initarg :from-last-p)
   (bindings :accessor bindings
             :initarg :bindings
             :initform nil)
   (tunit :accessor tunit
          :initarg :tunit
          :initform nil)))

(defclass gp-binding ()
  ((lock-path :accessor lock-path
              :initarg :lock-path
              :initform nil)
   (features :accessor features
             :initarg :features
             :initform nil)
   (lock-values :accessor lock-values
                :initarg :lock-values
                :initform nil)
   (bindings :accessor bindings
             :initarg :bindings
             :initform nil)
   (set-of-predicates-p :accessor set-of-predicates-p
                        :initarg :set-of-predicates-p
                        :initform nil)
   ; TODO feature type / spec op (e.g. set)
   ; TODO mrg vs. unify
   ; TODO unit binding?
   ; TODO filtered?
   ; TODO swapped?
   ))

(defun binding= (bnd1 bnd2 &key (test #'equalp))
  "To bindings are equal if their from and to
   values are equal."
  (and (apply test (mapcar #'from-value (list bnd1 bnd2)))
       (apply test (mapcar #'to-value (list bnd1 bnd2)))))

(defun find-gp-unit (name id data)
  (find name (units (find id (cxns data)
                          :key #'id))
        :key #'name :test #'eq))


(defun gp-data->sexpr (data)
  (list (cons 'cxns
              (mapcar (lambda (cxn)
                        (list (cons 'name (name cxn))
                              (cons 'id (id cxn))
                              (cons 'snd-mrg-binds (snd-mrg-binds cxn))
                              (cons 'snd-mrg-struc (snd-mrg-struc cxn))
                              (cons 'units
                                    (mapcar (lambda (unit)
                                              (list (cons 'name (name unit))
                                                    (cons 'prod-lock (prod-lock unit))
                                                    (cons 'comp-lock (comp-lock unit))
                                                    (cons 'cont-part (cont-part unit))
                                                    (cons 'tunit (tunit unit))))
                                            (units cxn)))))
                      (cxns data)))
        (cons 'dependencies
              (mapcar (lambda (dep)
                        (list (cons 'from
                                    (list (cons 'unit-name (name (from dep)))
                                          (cons 'cxn-id (id (cxn (from dep))))))
                              (cons 'to
                                    (list (cons 'unit-name (name (to dep)))
                                          (cons 'cxn-id (id (cxn (to dep))))))
                              (cons 'from-last-p (from-last-p dep))
                              (cons 'tunit (tunit dep))
                              (cons 'bindings
                                    (mapcar (lambda (binding)
                                              (list (cons 'lock-path (lock-path binding))
                                                    (cons 'features (features binding))
                                                    (cons 'lock-values (lock-values binding))
                                                    (cons 'bindings (bindings binding))
                                                    (cons 'set-of-predicates-p (set-of-predicates-p binding))
                                                    ;(cons 'feature-path (feature-path binding))
                                                    ;(cons 'from-value (from-value binding))
                                                    ;(cons 'to-value (to-value binding))
                                                    ))
                                            (bindings dep)))))
                      (dependencies data)))))

(defun gp-data->json (data)
  (json-object (cons 'cxns
                     (apply #'json-array
                            (mapcar (lambda (cxn)
                                      (json-object (cons 'name
                                                         (json-string (name cxn)))
                                                   (cons 'id
                                                         (json-number (id cxn)))
                                                   (cons 'snd-mrg-binds
                                                         (json-string (snd-mrg-binds cxn)))
                                                   (cons 'snd-mrg-struc
                                                         (json-string (snd-mrg-struc cxn)))
                                                   (cons 'units
                                                         (apply #'json-array
                                                                (mapcar (lambda (unit)
                                                                          (json-object (cons 'name
                                                                                             (json-string (name unit)))
                                                                                       (cons 'tunit
                                                                                             (json-string (tunit unit)))
                                                                                       (cons 'prod-lock
                                                                                             (json-string (prod-lock unit)))
                                                                                       (cons 'comp-lock
                                                                                             (json-string (comp-lock unit)))
                                                                                       (cons 'cont-part
                                                                                             (json-string (cont-part unit)))
                                                                                       ))
                                                                        (units cxn))))
                                                   ))
                                    (cxns data))))
               (cons 'dependencies
                     (apply #'json-array
                            (mapcar (lambda (dep)
                                      (json-object (cons 'from
                                                         (json-object (cons 'unit-name
                                                                            (json-string (name (from dep))))
                                                                      (cons 'cxn-id
                                                                            (json-number (id (cxn (from dep)))))))
                                                   (cons 'to
                                                         (json-object (cons 'unit-name
                                                                            (json-string (name (to dep))))
                                                                      (cons 'cxn-id
                                                                            (json-number (id (cxn (to dep)))))))
                                                   (cons 'from-last-p
                                                         (json-bool (from-last-p dep)))
                                                   (cons 'tunit
                                                         (json-string (tunit dep)))
                                                   (cons 'bindings
                                                         (apply #'json-array
                                                                (mapcar (lambda (binding)
                                                                          (json-object ;(cons 'feature-path
                                                                                       ;      (apply #'json-array
                                                                                       ;             (mapcar #'json-string
                                                                                       ;                     (feature-path binding))))
                                                                                       ;(cons 'from-features
                                                                                       ;      (apply #'json-array
                                                                                       ;             (mapcar #'json-string
                                                                                       ;                     (from-features binding))))
                                                                                       ;(cons 'to-features
                                                                                       ;      (apply #'json-array
                                                                                       ;             (mapcar #'json-string
                                                                                       ;                     (to-features binding))))
                                                                                       (cons 'lock-path
                                                                                             (json-string (lock-path binding)))
                                                                                       (cons 'features
                                                                                             (json-string (features binding)))
                                                                                       (cons 'lock-values
                                                                                             (json-string (lock-values binding)))
                                                                                       (cons 'bindings
                                                                                             (json-string (bindings binding)))
                                                                                       (cons 'set-of-predicates-p
                                                                                             (json-string (set-of-predicates-p binding)))
                                                                                       ;(cons 'from-value
                                                                                       ;      (json-string (from-value binding)))
                                                                                       ;(cons 'to-value
                                                                                       ;      (json-string (to-value binding)))
                                                                                       ))
                                                                        (bindings dep))))
                                                   ))
                                    (dependencies data))))))

(defun json-array (&rest elts)
  (format nil "[~{~A~^, ~}]" elts))

(defun json-object (&rest pairs)
  (format nil "{~{~A~^, ~}}"
          (mapcar (lambda (pair)
                    (format nil "\"~A\" : ~A" (string-downcase (car pair)) (cdr pair)))
                  pairs)))

(defun json-string (value)
  (format nil "\"~A\"" value))

(defun json-number (value)
  (format nil "~A" value))

(defun json-bool (value)
  (if value
    "true"
    "false"))

(defun sexpr->gp-data (sexpr)
  (let ((data (make-instance 'gp-data
                 :cxns
                 (mapcar (lambda (s-cxn)
                           (let ((cxn (make-instance 'gp-cxn
                                                     :name (cdr (assoc 'name s-cxn))
                                                     :id (cdr (assoc 'id s-cxn))
                                                     :snd-mrg-binds (cdr (assoc 'snd-mrg-binds s-cxn))
                                                     :snd-mrg-struc (cdr (assoc 'snd-mrg-struc s-cxn))
                                                     :units nil)))
                             (setf (units cxn)
                                   (mapcar (lambda (unit)
                                             (make-instance 'gp-unit
                                                            :name (cdr (assoc 'name unit))
                                                            :prod-lock (cdr (assoc 'prod-lock unit))
                                                            :comp-lock (cdr (assoc 'comp-lock unit))
                                                            :cont-part (cdr (assoc 'cont-part unit))
                                                            :tunit (cdr (assoc 'tunit unit))
                                                            :cxn cxn))
                                           (cdr (assoc 'units s-cxn))))
                             cxn))
                         (cdr (assoc'cxns sexpr)))
                 :dependencies nil)))
    (setf (dependencies data)
          (mapcar (lambda (dep)
                    (make-instance 'gp-dependency
                                   :from (find-gp-unit (cdr (assoc 'unit-name (cdr (assoc 'from dep))))
                                                       (cdr (assoc 'cxn-id (cdr (assoc 'from dep))))
                                                       data)
                                   :to (find-gp-unit (cdr (assoc 'unit-name (cdr (assoc 'to dep))))
                                                     (cdr (assoc 'cxn-id (cdr (assoc 'to dep))))
                                                     data)
                                   :tunit (cdr (assoc 'tunit dep))
                                   :from-last-p (cdr (assoc 'from-last-p dep))
                                   :bindings (mapcar (lambda (binding)
                                                       (make-instance 'gp-binding
                                                                      ;:feature-path (cdr (assoc 'feature-path binding))
                                                                      ;:from-value (cdr (assoc 'from-value binding))
                                                                      ;:to-value (cdr (assoc 'to-value binding))
                                                                      :lock-path (cdr (assoc 'lock-path binding))
                                                                      :features (cdr (assoc 'features binding))
                                                                      :lock-values (cdr (assoc 'lock-values binding))
                                                                      :bindings (cdr (assoc 'bindings binding))
                                                                      :set-of-predicates-p (cdr (assoc 'set-of-predicates-p binding))))
                                                     (cdr (assoc 'bindings dep)))))
                  (cdr (assoc 'dependencies sexpr))))
    data))

(defun read-data (&key (filename ".tmp/data.sexpr"))
  (let ((sexpr nil))
    (with-open-file (s filename)
      (setf sexpr (read s)))
    (sexpr->gp-data sexpr)))

(defun write-data (data &key (filename ".tmp/data") (formats '(sexpr json)))
  (when formats
    (case (car formats)
       (sexpr (write-data-sexpr data (string-append filename ".sexpr")))
       (json (write-data-json data (string-append filename ".json"))))
    (write-data data :filename filename :formats (cdr formats))))

(defun write-data-sexpr (data filename)
  (with-open-file (stream filename :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (format stream "~a"  (gp-data->sexpr data))))

(defun write-data-json (data filename)
  (with-open-file (stream filename :direction :output :if-exists :overwrite :if-does-not-exist :create)
    (format stream "~a" (gp-data->json data))))

