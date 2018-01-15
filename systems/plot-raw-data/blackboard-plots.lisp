(in-package :plot-raw-data)

(export '(create-bar-plot-from-serialized-data create-bar-plot-a-vs-b-from-serialized-data create-evo-plot-from-serialized-data create-stacked-bar-plot-from-serialized-data))
(export '(serialize-plot-data reserialize-data-with-args get-plot-data-from-blackboards write-serialized-plot-data read-serialized-plot-data append-groups replace-recursively select-series-recursively))

;; assembles one argument-list
(defun serialize-plot-data (data &rest args)
  `(,@args :data ,data))

;; replaces values from existing argument-list
;; (e.g. for using the same data with other plot parameters)
(defun reserialize-data-with-args (data &rest args)
  (loop with new-data = data
        for key in args
        for val in (cdr args)
        if (and (keywordp key) (not (keywordp val)))
        do (setf new-data (add-or-replace-keyword-value-pair key val new-data))
        finally (return new-data)))

(defmethod get-plot-data-from-blackboards (blackboards (field symbol))
  (loop for v in blackboards
        if (typep v 'blackboard)
        collect (reverse-recursively (get-data v field))
        if (listp v)
        collect (get-plot-data-from-blackboards v field)))

(defmethod get-plot-data-from-blackboards (blackboards (fields list))
  (loop for field in fields
        if (typep field 'symbol)
        append (get-plot-data-from-blackboards blackboards field)))

(defun create-evo-plot-from-serialized-data (arg-list)
  (apply #'create-evo-plot-from-serialized-data-aux arg-list))

(defun create-evo-plot-from-serialized-data-aux (&rest args &key data average-windows &allow-other-keys)
  (let* ((data (if average-windows (apply-average-window data average-windows) data))
         (batch-points (convert-to-batch-points-for-evo-plot data)))
    (apply #'plot-evo-data batch-points `(,@args))))

(defun create-bar-plot-from-serialized-data (arg-list)
  (apply #'create-bar-plot-from-serialized-data-aux arg-list))

(defun create-bar-plot-from-serialized-data-aux (&rest args &key data &allow-other-keys)
  (let ((batch-points (convert-to-batch-points-for-bar-plot data)))
    (apply #'create-bar-plot `(:data ,batch-points ,@args))))

(defun create-stacked-bar-plot-from-serialized-data (arg-list)
  (apply #'create-stacked-bar-plot-from-serialized-data-aux arg-list))

(defun create-stacked-bar-plot-from-serialized-data-aux (&rest args &key data &allow-other-keys)
  (let ((batch-points (convert-to-batch-points-for-bar-plot data)))
    (apply #'create-stacked-bar-plot-a-b `(:data ,batch-points ,@args))))

(defun create-bar-plot-a-vs-b-from-serialized-data (arg-list)
  (apply #'create-bar-plot-a-vs-b-from-serialized-data-aux arg-list))

(defun create-bar-plot-a-vs-b-from-serialized-data-aux (&rest args &key data &allow-other-keys)
  (let ((batch-points (convert-to-batch-points-for-bar-plot data)))
    (apply #'create-bar-plot-a-vs-b `(:data ,batch-points ,@args))))

(defun batchp (s)
  (and (listp s)
       (loop for ss in s
             always (and (listp ss)
                         (loop for v in ss
                               always (or (not v) (numberp v)))))))

(defun convert-to-batch-points-for-bar-plot (s)
  (cond
   ((batchp s) (compute-batch-point (remove-nil-from-batch s) "auto"))
   ((listp s) (loop for v in s
                    for bp = (convert-to-batch-points-for-bar-plot v)
                    if bp
                    collect bp))
   (t nil)))

(defun remove-nil-from-batch (s)
  (loop for serie in s
        collect (remove nil serie)))

(defun convert-to-batch-points-for-evo-plot (s)
  (cond
   ((batchp s) (compute-batch-points-for-evo-plot s "auto"))
   ((listp s) (loop for v in s
                    for bp = (convert-to-batch-points-for-evo-plot v)
                    if bp
                    collect bp))
   (t nil)))

(defun write-serialized-plot-data (data path)
  (with-open-file (stream path :direction :output :if-exists :supersede)
    (write data :stream stream)))

(defun read-serialized-plot-data (path)
  (with-open-file (stream path :direction :input)
    (read stream)))

(defun add-or-replace-keyword-value-pair (keyword value arglist)
  (let ((index (position keyword arglist)))
    (if index
      ;; keyword exists
      (if (and
           (< (1+ index) (length arglist))
           (not (keywordp (nth (1+ index) arglist))))
        ;; value exists: replace
        (replace arglist (list value) :start1 (1+ index))
        ;; value does not exist: insert
        (let ((cell (nthcdr index arglist)))
          (setf (cdr cell) (cons value (cdr cell)))
          arglist))
      ;; keyword does not exist add to front
      (progn
        (push value arglist)
        (push keyword arglist)
        arglist))))

(defun get-value-from-serialilzed-data (data keyword)
  (let ((index (position keyword data)))
    (when (and
         index
         (< (1+ index) (length data))
         (not (keywordp (nth (1+ index) data))))
      (nth (1+ index) data))))

(defun apply-average-window (s window)
  (cond
   ((and (batchp s) (numberp window))
    (data-apply-average-window (mapcar #'list->array s) window))
   ((and (listp s) (listp window))
    (loop for ss in s
          for i from 0
          for w = (or (nth i window) 1)
          collect (apply-average-window ss w)))
   (t nil)))

(defun reverse-recursively (x)
  (if (listp x)
    (mapcar #'reverse-recursively (reverse x))
    x))
(defun append-groups (data)
  (reduce #'append data))


(defun replace-recursively (val source-value target-value &key (test 'eq))
  (cond
   ((and val (listp val))
    (loop for e in val
          collect (replace-recursively e source-value target-value :test test)))
   ((eq val source-value) target-value)
   (t val)))


(defun select-series-recursively (val serie-numbers)
  (cond
   ((and val (batchp val))
    (loop for serie-number in serie-numbers
          for serie = (nth serie-number val)
          if serie
          collect serie))
   ((and val (listp val))
    (loop for e in val
          collect (select-series-recursively e serie-numbers)))
   (t val)))