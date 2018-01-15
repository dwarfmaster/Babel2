;;;;
;;;; File: configuration.lisp
;;;;
;;;; Methods for configuring information processing
;;;; e.g. for an agent or experiment
;;;;

(in-package :utils)

;; ############################################################################

(export '(configuration
	  make-configuration
          make-config
	  entries
	  parent-configuration
	  get-configuration
          set-configuration
          set-configurations
          get-configuration-or-default
	  define-configuration-default-value
          require-configuration))

(defclass configuration ()
  ((configuration :initarg :entries
                  :accessor entries
                  :accessor configuration
                  :initform nil :type list
                  :documentation "alist of key value pairs")
   (parent-configuration :initarg :parent-configuration
                         :accessor parent-configuration
                         :initform nil :type (or null configuration))))

(defmethod initialize-instance :after ((c configuration)
                                       &key entries configuration &allow-other-keys)
  "it is possible to pass a configuration instance or an alist with
   through the initarg :configuration. This method handles both cases."
  ;; you can't use both :entries and :configuration as initargs
  (assert (not (and configuration entries)))
  (when configuration
    (setf (entries c)
          (if (typep configuration 'configuration)
            (entries configuration)
            configuration))))

;; ----------------------------------------------------------------------------  

(defun make-configuration (&key entries parent-configuration)
  "deprecated. configuration was a struct before"
  (make-instance 'configuration
                 :entries entries
                 :parent-configuration parent-configuration))

(defmacro make-config (&rest key-value-lists)
  "Example: (make-config (key1 123) (key2 'symbol))
   The keys are quoted while the values are evaluated."
  `(make-configuration :entries
                       (list . ,(loop for (key value) in key-value-lists
                                      collect `(cons ',key ,value)))))

;; ----------------------------------------------------------------------------

(defmethod print-object ((configuration configuration) stream)
  (if *print-pretty*
    (pprint-logical-block (stream nil)
      (format stream "<configuration:~:_ ~{~a~^,~:_ ~}" 
              (loop for (key . value) in (slot-value configuration 'configuration)
                    collect (format nil "~(~a~): ~a" key value)))
      (format stream ">"))
    (format stream "<configuration (~a entries)>" 
            (length (entries configuration)))))

;; ----------------------------------------------------------------------------

(defmethod copy-object ((configuration configuration))
  (let ((copy (make-configuration)))
    (copy-object-content configuration copy)
    copy))

(defmethod copy-object-content ((source configuration)
				(destination configuration))
  (setf (entries destination)
	(copy-alist (entries source))))

;; ----------------------------------------------------------------------------
;; get-configuration

(defgeneric get-configuration (object key &key)
  (:documentation "Gets the configuration value for a key"))

(defmethod get-configuration ((entries list) key &key)
  (let ((entry (assoc key entries
		      :test #'(lambda (entry-1 entry-2)
				(equalp (symbol-name entry-1) (symbol-name entry-2))))))
    (values (rest entry) (not (null entry)))))

(defmethod get-configuration ((configuration configuration) (key t)
                              &key omit-owner)
  (multiple-value-bind
      (entry found) (get-configuration (entries configuration) key)
    (cond
     ((and (not found) (parent-configuration configuration))
      (get-configuration (parent-configuration configuration) key))
     ((and (not found) (not omit-owner))
      (get-configuration (owner configuration) key))
     (t
      (values entry found)))))

(defun get-configuration-or-default (configuration key default)
  "checks configuration (can be nil) and returns the found configuration or
   default"
  (multiple-value-bind (value key-exists)
      (get-configuration configuration key)
    (if key-exists
      value
      default)))

;; ----------------------------------------------------------------------------
;; set-configuration

(defgeneric set-configuration (object key value &key replace)
  (:documentation "Sets the configuration value for the given key, replacing
     an already present value if :replace is true."))

(defmethod set-configuration ((configuration configuration) key value
                              &key (replace t)
                              &allow-other-keys)
  (let ((previous-entry (assoc key (entries configuration) 
			       :test #'(lambda (entry-1 entry-2)
					 (equalp (symbol-name entry-1) (symbol-name entry-2))))))
    (if previous-entry
      (when replace (setf (cdr previous-entry) value))
      (push (cons key value) (entries configuration)))
    configuration))

(defun set-configurations (configuration configurations &key (replace t))
  (loop for (key . value) in configurations
        do (set-configuration configuration key value :replace replace)))

;; ----------------------------------------------------------------------------
;; 

(defmacro define-configuration-default-value (key value)
  "when a configuration was not set, this value is returned from
   a call to (get-configuration x key)"
  `(defmethod get-configuration :around ((configuration configuration) (key (eql ,key)) &key)
       (multiple-value-bind (returned-value key-exists)
           (call-next-method)
         (if key-exists
             (values returned-value key-exists)
             (values ,value :default)))))

;; ----------------------------------------------------------------------------
;; 

(defmacro require-configuration (key)
  "throws an error when a configuration was not set"
  `(defmethod get-configuration :around ((object t) (key (eql ,key)) &key)
     (multiple-value-bind (value key-exists)
	 (call-next-method)
       (unless key-exists 
	 (error 
	  (format nil "Please set the ~(~:w~) configuration in ~a" ,key object)))
       (values value key-exists))))

;; ############################################################################
