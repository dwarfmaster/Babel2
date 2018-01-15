
(in-package :fcg)

(export '(constructions size clear find-cxn add-cxn delete-cxn cxn-added cxn-deleted
          set-expansion-data-for-type get-expansion-data-for-type expansion-data configuration visualization-configuration))

(defclass construction-inventory ()
  ((name
    :type symbol
    :initform (gensym "GRAMMAR-")
    :initarg :name
    :accessor name
    :documentation "The name of the construction inventory. Typically,
this is the name that you use in the def-fcg-constructions macro.")
   (data :type blackboard :initform (make-blackboard)
         :initarg :blackboard
         :accessor blackboard)
   (configuration 
    :type configuration
    :initform (make-config)
    :initarg :configuration
    :accessor configuration
    :documentation "Determines the behavior of the construction inventory in processing")
   (visualization-configuration 
    :type configuration
    :initform (make-config)
    :initarg :visualization-configuration
    :accessor visualization-configuration
    :documentation "Determines the behavior of the visualization in the web-interface")
   (expansion-data 
    :type blackboard
    :initform (make-instance 'blackboard)
    :accessor expansion-data
    :documentation "Data for constructions using the expansion
    operator ++.")
   (hierarchy-features 
    :type (or list null)
    :initform '(subunits)
    :initarg :hierarchy-features
    :accessor hierarchy-features)
   (original-cxn-set
   ;; This is a pointer back to the fcg-light construction-inventory to which this construction-set belongs
   ;; (to which is appears in the slot 'processing-cxn-inventory'). So, if you know which cxn applied,
   ;; you know where to find that cxn in fcg-light notation. Type should actually be fcg-construction-inventory,
   ;; a subtype of construction-inventory  but as this class in loaded in fcg-light, the ccl compiler is confused
   ;; when loading fcg, and lets the tests in test-construction-inventory fail, lispworks does fine with both.
   :type (or null construction-inventory) :initform nil :initarg :original-cxn-set :accessor original-cxn-set))
  (:documentation "A construction-inventory is an abstract class of
                   which every organization of constructions should
                   subclass."))

(defun set-expansion-data-for-type (construction-inventory &key type data)
  "If label is new, it is added. If the label already existed its data
is overwritten."
  (set-data (expansion-data construction-inventory) type data))

(defun get-expansion-data-for-type (construction-inventory &key type)
  (find-data (expansion-data construction-inventory) type))

(defmethod initialize-instance :after ((construction-inventory construction-inventory)
                                       &key &allow-other-keys)
  (set-configuration construction-inventory
                     :root-mode t)
  (set-configuration construction-inventory
                     :production-order '(lex cxn morph)) 
  (set-configuration construction-inventory
                     :parse-order '(morph lex cxn))
  (set-configuration construction-inventory
                     :create-initial-structure-mode :one-pole-mode :replace nil)
  (set-configuration construction-inventory
                     :render-mode :render-string-meets-precedes :replace nil)
  (set-configuration construction-inventory 
                     :de-render-mode :de-render-string-meets-precedes :replace nil)
  (set-configuration construction-inventory
                     :parse-goal-tests '(:no-applicable-cxns) :replace nil)
  (set-configuration construction-inventory
                     :production-goal-tests '(:no-applicable-cxns) :replace nil)
  (set-configuration construction-inventory
                     :draw-meaning-as-network t :replace nil)
  (set-configuration construction-inventory
                     :shuffle-cxns-before-application t :replace nil)
  (set-configuration construction-inventory
                     :cxn-supplier-mode :ordered-by-label-and-score :replace nil)
  (set-configuration construction-inventory
                     :use-meta-layer t :replace nil)
  (set-configuration construction-inventory
                      :consolidate-repairs nil :replace nil)
  (set-configuration construction-inventory
                     :form-predicates'(meets precedes fields first) :replace nil)
  (set-configuration construction-inventory 
                     :equivalent-meaning-mode :unify-no-equalities :replace nil)
  (set-configuration construction-inventory
                     :node-expansion-mode :default :replace nil)
  (set-configuration construction-inventory
                     :queue-mode :by-priority :replace nil)
  (set-configuration construction-inventory
                     :priority-mode :depth-first :replace nil)
  (set-configuration construction-inventory
                     :node-tests '(:check-duplicate :restrict-nr-of-nodes :restrict-search-depth) :replace nil)
  (set-configuration construction-inventory
                     :max-search-depth 25 :replace nil)
  (set-configuration construction-inventory
                     :max-nr-of-nodes 250 :replace nil)
  (set-configuration construction-inventory
                     :show-meaning/utterance nil :replace nil)
  (set-configuration construction-inventory
                     :update-boundaries-feature 'subunits)
  
  ;; Set default visualization configuration
  (set-configuration (visualization-configuration construction-inventory) :coupled-mode nil :replace nil)
  (set-configuration (visualization-configuration construction-inventory) :expand-nodes-in-search-tree t :replace nil) 
  (set-configuration (visualization-configuration construction-inventory) :subfeatures nil :replace nil)
  (set-configuration (visualization-configuration construction-inventory) :hide-features '(footprints) :replace nil)
  (set-configuration (visualization-configuration construction-inventory) :selected-hierarchy 'subunits :replace nil)
  (set-configuration (visualization-configuration construction-inventory) :select-subfeatures nil :replace nil)
  (set-configuration (visualization-configuration construction-inventory) :with-search-debug-data nil :replace nil)
  (set-configuration (visualization-configuration construction-inventory) :show-upper-menu nil :replace nil)
  (set-configuration (visualization-configuration construction-inventory) :remove-empty-units nil :replace nil)
  (set-configuration (visualization-configuration construction-inventory) :add-form-and-meaning-to-car t :replace nil)
  (set-configuration (visualization-configuration construction-inventory) :latex-visualization t :replace nil)
  (set-configuration (visualization-configuration construction-inventory) :show-constructional-dependencies t :replace nil)
  (set-configuration (visualization-configuration construction-inventory) :labeled-paths nil :replace nil)
  (set-configuration (visualization-configuration construction-inventory) :colored-paths nil :replace nil)
  )

;; ------------------------------------------------------------------------
;; constructions

(defgeneric constructions (construction-inventory &key &allow-other-keys)
  (:documentation "Returns all constructions of an inventory as a list."))

(defgeneric (setf constructions) (construction-list construction-inventory)
  (:documentation "Sets the constructions of an inventory."))

;(defmethod (setf constructions) ((construction-list list) (construction-inventory construction-inventory))
;  "Default implementation invoking add-cxn for every element of the list"
;  (loop for cxn in construction-list
;     do
;       (add-cxn cxn construction-inventory)))

(defgeneric size (construction-inventory)
  (:documentation "Returns the number of constructions stored in this inventory."))

(defmethod size ((construction-inventory construction-inventory))
  "Default implementation returning (length (constructions ci))"
  (length (constructions construction-inventory)))

;; ------------------------------------------------------------------------
;; clear

(defgeneric clear (construction-inventory &key &allow-other-keys)
  (:documentation "Removes all constructions from an inventory"))



;; ------------------------------------------------------------------------
;; add-cxn

(defgeneric add-cxn (construction construction-inventory &key &allow-other-keys)
  (:documentation "Adds a construction to a construction inventory"))

(define-event cxn-added (construction construction)  
              (construction-inventory construction-inventory))

(defmethod add-cxn :before ((construction construction)
                            (construction-inventory construction-inventory)
                            &key (replace-when-equivalent t)
                            (equivalent-test #'eql) (equivalent-key #'name))
  (when replace-when-equivalent
    (delete-cxn construction construction-inventory 
                :test equivalent-test :key equivalent-key)))

(defmethod add-cxn :after ((construction construction)
                           (construction-inventory construction-inventory) &key)
  ;(notify cxn-added construction construction-inventory)
  )



;; ------------------------------------------------------------------------
;; delete-cxn

(defgeneric delete-cxn (construction construction-inventory &key test key)
  (:documentation "Deletes a construction from a construction
                   inventory (destructive). Returns the deleted
                   construction (or nil when it could not be found in
                   the inventory"))

(defmethod delete-cxn ((construction t)
                       (construction-inventory construction-inventory)
		       &key (key #'identity) (test #'eql))
  (let ((construction (find-cxn construction construction-inventory :key key :test test)))
    (when construction
      (delete-cxn construction construction-inventory))))

(define-event cxn-deleted (construction construction)
  (construction-inventory construction-inventory))

(defmethod delete-cxn :around ((construction construction)
                               (construction-inventory construction-inventory)
                               &key &allow-other-keys)
  (let ((result (call-next-method)))
    (when result
      (notify cxn-deleted construction construction-inventory)
      result)))


;; ------------------------------------------------------------------------
;; find-cxn 

(defgeneric find-cxn (construction construction-inventory 
                                   &key test key &allow-other-keys)
  (:documentation "Finds a construction in the inventory"))


;; ------------------------------------------------------------------------
;; implementations of other methods

(defmethod copy-object-content ((source construction-inventory)
                                (destination construction-inventory))
  ;; For FCG-Light, we don't want to copy the configuration here (it already happend)
  ;; but the method combination tries to do it again here
  (unless (string= (type-of source) 'fcg-construction-set) 
    (setf (configuration destination) (copy-object (configuration source)))
    (setf (blackboard destination) (blackboard source)))
  (setf (hierarchy-features destination) (copy-list (hierarchy-features source)))
  (setf (original-cxn-set destination) (original-cxn-set source))
  
  ;; note that expansion-data is not deeply copied!
  (setf (expansion-data destination) (expansion-data source)))


(defmethod set-configuration ((construction-inventory construction-inventory) 
                              key value &key (replace t))
  (set-configuration (configuration construction-inventory)
                     key value :replace replace))

(defmethod get-configuration ((construction-inventory construction-inventory) key &key)
  (get-configuration (configuration construction-inventory) key))


(defmethod print-object ((construction-inventory construction-inventory) stream)
  (format stream "<~(~a~): ~a cxns>" 
          (class-name (class-of construction-inventory))
          (size construction-inventory)))

;; ----------------------------------------------------------------------------
;; Functions for storing and restoring large grammars

(defun store-grammar (grammar &key(directory '(".tmp"))
                              (name (format nil "~a-cxns-grammar" (size grammar)))
                              (type "lisp"))
  "Stores the grammar 'grammar' in path 'file-path'"
  (let ((path (babel-pathname :directory directory :name name :type type)))
    (cl-store:store grammar path)))

;; (store-grammar *fcg-constructions*)

(defun restore-grammar (filename &key (directory '(".tmp")) (type "lisp"))
  "Loads and returns the grammar 'grammar' in path 'file-path'"
  (let ((path (babel-pathname :directory directory :name filename :type type)))
    (cl-store:restore path)))