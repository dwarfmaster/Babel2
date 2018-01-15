(in-package :fcg)


;; ############################################################################
;; construction-inventory-collection
;; ############################################################################

(export '(construction-inventory-collection construction-inventories
add-construction-inventory delete-construction-inventory
find-construction-inventory))

(defclass construction-inventory-collection (construction-inventory)
  ((construction-inventories :documentation "A blackboard containing construction-inventories."
			     :type blackboard
			     :initarg :construction-inventories
			     :accessor construction-inventories
                             :initform (make-instance 'blackboard)))
  (:documentation "A construction-inventory-collection is a
  construction-inventory and contains a blackboard. This blackboard
  should contain other construction-inventories. Optionally you can
  also make use of the active-construction-inventory but be carefull
  to always update this correctly."))

(defmethod initialize-instance :after ((construction-inventory-collection construction-inventory-collection) 
				       &key &allow-other-keys)
  (set-configuration (configuration construction-inventory-collection) 
		     :cxn-supplier-mode :simple-queue)
  (loop 
     for (nil . construction-inventory) 
     in (data-fields (construction-inventories construction-inventory-collection))
     do (setf (parent-configuration (configuration construction-inventory))
	      (configuration construction-inventory-collection))))

(defmethod print-object ((construction-inventory-collection construction-inventory-collection) stream)
  (if *print-pretty*
      (pprint-logical-block (stream nil)
	(format 
	 stream "<~a :~:_ construction-inventories: ~a ~:_ ~:w> "
	 (class-name (class-of construction-inventory-collection))
	 (loop 
	    for (label . construction-inventory) 
	    in (data-fields (construction-inventories construction-inventory-collection))
	    collect (format nil "~a: ~a" label construction-inventory))
	 (configuration construction-inventory-collection)))
      (format stream "<~a : ~a constructions>" 
	      (class-name (class-of construction-inventory-collection))
	      (size construction-inventory-collection))))

(defmethod copy-object-content ((source construction-inventory-collection) 
				(destination construction-inventory-collection))
  "copy-objects all of the slots of the blackboard"
  (setf (construction-inventories destination) (copy-object (construction-inventories source))))

(defmethod size ((cic construction-inventory-collection))
  "Default implementation returning (length (constructions ci))"
  (let ((data (data (construction-inventories cic))))
    (if data
      (length (constructions (rest (first data))))
      0)))

(defgeneric add-construction-inventory (construction-inventory-collection label construction-inventory &key &allow-other-keys)
  (:documentation "Adds the construction-inventory to the construction-inventory-collection"))

(defmethod add-construction-inventory ((cic construction-inventory-collection) (label symbol) 
				       (construction-inventory construction-inventory) &key)
  (setf (parent-configuration (configuration construction-inventory)) (configuration cic))
  (set-data (construction-inventories cic) label construction-inventory))

(defgeneric find-construction-inventory (construction-inventory-collection label)
  (:documentation "Returns the construction-inventory stored by the given label."))

(defmethod find-construction-inventory ((rcc construction-inventory-collection) (label symbol))
  (find-data (construction-inventories rcc) label))

(defgeneric delete-construction-inventory (construction-inventory-collection label)
  (:documentation "Deletes the construction-inventory stored with the given label."))

(defmethod delete-construction-inventory ((rcc construction-inventory-collection) (label symbol))
  (let ((construction-inventory (find-construction-inventory rcc label)))
    (when construction-inventory
      (setf (parent-configuration (configuration construction-inventory)) nil)
      (remove-data (construction-inventories rcc) label))))

(defmethod constructions ((construction-inventory-collection construction-inventory-collection) 
			  &key (label nil) &allow-other-keys)
  (if label
      (constructions (find-construction-inventory construction-inventory-collection label))
      ;; else
      (loop 
	 for (nil . construction-inventory) 
	 in (data-fields (construction-inventories construction-inventory-collection))
	 when (typep construction-inventory 'construction-inventory)
	 append (constructions construction-inventory))))

(defmethod (setf constructions) (constructions-list 
				 (construction-inventory-collection construction-inventory-collection))
  (loop 
     for (nil . construction-inventory) 
     in (data-fields (construction-inventories construction-inventory-collection))
     when (typep construction-inventory 'construction-inventory)
     do (setf (constructions construction-inventory) constructions-list)))

(defmethod delete-cxn ((construction t)
                       (construction-inventory construction-inventory-collection)
		       &key (key #'identity) (test #'eql) (label nil))
  (delete-cxn (find-cxn construction construction-inventory :key key :test test :label label)
              construction-inventory))

(defmethod delete-cxn ((construction construction)
                       (construction-inventory-collection construction-inventory-collection) 
                       &key (key #'identity) (test #'eql) (label nil))
  (if label
      (delete-cxn construction
                  (find-construction-inventory
                   construction-inventory-collection label) 
                  :key key :test test)
      (loop 
	 for (nil . construction-inventory) 
	 in (data-fields (construction-inventories construction-inventory-collection))
	 when (typep construction-inventory 'construction-inventory)
	 do (delete-cxn construction construction-inventory :key key :test test)))
  construction-inventory-collection)



(defmethod add-cxn :around ((construction construction)
                            (construction-inventory construction-inventory-collection)
                            &key (replace-when-equivalent t) (label nil)
                            (equivalent-test #'eql) (equivalent-key #'name))
  ;; This around is a hack to circumvent the :before method that
  ;; deletes when replace-when-equivalent because it doesn't know
  ;; about the :label and will delete in all cxn-inventories.
  (when replace-when-equivalent
    (delete-cxn construction construction-inventory 
                :test equivalent-test :key equivalent-key :label label))
  (call-next-method construction construction-inventory 
		    :replace-when-equivalent nil :label label))

(defmethod add-cxn ((construction construction)
                    (rcc construction-inventory-collection)
		     &key (replace-when-equivalent t) (equivalent-test #'eql) 
		    (equivalent-key #'name) (label nil) (copy-construction nil))
  (if label
      (add-cxn construction (find-construction-inventory rcc label)
               :replace-when-equivalent nil)
      (loop 
	 for (nil . construction-inventory) 
	 in (data-fields (construction-inventories rcc))
	 when (typep construction-inventory 'construction-inventory)
	 do (add-cxn (if copy-construction (copy-object construction) construction)
                     construction-inventory
		     :replace-when-equivalent replace-when-equivalent
                     :equivalent-test equivalent-test
		     :equivalent-key equivalent-key)))
  rcc)

(defmethod find-cxn ((construction t) 
                     (construction-inventory-collection construction-inventory-collection)
		     &key (key #'identity) (test #'eql) (label nil))
  (if label
      (find-cxn construction
                (find-construction-inventory construction-inventory-collection label)
                :key key :test test)
      (loop 
	 for (nil . construction-inventory) 
	 in (data-fields (construction-inventories construction-inventory-collection))
	 when (and (typep construction-inventory 'construction-inventory)
		   (find-cxn construction construction-inventory
                             :key key :test test))
	 ;; remember that the last value of the and is returned which is if all goes well the construction
	 return it)))

(defmethod create-and-add-edge ((rcc construction-inventory-collection) (label symbol) (start t) (end t) 
				&key (node-key #'identity) (node-test #'eq) (rc-label nil) &allow-other-keys)
  (if rc-label
      (create-and-add-edge (find-construction-inventory rcc rc-label) label start end
			   :node-test node-test :node-key node-key)
      (loop 
	 for (nil . construction-inventory) 
	 in (data-fields (construction-inventories rcc))
	 when (typep construction-inventory 'construction-inventory)
	 do (create-and-add-edge construction-inventory label start end
				 :node-test node-test :node-key node-key))))

(defmethod clear ((construction-inventory-collection construction-inventory-collection) 
		  &key (label nil))
  "Removes all nodes that are of type 'construction. Also removes all edges
connected to them. Thus if all edges connect constructions then it will clear
the complete network."
  (if label
      (let ((found-cic (find-construction-inventory construction-inventory-collection label)))
	(when found-cic (clear found-cic)))
      (loop 
	 for (nil . construction-inventory) 
	 in (data-fields (construction-inventories construction-inventory-collection))
	 when (typep construction-inventory 'construction-inventory)
	 do (clear construction-inventory)))
  construction-inventory-collection)
 

;; (defmethod print-constructions ((construction-inventory-collection construction-inventory-collection) 
;; 			&key (key #'identity) (label nil))
;;   (loop for (label-1 . construction-inventory) in (data-fields (construction-inventories construction-inventory-collection))
;; 	  when (and (or (not label)
;; 		   (equal label label-1))
;; 	       (typep construction-inventory 'construction-inventory))
;; 	  do (print-constructions construction-inventory :key key)))

;; ----------------------------------------------------------------------------
;; Delegate data-field accessing to the construction-inventories data of the construction-inventory-collection

(defmethod add-data-field ((construction-inventory-collection construction-inventory-collection) label
			   &optional (initial-value nil))
  (add-data-field (construction-inventories construction-inventory-collection) label initial-value))

(defmethod get-data ((construction-inventory-collection construction-inventory-collection) label &key)
  (get-data (construction-inventories construction-inventory-collection) label))

(defmethod find-data ((construction-inventory-collection construction-inventory-collection) label &key)
  (find-data (construction-inventories construction-inventory-collection) label))

(defmethod set-data ((construction-inventory-collection construction-inventory-collection) label data)
  (set-data (construction-inventories construction-inventory-collection) label data))

(defmethod fields ((construction-inventory-collection construction-inventory-collection))
  (fields (construction-inventories construction-inventory-collection)))

(defmethod field? ((construction-inventory-collection construction-inventory-collection) label)
  (field? (construction-inventories construction-inventory-collection) label))
