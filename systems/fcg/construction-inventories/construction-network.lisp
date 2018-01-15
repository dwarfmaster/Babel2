(in-package :fcg)

(export '(construction-node construction-network find-cxn-node construction-nodes link-constructions
          construction-inventory->construction-network))

(defclass construction-node (net-node)
  ((construction :type (or null construction)
		 :accessor construction
		 :accessor cxn
		 :initarg :construction
		 :initarg :cxn
		 :initform nil
		 :documentation "Just a regular construction"))
  (:documentation "A construction-node is a net-node that contains a
  construction. This has-a relation is better than a is-a relation
  since you might want to use the exact same construction in different
  nets. (For example when you have a production and parsing net.)"))

(defmethod name ((cxn-node construction-node))
  (name (construction cxn-node)))

(defmethod copy-object-content ((source construction-node) (destination construction-node))
  (setf (construction destination) (copy-object (construction source))))


(defclass construction-network (construction-inventory network)
  ()
  (:documentation "A construction-network is a construction-inventory
  and a network."))

(defmethod initialize-instance :after ((construction-network construction-network)
                                       &key &allow-other-keys)
  (set-configuration (configuration construction-network) 
                     :cxn-supplier-mode :simple-queue))

(defmethod copy-object-content ((source construction-network) (destination construction-network))
  ;; This is done by the copy-object-content of network
  )

(defmethod constructions ((construction-network construction-network) &key &allow-other-keys)
  (loop for node in (nodes construction-network)
     when (typep node 'construction-node)
     collect (construction node)))

(defmethod (setf constructions) (constructions-list (construction-network construction-network))
  (setf (nodes construction-network) 
	(mapcar #'(lambda (cxn) (make-instance 'construction-node
					       :construction cxn))
		constructions-list)))

(defgeneric construction-nodes (construction-network &key &allow-other-keys)
  (:documentation "Instead of returning the real constructions this
  one returns the nodes that encapsulate the constructions."))

(defmethod construction-nodes ((construction-network construction-network) &key &allow-other-keys)
  (loop for node in (nodes construction-network)
     when (typep node 'construction-node)
     collect node))

(defmethod find-cxn ((construction construction) 
                     (construction-network construction-network)
		     &key (key #'name) (test #'eql))
  ;; I use find-if because I have to check on the type and like this I
  ;; loop only once. I also cannot pass the key to the find-if because
  ;; it might not be applicable to every node.
  (let ((construction-node (find-if #'(lambda (node) (and (typep node 'construction-node)
							  (funcall test (funcall key (construction node)) 
								   (funcall key construction))))
				    (nodes construction-network))))
    (when construction-node
      (values (construction construction-node) construction-node))))

(defmethod find-cxn ((construction t)
                     (construction-network construction-network)
		     &key (key #'identity) (test #'eql))
  ;; See explanation for find-if in comment above.
  (let ((construction-node (find-if #'(lambda (node) (and (typep node 'construction-node)
							  (funcall test (funcall key (construction node)) 
								   construction)))
				    (nodes construction-network))))
    (when construction-node
      (values (construction construction-node) construction-node))))

(defun find-cxn-node (construction construction-network &key (key #'identity) (test #'eql))
  (multiple-value-bind (cxn construction-node)
      (if (typep construction 'construction)
          (find-cxn (persistent-cxn construction) construction-network :key key :test test)
          (find-cxn construction construction-network :key key :test test))
    (declare (ignore cxn))
    construction-node))

(defmethod add-cxn ((construction construction)
                    (construction-network construction-network)
		    &key (check-consistency nil) edges)
  (let ((construction-node (make-instance 'construction-node :construction construction)))
    (add-node construction-network construction-node
	      :edges edges :check-consistency check-consistency)
    (values construction-network construction-node)))


(defmethod delete-cxn ((construction construction) 
                       (construction-network construction-network) 
		       &key (key #'identity) (test #'eql))
  (delete-node construction-network (funcall key construction) 
	       :key #'(lambda (el)
			(when (typep el 'construction-node) (funcall key (construction el)))) 
	       :test test)
  construction-network)

(defmethod clear ((construction-network construction-network) &key &allow-other-keys)
  "Removes all nodes that are of type 'construction. Also removes all edges
connected to them. Thus if all edges connect constructions then it will clear
the complete network."
  (setf (nodes construction-network)
	(loop for node in (nodes construction-network)
	   if (typep node 'construction-node)
	   do (loop for edge in (node-edges node)
		 do (setf (edges construction-network) (delete edge (edges construction-network))))
	   else collect node
	   finally (complete-edges construction-network)))
  construction-network)

(defgeneric link-constructions (start end label construction-network &key &allow-other-keys)
  (:documentation "Creates and adds an edge between the given
  constructions with the label. It assumes that start and end can be
  found in the network."))

(defmethod link-constructions ((start construction-node) (end construction-node) 
			       (label symbol) (construction-network construction-inventory) &key)
  (create-and-add-edge construction-network label start end))

(defmethod link-constructions ((start construction) (end construction) 
			       (label symbol) (construction-network construction-inventory) &key)
  (link-constructions (find-cxn-node start construction-network)
		      (find-cxn-node end construction-network) label construction-network))

(defmethod link-constructions ((start symbol) (end symbol) 
			       (label symbol) (construction-network construction-inventory) &key)
  ;; We assume start and end are names of constructions
  (link-constructions (find-cxn-node start construction-network :key #'name)
		      (find-cxn-node end construction-network :key #'name) label construction-network))

(defmethod construction-inventory->construction-network ((rc construction-inventory) &key &allow-other-keys)
  (with-disabled-monitors
    (loop 
       with rn = (make-instance 'construction-network)
       for construction in (constructions rc)
       do (add-cxn construction rn)
       finally 
         (setf (configuration rn) (copy-object (configuration rc)))
         (return rn))))

;; (def-transitive_cxn :name creating_cxn
;;                     :meaning ...
;;                     :form ....
;;                     :outgoing-relations ((transitive_cxn inheritance_polysemy)))

