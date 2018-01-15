(in-package :network)

(export '(net-node id edges net-edge label node-edges score start end network short-id
nodes edges add-edge add-node primitives complete-edges delete-node delete-edge
deep-copy-network neighbours all-neighbours neighbours-in neighbours-out make-net-edge create-and-add-edge get-sub-network))

(defclass net-node ()
  ((id :documentation "Every node must have a unique ID."
       :initform (gensym)
       :initarg :id
       :accessor id)
   (node-edges :documentation "The list of incoming and outgoing edges
  for this node."
	 :type list
	 :initform nil
	 :initarg :node-edges
	 :accessor node-edges))
  (:documentation "Inherit from this class to be able to use as a network-node."))

(defmethod copy-object-content ((source net-node) (destination net-node))
  "Normally you should copy full networks with deep-copy-network."
  (setf (id destination) (gensym))
  (setf (node-edges destination) (copy-list (node-edges source))))

(defun short-id (id)
  "A quick helper function that only returns the last three characters
of an ID."
  (let* ((id-string (symbol-name id))
	 (id-length (length id-string)))
    (if (> id-length 3)
	(subseq id-string (- id-length 3))
	id-string)))

(defmethod print-object ((node net-node) stream)
  (pprint-logical-block (stream nil)
    (format stream "<node (~a)" (short-id (id node)))
    (call-next-method)
    (format stream ">")))

(defclass net-edge ()
  ((id :documentation "Every edge must have a unique ID."
       :type symbol
       :initform (gensym)
       :initarg :id
       :accessor id)
   (label :documentation "The label of the edge. Need not be unique."
	  :type symbol
	  :initform nil
	  :initarg :label
	  :accessor label)
   (score :documentation "the score of the edge."
	  :type real
	  :initform 0.5
	  :initarg :score
	  :accessor score)
   (start :documentation "The node from where this edge starts"
	  :type net-node
	  :initform (make-instance 'net-node)
	  :initarg :start
	  :accessor start)
   (end :documentation "The node to where this edge goes"
	:type net-node
	:initform (make-instance 'net-node)
	:initarg :end
	:accessor end))
  (:documentation "Represents an edge in a network of net-nodes"))

(defun make-net-edge (start label end &key (id (gensym)) (score 0.5))
  (make-instance 'net-edge
                 :start start :label label :end end
                 :id id :score score))

(defmethod copy-object-content ((source net-edge) (destination net-edge))
  "Normally you should copy full networks with deep-copy-network."
  (setf (id destination) (gensym))
  (setf (label destination) (label source))
  (setf (score destination) (score source))
  (setf (start destination) (start source)) ;; no copy here (use deep-copy-network)
  (setf (end destination) (end source)))

(defmethod print-object ((edge net-edge) stream)
  (pprint-logical-block (stream nil)
    (format stream "<edge (~a, ~a) ~a" 
	    (short-id (id edge)) (score edge) (label edge))
    (call-next-method)
    (format stream ">")))

(defclass network ()
  ((nodes :documentation "All the nodes in this network"
	  :type list
	  :initform nil
	  :initarg :nodes
	  :accessor nodes)
   (edges :documentation "All the edges in this network"
	  :type list
	  :initform nil
	  :initarg :edges
	  :accessor edges))
  (:documentation "Maintains a labeled network."))

(defgeneric primitives (network)
  (:documentation "Returns all nodes and edges from the network (all
  nodes will be first)"))

(defmethod primitives ((network network))
  (append (nodes network) (edges network)))

(defgeneric check-consistency (node net)
  (:documentation "Can check the consistency of nodes, edges and
  nets. Probably want to use it on nets."))

(defmethod check-consistency ((node net-node) (net network))
  (loop for edge in (node-edges node)
     always (cond ((not (find edge (edges net)))
		   (warn (format nil "~%Problem 1 with ~a" node)))
		  ((not (or (eq node (start edge))
			    (eq node (end edge))))
		   (warn (format nil "~%Problem 2 with ~a" node)))
		  (t t))))

(defmethod check-consistency ((edge net-edge) (net network))
  (cond ((not (find (start edge) (nodes net)))
	 (error (format nil "~%Problem 1 with ~a" edge)))
	((not (find (end edge) (nodes net)))
	 (error (format nil "~%Problem 2 with ~a" edge)))
	((not (find edge (node-edges (start edge))))
	 (error (format nil "~%Problem 3 with ~a" edge)))
	((not (find edge (node-edges (end edge))))
	 (error (format nil "~%Problem 4 with ~a" edge)))
	(t t)))

(defun check-network-consistency (network)
  (loop for prim in (primitives network)
     always (check-consistency prim network)))

(defun complete-edges (net &key (check-consistency t))
  "This function checks whether for example an edge is defined in
the nodes slot of a network but not in both nodes themselves. If it
finds discrepancies it fixes them. In the end the net should meet the
demands of check-network-consistency."
  (loop 
     for edge in (edges net)
     for start-node = (start edge)
     for end-node = (end edge)
     when (and start-node
	       (not (find edge (node-edges start-node))))
     do (push edge (node-edges start-node))
     when (and end-node
	       (not (find edge (node-edges end-node))))
     do (push edge (node-edges end-node)))
  (loop for node in (nodes net)
     do (loop for edge in (node-edges node)
	   unless (find edge (edges net))
	   do (setf (node-edges node) 
		    (remove edge (node-edges node) :count 1))))
  (when check-consistency (check-network-consistency net))
  net)

(defmethod copy-object-content ((source network) (destination network))
  "As long as you don't start modifying edges this should be ok. Else
use deep-copy-network"
  (setf (nodes destination) (copy-list (nodes source)))
  (setf (edges destination) (copy-list (edges source))))

(defgeneric deep-copy-network (network &key check-consistency)
  (:documentation "Makes a real deep copy of the network."))

(defmethod deep-copy-network ((network network) &key (check-consistency t))
  (let* ((node-mapping (make-hash-table)) 
	 ;; The above is needed to make (nodes ...) of edges point to the new ones.
	 (copy (copy-object network)))
    (setf (nodes copy) (loop for node in (nodes network)
			  for node-copy = (copy-object node)
			  do (setf (gethash node node-mapping) node-copy)
			  collect node-copy))
    (setf (edges copy)
	  (loop for edge in (edges network)
	     for edge-copy = (copy-object edge)
	     do (setf (start edge-copy) (gethash (start edge) node-mapping))
	       (setf (end edge-copy) (gethash (end edge) node-mapping))
	     collect edge-copy))
    ;; At this point the edges will be fine and point to the nodes
    ;; but the nodes themselves will still have the old edges. This
    ;; is solved by complete-edges. This could be optimised because
    ;; complete-edges does a bit more then necessary for this case.
    (complete-edges copy :check-consistency check-consistency) ;; This will fill the (edges ...) slots of the nodes
    copy))


;; This defgeneric is already defined in tree.lisp
;; (defgeneric add-node (node network &key &allow-other-keys)
;;   (:documentation "Adds the node to the network. Optionally also
;;   includes the edges to immediatly connect the node. The input can be
;;   checked for consistency if wanted."))

(defmethod add-node ((network network) (node net-node)
		     &key (check-consistency nil) edges)
  (push node (nodes network))
  (loop for edge in edges
     do (push edge (edges network)))
  (when edges
    (complete-edges network))
  (when check-consistency
    (check-network-consistency network))
  network)

(defgeneric add-edge (network edge &key check-consistency start-node end-node)
  (:documentation "Adds the edge to the network. It will also check
  the start- and end-node of this edge and make sure their edges-slot
  is correct. Optionally you can supply a start-node and end-node if
  they are not yet in the network."))

(defmethod add-edge ((network network) (edge net-edge) &key (check-consistency nil) start-node end-node)
  (when start-node
    (push start-node (nodes network))
    (setf (start edge) start-node))
  (when end-node
    (push end-node (nodes network))
    (setf (end edge) end-node))
  (push edge (edges network))
  ;; At this point it is still possible that the start- and end-nodes
  ;; have inconsistent edges-slots.
  (complete-edges network :check-consistency check-consistency)
  network)

(defgeneric create-and-add-edge (network label start end &key node-key node-test &allow-other-keys)
  (:documentation "Creates and adds and edge with the given label to
  the network. You have to supply a start and end node and it is
  assumed these already exist in the network. You can also supply a
  node-key and node-test that will be used to find the start and end
  node in the network."))

(defmethod create-and-add-edge ((network network) (label symbol) (start t) (end t) 
				&key (node-key #'identity) (node-test #'eq) &allow-other-keys)
  (let ((start (find start (nodes network) :key node-key :test node-test))
	(end (find end (nodes network) :key node-key :test node-test)))
    (if (and start end)
	(setf network (add-edge network (make-instance 'net-edge
						       :label label
						       :start start
						       :end end)))
	(progn (warn "create-and-add-edge: start (~a) or end (~a) not found"
		     start end)
	       network))))

(defgeneric delete-node (network node &key check-consistency key test)
  (:documentation "Deletes the given node from the network. Will also
  delete all edges connected to this node."))

(defmethod delete-node ((network network) (node t)
			&key (check-consistency t) (key #'identity) (test #'eql))
  (setf (nodes network) (delete node (nodes network) :key key :test test))
  (loop for edge in (edges network)
     when (or (funcall test node (funcall key (start edge)))
	      (funcall test node (funcall key (end edge))))
     do ;; I don't call delete-edge because it would call
	;; complete-edges every time. Also do not pass the key and
	;; test here!
       (setf (edges network) (delete edge (edges network))))
  (complete-edges network :check-consistency check-consistency))

(defgeneric delete-edge (network edge &key &allow-other-keys)
  (:documentation "Deletes the given edge from the network. Makes sure
  the network is consistent again."))

(defmethod delete-edge ((network network) (edge t) &key (check-consistency t) (key #'identity) (test #'eql))
  (setf (edges network) (delete edge (edges network) :key key :test test))
  ;; Just calling complete-edges will clean-up the residue
  ;; edge-information in the nodes.
  (complete-edges network :check-consistency check-consistency))

(defgeneric neighbours (node &key label direction &allow-other-keys)
  (:documentation "Returns all direct neighbours from the given node
  in (node edge) pairs. Optionally you can limit the result by giving
  a label for the edges and/or giving a direction (:in, :out
  or :both) (default = :both) to limit only on incoming or outgoing
  edges."))

(defmethod neighbours ((node net-node) &key label (direction :both))
  (loop for edge in (node-edges node)
     if (and (find direction '(:out :both)) 
             (eq node (start edge))
	     (or (not label)
		 (eq label (label edge))))
     collect (list (end edge) edge)
     if (and (find direction '(:in :both))
             (eq node (end edge))
             (or (not label)
                 (eq label (label edge))))
     collect (list (start edge) edge)))

(defgeneric all-neighbours (node &key label depth direction visited-nodes)
  (:documentation "Recursively gets all neighbours from the given node
  until the given depth (default is infinite depth).  Optionally you
  can limit the result by giving a label for the edges and/or giving a
  direction (:in, :out or :both) (default = :both) to limit only on
  incoming or outgoing edges. Has cycle detection to avoid infinite
  loops."))

(defmethod all-neighbours ((node net-node) &key label (direction :both) (depth -1) visited-nodes)
  (unless (= depth 0)
    (let ((new-neighbours (set-difference 
                           (mapcar #'first (neighbours node :label label :direction direction)) 
                           visited-nodes)))
      (append new-neighbours
              (loop for node in new-neighbours
                 append (all-neighbours node :label label :depth (1- depth) :direction direction
                                        :visited-nodes (append visited-nodes new-neighbours)))))))

(defgeneric neighbours-out (node &key label)
  (:documentation "Returns all direct outgoing neighbours from the
  given node in (node edge) pairs. Optionally you can limit the result
  by giving a label for the edges."))

(defmethod neighbours-out ((node net-node) &key label)
  (neighbours node :label label :direction :out))

(defgeneric neighbours-in (node &key label)
  (:documentation "Returns all direct outgoing neighbours from the
  given node in (node edge) pairs. Optionally you can limit the result
  by giving a label for the edges."))

(defmethod neighbours-in ((node net-node) &key label)
  (neighbours node :label label :direction :in))

(defmethod get-sub-network ((node net-node) &key (sub-net (make-instance 'network)))
  (add-node sub-net node)
  (loop for (neighbour edge) in (neighbours node)
     unless (find neighbour (nodes sub-net))
     do (add-edge sub-net edge)
       (setf sub-net (get-sub-network neighbour :sub-net sub-net))
     finally (return sub-net)))
