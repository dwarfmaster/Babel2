(in-package :network)

(deftest test-complete-edges ()
  (let* ((node-1 (make-instance 'net-node))
	 (node-2 (make-instance 'net-node))
	 (edge (make-instance 'net-edge
				    :edge-label 'A->B
				    :edge-start node-1
				    :edge-end node-2))
	 (A+B-net (complete-edges
		   (make-instance 'network
				  :network-nodes (list node-1 node-2)
				  :network-edges (list edge)))))
    (test-assert (and (find edge (node-edges node-1))
		      (find edge (node-edges node-2))))
    (push (make-instance 'net-edge :edge-label 'bad) (node-edges node-1))
    (setf A+B-net (complete-edges A+B-net))
    (test-assert (and (= (length (node-edges node-1)) 1)
		      (find edge (node-edges node-1))))))

;; (test-complete-edges)

