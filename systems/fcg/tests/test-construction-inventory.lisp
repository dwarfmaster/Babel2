(in-package :fcg)

(deftest test-construction-set ()
  (let* ((cxn-1 (make-instance 'construction :name 'cxn-1))
	 (cxn-2 (make-instance 'construction :name 'cxn-2))
	 (cxn-3 (make-instance 'construction :name 'cxn-3))
         (cxn-with-same-name-as-1 (make-instance 'construction :name 'cxn-1))
         (cxn-for-trash-1 (make-instance 'construction :name 'cxn-6))
         (cxn-for-trash-2 (make-instance 'construction :name 'cxn-7))       
         (cxn-with-same-name-as-6 (make-instance 'construction :name 'cxn-6))
	 (cxn-set (make-instance 'construction-set))
	 copied-cxn-set)
    ;; First we test trivial initialization
    (add-cxn cxn-1 cxn-set)
    (add-cxn cxn-2 cxn-set)
    (test-assert (and (configuration cxn-set)
		      (= (length (constructions cxn-set)) 2)))
    (setf copied-cxn-set (copy-object cxn-set))
    (test-assert (= (length (constructions copied-cxn-set)) 2))
    ;; Slightly less trivial tests
    (add-cxn cxn-3 cxn-set)
    (test-assert (and (= (length (constructions cxn-set)) 3)
		      (= (length (constructions copied-cxn-set)) 2)))
    (add-cxn cxn-with-same-name-as-1 cxn-set) ;; length should not increase
    (test-assert (and (= (length (constructions cxn-set)) 3)
		      (find-cxn cxn-with-same-name-as-1 cxn-set)))
    (delete-cxn 'cxn-5 cxn-set :key #'name)
    (delete-cxn cxn-2 cxn-set)
    (test-assert (= (length (constructions cxn-set)) 2))
    (test-assert (and (find-cxn cxn-3 cxn-set :test 'eq)
		      (find-cxn 'cxn-3 cxn-set)))
    (add-cxn (copy-object cxn-3) cxn-set :equivalent-key #'identity)
    (test-assert (= (length (find-all 'cxn-3 (constructions cxn-set) :key #'name)) 2))
    (clear copied-cxn-set)
    (test-assert (= 0 (length (constructions copied-cxn-set))))
    
    ;; Tests for trash
    (setf (trash cxn-set) (list cxn-for-trash-1))
    (setf (trash cxn-set) (append (trash cxn-set) (list cxn-for-trash-2)))
    (test-assert (= (length (trash cxn-set)) 2))
    (test-assert (find-cxn cxn-for-trash-2 cxn-set :search-trash t))
    (setf copied-cxn-set (copy-object cxn-set))
    (test-assert (= (length (trash copied-cxn-set)) 2))
    (add-cxn cxn-with-same-name-as-6 cxn-set :recover-from-trash t)
    (test-assert (= (length (trash cxn-set)) 1))
    (test-assert (find-cxn cxn-with-same-name-as-6 cxn-set))
    (delete-cxn cxn-1 cxn-set :move-to-trash t)
    (test-assert (= (length (trash cxn-set)) 1))
    (test-assert (not (find cxn-1 (constructions cxn-set))))
    (clear copied-cxn-set :include-trash t)
    (test-assert (= (length (trash copied-cxn-set)) 0))
    (test-assert (= (length (constructions copied-cxn-set)) 0))))

;; (test-construction-set)

(deftest test-construction-network ()
  (let* ((construction-1 (make-instance 'construction :name 'construction-1))
	 (construction-2 (make-instance 'construction :name 'construction-2))
	 (construction-3 (make-instance 'construction :name 'construction-3))
	 (node (make-instance 'net-node))
	 (construction-network-1 (make-instance 'construction-network))
	 (construction-network-2 (make-instance 'construction-network))
	 shallow-copied-construction-network deep-copied-construction-network)

    ;; First we initialize the networks
    (add-cxn construction-1 construction-network-1)
    (add-cxn construction-2 construction-network-1)

    (add-cxn construction-1 construction-network-2)
    (add-cxn construction-2 construction-network-2)
    (add-edge construction-network-2 
	      (make-instance 'net-edge 
			     :label 'primes
			     :score 0.8 
			     :start (find-cxn-node construction-1 construction-network-2)
			     :end (find-cxn-node construction-2 construction-network-2)))

    (setf shallow-copied-construction-network (copy-object construction-network-2))
    (setf deep-copied-construction-network (deep-copy-network construction-network-2))

    ;; Now we can start testing
    (test-assert (and (= (length (constructions construction-network-1)) 2)
		      (= (length (nodes construction-network-1)) 2)
		      (null (edges construction-network-1))
		      (= (length (edges construction-network-2)) 1)))
    (complete-edges construction-network-2 :check-consistency t)
    (test-assert (find 'primes (node-edges (first (nodes construction-network-2))) :key #'label))
    (test-assert (and (loop for construction in (constructions construction-network-1)
                            for construction-1 in (constructions shallow-copied-construction-network)
                            always (eq construction construction-1))
		      (loop for construction in (constructions construction-network-1)
                            for construction-1 in (constructions deep-copied-construction-network)
                            never (eq construction construction-1))))

    (add-node deep-copied-construction-network node :check-consistency t 
	      :edges (list (make-instance 'net-edge :label 'bla 
					  :start (second (nodes deep-copied-construction-network))
					  :end node)))
    (test-assert (and (= (length (constructions deep-copied-construction-network)) 2)
		      (= (length (nodes deep-copied-construction-network)) 3)
		      (= (length (edges deep-copied-construction-network)) 2)
		      (= (length (nodes construction-network-2)) 2)
		      (= (length (edges construction-network-2)) 1)))

    (delete-cxn 'construction-5 construction-network-2 :key #'name)
    (delete-cxn construction-1 construction-network-2) ;;should delete the primes edge
    (test-assert (and (= (length (constructions construction-network-2)) 1)
		      (= (length (edges construction-network-2)) 0)))

    (add-cxn construction-1 construction-network-2)
    (add-cxn construction-3 construction-network-2)
    (create-and-add-edge construction-network-2 'primes
			 (find-cxn-node construction-2 construction-network-2) 
			 (find-cxn-node construction-3 construction-network-2))
    (create-and-add-edge construction-network-2 'inhibits 
			 (find-cxn-node construction-3 construction-network-2) 
			 (find-cxn-node construction-1 construction-network-2))
    (test-assert (and (= (length (constructions construction-network-2)) 3)
		      (= (length (edges construction-network-2)) 2)
		      (= (length (neighbours (find-cxn-node construction-3 construction-network-2))) 2)
		      (eq (construction (first (first (neighbours (find-cxn-node construction-3 
										 construction-network-2) 
								  :label 'primes)))) construction-2)
		      (null (neighbours-out (find-cxn-node construction-3 construction-network-2) :label 'primes))
		      (neighbours-in (find-cxn-node construction-3 construction-network-2) :label 'primes)))
    (test-error 
     (add-cxn construction-1  construction-network-1 ;; construction-3 is not part of net
	      :edges (list (make-instance 'net-edge 
					  :start (find-cxn-node construction-3 construction-network-1) 
					  :end (find-cxn-node construction-1 construction-network-1)))
	      :check-consistency t))
    
    (test-assert (and (find-cxn construction-2 construction-network-2)
		      (find-cxn 'construction-1 construction-network-2 :key #'name)
		      (null (find-cxn 'construction-6 construction-network-2 :key #'name))))

    (clear deep-copied-construction-network)
    (test-assert (and (= (length (constructions deep-copied-construction-network)) 0)
		      (= (length (nodes deep-copied-construction-network)) 1)
		      (= (length (edges deep-copied-construction-network)) 0)))))

;; (test-construction-network)

(deftest test-construction-inventory-collection ()
  (let* ((construction-1 (make-instance 'construction :name 'construction-1))
	 (construction-2 (make-instance 'construction :name 'construction-2))
	 (construction-3 (make-instance 'construction :name 'construction-3))
	 (construction-5 (make-instance 'construction :name 'construction-5))
	 (construction-set-1 (make-instance 'construction-set))
	 (construction-set-2 (make-instance 'construction-set))
	 (construction-network (make-instance 'construction-network))
	 (rcc (make-instance 'construction-inventory-collection)))

    ;; Testing whether the functions size and constructions work properly
    (test-assert (size rcc))

    ;; Initialization

    (add-cxn construction-1 construction-set-1 :copy-construction nil)
    (add-cxn construction-2 construction-set-1 :copy-construction nil)
    
    (add-cxn construction-3 construction-set-2 :copy-construction nil)

    (add-cxn construction-2 construction-network :copy-construction nil)    
    
    ;; add-cxn-inventory
    (test-ok (add-construction-inventory rcc 'lex construction-set-1))
    (test-ok (add-construction-inventory rcc 'con construction-set-2))
    (test-ok (add-construction-inventory rcc 'net construction-network))
    
    (test-assert (= (length (constructions rcc)) 4))
    (test-assert (= (length (constructions rcc :label 'lex)) 2))
    (test-assert (= (length (constructions rcc :label 'net)) 1))

    ;; find-construction-inventory
    (test-assert (find-construction-inventory rcc 'lex))
    (test-assert (not (find-construction-inventory rcc 'foo)))

    ;; delete-construction-inventory
    (delete-construction-inventory rcc 'net)
    (test-assert (not (find-construction-inventory rcc 'net)))
    (test-ok (delete-construction-inventory rcc 'foo))
    (add-construction-inventory rcc 'net construction-network)

    ;; delete-cxn
    (delete-cxn construction-1 rcc :label 'con)
    (test-assert (and (= (length (constructions rcc :label 'lex)) 2)
		      (= (length (constructions rcc :label 'con)) 1)))
    (delete-cxn construction-1 rcc :label 'lex)
    (test-assert (= (length (constructions rcc :label 'lex)) 1))
    (delete-cxn 'construction-2 rcc :key #'name)
    (test-assert (and (= (length (constructions rcc :label 'lex)) 0)
		      (= (length (constructions rcc :label 'net)) 0)))

    ;; add-cxn
    (add-cxn  construction-1 rcc :copy-construction nil)
    (test-assert (and (= (length (constructions rcc :label 'lex)) 1)
		      (= (length (constructions rcc :label 'net)) 1)))
    (add-cxn construction-1 rcc :label 'lex) ;; won't add it (equivalent)
    (test-assert (= (length (constructions rcc :label 'lex)) 1))
    (add-cxn construction-5 rcc :label 'net) ;; will add a copy
    (test-assert (= (length (constructions rcc :label 'net)) 2))
    (add-cxn construction-2 rcc)
    (add-cxn construction-5 rcc)
    (test-assert (and (= (length (constructions rcc :label 'lex)) 3)
		      (= (length (constructions rcc :label 'con)) 4)
		      (= (length (constructions rcc :label 'net)) 3)))

    ;; find-cxn
    (test-assert (find-cxn 'construction-1 rcc :label 'lex :key #'name))
    (test-assert (not (find-cxn 'construction-4 rcc :label 'lex :key #'name)))
    (test-assert (find-cxn construction-1 rcc))

    ;;clear
    (clear rcc :label 'foo)
    (test-assert (and (= (length (constructions rcc :label 'lex)) 3)
		      (= (length (constructions rcc :label 'con)) 4)
		      (= (length (constructions rcc :label 'net)) 3)))
    (clear rcc :label 'con)
    (test-assert (and (= (length (constructions rcc :label 'lex)) 3)
		      (= (length (constructions rcc :label 'con)) 0)
		      (= (length (constructions rcc :label 'net)) 3)))
    (clear rcc)
    (test-assert (and (= (length (constructions rcc :label 'lex)) 0)
		      (= (length (constructions rcc :label 'con)) 0)
		      (= (length (constructions rcc :label 'net)) 0)))
    
      
    ;; this will overwrite all constructions
    (add-cxn construction-1 rcc)
    (add-cxn construction-2 rcc)
    (add-cxn construction-3 rcc)
    (test-assert (loop for (nil . construction-inventory) in (data-fields (construction-inventories rcc))
                       always (= (length (constructions construction-inventory)) 3)))))

;;(test-construction-inventory-collection)

(deftest test-hashed-construction-set ()
  (let* ((cxn-1 (make-instance 'construction :name 'cxn-1))
         (cxn-2 (make-instance 'construction :name 'cxn-2))
         (cxn-3 (make-instance 'construction :name 'cxn-3))
         (cxn-with-same-name-as-1 (make-instance 'construction :name 'cxn-1))
         (cxn-for-trash-1 (make-instance 'construction :name 'cxn-6))
         (cxn-for-trash-2 (make-instance 'construction :name 'cxn-7))       
         (cxn-with-same-name-as-6 (make-instance 'construction :name 'cxn-6))
         (cxn-set (make-instance 'hashed-construction-set))
         copied-cxn-set)
    ;; # ########################################################
    ;; first test set functionality
    ;; First we test trivial initialization
    (add-cxn cxn-1 cxn-set)
    (add-cxn cxn-2 cxn-set)
    (test-assert (and (configuration cxn-set)
                      (= (length (constructions cxn-set)) 2)))
    (setf copied-cxn-set (copy-object cxn-set))
    (test-assert (= (length (constructions copied-cxn-set)) 2))
    ;; Slightly less trivial tests
    (add-cxn cxn-3 cxn-set)
    (test-assert (and (= (length (constructions cxn-set)) 3)
                      (= (length (constructions copied-cxn-set)) 2)))
    (add-cxn cxn-with-same-name-as-1 cxn-set) ;; length should not increase
    (test-assert (and (= (length (constructions cxn-set)) 3)
                      (find-cxn cxn-with-same-name-as-1 cxn-set)))
    (delete-cxn 'cxn-5 cxn-set :key #'name)
    (delete-cxn cxn-2 cxn-set)
    (test-assert (= (length (constructions cxn-set)) 2))
    (test-assert (and (find-cxn cxn-3 cxn-set :test 'eq)
                      (find-cxn 'cxn-3 cxn-set)))
    (add-cxn (copy-object cxn-3) cxn-set :equivalent-key #'identity)
    (test-assert (= (length (find-all 'cxn-3 (constructions cxn-set) :key #'name)) 2))
    (clear copied-cxn-set)
    (test-assert (= 0 (length (constructions copied-cxn-set))))
     
    ;; Tests for trash
    (setf (trash cxn-set) (list cxn-for-trash-1))
    (setf (trash cxn-set) (append (trash cxn-set) (list cxn-for-trash-2)))
    (test-assert (= (length (trash cxn-set)) 2))
    (test-assert (find-cxn cxn-for-trash-2 cxn-set :search-trash t))
    (setf copied-cxn-set (copy-object cxn-set))
    (test-assert (= (length (trash copied-cxn-set)) 2))
    (add-cxn cxn-with-same-name-as-6 cxn-set :recover-from-trash t)
    (test-assert (= (length (trash cxn-set)) 1))
    (test-assert (find-cxn cxn-with-same-name-as-6 cxn-set))
    (delete-cxn cxn-1 cxn-set :move-to-trash t)
    (test-assert (= (length (trash cxn-set)) 1))
    (test-assert (not (find cxn-1 (constructions cxn-set))))
    (clear copied-cxn-set :include-trash t)
    (test-assert (= (length (trash copied-cxn-set)) 0))
    (test-assert (= (length (constructions copied-cxn-set)) 0)))
  
  ;;;; ########################################################
  ;;;; test hash functionality
  
  ;; hash :hash-words-entities+top
  ;; test hash construction
  (let* ((construction (make-instance
                        'construction
                        :name 'cxn-1
                        :attributes '((:string . "string")
                                      (:meaning . test-entity))
                        :left-pole '((root
                                      (meaning ((bind test-type ?test test-entity)))))
                        :right-pole '((root
                                       (form ((string ?string "string")))
                                       (footprints (==0 cxn-1))))))
         (hs (hash construction :hash-word-entity-root)))

    (test-equal 2 (length hs))
    (test-equal (first hs) "string")
    (test-equal (second hs) 'test-entity))
  
  ;; test hash cip-node production (irl meaning)
  (let* ((cip (make-instance
               'construction-inventory-processor
               :construction-inventory (make-instance 'construction-set)
               :direction '->
               :initial-cfs (create-initial-structure '((bind test-type ?test test-entity))
                                                      :root-mode)))
         (hs (hash (top-node cip) :hash-word-entity-root)))
    (test-equal 1 (length hs))
    (test-equal (first hs) 'test-entity))
  ;; test hash cip-node production (non irl-meaning)
  (let* ((cip (make-instance
               'construction-inventory-processor
               :construction-inventory (make-instance 'construction-set)
               :direction '->
               :initial-cfs (create-initial-structure '((test ?test-1 ?test-2 tast))
                                                      :root-mode)))
         (hs (hash (top-node cip) :hash-word-entity-root)))
    (test-equal 1 (length hs))
    (test-equal (first hs) 'test))
  ;; test hash cip-node production (2 meanings)
  (let* ((cip (make-instance
               'construction-inventory-processor
               :construction-inventory (make-instance 'construction-set)
               :direction '->
               :initial-cfs (create-initial-structure '((bind test-type ?test test-entity)
                                                        (bind test-type2 ?test test-entity2))
                                                      :root-mode)))
         (hs (hash (top-node cip) :hash-word-entity-root)))
    (test-equal 2 (length hs))
    (test-assert (member 'test-entity
                         hs :test #'equalp))
    (test-assert (member 'test-entity2
                         hs :test #'equalp)))
  ;; test hash cip-node production (2 meanings, non-irl)
  (let* ((cip (make-instance
               'construction-inventory-processor
               :construction-inventory (make-instance 'construction-set)
               :direction '->
               :initial-cfs (create-initial-structure '((test1 ?test-1 ?test-2 tast-1)
                                                        (test2 ?test-3 ?test-2 tast-2))
                                                      :root-mode)))
         (hs (hash (top-node cip) :hash-word-entity-root)))
    (test-equal 2 (length hs))
    (test-assert (member 'test1
                         hs :test #'equalp))
    (test-assert (member 'test2
                         hs :test #'equalp)))
  ;; test hash cip-node parsing
  (let* ((cip (make-instance
               'construction-inventory-processor
               :construction-inventory (make-instance 'construction-set)
               :direction '<-
               :initial-cfs (handler-bind ((warning #'(lambda (c)
                                                        (declare (ignore c))
                                                        (muffle-warning))))
                              (de-render '("string") :de-render-in-root-mode))))
         (hs (hash (top-node cip) :hash-word-entity-root)))
    (test-equal 1 (length hs))
    (test-equal (first hs) "string"))
  ;; test hash cip-node parsing (2 strings)
  (let* ((cip (make-instance
               'construction-inventory-processor
               :construction-inventory (make-instance 'construction-set)
               :direction '<-
               :initial-cfs (handler-bind ((warning #'(lambda (c)
                                                        (declare (ignore c))
                                                        (muffle-warning))))
                              (de-render '("string" "king") :de-render-in-root-mode))))
         (hs (hash (top-node cip) :hash-word-entity-root)))
    (test-equal 2 (length hs))
    (test-assert (member "string" hs :test #'equalp))
    (test-assert (member "king" hs :test #'equalp)))
    
  ;; test add/delete/clear with hashes
  (let ((constructions
         (loop
          for i from 1 to 10
          for string = (format nil "test-~d" i)
          for name = (make-symbol string)
          collect (make-instance
                   'construction
                   :name name
                   :attributes `((:string . ,string)
                                 (:meaning . ,name))
                   :left-pole `((root
                                 (meaning ((bind test-type ?test ,name)))))
                   :right-pole `((root
                                  (form ((string ?string ,string)))
                                  (footprints (==0 cxn-1)))))))
        (cxn-hash-table (make-instance 'hashed-construction-set)))
    ;; add-cxn
    (loop for cxn in constructions
          do (add-cxn cxn cxn-hash-table))
    (test-equal 10 (length (constructions cxn-hash-table)))
    (test-equal 20 (hash-table-count (constructions-hash-table
                                      cxn-hash-table)))
    (test-equal 10 (length
                    (remove-duplicates
                     (loop for value being the hash-values of
                           (constructions-hash-table cxn-hash-table)
                           append value))))
    ;; delete-cxn
    (delete-cxn (first constructions) cxn-hash-table)
    (test-equal 9 (length (constructions cxn-hash-table)))
    (test-equal 9 (length
                   (remove-duplicates
                    (loop for value being the hash-values of
                          (constructions-hash-table cxn-hash-table)
                          append value))))
    ;; clear
    (clear cxn-hash-table)
    (test-equal 0 (length (constructions cxn-hash-table)))
    (test-equal 0 (length
                   (remove-duplicates
                    (loop for value being the hash-values of
                          (constructions-hash-table cxn-hash-table)
                          append value)))))
    
  ;; test cxn-supplier-with-hashed-simple-queue
  (let* ((entities (loop for i from 1 to 10
                         for string = (format nil "test-~d" i)
                         collect (make-symbol string)))
         (constructions
          (loop
           for i from 1 to 10
           for entity in entities
           for string = (format nil "test-~d" i)
           collect (make-instance
                    'construction
                    :name entity
                    :attributes `((:string . ,string)
                                  (:meaning . ,entity))
                    :left-pole `((root
                                  (TAG ?meaning
                                       (meaning (== (bind test-type ?test ,entity))))
                                  (footprints nil))
                                 ((J ?new-unit)
                                  ?meaning
                                  (footprints ,entity)))
                    :right-pole `((root
                                   (TAG ?form
                                        (form (== (string ?string ,string))))
                                   (footprints nil))
                                  ((J ?new-unit)
                                   ?form
                                   (footprints ,entity))))))
         (cxn-hash-table (make-instance 'hashed-construction-set)))
    ;; add all constructions
    (loop for cxn in constructions
          do (add-cxn cxn cxn-hash-table))
    ;; test parsing simple
    (let ((meaning
           (handler-bind ((warning #'(lambda (c)
                                       (declare (ignore c))
                                       (muffle-warning))))
             (parse '("test-2") cxn-hash-table))))
      (test-equal (length meaning) 1))
    ;; test parsing more complex 
    (let ((meaning
           (handler-bind ((warning #'(lambda (c)
                                       (declare (ignore c))
                                       (muffle-warning))))
             (parse '("test-2" "test-3" "test-3") cxn-hash-table))))
      (test-equal (length meaning) 3)
      (test-equal (length (remove-duplicates
                           (mapcar #'fourth meaning))) 2))
    ;; test production simple
    (let ((strings  
           (produce `((bind test-type ?test ,(first entities))) cxn-hash-table)))
      (test-equal 1 (length strings)))
    ;; test production more complex
    (let ((strings  
           (produce `((bind test-type ?test ,(first entities))
                      (bind test-type ?test ,(second entities))
                      (bind test-type ?test ,(second entities)))
                    cxn-hash-table)))
      (test-equal 3 (length strings))
      (test-equal 2 (length (remove-duplicates strings :test #'equalp))))

    ;; test nil bucket
    (let ((nil-hash-cxn (make-instance
                         'construction
                         :name 'other-cxn               
                         :left-pole `((root
                                       (TAG ?meaning
                                            (meaning (== (bind test-type ?test other))))
                                       (footprints nil))
                                      ((J ?new-unit)
                                       ?meaning
                                       (footprints other)))
                         :right-pole `((root
                                        (TAG ?form
                                             (form (== (string ?string "other"))))
                                        (footprints nil))
                                       ((J ?new-unit)
                                        ?form
                                        (footprints other))))))
      (test-assert (null (gethash nil (constructions-hash-table cxn-hash-table))))
      (test-equal nil (hash nil-hash-cxn :hash-word-entity-root))
      (add-cxn nil-hash-cxn cxn-hash-table)
      (test-equal 1 (length (gethash nil (constructions-hash-table cxn-hash-table)))) 
      ;; test parsing simple
      (let ((meaning
             (handler-bind ((warning #'(lambda (c)
                                         (declare (ignore c))
                                         (muffle-warning))))
               (parse '("other") cxn-hash-table))))
        (test-equal (length meaning) 1)) 
      ;; test parsing more complex 
      (let ((meaning
             (handler-bind ((warning #'(lambda (c)
                                         (declare (ignore c))
                                         (muffle-warning))))
               (parse '("other" "other" "test-3") cxn-hash-table))))
        (test-equal (length meaning) 3)
        (test-equal (length (remove-duplicates (mapcar #'fourth meaning))) 2)) 
      ;; test production simple
      (let ((strings  
             (produce `((bind test-type ?test other)) cxn-hash-table)))
        (test-equal 1 (length strings))) 
      ;; test production more complex
      (let ((strings  
             (produce `((bind test-type ?test ,(first entities))
                        (bind test-type ?test ,(second entities))
                        (bind test-type ?test ,(second entities))
                        (bind test-type ?test other))
                      cxn-hash-table)))
        (test-equal 4 (length strings))
        (test-equal 3 (length (remove-duplicates strings :test #'equalp))))))

  ;; test cxn-supplier-with-hashed-simple-queue (non-irl meanings)
  (let* ((predicates (loop for i from 1 to 10
                           for string = (format nil "test-~d" i)
                           collect (make-symbol string)))
         (constructions
          (loop
           for i from 1 to 10
           for predicate in predicates
           for string = (format nil "test-~d" i)
           collect (make-instance
                    'construction
                    :name predicate
                    :attributes `((:string . ,string)
                                  (:meaning . ,predicate))
                    :left-pole `((root
                                  (TAG ?meaning
                                       (meaning (== (,predicate ?test-1 ?test-2))))
                                  (footprints nil))
                                 ((J ?new-unit)
                                  ?meaning
                                  (footprints ,predicate)))
                    :right-pole `((root
                                   (TAG ?form
                                        (form (== (string ?string ,string))))
                                   (footprints nil))
                                  ((J ?new-unit)
                                   ?form
                                   (footprints ,predicate))))))
         (cxn-hash-table (make-instance 'hashed-construction-set))) 
    ;; add all constructions
    (loop for cxn in constructions
          do (add-cxn cxn cxn-hash-table))
    ;; test parsing simple
    (let ((meaning
           (handler-bind ((warning #'(lambda (c)
                                       (declare (ignore c))
                                       (muffle-warning))))
             (parse '("test-2") cxn-hash-table))))
      (test-equal (length meaning) 1)) 
    ;; test parsing more complex 
    (let ((meaning
           (handler-bind ((warning #'(lambda (c)
                                       (declare (ignore c))
                                       (muffle-warning))))
             (parse '("test-2" "test-3" "test-3") cxn-hash-table))))
      (test-equal (length meaning) 3)
      (test-equal (length (remove-duplicates
                           (mapcar #'first meaning))) 
                  2)) 
    ;; test production simple
    (let ((strings  
           (produce `((,(first predicates) ?t1 ?t2)) cxn-hash-table)))
      (test-equal 1 (length strings))
      (test-equal (first strings) "test-1"))
    ;; test production more complex
    (let ((strings  
           (produce `((,(first predicates) ?test-1 ?test-2)
                      (,(second predicates) ?test-3 ?test-2)
                      (,(first predicates) ?test-1 ?test-2))
                    cxn-hash-table)))
      (test-equal 3 (length strings))
      (test-equal 2 (length (remove-duplicates strings :test #'equalp)))) 

    ;; test nil bucket
    (let ((nil-hash-cxn (make-instance
                         'construction
                         :name 'other-cxn               
                         :left-pole `((root
                                       (TAG ?meaning
                                            (meaning (== (other ?test-7 ?test-8))))
                                       (footprints nil))
                                      ((J ?new-unit)
                                       ?meaning
                                       (footprints other)))
                         :right-pole `((root
                                        (TAG ?form
                                             (form (== (string ?string "other"))))
                                        (footprints nil))
                                       ((J ?new-unit)
                                        ?form
                                        (footprints other))))))
      (test-assert (null (gethash nil (constructions-hash-table cxn-hash-table))))
      (test-equal nil (hash nil-hash-cxn :hash-word-entity-root))
      (add-cxn nil-hash-cxn cxn-hash-table)
      (test-equal 1 (length (gethash nil (constructions-hash-table cxn-hash-table)))) 
      ;; test parsing simple
      (let ((meaning
             (handler-bind ((warning #'(lambda (c)
                                         (declare (ignore c))
                                         (muffle-warning))))
               (parse '("other") cxn-hash-table))))
        (test-equal (length meaning) 1)
        (test-equal (first (first meaning)) 'other))
      ;; test parsing more complex 
      (let ((meaning
             (handler-bind ((warning #'(lambda (c)
                                         (declare (ignore c))
                                         (muffle-warning))))
               (parse '("other" "other" "test-3") cxn-hash-table))))
        (test-equal (length meaning) 3)
        (test-equal (length (remove-duplicates (mapcar #'first meaning))) 2))
      ;; test production simple
      (let ((strings  
             (produce `((other ?test-7 ?test-8)) cxn-hash-table)))
        (test-equal 1 (length strings))
        (test-equal (first strings) "other"))
      ;; test production more complex
      (let ((strings  
             (produce `((,(first predicates) ?t4 ?3)
                        (,(second predicates) ?t1 ?t2)
                        (,(second predicates) ?t5 ?t6)
                        (other ?t1 ?t2))
                      cxn-hash-table)))
        (test-equal 4 (length strings))
        (test-equal 3 (length (remove-duplicates strings :test #'equalp)))))))

;; (test-hashed-construction-set)

(deftest test-cxn-supplier-hashed-ordered-label ()
  ;; test cxn-supplier-with-hashed-simple-queue
  (let* ((entities (loop for i from 1 to 10
                         for string = (format nil "test-~d" i)
                         collect (make-symbol string)))
         (constructions
          (loop
           for i from 1 to 10
           for entity in entities
           for string = (format nil "test-~d" i)
           collect (make-instance
                    'construction
                    :name entity
                    :attributes `((:string . ,string)
                                  (:meaning . ,entity)
                                  (:label . ,i))
                    :left-pole `((root
                                  (TAG ?meaning
                                       (meaning (== (bind test-type ?test ,entity))))
                                  (footprints nil))
                                 ((J ?new-unit)
                                  ?meaning
                                  (footprints ,entity)))
                    :right-pole `((root
                                   (TAG ?form
                                        (form (== (string ?string ,string))))
                                   (footprints nil))
                                  ((J ?new-unit)
                                   ?form
                                   (footprints ,entity))))))
         (cxn-hash-table (make-instance 'hashed-construction-set))
         (cip-produce (make-instance
                       'construction-inventory-processor
                       :construction-inventory cxn-hash-table
                       :direction '->
                       :initial-cfs (create-initial-structure
                                     (loop for entity in entities
                                           collect `(bind test-type ?test ,entity))
                                     :root-mode)))
         (top-node-produce (top-node cip-produce))
         (cip-parse (make-instance
                     'construction-inventory-processor
                     :construction-inventory cxn-hash-table
                     :direction '<-
                     :initial-cfs (handler-bind ((warning #'(lambda (c)
                                                              (declare (ignore c))
                                                              (muffle-warning))))
                                    (de-render (loop for i from 1 to 10
                                                     collect (format nil "test-~d" i))
                                               :de-render-in-root-mode))))
         (top-node-parse (top-node cip-parse))) 

    ;; add all constructions
    (loop for cxn in constructions
          do (add-cxn cxn cxn-hash-table)) 
    ;; set configurations
    (set-configuration cxn-hash-table :cxn-supplier-mode :hashed-ordered-by-label)
    (set-configuration cxn-hash-table :production-order '(1 2 3 4 5 6 7 8 9 10))
    (set-configuration cxn-hash-table :parse-order '(1 2 3 4 5 6 7 8 9 10))
    (set-configuration cxn-hash-table 
                       :production-goal-tests '(:no-applicable-cxns 
                                                :no-meaning-in-root))
    (set-configuration cxn-hash-table 
                       :parse-goal-tests '(:no-applicable-cxns ))
    ;; test all-constructions-of-label-hashed production
    (loop for i from 1 to 10
          for cxn in constructions
          for cxns = (all-constructions-of-label-hashed top-node-produce i)
          do (test-equal 1 (length cxns))
          do (test-equal (first cxns) cxn))
    ;; test all-constructions-of-label-hashed parsing
    (loop for i from 1 to 10
          for cxn in constructions
          for cxns = (all-constructions-of-label-hashed top-node-parse i)
          do (test-equal 1 (length cxns))
          do (test-equal (first cxns) cxn)) 

    ;; test parsing simple
    (let ((meaning
           (handler-bind ((warning #'(lambda (c)
                                       (declare (ignore c))
                                       (muffle-warning))))
             (parse '("test-2") cxn-hash-table))) )
      (test-equal (length meaning) 1)
      (test-equal (second entities) (fourth (first meaning))))
    ;; test observation of labels (parsing fail)
    (set-configuration cxn-hash-table :parse-order '(1 3 4 5 6 7 8 9 10))
    (let ((meaning
           (handler-bind ((warning #'(lambda (c)
                                       (declare (ignore c))
                                       (muffle-warning))))
             (parse '("test-2") cxn-hash-table))))
      (test-equal (length meaning) 0))

    (set-configuration cxn-hash-table :production-order '(1 2 3 4 5 6 7 8 9 10))
    (set-configuration cxn-hash-table :parse-order '(1 2 3 4 5 6 7 8 9 10))

    ;; test parsing more complex 
    (let ((meaning
           (handler-bind ((warning #'(lambda (c)
                                       (declare (ignore c))
                                       (muffle-warning))))
             (parse '("test-2" "test-3" "test-3") cxn-hash-table))))
      (test-equal (length meaning) 3)
      (test-equal (length (remove-duplicates
                           (mapcar #'fourth meaning))) 2))
    ;; test production simple
    (let ((strings  
           (produce `((bind test-type ?test ,(first entities))) cxn-hash-table)))
      (test-equal 1 (length strings)))
    ;; test observation of labels (produce fail)
    (set-configuration cxn-hash-table :production-order '(2 3 4 5 6 7 8 9 10))
    (let ((meaning 
           (produce `((bind test-type ?test ,(first entities))) cxn-hash-table)))
      (test-equal (length meaning) 0))

    (set-configuration cxn-hash-table :production-order '(1 2 3 4 5 6 7 8 9 10))
    (set-configuration cxn-hash-table :parse-order '(1 2 3 4 5 6 7 8 9 10))

    ;; test production more complex
    (let ((strings  
           (produce `((bind test-type ?test ,(first entities))
                      (bind test-type ?test ,(second entities))
                      (bind test-type ?test ,(second entities)))
                    cxn-hash-table)))
      (test-equal 3 (length strings))
      (test-equal 2 (length (remove-duplicates strings :test #'equalp)))
      strings)

    ;; test nil hashed construction
    (let ((nil-hash-cxn (make-instance
                         'construction
                         :name 'other-cxn
                         :attributes '((:label . 0))
                         :left-pole `((root
                                       (TAG ?meaning
                                            (meaning (== (bind test-type ?test other))))
                                       (footprints nil))
                                      ((J ?new-unit)
                                       ?meaning
                                       (footprints other)))
                         :right-pole `((root
                                        (TAG ?form
                                             (form (== (string ?string "other"))))
                                        (footprints nil))
                                       ((J ?new-unit)
                                        ?form
                                        (footprints other))))))
      (test-assert (null (gethash nil (constructions-hash-table cxn-hash-table))))
      (test-equal nil (hash nil-hash-cxn :hash-word-entity-root))
      (add-cxn nil-hash-cxn cxn-hash-table)
      (test-equal 1 (length (gethash nil (constructions-hash-table cxn-hash-table))))
      ;; test parsing simple (fail)
      (let ((meaning
             (handler-bind ((warning #'(lambda (c)
                                         (declare (ignore c))
                                         (muffle-warning))))
               (parse '("other") cxn-hash-table))))
        (test-equal (length meaning) 0))
      ;; test parsing simple
      (set-configuration cxn-hash-table :parse-order '(1 2 3 4 5 6 7 8 9 10 0))
      (let ((meaning
             (handler-bind ((warning #'(lambda (c)
                                         (declare (ignore c))
                                         (muffle-warning))))
               (parse '("other") cxn-hash-table))))
        (test-equal (length meaning) 1)
        (test-equal 'other (fourth (first meaning))))
      ;; test parsing more complex
      (let ((meaning
             (handler-bind ((warning #'(lambda (c)
                                         (declare (ignore c))
                                         (muffle-warning))))
               (parse '("other" "other" "test-3") cxn-hash-table))))
        (test-equal (length meaning) 3)
        (test-equal (length (remove-duplicates (mapcar #'fourth meaning)))
                    2))
      ;; test production simple (fail)
      (let ((strings  
             (produce `((bind test-type ?test other)) cxn-hash-table)))
        (test-equal 0 (length strings)))
      ;; test production simple
      (set-configuration cxn-hash-table :production-order '(1 2 3 4 5 6 7 8 9 10 0))
      (let ((strings  
             (produce `((bind test-type ?test other)) cxn-hash-table)))
        (test-equal 1 (length strings)))
      ;; test production more complex
      (let ((strings  
             (produce `((bind test-type ?test ,(first entities))
                        (bind test-type ?test ,(second entities))
                        (bind test-type ?test ,(second entities))
                        (bind test-type ?test other))
                      cxn-hash-table)))
        (test-equal 4 (length strings))
        (test-equal 3 (length (remove-duplicates strings :test #'equalp)))))))

;; (test-cxn-supplier-hashed-ordered-label)
   

