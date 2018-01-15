(in-package :utils)

(deftest test-parent-configurations ()
  (let* ((parent-configuration
	  (make-configuration
	   :entries (list (cons 'key-a 'parent-value-a)
			  (cons 'key-b 'parent-value-b))))
	 (child-a-configuration
	  (make-configuration
	   :parent-configuration parent-configuration))
	 (child-b-configuration
	  (make-configuration
	   :parent-configuration parent-configuration)))
    (test-assert (eq (get-configuration parent-configuration 'key-a) 'parent-value-a))
    (test-assert (eq (get-configuration parent-configuration 'key-b) 'parent-value-b))
    (test-assert (null (get-configuration parent-configuration 'key-c)))
    ;; children inherit all defined values from parent
    (test-assert (eq (get-configuration child-a-configuration 'key-a) 'parent-value-a))
    (test-assert (eq (get-configuration child-a-configuration 'key-b) 'parent-value-b))
    (test-assert (null (get-configuration child-a-configuration 'key-c)))
    ;; setting a configuration at a lower level, only affects the child but not
    ;; its siblings, nor its parent
    (set-configuration child-a-configuration 'key-a 'child-value-a)
    (test-assert (eq (get-configuration parent-configuration 'key-a) 'parent-value-a))
    (test-assert (eq (get-configuration child-a-configuration 'key-a) 'child-value-a))
    (test-assert (eq (get-configuration child-b-configuration 'key-a) 'parent-value-a))
    ;; setting a configuration in a child does not add the configuration to its parent
    (set-configuration child-a-configuration 'key-c 'child-value-c)
    (test-assert (null (get-configuration parent-configuration 'key-c)))
    (test-assert (eq (get-configuration child-a-configuration 'key-c) 'child-value-c))
    (test-assert (null (get-configuration child-b-configuration 'key-c)))))
    
    
    