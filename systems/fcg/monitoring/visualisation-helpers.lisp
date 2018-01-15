(in-package :fcg)

(export '(add-final-transient-structure-to-wi
          add-cxn-to-wi
          final-transient-structure
          get-meaning-network))

(defun add-final-transient-structure-to-wi (function input  &optional (cxn-inventory *fcg-constructions*))
  "Adds the final transient structure to the web interface.
   Use: (add-final-transient-structure-to-wi 'comprehend \"the cat\")"
  (add-element (make-html-fcg-light (final-transient-structure function input cxn-inventory)
                                    :feature-types (feature-types cxn-inventory)
                                    :configuration (visualization-configuration cxn-inventory))))

(defun final-transient-structure (function input &optional (cxn-inventory *fcg-constructions*))
  "Returns the final transient structure.
   Use: (final-transient-structure 'comprehend \"the cat\")"
  (multiple-value-bind (solution cipn)
      (with-disabled-monitor-notifications
        (funcall function input :cxn-inventory cxn-inventory))
    (declare (ignore solution))
    (car-resulting-cfs (cipn-car cipn))))

(defun get-meaning-network (utterance &optional (cxn-inventory *fcg-constructions*))
  "Comprehends utterance using cxn-inventory, writes the meaning network to a pdf file, and opens it."
  (let ((meaning (comprehend utterance :cxn-inventory cxn-inventory :silent t)))
    (s-dot->image (wi::predicate-network->s-dot meaning) :format "pdf" :open t)))

(defun add-cxn-to-wi (cxn &optional (cxn-inventory *fcg-constructions*))
  "Adds cxn to the web interface."
  (add-element (make-html (find-cxn cxn cxn-inventory :key 'name :test 'string=))))

(defun get-constructional-dependencies (utterance-or-meaning &key (cxn-inventory *fcg-constructions*)
                                                             (labeled-paths 'no-bindings)
                                                             (colored-paths nil)
                                                             (trace-units nil))
  "Comprehends utterance using cxn-inventory, writes the meaning network to a pdf file, and opens it."
  (set-configuration (visualization-configuration cxn-inventory)
                   :labeled-paths labeled-paths)
  (set-configuration (visualization-configuration cxn-inventory)
                   :colored-paths colored-paths)
  (set-configuration (visualization-configuration cxn-inventory)
                   :trace-units trace-units)
  (let ((direction (cond ((stringp utterance-or-meaning)
                          '<-)
                         ((not (listp utterance-or-meaning))
                          (error "The input to get-constructional-dependencies should be an utterance or a meaning"))
                         ((stringp (first utterance-or-meaning))
                          '<-)
                         ((listp (first utterance-or-meaning))
                          '->))))
    (if (eq direction '<-)
      (multiple-value-bind (meaning cip-node cip)
          (comprehend utterance-or-meaning :cxn-inventory cxn-inventory :silent t)
        (declare (ignore meaning cip))
        (let* ((gp-data (analyse-solution cip-node '<-))
               (s-dot (unit-bindings->graph :data gp-data
                                            :prefered-font "Arial"
                                            :visualization-configuration (visualization-configuration cxn-inventory)))
               (path (monitors::make-file-name-with-time-and-experiment-class
                      (merge-pathnames (babel-pathname :directory '(".tmp"))
                                       (make-pathname :name (string-append "comprehend-"
                                                                           (substitute #\- #\SPACE utterance-or-meaning))
                                                      :type "pdf"))
                      (mkstr (make-id 'graph)))))            
          (s-dot->image s-dot :path path
                        :format "pdf" :open t)))
      (multiple-value-bind (utterance cip-node cip)
          (formulate utterance-or-meaning :cxn-inventory cxn-inventory :silent t)
        (declare (ignore utterance cip))
        (let* ((gp-data (analyse-solution cip-node '->))
               (s-dot (unit-bindings->graph :data gp-data
                                             :prefered-font "Arial"
                                             :construction-inventory cxn-inventory))
               (path (monitors::make-file-name-with-time-and-experiment-class
                      (merge-pathnames (babel-pathname :directory '(".tmp"))
                                       (make-pathname :name "production"
                                                      :type "pdf"))
                      (mkstr (make-id 'graph)))))
               
          (s-dot->image s-dot :path path :format "pdf" :open t))))))
