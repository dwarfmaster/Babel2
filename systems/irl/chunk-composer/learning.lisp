(in-package :irl)

;; ########################################
;; make-compositional-chunks
;; ---------------------------------------

(export '(make-compositional-chunks))

(defun make-compositional-chunks (chunk1 chunk2)
  (let* ((irl-program1 (irl-program chunk1))
         (irl-program2 (irl-program chunk2))
         (possible-compositions
          (loop for tuple in (find-intersection irl-program1
                                                irl-program2 :extension-test #'surjective-function-frame)
                for root-net = (intersection-tuple-intersection tuple)
                for leaf-net1 = (intersection-tuple-trash tuple)
                for leaf-net2 = (intersection-tuple-right-irl-network tuple)
                if (and root-net leaf-net1 leaf-net2)
                collect (list (ignore-errors (create-chunk-from-irl-program root-net))
                              (ignore-errors (create-chunk-from-irl-program leaf-net1))
                              (ignore-errors (create-chunk-from-irl-program leaf-net2))))))
    (loop for composition in possible-compositions
          if (and
              (first composition) (second composition) (third composition)
              (composition-generates-chunks? composition chunk1 chunk2))
          collect composition)))

(defun composition-generates-chunks? (composition chunk1 chunk2)
  (let ((new-chunks1 (append
                      (combine-chunk-program (second composition) (first composition))
                      (combine-chunk-program (first composition) (second composition))))
        (new-chunks2 (append
                      (combine-chunk-program (third composition) (first composition))
                      (combine-chunk-program (first composition) (third composition)))))
    (and 
     (loop for new-chunk in new-chunks1
           thereis (equivalent-irl-programs? (irl-program chunk1) (irl-program new-chunk)))
     (loop for new-chunk in new-chunks2
           thereis (equivalent-irl-programs? (irl-program chunk2) (irl-program new-chunk))))))

