
(in-package :fcg)

;; ############################################################################
;; Initial structures for root-mode
;; ############################################################################

(defmethod create-initial-structure ((meaning list) (mode t))
  (make-instance 'coupled-feature-structure
                 :left-pole `((root
			       (referent)
			       (meaning ,meaning)
			       (sem-cat nil)))
		 :right-pole '((root
				(form nil)
				(syn-cat nil)))
		 :left-pole-domain 'sem
		 :right-pole-domain 'syn))

(defmethod create-initial-structure ((meaning list)
                                     (mode (eql :root-mode)))
  (create-initial-structure meaning t))

(defmethod create-initial-structure ((meaning list)
                                     (mode (eql :root-mode-replace-variables-with-symbols)))
  (create-initial-structure (replace-variables-with-symbols meaning) t))

;; ############################################################################
;; Initial structures for one-pole-mode
;; ############################################################################

(defmethod create-initial-structure ((meaning list)
                                     (mode (eql :one-pole-mode)))
  (make-instance 'coupled-feature-structure
                 :left-pole `((root
			       (referent)
			       (meaning ,meaning)
			       (sem-cat nil)
                               (form nil)
                               (syn-cat nil)))
		 :right-pole '((root))
		 :left-pole-domain 'sem
		 :right-pole-domain 'syn))

