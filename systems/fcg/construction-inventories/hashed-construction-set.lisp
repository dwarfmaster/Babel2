(in-package :fcg)

(export '(hashed-construction-set constructions-hash-table))
  
(defclass hashed-construction-set (construction-set)
  ((constructions-hash-table
    :initarg :constructions-hash-table
    :accessor constructions-hash-table
    :initform (make-hash-table :test #'equalp)
    :documentation "The hash table maps hash-keys to lists of constructions.
                    Is automatically is updated if you use the methods provided in this file"))
  (:documentation "An organization of fcg constructions which supports
                   hashing and direct access"))

(defmethod initialize-instance :after ((construction-set hashed-construction-set)
                                       &key &allow-other-keys)
  (set-configuration (configuration construction-set) 
                     :cxn-supplier-mode :hashed-simple-queue)
  (set-configuration (configuration construction-set) 
                     :hash-mode :hash-word-entity-root)
  (set-configuration (configuration construction-set) 
                     :hash-mode :hash-word-entity-root)
  (set-configuration (configuration construction-set) 
                     :production-goal-tests '(:no-applicable-cxns))
  (set-configuration (configuration construction-set) 
                     :parse-goal-tests '(:no-applicable-cxns))
  (set-configuration (configuration construction-set) 
                     :create-initial-structure-mode  :root-mode)
  (set-configuration (configuration construction-set) 
                     :render-mode :render-in-root-mode)
  (set-configuration (configuration construction-set) 
                     :de-render-mode :de-render-in-root-mode))

;; #########################################################
;; hash
;; ---------------------------------------------------------

(defgeneric hash (thing mode &key &allow-other-keys)
  (:documentation "To implement hashing you need provide two hash functions
                   See examples in cxn-supplier.lisp
                   1) given a construction should return hash keys (for production and parsing),
                   2) given a node should return the hashes for a node (or more precisely the cfs)"))

;; #########################################################
;; clear/setf
;; ---------------------------------------------------------

(defmethod clear ((construction-set hashed-construction-set)
                  &key (include-trash t) &allow-other-keys)
  (setf (constructions construction-set) nil)
  (clrhash (constructions-hash-table construction-set))
  (when include-trash
    (setf (trash construction-set) nil)))

;; #########################################################
;; add-cxn
;; ---------------------------------------------------------

(defmethod add-cxn ((construction construction) (construction-set hashed-construction-set)
                    &key (recover-from-trash nil) (equivalent-key #'name) 
                    (equivalent-test #'eql) &allow-other-keys)
  (if recover-from-trash
    (let* ((trashed-cxn (find (funcall equivalent-key construction) (trash construction-set) 
                              :key equivalent-key
                              :test equivalent-test)))
      (push trashed-cxn (constructions construction-set))
      (setf (trash construction-set) (remove trashed-cxn (trash construction-set)))
      (loop for hash in (hash trashed-cxn (get-configuration construction-set :hash-mode))
            do (setf (gethash hash (constructions-hash-table construction-set))
                     (push construction
                           (gethash hash (constructions-hash-table construction-set))))))
    (progn
      (loop
       with hashes = (hash construction
                          (get-configuration construction-set :hash-mode))
       for hash in (if (null hashes) (list nil) hashes)
       do (setf (gethash hash (constructions-hash-table construction-set))
                (cons construction
                      (gethash hash (constructions-hash-table construction-set)))))
      (push construction (constructions construction-set))))
  construction-set)

;; #########################################################
;; delete-cxn
;; ---------------------------------------------------------

(defmethod delete-cxn ((construction construction) 
                       (construction-set hashed-construction-set)
                       &key (key #'identity) (test #'eql) (move-to-trash nil))
  (let ((to-delete (find-cxn construction construction-set :test test :key key)))
    (when to-delete
      (setf (constructions construction-set)
            (remove to-delete (constructions construction-set)))
      (when move-to-trash
        (push to-delete (trash construction-set)))
      (loop
       with hashes = (hash to-delete (get-configuration construction-set :hash-mode))
       for hash in (if (null hashes) (list nil) hashes)
       do (setf (gethash hash (constructions-hash-table construction-set))
                (remove to-delete (gethash hash (constructions-hash-table construction-set)))))
      to-delete)))

;; #########################################################
;; delete-cxn
;; ---------------------------------------------------------

(defmethod copy-object-content ((source hashed-construction-set)
                                (destination hashed-construction-set))
  (setf (constructions destination) (copy-list (constructions source)))
  (setf (trash destination) (copy-list (trash source)))
  (loop for key being the hash-keys of
        (constructions-hash-table source)
        do (setf (gethash key (constructions-hash-table destination))
                 (gethash key (constructions-hash-table source)))))
        




