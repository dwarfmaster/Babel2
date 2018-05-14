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
                     :hash-mode :hash-word-entity-root-one-pole))

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
  (setf (constructions construction-set) nil) ;;always nil
  (clrhash (constructions-hash-table construction-set))
  (when include-trash
    (setf (trash construction-set) nil)))

;; #########################################################
;; add-cxn
;; ---------------------------------------------------------

(defmethod add-cxn :before ((construction construction)
                            (hashed-construction-set hashed-construction-set)
                            &key (replace-when-equivalent t)
                            (equivalent-test #'eql) (equivalent-key #'name) (hash-keys nil))
  (when replace-when-equivalent
    (loop for hash-key in hash-keys
          do (delete-cxn construction hashed-construction-set 
                         :test equivalent-test :key equivalent-key :hash-key hash-key))))

(defmethod add-cxn ((construction construction) (construction-set hashed-construction-set)
                    &key (recover-from-trash nil) (equivalent-key #'name) 
                    (equivalent-test #'eql) &allow-other-keys)
  (if recover-from-trash
    (let* ((trashed-cxn (find (funcall equivalent-key construction) (trash construction-set) 
                              :key equivalent-key
                              :test equivalent-test)))
      (if trashed-cxn
        (loop with hashes = (hash construction
                                    (get-configuration construction-set :hash-mode))
                for hash in (if (null hashes) (list nil) hashes)
                do (setf (gethash hash (constructions-hash-table construction-set))
                         (cons construction
                               (gethash hash (constructions-hash-table construction-set))))
                finally do
                (setf (trash construction-set) (remove trashed-cxn (trash construction-set))))
        (loop for hash in (hash construction (get-configuration construction-set :hash-mode))
              do (setf (gethash hash (constructions-hash-table construction-set))
                       (cons construction
                             (gethash hash (constructions-hash-table construction-set)))))))
    (progn
      (loop
       with hashes = (hash construction
                          (get-configuration construction-set :hash-mode))
       for hash in (if (null hashes) (list nil) hashes)
       do (setf (gethash hash (constructions-hash-table construction-set))
                (cons construction
                      (gethash hash (constructions-hash-table construction-set)))))))
  construction-set)

;; #########################################################
;; find-cxn
;; ---------------------------------------------------------

(defmethod find-cxn ((construction construction)
                     (hashed-construction-set hashed-construction-set) 
                     &key (key #'name) (test #'eql) (search-trash nil))

  (when search-trash
    (let ((found-cxn
           (find (funcall key construction) (trash hashed-construction-set) :key key :test test)))
      (when found-cxn
        (return-from find-cxn found-cxn))))
  
  (loop with hashes = (hash construction
                            (get-configuration hashed-construction-set :hash-mode))
        for hash in (if (null hashes) (list nil) hashes)
        for cxn = (find (funcall key construction)
                        (gethash hash (constructions-hash-table hashed-construction-set))
                        :test test :key key)
        when cxn
        do (return cxn)))

(defmethod find-cxn ((construction t) (hashed-construction-set hashed-construction-set) 
                     &key (key #'name) (test #'eql) (search-trash nil) (hash-key nil))
  (find construction
        (append (gethash hash-key (constructions-hash-table hashed-construction-set))
                (when search-trash (trash hashed-construction-set)))
        :key key :test test))


;; #########################################################
;; delete-cxn
;; ---------------------------------------------------------

(defmethod delete-cxn ((construction construction) 
                       (construction-set hashed-construction-set)
                       &key (key #'identity) (test #'eql) (move-to-trash nil) (hash-key nil))
  (let ((to-delete (find-cxn (funcall key construction) construction-set :test test :key key :hash-key hash-key)))
    (when to-delete
      (when move-to-trash
        (push to-delete (trash construction-set)))
      (loop
       with hashes = (hash to-delete (get-configuration construction-set :hash-mode))
       for hash in (if (null hashes) (list nil) hashes)
       do (setf (gethash hash (constructions-hash-table construction-set))
                (remove to-delete (gethash hash (constructions-hash-table construction-set))))
       (unless (gethash hash (constructions-hash-table construction-set))
         (remhash hash (constructions-hash-table construction-set))))
      to-delete)))

;; #########################################################
;; copy-object-content
;; ---------------------------------------------------------

(defmethod copy-object-content ((source hashed-construction-set)
                                (destination hashed-construction-set))
  (setf (constructions destination) (copy-list (constructions source)))
  (setf (trash destination) (copy-list (trash source)))
  (loop for key being the hash-keys of
        (constructions-hash-table source)
        do (setf (gethash key (constructions-hash-table destination))
                 (gethash key (constructions-hash-table source)))))
        


;; #########################################################
;; size
;; ---------------------------------------------------------

(defmethod size ((hashed-construction-set hashed-construction-set))
  "Calculates the size of the hash table of a hashed construction set
in terms of the number of hash keys."
  (hash-table-count (constructions-hash-table hashed-construction-set)))


