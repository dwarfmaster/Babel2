(in-package :fcg)

;(export '(:followed-by-front-vowel))

(defmethod fcg-expand ((type (eql :followed-by-front-vowel?))
                       &key value source bindings merge?)
  
  (if merge?
    value
    (when source
      (let ((front-vowels '("e" "é" "i" "í")))
        (if (member (first source) front-vowels :test #'string=)
          source
          nil)))))

(defmethod fcg-expand ((type (eql :stressed-stem-assimilation?))
                       &key value source bindings merge?)
  
  (if merge?
    value
    (when source
      (let ((front-vowels '("o" "a" "ó" "á")))
        (if (and (member (first source) front-vowels :test #'string=)
                 (= (length source) 1))
          source
          nil)))))

(defmethod fcg-expand ((type (eql :unstressed-stem-assimilation?))
                       &key value source bindings merge?)
  
  (if merge?
    value
    (when source
      (let ((front-vowels '("o" "a" "ó" "á")))
        (if (and (member (first source) front-vowels :test #'string=)
                 (> (length source) 1))
          source
          nil)))))


(defmethod fcg-expand ((type (eql :stressed-stem?))
                       &key value source bindings merge?)
  
  (if merge?
    value
    (when source
      (if (= (length source) 1)
        source
        nil))))

(defmethod fcg-expand ((type (eql :unstressed-stem?))
                       &key value source bindings merge?)
  
  (if merge?
    value
    (when source
      (if (= (length source) 1) ;;not length but look inside strings
        source
        nil))))


