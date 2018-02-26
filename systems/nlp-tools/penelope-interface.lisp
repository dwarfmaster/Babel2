
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interfacing with the NLP tools provided by the Penelope web service  ;;
;; Katrien and Paul, October 2017                                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :nlp-tools)

(export '(get-penelope-noun-chunks
          get-penelope-named-entities
          get-penelope-pos-tags
          run-penelope-dependency-parser
          get-penelope-tokens
          get-penelope-sentence-tokens
          get-word-similarity
          get-word-embeddings
          curl-json))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Intefacing with using http request and json ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This used curl, but we're using DEX now directly from lisp (for using curl, you need to encode-json-as-string-for-shell!!)
;; update: using it again with lispworks until the boringssl bug is fixed   

#+lispworks
(defun curl-json (url json)
  "Send curl request and returns the answer."
  (let ((response (exec-and-return "curl" url "-H"
                                   #+lispworks (format nil "~s" "Content-Type: application/json")
                                   #-lispworks (format nil "~a" "Content-Type: application/json")
                                   "-s"   "-d "  json ))) ;; remove single quotes for non lispworks
    (when response (cl-json:decode-json-from-string (first response)))))

#-lispworks
(defun curl-json (url json)
  "Send curl request and returns the answer."
  (let ((response (dex:post url
                            :headers '((Content-Type . "application/json"))
                            :content json)))
    (when response (cl-json:decode-json-from-string response))))

;;;;;;;;;;;;;;;;;;;;;
;; Noun chunks     ;;
;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-noun-chunker (sentence &key (model "en"))
  "Call the penelope server to get the noun chunks, dependency labels
and POS tags for a sentence."
  (unless (stringp sentence)
    (if (listp sentence)
      (setf sentence (format nil "~{~a~^ ~}" sentence))
      (error "The function <run-penelope-noun-chunker> expects a string as input")))
  (let ((server-result
          (curl-json "https://www.fcg-net.org/penelope/nlp/nchunk" (encode-json-to-string-for-shell `((:text . ,sentence) (:model . ,model))))))
    (rest (first server-result))))

;;(run-penelope-noun-chunker "April is the fourth month of the year")

(defun get-penelope-noun-chunks (sentence)
  "Takes a sentence as a string as input and returns a list with lists
of strings, each list corresponding to a noun chunk."
  (let ((penelope-chunks (run-penelope-noun-chunker sentence)))
    (loop for chunk in penelope-chunks
          collect (mapcar #'(lambda (chunk)
                              (cdr (find :text chunk :key #'first)))
                          chunk))))

;;(get-penelope-noun-chunks "April is the fourth month of the year.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; POS tagging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-pos-tagger (sentence &key (model "en"))
  "Call the penelope server to get the POS tags for a sentence."
  (unless (stringp sentence)
    (error "The function <run-penelope-pos-tagger> expects a string as input"))
  (let ((server-result
         (curl-json "https://www.fcg-net.org/penelope/nlp/pos" (encode-json-to-string-for-shell `((:text . ,sentence) (:model . ,model))))))
    (rest (first server-result))))

(defun get-penelope-pos-tags (transient-structure/sentence)
  "Takes a sentence as a string as input and returns a list with lists
of strings, each list corresponding to a word with its most likely POS tag."
  (let* ((sentence (if (stringp transient-structure/sentence)
                     transient-structure/sentence
                     (if (stringp (get-data transient-structure/sentence :utterance))
                       (get-data transient-structure/sentence :utterance)
                       (list-of-strings->string (get-data transient-structure/sentence :utterance)))))
         (penelope-pos-tags (run-penelope-pos-tagger sentence)))
    (loop for entry in penelope-pos-tags
          for word = (rest (assoc :text entry ))
          for tag = (rest (assoc :tag entry ))
          collect (list word tag))))

;;(get-penelope-pos-tags "April is the fourth month of the year.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Named Entity Recognition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-named-entity-recognition (sentence &key (model "en"))
  "Call the penelope server to get the named entities from a sentence."
  (unless (stringp sentence)
    (error "The function <run-penelope-named-entity-recognition> expects a string as input"))
  (let ((server-result
         (curl-json "https://www.fcg-net.org/penelope/nlp/ent" (encode-json-to-string-for-shell `((:text . ,sentence) (:model . ,model))))))
    (rest (first server-result))))

(defun get-penelope-named-entities (transient-structure/sentence)
  "Takes a sentence as a string as input and returns a list with lists
of strings, each list corresponding to a named entity."
  (let* ((sentence (if (stringp transient-structure/sentence)
                     transient-structure/sentence
                     (if (stringp (get-data transient-structure/sentence :utterance))
                       (get-data transient-structure/sentence :utterance)
                       (list-of-strings->string (get-data transient-structure/sentence :utterance)))))
         (penelope-named-entities (run-penelope-named-entity-recognition sentence)))
    (loop for entry in penelope-named-entities
          for entity = (rest (assoc :text entry ))
          for type = (rest (assoc :ent entry ))
          collect (list entity type))))

;;(get-penelope-named-entities "The study, carried out at Geomar Helmholtz Centre for Ocean Research in Germany, was the most comprehensive of the subject to date.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Dependency parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-dependency-parser (sentence &key (model "en"))
  "Call the penelope server to get the dependency labels all words in a sentence."
  (unless (stringp sentence)
    (error "The function <run-penelope-dependency-parser> expects a string as input"))
  (let ((server-result
         (curl-json "https://www.fcg-net.org/penelope/nlp/dep" (encode-json-to-string-for-shell `((:text . ,sentence) (:model . ,model))))))
    (rest (first server-result))))

;;(run-penelope-dependency-parser "April is the fourth month of the year")

;; TODO Add get function once we know what to use it for.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Tokenization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-tokenizer (sentence &key (model "en"))
  "Call the penelope server to tokenize a sentence."
  (unless (stringp sentence)
    (error "The function <run-penelope-tokenizer> expects a string as input"))
  (let* ((server-result
          (curl-json "https://www.fcg-net.org/penelope/nlp/tok" (encode-json-to-string-for-shell `((:text . ,sentence) (:model . ,model))))))
    (rest (first server-result))))

;;(run-penelope-tokenizer "Paul kicked the ball. Mary caught it.")

(defun get-penelope-tokens (sentence)
  "Returns tokens."
  (run-penelope-tokenizer sentence))

;;(get-penelope-tokens "Paul kicked the ball. Mary caught it.")

(defun run-penelope-sentence-tokenizer (sentences &key (model "en"))
  "Call the penelope server to get the dependency labels all words in a sentence."
  (unless (stringp sentences)
    (error "The function <run-penelope-sentence-tokenizer> expects a string as input"))
  (let ((server-result
         (curl-json "https://www.fcg-net.org/penelope/nlp/stok" (encode-json-to-string-for-shell `((:text . ,sentences) (:model . ,model))))))
    (rest (first server-result))))

(defun get-penelope-sentence-tokens (sentences)
  "Returns sentences."
  (run-penelope-sentence-tokenizer sentences))

;;(run-penelope-sentence-tokenizer "Paul kicked the ball. Mary caught it.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Word embeddings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-penelope-sentence-word-embeddings (sentence &key (model "en"))
  "Call the penelope server to get the word embeddings of a single sentence."
  (unless (stringp sentence)
    (error "The function <run-penelope-sentence-word-embeddings> expects a string as input"))
  (let ((server-result
         (curl-json "https://www.fcg-net.org/penelope/nlp/glove" (encode-json-to-string-for-shell `((:text . ,sentence) (:model . ,model))))))
    (rest (first server-result))))

;(run-penelope-sentence-word-embeddings "hello")

(defun get-word-embeddings (sentence)
  "Get the word embeddings for a sentence in a '((word1 vector1) (word2 vector2)) format."
  (let ((penelope-embeddings (run-penelope-sentence-word-embeddings sentence)))
    (loop for word-embedding in penelope-embeddings
          for token = (rest (assoc :token word-embedding))
          for vector = (rest (assoc :vector word-embedding))
          collect (list token vector))))

;(get-word-embeddings "hello world")

(defun get-word-similarity (word1 word2)
  "Calculates the cosine similarity between two words based on the word embeddings from Glove."
  (let ((vector1 (second (first (get-word-embeddings word1))))
        (vector2 (second (first (get-word-embeddings word2)))))
    (cosine-similarity vector1 vector2)))

;;(get-word-similarity "boy" "banana")

(defun get-phrase-similarity (phrase1 phrase2)
  ;;multiply? pretty girl vs handsome boy
  (let* ((vectors-for-phrase-1 (mapcar #'(lambda (word)
                                           (second (first (get-word-embeddings word))))
                                       (split-sequence:split-sequence #\Space phrase1)))
         (vector1 (utils::multiply-list-of-vectors vectors-for-phrase-1))
         (vectors-for-phrase-2 (mapcar #'(lambda (word)
                                           (second (first (get-word-embeddings word))))
                                       (split-sequence:split-sequence #\Space phrase2)))
         (vector2 (utils::multiply-list-of-vectors vectors-for-phrase-2)))
    (utils::cosine-similarity vector1 vector2)))

;;(get-phrase-similarity "handsome boy" "pretty girl")
