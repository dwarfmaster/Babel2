(in-package :fcg)

(defun feature-value-p (string)
  (string/= string ""))

(defun handle-feature-atom (string &key (return-var nil))
  (if (feature-value-p string) ;;feature has a value assigned to it
    (intern (upcase (string-trim " " string)))
    (if return-var
      (make-var)
      nil)))

(defun empty-string-p (string)
  (or (string= string "")
      (string= string " ")))

(defun handle-feature-list (string) ;;can be of one elt!

  (loop for elt in (split-sequence:split-sequence #\Space string)
        unless (empty-string-p elt)
        collect (intern (upcase elt))))

(defun get-entry (header line headers)
  (nth (position header headers :test #'string=) line))

(defun add-determiners (file basic-grammar &key (delimitator #\;))
  (when file
    (loop with headers = nil
          for line = (read-line file nil)
          for i from 0
          while line
          do (setf line (split-sequence:split-sequence delimitator line))
          (cond ((= i 0)
                 (setf headers (loop for s in line
                                     collect (intern (upcase s)))))
                ((string= (get-entry 'label line headers) "lex")
                 (let ((meaning (handle-feature-list (get-entry 'meaning line headers)))
                       (lex-class (handle-feature-atom (get-entry 'lex-class line headers)))
                       (definiteness (handle-feature-atom (get-entry 'definiteness line headers))))
                   (create-determiner-lemma-cxn (get-entry 'form line headers)
                                                :meaning meaning :lex-class lex-class
                                                :definiteness definiteness
                                                :cxn-inventory basic-grammar)))
                ((string= (get-entry 'label line headers) "morph")
                 (let ((number (handle-feature-atom (get-entry 'number line headers)))
                       (gender (handle-feature-atom (get-entry 'gender line headers)))
                       (floating (handle-feature-atom (get-entry 'floating line headers)))
                       (downward (handle-feature-atom (get-entry 'downward line headers)))
                       (definiteness (handle-feature-atom (get-entry 'definiteness line headers)))
                       (lex-class (handle-feature-atom (get-entry 'lex-class line headers))))
                 (create-determiner-morph-cxn (get-entry 'lemma line headers)
                                              (get-entry 'form line headers)
                                              :number number :gender gender
                                              :definiteness definiteness
                                              :floating floating :downward downward
                                              :lex-class lex-class
                                              :cxn-inventory basic-grammar)))))))
          
(defun add-pronouns (file basic-grammar &key (delimitator #\;))
  (when file
    (loop with headers = nil
          for line = (read-line file nil)
          for i from 0
          while line
          do (setf line (split-sequence:split-sequence delimitator line))
          (cond ((= i 0)
                 (setf headers (loop for s in line
                                     collect (intern (upcase s)))))
                ((and (string= (get-entry 'form line headers)
                               (get-entry 'lemma line headers)) ;;both lex and morph needed
                      (null (string= (get-entry 'lex-class line headers) "clitic")))
                 (let ((meaning (handle-feature-list (get-entry 'meaning line headers)))
                       (person (when (feature-value-p (get-entry 'person line headers))
                                 (parse-integer (get-entry 'person line headers))))
                       (number (handle-feature-atom (get-entry 'number line headers)))
                       (gender (handle-feature-atom (get-entry 'gender line headers)))
                       (case (handle-feature-list (get-entry 'case line headers)))
                       (lex-class (handle-feature-atom (get-entry 'lex-class line headers))))
                   (create-personal-pronoun-lemma-cxn (get-entry 'lemma line headers)
                                                      :number number :person person :meaning meaning
                                                      :gender gender
                                                      :relative (if (string= lex-class 'relative) '+ '-)
                                                      :reflexive (if (string= lex-class 'reflexive) '+ '-)
                                                      :demonstrative (if (string= lex-class 'demonstrative) '+ '-)
                                                      :cxn-inventory basic-grammar)
                   (create-personal-pronoun-morph-cxn (get-entry 'lemma line headers)
                                                      (get-entry 'form line headers)
                                                      :number number :person person :case case
                                                      :gender gender :lex-class lex-class
                                                      :relative (if (string= lex-class 'relative) '+ '-)
                                                      :cxn-inventory basic-grammar)))
                ((string= (get-entry 'case line headers) "prepositional-object")
                 (let ((person (when (feature-value-p (get-entry 'person line headers))
                                 (parse-integer (get-entry 'person line headers))))
                       (number (handle-feature-atom (get-entry 'number line headers)))
                       (gender (handle-feature-atom (get-entry 'gender line headers)))
                       (lex-class (handle-feature-atom (get-entry 'lex-class line headers))))
                   
                   (create-prepositional-object-morph-cxn (get-entry 'lemma line headers)
                                                          (get-entry 'form line headers)
                                                          :number number :person person
                                                          :gender gender :lex-class  lex-class
                                                          :reflexive (if (string= lex-class 'reflexive) '+ '-)
                                                          :relative (if (string= lex-class 'relative) '+ '-)
                                                          :cxn-inventory basic-grammar)))
                ((and (get-entry 'form line headers)
                      (null (empty-string-p (get-entry 'lemma line headers)))) ;;morph needed
                 (let ((person (when (feature-value-p (get-entry 'person line headers))
                                 (parse-integer (get-entry 'person line headers))))
                       (number (handle-feature-atom (get-entry 'number line headers)))
                       (gender (handle-feature-atom (get-entry 'gender line headers)))
                       (case (handle-feature-list (get-entry 'case line headers)))
                       (lex-class (handle-feature-atom (get-entry 'lex-class line headers))))
                   
                   (create-personal-pronoun-morph-cxn (get-entry 'lemma line headers)
                                                      (get-entry 'form line headers)
                                                      :number number :person person 
                                                      :gender gender :lex-class  lex-class :case case
                                                      :reflexive (if (string= lex-class 'reflexive) '+ '-)
                                                      :relative (if (string= lex-class 'relative) '+ '-)
                                                      :cxn-inventory basic-grammar)))
                ((null (empty-string-p (get-entry 'form line headers)))
                 (let ((meaning (handle-feature-list (get-entry 'meaning line headers)))
                       (person (when (feature-value-p (get-entry 'person line headers))
                                 (parse-integer (get-entry 'person line headers))))
                       (number (handle-feature-atom (get-entry 'number line headers)))
                       (gender (handle-feature-atom (get-entry 'gender line headers)))
                      ; (case (handle-feature-list (get-entry 'case line headers)))
                       (lex-class (handle-feature-atom (get-entry 'lex-class line headers))))
                   (create-pronoun-lex-cxn (get-entry 'form line headers)
                                           :number number :person person :meaning meaning
                                           :gender gender :cxn-set 'lex
                                           :relative (if (string= lex-class 'relative) '+ '-)
                                           :demonstrative (if (string= lex-class 'demonstrative) '+ '-)
                                           :reflexive (if (string= lex-class 'reflexive) '+ '-)
                                           :cxn-inventory basic-grammar)))))))

(defun add-adverbs (file basic-grammar &key (delimitator #\;))
  (when file
    (loop with headers = nil
          for line = (read-line file nil)
          for i from 0
          while line
          do (setf line (split-sequence:split-sequence delimitator line))
          (cond ((= i 0)
                 (setf headers (loop for s in line
                                     collect (intern (upcase s)))))
                (t
                 (let ((lemma (first line))
                       (sem-class (handle-feature-list (get-entry 'sem-class line headers)))
                       (operator-like (handle-feature-atom (get-entry 'operator-like line headers)))
                       (negation (handle-feature-atom (get-entry 'negation line headers)))
                       (meaning (handle-feature-atom (get-entry 'meaning line headers))))
                   (create-adverb-cxn lemma :sem-class sem-class
                                      :meaning meaning :operator-like operator-like
                                      :negation negation :cxn-inventory basic-grammar)))))))

(defun add-conjunctions (file basic-grammar &key (delimitator #\;))
  (when file
    (loop with headers = nil
          for line = (read-line file nil)
          for i from 0
          while line
          do (setf line (split-sequence:split-sequence delimitator line))
          (cond ((= i 0)
                 (setf headers (loop for s in line
                                     collect (intern (upcase s)))))
                (t
                 (let ((lemma (first line))
                       (syn-function (handle-feature-atom (get-entry 'syn-function line headers)))
                       (sem-class (handle-feature-list (get-entry 'sem-class line headers)))
                       (meaning (handle-feature-list (get-entry 'meaning line headers))))
                   (create-conjunction-cxn lemma :sem-class sem-class :meaning meaning
                                           :syn-function syn-function
                                           :cxn-inventory basic-grammar)))))))

(defun add-adjectives (file basic-grammar &key (delimitator #\;))
  (when file
    (loop with headers = nil
          for line = (read-line file nil)
          for i from 0
          while line
          do (setf line (split-sequence:split-sequence delimitator line))
          (cond ((= i 0)
                 (setf headers (loop for s in line
                                     collect (intern (upcase s)))))
                (t
                 (let ((lemma (first line))
                       (sem-class (handle-feature-list (get-entry 'sem-class line headers)))
                       (gender (handle-feature-atom (get-entry 'gender line headers)))
                       (number (handle-feature-atom (get-entry 'number line headers)))
                       (meaning (handle-feature-list (get-entry 'meaning line headers))))
                   (create-adjective-cxn lemma :sem-class sem-class :meaning meaning
                                           :gender gender :number number
                                           :cxn-inventory basic-grammar)))))))

(defun add-prepositions (file basic-grammar &key (delimitator #\;))
  (when file
    (loop with headers = nil
          for line = (read-line file nil)
          for i from 0
          while line
          do (setf line (split-sequence:split-sequence delimitator line))
          (cond ((= i 0)
                 (setf headers (loop for s in line
                                     collect (intern (upcase s)))))
                (t
                 (let ((lemma (first line))
                       (sem-class (handle-feature-list (get-entry 'sem-class line headers)))
                       (meaning (handle-feature-atom (get-entry 'meaning line headers))))
                   (create-prepositional-cxn lemma :sem-class sem-class :meaning meaning
                                           :cxn-inventory basic-grammar)))))))

(defun add-nouns (file basic-grammar &key (delimitator #\;))
  ;(activate-monitor trace-fcg-light)
  (when file
    (loop for line = (read-line file nil)
          while line
          do (setf line (split-sequence:split-sequence delimitator line))
          (let ((lemma (first line))
                (type (handle-feature-atom (second line)))
                (person (if (feature-value-p (third line))
                                  (parse-integer (third line)) (make-var)))
                (number (handle-feature-atom (fourth line)))
                (gender (handle-feature-atom (fifth line)))
                (sem-class (handle-feature-list (sixth line)))
                (meaning (handle-feature-list (seventh line))))
            (create-noun-cxn lemma :person person :lex-class type
                             :number number :gender gender :sem-class sem-class
                             :meaning meaning :cxn-inventory basic-grammar)))))
                    
(defun add-verbs (verb-stream basic-grammar &key (delimitator #\;))
  ;(activate-monitor trace-fcg-light)
  ;;check sem-class
  (when verb-stream
         (loop for line = (read-line verb-stream nil)
               for i from 0
               while line
               do 
               (setf line (split-sequence:split-sequence delimitator line)) ;;line is a list of strings!
               if (string= (second line) "infinitive")
               do (let ((lemma (first line))
                        (conj-class (handle-feature-atom (nth 2 line)))
                        (reflexiveness (handle-feature-atom (nth 3 line)))
                        (sem-class (handle-feature-list (nth 4 line)))
                        (meaning (handle-feature-list (nth 5 line))))
                    (create-verb-lemma-cxn
                     lemma :meaning meaning :sem-class sem-class :reflexive? reflexiveness
                     :conj-class conj-class :cxn-inventory basic-grammar)
                    (create-infinitive-morph-cxn
                     lemma :sem-class sem-class 
                     :conj-class conj-class :cxn-inventory basic-grammar))
                    ;;also create morpheme for infinitive)
               else do (let ((lemma (third line))
                             (verb-form (first line))
                             (person (if (feature-value-p (nth 3 line))
                                       (parse-integer (nth 3 line)) (make-var)))
                             (number (handle-feature-atom (nth 4 line) :return-var t))
                             (tense (handle-feature-atom (nth 5 line) :return-var t))
                      ;  (aspect (handle-feature-atom (nth 6 line) :return-var t))
                             (mood (handle-feature-atom (nth 7 line) :return-var t))
                             (sem-class (handle-feature-list (nth 8 line)))
                             (conj-class (handle-feature-atom (nth 9 line) :return-var t)))
                         (create-verb-morph-cxn lemma verb-form
                                                :person person :number number :sem-class sem-class
                                                :tense tense :mood mood :conj-class conj-class
                                                ;;aspect not added yet!
                                                :cxn-inventory basic-grammar)))))

(defun create-grammar ()
  (setf *portuguese-grammar* nil)
  ;;this should load the grammatical cxns from PROPOR
  (let ((basic-grammar (make-proclisis-extended-cxns)))
    ;;this loads extensions for the larger grammar we need now:
    (pp (name basic-grammar))

    ;;I. Read in verbs
    (let ((verb-stream (open (babel-pathname :directory '("grammars" "Portuguese" "Mini-Corpus" "word-classes")
                                             :name "verbs"
                                             :type "csv"))))
      (add-verbs verb-stream basic-grammar)
      (close verb-stream))

    ;;II. Read in nouns and pronouns
    (let ((noun-stream (open (babel-pathname :directory '("grammars" "Portuguese" "Mini-Corpus" "word-classes")
                                             :name "nouns"
                                             :type "csv")))
          (pronoun-stream (open (babel-pathname :directory '("grammars" "Portuguese" "Mini-Corpus" "word-classes")
                                                :name "pronouns"
                                                :type "csv"))))
      (add-nouns noun-stream basic-grammar)
      (add-pronouns pronoun-stream basic-grammar)
      (close noun-stream)
      (close pronoun-stream)) 

    ;;;III. Read in determiners
    (let ((determiner-stream (open (babel-pathname :directory '("grammars" "Portuguese" "Mini-Corpus" "word-classes")
                                                   :name "determiners"
                                                   :type "csv"))))
      (add-determiners determiner-stream basic-grammar)
      (close determiner-stream))

    ;;;VI. Read in adverbs
    (let ((adverbs-stream (open (babel-pathname :directory '("grammars" "Portuguese" "Mini-Corpus" "word-classes")
                                                :name "adverbs"
                                                :type "csv"))))
      (add-adverbs adverbs-stream basic-grammar)
      (close adverbs-stream))

    ;;;V. Read in conjunctions
    (let ((conjunction-stream (open (babel-pathname :directory '("grammars" "Portuguese" "Mini-Corpus" "word-classes")
                                                    :name "conjunctions"
                                                    :type "csv"))))
      (add-conjunctions conjunction-stream basic-grammar)
      (close conjunction-stream))

    ;;;VI. Read in adjectives
    (let ((adjective-stream (open (babel-pathname :directory '("grammars" "Portuguese" "Mini-Corpus" "word-classes")
                                                  :name "adjectives"
                                                  :type "csv"))))
      (add-adjectives adjective-stream basic-grammar)
      (close adjective-stream))

    ;;;VII. Read in prepositions
    (let ((preposition-stream (open (babel-pathname :directory '("grammars" "Portuguese" "Mini-Corpus" "word-classes")
                                                    :name "prepositions"
                                                    :type "csv"))))
      (add-prepositions preposition-stream basic-grammar)
      (close preposition-stream))

     
    basic-grammar))


