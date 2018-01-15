;; Katrien Beuls, September 2016
(in-package :fcg)

(export '(full-grammar-evaluation make-gold-standard-meaning-file
                                  evaluate-sentence-for-comprehension write-evaluation-results print-evaluation-report create-sentence-profiles-for-comprehension
                                  evaluate-grammar-for-comprehension evaluate-grammar-for-production))

(defstruct (sentence-processing-result (:conc-name pr-))
  original-sentence
  original-meaning
  parsed-meaning
  gold-standard-meaning
  parsing-final-node
  formulated-sentence
  gold-standard-sentence
  production-final-node)

(defstruct (sentence-profile (:conc-name sp-))
  unprocessed-sentence
  sentence
  meaning
  (nr-of-words 0)
  parsed-meaning
  gold-standard-meaning
  gold-standard-sentence
  (nr-of-meaning-chunks '(0.0 0.0))
  (nr-of-syntactic-trees '(0.0 0.0))
  (smatch-score '(0.0 0.0))
  cxns-comprehension 
  (average-branching-factor-comprehension '(0.0 0.0))
  formulated-utterance
  bi-directionalp 
  cxns-formulation
  (average-branching-factor-formulation '(0.0 0.0))
  (longest-common-substring '(0.0 0.0))
  (word-level-edit-distance '(0 0.0)))

(define-event comprehension-finished
  (parsed-meaning list)
  (final-node-parsing cip-node))

(define-event formulation-finished
  (formulated-utterance list)
  (final-node-production cip-node))

(defun make-sentence/gs-meaning-list (input-file &key max-sentence-length segmentor)
  (let ((test-sentences-with-meanings nil))
    (with-open-file (in input-file :direction :input :external-format '(:utf-8) :element-type :default) ;
      (loop for sentence-with-meaning = (read-line in nil)
            while sentence-with-meaning
            do (let ((list (cl-ppcre:split segmentor sentence-with-meaning)))
                 (when list ;;sentence-with-meaning can be an empty string
                   (cond ((= (length list) 1) ;;only sentence, no meaning
                          (setf test-sentences-with-meanings (cons (list (string-trim (format nil "~C~C" #\Space #\Tab) (first list)))
                                                                   test-sentences-with-meanings)))
                         ((= (length list) 2) ;;sentence, aligned with meaning
                          (setf test-sentences-with-meanings
                                (cons (list (string-trim (format nil "~C~C" #\Space #\Tab) (first list)) ;;remove empty strings at the beginning/end
                                            (read-from-string (second list))) test-sentences-with-meanings)))
                         (t (error "The input file is not correctly formatted. Please provide a file with rows consisting of utterances (single strings with spaces) and meanings (s-expressions). The default segmentor between the utterances and the meanings is <->. You can optionally also choose not to provide meanings and only work with utterances.")))))))
    (setf test-sentences-with-meanings ;;sort them on length
          (sort test-sentences-with-meanings #'<
                :key #'(lambda (sentence-with-meaning)
                         (length (split-sequence:split-sequence #\Space (first sentence-with-meaning))))))
    (when max-sentence-length ;;if max sentence length is specified:
      (setf test-sentences-with-meanings
            (loop for (sentence meaning) in test-sentences-with-meanings
                  while (<= (length (remove-if #'(lambda (word)
                                                   (string= word " ")
                                                   (string= word ".")
                                                   (string= word ","))
                                               (split-sequence::split-sequence #\Space sentence)))
                            max-sentence-length)
                  collect (list sentence meaning))))
    test-sentences-with-meanings))


(defun make-meaning/gs-sentence-list (input-file &key max-meaning-size segmentor)
  (let ((test-meanings-with-sentences nil))
    (with-open-file (in input-file :direction :input)
      (loop for sentence-with-meaning = (read-line in nil )
            while sentence-with-meaning
            do (let ((list (cl-ppcre:split segmentor sentence-with-meaning)))
                 (when list
                   (cond ((= (length list) 2) ;;sentence, aligned with meaning
                          (setf test-meanings-with-sentences
                                (cons (list (read-from-string (second list)) (string-trim (format nil "~C~C" #\Space #\Tab)
                                                                                          (first list) )) test-meanings-with-sentences)))
                         (t (error "The input file is not correctly formatted. Please provide a file with rows consisting of utterances (single strings with spaces) and meanings (s-expressions). The default segmentor between the utterances and the meanings is <->.")))))))
    (setf test-meanings-with-sentences ;;sort them on meaning size
          (sort test-meanings-with-sentences #'< :key #'(lambda (meaning-with-sentence) (length (first meaning-with-sentence)))))
    (when max-meaning-size ;;if max sentence length is specified:
      (setf test-meanings-with-sentences
            (loop for (meaning sentence) in test-meanings-with-sentences
                  while (<= (length meaning) max-meaning-size)
                  collect (list meaning sentence))))
    test-meanings-with-sentences))

(defun run-comprehension-and-reformulation-on-test-set (file-with-sentences-optionally-aligned-with-gold-standard-meanings
                                                        grammar
                                                        &key (max-sentence-length nil) (exclude-sentences-with-single-word t)
                                                        (bi-directional? t) (segmentor "<->")
                                                        (series 4))
  "Run comprehension on the test sentences (ordered from short to
long) and reformulate the parsed meanings. Optionally, you can
restrict the sentence length if you want to test only short sentences.
Returns a list of list of sentence processing results."
  
  ;; 1. Read and order test sentences on length
  ;;---------------------------------------------
  (let ((test-sentences-with-meanings
         (make-sentence/gs-meaning-list file-with-sentences-optionally-aligned-with-gold-standard-meanings
                                        :segmentor segmentor :max-sentence-length max-sentence-length)))
    ;(with-disabled-monitor-notifications

      ;; 2. Comprehend and reformulate sentences and return the processing results
      ;;----------------------------------------------------------------------
      (loop for (sentence meaning) in test-sentences-with-meanings
            for nr-of-words = (length (remove-if #'(lambda (word)
                                                     (or (string= word " ")
                                                         (string= word ".")))
                                                 (split-sequence:split-sequence #\Space sentence)))
            when (if exclude-sentences-with-single-word
                   (> nr-of-words 1) t)
            ;do (format t "~%~s" sentence)
            collect (evaluate-sentence-for-comprehension sentence grammar :meaning meaning :series series :bi-directional? bi-directional?))))


(defun run-formulation-and-recomprehension-on-test-set (file-with-sentences-aligned-with-gold-standard-meanings
                                                        grammar
                                                        &key (bi-directional? t) (max-meaning-size nil)
                                                        (series 4) (segmentor "<->"))
  "Run formulation on the test meanings (ordered from short to long)
and recomprehend the formulated utterances. Returns a list of list of
sentence processing results."
  
  ;; 1. Read and order test meanings on length
  ;;---------------------------------------------
  (let ((test-meanings-with-sentences
         (make-meaning/gs-sentence-list file-with-sentences-aligned-with-gold-standard-meanings
                                        :segmentor segmentor :max-meaning-size max-meaning-size)))
  ;(with-disabled-monitor-notifications
      ;; 2. Formulate and recomprehend sentences and return the processing results
      ;;---------------------------------------------------------------------------
      (loop for (meaning gold-standard-sentence) in test-meanings-with-sentences
            collect
            (loop for i from 1 to series ;;iteration
                  collect
                  (multiple-value-bind (formulated-utterance final-node-production)
                      (formulate meaning :cxn-inventory grammar)
             
                    (notify formulation-finished formulated-utterance final-node-production)
             
                    (multiple-value-bind (parsed-meaning final-node-parsing)
                        (if bi-directional?
                          (comprehend formulated-utterance :cxn-inventory grammar)
                          (values nil nil))
                      (format t ".")
                      (make-sentence-processing-result
                       :original-meaning meaning
                       :formulated-sentence (downcase (remove-punctuation formulated-utterance))
                       :gold-standard-sentence (downcase (remove-punctuation gold-standard-sentence))
                       :parsing-final-node final-node-parsing
                       :parsed-meaning parsed-meaning
                       :production-final-node final-node-production)))))))

(define-event nr-of-meaning-chunks-recorded (average-nr-of-chunks t))
(define-event nr-of-words-recorded (nr-of-words t))
(define-event cxns-comprehension-recorded (cxns list))
(define-event branching-factor-comprehension-recorded (average-branching-factor t))
(define-event nr-of-syntactic-trees-recorded (nr-of-trees t))

(defun create-sentence-profiles-for-comprehension (processing-results)
  "Evaluate a list of sentence processing results based on a number of
criteria such as average branching factor, smatch score, nr of
semantic chunks, word level edit distance, etc. If gold standard
meanings are provided, the parsed meanings can be compared to the gold
standard meanings."
  (loop for i from 0
        for sentence-processing-results in processing-results
        for processing-result = (random-elt sentence-processing-results) ;;exemplar result
        for plain-sentence = (format nil "~(~a~)" (remove-punctuation (pr-original-sentence processing-result)))
        for list-with-nrs-of-chunks/processing-result = (loop for pr in sentence-processing-results
                                                              collect (second (multiple-value-list
                                                                               (connected-semantic-network (pr-parsed-meaning pr)))))
        for list-with-nrs-of-trees/processing-result = (loop for pr in sentence-processing-results
                                                             collect (second (multiple-value-list
                                                                              (connected-syntactic-structure
                                                                               (left-pole-structure
                                                                                (car-resulting-cfs (cipn-car (pr-parsing-final-node pr))))
                                                                               :grammar-hierarchy-features
                                                                               (hierarchy-features (construction-inventory (pr-parsing-final-node pr)))))))
        for applied-cxns = (applied-constructions (pr-parsing-final-node processing-result))
        for nr-of-words = (length (split-sequence::split-sequence #\Space (pr-original-sentence processing-result)))
        for list-with-nrs-of-branching-factors = (loop for pr in sentence-processing-results
                                                       collect (average-branching-factor (pr-parsing-final-node pr)))
        for list-with-smatch-scores = (when (pr-gold-standard-meaning processing-result)
                                        (loop for pr in sentence-processing-results
                                              collect (smatch-score (pr-parsed-meaning pr) (pr-gold-standard-meaning processing-result))))
        do
        (notify interaction-finished t t 1)
        (notify nr-of-meaning-chunks-recorded (average list-with-nrs-of-chunks/processing-result))
        (notify nr-of-syntactic-trees-recorded (average list-with-nrs-of-trees/processing-result))
        (notify nr-of-words-recorded nr-of-words)
        (notify cxns-comprehension-recorded applied-cxns)
        (notify branching-factor-comprehension-recorded (average list-with-nrs-of-branching-factors))
        collect
        (make-sentence-profile
         :unprocessed-sentence (pr-original-sentence processing-result)
         :sentence plain-sentence
         ;; Comprehension evaluation criteria:
         ;;----------------------------------
         :parsed-meaning (pr-parsed-meaning processing-result) ;;parsed meaning of first series
         :nr-of-meaning-chunks (list (average list-with-nrs-of-chunks/processing-result)
                                     (stdev list-with-nrs-of-chunks/processing-result))
         :nr-of-syntactic-trees (list (average list-with-nrs-of-trees/processing-result)
                                      (stdev list-with-nrs-of-trees/processing-result))
         :nr-of-words nr-of-words
         :cxns-comprehension (mapcar #'name applied-cxns)
         :average-branching-factor-comprehension (list (average list-with-nrs-of-branching-factors)
                                                       (stdev list-with-nrs-of-branching-factors))
         :gold-standard-meaning (pr-gold-standard-meaning processing-result) ;;can be a list in case of comprehend-all
         :smatch-score (when list-with-smatch-scores (list (average list-with-smatch-scores)
                                                           (stdev list-with-smatch-scores)))
         ;;Reformulation evaluation criteria:
         ;;----------------------------------
         :formulated-utterance (pr-formulated-sentence processing-result) ;;return more than one?
         :cxns-formulation (when (pr-formulated-sentence processing-result)
                             (mapcar #'name (applied-constructions (pr-production-final-node processing-result))))
         :average-branching-factor-formulation (when (pr-formulated-sentence processing-result)
                                                 (let ((list-with-nrs-of-branching-factors
                                                        (loop for pr in sentence-processing-results
                                                              collect (average-branching-factor (pr-production-final-node pr)))))
                                                   (list (average list-with-nrs-of-branching-factors)
                                                         (stdev list-with-nrs-of-branching-factors))))
         :longest-common-substring (when (pr-formulated-sentence processing-result)
                                     (let ((list-with-nrs-of-lcs-scores
                                            (loop for pr in sentence-processing-results
                                                  collect (read-from-string
                                                           (first (longest-common-substring plain-sentence
                                                                                    (pr-formulated-sentence pr)))))))
                                       (list (average list-with-nrs-of-lcs-scores)
                                             (stdev list-with-nrs-of-lcs-scores))))
         :word-level-edit-distance (when (pr-formulated-sentence processing-result)
                                     (let ((list-with-nrs-of-wle-scores
                                            (loop for pr in sentence-processing-results
                                                  collect (word-level-edit-distance plain-sentence
                                                                                    (pr-formulated-sentence pr)))))
                                       (list (average list-with-nrs-of-wle-scores)
                                             (stdev list-with-nrs-of-wle-scores))))
         :bi-directionalp (when (= 0.0 (word-level-edit-distance plain-sentence
                                                                 (pr-formulated-sentence processing-result)))
                            t))))
  
(defun create-sentence-profiles-for-production (processing-results &key)                 
  "Evaluate a list of sentence processing results based on a number of
criteria such as average branching factor, smatch score, nr of
semantic chunks, word level edit distance, etc. If gold standard
sentences are provided, the formulated utterances can be compared to the gold
standard sentences"

  (loop for sentence-processing-results in processing-results
        for processing-result = (random-elt sentence-processing-results) ;;exemplar result
        collect
        (make-sentence-profile
         :meaning (pr-original-meaning processing-result)
         ;;Formulation evaluation criteria:
         ;;----------------------------------
         :formulated-utterance (pr-formulated-sentence processing-result) ;;exemplar sentence of first result
         :cxns-formulation (mapcar #'name (applied-constructions (pr-production-final-node processing-result)))
         :average-branching-factor-formulation (let ((list-with-nrs-of-branching-factors
                                                      (loop for pr in sentence-processing-results
                                                            collect (average-branching-factor (pr-production-final-node pr)))))
                                                 (list (average list-with-nrs-of-branching-factors)
                                                       (stdev list-with-nrs-of-branching-factors)))
         :gold-standard-sentence (pr-gold-standard-sentence processing-result)
         :longest-common-substring (when (pr-gold-standard-sentence processing-result)
                                     (let ((list-with-nrs-of-lcs-scores
                                            (loop for pr in sentence-processing-results
                                                  collect (longest-common-substring (pr-gold-standard-sentence processing-result)
                                                                                    (pr-formulated-sentence pr)))))
                                       (list (average list-with-nrs-of-lcs-scores)
                                             (stdev list-with-nrs-of-lcs-scores))))
         :word-level-edit-distance (when (pr-gold-standard-sentence processing-result)
                                     (let ((list-with-nrs-of-wle-scores
                                            (loop for pr in sentence-processing-results
                                                  collect (word-level-edit-distance
                                                           (pr-gold-standard-sentence processing-result)
                                                           (pr-formulated-sentence pr)))))
                                       (list (average list-with-nrs-of-wle-scores)
                                             (stdev list-with-nrs-of-wle-scores))))
         ;;Recomprehension evaluation criteria:
         ;;--------------------------------------
         :parsed-meaning (pr-parsed-meaning processing-result)
         :bi-directionalp (when (unify (cons '== (pr-parsed-meaning processing-result))
                                       (pr-original-meaning processing-result))
                            t)
         :nr-of-meaning-chunks (when (pr-parsed-meaning processing-result)
                                 (let ((list-with-nrs-of-chunks/processing-result
                                        (loop for pr in sentence-processing-results
                                              collect (second (multiple-value-list
                                                               (connected-semantic-network (pr-parsed-meaning pr)))))))
                                   (list (average list-with-nrs-of-chunks/processing-result)
                                         (stdev list-with-nrs-of-chunks/processing-result))))
         :cxns-comprehension (when (pr-parsed-meaning processing-result)
                               (mapcar #'name (applied-constructions (pr-parsing-final-node processing-result))))
         :average-branching-factor-comprehension (when (pr-parsed-meaning processing-result)
                                                   (let ((list-with-nrs-of-branching-factors
                                                          (loop for pr in sentence-processing-results
                                                                collect (average-branching-factor (pr-parsing-final-node pr)))))
                                                     (list (average list-with-nrs-of-branching-factors)
                                                           (stdev list-with-nrs-of-branching-factors))))
         :smatch-score (when (pr-parsed-meaning processing-result)
                         (let ((list-with-smatch-scores
                                (loop for pr in sentence-processing-results
                                      collect (smatch-score (pr-parsed-meaning pr)
                                                            (pr-original-meaning processing-result)))))
                           (list (average list-with-smatch-scores)
                                 (stdev list-with-smatch-scores)))))))

;;##############################
;; TEST SET EVALUATION CRITERIA
;;##############################

(defun overall-average-branching-factor-comprehension (sentence-profiles)
  "The average and standard deviation of the average branching factor over all sentences in the test set (in comprehension)."
  (let ((average-branching-factors (mapcar #'first (mapcar #'sp-average-branching-factor-comprehension sentence-profiles))))
    (when (first average-branching-factors)
      (values (average average-branching-factors)
              (if (> (length average-branching-factors) 1)
                (stdev average-branching-factors) 0)))))

(defun overall-smatch-scores (sentence-profiles)
  "The average and standard deviation of the average branching factor over all sentences in the test set (in comprehension)."
  (let ((average-smatch-scores (remove nil (mapcar #'first (mapcar #'sp-smatch-score sentence-profiles))))) ;;or average?
    (values (average average-smatch-scores)
            (if (> (length average-smatch-scores) 1)
              (stdev average-smatch-scores) 0))))

(defun overall-reversibility (sentence-profiles)
  "The average and standard deviation of the average branching factor over all sentences in the test set (in comprehension)."
  (let ((reversibility (mapcar #'sp-bi-directionalp sentence-profiles)))
    (length (find-all t reversibility))))

(defun overall-average-branching-factor-formulation (sentence-profiles)
  "The average average branching factor over all sentences in the test set (in formulation)."
  (let ((average-branching-factors (mapcar #'first (mapcar #'sp-average-branching-factor-formulation sentence-profiles))))
    (when (first average-branching-factors)
      (values (average average-branching-factors)
              (if (> (length average-branching-factors) 1)
                (stdev average-branching-factors) 0)))))

(defun overall-integrated-meaning-network-ratio (sentence-profiles)
  "How many percent of the total number of sentences has a fully integrated meaning network after parsing."
  (let* ((nr-of-meaning-chunks-per-sentence (mapcar #'first (mapcar #'sp-nr-of-meaning-chunks sentence-profiles)))
         (nr-of-single-chunked-sentences (count-if #'(lambda (nb) (= nb 1)) nr-of-meaning-chunks-per-sentence)))
    `(/ ,nr-of-single-chunked-sentences
        ,(length sentence-profiles))))

(defun overall-word-level-edit-distance (sentence-profiles)
  "The average word level edit distance over all sentences in the test set."
  (let ((word-level-edit-distances (mapcar #'first (mapcar #'sp-word-level-edit-distance sentence-profiles))))
    (when (first word-level-edit-distances)
      (values (average word-level-edit-distances)
              (if (> (length word-level-edit-distances) 1)
                (stdev word-level-edit-distances) 0)))))
  
;;##############################
;; TEST SET PROFILING
;;##############################

(defun number-of-sentences (sentence-profiles)
  (length sentence-profiles))

(defun average-sentence-length (sentence-profiles)
  (let ((sentences (mapcar #'(lambda (sp)
                               (let ((sentence (sp-sentence sp)))
                                 (if (listp sentence)
                                   sentence
                                   (remove-if #'(lambda (word)
                                                  (or (string= word " ")
                                                      (string= word ".")))
                                              (split-sequence::split-sequence #\Space sentence)))))
                                  sentence-profiles)))
    (values 
     (average (mapcar #'length sentences))
     (stdev (mapcar #'length sentences)))))


;;##############################
;; PRINTING
;;##############################


(defun print-evaluation-report (sentence-profiles &key test-set-name series grammar
                                                  production comprehension bi-directional? time-in-seconds)
  "Print evaluation to output buffer."
  
  (format t "~% ~% +++++++++++++++++++++++++++++++++++ ~% Evaluation report ~% +++++++++++++++++++++++++++++++++++ ~% ~%")
  
  (format t "Evaluated on test set:  ~a ~%" (upcase test-set-name))
  (when time-in-seconds
    (multiple-value-bind (hrs mins secs)
        (seconds-to-hours-minutes-seconds time-in-seconds)
      (format t "in ~a hrs, ~a mins, ~a secs ~%" hrs mins secs)))
  
  (if comprehension
    (progn
      (format t "Number of sentences: ~d  ~%" (length sentence-profiles))
      (multiple-value-bind (average sd)
          (average-sentence-length sentence-profiles)
        (format t "Average sentence length: ~$ (sd: ~$) ~%" average sd)))
    (format t "Number of meanings: ~d  ~%" (length sentence-profiles)))

  (format t "~% Evaluated with grammar: ~a ~%" (name grammar))

  (format t "Number of series: ~d  ~% ~%" series)

  (when comprehension
    (format t "[   Utterance >> Meaning   ]  ~%")
    (format t "----------------------------- ~%")
    (format t "Semantic coherence ratio: ~d/~d (~$) ~%"
            (second (overall-integrated-meaning-network-ratio sentence-profiles))
            (third (overall-integrated-meaning-network-ratio sentence-profiles))
            (eval (overall-integrated-meaning-network-ratio sentence-profiles)))
    (multiple-value-bind (average sd)
        (overall-average-branching-factor-comprehension sentence-profiles)
      (format t "Average branching factor (comprehension): ~$ (sd: ~$) ~%" average sd))
    
    (multiple-value-bind (average sd)
        (overall-smatch-scores sentence-profiles)
      (format t "Average Smatch Score (gold standard): ~$ (sd: ~$)  ~% ~%" (if (> average 0.0) average 'N/A)
              (if (> average 0.0) sd 'N/A)))
         
    (when bi-directional?
      (format t "[   Parsed meaning >> Utterance   ]  ~%")
      (format t "----------------------------------- ~%")
      (format t "Reversibility ratio: ~d/~d (~$) ~%"
              (overall-reversibility sentence-profiles)
              (length sentence-profiles)
              (/ (overall-reversibility sentence-profiles) (length sentence-profiles)))
           
      (when (overall-average-branching-factor-formulation sentence-profiles)
        (multiple-value-bind (average sd)
            (overall-average-branching-factor-formulation sentence-profiles)
          (format t "Average branching factor (reformulation): ~$ (sd: ~$) ~%" average sd)))
      (when (overall-word-level-edit-distance sentence-profiles)
        (multiple-value-bind (average sd)
            (overall-word-level-edit-distance sentence-profiles)
          (format t "Average word level edit distance (input vs. formulated utterance):  ~$ (sd: ~$) ~%" average sd)))))
  
  (when production
    (format t "[   Meaning >> Utterance   ]  ~%")
    (format t "----------------------------- ~%")
    (multiple-value-bind (average sd)
        (overall-average-branching-factor-formulation sentence-profiles)
      (format t "Average branching factor (formulation): ~$ (sd: ~$) ~%" average sd))
    (when (overall-word-level-edit-distance sentence-profiles)
      (multiple-value-bind (average sd)
          (overall-word-level-edit-distance sentence-profiles)
        (format t "Average word level edit distance (gold standard):  ~$ (sd: ~$) ~% ~%" average sd)))
         
    (when bi-directional?
      (format t "[   Formulated utterance >> Meaning   ]  ~%")
      (format t "--------------------------------------- ~%")
      (format t "Reversibility ratio: ~d/~d (~$) ~%"
              (overall-reversibility sentence-profiles)
              (length sentence-profiles)
              (/ (overall-reversibility sentence-profiles) (length sentence-profiles)))
      (format t "Semantic coherence ratio: ~d/~d (~$) ~%"
              (second (overall-integrated-meaning-network-ratio sentence-profiles))
              (third (overall-integrated-meaning-network-ratio sentence-profiles))
              (eval (overall-integrated-meaning-network-ratio sentence-profiles)))
      (multiple-value-bind (average sd)
          (overall-average-branching-factor-comprehension sentence-profiles)
        (format t "Average branching factor (recomprehension): ~$ (sd: ~$) ~%" average sd))
      (multiple-value-bind (average sd)
          (overall-smatch-scores sentence-profiles)
        (format t "Average Smatch Score (input vs. parsed meaning): ~$ (sd: ~$)  ~%" average sd))))
         
  (format t "~% +++++++++++++++++++++++++++++++++++ ~%"))

(defun write-evaluation-results (sentence-profiles
                                 &key output
                                 ;;extra files for bidirectional sentences and meanings
                                 ;; (for fcg-interactive example sentences)
                                 (write-bidirectional-sentences nil)
                                 (write-bidirectional-meanings nil))
  "Write evaluation results to csv file." ;;TO DO: make this function more modular! Now it works best in comprehension
  (setf output (or output
                   (monitors::make-file-name-with-time
                    (babel-pathname :directory '(".tmp")
                                    :name "evaluation-summary"
                                    :type "csv"))))
  (with-open-file (evaluation-results output :direction :output :if-exists :overwrite :if-does-not-exist :create)
    ;; a. write header
    (format evaluation-results
            "sentence;words;parsed-meaning;nr-of-meaning-chunks(average);nr-of-meaning-chunks(sd);gold-standard-meaning;smatch-score(mean);smatch-score(sd);cxns-comprehension;branching-factor-comprehension(mean);branching-factor-comprehension(sd);formulated-utterance;bi-directional?;cxns-formulation;branching-factor-formulation(mean);branching-factor-formulation(sd);lcs(mean);lcs(sd);edit-distance(mean);edit-distance(sd); ~%")
    ;; b. write sentence evaluation result
    (loop for profile in sentence-profiles
          do
          (format evaluation-results
                  "~a ; ~d ; ~a ; ~d ; ~d ; ~a ; ~$ ; ~$; ~{~a ~} ;  ~$ ; ~$ ;~a ; ~a ; ~{~a ~} ;  ~$ ;  ~$ ;  ~$ ;  ~$ ; ~d ; ~$ ; ~%"
                  (sp-sentence profile)
                  (sp-nr-of-words profile)
                  (sp-parsed-meaning profile)
                  (first (sp-nr-of-meaning-chunks profile))
                  (second (sp-nr-of-meaning-chunks profile))
                  (sp-gold-standard-meaning profile)
                  (first (sp-smatch-score profile))
                  (second (sp-smatch-score profile))
                  (sp-cxns-comprehension profile)
                  (first (sp-average-branching-factor-comprehension profile))
                  (second (sp-average-branching-factor-comprehension profile))
                  (sp-formulated-utterance profile)
                  (sp-bi-directionalp profile)
                  (sp-cxns-formulation profile)
                  (first (sp-average-branching-factor-formulation profile))
                  (second (sp-average-branching-factor-formulation profile))
                  (first (sp-longest-common-substring profile))
                  (second (sp-longest-common-substring profile))
                  (first (sp-word-level-edit-distance profile))
                  (second (sp-word-level-edit-distance profile)))))

  ;; 4. Write bi-directional sentences and meanings to an external file (for FCG-interactive)
  ;;----------------------------------------------------------------------------------------
  (when write-bidirectional-sentences
    (unless (pathnamep write-bidirectional-sentences)
      (setf write-bidirectional-sentences (babel-pathname :directory '(".tmp") :name "example-sentences" :type "txt")))
    (with-open-file (sentences write-bidirectional-sentences :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (loop for profile in sentence-profiles
            when (sp-bi-directionalp profile)
            do (format sentences "~a~%" (sp-unprocessed-sentence profile))))) ;;actually we want the original sentence here

  (when write-bidirectional-meanings
    (unless (pathnamep write-bidirectional-meanings)
      (setf write-bidirectional-meanings (babel-pathname :directory '(".tmp") :name "example-meanings" :type "txt")))
    (with-open-file (meanings write-bidirectional-meanings :direction :output :if-exists :overwrite :if-does-not-exist :create)
      (loop for profile in sentence-profiles
            when (sp-bi-directionalp profile)
            do (format meanings "~a~%" (sp-meaning profile))))))

;;#####################
;; Evaluate grammar
;;#####################

(defun sentence-length (sentence)
  (length (split-sequence::split-sequence #\Space sentence)))


(defun evaluate-sentence-for-comprehension (sentence grammar &key (meaning nil) (series 1) (bi-directional? nil) (silent nil))

  (loop for i from 1 to series ;;iterate process 
        collect
        (multiple-value-bind (parsed-meaning final-node-parsing)
            (if silent
              (with-disabled-monitor-notifications
                (comprehend sentence :cxn-inventory grammar))
              (comprehend sentence :cxn-inventory grammar))
          (multiple-value-bind (formulated-utterance final-node-production)
              (if bi-directional?
                (if silent
                  (with-disabled-monitor-notifications
                    (formulate (instantiate-variables parsed-meaning) :cxn-inventory grammar))
                  (formulate (instantiate-variables parsed-meaning) :cxn-inventory grammar))
                (values nil nil))
            (if (and bi-directional? (listp formulated-utterance))
              (setf formulated-utterance
                    (format nil "~{~A~^ ~}" formulated-utterance)))
            (format t ".")
            ;(format t "~a (~d words) ~%" sentence (sentence-length sentence))
            (make-sentence-processing-result
             :original-sentence sentence ;(downcase (remove-punctuation sentence)) ;;verwijder leestekens hier?
             :parsed-meaning parsed-meaning
             :gold-standard-meaning meaning
             :parsing-final-node final-node-parsing
             :formulated-sentence (when formulated-utterance
                                    (downcase (remove-punctuation formulated-utterance)))
             :production-final-node final-node-production)))))


(defun evaluate-grammar-for-comprehension (file-with-sentences-optionally-aligned-with-gold-standard-meanings
                                           grammar ;;fcg-constructions
                                           &key (segmentor "<->") ;;segmentor used in input file to split sentences from meanings
                                           max-sentence-length ;;evaluate sentences until length X
                                           (bi-directional? t)
                                           (exclude-sentences-with-single-word t)
                                           (csv-output-file (monitors::make-file-name-with-time
                                                             (babel-pathname :directory '(".tmp")
                                                                             :name "evaluation-summary-comprehension"
                                                                             :type "csv")))
                                           write-bidirectional-sentences ;;write the sentences that are bi-directional to an output file
                                          ; monitors
                                           (series 4))
  "This function is the core of the evaluation procedure. It is
optimized for comprehension and starts from test sentences. It creates
sentences profiles and then writes information in these to a csv file
(optional) and to the output buffer (evaluation report)."
  (format t "~%")
  (unless (probe-file file-with-sentences-optionally-aligned-with-gold-standard-meanings)
    (error "Input file ~a does not exist." file-with-sentences-optionally-aligned-with-gold-standard-meanings))
  
  (let* ((init-time (get-universal-time))
         (sentence-profiles
          (create-sentence-profiles-for-comprehension
           (run-comprehension-and-reformulation-on-test-set
            file-with-sentences-optionally-aligned-with-gold-standard-meanings grammar
            :bi-directional? bi-directional? :exclude-sentences-with-single-word exclude-sentences-with-single-word
            :max-sentence-length max-sentence-length :series series :segmentor segmentor))))
    (assert sentence-profiles)
    
    ;;print to buffer
    (when csv-output-file
      (write-evaluation-results
       sentence-profiles :output csv-output-file
       :write-bidirectional-sentences write-bidirectional-sentences))
    
    ;;for monitoring:
    (notify batch-finished "")
    (notify reset-monitors)
    
    (print-evaluation-report sentence-profiles
                             :test-set-name (pathname-name file-with-sentences-optionally-aligned-with-gold-standard-meanings)
                             :series series :grammar grammar :comprehension t :bi-directional? bi-directional?
                             :time-in-seconds (- (get-universal-time) init-time))
     
    'evaluation-finished))

(defun evaluate-sentence-for-production (meaning grammar &key (sentence nil) (series 1) (bi-directional? nil) (silent nil))
 ; (with-disabled-monitors 
  (loop for i from 1 to series ;;iterate process 
        collect
        (multiple-value-bind (formulated-utterance final-node-production)
            (if silent
              (with-disabled-monitors (formulate meaning :cxn-inventory grammar))
              (formulate meaning :cxn-inventory grammar))
          (multiple-value-bind (parsed-meaning final-node-parsing)
              (if bi-directional?
                (if silent
                  (with-disabled-monitors (comprehend formulated-utterance :cxn-inventory grammar))
                  (comprehend formulated-utterance :cxn-inventory grammar))
                (values nil nil))
            (format t ".")
            (make-sentence-processing-result
             :original-meaning meaning ;(downcase (remove-punctuation sentence)) ;;verwijder leestekens hier?
             :formulated-sentence formulated-utterance
             :gold-standard-sentence sentence
             :production-final-node final-node-production
             :parsed-meaning parsed-meaning 
             :parsing-final-node final-node-parsing)))))

(defun evaluate-grammar-for-production (file-with-sentences-aligned-with-gold-standard-meanings
                                        grammar ;;fcg-constructions
                                        &key max-meaning-size
                                        (bi-directional? t) (segmentor "<->")
                                        (csv-output-file (monitors::make-file-name-with-time
                                                          (babel-pathname :directory '(".tmp")
                                                                          :name "evaluation-summary-production"
                                                                          :type "csv")))
                                        write-bidirectional-meanings
                                        (series 4))
  "This function is the core of the evaluation procedure. It is
optimized for production and starts from test meanings It creates
sentences profiles and then writes information in these to a csv file
(optional) and to the output buffer (evaluation report)."
  (format t "~%")
  (unless (probe-file file-with-sentences-aligned-with-gold-standard-meanings)
    (error "Input file ~a does not exist." file-with-sentences-aligned-with-gold-standard-meanings))
  
  (let* ((init-time (get-universal-time))
         (processing-results
          (run-formulation-and-recomprehension-on-test-set
           file-with-sentences-aligned-with-gold-standard-meanings grammar
           :bi-directional? bi-directional? :series series :segmentor segmentor :max-meaning-size max-meaning-size))
         (sentence-profiles (create-sentence-profiles-for-production processing-results)))

    (when csv-output-file
      (write-evaluation-results sentence-profiles :output csv-output-file
                                :write-bidirectional-meanings write-bidirectional-meanings))
    ;;print to buffer
    (print-evaluation-report sentence-profiles :test-set-name (pathname-name file-with-sentences-aligned-with-gold-standard-meanings)
                             :series series :grammar grammar :production t :bi-directional? bi-directional?
                             :time-in-seconds (- (get-universal-time) init-time))
     
    'evaluation-finished))
      
(defun full-grammar-evaluation (file-with-sentences-aligned-with-gold-standard-meanings grammar 
                                &key  (series 4))
  "Run evaluation in both directions and write results to a csv file
and print the evaluation report in the output buffer."
  ;;to do: optimize evaluate-grammar functions to take direction as keywords
  (evaluate-grammar-for-production file-with-sentences-aligned-with-gold-standard-meanings grammar :series series)
  (evaluate-grammar-for-comprehension file-with-sentences-aligned-with-gold-standard-meanings grammar :series series))

(defun make-gold-standard-meaning-file (input-file-with-sentences grammar &optional output-file-sentences-meanings)
  "from an input file with sentences, make one file with sentence;meaning, using comprehend"
  (let ((sentences nil)
        (meanings nil)
        (output-file-sentences-meanings (or output-file-sentences-meanings
                                             (babel-pathname :directory '(".tmp")
                                                             :name "evaluation-sentences-meanings"
                                                             :type "csv"))))
    ;; Check wheter input-file exists
    (unless (probe-file input-file-with-sentences)
      (error (format nil "The following input-file could not be found: ~a" input-file-with-sentences)))
    ;; Read sentences from input-file and store them in sentences
    (with-open-file (in-stream input-file-with-sentences :direction :input)
      (loop for sentence = (read-line in-stream nil nil)
            while sentence
            do (push sentence sentences)))
    ;; comprehend sentences and push meanings to meanings
    (dolist (sentence sentences)
      (let ((meaning
             (instantiate-variables (with-disabled-monitor-notifications (comprehend sentence :cxn-inventory grammar)))))
        (push meaning meanings)))
    ;; Write to output files
    (with-open-file (out-stream output-file-sentences-meanings :direction :output :if-exists :supersede)
      (loop for sentence in (reverse sentences)
            for meaning in meanings
            do (format out-stream "~a<->~a~%" sentence meaning))) ;;use <-> to separate sentences from meanings
    (warn "Always verify your meanings before evaluating!! Otherwise you're potentially committing a serious scientific crime!")
    output-file-sentences-meanings))


