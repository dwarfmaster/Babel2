
(in-package :fcg)

(defparameter *allowed-string-chars*
  '(#\- #\' #\{ #\} #\|))

;;;;; ####################################################################################################
;;;;; NEW FUNCTIONS (TO BE ADDED TO THE SYSTEMS FOLDER)
;;;;; ####################################################################################################

;;;;; SUPPORTING FUNCTIONS & MACROS
;;;;; -------------------------------------------------------------------------------------------------
(defmacro apply-when-t (fn arg)
  "Apply a function if the argument is not nil."
  `(when ,arg
     (apply ,fn ,arg)))

(defun extract-strings-from-simple-vector (svector &optional result)
  (dotimes (i (array-total-size svector))
    (push (svref svector i) result))
  (reverse result))

(defun find-matching-strings-in-source (match-lst source)
  (multiple-value-bind (matching-string simple-vector-with-strings)
      (cl-ppcre:scan-to-strings (apply #'string-append match-lst) source)
    (declare (ignorable matching-string))
    (when simple-vector-with-strings
      (extract-strings-from-simple-vector simple-vector-with-strings))))

(defun remove-round-brackets (string)
  "Remove the round brackets from a string."
  (remove-if #'(lambda(char)
                 (member char '(#\( #\)) :test #'char=))
             string))
;; FCG> (remove-round-brackets "t((e))st")
;; "test"


;; Why not just use (get-base-name variable :remove-question-mark nil) ????
(defun base-var-name (variable)
  "Return the symbol-name of a symbol without the final number."
  (let ((result (reverse (coerce (symbol-name variable) 'list))))
    (loop for char in result
          do (if (char= char #\-)
               (return t)
               (pop result)))
    (if (null result)
      (symbol-name variable)
      (coerce (reverse (rest result)) 'string))))
;; FCG> (base-var-name '?test-x-1)
;; "?TEST-X"

(defun make-overwriting-var ()
  "Make a variable recognizable for overwriting."
  (make-var "overwrite"))
;; FCG> (make-overwriting-var)
;; #:?OVERWRITE-70

(defun overwriting-var? (variable)
  "Check whether a variable is an overwriting-variable."
  (string= "?OVERWRITE" (base-var-name variable)))
;; FCG> (overwriting-var? (make-overwriting-var))
;; T
;; FCG> (overwriting-var? '?test)
;; NIL

(defun collect-overwrite-sublist (bindings)
  "Collect all the overwrite-statements from a list of bindings."
  (loop for binding in bindings
        when (overwriting-var? (first binding))
        collect `(,(second binding) . ,(third binding))))

(defun apply-string-changes (structure bindings)
  "Executes the overwrite-statements found in the bindings."
  (sublis (collect-overwrite-sublist bindings) structure :test #'equal))

(defun unpack-character-changes (the-string)
  "Returns three values if character changes are present, NIL otherwise."
  (when (search "{" the-string)
    (labels ((return-four-values (string &optional (left-pattern "") (right-pattern "")
                                         (left-split-sequence nil) (right-split-sequence nil))
               (if (or (null string) (string= "" string))
                 (values left-pattern right-pattern
                         (reverse left-split-sequence) (reverse right-split-sequence))
                 (let* ((pos-1 (search "{" string))
                        (pos-2 (search "|" string))
                        (pos-3 (search "}" string))
                        (left-str (when pos-1 (subseq string (1+ pos-1) pos-2)))
                        (right-str (when pos-1 (subseq string (1+ pos-2) pos-3)))
                        (b-left-str (when left-str (string-append "(" left-str ")")))
                        (b-right-str (when right-str (string-append "(" right-str ")"))))
                   (cond ((null pos-1)
                          (let ((last-split (string-append "(" string ")")))
                            (values (string-append left-pattern string)
                                    (string-append right-pattern string)
                                    (reverse (cons last-split left-split-sequence))
                                    (reverse (cons last-split right-split-sequence)))))
                         ((= 0 pos-1)
                          (return-four-values (subseq string (1+ pos-3))
                                              (string-append left-pattern left-str)
                                              (string-append right-pattern right-str)
                                              (cons b-left-str left-split-sequence)
                                              (cons b-right-str right-split-sequence)))
                         (t
                          (let* ((str (subseq string 0 pos-1))
                                 (b-str (string-append "(" str ")")))
                            (return-four-values (subseq string (1+ pos-3))
                                                (string-append left-pattern str left-str)
                                                (string-append right-pattern str right-str)
                                                (cons b-left-str (cons b-str left-split-sequence))
                                                (cons b-right-str (cons b-str right-split-sequence))))))))))
      (return-four-values the-string))))
;; Examples:
;; FCG 75 > (unpack-character-changes "sing")
;; NIL
;; FCG 80 > (unpack-character-changes "s{i|a}ng")
;; "sing"
;; "sang"
;; ("(s)" "(i)" "(ng)")
;; ("(s)" "(a)" "(ng)")
;; FCG 84 > (unpack-character-changes "h{ie|<?a>}bb{a|o}n")
;; "hiebban"
;; "h<?a>bbon"
;; ("(h)" "(ie)" "(bb)" "(a)" "(n)")
;; ("(h)" "(<?a>)" "(bb)" "(o)" "(n)")

(defun split-variables-and-string (string)
  "Splits a string into substrings whenever a variable is encountered."
  (if (or (null string)
          (string= "" string))
    nil
    (let ((pos-1 (search "<" string))
          (pos-2 (search ">" string)))
      (cond
       ((null pos-1) (list string))
       ((= 0 pos-1) (cons (upcase (subseq string 1 pos-2))
                          (split-variables-and-string (subseq string (1+ pos-2)))))
       (t
        (cons (subseq string 0 pos-1)
              (cons (upcase (subseq string (1+ pos-1) pos-2))
                    (split-variables-and-string (subseq string (1+ pos-2))))))))))
;; FCG> (split-variables-and-string "<?onset>ing")
;; ("?ONSET" "ing")

(defun substitute-variables-in-string (input-string bindings)
  "Given a single bindings-hypothesis, substitute variables in a string."
  (let* (;; We first split the string into a list of strings.
         (split-string (split-variables-and-string input-string))
         ;; We then try to find substitutions for the variables in the string.
         (split-string-with-substitutions
          (loop for string in split-string
                collect (if (char= (char string 0) #\?) ;; If the string starts with a ?
                          ;; Try to substitute it with another string
                          (or (rest
                               (find-if #'(lambda (binding)
                                            (string= string (base-var-name (first binding))))
                                        bindings))
                              ;; Or substitute it with a regular expression.
                              ".+")
                          ;; Otherwise just return the string
                          string))))
    ;; Finally, return a single string.
    (apply #'string-append split-string-with-substitutions)))

(defun substitute-variables-in-strings-of-list (list-of-strings bindings)
  (loop for string in list-of-strings
        collect (substitute-variables-in-string string bindings)))
;; FCG> (substitute-variables-in-strings-of-list '("test" "(<?a>)") '((?a . "ing")))
;; ("test" "(ing)")


(defun string-replace-first (string old new &optional (result ""))
  "Nondestructively replace the first occurence of a substring with a new substring."
  (cond
   ((or (string= "" string) (> (length old) (length string)))
    (string-append result string))
   ((string= (subseq string 0 (length old)) old)
    (string-append result new (subseq string (length old))))
   (t
    (string-replace-first (subseq string 1) old new (string-append result (subseq string 0 1))))))
;; FCG> (string-replace-first "testtest" "test" "goal")
;; "goaltest"

(defun add-string-boundaries (string)
  "Adds special boundary characters for obtaining exact matches."
  (string-append "^" string "$"))
;; FCG> (add-string-boundaries "string")
;; "^string$"

(defun substitute-bindings-and-strings (bindings x)
  "Not only substitutes bindings in a pattern (x) but also strings."
  (declare (type bindings bindings))
  (labels ((aux (x)
	     (cond ((variable-p x)
                    (let ((y (assoc x bindings :test #'eq)))
                      (if (and y (not (eq (cdr y) x))) (aux (cdr y)) x)))
		   ((stringp x)
		    (let ((y (loop for binding in bindings
                                   when (and (listp (rest binding))
                                             (equal x (second binding)))
                                   do (return (third binding)))))
		      (if y y x)))
		   ((atom x) x)
                   (t (cons (aux (car x)) (aux (cdr x)))))))
    (cond ((eq bindings +fail+) +fail+)
          ((eq bindings +no-bindings+) x)
          (t (aux x)))))

(defun remove-regular-expressions (string)
  "Recursively check for strings and remove reg exp, except substitutions."
  (remove-if-not #'(lambda(char)
		     (or (alphanumericp char)
			 (member char *allowed-string-chars* :test #'char=)))
		 string))

(defun remove-all-regular-expressions (list)
  "Remove regular expressions."
  (cond
    ((null list) nil)
    ((null (listp list)) list)
    ((listp (first list))
     (cons (remove-all-regular-expressions (first list))
	   (remove-all-regular-expressions (rest list))))
    (t
     (cons (if (stringp (first list))
	       (remove-regular-expressions (first list))
	       (first list))
	   (remove-all-regular-expressions (rest list))))))

(defun remove-bracket-expressions (string)
  (let ((left-bracket (search "(" string))
        (right-bracket (search ")" string)))
    (if left-bracket
      (string-append (subseq string 0 left-bracket)
                     (subseq string (+ 1 right-bracket)))
      string)
  ))

(defun split-match-from-merge (string &optional (match-pattern nil) (merge-pattern ""))
  "Gives two values back needed for matching and merging."
  (if (or (null string) (string= "" string))
    (values (reverse match-pattern) merge-pattern)
    (let ((pos-1 (search "(" string))
          (pos-2 (search ")" string)))
      (cond
       ((null pos-1) (values (reverse (cons (string-append "(" string ")") match-pattern))
                             (string-append merge-pattern "(" string ")")))
       ((= 0 pos-1)
        (split-match-from-merge (subseq string (1+ pos-2))
                                match-pattern
                                (string-append merge-pattern (subseq string 1 pos-2))))
       (t
        (split-match-from-merge (subseq string (1+ pos-2))
                                (cons (string-append "(" (subseq string 0 pos-1) ")") match-pattern)
                                (string-append merge-pattern "(" (subseq string 0 pos-1) ")"
                                               (subseq string (1+ pos-1) pos-2))))))))

;;;;; UNIFY
;;;;; -------------------------------------------------------------------------------------------------


(defun unify-substituted-strings (pattern source bindings-list &optional match-pattern merge-pattern
                                          split-match-pattern split-merge-pattern &key cxn-inventory)
  "If the string contains variables, substitute them before unifying."
  (unless match-pattern (setf match-pattern pattern))
  (let (results)
    (dolist (bindings bindings-list)
      (let ((new-match-pattern (substitute-variables-in-string match-pattern bindings))
            (new-merge-pattern (when merge-pattern (substitute-variables-in-string merge-pattern bindings)))
            (new-split-match-pattern (when split-match-pattern
                                       (substitute-variables-in-strings-of-list split-match-pattern bindings)))
            (new-split-merge-pattern (when split-match-pattern
                                       (substitute-variables-in-strings-of-list split-merge-pattern bindings))))
        (setf results (append (unify-strings-with-regular-expressions pattern source (list bindings)
                                                                      new-match-pattern new-merge-pattern
                                                                      (pp new-split-match-pattern) (pp new-split-merge-pattern)
                                                                      :cxn-inventory cxn-inventory)
                              results))))
    results))

(defun unify-strings-with-regular-expressions (pattern source bindings
                                                       &optional match-pattern merge-pattern split-match-pattern split-merge-pattern
                                                       &key cxn-inventory)
                                                       
  "Unification of strings with special characters."
  (unless match-pattern (setf match-pattern pattern)) ;; If there are no substitutions required, just take the pattern.
  ;; If there are variables, they need to be replaced:
  (if (search "?" match-pattern)
    (unify-substituted-strings pattern source bindings match-pattern merge-pattern split-match-pattern split-merge-pattern
                               :cxn-inventory cxn-inventory)
    ;; Otherwise, just try to find a match for the match-pattern:
    (let ((bounded-pattern (add-string-boundaries match-pattern)))
      (multiple-value-bind (successful-match svector-with-strings)
          (cl-ppcre:scan-to-strings bounded-pattern source)
        (when successful-match ;; We found a match
          (if merge-pattern ;; If we need to make substitutions...
            (let* ((matched-strings (extract-strings-from-simple-vector svector-with-strings))
                   (new-string (substitute-matched-strings
                                matched-strings split-match-pattern match-pattern
                                split-merge-pattern merge-pattern 'unify :cxn-inventory cxn-inventory)))
              (unify (list (make-var (gensym)) (make-overwriting-var))
                     (list (list pattern source)
                           (list (get-string-window)
                                 (substitute new-string source (get-string-window) :test #'equal)))
                     bindings))
            ;; Else just unify.
            (unify (make-var (gensym)) (list pattern source) bindings :cxn-inventory cxn-inventory)))))))


(defun alphanumeric-string (string)
  "returns true if string consists of alphanumeric characters only, otherwise nil"
  (loop for char across string
        unless (alphanumericp char)
        do (return-from alphanumeric-string nil)
        finally (return t)))

(defun unify-strings (pattern source bindings &key cxn-inventory)
  "Unify to strings using special characters."
  ;; For optimization, first check whether the source is a variable or whether we have an exact match:
  (cond ((variable-p source)
         (unify source (remove-all-regular-expressions pattern) bindings :cxn-inventory cxn-inventory))
        ((string= pattern source)
         bindings)
        ;; Else we turn to the special characters.
        ((not (alphanumeric-string source))
         ;; Step 1: Check whether the string includes a substitution command.
         (multiple-value-bind (left-pattern right-pattern left-split right-split)
             (unpack-character-changes (remove-round-brackets pattern)) ;; Round brackets are ignored in matching.
           (declare (ignorable left-pattern right-pattern))
           ;; Depending on the direction, we order the obtained values in an argument-specification:
           (let ((args-spec (if (eql (get-current-direction) '->)
                              (list left-split right-split)
                              (list right-split left-split))))
             (unify-strings-with-regular-expressions pattern source bindings
                                                     ;; Now we unpack the argument specification:
                                                     (apply-when-t #'string-append (first args-spec)) ;; match-pattern
                                                     (apply-when-t #'string-append (second args-spec)) ;; merge-pattern
                                                     (first args-spec) ;; split-match-pattern
                                                     (second args-spec)
                                                     :cxn-inventory cxn-inventory)))))) ;; split-merge-pattern


;;; Closure for string-window: these functions will temporarily hold and fetch the
;;; context (or "window") in which a string occurs. This makes it possible to substitute
;;; strings only when they occur within that context, instead of substituting ALL matches
;;; of the string.
;;; (see the use of these functions in #'unify and #'fcg-merge further below.

;;; Someone should get rid of this and properly pass the string-window as an argument.
(let (string-window)
  (defun reset-string-window ()
    (setf string-window nil))
  (defun get-string-window ()
    string-window)
  (defun set-string-window (value)
    "Temporarily store the larger context in which a string occurs."
    (if (stringp (third value))
      (setf string-window value))))

;;;;; MERGE
;;;;; -------------------------------------------------------------------------------------------------
(defun overwrite-substring-in-merge (source-string source-pattern target-pattern)
  (let* ((target-substring (remove-regular-expressions target-pattern))
         (substring-start
          (search (remove-regular-expressions source-pattern)
                  source-string))
         (substring-end
          (+ substring-start
             (length (remove-regular-expressions source-pattern))))
         (first-letters-source
          (subseq source-string 0 substring-start))
         (final-letters-source
          (subseq source-string substring-end))
         (target-string
          (mkstr first-letters-source target-substring final-letters-source)))
    target-string)
  ;;return target-string
  )

(defun substitute-matched-strings (matched-strings
                                   match-list-1 merge-pattern-1
                                   &optional match-list-2 merge-pattern-2
                                   (operation 'merge)
                                   &key cxn-inventory)
  (declare (ignore cxn-inventory))
  
  (unless match-list-2
    (setf match-list-2 match-list-1
          merge-pattern-2 merge-pattern-1))

  (labels ((do-the-substitution (strings ml-1 mp-1 ml-2 mp-2)
             
             (if (null ml-2)
               (remove-if #'(lambda(str)
                              (member str '(#\( #\)) :test #'string=))
                          mp-2)
               (if (and (search (remove-regular-expressions mp-1)
                                (first strings)) ;;mp2 contains regular expression such as ".+"
                        (eq operation 'merge))
                 (overwrite-substring-in-merge (first strings)
                                               mp-1 mp-2)
                 (do-the-substitution (rest strings)
                                      (rest ml-1) mp-1
                                      (rest ml-2)
                                      (if (equal (first ml-1) (first ml-2))
                                        (string-replace-first mp-2 (first ml-2) (first strings))
                                        mp-2))))))
    
    (do-the-substitution matched-strings
                         match-list-1 merge-pattern-1
                         match-list-2 merge-pattern-2)))

(defun merge-strings-with-regular-expressions (pattern source bindings &optional match-pattern merge-pattern &key cxn-inventory)
  (unless match-pattern (setf match-pattern pattern))
  ;; This function is only called when there is a source string. We therefore need to check
  ;; whether the "obligatory" parts of the string (i.e. *NOT* between brackets) can be matched
  ;; against the source string. We also need to take possible variables into account.
  (let ((match-pattern-with-vars-substituted (substitute-variables-in-string match-pattern bindings))
        (merge-pattern-with-vars-substituted
         (when merge-pattern
           (substitute-variables-in-string merge-pattern bindings))))
    ;; Step 1: Extract a list of obligatory strings to be matched, and a more elaborate string for substitution.
    (multiple-value-bind (match-lst merge-pattern-before-substitution)
        (split-match-from-merge match-pattern-with-vars-substituted)
      ;; Step 2: Find a match for every obligatory string:
      (let ((matched-strings (find-matching-strings-in-source match-lst source)))
        ;; This match is successful if we find a match for each one of the strings:
        (when (= (length matched-strings) (length match-lst))
          ;; Step 3: Obtain the string that will be merged in the transient structure, and bindings for overwriting characters.
          (multiple-value-bind (to-be-substituted to-be-merged)
               (split-match-from-merge merge-pattern-with-vars-substituted)
            (let ((new-string (substitute-matched-strings matched-strings
                                                          match-lst merge-pattern-before-substitution
                                                          to-be-substituted to-be-merged))
                  (bsl (if (eql bindings +no-bindings+) +no-bindings+ (list bindings))))
            ;  (pp new-string) (pp merge-pattern-before-substitution)
            ;; Step 4: Return a merge-result if all of the above is successful.
            (list (make-merge-result new-string
                                     (unify (list (make-var (gensym)) (make-overwriting-var))
                                            (list (list pattern source)
                                                  (list (get-string-window)
                                                        (substitute new-string source (get-string-window) :test #'equal)))
                                            bsl
                                            :cxn-inventory cxn-inventory))))))))))

(defun merge-strings (pattern source bindings &key cxn-inventory)
  (cond ((null source)
         (when (search "?" pattern) ;; There should be no variables in the string.
           (warn "You are merging a string with variables with an empty source."))
         (list (make-merge-result (remove-regular-expressions pattern)
                                  (list bindings))))
        ((string= pattern source)
         (list (make-merge-result source (list bindings))))
        (t
         (multiple-value-bind (left-pattern right-pattern)
             (unpack-character-changes pattern) ;; Check for character changes.
           (let ((match-and-merge-patterns (if (eql (get-current-direction) '->)
                                             (list left-pattern right-pattern)
                                             (list right-pattern left-pattern))))
             (merge-strings-with-regular-expressions pattern source bindings
                                                     (first match-and-merge-patterns)
                                                     (second match-and-merge-patterns)
                                                     :cxn-inventory cxn-inventory))))))

;;;;; ####################################################################################################
;;;;; THE FOLLOWING FUNCTIONS FROM SYSTEMS ARE SLIGHTLY MODIFIED
;;;;; ####################################################################################################

;;; Substitute not only variables, but regular expressions as well by checking the binding of
;;; gensym-generated variables.
(defun unify-TAGs (tags y bindings-list &optional (unify-fn #'unify) &key cxn-inventory)
  (do ((remaining (rest tags) (rest (rest remaining))))
      ((or (null remaining)
	   (fail? bindings-list))
       bindings-list)
    (setq bindings-list
	  (loop for bs in (funcall unify-fn (second remaining) y bindings-list :cxn-inventory cxn-inventory)
	     append (unify (first remaining) 
			   (substitute-bindings-and-strings ;; We substitute regular expressions as well.
			    bs
			    (remove-special-operators (second remaining) bs))
			   (list bs)
                           :cxn-inventory cxn-inventory)))))

;;; Unify: add a condition where x is a string.
;;; Keep the larger context in which a string occurs.
(defun unify (x y &optional (bindings-list (list +no-bindings+)) &key cxn-inventory)
  (when bindings-list
    (cond ((stringp x)
	   (unify-strings x y bindings-list :cxn-inventory cxn-inventory))
	  ((and (consp x) (unify-fn (first x)) (not (variable-p y)))
	   (unify-special x y bindings-list :cxn-inventory cxn-inventory))
	  ((and (consp y) (unify-fn (first y)) (not (variable-p x)))
	   (unify-special y x bindings-list :cxn-inventory cxn-inventory))
	  ((and (consp x) (consp y))
           (set-string-window y) ;; Store the context of the string.
	   (loop for bindings in bindings-list append 
		(unify (rest x) (rest y) 
		       (unify (first x) (first y) (list bindings) :cxn-inventory cxn-inventory))))
	  (t (loop for bindings in bindings-list
                for try = (unify-atom x y bindings :cxn-inventory cxn-inventory)
                unless (fail? try) 
                collect try)))))

;;; FCG-MERGE: We add a first condition: if the pattern is a string, then call merge-strings.
;;; Keep the larger context in which a string occurs.
(defun fcg-merge (pattern source bindings &key (cutoff nil) (destructive t) (merge-fn #'fcg-merge) cxn-inventory)
  "The merger will find the minimal expansion of source so that it
unifies with the given pattern."
  (cond ((stringp pattern)
	 (merge-strings pattern source bindings :cxn-inventory cxn-inventory))
	((and (null pattern) (listp source))
	 (list (make-merge-result source
				  (list bindings))))
	((and (null source)
	      (listp pattern))
	 (when (and destructive
		    (or (null cutoff)
			(>= cutoff (length pattern))))
	   ;; when source is empty just return as maximally
	   ;; instantiated pattern without special operators
	   (multiple-value-bind (r new-bindings)
	       (remove-special-operators (substitute-bindings bindings pattern) bindings)
	     (list (make-merge-result r
				      (list new-bindings)
				      r)))))
	((or (variable-p pattern) (variable-p source))
	 (let ((bsl (unify pattern source (list bindings) :cxn-inventory cxn-inventory)))
	   (when bsl 
	     (list (make-merge-result source bsl)))))
        ;; Does this case ever occur??
	((and (consp source) (symbolp (first source)) (merge-fn (first source)))
	 (merge-special source pattern bindings cutoff merge-fn :cxn-inventory cxn-inventory))
	((and (consp pattern) (symbolp (first pattern)) (merge-fn (first pattern)))
	 ;; when there is a special operator let them handle it
	 (merge-special pattern source bindings cutoff merge-fn :cxn-inventory cxn-inventory))
	((or (atom pattern) (atom source))
	 (let ((bsl (unify pattern source (list bindings) :cxn-inventory cxn-inventory)))
	   (when bsl 
	     (list (make-merge-result source bsl)))))
	((and (consp pattern) (consp source))
         (set-string-window source) ;; Keep the larger context of the string.
	 (let ((first-tries
		(if destructive 
                  (funcall merge-fn (first pattern) (first source) bindings :cutoff cutoff :cxn-inventory cxn-inventory)
		    (let ((bsl (unify (first source) (first pattern) (list bindings))))
		      (when bsl (list (make-merge-result (first source) bsl)))))))
	   (cond (first-tries
		  (let ((res nil))
		    (dolist (ft first-tries)
		      (dolist (bs (mr-bsl ft))
			(let ((mrs (funcall merge-fn (rest pattern) (rest source) bs 
					    :cutoff (when cutoff (- cutoff (length (mr-added ft))))
                                            :cxn-inventory cxn-inventory)))
			  (dolist (mr mrs)
			    (setf (mr-expr mr) (cons (mr-expr ft) (mr-expr mr))
				  (mr-added mr) (append (mr-added ft) (mr-added mr)))
			    (let ((prev (find (mr-expr mr) res :key #'mr-expr :test #'equal)))
			      (if prev 
				  (setf (mr-bsl prev) (append (mr-bsl mr)
							      (mr-bsl prev)))
				  (push mr res)))))))
		    res)) ;; This (rest (last pattern)) stuff really
                          ;; has to go (this is the second bug that it
                          ;; has caused)
		 ((and destructive (or (and (rest (last pattern))
                                            (symbolp (rest (last pattern))))                                  
                                       (> (length pattern) (fcg-length source)))
		       (or (not cutoff)
			   (> cutoff 0)))
		  (let ((mrs (funcall merge-fn (rest pattern) source bindings
                                      :cutoff (when cutoff (- cutoff 1))
                                      :cxn-inventory cxn-inventory))
			result)
;;		  (format t "~%mrs=~A" mrs)
		    (dolist (mr mrs)
		      (dolist (bs (mr-bsl mr))
			(multiple-value-bind (first new-bs)
			    (remove-special-operators (substitute-bindings bs (first pattern)) bs)
			  (let ((previous (find-if #'(lambda (mr)
						       (and (consp (mr-expr mr))
							    (equalp first (first (mr-expr mr)))))
						   result)))
			    (if previous 
				(push new-bs (mr-bsl previous))
				(push (make-merge-result (cons first (mr-expr mr)) 
							 (list new-bs)
							 (cons first (mr-added mr)))
				      result))))))
		    result))
		 (t nil))))	
	(t nil)))

;;; The following function "executes" overwrite-variables found in the bindings:
(defun merge-structures-without-Js (pattern source bindings &key cxn-inventory)
  ;;  (format t "~%bs=~A" bindings)
  (make-subset (substitute-bindings bindings (remove-j-units pattern))
               (apply-string-changes source bindings) ;; Apply the string changes in merging.
               bindings
               :test-fn #'(lambda (u1 u2)
                            (unify-simple (unit-name u1) (unit-name u2) bindings :cxn-inventory cxn-inventory))
               :merge-fn #'merge-units))

;;; This function stays entirely the same, except that it removes all regular expressions
;;; if there is new information added ("to-add"):
(defun initial-mss-state (subset superset bindings test-fn merge-fn cutoff destructive
			  &key (remove-special-operators t) cxn-inventory)
  (let ((superset-elts+positions (bag-to-position-list superset))
	(remaining nil)
	(continue t))
    (setq remaining
	  (loop 
	     for subset-elt in subset 
	     while continue 
	     collect
	       (let ((result-for-subset-elt nil)
		     (candidate-elts+positions (if test-fn
						   (find-all subset-elt superset-elts+positions 
							     :test (make-compare-no-tags-fn test-fn) :key #'car)
						   superset-elts+positions)))
		 (dolist (candidate-elt+positions candidate-elts+positions)
		   (let ((mrs (funcall merge-fn subset-elt (car candidate-elt+positions)
                                       bindings :cutoff cutoff
                                       :cxn-inventory cxn-inventory)))
		     (when mrs
;;		       (format t "~%  subset-elt=~A~%  candidate=~A~%  => mrs=~A" subset-elt (car candidate-elt+positions) mrs)
		       (push (list (cdr candidate-elt+positions) mrs (length (mr-added (first mrs))))
			     result-for-subset-elt))))
		 (when (and destructive
			    (or (not test-fn) (null candidate-elts+positions))
			    (or (null cutoff) (> cutoff 1)))
		   (multiple-value-bind (to-add new-bs)
		       (if remove-special-operators
			   (remove-special-operators subset-elt bindings)
			   (values subset-elt bindings))
		     (when new-bs
                       ;; REMI: changed "to-add" in "(remove-all-regular-expressions to-add)"
		       (push (list '(new) (list (make-merge-result (remove-all-regular-expressions to-add) (list new-bs) (list to-add))) 1)
			     result-for-subset-elt))))
		 (setq continue result-for-subset-elt)
		 (cons subset-elt (sort result-for-subset-elt #'< :key #'third)))))
    ;; so for each subset element, remaining has an entry holding a
    ;; sorted list of (positions mrs weight) lists.
    (when continue
      (make-mss-state 
       :mr-so-far (make-merge-result nil (list bindings) nil)
       :remaining remaining
       :g-cost 0 
       :h-cost (reduce #'+ remaining :key #'(lambda (subset-elt-entries) (third (first (cdr subset-elt-entries)))))
       :taken nil))))

;; Here, we apply string changes if the argument unified is T:
(defun merge-structures (pattern source &optional (bindings +no-bindings+) unified &key cxn-inventory)
  ;; non-destructive because of copy-tree
  #+dbg
  (format t "~%++++++++++++++ merge-structures +++++++++++++++
  pattern  = ~A
  source   = ~A
  bindings = ~A
  unified  = ~A" pattern source bindings unified)
;;   (format t "~%(merge-structures-without-Js~%  '~A~%  '~A~%  '~A"
;; 	  pattern source bindings)
  (let ((result nil))
    (dolist (mr (if unified
                  ;; Following line changed by Remi for regular expressions:
                  (list (make-merge-result (apply-string-changes source bindings) (list bindings) nil))
                  (merge-structures-without-Js pattern (copy-tree source) bindings :cxn-inventory cxn-inventory)))
      #+dbg
      (format t "~%mr=~A" mr)
      (dolist (bindings (mr-bsl mr))
	;; added copy-tree because some of the underlying functions (eg. make-child) 
	;; are destructive so if there is more than one bindings the handling of
	;; the j-units might interfere with each other (for example producing
	;; when ball is twice in the meaning)
	#+dbg
	(format t "~%bs=~A" bindings)
	(dolist (fmr (handle-J-units pattern (copy-tree (mr-expr mr)) bindings
				     ;; mr-added is a list of pointers so i only 
				     ;; copy the outer list structure
				     (copy-list (mr-added mr))
                                     :cxn-inventory cxn-inventory))
	  #+dbg
	  (format t "~%fmr=~A" fmr)
	  (let ((prev (find-if #'(lambda (rm) (equal (mr-expr rm)
						     (mr-expr fmr)))
			       ;; not checking the mr-added because equal expects
			       ;; symbol within lists to be eq anyway
			       result)))
	    (if prev 
		(push bindings (mr-bsl prev))
		(push fmr result))))))
    #+dbg
    (format t "~%-------------- merge-structures ---------------")
    (notify merging-finished pattern source result)
    result))