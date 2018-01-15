(in-package :fcg)
(export '(test-fcg-example all-form-string-permutations))
(defun test-fcg-example (meanings utterances construction-inventory
                                  &key (direction '<->) (equivalent-utterance-fn #'equal))
  "Tests whether all produced utterances for each meaning is a member
of the expected utterances and whether all parses of each utterance
correspond to one of the expected meanings."
  ;; possibly correct values of arguments
  (unless (listp (first utterances)) (setf utterances (list utterances)))
  (unless (listp (first (first meanings))) (setf meanings (list meanings)))
  ;; tests production
  (when (or (eq direction '<->) (eq direction '->))
    (loop for meaning in meanings
          do (loop with produced-utterances = (produce-all meaning construction-inventory)
                   for produced-utterance in produced-utterances
                   do (test-assert (find produced-utterance utterances :test equivalent-utterance-fn)
                                   "Unexpected utterance ~a ~%   when producing ~a; ~%   expected utterances ~a"
                                   produced-utterance meaning utterances)
                   finally (test-assert produced-utterances
                                        "Could not produce ~a"
                                        meaning))))
  ;; tests interpretation
  (when (or (eq direction '<->) (eq direction '<-))
    (let ((equivalent-meaning-mode (get-configuration construction-inventory :equivalent-meaning-mode)))
      (loop for utterance in utterances
            do (loop with parsed-meanings = (parse-all utterance construction-inventory)
                     for parsed-meaning in parsed-meanings
                     do (test-assert (find parsed-meaning meanings
                                           :test #'(lambda (parsed-meaning meaning)
                                                     (equivalent-meaning? parsed-meaning meaning
                                                                          equivalent-meaning-mode)))
                                     "Unexpected meaning ~a ~%   when parsing ~a; ~&   expected meanings ~a"
                                     parsed-meaning utterance meanings)
                     finally (test-assert parsed-meanings
                                          "Could not parse ~a"
                                          utterance))))))

(defun test-utterance (utterance construction-inventory
                                 &key (nr-of-expected-meanings 1) additional-expected-utterances)
  "Tests whether the production of all parsings of an utterance
correspond to the original utterance."
  (let* ((meanings (parse-all utterance construction-inventory))
         (nr-of-meanings (length meanings)))
    (test-assert (eq (length meanings) nr-of-expected-meanings)
                 "Unexpected number of parses for ~a: ~%  got ~a, expected ~a"
                 utterance nr-of-meanings nr-of-expected-meanings)
    (test-fcg-example (mapcar #'instantiate-expression meanings)
                      (append (list utterance) additional-expected-utterances)
                      construction-inventory
                      :direction '->)))

(defun all-form-string-permutations (l)
  "Computes all flattened permutations of lists of strings.
    Example:
     ( (he goes) (to bed) )
     -> ( (he goes to bed) (to bed he goes) )"
    (mapcar #'(lambda (x) (flatten x)) 
	  (permutations-of-length l (length l))))