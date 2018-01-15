(in-package :nlp-tools)

(export '(proper-noun-p
          decapitalize-string))

(defun proper-noun-p (word &optional (language-model "en"))
  "Calls Penelope POS tagger to verify if the word is a proper noun."
  (let ((pos-tag
         (rest (assoc :tag (first (run-penelope-pos-tagger word :model language-model))))))
    (when (member pos-tag '("NNP" "NNPS") :test #'equalp)
      t)))

(defun decapitalize-string (sentence &key (language-model "en"))
  "Decapitalize a the first word of a string. Takes into account proper nouns in the first position of the sentence (does not decapitalize them)."
  (if (or
       (lower-case-p (char sentence  0)) ;;first word is lowercase
       (proper-noun-p (first-word sentence) language-model) ;;first word is a proper noun
       (equalp (first-word sentence) "I"))
    sentence ;;keep as it is
    (replace sentence (downcase sentence) :start1 0 :end1 1 :start2 0 :end2 1)))
