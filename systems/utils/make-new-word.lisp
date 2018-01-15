;;;; functions for inventing new words

(in-package :utils)

(export 'make-new-word)

(defvar *vowels* '("a" "e" "i" "o" "u"))
(defvar *consonants* '("b" "d" "f" "g" "k" "l" "m" "n" "p" "r" "s" "t" "v" "w" "x" "z"))
(defvar *words-so-far* nil)

(defun random-syllable ()
  (format nil "~a~a"
          (random-elt *consonants*)
          (random-elt *vowels*)))

(defun random-word (&optional (nof-syllables 3))
  (format nil "~{~a~}"
          (loop for i from 1 to nof-syllables
                collect (random-elt *consonants*)
                collect (random-elt *vowels*))))

(defun make-new-word (&optional (nof-syllables 3))
  (loop for word = (random-word nof-syllables)
    unless (member word *words-so-far*)
    do (setf *words-so-far* (push word *words-so-far*))
    (return word)))
