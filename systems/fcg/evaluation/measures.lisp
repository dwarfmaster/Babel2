;; Katrien Beuls, September 2016
(in-package :fcg)

;;python smatch.py -m '(BOY ?X)' '(BOY ?X) (UNIQUE ?X)'

(defun smatch-score (L1 L2)
  "Calls the python smatch program which calculates the smatch score
of a parsed meaning network and an AMR gold standard meaning."
  (assert (progn (listp L1) (listp L2)))
  (setf L1 (sort L1 #'string-lessp :key #'first))
  (setf L2 (sort L2 #'string-lessp :key #'first))
  (setf L1 (format nil "~{~a ~}" L1))
  (setf L2 (format nil "~{~a ~}" L2))
  (let* ((program (babel-pathname :directory '("libraries" "smatch")
                                  :name "smatch" :type "py"))
         (stream (pipe-input "python" :args (list program "-m"
                                                  (format nil "~s" L1) (format nil "~s" L2))))
         (output (read-from-string
                  (second (split-sequence:split-sequence ":"  (read-line stream) :test #'string=)))))
   (close stream)
   output))

;(smatch-score '((boy ?x) (paul ?x)) '((boy ?x) (paul ?x)))
;(smatch-score '((paul ?x) (boy ?x) ) '((boy ?x) (paul ?x)))

(defun longest-common-substring (L1 L2)
  "Calculates the longest common substring between two utterances (lists or strings)."
  (when (listp L1)
    (setf L1 (format nil "~{~a ~}" L1)))
  (when (listp L2)
    (setf L2 (format nil "~{~a ~}" L2)))
  (let ((program (babel-pathname :directory '("libraries" "longest-common-substring")
                                 :name "lcs" :type "py")))
    (exec-and-return (format nil "python ~a" program)
                     "-s" (format nil "~s" L1) (format nil "~s" L2))))

;;(longest-common-substring '("i" "gave" you the book) "i gave book you the")

(defun word-level-edit-distance (L1 L2)
  "Calculates the Levenshtein distance between the elements of two lists, returns an editing distance (int)."
  (when (stringp L1)
    (setf L1 (split-sequence:split-sequence #\Space L1 :remove-empty-subseqs t)))
  (when (stringp L2)
    (setf L2 (split-sequence:split-sequence #\Space L2 :remove-empty-subseqs t)))
  
  (let ((n (length L1))
	(m (length L2)))
    ;; Check trivial cases
    (cond ((= 0 n) (return-from word-level-edit-distance m))
	  ((= 0 m) (return-from word-level-edit-distance n)))
    (let ((col (make-array (1+ m) :element-type 'integer))
	  (prev-col (make-array (1+ m) :element-type 'integer)))
      ;; We need to store only two columns---the current one that
      ;; is being built and the previous one
      (dotimes (i (1+ m))
	(setf (svref prev-col i) i))
      ;; Loop across all elements of each list
      (dotimes (i n)
	(setf (svref col 0) (1+ i))
	(dotimes (j m)
	  (setf (svref col (1+ j))
		(min (1+ (svref col j))
		     (1+ (svref prev-col (1+ j)))
		     (+ (svref prev-col j)
			(if (equalp (nth i L1) (nth j L2)) 0 1)))))
	(rotatef col prev-col))
      (svref prev-col m))))


;;(word-level-edit-distance '("hallo" "wereld") '("wereld" "hallo"))
;;(word-level-edit-distance '("i" "gave" "you" "the" "book") '("i" "gave" "you" "the"))
;;(word-level-edit-distance '("i" "gave" "you" "the" "book") '("i" "gave" "book" "you" "the"))


(defun average-branching-factor (cip-solution &key (only-succeeded-nodes nil))
  "Calculate the average branching factor for an FCG search tree. The
average branching factor is defined as the total number of nodes
divided by the depth of the search tree. You can optionally exclude
nodes with a second-merge-failed result."
  (let ((depth (length (all-parents cip-solution)))
        (all-nodes (if only-succeeded-nodes
                     (length (succeeded-nodes (cip cip-solution)))
                     (node-counter (cip cip-solution)))))
    (if (= 0 depth)
      0
      (float (/ all-nodes depth)))))
