
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                    ;;
;; How to Use the Grammar Evaluation Module in FCG                    ;;
;; November 2016 - Katrien                                            ;;
;;                                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;(ql:quickload 'fcg)
(in-package :fcg)

;;1. First create files for evaluating your grammar
;;-------------------------------------------------
;; The grammar
(defparameter *demo-grammar* (load-demo-grammar))

;; Sentences you want to evaluate should be stored in a separate file (one sentence per line)
(defparameter *test-sentences*
  '("the mouse" "the linguist"
    "the linguist likes the mouse"
    "the mouse likes the linguist"))

(defparameter *test-file-with-sentences*
  (with-open-file (out (babel-pathname :directory '(".tmp")
                                  :name "test-sentences" :type "txt")
                       :direction :output :if-does-not-exist :create :if-exists :overwrite)
    (loop for sentence in *test-sentences*
          do (write-line sentence out))
    out))

(defparameter *test-file-with-sentences-and-gold-standard-meanings*
  (make-gold-standard-meaning-file *test-file-with-sentences* *demo-grammar*
                                   (babel-pathname :directory '(".tmp")
                                  :name "test-sentences-with-meanings" :type "txt")))
 


;;2. Call the evaluation functions (two directions)
;;-------------------------------------------------

;;a) Starting from form -> meaning (and reformulate)
(evaluate-grammar-for-comprehension *test-file-with-sentences-and-gold-standard-meanings* ;;test set
                                    *demo-grammar* ;;grammar
                                    :bi-directional? t ;;comprehend and reformulate
                                    :series 4) ;;iterations per sentence (4 by default)

;;You can now check the output buffer for an evaluation report.

;;b) Starting from meaning -> form (and recomprehend)
(evaluate-grammar-for-production *test-file-with-sentences-and-gold-standard-meanings*
                                 *demo-grammar* 
                                 :series 4)

;;c) Evaluate in two directions
(full-grammar-evaluation *test-file-with-sentences-and-gold-standard-meanings* *demo-grammar*)

 
;;3. Activating monitors for plotting the results
;;-------------------------------------------------


