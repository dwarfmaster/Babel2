
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tutorial for corpus processing system ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load grammar and corpus-processing system
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(asdf:operate 'asdf:load-op :dutch-vp)
(asdf:operate 'asdf:load-op :corpus-processing)
(in-package :dutch-vp)

;; Specify input and ouputfiles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *input-file*
  (babel-pathname :directory '("tutorial" "corpus-processing") :name "testfile-long" :type "txt"))
(defparameter *output-file*
  (babel-pathname :directory '("tutorial" "corpus-processing") :name "testfile-output" :type "txt"))

;; the function you want to call on each line

(defun comprehend-and-formulate-with-dutch-vp-grammar (line)
  (comprehend-and-formulate line :cxn-inventory *fcg-constructions*))


;; Call the function

;; Single batch - 8 threads
(process-corpus :inputfile *input-file*
                :outputfile *output-file*
                :function #'comprehend-and-formulate-with-dutch-vp-grammar
                :number-of-threads 8
                :number-of-lines-per-thread 2000)

;; Multiple-batches - 4 threads
(process-corpus :inputfile *input-file*
                :outputfile *output-file*
                :function #'comprehend-and-formulate-with-dutch-vp-grammar
                :number-of-threads 4
                :number-of-lines-per-thread 20)



