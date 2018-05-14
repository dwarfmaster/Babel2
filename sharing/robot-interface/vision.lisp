
(in-package :robot-interface)

(export '(take-picture observe-scene))

(defun take-picture (robot &key (open t))
  "Takes a picture, gets it to your machine and optionally opens it."
  #+nao (let ((picture-details (nao-take-picture robot)))
          (let* ((path (rest (assoc :path picture-details)))
                 (file (rest (assoc :name picture-details)))
                 (ext (rest (assoc :ext picture-details)))
                 (remote-pathname (pathname (format nil "~a~a.~a" path file ext)))
                 (local-pathname (pathname (babel-pathname :directory '(".tmp" "nao-img") :name file :type ext))))
            (nao-scp-get robot remote-pathname local-pathname)
            (when open
              (let ((arg (format nil "open ~a" local-pathname)))
                (run-prog "/bin/sh" :args (list "-c" arg))))
            (format nil "~a.~a" file ext))))

(defun observe-scene (robot &key (open nil))
  "Takes a picture, gets it to your machine and analyzes it using machine vision."
  #+nao (let* ((img-filename (take-picture robot :open nil))
               (analysis (nao-analyze-scene robot img-filename))
               (analysis-img (when (assoc :filename analysis) (rest (assoc :filename analysis))))
               (analysis-data (when (assoc :data analysis) (rest (assoc :data analysis)))))
          (when open
            (let* ((local-pathname (babel-pathname :directory '(".tmp" "nao-img")
                                                   :name (format nil "~a" analysis-img)
                                                   :type "jpg"))
                   (arg (format nil "open ~a" local-pathname)))
              (run-prog "/bin/sh" :args (list "-c" arg))))
          (values analysis-data analysis-img)))
