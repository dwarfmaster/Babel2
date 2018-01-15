(in-package :asdf)

(defsystem :plot-raw-data
  :depends-on (:utils
               :monitors)
    :serial t
    :components 
    ((:file "package")
     (:file "raw-data")
     (:file "gnuplot-utils")
     (:file "evo-plots")
     (:file "bar-plots")
     (:file "blackboard-plots")))
