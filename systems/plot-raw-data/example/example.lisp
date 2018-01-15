(asdf:operate 'asdf:load-op 'plot-raw-data)

(in-package :plot-raw-data)

;; Some functionality (like transparancy) requires gnuplot 4.4 or
;; higher. To install this on a mac you might first consider
;; installing gnuplot through macports in order to install all
;; dependencies. This currently does not install version 4.4. So
;; afterwards download the tar.gz source from official website, untar
;; it. Thourgh terminal do cd gnuplot-4.4.0, mkdir build, cd build,
;; ../configure --with-readline=bsd --disable-wxwidgets, make, sudo
;; make install

;; ---------------------
;; Example for evo-plots 
;; ---------------------

;; There is only a single function for creating evo plots from raw
;; data which is raw-files->evo-plot. You pass it a list of paths to
;; raw data files :raw-file-paths and :average-windows. All other
;; parameters are optional.


;; Example of a minimal call. This function combines the data from the
;; two raw data-files (assuming they are .lisp files), averages both
;; with a window of 100 and outputs this as a pdf in the directory of
;; the first raw data-file. It also tries to open the file.

(raw-files->evo-plot
 :raw-file-paths '(("systems" "plot-raw-data" "example" "raw-data" "communicative-success")
                   ("systems" "plot-raw-data" "example" "raw-data" "alignment"))
 :average-windows 100)


;; An example using most parameters with explanation. 

(raw-files->evo-plot
 ;; the raw-file-paths do not need to have the same directory path.
 :raw-file-paths '(("systems" "plot-raw-data" "example" "raw-data" "communicative-success")
                   ("systems" "plot-raw-data" "example" "raw-data" "alignment"))
 ;; average-windows can be either a list (which then needs to be of
 ;; equal length as raw-file-paths or a number which is then taken for
 ;; all files.
 :average-windows '(100 50)
 :title "Example experiment"
 ;; :captions can be a list of length equal to raw-file-paths
 :captions '("Communicative success" "Lexical alignment")
 ;; By default a file-name is generated using the file-names of the raw-data
 :plot-file-name "my-graph"
 :plot-directory  '("systems" "plot-raw-data" "example" "graphs")
 ;; file-type can also be "dat" instead of "lisp". You cannot combine
 ;; different file-types.
 :file-type "lisp"
 ;; graphic-type is pdf by default but can also be postscript
 :graphic-type "pdf"
 :start 1000
 :end 2500
 :series-numbers '(0 1)
 :key-location "above"
 ;; By default only the left y-axis (1) is used, you can also use the
 ;; second one. The list should be of length equal to :raw-file-paths.
 :use-y-axis '(1 2)
 ;; you can provide :y1-min :y1-max :y2-min :y2-max
 :y1-min -1
 :line-width 1
 ;; By default y1-grid is drawn and y2-grid is not
 :draw-y1-grid nil
 :x-label "Interactions divided by 50"
 :y1-label "Communicative success"
 :y2-label "Lexical alignment"
 :grid-line-width 1 ;; default 0.5 :divide-indices-by can be used to
 ;; adjust for plotting interactions per agent, use half of the
 ;; population-size
 :divide-indices-by 50
 :average-mode :mean ;; can also be median (is used for averaging batches)
 :error-bars :min-max ;; can also be :stdev or '(:percentile low up) with low up numbers
 :error-bar-modes '(:filled :lines) ;; :filled requires gnuplot 4.4 or above
 :open t
 )



;; -----------------------------
;; Example for regular bar plots
;; -----------------------------

;; Example of a minimal call. This example will generate one pdf with
;; two bars representing communicative success and alignment. The
;; labels in the plot as the output pdf name are based on the
;; file-names of the raw-data. Each bar represents the average of all
;; values recorded

(raw-files->bar-plot
  :raw-file-paths '(("systems" "plot-raw-data" "example" "raw-data" "communicative-success")
                    ("systems" "plot-raw-data" "example" "raw-data" "alignment")))

;; A more elaborate example

(raw-files->bar-plot
  :raw-file-paths '(("systems" "plot-raw-data" "example" "raw-data" "communicative-success")
                    ("systems" "plot-raw-data" "example" "raw-data" "alignment"))
  :file-type "lisp"
  :plot-name "my-bar-chart"
  :plot-directory '("systems" "plot-raw-data" "example" "graphs")
  :data-labels '("Communicative success" "Lexical alignment")
  ;; :only-x-last-interactions might be the most important
  ;; parameter. If set only the values of the x last interactions are
  ;; taken into account when averaging. Can also be one if you already
  ;; average during data collection.
  :only-x-last-interactions 100
  :logscale nil
  :y-min 0
  :y-max 1.05
  :title "My bar charts"
  :x-label "Measures"
  :y-label "Bars"
  :error-bars '(:percentile 5 95)
  :open t)


;; -----------------------------
;; Example for grouped bar plots
;; -----------------------------

;; As the input for a grouped bar plot is different from that of a
;; normal bar-plot the call differs as well. You now pass a list of
;; lists of file-paths with each sublist corresponding to a group of
;; bars. I assume that each group has the same amount of data-files.

(file-structure->grouped-bar-plot
 :raw-file-paths '((("systems" "plot-raw-data" "example" "raw-data" "communicative-success")
                    ("systems" "plot-raw-data" "example" "raw-data" "alignment"))
                   (("systems" "plot-raw-data" "example" "raw-data" "exp1" "communicative-success")
                    ("systems" "plot-raw-data" "example" "raw-data" "exp1" "alignment"))
                   (("systems" "plot-raw-data" "example" "raw-data" "exp2" "communicative-success")
                    ("systems" "plot-raw-data" "example" "raw-data" "exp2" "alignment"))))

;; more parameters

(file-structure->grouped-bar-plot
 :raw-file-paths '((("systems" "plot-raw-data" "example" "raw-data" "communicative-success")
                    ("systems" "plot-raw-data" "example" "raw-data" "alignment"))
                   (("systems" "plot-raw-data" "example" "raw-data" "exp1" "communicative-success")
                    ("systems" "plot-raw-data" "example" "raw-data" "exp1" "alignment"))
                   (("systems" "plot-raw-data" "example" "raw-data" "exp2" "communicative-success")
                    ("systems" "plot-raw-data" "example" "raw-data" "exp2" "alignment")))
 :file-type "lisp"
 :plot-name "my-grouped-bar-plot"
 :plot-directory '("systems" "plot-raw-data" "example" "graphs")
 :labels-a '("Communicative success" "Lexical alignment")
 :labels-b '("5" "50" "20")
 :title "My grouped bar plots"
 :only-x-last-interactions 100
 :y-max 1.05
 :y-label "success and alignment"
 :x-label "Population size"
 :error-bars :min-max
 :open t
 )


;; -----------------------------
;; Example for stacked bar plots
;; -----------------------------

;; input-wise this is essentially the same as grouped-bar-plot

(file-structure->stacked-bar-plot
 :raw-file-paths '((("systems" "plot-raw-data" "example" "raw-data" "exp2" "coverage-0-2")
                    ("systems" "plot-raw-data" "example" "raw-data" "exp2" "coverage-3-5")
                    ("systems" "plot-raw-data" "example" "raw-data" "exp2" "coverage-6+"))))

(file-structure->stacked-bar-plot
 :raw-file-paths '((("systems" "plot-raw-data" "example" "raw-data" "exp2" "coverage-0-2")
                    ("systems" "plot-raw-data" "example" "raw-data" "exp2" "coverage-3-5")
                    ("systems" "plot-raw-data" "example" "raw-data" "exp2" "coverage-6+")))
 :plot-name "my-stacked-bar-plot"
 :plot-directory '("systems" "plot-raw-data" "example" "graphs")
 :labels-a '("meaning coverage = 0-2" "meaning coverage = 3-5" "meaning coverage = 6+")
 :labels-b '("experiment 2")
 :only-x-last-interactions 50
 :title "Meaning coverage for experiment 2"
 :x-label "experiment"
 :y-label "Meaning coverage"
 :open t)