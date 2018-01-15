;;; Copyright (c) 2012, Michael Spranger (http://www.michael-spranger.com).
;;; All rights reserved.
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 
(in-package :cl-google-charts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; export static charts (examples taken from google's documentation)
;; creates an html file called test.html (adapt as suited)

(with-open-file (stream "test.html" :direction :output
                        ;; :if-exists :overwrite
                        :if-does-not-exist :create)
  (format stream "<html><head>~a</head><body>~{~a~}</body></html>"
          (get-head-html)
          (list
           ;; area chart
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/areachart\" target=\"_blank\">AreaChart (area-chart)</a></h2>"
           (chart->html
            (make-instance 'area-chart
                           :data '(("Year" "Sales" "Expenses")
                                   ("2004"  1000      400)
                                   ("2005"  1170      460)
                                   ("2006"  660       1120)
                                   ("2007"  1030      540))
                           :options '(("title" . "Company Performance")
                                      ("hAxis" . (:obj ("title" . "Year")
                                                  ("titleTextStyle" . (:obj ("color" . "red")))))
                                      ("height" . 400)
                                      ("width" . 500))))
           ;; bar-chart
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/barchart\" target=\"_blank\">BarChart (bar-chart)</a></h2>"
           (chart->html
            (make-instance 'bar-chart
                           :data '(("Year" "Sales" "Expenses")
                                   ("2004"  1000      400)
                                   ("2005"  1170      460)
                                   ("2006"  660       1120)
                                   ("2007"  1030      540))
                           :options '(("title" . "Company Performance")
                                      ("hAxis" . (:obj ("title" . "Year")
                                                  ("titleTextStyle" . (:obj ("color" . "red")))))
                                      ("height" . 800)
                                      ("width" . 800))))
           ;; bubble-chart
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/bubblechart\" target=\"_blank\">BubbleChart (bubble-chart)</a></h2>"
           (chart->html
            (make-instance 'bubble-chart
                           :data-source '(("ID" "Life Expectancy" "Fertility Rate" "Region" "Population")
                                          ("CAN" 80.66 1.67 "North America" 33739900)
                                          ("DEU" 79.84 1.36 "Europe" 81902307)
                                          ("DNK" 78.6 1.84 "Europe" 5523095)
                                          ("EGY" 72.73 2.78 "Middle East" 79716203)
                                          ("GBR" 80.05 2 "Europe" 61801570)
                                          ("IRN" 72.49 1.7 "Middle East" 73137148)
                                          ("IRQ" 68.09 4.77 "Middle East" 31090763)
                                          ("ISR" 81.55 2.96 "Middle East" 7485600)
                                          ("RUS" 68.6 1.54 "Europe" 141850000)
                                          ("USA" 78.09 2.05 "North America"307007000))
                           :options '(("title" . "Correlation between life expectancy, fertility rate and population of some world countries (2010)")
                                      ("hAxis" . (:obj ("title" . "Life Expectancy")))
                                      ("vAxis" . (:obj ("title" . "Fertility Rate")))
                                      ("width" . 600)
                                      ("height" . 600))))
               
           (chart->html
            (make-instance 'bubble-chart
                           :data-source '(("ID" "X" "Y" "Temperatur")
                                          (" "  80 167 120)
                                          (" "  79 136 130)
                                          (" "  78 184 50)
                                          (" "  72 278 230)
                                          (" "  81 200 210)
                                          (" "  72 170 100)
                                          (" "  68 477 80))
                           :options "{colorAxis: {colors: ['yellow', 'red']}}"))
           ;; candlestick-chart
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/candlestickchart\" target=\"_blank\">CandlestickChart (candlestick-chart)</a></h2>"
           (chart->html (make-instance 'candlestick-chart
                                       :data '(("Mon" 20 28 38 45)
                                               ("Tue" 31 38 55 66)
                                               ("Wed" 50 55 77 80)
                                               ("Thu" 77 77 66 50)
                                               ("Fri" 68 66 22 15))
                                       :options '(("legend" . "none")
                                                  ("width" . 700)
                                                  ("height" . 600))))
           ;; column-chart
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/columnchart\" target=\"_blank\">ColumnChart (column-chart)</a></h2>"  
           (chart->html (make-instance 'column-chart
                                       :data '(("Year" "Sales" "Expenses")
                                               ("2004"  1000      400)
                                               ("2005"  1170      460)
                                               ("2006"  660       1120)
                                               ("2007"  1030      540))
                                       :options '(("title" . "Company Performance")
                                                  ("hAxis" . (:obj ("title" . "Year")
                                                              ("titleTextStyle" . (:obj ("color" . "red")))))
                                                  ("width" . 700)
                                                  ("height" . 600))))
           ;; combo-chart
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/combochart\" target=\"_blank\">ComboChart (combo-chart)</a></h2>"  
           (chart->html (make-instance 'combo-chart
                                       :data '(("Month" "Bolivia" "Ecuador" "Madagascar" "Papua New Guinea" "Rwanda" "Average")
                                               ("2004/05"  165      938         522             998           450      614.6)
                                               ("2005/06"  135      1120        599             1268          288      682)
                                               ("2006/07"  157      1167        587             807           397      623)
                                               ("2007/08"  139      1110        615             968           215      609.4)
                                               ("2008/09"  136      691         629             1026          366      569.6))
                                       :options '(("title" . "Monthly Coffee Production by Country")
                                                  ("vAxis" . (:obj ("title" . "Cups")))
                                                  ("hAxis" . (:obj ("title" . "Month")))
                                                  ("seriesType" . "bars")
                                                  ("series" . (:obj (5 . (:obj ("type" . "line")))))
                                                  ("width" . 700)
                                                  ("height" . 600))))
           ;; gauge
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/gauge\" target=\"_blank\">Gauge (gauge)</a></h2>"
           (chart->html (make-instance 'gauge
                                       :data '(("Label" "Value")
                                               ("Memory" 80)
                                               ("CPU" 55)
                                               ("Network" 68))
                                       :options '(("width" . 400)
                                                  ("height" . 120)
                                                  ("redFrom" . 90)
                                                  ("redTo" . 100)
                                                  ("yellowFrom" . 75)
                                                  ("yellowTo" . 90)
                                                  ("minorTicks" . 5))))
           ;; geo-chart
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/geochart\" target=\"_blank\">GeoChart (geo-chart)</a></h2>"
           (chart->html (make-instance 'geo-chart
                                       :data '(("Country" "Popularity")
                                               ("Germany" 200)
                                               ("United States" 300)
                                               ("Brazil" 400)
                                               ("Canada" 500)
                                               ("France" 600)
                                               ("RU" 700))))
           ;; line-chart
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/linechart\" target=\"_blank\">LineChart (line-chart)</a></h2>"
           (chart->html (make-instance 'line-chart
                                       :data '(("Year" "Sales" "Expenses")
                                               ("2004"  1000      400)
                                               ("2005"  1170      460)
                                               ("2006"  660       1120)
                                               ("2007"  1030      540))
                                       :options '(("title" . "Company Performance")
                                                  ("width" . 700)
                                                  ("height" . 600))))
           ;; pie-chart
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/piechart\" target=\"_blank\">PieChart (pie-chart)</a></h2>"
           (chart->html (make-instance 'pie-chart
                                       :data '(("Task" "Hourse per Day")
                                               ("work" 11)
                                               ("eat" 2)
                                               ("commute" 2)
                                               ("watch tv" 2)
                                               ("sleep" 7))))
           ;; scatter-chart
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/scatterchart\" target=\"_blank\">ScatterChart (scatter-chart)</a></h2>"
           (chart->html (make-instance 'scatter-chart
                                       :data  '(("Age" "Weight")
                                                ( 8      12)
                                                ( 4      5.5)
                                                ( 11     14)
                                                ( 4      5)
                                                ( 3      3.5)
                                                ( 6.5    7))
                                       :options '(("title" . "Age vs. Weight comparison")
                                                  ("hAxis" . (:obj ("title" . "Age")("minValue" . 0)("maxValue" . 15)))
                                                  ("vAxis" . (:obj ("title" . "Cups")("minValue" . 0)("maxValue" . 15)))
                                                  ("width" . 500)
                                                  ("height" . 500))))
           ;; stepped-area-chart
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/steppedareachart\" target=\"_blank\">SteppedAreaChart (stepped-area-chart)</a></h2>"
           (chart->html (make-instance 'stepped-area-chart
                                       :data  '(("Director (Year)"  "Rotten Tomatoes" "IMDB")
                                                ("Alfred Hitchcock (1935)" 8.4         7.9)
                                                ("Ralph Thomas (1959)"     6.9         6.5)
                                                ("Don Sharp (1978)"        6.5         6.4)
                                                ("James Hawes (2008)"      4.4         6.2))
                                       :options '(("title" . "The decline of 'The 39 Steps'")
                                                  ("vAxis" . (:obj ("title" . "Accumulated Rating")("minValue" . 0)("maxValue" . 15)))
                                                  ("isStacked" . t)
                                                  ("width" . 600)
                                                  ("height" . 500))))
           ;; table
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/table\" target=\"_blank\">Table (table)</a></h2>"
           (chart->html
            (make-instance 'table
                           :data
                           (make-instance 'data-table
                                          :columns '(("Name" string)("Salary" number)("Full Time Employee" boolean))
                                          :rows '(("Mike" (10000 . "$10,000") :true)
                                                  ("Jim" (8000 . "$8,000")  :false)
                                                  ("Alice" (12500 . "$12,500") :true)
                                                  ("Bob"   (7000 . "$7,000")  :true)))
                           :options '(("width" . 800)
                                      ("height" . 150))))
           ;; tree
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/treemap\" target=\"_blank\">TreeMap (tree-map)</a></h2>"
           (chart->html (make-instance 'tree-map
                                       :data '(("Location" "Parent" "Market trade volume (size)" "Market increase/decrease (color)")
                                               ("Global"    ""                 0                               0)
                                               ("America"   "Global"             0                               0)
                                               ("Europe"    "Global"             0                               0)
                                               ("Asia"      "Global"             0                               0)
                                               ("Australia" "Global"             0                               0)
                                               ("Africa"    "Global"             0                               0)
                                               ("Brazil"    "America"            11                              10)
                                               ("USA"       "America"            52                              31)
                                               ("Mexico"    "America"            24                              12)
                                               ("Canada"    "America"            16                              -23)
                                               ("France"    "Europe"             42                              -11)
                                               ("Germany"   "Europe"             31                              -2)
                                               ("Sweden"    "Europe"             22                              -13)
                                               ("Italy"     "Europe"             17                              4)
                                               ("UK"        "Europe"             21                              -5)
                                               ("China"     "Asia"               36                              4)
                                               ("Japan"     "Asia"               20                              -12)
                                               ("India"     "Asia"               40                              63)
                                               ("Laos"      "Asia"               4                               34)
                                               ("Mongolia"  "Asia"               1                               -5)
                                               ("Israel"    "Asia"               12                              24)
                                               ("Iran"      "Asia"               18                              13)
                                               ("Pakistan"  "Asia"               11                              -52)
                                               ("Egypt"     "Africa"             21                              0)
                                               ("S. Africa" "Africa"             30                              43)
                                               ("Sudan"     "Africa"             12                              2)
                                               ("Congo"     "Africa"             10                              12)
                                               ("Zair"      "Africa"             8                               10))
                                       :options '(("minColor" . "#f00")
                                                  ("midColor" . "#ddd")
                                                  ("maxColor" . "#0d0")
                                                  ("headerHeight" . 15)
                                                  ("fontColor" . "black")
                                                  ("showScale" . t)
                                                  ("width" . 600)
                                                  ("height" . 400))))

           ;; motion-chart
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/motionchart\" target=\"_blank\">MotionChart (motion-chart)</a></h2>"
           (chart->html (make-instance 'motion-chart
                                       :data 
                                       (make-instance 'data-table
                                                      :columns '(("Fruit" string)("Data" date)("Sales" number)("Expenses" number)("Location" string))
                                                      :rows '(("Apples"  ((:year . 1988)(:month . 0)(:day . 1)) 1000 300 "East")
                                                              ("Oranges" ((:year . 1988)(:month . 0)(:day . 1)) 1150 200 "West")
                                                              ("Bananas" ((:year . 1988)(:month . 0)(:day . 1)) 300  250 "West")
                                                              ("Apples"  ((:year . 1989)(:month . 6)(:day . 1)) 1200 400 "East")
                                                              ("Oranges" ((:year . 1989)(:month . 6)(:day . 1)) 750  150 "West")
                                                              ("Bananas" ((:year . 1989)(:month . 6)(:day . 1)) 788  617 "West")))))


           ;; intensity-map
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/intensitymap\" target=\"_blank\">IntensityMap (intensity-map)</a></h2>"
           (chart->html
            (make-instance 'intensity-map
                           :data (make-instance 'data-table
                                                :data '(("Country" "Population (mil)" "Area (km2)")
                                                        ("CN"            1324           9640821)
                                                        ("IN"            1133           3287263)
                                                        ("US"            304            9629091)
                                                        ("ID"            232            1904569)
                                                        ("BR"            187            8514877)))))
           ;; annotated-time-line
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/annotatedtimeline\" target=\"_blank\">AnnotatedTimeLine (annotated-time-line)</a></h2>"
           (chart->html
            (make-instance 'annotated-time-line
                           :data 
                           (make-instance 'data-table
                                          :columns '(("Date" date)
                                                     ("Sold Pencils" number)
                                                     ("title1" string)
                                                     ("text1" string)
                                                     ("Sold Pens" number)
                                                     ("title2" string)
                                                     ("text2" string))
                                          :rows
                                          '((((:year . 2008)(:month . 1)(:day . 1)) 30000 :undefined :undefined 40645 :undefined :undefined)
                                            (((:year . 2008)(:month . 1)(:day . 2)) 14045 :undefined :undefined 20374 :undefined :undefined)
                                            (((:year . 2008)(:month . 1)(:day . 3)) 55022 :undefined :undefined 50766 :undefined :undefined)
                                            (((:year . 2008)(:month . 1)(:day . 4)) 75284 :undefined :undefined 14334 "Out of Stock""Ran out of stock on pens at 4pm")
                                            (((:year . 2008)(:month . 1)(:day . 5)) 41476 "Bought Pens""Bought 200k pens" 66467 :undefined :undefined)
                                            (((:year . 2008)(:month . 1)(:day . 6)) 33322 :undefined :undefined 39463 :undefined :undefined))))
            :style "width: 700px; height: 240px;"))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; export external-data-source
;; creates an html file called test.html (adapt as suited)

(with-open-file (stream "test.html" :direction :output
                        ;; :if-exists :overwrite
                        :if-does-not-exist :create)
  (format stream "<html><head>~a</head><body>~{~a~}</body></html>"
          (get-head-html)
          (list
           ;; external data source
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/fusiontables\" target=\"_blank\">Fusion Tables Example</a></h2>"
           (chart->html
            (make-instance 'bar-chart
                           :data (make-instance 'external-data-source
                                                :source-url "http://www.google.com/fusiontables/gvizdata?tq="
                                                :query "SELECT Year, Austria, Bulgaria, Denmark, Greece FROM 641716")
                           :options '(("width" . "600")
                                      ("height" . "600")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; export dynamic charts 
;; creates an html file called test.html (adapt as suited)
;; then change the data

;; create dynamic data source, add it to global data sources
(defparameter *dynamic-example-data*
  (let ((data
         (make-instance 'dynamic-data-source
                        :data '(("Year" "Sales" "Expenses")
                                ("2004"  1000      400)
                                ("2005"  1170      460)
                                ("2006"  660       1120)
                                ("2007"  1030      540)))))
    (add-dynamic-data-source data)
    data))

;; create an html file (open it)
(with-open-file (stream "test.html" :direction :output
                        ;; :if-exists :overwrite
                        :if-does-not-exist :create)
  (format stream "<html><head>~a</head><body>~{~a~}</body></html>"
          (get-head-html)
          (list
           ;; area chart
           "<h2><a href=\"https://developers.google.com/chart/interactive/docs/gallery/areachart\" target=\"_blank\">AreaChart (area-chart)</a></h2>"
           (chart->html
            (make-instance 'area-chart :data *dynamic-example-data*)))))

;; let's change the data (we are going to push another year)
(progn
  (push '("2003" 500 5000)
        (data *dynamic-example-data*))
  (setf (sig *dynamic-example-data*) (get-new-sig)))
