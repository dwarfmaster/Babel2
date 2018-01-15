;;;;;
;;;;; File: test-babel-installation.lisp
;;;;;
;;;;; This file helps you to check whether Babel2 was initialized
;;;;; successfully and whether supporting tools such as gnuplot and
;;;;; graphviz were installed correctly. This file accompanies the
;;;;; step by step instruction for getting Babel2 running.
;;;;; 
;;;;; Please evaluate each expression step by step and compare
;;;;; the results with the expected ones.
;;;;;

;;;;; --------------------------------------------------------------------------
;;;;; 1. The minimal requirements: a Lisp and Babel2

;;;;; In order to use Babel2, the Babel2/init-babel.lisp file needs to
;;;;; be loaded.

;;;;; We recommend adding the following lines to your lisp
;;;;; initialization file so that Babel2 is automatically initialized
;;;;; when your lisp starts.

;; (load "/directory-to-Babel/libraries/asdf")
;; (load "/directory-to-Babel/init-babel")

;;;;; replace directory-to-Babel by the correct path to where the
;;;;; file init-babel.lisp resides.
;;;;; e.g. (load "/Users/pieter/Babel2/init-babel")


(in-package :cl-user)

;;;;; This function should return the path to your Babel directory, for
;;;;; example #P"/Users/pieter/Projects/Babel2/".
(print (babel-pathname))

;;;;; If not, then something went wrong with your initialization of
;;;;; Babel2 (i.e. init-babel.lisp was not loaded) (go back to step 1
;;;;; of "Installing Babel 2")


;;;;; The next expression loads the most of the Babel2 systems. If
;;;;; that does not work, one of the steps above went wrong.
(asdf:operate 'asdf:load-op :irl)


;;;;; If all of the above works you have the minimal requirements to
;;;;; run Babel2.

;;;;; --------------------------------------------------------------------------
;;;;; 2. The web interface


;;;;; You will have only a very limited user experience with Babel2
;;;;; and especially FCG and IRL when the web interface does not work
;;;;; on your machine/ lisp.  Basically, your lisp needs to support
;;;;; threading.

;;;;; If the evaluation of the following line give you a 'yes!', then
;;;;; the webserver runs on your machine. If not try another lisp
;;;;; implementation.
(progn #+hunchentoot-available-on-this-platform (print "yes!"))

;;;;; The following line will load the web-interface:
(asdf:operate 'asdf:load-op :web-interface)


;;;;; Open your web browser, preferably firefox, safari or
;;;;; google chrome (and definitely not internet explorer) and open
;;;;; http://localhost:8000/. This should present you with a white
;;;;; page containing a reset button.

;;;;; And the following should display hello world in your web
;;;;; browser.
(wi:add-element `((h1) "hello world"))


;;;;; --------------------------------------------------------------------------
;;;;; 3. Gnuplot


;;;;; Gnuplot is used by virtually all multi-agent experiments for
;;;;; plotting graphs. It is only required when running language game
;;;;; experiments. It is not needed for grammar engineers only
;;;;; interested in FCG. The next expression tests two things: -
;;;;; whether pipes into programs work on your lisp - whether your
;;;;; gnuplot is installed properly.
;;;;;
;;;;; What should happen is that a gnuplot window opens with the plot
;;;;; of a sinus in it.
(utils:with-open-pipe  (stream (monitors:pipe-to-gnuplot))
  (format stream "plot sin(x)~c" #\newline)
  (finish-output stream))


;;;;; If nothing happens, then 3 things might be wrong:
;;;;;
;;;;; 1. You don't have gnuplot. Get one through apt-get/ fink/ cygwin
;;;;; etc. or by manually installing it from http://www.gnuplot.info/
;;;;;
;;;;; 2. You have gnuplot but your lisp doesn't find it. Make sure
;;;;; that it is in the path. (Note that lisp usually doesn't open a
;;;;; shell to execute a program, so it has to be where all other
;;;;; system wide programs are). 
;;;;; 
;;;;; On Macosx and linux try linking your binary to /usr/bin:
;;;;; ln -s /path/to/your/gnuplot /usr/bin/gnuplot
;;;;; On windows, add the c:\path-to-gnuplot\gnuplot\bin directory
;;;;; to the system path.
;;;;;
;;;;; 3. Your gnuplot is not configured to automatically open a graph
;;;;; window. To try this out, go to your terminal/ command line 
;;;;; and type 'gnuplot'. Gnuplot should start and then you type
;;;;; 'plot sin(x)':
;; Leibniz:~ loetzsch$ gnuplot

;; 	G N U P L O T
;; 	Version 4.6 patchlevel 3    last modified 2013-04-12 
;; 	Build System: Darwin x86_64

;; 	Copyright (C) 1986-1993, 1998, 2004, 2007-2013
;; 	Thomas Williams, Colin Kelley and many others

;; 	gnuplot home:     http://www.gnuplot.info
;; 	faq, bugs, etc:   type "help FAQ"
;; 	immediate help:   type "help"  (plot window: hit 'h')

;; Terminal type set to 'aqua'
;; gnuplot> plot sin(x)

;;;;; if you don't see a window with a sinus, then you need to
;;;;; configure your gnuplot to do so.




;;;;; --------------------------------------------------------------------------
;;;;; 4. Graphviz

;;;;; Some experiments use graphviz to visualize network structures
;;;;; (you definitely will want graphviz when you work with irl).

;;;;; This should create a .png file in Babel2/.tmp/ which contains a
;;;;; small network. Additionally, the image should be
;;;;; opened in a preview window. (This can take some time the first time)
(irl:draw-irl-program '((foo ?a ?b) (bar ?b ?c) (baz ?c ?a)) :open t :format "pdf")


;;;;; If this doesn't happen, then two things might be wrong: 
;;;;;
;;;;; 1. You don't have graphviz installed. To check this, go to to
;;;;; your command line/ terminal and type 'dot'. If this command is
;;;;; not found, then you don't have graphviz installed. Install it
;;;;; with apt-get/ fink/ cygwin etc or install it manually from
;;;;; http://www.graphviz.org/.
;;;;;
;;;;; 2. You have graphviz but your lisp doesn't find the 'dot'
;;;;; program. Either put the dot program somewhere where the system
;;;;; can find it or (on linux/ mac) create a symbolic link:
;;;;; ln -s /path/to/your/dot /usr/bin/dot
;;;;; On windows, add the c:\path-to-graphviz\bin directory
;;;;; to the system path.


