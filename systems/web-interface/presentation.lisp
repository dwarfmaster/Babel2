
(in-package :web-interface)

(export 'create-html-presentation)

(defun create-html-presentation (title &rest slides)
  "Creates a presentation-like web page out of things passed to 'slides'.
   Each slide is a list of html elements that will be embedded in an html 
   page (see example below)."
  (loop with l = (length slides)
     for slide in slides
     for number from 1
     for url = (mkstr "/" (url-encode title) "/" number ".html")
     do (eval 
	 `(define-easy-handler (,(make-id) :uri ,url) ()
	    (render-xml
	     '((html :xmlns "http://www.w3.org/1999/xhtml")
	       ((head) 
		((title) ,title)
		((link :rel "icon" :href "/favicon.ico" :type "image/png"))
                ,(wi::get-combined-css-definitions)
		((style :type "text/css") "
#left-nav, #right-nav  { position:absolute; top:0px; height: 100%; width: 200px; background-color:#ddd;overflow:hidden; }
#left-nav { left:0px; }
#right-nav { right:0px; }
.nav-2 { position:absolute; top:50%; left:0px; height: 300px; width: 200px; margin-top:-220px; font-size:300px; font-weight:bold; text-align:center; }
.nav-2 a:hover {text-decoration: none; }
.nav-visible { filter:alpha(opacity=50); -moz-opacity:0.5; opacity:0.5; }
.nav-hidden { filter:alpha(opacity=0); -moz-opacity:0; opacity:0; }"))
	       ((body :style "margin: 50px;")
		,(if (> number 1)
		     `((div :id "left-nav" :class "nav-hidden" :onmouseover "document.getElementById('left-nav').setAttribute('class','nav-visible');" :onmouseout "document.getElementById('left-nav').setAttribute('class','nav-hidden');")
		       ((div :class "nav-2") ((a :href ,(mkstr (- number 1) ".html")) "<"))) "")
		,(if (< number l)
		     `((div :id "right-nav" :class "nav-hidden" :onmouseover "document.getElementById('right-nav').setAttribute('class','nav-visible');" :onmouseout "document.getElementById('right-nav').setAttribute('class','nav-hidden');")
		       ((div :class "nav-2") ((a :href ,(mkstr (+ number 1) ".html")) ">"))) "")
		((center)
		 ((div :style "display: table; height: 100%; #position: relative; overflow: scroll;text-align:center;")
		  ((div :style " #position: absolute; #top: 50%;display: table-cell; vertical-align: middle;")
		   ((div :style" #position: relative; #top: -50%;")
		    ,@slide)))))))))
     collect url into urls
     finally 
       (progn
	 (add-element `((h1) ,title))
	 (add-element `((p) 
			 ((a :href ,(first urls)) ((b) "start here"))
			 " or go to directly to part "
			 ,@(loop for url in urls for number from 1
			      append `(((b) ((a :href ,url) " " ,number))))
			"."))
	 (format t "~% Created presentation ~s. See the result at http://~a:~a." 
		 title *address* *port*))))


;;; this is an example of how to use create-html-presentation.
;;; Run this function and see the result at http://localhost:8000
(defun create-html-presentation-example ()
  (create-html-presentation 
   "test presentation"
   `(((h1) "Page 1")
     ((p) "This is an example presentation created with " ((tt) "create-html-presentation") ".")
     ((p) "To navigate between pages, move the mouse to the left and right border of the screen" ((br)) "and then click on the arrows that appear."))
   
   `(((h1) "Page 2")
     ,(let ((common-flash-declarations "
import flash.display.Shape;

damping = 0.98;                    // the less, the more the motion is dampened 
repulsionFactor = -5;              // how much nodes repell each other
repulsionRange = 200;              // nodes further away than this don't repell each other
borderRepulsionFactor = -20;       // how much the borders of the drawing repell
borderRepulsionRange = 20;         // the range from the border where repulsion starts 
springFactor = 0.05;               // the higher, the more connected nodes attract each other
springMinLength = 5;               // when smaller than this length, springs don't attract anymore
lateralMode = 'off';               // can be 'horizontal', 'vertical' or 'off'. 
fixedLateralizationZones = false;  // when true, the display areas for each level are fixed zones
lateralizationRange = 0;           // the bigger this value, the bigger the gap between the levels
numberOfLevels = 1;                // the number of different levels for lateral modes

function addCircleNode(id:String,label:String,color:Number,radius:int,level:int,weight:int) {
  var circle = new Shape();
  circle.graphics.beginFill(color);
  circle.graphics.drawCircle(0,0,radius);
  circle.graphics.endFill();
  var node = addNode(id,level,weight);
  node.addChild(circle);
  node.addTooltip(label);
}
"))
	   `((table :cellspacing "30px")
	     ((tr)
	      ((td)
	       "nice graph:"
	       ((div :style "border:1px solid #aaa;width:350px;")
		,(create-flash-network 350 200 (mkstr common-flash-declarations "
addTextNode('u-39-3','m-jack',0x335500,1,100);
addTextNode('u-39-7','f-jack',0x335500,1,100);
addTextNode('u-39-5','m-jill',0x335500,1,100);
addTextNode('u-39-8','f-jill',0x335500,1,100);
addTextNode('u-39-6','f-married',0x3300ff,1,100);
addCircleNode('u-39-5-f-m-u-39-6','f-m',0x000000,2,1,50);
addEdge('u-39-5','u-39-5-f-m-u-39-6',0.25,0x3300ff,0.1,0.2);
addEdge('u-39-5-f-m-u-39-6','u-39-6',0.25,0x3300ff,0.1,0.2);
addCircleNode('u-39-3-f-m-u-39-6','f-m',0x000000,2,1,50);
addEdge('u-39-3','u-39-3-f-m-u-39-6',0.25,0x3300ff,0.1,0.2);
addEdge('u-39-3-f-m-u-39-6','u-39-6',0.25,0x3300ff,0.1,0.2);
addCircleNode('u-39-3-m-married-u-39-5','m-married',0x000000,2,1,50);
addEdge('u-39-3','u-39-3-m-married-u-39-5',0.25,0x3300ff,0.1,0.2);
addEdge('u-39-3-m-married-u-39-5','u-39-5',0.25,0x3300ff,0.1,0.2);
addCircleNode('u-39-5-f-m-u-39-8','f-m',0x000000,2,1,50);
addEdge('u-39-5','u-39-5-f-m-u-39-8',0.25,0x3300ff,0.1,0.2);
addEdge('u-39-5-f-m-u-39-8','u-39-8',0.25,0x3300ff,0.1,0.2);
addCircleNode('u-39-3-f-m-u-39-7','f-m',0x000000,2,1,50);
addEdge('u-39-3','u-39-3-f-m-u-39-7',0.25,0x3300ff,0.1,0.2);
addEdge('u-39-3-f-m-u-39-7','u-39-7',0.25,0x3300ff,0.1,0.2);
"))))
	      ((td)
	       "another nice graph:"
	       ((div :style "border:1px solid #aaa;width:350px;")
		,(create-flash-network 350 200 (mkstr common-flash-declarations "
addCircleNode('id-831','color-object-net',0x000000,10,1,100);
addCircleNode('id-830','red-net',0x000000,10,1,100);
addEdge('id-830','id-831',0.1,0xff9900);
addTextNode('id-825','m-color',0x66ffaa,1,100);
addEdge('id-825','id-830',0.1,0xff9900);
addTextNode('id-824','m-red',0x66ffaa,1,100);
addEdge('id-824','id-830',0.1,0xff9900);
addTextNode('id-809','f-red',0x66ffaa,1,100);
addEdge('id-809','id-830',0.1,0xff9900);
addEdge('id-824','id-809',0.1,0x002299);
addEdge('id-825','id-809',0.1,0x002299);
addEdge('id-824','id-825',0.1,0x002299);
addCircleNode('id-829','box-net',0x000000,10,1,100);
addEdge('id-829','id-831',0.1,0xff9900);
addTextNode('id-822','m-object',0x66ffaa,1,100);
addEdge('id-822','id-829',0.1,0xff9900);
addTextNode('id-821','m-box',0x66ffaa,1,100);
addEdge('id-821','id-829',0.1,0xff9900);
addTextNode('id-802','f-box',0x66ffaa,1,100);
addEdge('id-802','id-829',0.1,0xff9900);
addEdge('id-821','id-802',0.1,0x002299);
addEdge('id-822','id-802',0.1,0x002299);
addEdge('id-821','id-822',0.1,0x002299);
addEdge('id-830','id-829',0.1,0x002299);
addEdge('id-830','id-829',0.1,0x002299);
")))
	       ((br))
	       "and another one:"
	       ((div :style "border:1px solid #aaa;width:350px;")
		,(create-flash-network 350 200 (mkstr common-flash-declarations "
addCircleNode('id-831','color-object-net',0x000000,10,1,100);
addCircleNode('id-830','red-net',0x000000,10,1,100);
addEdge('id-830','id-831',0.1,0xff9900);
addTextNode('id-825','m-color',0x66ffaa,1,100);
addEdge('id-825','id-830',0.1,0xff9900);
addTextNode('id-824','m-red',0x66ffaa,1,100);
addEdge('id-824','id-830',0.1,0xff9900);
addTextNode('id-809','f-red',0x66ffaa,1,100);
addEdge('id-809','id-830',0.1,0xff9900);
addEdge('id-824','id-809',0.1,0x002299);
addEdge('id-825','id-809',0.1,0x002299);
addEdge('id-824','id-825',0.1,0x002299);
addCircleNode('id-829','box-net',0x000000,10,1,100);
addEdge('id-829','id-831',0.1,0xff9900);
addTextNode('id-822','m-object',0x66ffaa,1,100);
addEdge('id-822','id-829',0.1,0xff9900);
addTextNode('id-821','m-box',0x66ffaa,1,100);
addEdge('id-821','id-829',0.1,0xff9900);
addTextNode('id-802','f-box',0x66ffaa,1,100);
addEdge('id-802','id-829',0.1,0xff9900);
addEdge('id-821','id-802',0.1,0x002299);
addEdge('id-822','id-802',0.1,0x002299);
addEdge('id-821','id-822',0.1,0x002299);
addEdge('id-830','id-829',0.1,0x002299);
addEdge('id-830','id-829',0.1,0x002299);
"))))))))
   `(((h1) "Page 3")
     ((p) "last page"))))

;;(create-html-presentation-example)
