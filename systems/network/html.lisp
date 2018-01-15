(in-package :network)

(export '(draw-html generate-html import_flash.display.Shape flash-network-parameters define-addCircleNode))

(defgeneric draw-html (object &key &allow-other-keys)
  (:documentation "Draws the given object in html through
  hunchentoot. It calls add-element."))

(defgeneric generate-html (thing &key &allow-other-keys)
  (:documentation "Generates hunchentoot output that can be passed to
  add-element."))

(defmacro import_flash.display.Shape ()
  (format nil "~%import flash.display.Shape;~%"))

(defmacro flash-network-parameters (&key 
			    (damping 0.98) (repulsionFactor -2)
			    (repulsionRange 200) (borderRepulsionFactor -20)
			    (borderRepulsionRange 20) (springFactor 1.0)
			    (springMinLength 5) (lateralMode "off")
			    (fixedLateralizationZones "false")
			    (lateralizationRange 0) (numberOfLevels 1))
  (format nil "~%damping = ~a;                    // the less, the more the motion is dampened 
   ~%repulsionFactor = ~a;              // how much nodes repell each other
   ~%repulsionRange = ~a;              // nodes further away than this don't repell each other
   ~%borderRepulsionFactor = ~a;       // how much the borders of the drawing repell
   ~%borderRepulsionRange =~a;         // the range from the border where repulsion starts 
   ~%springFactor = ~a;              // the higher, the more connected nodes attract each other
   ~%springMinLength = ~a;               // when smaller than this length, springs don't attract anymore
   ~%lateralMode = '~a';               // can be 'horizontal', 'vertical' or 'off'. 
   ~%fixedLateralizationZones = ~a;  // when true, the display areas for each level are fixed zones
   ~%lateralizationRange = ~a;           // the bigger this value, the bigger the gap between the levels
   ~%numberOfLevels = ~a;                // the number of different levels for lateral modes~%"
	  damping repulsionFactor repulsionRange borderRepulsionFactor
	  borderRepulsionRange springFactor springMinLength lateralMode
	  fixedLateralizationZones lateralizationRange numberOfLevels))

(defmacro define-addCircleNode ()
  (format nil 
	  "~%function addCircleNode(id:String,label:String,color:Number,radius:int,level:int,weight:int) {
     ~%var circle = new Shape();
     ~%circle.graphics.beginFill(color);
     ~%circle.graphics.drawCircle(0,0,radius);
     ~%circle.graphics.endFill();
     ~%var node = addNode(id,level,weight);
     ~%node.addChild(circle);
     ~%node.addTooltip(label);
  ~%} "))

(defmethod draw-html ((net network) &key (width 250) (height 300)  (clear-page t) &allow-other-keys)
  (when clear-page 
    (wi:clear-page))
  (wi:add-element 
   `((div :style ,(format nil "border:1px solid #aaa;width:~apx;margin-bottom:30px;" width))
     ,(create-flash-network 
       width height 
       (string-append
	(import_flash.display.Shape)
	(flash-network-parameters)
	(define-addCircleNode)
	(render-as net))))))