
(in-package :web-interface)


(export 'create-flash-network)

(defun create-flash-network (width height code &optional (id (make-id)))
  "creates the html code for a flash graph display of the
   specified with and height and includes the code passed to it. 
   See examples below"
  (let ((swf-url "/Babel2/systems/web-interface/static-web-client.swf")
	(flashvars (mkstr "width=" width "&amp;height=" height "&amp;code=" (url-encode code))))
    `((object 
       :name ,(format nil "~(~a~)" id)
       :classid "clsid:d27cdb6e-ae6d-11cf-96b8-444553540000"
       :codebase "http://fpdownload.adobe.com/pub/shockwave/cabs/flash/swflash.cab#version=9,0,0,0"
       :width ,(mkstr width) :height ,(mkstr height) :align "middle")
      ((param :name "allowScriptAccess" :value "always"))
      ((param :name "movie" :value ,swf-url))
      ((param :name "FlashVars" :value ,flashvars))
      ((embed :id ,(format nil "~(~a~)" id)
	      :src ,swf-url
	      :pluginspage "http://www.macromedia.com/go/getflashplayer"
	      :type "application/x-shockwave-flash" 
	      :width ,(mkstr width) 
	      :height ,(mkstr height)
	      :allowScriptAccess "always" 
	      :FlashVars ,flashvars)))))


;;;; examples
;;;; direct your webbrowser to localhost:8000 and run this function:
(defun test-flash-network-creation ()
  (wi:clear-page)
  (wi:add-element '((h3) "A hierarchical network"))
  (wi:add-element 
   `((div :style "border:1px solid #aaa;width:250px;margin-bottom:30px;")
     ,(create-flash-network 250 300 "
damping = 0.95;                    // the less, the more the motion is dampened 
repulsionFactor = -10;             // how much nodes repell each other
repulsionRange = 200;              // nodes further away than this don't repell each other
borderRepulsionFactor = -20;       // how much the borders of the drawing repell
borderRepulsionRange = 20;         // the range from the border where repulsion starts 
springFactor = 0.0005;             // the higher, the more connected nodes attract each other
springMinLength = 30;              // when smaller than this length, springs don't attract anymore
lateralMode = 'horizontal';        // can be 'horizontal', 'vertical' or 'off'. 
fixedLateralizationZones = true;   // when true, the display areas for each level are fixed zones
lateralizationRange = 150;         // the bigger this value, the bigger the gap between the levels
numberOfLevels = 2;                // the number of different levels for lateral modes

function addObject(object:String) {
  addTextNode(object,object,0x108030,1,200);
}

function addWord(word:String) {
  addTextNode(word,word,0x101080,2,200);
}

addObject('obj-1'); addObject('obj-2'); addObject('obj-3');
addWord('word-1'); addWord('word-2'); addWord('word-3'); addWord('word-4');
addEdge('obj-1','word-1',0.8); addEdge('obj-1','word-2',0.3);addEdge('obj-2','word-3',1.0);
addEdge('obj-3','word-4',0.7);addEdge('obj-2','word-4',0.1);
"
)))

  (wi:add-element '((h3) "Undirected networks"))
  (wi:add-element 
   `((table)
     ((tr)
      ((td)
       ((div :style "border:1px solid #aaa;width:350px;")
	,(create-flash-network 350 200 "
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

toggleToolTips(false);
"
			       'network-1)))
      
      ((td)
       ((div :style "border:1px solid #aaa;width:500px;margin-left:30px;")
	,(create-flash-network 500 200 "
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

toggleToolTips(false);
" 'network-2))))
     ((tr) 
      ((td)
       ((a :href "javascript:nop();"
	    :onclick ,(mkstr "document.getElementById('network-1').execute('"
			     (url-encode "
addTextNode(\"foo\", \"foo\", 0x801030, 1, 200);")
			     "'); document.getElementById('network-2').execute('"
			     (url-encode "
addTextNode(\"node-1\", \"new-node\", 0x803010, 1, 100);
addTextNode(\"node-2\", \"new-node\", 0x803010, 1, 100);
addEdge(\"node-1\", \"node-2\", 1,1);")
			     "');"))
	 "Click here to change the networks"))))))


;;(test-flash-network-creation)