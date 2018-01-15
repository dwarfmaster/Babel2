(in-package :web-interface)

;; #####################################################################
;; Generates 3d svg images
;; The function draw-3d-objects calls the method generate-3d-wireframe for every element in objects
;; Overload generate-3d-wireframe for your classes


;;Example use:

;;(defclass foo () ())
;;(defmethod generate-3d-wireframe ((thing foo) &key)
;;  (let* ((circle (loop for i from 0 to 11
;;                       for rad = (* 2 pi (/ i 12))
;;                       for x = (* 100 (sin rad)) for y = (* 100 (cos rad))
;;                       collect (make-line-segment 0 0 0 x y 0 (format nil "#55~X000" i))))
;;         (bigger-circle (loop for i from 0 to 47
;;                              for rad = (* 2 pi (/ i 28))
;;                              for y = (* 250 (cos rad)) for z = (* 250 (sin rad))
;;                              append (translate-wireframe (rotate-wireframe-x circle rad) (make-3d-point 0 y z)))))
;;    (loop for i from 0 to 5
;;          append (rotate-wireframe-z (translate-wireframe bigger-circle (make-3d-point 0 (- 1000 (* 500 i)) (- 1300 (* 300 i)))) (* i 0.5)))))
;;(add-element (draw-3d-objects (list (make-instance 'foo)) :axes nil))

;; ### Main interface:
(export '(draw-3d-objects generate-3d-wireframe set-camera))
;; ### utils:
(export '(make-3d-point make-line-segment rotate-wireframe-x rotate-wireframe-y rotate-wireframe-z translate-wireframe))


;; #####################################################################
;; draw-3d-objects generates html-output for a list of objects

(defun draw-3d-objects (objects &key (size 1000) (pixels-per-mm 0.1) (border t) (axes t) (shadow t))
  (let* ((model (append
                 (loop for object in objects
                       append (generate-3d-wireframe object))))
         (projection (append (3d->2d-project model)
                             (when axes (3d->2d-project (generate-3d-axes (* 0.8 size))))))
         (sorted-projection (sort (copy-list projection) #'> :key #'(lambda (ls) (max (z (p1 ls)) (z (p2 ls)))))))
    (draw-2d-wireframe (append
                        (when shadow (3d->2d-project (compute-shadow model)))
                        sorted-projection
                        )
                       :size size :pixels-per-mm pixels-per-mm :border border)))

(defun generate-3d-axes (scale)
  (let ((color "#555555"))
    (append
     (list
      (make-line-segment 0.0 0.0 0.0 scale 0.0 0.0 color)
      (make-line-segment 0.0 0.0 0.0 0.0 scale 0.0 color)
      (make-line-segment 0.0 0.0 0.0 0.0 0.0 scale color))
     (character->wireframe #\x (* 1.1 scale) 0.0 0.0 color (/ scale 30.0))
     (character->wireframe #\y 0.0 (* 1.1 scale) 0.0 color (/ scale 30.0))
     (character->wireframe #\z 0.0 0.0 (* 1.1 scale) color (/ scale 30.0)))))

(defun character->wireframe (char x y z color scale)
  (let ((y1 (+ y scale))
        (y2 (+ y (* 2.0 scale)))
        (z1 (+ z (* 2.0 scale)))
        (z2 (+ z (* 4.0 scale))))
    (case char
      (#\x
       (list
        (make-line-segment x y z x y2 z2 color) (make-line-segment x y2 z x y z2 color)))
      (#\y
       (list
        (make-line-segment x y1 z1 x y2 z2 color) (make-line-segment x y1 z1 x y z2 color) (make-line-segment x y1 z1 x y1 z color)))
      (#\z
       (list
        (make-line-segment x y z2 x y2 z2 color) (make-line-segment x y2 z x y z2 color) (make-line-segment x y z x y2 z color))))))


;; #####################################################################
;; generate-3d-wireframe

(defgeneric generate-3d-wireframe (thing &key)
  (:documentation "generate an wirefrae for thing"))

(defmethod generate-3d-wireframe ((thing t) &key) ())


;; #####################################################################
;; Line segments

(defclass 3d-point ()
  ((x :initarg :x :accessor x :type float :initform 0.0
      :documentation "x position in mm")
   (y :initarg :y :accessor y :type float :initform 0.0
      :documentation "y position in mm")
   (z :initarg :z :accessor z :type float :initform 0.0
      :documentation "z position in mm")))

(defun make-3d-point (x y z)
  (make-instance '3d-point :x x :y y :z z))

(defmethod copy-object ((point 3d-point))
  (make-instance '3d-point
                 :x (x point)
                 :y (y point)
                 :z (z point)))

(defclass line-segment ()
  ((p1 :initarg :p1 :accessor p1 :type 3d-point
       :documentation "ending1")
   (p2 :initarg :p2 :accessor p2 :type 3d-point
       :documentation "ending2")
   (color :initarg :color :accessor color :type string :initform "#000000"
          :documentation "the color of the line")))

(defun make-line-segment (x1 y1 z1 x2 y2 z2 color)
  (make-instance 'line-segment
                 :p1 (make-instance '3d-point :x x1 :y y1 :z z1)
                 :p2 (make-instance '3d-point :x x2 :y y2 :z z2) :color color))


;; #####################################################################
;; HTML drawing

(defun draw-2d-wireframe
       (line-segments &key (size 1000) (pixels-per-mm 0.1) (border t))
  "draws a list of robots, objects and boxes."
  (let* (
         (x1 (* -1.0 size))
         (x2 size)
         (y1 (* -1.0 size))
         (y2 size)
         (width (* pixels-per-mm (- y2 y1)))
         (height (* pixels-per-mm (- x2 x1))))
    `((div :class "spatial-scene"
           :style ,(format nil  "width:~,2fpx;height:~,2fpx;~a" width height
                           (if border "border: 1px dotted #999;" "")))
      ((svg :xmlns "http://www.w3.org/2000/svg"
            :width ,(format nil "~,2f" width) :height ,(format nil "~,2f" height))
       ((g :transform 
           ,(format nil "rotate(-90) scale(~,2f,~,2f) translate(~,2f,~,2f)"
                    pixels-per-mm (* 1.0 pixels-per-mm)  (* -1.0 x2) (* 1.0 y2)))
        ((g);; :stroke "#000000" :stroke-width "10")
         ,@(loop for l in line-segments
                 collect `((g :stroke ,(color l) :stroke-width "10")
                           ((line :x1 ,(html-string (x (p1 l)))
                                  :y1 ,(html-string (y (p1 l)))
                                  :x2 ,(html-string (x (p2 l)))
                                  :y2 ,(html-string (y (p2 l)))))))))))))

(defun html-string (coord)
  (format nil "~,2f" coord))


;; #####################################################################
;; Camera

(defclass camera ()
  ((c :initarg :c :accessor c :type 3d-point
       :documentation "location of the camera")
   (a :initarg :a :accessor a :type 3d-point
       :documentation "camera angle")
   (viewer :initarg :viewer :accessor viewer :type 3d-point
           :documentation "viewers position relative to display surface")))

(defparameter *camera* nil)

(defun set-camera (cx cy cz ax ay az vx vy vz)
  (setf *camera* (make-instance 'camera
                                :c (make-instance '3d-point
                                                  :x cx
                                                  :y cy
                                                  :z cz)
                                :a (make-instance '3d-point
                                                  :x ax
                                                  :y ay
                                                  :z az)
                                :viewer (make-instance '3d-point
                                                       :x vx
                                                       :y vy
                                                       :z vz))))

;; ### default camera position
(set-camera -1500.0 500.0 1000.0 0.0 (+ (/ pi 2.0) 0.1) 0.0 0.0 0.0 -1500.0)

(defun compute-shadow (wireframe)
  (let ((d 0.2))
    (loop for segment in wireframe
          collect (make-line-segment (+ (x (p1 segment)) (* d (z (p1 segment))))
                                     (- (y (p1 segment)) (* d (z (p1 segment))))
                                     0.0
                                     (+ (x (p2 segment)) (* d (z (p2 segment))))
                                     (- (y (p2 segment)) (* d (z (p2 segment))))
                                     0.0
                                     "#999999"))))

       
;; #####################################################################
;; Projection

(defmethod 3d->2d-project ((thing t)) ())

(defmethod 3d->2d-project ((thing list))
  (loop for x in thing
        collect (3d->2d-project x)))

(defmethod 3d->2d-project ((thing line-segment))
  (make-instance 'line-segment
                 :p1 (3d->2d-project (p1 thing))
                 :p2 (3d->2d-project (p2 thing))
                 :color (color thing)))

(defmethod 3d->2d-project ((point 3d-point))
  (let* ((cam-p (c *camera*))
         (cam-a (a *camera*))
         (viewer (viewer *camera*))
         (ax-cx (- (x point) (x cam-p)))
         (ay-cy (- (y point) (y cam-p)))
         (az-cz (- (z point) (z cam-p)))
         (Ox (x cam-a))
         (Oy (y cam-a))
         (Oz (z cam-a))
         (ex (x viewer))
         (ey (y viewer))
         (ez (z viewer))
         (dx (- (* (cos Oy)
                   (+ (* (sin Oz) ay-cy)
                      (* (cos Oz) ax-cx)))
                (* (sin Oy) az-cz)))
         (dy (+ (* (sin Ox) (+ (* (cos Oy) az-cz)
                               (* (sin Oy) (+ (* (sin Oz) ay-cy)
                                              (* (cos Oz) ax-cx)))))
                (* (cos Ox) (- (* (cos Oz) ay-cy)
                               (* (sin Oz) ax-cx)))))
         (dz (- (* (cos Ox) (+ (* (cos Oy) az-cz)
                               (* (sin Oy) (+ (* (sin Oz) ay-cy)
                                              (* (cos Oz) ax-cx)))))
                (* (sin Ox) (- (* (cos Oz) ay-cy)
                               (* (sin Oz) ax-cx)))))
         (bx (* (- dx ex) (/ ez dz)))
         (by (* (- dy ey) (/ ez dz))))
    (make-instance '3d-point
                   :x bx
                   :y by
                   :z dz)))
                                                                   

;; #####################################################################
;; 3D Transformation operations

(defun rotate-wireframe-x (wireframe a)
  (loop for segment in wireframe
        collect (make-instance 'line-segment
                               :p1 (rotate-x (p1 segment) a)
                               :p2 (rotate-x (p2 segment) a)
                               :color (color segment))))

(defun rotate-wireframe-y (wireframe a)
  (loop for segment in wireframe
        collect (make-instance 'line-segment
                               :p1 (rotate-y (p1 segment) a)
                               :p2 (rotate-y (p2 segment) a)
                               :color (color segment))))

(defun rotate-wireframe-z (wireframe a)
  (loop for segment in wireframe
        collect (make-instance 'line-segment
                               :p1 (rotate-z (p1 segment) a)
                               :p2 (rotate-z (p2 segment) a)
                               :color (color segment))))


(defun translate-wireframe (wireframe reference)
  (loop for segment in wireframe
        collect (make-instance 'line-segment
                               :p1 (translate (p1 segment) reference)
                               :p2 (translate (p2 segment) reference)
                               :color (color segment))))

(defun rotate-z (point a)
  (let ((x (x point))
        (y (y point))
        (new-point (copy-object point)))
    (setf (x new-point) (- (* (cos a) x) (* (sin a) y)))
    (setf (y new-point) (+ (* (sin a) x) (* (cos a) y)))
    new-point))

(defun rotate-x (point a)
  (let ((z (z point))
        (y (y point))
        (new-point (copy-object point)))
    (setf (y new-point) (- (* (cos a) y) (* (sin a) z)))
    (setf (z new-point) (+ (* (sin a) y) (* (cos a) z)))
    new-point))

(defun rotate-y (point a)
  (let ((x (x point))
        (z (z point))
        (new-point (copy-object point)))
    (setf (x new-point) (+ (* (cos a) x) (* (sin a) z)))
    (setf (z new-point) (- (* (cos a) z) (* (sin a) x)))
    new-point))

(defmethod translate ((point 3d-point) (reference 3d-point))
  (let ((new-point (copy-object point)))
    (setf (x new-point) (+ (x point) (x reference)))
    (setf (y new-point) (+ (y point) (y reference)))
    (setf (z new-point) (+ (z point) (z reference)))
    new-point))

