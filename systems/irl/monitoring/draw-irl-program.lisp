
(in-package :irl)

(export '(draw-irl-program irl-program->svg))

(defun draw-irl-program (irl-program &key path (open nil) (format "png") draw-arrows)
  "Uses s-dot to draw an irl program. Returns the pathname of the
   generated graphic. When :open t, then it tries to open the
   irl-program. "
  (draw-predicate-network irl-program :path path :open open :format format :draw-arrows draw-arrows))

(defun irl-program->svg (irl-program &key draw-arrows (topic nil) (only-variables t))
  "renders irl-program into an svg"
  (predicate-network->svg irl-program :draw-arrows draw-arrows :topic topic :only-variables only-variables))



