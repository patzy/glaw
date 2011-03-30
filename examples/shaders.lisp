;;; Example template
(in-package #:glaw-examples)

(defstruct shaders
  vertex
  fragment
  program
  view)

(defmethod init-example ((it shaders))
  (setf (shaders-view it)
        (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*)
        (shaders-vertex it)
        (glaw::create-vertex-shader-from-file #P"data/test.vert" t)
        (shaders-fragment it)
        (glaw::create-fragment-shader-from-file #P"data/test.frag" t)
        (shaders-program it)
        (glaw::create-shader-program (shaders-vertex it) (shaders-fragment it) t)))

(defmethod shutdown-example ((it shaders))
  (glaw::destroy-shader-program (shaders-program it))
  (glaw::destroy-shader (shaders-vertex it))
  (glaw::destroy-shader (shaders-fragment it)))

(defmethod render-example ((it shaders))
  (glaw:begin-draw)
  (glaw:set-view-2d (shaders-view it))
  (glaw::set-shader-program (shaders-program it))

  (gl:color 1 1 1)
  (gl:with-primitive :polygon
    (gl:vertex 250 250 0)
    (gl:vertex 750 250 0)
    (gl:vertex 750 750 0)
    (gl:vertex 250 750 0))

  (glaw::set-shader-program nil)

  (glaw:with-resources ((fnt "default-font"))
    (glaw:format-at 50 100 fnt "FPS: ~a" (glaw:current-fps)))
  (glaw:end-draw))

(defmethod update-example ((it shaders) dt)
  (declare (ignore it dt)))

(defmethod reshape-example ((it shaders) w h)
  (glaw:update-2d-view (shaders-view it) 0 0 w h))

