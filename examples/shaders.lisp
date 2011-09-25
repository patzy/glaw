;;; Example template
(in-package #:glaw-examples)

(defstruct shaders
  texture
  vertex
  fragment
  program
  view)

(defmethod init-example ((it shaders))
  (setf (shaders-texture it)
        (glaw:load-asset "lisplogo_alien_256.png" :texture "lisplogo")
        (shaders-view it)
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
  (glaw:set-view-2d (shaders-view it))
  (glaw::set-shader-program (shaders-program it))
  (glaw:select-texture (shaders-texture it))
  (gl:color 1 1 1)
  (gl:with-primitive :polygon
    (gl:tex-coord 0.0 0.0)
    (gl:vertex 250 250 0)
    (gl:tex-coord 1.0 0.0)
    (gl:vertex 750 250 0)
    (gl:tex-coord 1.0 1.0)
    (gl:vertex 750 750 0)
    (gl:tex-coord 0.0 1.0)
    (gl:vertex 250 750 0))
  (glaw::set-shader-program nil))

(defmethod update-example ((it shaders) dt)
  (declare (ignore it dt)))

(defmethod reshape-example ((it shaders) w h)
  (glaw:update-2d-view (shaders-view it) 0 0 w h))

