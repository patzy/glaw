(in-package #:glaw-examples)


(defstruct sprites
  (font nil)
  (view (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*))
  animated-sprite
  anim-state
  animation
  (sprites '()))

(defmethod init-example ((it sprites))
  (glaw:init-content-manager "data/")
  (glaw:load-asset "font.png" :fixed-bitmap-font)
  (glaw:load-asset "lisplogo_alien_256.png" :texture)
  (glaw:load-asset "explosion-blue-1.png" :texture "expl0")
  (glaw:load-asset "explosion-blue-2.png" :texture "expl1")
  (glaw:load-asset "explosion-blue-3.png" :texture "expl2")
  (setf (sprites-animation it) (glaw:make-texture-anim
                                :frame-time 0.2
                                :start-frame 0
                                :nb-frames 3
                                :textures (glaw:use-resources "expl0" "expl1" "expl2")
                                :coords #(0.0 1.0 1.0 1.0 1.0 0.0 0.0 0.0)))
  (setf (sprites-anim-state it) (glaw:make-anim-state :animation (sprites-animation it)
                                                     :time 0.0
                                                     :scale 1.0))
  (setf (sprites-animated-sprite it) (glaw:create-sprite (float (random glaw:*display-width*))
                                                        (float (random glaw:*display-height*))
                                                        (+ 100.0 (random 100.0))
                                                        (+ 100.0 (random 100.0))
                                                        nil))
  (glaw:animation-apply-frame (sprites-animation it) (sprites-animated-sprite it) 0)
  (setf (sprites-font it) (glaw:use-resource "font.png"))
  (loop for i from 0 to 10 do
       (push (glaw:create-sprite (float (random glaw:*display-width*))
                                 (float (random glaw:*display-height*))
                           (+ 100.0 (random 100.0)) (+ 100.0 (random 100.0))
                           (glaw:use-resource "lisplogo_alien_256.png"))
             (sprites-sprites it))))

(defmethod shutdown-example ((it sprites))
  (glaw:dispose-asset "expl0")
  (glaw:dispose-asset "expl1")
  (glaw:dispose-asset "expl2")
  (glaw:dispose-asset "font.png")
  (glaw:dispose-asset "lisplogo_alien_256.png"))

(defmethod render-example ((it sprites))
  (glaw:begin-draw)
  (glaw:set-view-2d (sprites-view it))
  (dolist (sp (sprites-sprites it))
    (glaw:render-sprite sp))
  (glaw:render-sprite (sprites-animated-sprite it))
  (glaw:format-at 50 100  (sprites-font it) "FPS: ~a" (glaw:current-fps))
  (glaw:end-draw))

(defmethod update-example ((it sprites) dt)
  (glaw:anim-state-update (sprites-anim-state it) dt)
  (glaw:anim-state-apply (sprites-anim-state it) (sprites-animated-sprite it)))

(defmethod reshape-example ((it sprites) w h)
  (glaw:update-2d-view (sprites-view it) 0 0 w h))

