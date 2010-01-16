(in-package #:glaw-examples)


(defstruct sprites
  (font nil)
  (view (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*))
  (sprites '()))

(defmethod init-example ((it sprites))
  (glaw:init-content-manager "data/")
  (glaw:load-asset "font.png" :bitmap-font)
  (glaw:load-asset "lisplogo_alien_256.png" :texture)
  (setf (sprites-font it) (glaw:use-resource "font.png"))
  (loop for i from 0 to 10 do
       (push (glaw:create-sprite (float (random glaw:*display-width*))
                                 (float (random glaw:*display-height*))
                           (+ 100.0 (random 100.0)) (+ 100.0 (random 100.0))
                           (glaw:use-resource "lisplogo_alien_256.png"))
             (sprites-sprites it))))

(defmethod shutdown-example ((it sprites))
  (glaw:dispose-asset "font.png")
  (glaw:dispose-asset "lisplogo_alien_256.png"))

(defmethod render-example ((it sprites))
  (glaw:begin-draw)
  (glaw:set-view-2d (sprites-view it))
  (dolist (sp (sprites-sprites it))
    (glaw:render-sprite sp))
  (glaw:format-at 50 100  (sprites-font it) "FPS: ~a" (glaw:current-fps))
  (glaw:end-draw))

(defmethod update-example ((it sprites) dt)
  (declare (ignore it dt)))

(defmethod reshape-example ((it sprites) w h)
  (glaw:update-2d-view (sprites-view it) 0 0 w h))

