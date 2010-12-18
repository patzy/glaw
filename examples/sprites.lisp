(in-package #:glaw-examples)


(defstruct sprites
  view
  animated-sprite
  anim-state
  animation
  sprites)

(defmethod init-example ((it sprites))
  (glaw:load-asset "lisplogo_alien_256.png" :texture "lisplogo")
  (glaw:load-asset "explosion-blue-1.png" :texture "expl0")
  (glaw:load-asset "explosion-blue-2.png" :texture "expl1")
  (glaw:load-asset "explosion-blue-3.png" :texture "expl2")
  (setf (sprites-view it)
        (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*))
  (setf (sprites-animation it) (glaw:make-keyframe-anim
                                :frame-time 0.2
                                :start-frame 0
                                :nb-frames 3
                                :hints '((:texture . 0))
                                :channels (list (glaw:use-resources "expl0" "expl1" "expl2"))))
  (setf (sprites-anim-state it) (glaw:make-anim-state :animation (sprites-animation it)
                                                     :time 0.0
                                                     :scale 1.0))
  (setf (sprites-animated-sprite it) (glaw:create-sprite (float (random glaw:*display-width*))
                                                        (float (random glaw:*display-height*))
                                                        (+ 100.0 (random 100.0))
                                                        (+ 100.0 (random 100.0))
                                                        nil))
  (glaw:anim-state-apply (sprites-anim-state it) (sprites-animated-sprite it))
  (loop for i from 0 to 10 do
       (push (glaw:create-sprite (float (random glaw:*display-width*))
                                 (float (random glaw:*display-height*))
                           (+ 100.0 (random 100.0)) (+ 100.0 (random 100.0))
                           (glaw:use-resource "lisplogo")
                           :flip (glaw:random-nth '(:none :vertical :horizontal :both)))
             (sprites-sprites it))))

(defmethod shutdown-example ((it sprites))
  (glaw:dispose-asset "expl0")
  (glaw:dispose-asset "expl1")
  (glaw:dispose-asset "expl2")
  (glaw:dispose-asset "lisplogo"))

(defmethod render-example ((it sprites))
  (glaw:begin-draw)
  (glaw:set-view-2d (sprites-view it))
  (dolist (sp (sprites-sprites it))
    (glaw:render-sprite sp))
  (glaw:render-sprite (sprites-animated-sprite it))
  (glaw:with-resources ((fnt "default-font"))
    (glaw:format-at 50 100 fnt "FPS: ~a" (glaw:current-fps)))
  (glaw:end-draw))

(defmethod update-example ((it sprites) dt)
  (glaw:anim-state-update (sprites-anim-state it) dt)
  (glaw:anim-state-apply (sprites-anim-state it) (sprites-animated-sprite it)))

(defmethod reshape-example ((it sprites) w h)
  (glaw:update-2d-view (sprites-view it) 0 0 w h))

