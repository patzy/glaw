(in-package #:glaw-examples)


(defstruct framebuffer
  view
  animated-sprite
  anim-state
  animation
  sprites
  ;; render to texture
  fb
  fb-texture
  scene-sprite)

(defmethod init-example ((it framebuffer))
  (glaw:load-asset "lisplogo_alien_256.png" :texture "lisplogo")
  (glaw:load-asset "explosion-blue-1.png" :texture "expl0")
  (glaw:load-asset "explosion-blue-2.png" :texture "expl1")
  (glaw:load-asset "explosion-blue-3.png" :texture "expl2")
  (setf (framebuffer-view it)
        (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*))
  (setf (framebuffer-animation it) (glaw:make-keyframe-anim
                                :frame-time 0.2
                                :start-frame 0
                                :nb-frames 3
                                :hints '((:texture . 0))
                                :channels (list (glaw:use-resources "expl0" "expl1" "expl2"))))
  (setf (framebuffer-anim-state it) (glaw:make-anim-state :animation (framebuffer-animation it)
                                                     :time 0.0
                                                     :scale 1.0))
  (setf (framebuffer-animated-sprite it) (glaw:create-sprite (float (random glaw:*display-width*))
                                                        (float (random glaw:*display-height*))
                                                        (+ 100.0 (random 100.0))
                                                        (+ 100.0 (random 100.0))
                                                        nil))
  (glaw:anim-state-apply (framebuffer-anim-state it) (framebuffer-animated-sprite it))
  (loop for i from 0 to 10 do
       (push (glaw:create-sprite (float (random glaw:*display-width*))
                                 (float (random glaw:*display-height*))
                           (+ 100.0 (random 100.0)) (+ 100.0 (random 100.0))
                           (glaw:use-resource "lisplogo")
                           :flip (glaw:random-nth '(:none :vertical :horizontal :both)))
             (framebuffer-sprites it)))
  (setf (framebuffer-fb it)
        (glaw::create-framebuffer 128 128)
        (framebuffer-fb-texture it)
        (glaw::create-texture 128 128 4 nil))
  (setf (framebuffer-scene-sprite it)
        (glaw:create-sprite (float (random glaw:*display-width*))
                            (float (random glaw:*display-height*))
                            512 512
                            (framebuffer-fb-texture it)))
  (glaw::framebuffer-attach-color (framebuffer-fb it) (framebuffer-fb-texture it))
  (glaw::framebuffer-attach-depth (framebuffer-fb it)
                                  (glaw::create-renderbuffer 128 128 :depth-component24))
  (let ((framebuffer-status (gl:check-framebuffer-status-ext :framebuffer-ext)))
      (unless (gl::enum= framebuffer-status :framebuffer-complete-ext)
        (error "Framebuffer not complete: ~A." framebuffer-status))))


(defmethod shutdown-example ((it framebuffer))
  (glaw:dispose-asset "expl0")
  (glaw:dispose-asset "expl1")
  (glaw:dispose-asset "expl2")
  (glaw:dispose-asset "lisplogo"))

(defmethod render-example ((it framebuffer))
  ;; render scene to texture
  (glaw::select-framebuffer (framebuffer-fb it))
  (gl:clear-color 1.0 1.0 1.0 1.0)
  (gl:clear :color-buffer :depth-buffer)
  (gl:viewport 0 0 128 128)
  (glaw:set-view-2d (framebuffer-view it))
  (dolist (sp (framebuffer-sprites it))
    (glaw:render-sprite sp))
  (glaw:render-sprite (framebuffer-animated-sprite it))
  ;; render again to screen + sprite with scene texture
  (glaw::select-framebuffer nil)
  (gl:clear-color 0.5 0.5 0.5 1.0)
  (glaw:begin-draw)
  (gl:viewport 0 0 glaw:*display-width* glaw:*display-height*)
  (glaw:set-view-2d (framebuffer-view it))
  (dolist (sp (framebuffer-sprites it))
    (glaw:render-sprite sp))
  (glaw:render-sprite (framebuffer-animated-sprite it))
  (glaw:render-sprite (framebuffer-scene-sprite it)))

(defmethod update-example ((it framebuffer) dt)
  (glaw:anim-state-update (framebuffer-anim-state it) dt)
  (glaw:anim-state-apply (framebuffer-anim-state it) (framebuffer-animated-sprite it)))

(defmethod reshape-example ((it framebuffer) w h)
  (glaw:update-2d-view (framebuffer-view it) 0 0 w h))

