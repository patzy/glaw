(in-package #:glaw-examples)

(defstruct animated-sprite
  sprite
  anim-state)

(defun create-animated-sprite (anim)
  (let* ((anim-state (glaw:make-anim-state :animation anim
                                               :time 0.0
                                               :scale (glaw:random-between 0.5 1.5)))
         (sp (glaw:create-sprite (float (random glaw:*display-width*))
                                 (float (random glaw:*display-height*))
                                 (+ 1.0 (random 10.0)) (+ 1.0 (random 10.0))
                                         (glaw:use-resource "lisplogo")
                                      :flip (glaw:random-nth '(:none :vertical :horizontal :both))))
         (asp (make-animated-sprite
               :sprite sp
               :anim-state anim-state)))
    (glaw:anim-state-apply anim-state sp)
    asp))

(defstruct sprites
  view
  animations
  sprites
  batch
  (animate nil)
  (method :direct))

(defmethod init-example ((it sprites))
  (glaw:load-asset "lisplogo_alien_256.png" :texture "lisplogo")
  (glaw:load-asset "explosion-blue-1.png" :texture "expl0")
  (glaw:load-asset "explosion-blue-2.png" :texture "expl1")
  (glaw:load-asset "explosion-blue-3.png" :texture "expl2")
  (setf (sprites-view it)
        (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*))
  (push (glaw:make-keyframe-anim
         :frame-time 0.2
         :start-frame 0
         :nb-frames 3
         :hints '((:texture . 0))
         :channels (list (glaw:use-resources "expl0" "expl1" "expl2"))) (sprites-animations it))
  (push (glaw:make-keyframe-anim
         :frame-time 0.01
         :start-frame 0
         :nb-frames 10
         :hints '((:orientation . 0))
         :channels (loop for i below 10
                        for angle = 0
                        with angle-step = (/ pi 10.0)
                        do (incf angle angle-step)
                        collect angle))
        (sprites-animations it))
  (loop for i from 0 to 1000 do
       (push (create-animated-sprite (glaw:random-nth (sprites-animations it)))
             (sprites-sprites it)))
  (setf (sprites-batch it) (glaw::create-sprite-batch))
  (dolist (asp (sprites-sprites it))
    (glaw::sprite-batch-append (sprites-batch it) (animated-sprite-sprite asp)))
  (glaw:add-input-handler it))

(defmethod shutdown-example ((it sprites))
  (glaw::destroy-sprite-batch (sprites-batch it))
  (glaw:remove-input-handler it)
  (glaw:dispose-asset "expl0")
  (glaw:dispose-asset "expl1")
  (glaw:dispose-asset "expl2")
  (glaw:dispose-asset "lisplogo"))

(defmethod render-example ((it sprites))
  (glaw:begin-draw)
  (glaw:set-view-2d (sprites-view it))
  (case (sprites-method it)
    (:batch  (when (sprites-animate it)
               (glaw::sprite-batch-clear (sprites-batch it))
               (dolist (asp (sprites-sprites it))
                 (glaw::sprite-batch-append (sprites-batch it) (animated-sprite-sprite asp))))
            (glaw::sprite-batch-render (sprites-batch it)))
    (:direct (dolist (asp (sprites-sprites it))
               (glaw:render-sprite (animated-sprite-sprite asp)))))
  (glaw:with-resources ((fnt "default-font"))
    (glaw:format-at 50 80 fnt "mode: ~a" (sprites-method it))
    (glaw:format-at 50 100 fnt "FPS: ~a" (glaw:current-fps)))
  (glaw:end-draw))

(defmethod update-example ((it sprites) dt)
  (when (sprites-animate it)
    (dolist (sp (sprites-sprites it))
      (glaw:anim-state-update (animated-sprite-anim-state sp) dt)
      (glaw:anim-state-apply (animated-sprite-anim-state sp) (animated-sprite-sprite sp)))))

(defmethod reshape-example ((it sprites) w h)
  (glaw:update-2d-view (sprites-view it) 0 0 w h))

(glaw:key-handler (it sprites) (:space :press)
   (case (sprites-method it)
     (:batch (setf (sprites-method it) :direct))
     (:direct (setf (sprites-method it) :batch))))

(glaw:key-handler (it sprites) (:a :press)
    (setf (sprites-animate it) (not (sprites-animate it))))