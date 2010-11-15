(in-package #:glaw-examples)

(defstruct texture
  (view (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*))
  (image (glaw::create-image 256 256 4))
  (start-red 0)
  (texture nil)
  (sprites '()))

(defmethod init-example ((it texture))
  (setf (texture-texture it) (glaw:create-texture-from-image (texture-image it)))
  (loop for i from 0 to 10 do
       (push (glaw:create-sprite (float (random glaw:*display-width*))
                                 (float (random glaw:*display-height*))
                           (+ 100.0 (random 100.0)) (+ 100.0 (random 100.0))
                           (texture-texture it))
             (texture-sprites it))))

(defmethod shutdown-example ((it texture))
  (declare (ignore it)))

(defmethod render-example ((it texture))
  (glaw:begin-draw)
  (glaw:set-view-2d (texture-view it))
  (dolist (sp (texture-sprites it))
    (glaw:render-sprite sp))
  (glaw:with-resources ((fnt "default-font"))
    (glaw:format-at 50 100  (texture-font it) "FPS: ~a" (glaw:current-fps)))
  (glaw:end-draw))

(defmethod update-example ((it texture) dt)
  (let ((r (texture-start-red it))
        (g 255)
        (b 255))
    (loop for x below 256
       do (loop for y below 256
            do (glaw::image-set-pixel (texture-image it) x y r r r 255))
         (setf r (mod (incf r) 255))))
  (glaw::update-texture (texture-texture it) (glaw::image-data (texture-image it)))
  (setf (texture-start-red it) (mod (incf (texture-start-red it)) 255)))

(defmethod reshape-example ((it texture) w h)
  (glaw:update-2d-view (texture-view it) 0 0 w h))

