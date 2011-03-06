;;; Example template
(in-package #:glaw-examples)

(defstruct empty
  view)

(defmethod init-example ((it empty))
  (setf (empty-view it)
        (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*))
)

(defmethod shutdown-example ((it empty))
)

(defmethod render-example ((it empty))
  ;;(glaw::dformat "Begin frame~%")
  (glaw:begin-draw)
  (glaw:set-view-2d (empty-view it))
  (glaw:with-resources ((fnt "default-font"))
    (glaw:format-at 50 100 fnt "FPS: ~a" (glaw:current-fps)))
  (glaw:end-draw)
  ;;(glaw::dformat "End frame~%")
)

(defmethod update-example ((it empty) dt)
)

(defmethod reshape-example ((it empty) w h)
  (glaw:update-2d-view (empty-view it) 0 0 w h))

