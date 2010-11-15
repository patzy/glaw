(in-package #:glaw-examples)

(defstruct console
  (console (glaw::make-console))
  (view (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*)))

(defmethod init-example ((it console))
  (glaw:add-input-handler (console-console it)))

(defmethod shutdown-example ((it console))
  (glaw:remove-input-handler (console-console it)))

(defmethod render-example ((it console))
  (glaw:begin-draw)
  (glaw:set-view-2d (console-view it))
  (glaw:with-resources ((fnt "default-font"))
    (glaw::render-console (console-console it) fnt 10 10 700 200)
    (glaw:format-at 50 100 fnt "FPS: ~a" (glaw:current-fps)))
  (glaw:end-draw))

(defmethod update-example ((it console) dt)
  (declare (ignore it dt)))

(defmethod reshape-example ((it console) w h)
  (glaw:update-2d-view (console-view it) 0 0 w h))

