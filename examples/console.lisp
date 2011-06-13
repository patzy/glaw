(in-package #:glaw-examples)

(defstruct console
  console
  view)

(defmethod init-example ((it console))
  (setf (console-view it)
        (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*)
        (console-console it)
        (glaw::make-graphic-console))
  (glaw:add-input-handler (console-console it)))

(defmethod shutdown-example ((it console))
  (glaw:remove-input-handler (console-console it)))

(defmethod render-example ((it console))
  (glaw:set-view-2d (console-view it))
  (glaw:with-resources ((fnt "default-font"))
    (glaw::render-console (console-console it) fnt 10 10 700 200)))

(defmethod update-example ((it console) dt)
  (declare (ignore it dt)))

(defmethod reshape-example ((it console) w h)
  (glaw:update-2d-view (console-view it) 0 0 w h))

