(in-package #:glaw-examples)

(defstruct console
  (font nil)
  (console (glaw::make-console))
  (view (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*)))

(defmethod init-example ((it console))
  (glaw:init-content-manager "data/")
  (format t "Created console: ~S~%" (console-console it))
  (glaw:load-asset "dejavu-sans.fnt" :fonttool-bitmap-font "font")
  (setf (console-font it) (glaw:use-resource "font"))
  (glaw:add-input-handler (console-console it)))

(defmethod shutdown-example ((it console))
  (glaw:dispose-asset "font")
  (glaw:remove-input-handler (console-console it)))

(defmethod render-example ((it console))
  (glaw:begin-draw)
  (glaw:set-view-2d (console-view it))
  (glaw::render-console (console-console it) (console-font it) 10 10 700 200)
  ;;(glaw:format-at 50 120 (console-font it) "Press TAB to open the console.")
  (glaw:format-at 50 100 (console-font it) "FPS: ~a" (glaw:current-fps))
  (glaw:end-draw))

(defmethod update-example ((it console) dt)
  (declare (ignore it dt)))

(defmethod reshape-example ((it console) w h)
  (glaw:update-2d-view (console-view it) 0 0 w h))

