(in-package #:glaw-examples)


(defstruct sound
  (font nil)
  (view (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*)))

(defmethod init-example ((it sound))
  (glaw:init-content-manager "data/")
  (glaw:load-asset "font.png" :fixed-bitmap-font)
  (setf (sound-font it) (glaw:use-resource "font.png"))
  (glaw::init-sound)
  (format t "Nb sources: ~S~%" (glaw::sound-nb-channels)))

(defmethod shutdown-example ((it sound))
  (glaw:dispose-asset "font.png"))

(defmethod render-example ((it sound))
  (glaw:begin-draw)
  (glaw:set-view-2d (sound-view it))
  (glaw:format-at 50 100  (sound-font it) "FPS: ~a" (glaw:current-fps))
  (glaw:end-draw))

(defmethod update-example ((it sound) dt)
  (declare (ignore it dt)))

(defmethod reshape-example ((it sound) w h)
  (glaw:update-2d-view (sound-view it) 0 0 w h))

