(in-package #:glaw-examples)


(defstruct sound
  (font nil)
  (view (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*))
  (snd nil))

(glaw:key-handler (it sound) (:space :press)
   (glaw:play-sound (sound-snd it)))

(defmethod init-example ((it sound))
  (glaw:load-asset "font.png" :fixed-bitmap-font)
  (glaw:init-sound)
  (glaw:load-asset "pew.wav" :sound)
  (setf (sound-snd it) (glaw:use-resource "pew.wav"))
  (setf (sound-font it) (glaw:use-resource "font.png"))
  (format t "Nb channels: ~S~%" (glaw:sound-nb-channels))
  (glaw:add-input-handler it))

(defmethod shutdown-example ((it sound))
  (glaw:dispose-asset "font.png")
  (glaw:dispose-asset "pew.wav")
  (glaw:remove-input-handler it)
  (glaw:shutdown-sound))

(defmethod render-example ((it sound))
  (glaw:begin-draw)
  (glaw:set-view-2d (sound-view it))
  (glaw:format-at 50 160  (sound-font it) "Press spacebar for PEW PEW !!!")
  (glaw:format-at 50 140  (sound-font it) "Free Channels: ~a" (glaw:sound-nb-free-channels))
  (glaw:format-at 50 120  (sound-font it) "Used Channels: ~a" (glaw:sound-nb-used-channels))
  (glaw:format-at 50 100  (sound-font it) "FPS: ~a" (glaw:current-fps))
  (glaw:end-draw))

(defmethod update-example ((it sound) dt)
  (declare (ignore it dt))
  (glaw:update-sound))

(defmethod reshape-example ((it sound) w h)
  (glaw:update-2d-view (sound-view it) 0 0 w h))

