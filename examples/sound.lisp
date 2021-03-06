(in-package #:glaw-examples)


(defstruct sound
  view
  snd)

(glaw:key-handler (it sound) (:space :press)
   (glaw:play-sound (sound-snd it)))

(defmethod init-example ((it sound))
  (glaw:init-sound)
  (glaw:load-asset "pew.wav" :sound)
  (setf (sound-view it) (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*)
        (sound-snd it) (glaw:use-resource "pew.wav"))
  (format t "Nb channels: ~S~%" (glaw:sound-nb-channels))
  (glaw:add-input-handler it))

(defmethod shutdown-example ((it sound))
  (glaw:dispose-asset "pew.wav")
  (glaw:remove-input-handler it)
  (glaw:shutdown-sound))

(defmethod render-example ((it sound))
  (glaw:set-view-2d (sound-view it))
  (glaw:with-resources ((fnt "default-font"))
    (glaw:format-at 50 160 fnt "Press spacebar for PEW PEW !!!")
    (glaw:format-at 50 140 fnt "Free Channels: ~a" (glaw:sound-nb-free-channels))
    (glaw:format-at 50 120 fnt "Used Channels: ~a" (glaw:sound-nb-used-channels))))

(defmethod update-example ((it sound) dt)
  (declare (ignore it dt))
  (glaw:update-sound))

(defmethod reshape-example ((it sound) w h)
  (glaw:update-2d-view (sound-view it) 0 0 w h))

