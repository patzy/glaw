(in-package #:glaw-examples)


(defstruct input
  (font nil)
  (view (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*))
  (strings (list "'g' repeat" "x-c-v-b sequence" "vb chord"))
  (processors (list (glaw:make-input-repeat :input :g
                                            :output :g-repeat
                                            :delay 0.5)
                    (glaw:make-input-sequence :inputs '(:x :c :v :b)
                                              :output :xcvb-seq
                                              :delay 0.5)
                    (glaw:make-input-chord :inputs '(:v :b)
                                           :output :vb-chord
                                           :delay 1.0))))

(glaw:input-handler (it input) :g-repeat
   (format t "G Repeat !!!~%"))

(glaw:input-handler (it input) :xcvb-seq
   (format t "XCVB Sequence !!!~%"))

(glaw:input-handler (it input) :vb-chord
   (format t "VB Chord !!!~%"))

(defmethod init-example ((it input))
  (glaw:load-asset "font.png" :fixed-bitmap-font "font")
  (setf (input-font it) (glaw:use-resource "font"))
  (glaw:add-input-handler it)
  (dolist (proc (input-processors it))
    (glaw:input-processor-reset proc)
    (glaw:add-input-handler proc)))

(defmethod shutdown-example ((it input))
  (glaw:remove-input-handler it)
  (dolist (proc (input-processors it))
    (glaw:remove-input-handler proc))
  (glaw:dispose-asset "font"))

(defmethod render-example ((it input))
  (glaw:begin-draw)
  (glaw:set-view-2d (input-view it))
  (loop for str in (input-strings it)
       for proc in (input-processors it)
       with y = 130 do
       (glaw:format-at 50 y  (input-font it) "~a: ~a"
                       str
                       (glaw:input-processor-valid-p proc))
       (incf y (glaw::font-line-height (input-font it))))
  (glaw:format-at 50 100  (input-font it) "FPS: ~a" (glaw:current-fps))
  (glaw:end-draw))

(defmethod update-example ((it input) dt)
  (declare (ignore it dt)))

(defmethod reshape-example ((it input) w h)
  (glaw:update-2d-view (input-view it) 0 0 w h))

