(in-package #:glaw-examples)


(defstruct screens
  (nb 0)
  view
  stack)

(defstruct test-screen
  (id 0)
  (last-dt 0)
  (text-x (random glaw:*display-width*))
  (text-y (random glaw:*display-height*)))

(defmethod glaw:init-screen ((it test-screen) &key)
  (format t "Initializing new screen: ~S~%" it))

(defmethod glaw:shutdown-screen ((it test-screen))
  (format t "Shutting down screen: ~S~%" it))

(defmethod glaw:update-screen ((it test-screen) dt)
  (setf (test-screen-last-dt it) dt))

(defmethod glaw:render-screen ((it test-screen))
  (glaw:with-resources ((fnt "default-font"))
    (glaw:format-at (test-screen-text-x it) (test-screen-text-y it)
                    fnt "Screen ~S: ~S"
                    (test-screen-id it) (test-screen-last-dt it))))

(glaw:key-handler (it screens) (:r :press)
  (incf (screens-nb it))
  (glaw:push-screen (make-test-screen :id (screens-nb it))
                    (screens-stack it)
                    :propagate-rendering t))

(glaw:key-handler (it screens) (:u :press)
  (incf (screens-nb it))
  (glaw:push-screen (make-test-screen :id (screens-nb it))
                    (screens-stack it)
                    :propagate-updating t))

(glaw:key-handler (it screens) (:b :press)
  (incf (screens-nb it))
  (glaw:push-screen (make-test-screen :id (screens-nb it))
                    (screens-stack it)
                    :propagate-rendering t
                    :propagate-updating t))

(glaw:key-handler (it screens) (:n :press)
  (incf (screens-nb it))
  (glaw:push-screen (make-test-screen :id (screens-nb it))
                    (screens-stack it)))

(glaw:key-handler (it screens) (:p :press)
  (unless (zerop (screens-nb it))
    (decf (screens-nb it))
    (glaw:pop-screen (screens-stack it))))


(defmethod init-example ((it screens))
  (setf (screens-view it) (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*)
        (screens-stack it) (glaw:make-screen-stack))
  (glaw:add-input-handler it))

(defmethod shutdown-example ((it screens))
  (glaw:remove-input-handler it))

(defmethod render-example ((it screens))
  (glaw:begin-draw)
  (glaw:set-view-2d (screens-view it))
  (glaw:render-screens (screens-stack it))
  (glaw:with-resources ((fnt "default-font"))
    (glaw:format-at 50 140 fnt
                    "Press R/U/B/N to push a new screen (with render/update/both/none propagation)")
    (glaw:format-at 50 120 fnt "Press P to pop current screen from stack.")
    (glaw:format-at 50 100 fnt "FPS: ~a" (glaw:current-fps)))
  (glaw:end-draw))

(defmethod update-example ((it screens) dt)
  (glaw:update-screens (screens-stack it) dt))

(defmethod reshape-example ((it screens) w h)
  (glaw:update-2d-view (screens-view it) 0 0 w h))

