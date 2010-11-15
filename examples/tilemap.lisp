(in-package #:glaw-examples)

(defstruct tilemap
  (view (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*))
  tileset
  tilemap)

(defmethod init-example ((it tilemap))
  (glaw:load-asset "tileset3.png" :texture "tileset")
  (setf (tilemap-tileset it)
        (glaw:make-tileset :texture (glaw:use-resource "tileset")
                           :tile-width 32 :tile-height 32
                           :spacing 2 :margin 0 :start-index 0)
        (tilemap-tilemap it)
        (glaw:create-tilemap (glaw:tileset-tiles-width (tilemap-tileset it))
                             (glaw:tileset-tiles-height (tilemap-tileset it))))
  (format t "Tileset: ~ax~a:~a:~a -> ~ax~a~%"
          (glaw:tileset-pixel-width (tilemap-tileset it))
          (glaw:tileset-pixel-height (tilemap-tileset it))
          (glaw:tileset-margin (tilemap-tileset it))
          (glaw:tileset-spacing (tilemap-tileset  it))
          (glaw:tileset-tiles-width (tilemap-tileset it))
          (glaw:tileset-tiles-height (tilemap-tileset it)))
  (let ((tile-index (glaw:tileset-start-index (tilemap-tileset it))))
    (loop for i below (glaw:tilemap-nb-tiles (tilemap-tilemap it))
       do (setf (aref (glaw::tilemap-tiles (tilemap-tilemap it)) i) tile-index)
         (incf tile-index)
         (when (> tile-index (glaw::tileset-nb-tiles (tilemap-tileset it)))
           (setf tile-index 0)))))

(defmethod shutdown-example ((it tilemap))
  (glaw:dispose-asset "tileset"))

(defmethod render-example ((it tilemap))
  (glaw:begin-draw)
  (glaw:set-view-2d (tilemap-view it))
  (gl:with-pushed-matrix
    (gl:translate 100 400 0)
    (glaw:render-tilemap (tilemap-tilemap it) (tilemap-tileset it)))
  (glaw:with-resources ((fnt "default-font"))
    (glaw:format-at 50 100 fnt "FPS: ~a" (glaw:current-fps))
    (glaw:format-at 50 120 fnt "Tileset: ~ax~a:~a:~a -> ~ax~a~%"
                    (glaw:tileset-pixel-width (tilemap-tileset it))
                    (glaw:tileset-pixel-height (tilemap-tileset it))
                    (glaw:tileset-margin (tilemap-tileset it))
                    (glaw:tileset-spacing (tilemap-tileset  it))
                    (glaw:tileset-tiles-width (tilemap-tileset it))
                    (glaw:tileset-tiles-height (tilemap-tileset it))))
  (glaw:end-draw))

(defmethod update-example ((it tilemap) dt)
  (declare (ignore it dt)))

(defmethod reshape-example ((it tilemap) w h)
  (glaw:update-2d-view (tilemap-view it) 0 0 w h))

