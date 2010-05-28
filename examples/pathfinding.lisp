(in-package #:glaw-examples)

(defstruct pathfinding
  (font nil)
  (view (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*))
  (navmesh (glaw:create-grid-navmesh 20 20 20))
  (selected-index 0)
  selected-cell
  (moving :start)
  start-cell
  end-cell
  path)

(defmethod init-example ((it pathfinding))
  (glaw:add-input-handler it)
  (glaw:load-asset "font.png" :fixed-bitmap-font)
  (setf (pathfinding-font it) (glaw:use-resource "font.png"))
  (setf (pathfinding-selected-cell it) (glaw:navmesh-cell (pathfinding-navmesh it) 0))
  (loop for y below 1000 by 10
     when (< 100 y 300)
     do (glaw:navmesh-remove-cell-at (pathfinding-navmesh it) 50 (1+ y)))
  (glaw:connect-grid-navmesh (pathfinding-navmesh it) 20)
  ;;(glaw:simplify-navmesh (pathfinding-navmesh it))
  )

(defmethod shutdown-example ((it pathfinding))
  (glaw:remove-input-handler it)
  (glaw:dispose-asset "font.png"))

(defmethod render-example ((it pathfinding))
  (glaw:begin-draw)
  (glaw:set-view-2d (pathfinding-view it))
  (glaw:render-navmesh (pathfinding-navmesh it))
  (glaw:set-color/rgb 0 0 1)
  (glaw:select-texture nil)
  (gl:with-primitive :line-loop
    (loop for v in (glaw:navcell-vertices (pathfinding-selected-cell it))
       do (gl:vertex (first v)
                     (second v))))
  (when (pathfinding-start-cell it)
    (glaw:set-color/rgb 0 1 0)
    (gl:with-primitive :line-loop
      (loop for v in (glaw:navcell-vertices (pathfinding-start-cell it))
         do (gl:vertex (first v)
                       (second v)))))
  (when (pathfinding-end-cell it)
    (glaw:set-color/rgb 1 0 0)
    (gl:with-primitive :line-loop
      (loop for v in (glaw:navcell-vertices (pathfinding-end-cell it))
         do (gl:vertex (first v)
                       (second v)))))
  (when (pathfinding-path it)
    (glaw:set-color/rgb 0 0 1)
    (loop for c in (pathfinding-path it)
       do (gl:with-primitive :polygon
            (loop for v in (glaw:navcell-vertices c)
               do (gl:vertex (first v)
                             (second v))))))
  (glaw:set-color/rgb 1 1 1)
  (glaw:format-at 50 100  (pathfinding-font it) "FPS: ~a/~a/~a"
                  (glaw:min-fps) (glaw:current-fps) (glaw:max-fps))
  (glaw:end-draw))

(defmethod update-example ((it pathfinding) dt)
  (declare (ignore it dt)))

(defmethod reshape-example ((it pathfinding) w h)
  (glaw:update-2d-view (pathfinding-view it) 0 0 w h))

(glaw:key-handler (it pathfinding) (:left :press)
   (incf (pathfinding-selected-index it))
   (when (> (pathfinding-selected-index it) (- (glaw:navmesh-nb-cells (pathfinding-navmesh it)) 1))
     (setf (pathfinding-selected-index it) 0))
   (format t "Selected index: ~S~%" (pathfinding-selected-index it))
   (setf (pathfinding-selected-cell it) (glaw:navmesh-cell (pathfinding-navmesh it)
                                                           (pathfinding-selected-index it))))

(glaw:key-handler (it pathfinding) (:right :press)
   (decf (pathfinding-selected-index it))
   (when (< (pathfinding-selected-index it) 0)
     (setf (pathfinding-selected-index it) (- (glaw:navmesh-nb-cells (pathfinding-navmesh it)) 1)))
   (format t "Selected index: ~S~%" (pathfinding-selected-index it))
   (setf (pathfinding-selected-cell it) (glaw:navmesh-cell (pathfinding-navmesh it)
                                                            (pathfinding-selected-index it))))

(glaw:key-handler (it pathfinding) (:down :press)
   (incf (pathfinding-selected-index it) 20)
   (when (> (pathfinding-selected-index it) (- (glaw:navmesh-nb-cells (pathfinding-navmesh it)) 1))
     (setf (pathfinding-selected-index it) 0))
   (format t "Selected index: ~S~%" (pathfinding-selected-index it))
   (setf (pathfinding-selected-cell it) (glaw:navmesh-cell (pathfinding-navmesh it)
                                                            (pathfinding-selected-index it))))

(glaw:key-handler (it pathfinding) (:up :press)
   (decf (pathfinding-selected-index it) 20)
   (when (< (pathfinding-selected-index it) 0)
     (setf (pathfinding-selected-index it) (- (glaw:navmesh-nb-cells (pathfinding-navmesh it)) 1)))
   (format t "Selected index: ~S~%" (pathfinding-selected-index it))
   (setf (pathfinding-selected-cell it) (glaw:navmesh-cell (pathfinding-navmesh it)
                                                            (pathfinding-selected-index it))))

(glaw:key-handler (it pathfinding) (:t :press)
    (format t "Switching cell selection from: ~S~%" (pathfinding-moving it))
    (case (pathfinding-moving it)
      (:start (setf (pathfinding-moving it) :end))
      (:end (setf (pathfinding-moving it) :start))
      (otherwise (error "Should be :start or :end~%"))))

(glaw:key-handler (it pathfinding) (:space :press)
    (case (pathfinding-moving it)
      (:start (setf (pathfinding-start-cell it) (pathfinding-selected-cell it)))
      (:end (setf (pathfinding-end-cell it) (pathfinding-selected-cell it)))
      (otherwise (error "Should be :start or :end~%")))
    (when (and (pathfinding-start-cell it) (pathfinding-end-cell it))
      (setf (pathfinding-path it) (glaw:find-path/cells (pathfinding-navmesh it)
                                                         (pathfinding-start-cell it)
                                                         (pathfinding-end-cell it)))))


