(in-package #:glaw-examples)

(defun render-navmesh-cell (c)
  (glaw:select-texture nil)
  (glaw:set-color #(1.0 1.0 1.0 1.0))
  (gl:with-primitive :line-loop
    (loop for v in (glaw::polygon-vertices (glaw::navmesh-cell-polygon c))
       do (gl:vertex (glaw:point-2d-x v) (glaw:point-2d-y v))))
  (let ((center (glaw::navmesh-cell-center c)))
    ;; WTF? :line works on the laptop but not on the desktop !!!
    (gl:with-primitive :lines
      (loop for n in (glaw::navmesh-cell-neighbors c)
         do (let ((center-2 (glaw::navmesh-cell-center n)))
              (gl:color 1 0 0)
              (gl:vertex (glaw:point-2d-x center) (glaw:point-2d-y center))
              (gl:vertex (glaw:point-2d-x center-2) (glaw:point-2d-y center-2)))))))

(defun render-navmesh (nv)
  (loop for c in (glaw:navmesh-cells nv)
       do (render-navmesh-cell c)))

(defstruct pathfinding
  view
  navmesh
  (selected-index 0)
  selected-cell
  (moving :start)
  start-cell
  end-cell
  path)

(defmethod init-example ((it pathfinding))
  (glaw:add-input-handler it)
  (setf (pathfinding-view it)
        (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*)
        (pathfinding-navmesh it)
        (glaw:create-grid-navmesh 20 20 20))
  (setf (pathfinding-selected-cell it) (glaw:navmesh-cell (pathfinding-navmesh it) 0))
  (loop for y below 1000 by 10
     when (< 100 y 300)
     do (glaw:navmesh-remove-cell-at (pathfinding-navmesh it) 50 (1+ y)))
  (glaw:connect-grid-navmesh (pathfinding-navmesh it) 20))
;;  (glaw::simplify-navmesh (pathfinding-navmesh it)))

(defmethod shutdown-example ((it pathfinding))
  (glaw:remove-input-handler it))

(defmethod render-example ((it pathfinding))
  (glaw:begin-draw)
  (glaw:set-view-2d (pathfinding-view it))
  (render-navmesh (pathfinding-navmesh it))
  (glaw:set-color/rgb 0 0 1)
  (glaw:select-texture nil)
  (gl:with-primitive :line-loop
    (loop for v in (glaw::polygon-vertices
                    (glaw:navmesh-cell-polygon (pathfinding-selected-cell it)))
       do (gl:vertex (glaw:point-2d-x v) (glaw:point-2d-y v))))
  (when (pathfinding-start-cell it)
    (glaw:set-color/rgb 0 1 0)
    (gl:with-primitive :line-loop
      (loop for v in (glaw::polygon-vertices
                      (glaw:navmesh-cell-polygon (pathfinding-start-cell it)))
         do (gl:vertex (glaw:point-2d-x v) (glaw:point-2d-y v)))))
  (when (pathfinding-end-cell it)
    (glaw:set-color/rgb 1 0 0)
    (gl:with-primitive :line-loop
      (loop for v in (glaw::polygon-vertices
                      (glaw:navmesh-cell-polygon (pathfinding-end-cell it)))
         do (gl:vertex (glaw:point-2d-x v) (glaw:point-2d-y v)))))
  (when (pathfinding-path it)
    (glaw:set-color/rgb 0 0 1)
    (loop for c in (pathfinding-path it)
       do (gl:with-primitive :polygon
            (loop for v in (glaw::polygon-vertices
                            (glaw:navmesh-cell-polygon c))
               do (gl:vertex (glaw:point-2d-x v) (glaw:point-2d-y v))))))
  (glaw:set-color/rgb 1 1 1)
  (glaw:with-resources ((fnt "default-font"))
    (glaw:format-at 50 100 fnt "FPS: ~a" (glaw:current-fps)))
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
      (setf (pathfinding-path it) (glaw:find-path/nodes (pathfinding-navmesh it)
                                                        (pathfinding-start-cell it)
                                                        (pathfinding-end-cell it)))))


