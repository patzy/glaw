(in-package #:glaw-examples)

(defstruct mesh-viewer
  camera
  mesh)

(defmethod init-example ((it mesh-viewer))
  (glaw:setup-3d-defaults)
  (glaw:load-asset "monkey_smooth.obj" :mesh "mesh")
  (setf (mesh-viewer-camera it) (glaw:make-3d-view :fov (/ pi 3.0)
                                                   :near 1.0
                                                   :far 1000.0))
  (glaw:3d-view-translate (mesh-viewer-camera it) -200.0 0.0 0.0)
  (setf (mesh-viewer-mesh it) (glaw:use-resource "mesh"))
  (glaw:add-input-handler it))

(defmethod shutdown-example ((it mesh-viewer))
  (glaw:remove-input-handler it)
  (glaw:dispose-asset "mesh"))

(defmethod render-example ((it mesh-viewer))
  (glaw:begin-draw)
  (glaw:set-view-3d (mesh-viewer-camera it))
  (glaw::render-mesh (mesh-viewer-mesh it))
  (glaw:select-texture nil)
  (gl:disable :lighting)
  (glaw::draw-origin 1000.0)
  (gl:enable :lighting)
  (glaw:with-resources ((fnt "default-font"))
    (glaw:format-at 50 100 fnt "FPS: ~a" (glaw:current-fps)))
  (glaw:end-draw))

(defmethod update-example ((it mesh-viewer) dt)
  (declare (ignore it dt)))

(defmethod reshape-example ((it mesh-viewer) w h)
  (declare (ignore it w h))
  (glaw:reshape w h))

(glaw:key-handler (it mesh-viewer) (:left :press)
  (glaw:3d-view-translate (mesh-viewer-camera it) 0.0 5.0 0.0))

(glaw:key-handler (it mesh-viewer) (:right :press)
  (glaw:3d-view-translate (mesh-viewer-camera it) 0.0 -5.0 0.0))

(glaw:key-handler (it mesh-viewer) (:down :press)
  (glaw:3d-view-translate (mesh-viewer-camera it) 0.0 0.0 -5.0))

(glaw:key-handler (it mesh-viewer) (:up :press)
  (glaw:3d-view-translate (mesh-viewer-camera it) 0.0 0.0 5.0))

(glaw:key-handler (it mesh-viewer) (:p :press)
  (glaw:3d-view-translate (mesh-viewer-camera it) 5.0 0.0 0.0))

(glaw:key-handler (it mesh-viewer) (:m :press)
  (glaw:3d-view-translate (mesh-viewer-camera it) -5.0 0.0 0.0))

(glaw:key-handler (it mesh-viewer) (:o :press)
  (glaw:3d-view-roll (mesh-viewer-camera it) -0.1))

(glaw:key-handler (it mesh-viewer) (:l :press)
  (glaw:3d-view-roll (mesh-viewer-camera it) 0.1))

(glaw:key-handler (it mesh-viewer) (:i :press)
  (glaw:3d-view-pitch (mesh-viewer-camera it) -0.1))

(glaw:key-handler (it mesh-viewer) (:k :press)
  (glaw:3d-view-pitch (mesh-viewer-camera it) 0.1))

(glaw:key-handler (it mesh-viewer) (:u :press)
  (glaw:3d-view-yaw (mesh-viewer-camera it) -0.1))

(glaw:key-handler (it mesh-viewer) (:j :press)
  (glaw:3d-view-yaw (mesh-viewer-camera it) 0.1))

(glaw:key-handler (it mesh-viewer) (:v :press)
  (glaw:3d-view-point-at (mesh-viewer-camera it) #(0.0 0.0 0.0)))

(glaw:key-handler (it mesh-viewer) (:c :press)
   (glaw:3d-view-look-at (mesh-viewer-camera it) #(100.0 100.0 100.0)
                         #(0.0 0.0 0.0)))
