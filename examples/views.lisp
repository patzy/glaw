(in-package #:glaw-examples)

(defstruct views
  2d
  3d
  isometric
  sprites
  cube
  (current :2d))

(defmethod init-example ((it views))
  (glaw:load-asset "lisplogo_alien_256.png" :texture "lisplogo")
  (glaw:load-asset "brick.png" :texture "cube")
  (setf (views-2d it) (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*)
        (views-isometric it) (glaw:create-isometric-view -100.0 -100.0 200.0 200.0)
        (views-3d it) (glaw:make-3d-view :fov (/ pi 3.0)
                                         :near 1.0
                                         :far 1000.0))
  (glaw:3d-view-translate (views-3d it) -200.0 0.0 0.0)
  (loop for i from 0 to 10 do
       (push (glaw:create-sprite (float (random glaw:*display-width*))
                                 (float (random glaw:*display-height*))
                           (+ 100.0 (random 100.0)) (+ 100.0 (random 100.0))
                           (glaw:use-resource "lisplogo")
                           :flip (glaw:random-nth '(:none :vertical :horizontal :both)))
             (views-sprites it)))
  (setf (views-cube it) (glaw:create-box-shape 100 100 100))
  (glaw:add-input-handler it))

(defmethod shutdown-example ((it views))
  (glaw:remove-input-handler it)
  (glaw:dispose-asset "lisplogo"))

(defmethod render-example ((it views))
  (case (views-current it)
    (:iso (glaw:setup-3d-defaults)
          (glaw:set-view-isometric (views-isometric it)))
    (:2d (glaw:setup-2d-defaults)
         (glaw:set-view-2d (views-2d it)))
    (:3d (glaw:setup-3d-defaults)
         (glaw:set-view-3d (views-3d it))))
  (dolist (sp (views-sprites it))
    (glaw:render-sprite sp))
  (gl:enable :lighting)
  (glaw:with-resources ((tex "cube"))
    (glaw:select-texture tex)
    (glaw:render-shape (views-cube it)))
  (glaw:select-texture nil)
  (gl:disable :lighting)
  (glaw::draw-origin 1000.0))

(defmethod update-example ((it views) dt)
  (declare (ignore it dt)))

(defmethod reshape-example ((it views) w h)
  (declare (ignore it w h)))

(glaw:key-handler (it views) (:n :press)
  (case (views-current it)
    (:2d (setf (views-current it) :iso)
         (glaw:setup-3d-defaults))
    (:iso (setf (views-current it) :3d)
         (glaw:setup-3d-defaults))
    (:3d (setf (views-current it) :2d)
         (glaw:setup-2d-defaults))))

(glaw:key-handler (it views) (:left :press)
  (case (views-current it)
    (:2d (glaw:move-2d-view (views-2d it) -5.0 0.0))
    (:iso (glaw:move-isometric-view (views-isometric it) -5.0 0.0))
    (:3d (glaw:3d-view-translate (views-3d it) 0.0 5.0 0.0))))

(glaw:key-handler (it views) (:right :press)
  (case (views-current it)
    (:2d (glaw:move-2d-view (views-2d it) 5.0 0.0))
    (:iso (glaw:move-isometric-view (views-isometric it) 5.0 0.0))
    (:3d (glaw:3d-view-translate (views-3d it) 0.0 -5.0 0.0))))

(glaw:key-handler (it views) (:down :press)
  (case (views-current it)
    (:2d (glaw:move-2d-view (views-2d it) 0.0 -5.0))
    (:iso (glaw:move-isometric-view (views-isometric it) 0.0 -5.0))
    (:3d (glaw:3d-view-translate (views-3d it) 0.0 0.0 -5.0))))

(glaw:key-handler (it views) (:up :press)
  (case (views-current it)
    (:2d (glaw:move-2d-view (views-2d it) 0.0 5.0))
    (:iso (glaw:move-isometric-view (views-isometric it) 0.0 5.0))
    (:3d (glaw:3d-view-translate (views-3d it) 0.0 0.0 5.0))))

(glaw:key-handler (it views) (:p :press)
  (case (views-current it)
    (:3d (glaw:3d-view-translate (views-3d it) 5.0 0.0 0.0))))

(glaw:key-handler (it views) (:m :press)
  (case (views-current it)
    (:3d (glaw:3d-view-translate (views-3d it) -5.0 0.0 0.0))))

(glaw:key-handler (it views) (:o :press)
  (case (views-current it)
    (:3d (glaw:3d-view-roll (views-3d it) -0.1))))

(glaw:key-handler (it views) (:l :press)
  (case (views-current it)
    (:3d (glaw:3d-view-roll (views-3d it) 0.1))))

(glaw:key-handler (it views) (:i :press)
  (case (views-current it)
    (:3d (glaw:3d-view-pitch (views-3d it) -0.1))))

(glaw:key-handler (it views) (:k :press)
  (case (views-current it)
    (:3d (glaw:3d-view-pitch (views-3d it) 0.1))))

(glaw:key-handler (it views) (:u :press)
  (case (views-current it)
    (:3d (glaw:3d-view-yaw (views-3d it) -0.1))))

(glaw:key-handler (it views) (:j :press)
  (case (views-current it)
    (:3d (glaw:3d-view-yaw (views-3d it) 0.1))))

(glaw:key-handler (it views) (:v :press)
  (case (views-current it)
    (:3d (glaw:3d-view-point-at (views-3d it) #(0.0 0.0 0.0)))))

(glaw:key-handler (it views) (:c :press)
  (case (views-current it)
    (:3d (glaw:3d-view-look-at (views-3d it) #(100.0 100.0 100.0)
                                              #(0.0 0.0 0.0)))))
