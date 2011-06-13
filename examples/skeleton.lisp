(in-package #:glaw-examples)

(defstruct skeletons
  (view (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*))
  (skeleton (glaw::make-skeleton))
  anim-state animation)

(defmethod init-example ((it skeletons))
  (let ((skel (skeletons-skeleton it)))
    (glaw::skeleton-add-bone skel (glaw::make-bone :length (random 100.0) :angle 10.0))
    (glaw::skeleton-add-bone skel (glaw::make-bone :length (random 100.0) :angle 20.0))
    (glaw::skeleton-add-bone skel (glaw::make-bone :length (random 100.0) :angle 70.0))
    (glaw::skeleton-add-bone skel (glaw::make-bone :length (random 100.0) :angle 120.0)
                             :name "joe")
    (glaw::skeleton-add-bone skel (glaw::make-bone :length (random 100.0) :angle 250.0)
                             :parent-name "joe")
    (glaw::skeleton-add-bone skel (glaw::make-bone :length (random 100.0) :angle 90.0)
                             :parent-name "joe")
    (glaw::skeleton-add-bone skel (glaw::make-bone :length (random 100.0) :angle 120.0)
                             :parent-name "joe"))
  (setf (skeletons-animation it) (glaw:make-keyframe-anim
                                 :frame-time 0.04
                                 :start-frame 0
                                 :nb-frames 36
                                 :hints '((:angle . 0))
       :channels (list (loop for i below 360.0 by 10.0
                            collect i))))
  (setf (skeletons-anim-state it) (glaw:make-anim-state :animation (skeletons-animation it)))
  (glaw:anim-state-apply (skeletons-anim-state it)
                         (glaw::find-bone (skeletons-skeleton it) "joe"))
  (format t "Skeleton: ~S~%" (skeletons-skeleton it)))


(defmethod shutdown-example ((it skeletons)))

(defmethod render-example ((it skeletons))
  (glaw:set-view-2d (skeletons-view it))
  (glaw:set-color/rgb 1.0 1.0 1.0)
  (gl:with-pushed-matrix
      (gl:translate 300 300 0)
    (glaw::render-skeleton (skeletons-skeleton it))))

(defmethod update-example ((it skeletons) dt)
  (glaw:anim-state-update (skeletons-anim-state it) dt)
  (glaw:anim-state-apply (skeletons-anim-state it)
                         (glaw::find-bone (skeletons-skeleton it) "joe")))

(defmethod reshape-example ((it skeletons) w h)
  (glaw:update-2d-view (skeletons-view it) 0 0 w h))

