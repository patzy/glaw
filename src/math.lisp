(in-package #:glaw)

;;; Angles
(declaim (inline deg->rad))
(defun rad->deg (angle)
  (* angle (/ 180.0 pi)))

(declaim (inline deg->rad))
(defun deg->rad (angle)
  (* angle (/ pi 180.0)))

(defun between-pi (angle)
  (let ((restricted-angle (mod angle (* 2.0 pi))))
    (cond ((> restricted-angle pi) (- restricted-angle pi))
          ((< restricted-angle (- pi)) (+ restricted-angle pi))
          (t restricted-angle))))

;;; Generic vector operations
(defun vector-sum (&rest vecs)
  (apply #'map 'vector #'+ vecs))

(defun vector-diff (&rest vecs)
  (apply #'map 'vector #'- vecs))

(defun vector-scale (v factor)
  (map 'vector (lambda (item)
                 (* item factor))
       v))

;;; 2D vectors
(defstruct (vector-2d (:type (vector float)))
  (x 0.0)
  (y 0.0))

(defun make-vector-2d-from-list (coord-list)
  (make-vector-2d :x (first coord-list)
                  :y (second coord-list)))

(defun make-vector-2d-from-coords (&rest coords)
  (make-vector-2d-from-list coords))

(defun vector-2d-slope (v)
  (/ (vector-2d-y v)
     (vector-2d-x v)))

(defun vector-2d-dot-product (v1 v2)
  (+ (* (vector-2d-x v1) (vector-2d-x v2))
     (* (vector-2d-y v1) (vector-2d-y v2))))

(defun vector-2d-perp-dot-product (v1 v2)
  (vector-2d-dot-product (vector-2d-perp v1) v2))

(defun vector-2d-mag (v)
  (sqrt (vector-2d-dot-product v v)))

(defun vector-2d-normalize (v)
  (vector-2d-scale v (/ 1.0 (vector-2d-mag v))))

(defun vector-2d-null-p (v)
  (and (zerop (vector-2d-x v))
       (zerop (vector-2d-y v))))

(defun vector-2d-perp (v)
  (make-vector-2d :x (- (vector-2d-y v))
                  :y (vector-2d-x v)))

(defun vector-2d-opposite (v)
  (make-vector-2d :x (- (vector-2d-x v))
                  :y (- (vector-2d-y v))))

(defun vector-2d-rotate (v angle)
  (make-vector-2d :x (- (* (vector-2d-x v) (cos angle))
                        (* (vector-2d-y v) (sin angle)))
                  :y (+ (* (vector-2d-x v) (sin angle))
                        (* (vector-2d-y v) (cos angle)))))

(defun vector-2d-angle (v1 &optional (v2 (make-vector-2d :x 1 :y 0)))
  "Returns relative v1 angle relative to v2 in [-PI;+PI]."
  (atan (vector-2d-dot-product (vector-2d-perp v2) v1) (vector-2d-dot-product v2 v1)))

(defun vector-2d-sum (v1 v2)
  (vector-sum v1 v2))

(defun vector-2d-add (v1 v2)
  (incf (vector-2d-x v1) (vector-2d-x v2))
  (incf (vector-2d-y v1) (vector-2d-y v2))
  v1)

(defun vector-2d-diff (v1 v2)
  (vector-diff v1 v2))

(defun vector-2d-sub (v1 v2)
  (decf (vector-2d-x v1) (vector-2d-x v2))
  (decf (vector-2d-y v1) (vector-2d-y v2))
  v1)

(defun vector-2d-scale (v factor)
  (vector-scale v factor))

;;;; 2D points
(defstruct (point-2d (:type (vector float))
                     (:include vector-2d)))

(defun make-point-2d-from-polar (distance angle)
  (let ((s (sin angle))
        (c (cos angle)))
    (make-point-2d :x (* distance c)
                   :y (* distance s))))

(defun point-2d-angle (p)
  (vector-2d-angle p))

(defun point-2d-distance (p)
  (vector-2d-mag p))

;;; 3D vectors
(defstruct (vector-3d (:type (vector float)))
  (x 0.0)
  (y 0.0)
  (z 0.0))

(defun make-vector-3d-from-list (coord-list)
  (make-vector-3d :x (first coord-list)
                  :y (second coord-list)
                  :z (third coord-list)))

(defun make-vector-3d-from-coords (&rest coords)
  (make-vector-3d-from-list coords))

(defun vector-3d-dot-product (v1 v2)
  (+ (* (vector-3d-x v1) (vector-3d-x v2))
     (* (vector-3d-y v1) (vector-3d-y v2))
     (* (vector-3d-z v1) (vector-3d-z v2))))

;; (defun vector-3d-perp-dot-product (v1 v2)
;;   (vector-3d-dot-product (vector-3d-perp v1) v2))

(defun vector-3d-mag (v)
  (sqrt (vector-3d-dot-product v v)))

(defun vector-3d-normalize (v)
  (vector-3d-scale v (/ 1.0 (vector-3d-mag v))))

(defun vector-3d-null-p (v)
  (and (zerop (vector-3d-x v))
       (zerop (vector-3d-y v))
       (zerop (vector-3d-z v))))

(defun vector-3d-opposite (v)
  (make-vector-3d :x (- (vector-3d-x v))
                  :y (- (vector-3d-y v))
                  :z (- (vector-3d-z v))))

(defun vector-3d-sum (v1 v2)
  (vector-sum v1 v2))

(defun vector-3d-add (v1 v2)
  (incf (vector-3d-x v1) (vector-3d-x v2))
  (incf (vector-3d-y v1) (vector-3d-y v2))
  (incf (vector-3d-z v1) (vector-3d-z v2))
  v1)

(defun vector-3d-diff (v1 v2)
  (vector-diff v1 v2))

(defun vector-3d-sub (v1 v2)
  (decf (vector-3d-x v1) (vector-3d-x v2))
  (decf (vector-3d-y v1) (vector-3d-y v2))
  (decf (vector-3d-z v1) (vector-3d-z v2))
  v1)

(defun vector-3d-scale (v factor)
  (vector-scale v factor))

(defun vector-3d-project-xy (v)
  (make-vector-2d :x (vector-3d-x v)
                  :y (vector-3d-y v)))

(defun vector-3d-project-xz (v)
  (make-vector-2d :x (vector-3d-x v)
                  :y (vector-3d-z v)))

(defun vector-3d-project-yz (v)
  (make-vector-2d :x (vector-3d-y v)
                  :y (vector-3d-z v)))

;;; 3D points
(defstruct (point-3d (:include vector-3d)
                     (:type (vector float))))

(defun make-point-3d-from-spherical (distance yaw pitch)
  (let* ((sp (sin pitch))
         (cp (cos pitch))
         (sy (sin yaw))
         (cy (cos yaw))
         (dxy (* distance cp)))
    (make-point-3d :x (* dxy cy)
                   :y (* dxy sy)
                   :z (* (- distance) sp))))


;;; Orientation
(defstruct (orientation (:type (vector float)))
  (roll 0.0)
  (pitch 0.0)
  (yaw 0.0))

(defun make-orientation-from-vector-3d (v)
  (let ((dxy (sqrt (* (vector-3d-x v) (vector-3d-x v))
                   (* (vector-3d-y v) (vector-3d-y v)))))
    (if (zerop dxy)
        (make-orientation)
        (make-orientation :yaw (atan (vector-3d-x v) (vector-3d-y v))
                          :pitch (atan (vector-3d-z v) dxy)
                          :roll 0.0))))

;;; Axis
(defstruct (axis (:type (vector float))
                 (:include vector-3d)))

(defconstant +x-axis+ #(1.0 0.0 0.0))
(defconstant +y-axis+ #(0.0 1.0 0.0))
(defconstant +z-axis+ #(0.0 0.0 1.0))


;;; Quaternions
(defstruct (quaternion (:type (vector float)))
  (x 0.0) (y 0.0) (z 0.0) (w 0.0))

(defun make-quaternion-from-axis-angle (&key (axis +x-axis+) (angle 0.0))
  (make-quaternion :x (* (sin (/ angle 2.0)) (axis-x axis))
                   :y (* (sin (/ angle 2.0)) (axis-y axis))
                   :z (* (sin (/ angle 2.0)) (axis-z axis))
                   :w (cos (/ angle 2.0))))

(defun make-quaternion-from-angles (&key (roll 0.0) (pitch 0.0) (yaw 0.0))
  (let ((cr (cos (* 0.5 roll)))
        (cp (cos (* 0.5 pitch)))
        (cy (cos (* 0.5 yaw)))
        (sr (sin (* 0.5 roll)))
        (sp (sin (* 0.5 pitch)))
        (sy (sin (* 0.5 yaw))))
    (make-quaternion :x (- (* cr cp sy) (* sr sp cy))
                     :y (+ (* cr sp cy) (* sr cp sy))
                     :z (- (* sr cp cy) (* cr sp sy))
                     :w (+ (* cr cp cy) (* sr sp sy)))))

(defun make-quaternion-from-orientation (orientation)
  (make-quaternion-from-angles (orientation-roll orientation)
                               (orientation-pitch orientation)
                               (orientation-yaw orientation)))

(defun quaternion-mag (q)
  (sqrt (* (quaternion-x q) (quaternion-x q))
        (* (quaternion-y q) (quaternion-y q))
        (* (quaternion-z q) (quaternion-z q))
        (* (quaternion-w q) (quaternion-w q))))

(defun quaternion-scale (q factor)
  (setf (quaternion-x q) (* (quaternion-x q) factor)
        (quaternion-y q) (* (quaternion-y q) factor)
        (quaternion-z q) (* (quaternion-z q) factor)
        (quaternion-w q) (* (quaternion-w q) factor)))

(defun quaternion-normalize (q)
  (quaternion-scale q (quaternion-mag q)))

(defun quaternion-sum (q1 q2)
  (vector-sum q1 q2))

(defun quaternion-add (q1 q2)
  (incf (quaternion-x q1) (quaternion-x q2))
  (incf (quaternion-y q1) (quaternion-y q2))
  (incf (quaternion-z q1) (quaternion-z q2))
  (incf (quaternion-w q1) (quaternion-w q2))
  q1)

(defun quaternion-diff (q1 q2)
  (vector-diff q1 q2))

(defun quaternion-sub (q1 q2)
  (decf (quaternion-x q1) (quaternion-x q2))
  (decf (quaternion-y q1) (quaternion-y q2))
  (decf (quaternion-z q1) (quaternion-z q2))
  (decf (quaternion-w q1) (quaternion-w q2))
  q1)

(defun quaternion-mult (q1 q2)
  (make-quaternion :x (+ (* (quaternion-w q1) (quaternion-x q2))
                         (* (quaternion-x q1) (quaternion-w q2))
                         (* (quaternion-y q1) (quaternion-z q2))
                         (- (* (quaternion-z q1) (quaternion-y q2))))
                   :y (+ (* (quaternion-w q1) (quaternion-y q2))
                         (- (* (quaternion-x q1) (quaternion-z q2)))
                         (* (quaternion-y q1) (quaternion-w q2))
                         (* (quaternion-z q1) (quaternion-x q2)))
                   :z (+ (* (quaternion-w q1) (quaternion-z q2))
                         (* (quaternion-x q1) (quaternion-y q2))
                         (- (* (quaternion-y q1) (quaternion-x q2)))
                         (* (quaternion-z q1) (quaternion-w q2)))
                   :w (- (* (quaternion-w q1) (quaternion-w q2))
                         (* (quaternion-x q1) (quaternion-x q2))
                         (* (quaternion-y q1) (quaternion-y q2))
                         (* (quaternion-z q1) (quaternion-z q2)))))

;;; Matrix (column-major)
(defstruct (matrix (:type (vector float)))
  "3D transformation matrix."
  (r00 1.0) (r10 0.0) (r20 0.0) (z0 0.0)
  (r01 0.0) (r11 1.0) (r21 0.0) (z1 0.0)
  (r02 0.0) (r12 0.0) (r22 1.0) (z2 0.0)
  (tx 0.0) (ty 0.0) (tz 0.0) (one 1.0))

(defconstant +matrix-identity+
  #(1.0 0.0 0.0 0.0
    0.0 1.0 0.0 0.0
    0.0 0.0 1.0 0.0
    0.0 0.0 0.0 1.0))

;;; Spatial Basis
(defstruct (basis (:include matrix)
                  (:type (vector float)))
  "Transformation matrix without scale.")

(defun basis-local-x (basis)
  (make-axis :x (basis-r00 basis)
             :y (basis-r10 basis)
             :z (basis-r20 basis)))

(defun basis-local-y (basis)
  (make-axis :x (basis-r01 basis)
             :y (basis-r11 basis)
             :z (basis-r21 basis)))

(defun basis-local-z (basis)
  (make-axis :x (basis-r02 basis)
             :y (basis-r12 basis)
             :z (basis-r22 basis)))

(defun basis-position (basis)
  (make-point-3d :x (basis-tx basis)
                 :y (basis-ty basis)
                 :z (basis-tz basis)))

(defun basis-set-position (basis pos)
  (setf (basis-tx basis) (point-3d-x pos)
        (basis-ty basis) (point-3d-y pos)
        (basis-tz basis) (point-3d-z pos)))

(defsetf basis-position basis-set-position)

(defun basis-translate (basis dx dy dz)
  (incf (basis-tx basis) (+ (* dx (basis-r00 basis))
                            (* dy (basis-r01 basis))
                            (* dz (basis-r02 basis))))
  (incf (basis-ty basis) (+ (* dx (basis-r10 basis))
                            (* dy (basis-r11 basis))
                            (* dz (basis-r12 basis))))
  (incf (basis-tz basis) (+ (* dx (basis-r20 basis))
                            (* dy (basis-r21 basis))
                            (* dz (basis-r22 basis)))))

(defun basis-cancel-rotation (basis)
  (setf (basis-r00) 1.0
        (basis-r11) 1.0
        (basis-r22) 1.0
        (basis-r01) 0.0
        (basis-r02) 0.0
        (basis-r10) 0.0
        (basis-r12) 0.0
        (basis-r20) 0.0
        (basis-r21) 0.0))

(defconstant +basis-sin-precision+ 0.999999999) ;; 1.0-1.0e-9

;;      _                                _
;;     |  c2c3  s1s2c3-c1s3  s1s3+c1s2c3  |  s1,c1 : roll
;;     |  c2s3  c1c3+s1s2s3  c1s2s3-s1c3  |  s2,c2 : pitch
;;     |_  -s2         s1c2         c1c2 _|  s3,c3 : yaw
(defun basis-zyx-orientation (basis)
  (cond
    ((<= (basis-r20 basis) (- +basis-sin-precision+))
     (make-orientation :roll 0.0
                       :pitch (/ pi 2.0)
                       :yaw (atan (- (basis-r01 basis)) (basis-r11 basis))))
    ((>= (basis-r20 basis) +basis-sin-precision+)
     (make-orientation :roll 0.0
                       :pitch (- (/ pi 2.0))
                       :yaw (atan (- (basis-r01 basis)) (basis-r11 basis))))
    (t
     (make-orientation :roll (atan (basis-r21 basis) (basis-r22 basis))
                       :pitch (asin (- (basis-r20 basis)))
                       :yaw (atan (basis-r10 basis) (basis-r00 basis))))))

(defun basis-set-zyx-orientation (basis orientation)
  (let ((s1 (sin (orientation-roll orientation)))
        (c1 (cos (orientation-roll orientation)))
        (s2 (sin (orientation-pitch orientation)))
        (c2 (cos (orientation-pitch orientation)))
        (s3 (sin (orientation-yaw orientation)))
        (c3 (cos (orientation-yaw orientation))))
    (setf (basis-r00 basis) (* c2 c3)
          (basis-r01 basis) (- (* s1 s2 c3) (* c1 s3))
          (basis-r02 basis) (+ (* s1 s3) (* c1 s2 c3))
          (basis-r10 basis) (* c2 s3)
          (basis-r11 basis) (+ (* c1 c3) (* s1 s2 s3))
          (basis-r12 basis) (- (* c1 s2 s3) (* s1 c3))
          (basis-r20 basis) (- s2)
          (basis-r21 basis) (* s1 c2)
          (basis-r22 basis) (* c1 c2))))

(defsetf basis-zyx-orientation basis-set-zyx-orientation)

;;      _                                 _
;;     |         c2c3        -c2s3     s2  |  s1,c1 : roll
;;     |  s1s2c3+c1s3  c1c3-s1s2s3  -s1c2  |  s2,c2 : pitch
;;     |_ s1s3-c1s2c3  c1s2s3+s1c3   c1c2 _|  s3,c3 : yaw
(defun basis-xyz-orientation (basis)
  (cond
    ((<= (basis-r02 basis) (- +basis-sin-precision+))
     (make-orientation :roll 0.0
                       :pitch (- (/ pi 2.0))
                       :yaw (atan (basis-r10 basis) (basis-r11 basis))))
    ((>= (basis-r02 basis) +basis-sin-precision+)
     (make-orientation :roll 0.0
                       :pitch (/ pi 2.0)
                       :yaw (atan (basis-r10 basis) (basis-r11 basis))))
    (t
     (make-orientation :roll (atan (- (basis-r12 basis)) (basis-r22 basis))
                       :pitch (asin (basis-r02 basis))
                       :yaw (atan (- (basis-r01 basis)) (basis-r00 basis))))))

(defun basis-set-xyz-orientation (basis orientation)
  (let ((s1 (sin (orientation-roll orientation)))
        (c1 (cos (orientation-roll orientation)))
        (s2 (sin (orientation-pitch orientation)))
        (c2 (cos (orientation-pitch orientation)))
        (s3 (sin (orientation-yaw orientation)))
        (c3 (cos (orientation-yaw orientation))))
    (setf (basis-r00 basis) (* c2 c3)
          (basis-r01 basis) (- (* c2 s3))
          (basis-r02 basis) s2
          (basis-r10 basis) (+ (* s1 s2 c3) (* c1 s3))
          (basis-r11 basis) (- (* c1 c3) (* s1 s2 s3))
          (basis-r12 basis) (- (* s1 c2))
          (basis-r20 basis) (- (* s1 s3) (* c1 s2 c3))
          (basis-r21 basis) (+ (* c1 s2 s3) (* s1 c3))
          (basis-r22 basis) (* c1 c2))))

(defsetf basis-xyz-orientation basis-set-xyz-orientation)

;;; Projection matrices
(defun matrix-set-ortho (mtx left right bottom top near far)
  (setf (matrix-r00 mtx) (/ 2.0 (- right left))
        (matrix-r01 mtx) 0.0
        (matrix-r02 mtx) 0.0
        (matrix-r10 mtx) 0.0
        (matrix-r11 mtx) (/ 2.0 (- top bottom))
        (matrix-r12 mtx) 0.0
        (matrix-r20 mtx) 0.0
        (matrix-r21 mtx) 0.0
        (matrix-r22 mtx) (/ -2.0 (- far near))
        (matrix-tx mtx) (- (/ (+ right left) (- right left)))
        (matrix-ty mtx) (- (/ (+ top bottom) (- top bottom)))
        (matrix-tz mtx) (- (/ (+ far near) (- far near)))
        (matrix-z0 mtx) 0.0
        (matrix-z1 mtx) 0.0
        (matrix-z2 mtx) 0.0
        (matrix-one mtx) 1.0)
  mtx)

(defun make-ortho-matrix (left right bottom top near far)
  (matrix-set-ortho (make-matrix) left right bottom top near far))

(defun matrix-set-frustum (mtx left right bottom top near far)
  (setf (matrix-r00 mtx) (/ (* 2.0 near) (- right left))
        (matrix-r01 mtx) 0.0
        (matrix-r02 mtx) (/ (+ right left) (- right left))
        (matrix-r10 mtx) 0.0
        (matrix-r11 mtx) (/ (* 2.0 near) (- top bottom))
        (matrix-r12 mtx) (/ (+ top bottom) (- top bottom))
        (matrix-r20 mtx) 0.0
        (matrix-r21 mtx) 0.0
        (matrix-r22 mtx) (/ (+ near far) (- near far))
        (matrix-tx mtx) 0.0
        (matrix-ty mtx) 0.0
        (matrix-tz mtx) (/ (* 2.0 near far) (- near far))
        (matrix-z0 mtx) 0.0
        (matrix-z1 mtx) 0.0
        (matrix-z2 mtx) -1.0
        (matrix-one mtx) 0.0))

(defun make-frustum-matrix (left right bottom top near far)
  (matrix-set-frustum (make-matrix) left right bottom top near far))


;;; 3D Perspective
(defstruct (perspective (:constructor %make-perspective))
  fov
  ratio
  near
  far
  up-slope
  down-slope
  left-slope
  right-slope)

(defun %perspective-update-slopes (p)
  (let* ((up-down-slope (tan (* 0.5 (perspective-fov p))))
         (left-right-slope (* (perspective-ratio p) up-down-slope)))
    (setf (perspective-up-slope p) up-down-slope
          (perspective-down-slope p) (- up-down-slope)
          (perspective-left-slope p) left-right-slope
          (perspective-right-slope p) (- left-right-slope))))

(defun make-perspective (&key (fov (/ pi 3.0))
                              (ratio 1.0)
                              (near 0.1)
                              (far 100.0))
  (let ((persp (%make-perspective :fov fov
                                  :ratio ratio
                                  :near near
                                  :far far)))
    (%perspective-update-slopes persp)))

(defun perspective-set-fov (p fov)
  (setf (perspective-fov p) fov)
  (%perspective-update-slopes p))

(defsetf perspective-fov perspective-set-fov)

(defun perspective-set-ratio (p ratio)
  (setf (perspective-ratio p) ratio)
  (%perspective-update-slopes p))

(defsetf perspective-ratio perspective-set-ratio)

(defun perspective-left (p)
  (- (* (perspective-near p) (perspective-left-slope p))))

(defun perspective-right (p)
  (- (* (perspective-near p) (perspective-right-slope p))))

(defun perspective-bottom (p)
  (* (perspective-near p) (perspective-down-slope p)))

(defun perspective-top (p)
  (* (perspective-near p) (perspective-up-slope p)))

(defun apply-perspective (p mtx)
  (matrix-set-frustum mtx (perspective-left p)
                          (perspective-right p)
                          (perspective-bottom p)
                          (perspective-top p)
                          (perspective-near p)
                          (perspective-far p)))
