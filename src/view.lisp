(in-package #:glaw)

;;; Base view
(defstruct view
  proj-mtx
  eye-mtx)

(defun set-view (view)
  (set-view-matrices (view-proj-mtx view) (view-eye-mtx view)))

;;; 2D view
(defstruct (2d-view (:include view))
  left right bottom top
  (zoom 1.0))

(defun %2d-view-update-matrix (view)
  (matrix-set-ortho (2d-view-proj-mtx view)
                    (float (2d-view-left view))
                    (float (2d-view-right view))
                    (float (2d-view-bottom view))
                    (float (2d-view-top view))
                    -1.0 1.0))

(defun create-2d-view (x y width height)
  (let ((view (make-2d-view :left x :right (+ x width)
                            :bottom y :top (+ y height)
                            :proj-mtx (make-matrix)
                            :eye-mtx +matrix-identity+)))
    (%2d-view-update-matrix view)
    view))

(defmacro with-fullscreen-view-2d (&body body)
  `(progn (set-view-2d (create-2d-view 0 0 *display-width* *display-height*))
          ,@body))

(defun 2d-view-width (view)
  (- (2d-view-right view) (2d-view-left view)))

(defmethod (setf 2d-view-width) (value (view 2d-view))
  (setf (2d-view-right view) (+ (2d-view-left view) value))
  (%2d-view-update-matrix view))

(defun 2d-view-height (view)
  (- (2d-view-top view) (2d-view-bottom view)))

(defmethod (setf 2d-view-height) (value (view 2d-view))
  (setf (2d-view-top view) (+ (2d-view-bottom view) value))
  (%2d-view-update-matrix view))

(defun zoom-2d-view (view dfactor &key lock-left lock-bottom lock-right lock-top)
  (let ((width-diff (* dfactor (2d-view-width view)))
        (height-diff (* dfactor (2d-view-height view))))
    (unless lock-left
      (decf (2d-view-left view) width-diff))
    (unless lock-bottom
      (decf (2d-view-bottom view) height-diff))
    (unless lock-right
      (incf (2d-view-right view) width-diff))
    (unless lock-top
      (incf (2d-view-top view) height-diff))
    (incf (2d-view-zoom view) dfactor))
    (%2d-view-update-matrix view))

(defun move-2d-view (view dx dy)
  (incf (2d-view-left view) dx)
  (incf (2d-view-right view) dx)
  (incf (2d-view-bottom view) dy)
  (incf (2d-view-top view) dy)
  (%2d-view-update-matrix view))

(defun update-2d-view (view x y width height)
  (setf (2d-view-left view) x)
  (setf (2d-view-bottom view) y)
  (setf (2d-view-right view) (+ x width))
  (setf (2d-view-top view) (+ y height))
  (%2d-view-update-matrix view))

(defun set-view-2d (view)
  (set-view view))

(defun view-to-view (x y from-view to-view &optional (absolute t))
  (unless (or (zerop (2d-view-width from-view)) (zerop (2d-view-height from-view))
              (zerop (2d-view-width to-view)) (zerop (2d-view-height to-view)))
    (let ((width-factor (/ (2d-view-width to-view) (2d-view-width from-view)))
          (height-factor (/ (2d-view-height to-view) (2d-view-height from-view))))
      (if absolute
          (values (float (+ (2d-view-left to-view)
                            (* x width-factor)))
                  (float (+ (2d-view-bottom to-view)
                            (* y height-factor))))
          (values (float (* x width-factor))
                  (float (* y height-factor)))))))

;; special screen case for coords
;; handle inverted Y axis
;; for screen/view deltas just use view-to-view with NIL absolute argument
(defun screen-to-view (x y view)
  (unless (or (zerop (2d-view-width view)) (zerop (2d-view-height view))
              (zerop *display-width*) (zerop *display-height*))
    (let ((width-factor (/ (2d-view-width view) *display-width*))
          (height-factor (/ (2d-view-height view) *display-height*)))
      (values (float (+ (2d-view-left view)
                        (* x width-factor)))
              (float (+ (2d-view-bottom view)
                        (* (- *display-height* y) height-factor)))))))

(defun view-to-screen (x y view)
  (unless (or (zerop (2d-view-width view)) (zerop (2d-view-height view))
              (zerop *display-width*) (zerop *display-height*))
    (let ((width-factor (/ *display-width* (2d-view-width view)))
          (height-factor (/ *display-height* (2d-view-height view))))
      (values (float (- (* x width-factor) (* (2d-view-left view) width-factor)))
              (float (+ (- *display-height* (* (- y) height-factor))
                        (* height-factor (2d-view-bottom view))))))))

(defmacro with-2d-coords-from-screen (((x-sym x-val) (y-sym y-val)) to-view &body body)
  `(multiple-value-bind (,x-sym ,y-sym)
       (screen-to-view ,x-val ,y-val ,to-view)
     ,@body))

(defmacro with-2d-coords-to-screen (((x-sym x-val) (y-sym y-val)) from-view &body body)
  `(multiple-value-bind (,x-sym ,y-sym)
       (view-to-screen ,x-val ,y-val ,from-view)
     ,@body))

(defmacro with-2d-view-coords (((x-sym x-val) (y-sym y-val)) from-view to-view &body body)
  `(multiple-value-bind (,x-sym ,y-sym)
       (view-to-view ,x-val ,y-val ,from-view ,to-view)
     ,@body))

(defmacro with-2d-view-deltas (((x-sym x-val) (y-sym y-val)) from-view to-view &body body)
  `(multiple-value-bind (,x-sym ,y-sym)
       (view-to-view ,x-val ,y-val ,from-view ,to-view nil)
     ,@body))

(defmacro with-2d-screen-deltas (((x-sym x-val) (y-sym y-val)) to-view &body body)
  `(multiple-value-bind (,x-sym ,y-sym)
       (view-to-view ,x-val (- ,y-val)
                     (create-2d-view 0 0 glaw:*display-width* glaw:*display-height*)
                     ,to-view nil)
     ,@body))


;;; Isometric view
(defstruct (isometric-view (:include 2d-view)))

(defun %isometric-view-update-matrix (view)
  (matrix-set-ortho (2d-view-proj-mtx view)
                    (float (2d-view-left view))
                    (float (2d-view-right view))
                    (float (2d-view-bottom view))
                    (float (2d-view-top view))
                    -1000.0 1000.0))

(defun create-isometric-view (x y width height)
  (let ((view (make-isometric-view :left x :right (+ x width)
                                   :bottom y :top (+ y height)
                                   :proj-mtx (make-matrix)
                                   :eye-mtx (make-matrix))))
    (%isometric-view-update-matrix view)
    (basis-roll (isometric-view-eye-mtx view) (deg->rad -45.0))
    (basis-yaw (isometric-view-eye-mtx view) (deg->rad -30.0))
    view))

(defun set-view-isometric (view)
  (set-view view))

(defun move-isometric-view (view dx dy)
  (incf (2d-view-left view) dx)
  (incf (2d-view-right view) dx)
  (incf (2d-view-bottom view) dy)
  (incf (2d-view-top view) dy)
  (%isometric-view-update-matrix view))


;;; 3D view
;; forward: X
;; left: Y
;; up: Z
(defstruct (3d-view (:include view)
                    (:constructor %make-3d-view))
  perspective
  basis)

(defun %3d-view-update-proj-matrix (view)
  (perspective-apply (3d-view-perspective view)
                     (3d-view-proj-mtx view)))

(defun %3d-view-update-eye-matrix (view)
  (with-slots (eye-mtx basis) view
    ;; first apply rotations
    (setf (matrix-r00 eye-mtx) (- (basis-r01 basis)) ;; -y --> x
          (matrix-r01 eye-mtx) (- (basis-r11 basis))
          (matrix-r02 eye-mtx) (- (basis-r21 basis))
          (matrix-r10 eye-mtx) (basis-r02 basis)     ;;  z --> y
          (matrix-r11 eye-mtx) (basis-r12 basis)
          (matrix-r12 eye-mtx) (basis-r22 basis)
          (matrix-r20 eye-mtx) (- (basis-r00 basis)) ;; -x --> z
          (matrix-r21 eye-mtx) (- (basis-r10 basis))
          (matrix-r22 eye-mtx) (- (basis-r20 basis)))
    ;; then translations
    (let ((eye-x (- (basis-tx basis)))
          (eye-y (- (basis-ty basis)))
          (eye-z (- (basis-tz basis))))
      (setf (matrix-tx eye-mtx) (+ (* eye-x (matrix-r00 eye-mtx))
                                   (* eye-y (matrix-r01 eye-mtx))
                                   (* eye-z (matrix-r02 eye-mtx)))
            (matrix-ty eye-mtx) (+ (* eye-x (matrix-r10 eye-mtx))
                                   (* eye-y (matrix-r11 eye-mtx))
                                   (* eye-z (matrix-r12 eye-mtx)))
            (matrix-tz eye-mtx) (+ (* eye-x (matrix-r20 eye-mtx))
                                   (* eye-y (matrix-r21 eye-mtx))
                                   (* eye-z (matrix-r22 eye-mtx)))))))

(defun make-3d-view (&key (fov (/ pi 3.0)) (ratio 1.0) (near 0.1) (far 100.0)
                          (position (make-point-3d))
                          (orientation (make-orientation)))
  (let ((view (%make-3d-view)))
    (setf (3d-view-perspective view)
          (make-perspective :fov fov :ratio ratio :near near :far far)
          (3d-view-basis view) (make-basis)
          (3d-view-eye-mtx view) (make-matrix)
          (3d-view-proj-mtx view) (make-matrix))
    (%3d-view-update-proj-matrix view)
    (%3d-view-update-eye-matrix view)
    view))

(defun set-view-3d (view)
  (set-view view))

(defun 3d-view-forward (view)
  (basis-local-x (3d-view-basis view)))

(defun 3d-view-side (view)
  (basis-local-y (3d-view-basis view)))

(defun 3d-view-up (view)
  (basis-local-z (3d-view-basis view)))

(defun 3d-view-fov (v)
  (perspective-fov (3d-view-perspective v)))

(defun 3d-view-set-fov (v fov)
  (setf (perspective-fov (3d-view-perspective v)) fov)
  (%3d-view-update-proj-matrix v))

(defsetf 3d-view-fov 3d-view-set-fov)

(defun 3d-view-ratio (v)
  (perspective-ratio (3d-view-perspective v)))

(defun 3d-view-set-ratio (v ratio)
  (setf (perspective-ratio (3d-view-perspective v)) ratio)
  (%3d-view-update-proj-matrix v))

(defsetf 3d-view-ratio 3d-view-set-ratio)

(defun 3d-view-near (v)
  (perspective-near (3d-view-perspective v)))

(defun 3d-view-set-near (v near)
  (setf (perspective-near (3d-view-perspective v)) near)
  (%3d-view-update-proj-matrix v))

(defsetf 3d-view-near 3d-view-set-near)

(defun 3d-view-far (v)
  (perspective-far (3d-view-perspective v)))

(defun 3d-view-set-far (v far)
  (setf (perspective-far (3d-view-perspective v)) far)
  (%3d-view-update-proj-matrix v))

(defsetf 3d-view-far 3d-view-set-far)

(defun 3d-view-position (v)
  (basis-position (3d-view-basis v)))

(defun 3d-view-set-position (v pos)
  (basis-set-position (3d-view-basis v) pos)
  (%3d-view-update-eye-matrix v))

(defsetf 3d-view-position 3d-view-set-position)

(defun 3d-view-translate (v dx dy dz)
  (basis-translate (3d-view-basis v) dx dy dz)
  (%3d-view-update-eye-matrix v))

(defun 3d-view-orientation (v)
  (basis-zyx-orientation (3d-view-basis v))
  (%3d-view-update-eye-matrix v))

(defun 3d-view-set-orientation (v orientation)
  (basis-roll (3d-view-basis v) (orientation-roll orientation))
  (basis-pitch (3d-view-basis v) (orientation-pitch orientation))
  (basis-yaw (3d-view-basis v) (orientation-yaw orientation))
  (%3d-view-update-eye-matrix v))

(defun 3d-view-roll (v droll)
  (basis-roll (3d-view-basis v) droll)
  (%3d-view-update-eye-matrix v))

(defun 3d-view-pitch (v dpitch)
  (basis-pitch (3d-view-basis v) dpitch)
  (%3d-view-update-eye-matrix v))

(defun 3d-view-yaw (v dyaw)
  (basis-yaw (3d-view-basis v) dyaw)
  (%3d-view-update-eye-matrix v))

(defun 3d-view-point-at (v target)
  (let* ((dir (vector-3d-diff target (basis-position (3d-view-basis v))))
         (orientation (make-orientation-from-vector-3d dir)))
    (basis-cancel-rotation (3d-view-basis v))
    (setf (basis-zyx-orientation (3d-view-basis v)) orientation))
  (%3d-view-update-eye-matrix v))

(defun 3d-view-look-at (v eye target)
  (basis-set-position (3d-view-basis v) eye)
  (3d-view-point-at v target))
