(in-package #:glaw)

;;; Bouding volumes

;;; Bounding box
(defstruct bbox
  "Axis Aligned Bounding Box."
  valid
  x-min y-min z-min
  x-max y-max z-max)

(defun bbox-invalidate (bbox)
  (setf (bbox-valid bbox) nil))

(defun render-bbox (bbox)
  (gl:begin :line-strip)
  (gl:vertex (bbox-x-min bbox) (bbox-y-max bbox))
  (gl:vertex (bbox-x-max bbox) (bbox-y-max bbox))
  (gl:vertex (bbox-x-max bbox) (bbox-y-min bbox))
  (gl:vertex (bbox-x-min bbox) (bbox-y-min bbox))
  (gl:vertex (bbox-x-min bbox) (bbox-y-max bbox))
  (gl:end))

(defun bbox-intersect-p (bbox-1 bbox-2)
  (and (coords-overlap-p (bbox-x-min bbox-1) (bbox-x-max bbox-1)
                         (bbox-x-min bbox-2) (bbox-x-max bbox-2))
       (coords-overlap-p (bbox-y-min bbox-1) (bbox-y-max bbox-1)
                         (bbox-y-min bbox-2) (bbox-y-max bbox-2))))

(defun bbox-inside-p (bbox x y &optional z)
  (and (< (bbox-x-min bbox) x (bbox-x-max bbox))
       (< (bbox-y-min bbox) y (bbox-y-max bbox))))

;; TODO: have this for both 2D and 3D views
(defun bbox-visible-p (bbox view)
  (let ((view-box (make-bbox :valid t
                             :z-min 0.0 :z-max 0.0
                             :x-min (2d-view-left view)
                             :x-max (2d-view-right view)
                             :y-min (2d-view-bottom view)
                             :y-max (2d-view-top view))))
    (bbox-intersect-p bbox view-box)))

(defun bbox-update (bbox x y &optional (z 0.0))
  ;;(declare (type single-float x y z))
  (if (bbox-valid bbox)
      (progn (when (< x (bbox-x-min bbox))
               (setf (bbox-x-min bbox) x))
             (when (< y (bbox-y-min bbox))
               (setf (bbox-y-min bbox) y))
             (when (< z (bbox-z-min bbox))
               (setf (bbox-z-min bbox) z))
             (when (> x (bbox-x-max bbox))
               (setf (bbox-x-max bbox) x))
             (when (> y (bbox-y-max bbox))
               (setf (bbox-y-max bbox) y))
             (when (> z (bbox-z-max bbox))
               (setf (bbox-z-max bbox) z)))
      (progn (setf (bbox-x-min bbox) x
                   (bbox-y-min bbox) y
                   (bbox-z-min bbox) z
                   (bbox-x-max bbox) x
                   (bbox-y-max bbox) y
                   (bbox-z-max bbox) z
                   (bbox-valid bbox) t))))

(defun bbox-update/shape (bbox shape)
  (loop for i from 0 below (shape-nb-vertices shape) do
       (bbox-update bbox (aref (shape-vertices shape) (* i 3))
                         (aref (shape-vertices shape) (+ (* i 3) 1))
                         (aref (shape-vertices shape) (+ (* i 3) 2)))))

(defun bbox-overwrite/shape (bbox shape)
  (bbox-invalidate bbox)
  (bbox-update/shape bbox shape))

(defun bbox-translate (bbox dx dy &optional (dz 0.0))
  (incf (bbox-x-min bbox) dx)
  (incf (bbox-y-min bbox) dy)
  (incf (bbox-z-min bbox) dz)
  (incf (bbox-x-max bbox) dx)
  (incf (bbox-y-max bbox) dy)
  (incf (bbox-z-max bbox) dz))


(defun create-bbox-from-shape (shape)
  (let ((bbox (make-bbox)))
    (bbox-update/shape bbox shape)
    bbox))

;;; Bounding sphere
(defstruct bsphere
  valid
  x y z ;; center
  radius)

(defun bsphere-center (bsphere)
  (make-vector-3d :x (bsphere-x bsphere) :y (bsphere-y bsphere) :z (bsphere-z bsphere)))

(defun bsphere-invalidate (bsphere)
  (setf (bsphere-valid bsphere) nil))

(defun bsphere-inside-p (bsphere x y &optional (z 0.0))
  (point-3d-distance (make-point-3d :x (+ x (bsphere-x bsphere))
                                    :y (+ y (bsphere-y bsphere))
                                    :z (+ z (bsphere-z bsphere)))))

(defun bsphere-update (bsphere x y &optional (z 0.0))
  (let ((dist (point-3d-distance (make-point-3d :x x :y y :z z))))
    (if (bsphere-valid bsphere)
        (setf (bsphere-radius bsphere) (max (bsphere-radius bsphere) dist))
        (setf (bsphere-radius bsphere) dist))))

(defun bsphere-update/shape (bsphere shape)
  (loop for i from 0 below (shape-nb-vertices shape) do
       (bsphere-update bsphere (aref (shape-vertices shape) (* i 3))
                               (aref (shape-vertices shape) (+ (* i 3) 1))
                               (aref (shape-vertices shape) (+ (* i 3) 2)))))

(defun bsphere-overwrite/shape (bsphere shape)
  (bsphere-invalidate bsphere)
  (bsphere-update/shape bsphere shape))

(defun create-bsphere-from-shape (shape)
  (let ((bsphere (make-bsphere)))
    (bsphere-update/shape bsphere shape)
    bsphere))