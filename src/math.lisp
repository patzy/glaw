(in-package #:glaw)

;;; Angles
(declaim (inline deg->rad))
(defun rad->deg (angle)
  (* angle (/ 180.0 pi)))

(declaim (inline deg->rad))
(defun deg->rad (angle)
  (* angle (/ pi 180.0)))

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

;;; Axis
(defstruct (axis (:type (vector float))
                 (:include vector-3d)))

;;; Quaternions
(defstruct (quaternion (:type (vector float)))
  (x 0.0) (y 0.0) (z 0.0) (w 0.0))

(defun quaternion-from-axis-angle (axis angle)
  (make-quaternion :x (* (sin (/ angle 2.0)) (axis-x axis))
                   :y (* (sin (/ angle 2.0)) (axis-y axis))
                   :z (* (sin (/ angle 2.0)) (axis-z axis))
                   :w (cos (/ angle 2.0))))
