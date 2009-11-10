(in-package #:glaw)

(defun 2+ (value)
  (+ value 2))

(defun nearest-power-of-two (size)
  (let ((next-size 1))
    ;; compute next power of two
    (loop while (< next-size size)
       do (setf next-size (ash next-size 1)))
    ;; compute previous one
    (let ((prev-size (ash next-size -1)))
      ;; take the nearest
      (if (< (abs (- next-size size))
             (abs (- prev-size size)))
          next-size
          prev-size))))

(defun elapsed-time-since (start-time &optional (resolution-unit nil))
  "Return elapsed time between provided start time and now."
    (let ((time-units (- (get-internal-real-time)
                         start-time)))
      (if resolution-unit
          (convert-time time-units (or resolution-unit 1.0))
          time-units)))

(defun convert-time (time &optional (resolution-unit 1.0))
  "Converts realtime to seconds with optional scale factor."
  (/ (/ time internal-time-units-per-second) resolution-unit))

(defun rad->deg (angle)
  (declaim (inline deg-to-rad))
  (* angle (/ 180.0 pi)))

(defun deg->rad (angle)
  (declaim (inline deg-to-rad))
  (* angle (/ pi 180.0)))

(defun sqr-dist (x0 y0 x1 y1)
  (+ (* (- x1 x0) (- x1 x0))
     (* (- y1 y0) (- y1 y0))))

;;; Random numbers
(defun random-between (min max)
  (+ min (random max)))

;;; List manipulation
(defun rotate-list-right (lst n)
   (if (zerop n)
       lst
       (rotate-list-right (append (last lst) (butlast lst))
                          (- n 1))))

(defun rotate-list-left (lst n)
   (if (zerop n)
       lst
       (rotate-list-left (append (rest lst) (list (first lst)))
                         (- n 1))))


(defun rotate-list (lst &key (direction :left) (distance 1))
  ;; check distance is positive
  (when (< distance 0)
    (if (eq direction :left)
        (setf direction :right)
        (setf direction :left))
    (setf distance (- distance)))
  ;; rotate the list
  (if (eq direction :left)
      (rotate-list-left  lst distance)
      (rotate-list-right lst distance)))


(defun list-insert (lst1 lst2 insert-pos)
  (let ((lst-start (butlast lst1 (- (length lst1) insert-pos)))
        (lst-end (nthcdr insert-pos lst1)))
    (append lst-start
            lst2
            lst-end)))

(defun list-from (item nb-items)
  (loop for i from 0 to nb-items
       collect item))

(defun sign-of (nb)
  (declaim (inline sign-of))
  (if (>= nb 0)
      :positive
      :negative))

;;; 2D vectors manipulation
(defstruct vector-2d
  (x 0.0)
  (y 0.0))

(defun vec-from-lst (coords)
  (make-vector-2d :x (first coords)
                  :y (second coords)))

(defun vec-from-coords (&rest coords)
  (vec-from-lst coords))

(defun vec-slope (v)
  (/ (vector-2d-y v)
     (vector-2d-x v)))

(defun vec-dot-product (v1 v2)
  (+ (* (vector-2d-x v1) (vector-2d-x v2))
     (* (vector-2d-y v1) (vector-2d-y v2))))

(defun vec-perp-dot-product (v1 v2)
  (dot-product (perp-vec v1) v2))

(defun vec-mag (v)
  (sqrt (dot-product v v)))

(defun vec-normalize (v)
  (scale v (/ 1.0 (norm v))))

(defun vec-null-p (v)
  (and (zerop (vector-2d-x v))
       (zerop (vector-2d-y v))))

(defun vec-perp (v)
  "Returns V rotated by +pi/2."
  (make-vector-2d :x (- (vector-2d-y v))
                  :y (vector-2d-x v)))

(defun vec-opposite (v)
  (make-vector-2d :x (- (vector-2d-x v))
                  :y (- (vector-2d-y v))))

(defun vec-rotate (v angle)
  (make-vector-2d :x (- (* (vector-2d-x v) (cos angle))
                        (* (vector-2d-y v) (sin angle)))
                  :y (+ (* (vector-2d-x v) (sin angle))
                        (* (vector-2d-y v) (cos angle)))))

(defun vec-angle (v1 &optional (v2 (make-vector-2d :x 1 :y 0)))
  "Returns angle between vector v1 taking v2 as the origin."
  (atan (dot-product (perp-vec v2) v1) (dot-product v2 v1)))

(defun vec-add (v1 v2)
  (make-vector-2d :x (+ (vector-2d-x v1) (vector-2d-x v2))
                  :y (+ (vector-2d-y v1) (vector-2d-y v2))))

(defun vec-diff (v1 v2)
  (make-vector-2d :x (- (vector-2d-x v1) (vector-2d-x v2))
                  :y (- (vector-2d-y v1) (vector-2d-y v2))))

(defun vec-scale (v factor)
  (make-vector-2d :x (* factor (vector-2d-x v))
                  :y (* factor (vector-2d-y v))))

;; profiling
(defmacro with-profiling (fmt &body body)
  `(let ((start-time (get-internal-real-time)))
     ,@body
     (format t ,fmt (* 1.0 ( / (- (get-internal-real-time) start-time)
                              internal-time-units-per-second)))))
