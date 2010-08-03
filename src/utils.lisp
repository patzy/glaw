(in-package #:glaw)

(defun ensure-adjustable (vec)
  (make-array (length vec) :fill-pointer (length vec) :initial-contents vec
              :adjustable t))

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

(declaim (inline deg->rad))
(defun rad->deg (angle)
  (* angle (/ 180.0 pi)))

(declaim (inline deg->rad))
(defun deg->rad (angle)
  (* angle (/ pi 180.0)))

(defun sqr-dist (x0 y0 x1 y1)
  (+ (* (- x1 x0) (- x1 x0))
     (* (- y1 y0) (- y1 y0))))

(defun coords-overlap-p (a b c d)
  (or (= a c) (= b d)
      (< c a d b) (< a c b d)
      (< a c d b) (< c a b d)))

;;; Random numbers
(defun random-between (min max)
  (+ min (random (- max min))))

(defun random-nth (lst)
  (nth (random-between 0 (length lst)) lst))

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
  (declare (inline sign-of))
  (if (>= nb 0)
      :positive
      :negative))

;;; 2D vectors manipulation
(defstruct (vector-2d (:type (vector float)))
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
  (vec-dot-product (vec-perp v1) v2))

(defun vec-mag (v)
  (sqrt (vec-dot-product v v)))

(defun vec-normalize (v)
  (vec-scale v (/ 1.0 (vec-mag v))))

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
  (atan (vec-dot-product (vec-perp v2) v1) (vec-dot-product v2 v1)))

(defun vec-sum (v1 v2)
  (make-vector-2d :x (+ (vector-2d-x v1) (vector-2d-x v2))
                  :y (+ (vector-2d-y v1) (vector-2d-y v2))))

(defun vec-add (v1 v2)
  (incf (vector-2d-x v1) (vector-2d-x v2))
  (incf (vector-2d-y v1) (vector-2d-y v2))
  v1)

(defun vec-diff (v1 v2)
  (make-vector-2d :x (- (vector-2d-x v2) (vector-2d-x v1))
                  :y (- (vector-2d-y v2) (vector-2d-y v1))))

(defun vec-sub (v1 v2)
  (decf (vector-2d-x v2) (vector-2d-x v1))
  (decf (vector-2d-y v2) (vector-2d-y v1))
  v2)

(defun vec-scale (v factor)
  (make-vector-2d :x (* factor (vector-2d-x v))
                  :y (* factor (vector-2d-y v))))

;; profiling
(defmacro with-profiling (fmt &body body)
  `(let ((start-time (get-internal-real-time)))
     ,@body
     (format t ,fmt (* 1.0 ( / (- (get-internal-real-time) start-time)
                              internal-time-units-per-second)))))

;; binary files (stolen from imago)
(defun read-integer (stream size &optional big-endian)
  (loop with number = 0
        for i below size
        as byte = (read-byte stream)
        do (if big-endian
               (setf number (logior (ash number 8) byte))
               (incf number (ash byte (* i 8))))
        finally (return number)))

;; string manipulation
(defun split-string (string delim)
  "Split the provided string and returns a list."
  (loop for i = 0 then (1+ j)
        as j = (position delim string :start i)
        when (not (= (length (subseq string i j)) 0)) ;remove last elt
        collect (subseq string i j)
        while j))

;; Conditions
(define-condition glaw-error (error)
  () (:documentation "Any glaw specific error should inherit this."))

(define-condition not-implemented (glaw-error)
  () (:documentation "Unimplemented."))
