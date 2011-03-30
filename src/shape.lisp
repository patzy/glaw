(in-package :glaw)

;;; Shapes management
(defstruct shape
  (primitive :triangle-strip)
  vertices   ;; x,y,z
  normals    ;; x y z
  colors     ;; r,g,b,a
  tex-coords ;; u,v
  indices)

(defun create-shape (nb-vertices nb-indices &key color texture normals
                                             (primitive :triangles))
  (make-shape :primitive primitive
              :vertices (make-array (* nb-vertices 3)
                                    :element-type 'float
                                    ;;:adjustable t
                                    :fill-pointer 0)
              :normals (when normals
                         (make-array (* nb-vertices 3)
                                     :element-type 'float
                                     ;;:adjustable t
                                     :fill-pointer 0))
              :colors (when color
                        (make-array (* nb-vertices 4)
                                    :element-type 'float
                                    ;;:adjustable t
                                    :fill-pointer 0))
              :tex-coords (when texture
                            (make-array (* nb-vertices 2)
                                        :element-type 'float
                                        ;;:adjustable t
                                        :fill-pointer 0))
              :indices (make-array nb-indices
                                   :element-type 'unsigned-byte
                                   ;;:adjustable t
                                   :fill-pointer 0)))

(defun create-shape-from-arrays (indices vertices colors tex-coords normals
                                 &optional (primitive :triangles))
  (make-shape :primitive primitive
              :indices indices
              :vertices vertices
              :colors colors
              :tex-coords tex-coords
              :normals normals))

(defun shape-has-color (shape)
  (not (null (shape-colors shape))))

(defun shape-has-texture (shape)
  (not (null (shape-tex-coords shape))))

(defun shape-has-normals (shape)
  (not (null (shape-normals shape))))

(defun %vertex-data-from-face-data (data indices)
  "Linearize per face data to per vertex mode."
  ;; FIXME: set element-type from DATA
  (when data
    (let ((vertex-data (make-array (* 3 (length indices)))))
      (loop for i below (length indices)
           do (replace vertex-data data
                       :start1 (* i 3) :end1 (+ (* i 3) 3)
                       :start2 (* 3 (aref indices i)) :end2 (+ (* 3 (aref indices i)) 3)))
      vertex-data)))

(defun shape-nb-vertices (shape)
  (/ (length (shape-vertices shape)) 3))

(defun shape-nb-indices (shape)
  (length (shape-indices shape)))

(defmacro with-shape-vertices ((v-sym shape) &body body)
  `(let ((,v-sym (shape-vertices ,shape)))
     ,@body))

(defmacro with-shape-colors ((c-sym shape) &body body)
  `(let ((,c-sym (shape-colors ,shape)))
     ,@body))

(defmacro with-shape-tex-coords ((t-sym shape) &body body)
  `(let ((,t-sym (shape-tex-coords ,shape)))
     ,@body))

(defmacro with-shape-normals ((v-sym shape) &body body)
  `(let ((,v-sym (shape-normals ,shape)))
     ,@body))

(defun shape-get-index (shape index)
  (aref (shape-indices shape) index))

(defun shape-set-vertex (shape index x y &optional (z 0.0))
  (setf (aref (shape-vertices shape) (* index 3)) x
        (aref (shape-vertices shape) (+ 1 (* index 3))) y
        (aref (shape-vertices shape) (+ 2 (* index 3))) z))

(defun shape-get-vertex (shape index)
  (subseq (shape-vertices shape) (* index 3) (+ 3 (* index 3))))

(defun shape-get-vertex/values (shape index)
  (values (aref (shape-vertices shape) (* index 3))
          (aref (shape-vertices shape) (+ 1 (* index 3)))
          (aref (shape-vertices shape) (+ 2 (* index 3)))))

(defun shape-set-color (shape index color)
  (setf (aref (shape-colors shape) (* index 4)) (aref color 0)
        (aref (shape-colors shape) (+ 1 (* index 4))) (aref color 1)
        (aref (shape-colors shape) (+ 2 (* index 4))) (aref color 2)
        (aref (shape-colors shape) (+ 3 (* index 4))) (aref color 3)))

(defun shape-set-color/rgb (shape index r g b &optional (a 0.0))
  (setf (aref (shape-colors shape) (* index 4)) r
        (aref (shape-colors shape) (+ 1 (* index 4))) g
        (aref (shape-colors shape) (+ 2 (* index 4))) b
        (aref (shape-colors shape) (+ 3 (* index 4))) a))

(defun shape-get-color (shape index)
  (subseq (shape-colors shape) (* index 4) (+ 4 (* index 4))))

(defun shape-set-tex-coord (shape index u v)
  (setf (aref (shape-tex-coords shape) (* index 2)) u
        (aref (shape-tex-coords shape) (+ 1 (* index 2))) v))

(defun shape-get-tex-coord (shape index)
  (subseq (shape-tex-coords shape) (* index 2) (+ 2 (* index 2))))

(defun shape-set-normal (shape index x y &optional (z 0.0))
  (setf (aref (shape-normals shape) (* index 3)) x
        (aref (shape-normals shape) (+ 1 (* index 3))) y
        (aref (shape-normals shape) (+ 2 (* index 3))) z))

(defun shape-set-normal/v (shape index vec)
  (setf (aref (shape-normals shape) (* index 3)) (vector-3d-x vec)
        (aref (shape-normals shape) (+ 1 (* index 3))) (vector-3d-y vec)
        (aref (shape-normals shape) (+ 2 (* index 3))) (vector-3d-z vec)))

(defun shape-get-normal (shape index)
  (make-vector-3d :x (aref (shape-normals shape) (* index 3))
                  :y (aref (shape-normals shape) (+ 1 (* index 3)))
                  :z (aref (shape-normals shape) (+ 2 (* index 3)))))

(defun translate-shape (shape dx dy &optional (dz 0.0))
  (loop for i from 0 below (length (shape-vertices shape)) by 3 do
       (incf (aref (shape-vertices shape) i) dx)
       (incf (aref (shape-vertices shape) (+ i 1)) dy)
       (incf (aref (shape-vertices shape) (+ i 2)) dz)))

(defun rotate-shape-2d (shape angle &optional (center-x 0.0) (center-y 0.0))
  (unless (and (zerop center-x) (zerop center-y))
    (translate-shape shape (- center-x) (- center-y) 0.0))
  (loop for i from 0 below (length (shape-vertices shape)) by 3 do
       (let ((x (aref (shape-vertices shape) i))
             (y (aref (shape-vertices shape) (+ i 1))))
         (setf (aref (shape-vertices shape) i)
               (- (* x (cos angle)) (* y (sin angle)))
               (aref (shape-vertices shape) (+ i 1))
               (+ (* y (cos angle)) (* x (sin angle))))))
  (unless (and (zerop center-x) (zerop center-y))
    (translate-shape shape center-x center-y 0.0)))

(defun scale-shape-2d (shape sx sy)
  (loop for i from 0 below (length (shape-vertices shape)) by 3 do
       (let ((x (aref (shape-vertices shape) i))
             (y (aref (shape-vertices shape) (+ i 1))))
         (setf (aref (shape-vertices shape) i) (* x sx)
               (aref (shape-vertices shape) (+ i 1)) (* y sy)))))

(defun render-shape (shape &optional (primitive (shape-primitive shape)))
  (render-primitive (shape-indices shape)
                    (shape-vertices shape)
                    :primitive primitive
                    :colors (shape-colors shape)
                    :tex-coords (shape-tex-coords shape)
                    :normals (shape-normals shape)))

(defun render-shape-normals (shape)
  (gl:disable :lighting)
  (set-color #(1.0 0.0 0.0 1.0))
  (gl:begin :lines)
  (loop for i below (shape-nb-vertices shape)
       for vertex = (shape-get-vertex shape i)
       for normal = (shape-get-normal shape i) do
       (gl:vertex (vector-3d-x vertex)
                  (vector-3d-y vertex)
                  (vector-3d-z vertex))
       (gl:vertex (+ (vector-3d-x vertex) (vector-3d-x normal))
                  (+ (vector-3d-y vertex) (vector-3d-y normal))
                  (+ (vector-3d-z vertex) (vector-3d-z normal))))
  (gl:end))

;; FIXME: this is sloooooooowww, we can probably optimize this
(defun shape-compact-vertices (shape &key ignore-colors
                                          ignore-tex-coords
                                          ignore-normals)
  (let ((new-vertices (make-array 0 :adjustable t :fill-pointer t))
        (new-colors (when (shape-has-color shape)
                      (make-array 0 :adjustable t :fill-pointer t)))
        (new-tex-coords (when (shape-has-texture shape)
                          (make-array 0 :adjustable t :fill-pointer t)))
        (new-normals (when (shape-has-normals shape)
                       (make-array 0 :adjustable t :fill-pointer t))))
    (dformat "Compacting shape vertices (C/T/N): ~S/~S/~S~%"
             ignore-colors ignore-tex-coords ignore-normals)
    (dformat "Working")
    (loop for i below (shape-nb-vertices shape)
         for vertex = (shape-get-vertex shape i)
         for color = (when new-colors (shape-get-color shape i))
         for tex-vertex = (when new-tex-coords (shape-get-tex-coord shape i))
         for normal = (when new-normals (shape-get-normal shape i))
         for found = nil
         do (loop for j below (/ (length new-vertices) 3)
                 do (when (and (equalp vertex (subseq new-vertices (* j 3) (+ 3 (* j 3))))
                               (if (and new-colors (not ignore-colors))
                                   (equalp color (subseq new-colors (* j 4) (+ 4 (* j 4)))) t)
                               (if (and new-tex-coords (not ignore-tex-coords))
                                   (equalp tex-vertex (subseq new-tex-coords (* j 2) (+ 2 (* j 2))))
                                   t)
                               (if (and new-normals (not ignore-normals))
                                   (equalp normal (subseq new-normals (* j 3) (+ 3 (* j 3)))) t))
                      (setf found j)))
         (dformat ".") ;; FIXME: don't output so much dots !!!
         (unless found ;; vertex not stored, add it before fixing indices
           (vector-push-extend (vector-3d-x vertex) new-vertices)
           (vector-push-extend (vector-3d-y vertex) new-vertices)
           (vector-push-extend (vector-3d-z vertex) new-vertices)
           (when new-colors
             (vector-push-extend (vector-4d-x color) new-colors)
             (vector-push-extend (vector-4d-y color) new-colors)
             (vector-push-extend (vector-4d-z color) new-colors)
             (vector-push-extend (vector-4d-w color) new-colors))
           (when new-tex-coords
             (vector-push-extend (vector-2d-x tex-vertex) new-tex-coords)
             (vector-push-extend (vector-2d-y tex-vertex) new-tex-coords))
           (when new-normals
             (vector-push-extend (vector-3d-x normal) new-normals)
             (vector-push-extend (vector-3d-y normal) new-normals)
             (vector-push-extend (vector-3d-z normal) new-normals))
           (setf found (1- (/ (length new-vertices) 3))))
         ;; fix indices array
         (loop for j below (shape-nb-indices shape)
            when (= (aref (shape-indices shape) j) i)
            do (setf (aref (shape-indices shape) j) found)
            finally (setf found nil)))
    (dformat "~%")
    (dformat "Shape vertices # before/after compact: ~S / ~S~%"
             (shape-nb-vertices shape)
             (/ (length new-vertices) 3))
    (setf (shape-vertices shape) new-vertices
          (shape-colors shape) new-colors
          (shape-tex-coords shape) new-tex-coords
          (shape-normals shape) new-normals)))

;; http://www.emeyex.com/site/tuts/VertexNormals.pdf
(defun shape-compute-normals (shape &optional (normalize t))
  (assert (eq (shape-primitive shape) :triangles))
  (when (shape-normals shape)
    (warn "Overwriting existing shape normal data."))
  (setf (shape-normals shape)  ;; per-vertex normals
        (make-array (length (shape-vertices shape))))
  (let ((tmp-normals (make-array (shape-nb-indices shape)))    ;; normal for each face
        (used-normals (make-array (shape-nb-vertices shape)))) ;; normals shared by vertices
    (loop for i below (shape-nb-indices shape) by 3
       for i0 = (aref (shape-indices shape) i)
       for i1 = (aref (shape-indices shape) (+ 1 i))
       for i2 = (aref (shape-indices shape) (+ 2 i))
       for p0 = (shape-get-vertex shape i0)
       for p1 = (shape-get-vertex shape i1)
       for p2 = (shape-get-vertex shape i2)
       for normal = (vector-3d-normalize
                     (vector-3d-cross-product (vector-3d-diff p1 p0)
                                              (vector-3d-diff p2 p0)))
       do (replace tmp-normals normal :start1 i) ;; store current face normal
         (push i (aref used-normals i0)) ;; current normal is used by all face's vertices
         (push i (aref used-normals i1))
         (push i (aref used-normals i2)))
    ;; compute smoothed per vertex normals
    (loop for i below (shape-nb-vertices shape)
       for normal = (make-vector-3d) do
         ;; average all normals used by ith vertex
         (loop for j below (length (aref used-normals i))
            with face-idx = (nth j (aref used-normals i)) do
              (vector-3d-add normal (subseq tmp-normals face-idx (+ 3 face-idx)))
              (when normalize
                (setf normal (vector-3d-normalize normal))))
         (shape-set-normal/v shape i normal))))

(defun shape-revert-normals (shape)
  (loop for i below (shape-nb-vertices shape)
       do (setf (aref (shape-normals shape) i)
                (vector-3d-scale (aref (shape-normals shape) i) -1.0))))

(defun shape-add-vertex (shape x y &optional (z 0.0))
  ;;(declare (type single-float x y z))
  (vector-push-extend x (shape-vertices shape))
  (vector-push-extend y (shape-vertices shape))
  (vector-push-extend z (shape-vertices shape)))

(defun shape-add-normal (shape x y z)
  (vector-push-extend x (shape-normals shape))
  (vector-push-extend y (shape-normals shape))
  (vector-push-extend z (shape-normals shape)))

(defun shape-add-color (shape color)
  ;;(declare (type color color))
  (vector-push-extend (color-r color) (shape-colors shape))
  (vector-push-extend (color-g color) (shape-colors shape))
  (vector-push-extend (color-b color) (shape-colors shape))
  (vector-push-extend (color-a color) (shape-colors shape)))

(defun shape-add-color/rgb (shape r g b &optional (a 1.0))
  ;;(declare (type single-float r g b a))
  (vector-push-extend r (shape-colors shape))
  (vector-push-extend g (shape-colors shape))
  (vector-push-extend b (shape-colors shape))
  (vector-push-extend a (shape-colors shape)))

(defun shape-add-tex-vertex (shape u v)
  ;;(declare (type single-float u v))
  (vector-push-extend u (shape-tex-coords shape))
  (vector-push-extend v (shape-tex-coords shape)))

(defun shape-add-indices (shape &rest indices)
  (dolist (i indices)
    (declare (type unsigned-byte i))
    (vector-push-extend i (shape-indices shape))))

(defun shape-add-vertex/index (shape x y &optional (z 0.0))
  (shape-add-vertex shape x y z)
  (shape-add-indices shape (fill-pointer (shape-indices shape))))

;;; Predefined shapes helpers
(defun create-grid-shape (width height step-x step-y
                          &key (start-x 0) (start-y 0)
                               (altitude 0.0) color texture)
  (incf width) (incf height)
  (let ((shape (create-shape (* width height)
                             (* width height 2 3)
                             :color color
                             :texture texture
                             :primitive :quads)))
    (loop for y from start-y below (+ start-y (* height step-y)) by step-y
       do (loop for x from start-x below (+ start-x (* width step-x)) by step-x
             for z = (if (functionp altitude)
                                 (funcall altitude x y)
                                 altitude)
             do (progn (shape-add-vertex shape x y z)
                       (when color
                         (multiple-value-bind (r g b a)
                             (funcall color x y z)
                           (shape-add-color/rgb shape r g b a)))
                       (when texture
                         (multiple-value-bind (u v)
                             (funcall texture x y z)
                           (shape-add-tex-vertex shape u v))))))
    (loop for y from 0 below (1- height)
       do (loop for x from 0 below (1- width)
               for i = (+ x (* width y))
             do (shape-add-indices shape
                   i (+ i 1) (+ i width 1) (+ i width))))
    shape))

(defun create-circle-shape (x y radius &key (resolution 20) (filled t))
  (let ((shape (create-shape (round (/ 360 resolution) 1.0)
                             (round (/ 360 resolution) 1.0)
                             :primitive (if filled :triangle-fan :line-loop))))
    (loop for angle from 0 to 360 by resolution
       for radian = (deg->rad angle)
       do (shape-add-vertex/index shape
                                  (+ x (* radius (cos radian)))
                                  (+ y (* radius (sin radian)))))
    shape))

(defun create-triangle-shape (x0 y0 x1 y1 x2 y2 &key (filled t))
  (let ((shape (create-shape 3
                             3
                             :primitive (if filled :triangles :line-loop))))
    (shape-add-vertex/index shape x0 y0)
    (shape-add-vertex/index shape x1 y1)
    (shape-add-vertex/index shape x2 y2)
    shape))

(defun create-cross-shape (x y size)
  (let ((shape (create-shape 4 4
                             :color nil
                             :texture nil
                             :primitive :lines)))
    (shape-add-vertex/index shape (- x size) y)
    (shape-add-vertex/index shape (+ x size) y)
    (shape-add-vertex/index shape x (- y size))
    (shape-add-vertex/index shape x (+ y size))
    shape))

(defun create-rectangle-shape (left bottom right top &key (filled t)
                                                          tex-width tex-height color)
  (let ((shape (create-shape 4 5
                             :color nil
                             :texture t
                             :primitive (if filled :quads :line-strip)))
        (width (- right left))
        (height (- top bottom)))
  (shape-add-vertex/index shape left bottom)
  (when color (shape-add-color shape color))
  (shape-add-tex-vertex shape 0.0 0.0)
  (shape-add-vertex/index shape right bottom)
  (when color (shape-add-color shape color))
  (shape-add-tex-vertex shape (if tex-width (/ width tex-width) 1.0) 0.0)
  (shape-add-vertex/index shape right top)
  (when color (shape-add-color shape color))
  (shape-add-tex-vertex shape (if tex-width (/ width tex-width) 1.0)
                              (if tex-height (/ height tex-height) 1.0))
  (shape-add-vertex/index shape left top)
  (when color (shape-add-color shape color))
  (shape-add-tex-vertex shape 0.0 (if tex-height (/ height tex-height) 1.0))
  (unless filled (shape-add-indices shape 0))
  shape))

(defun create-line-shape (x0 y0 x1 y1)
  (let ((shape (create-shape 2 2
                             :color nil
                             :texture nil
                             :primitive :lines)))

  (shape-add-vertex/index shape x0 y0)
  (shape-add-vertex/index shape x1 y1)
  shape))

(defun create-polygon-shape (coords)
  (let ((shape (create-shape (length coords) (length coords)
                             :color nil
                             :texture nil
                             :primitive :polygon)))
    (loop for v in coords
         do (shape-add-vertex/index shape (first v) (second v)))
    shape))

(defun create-box-shape (size-x size-y size-z)
  (let ((shape (create-shape 24 36
                             :normals t
                             :texture t
                             :primitive :triangles))
        (sx (* 0.5 size-x))
        (sy (* 0.5 size-y))
        (sz (* 0.5 size-z)))
    ;; Y-
    (shape-add-vertex shape (- sx) (- sy) (- sz))
    (shape-add-normal shape 0.0 -1.0 0.0)
    (shape-add-tex-vertex shape 0.0 0.0)
    (shape-add-vertex shape sx (- sy) (- sz))
    (shape-add-normal shape 0.0 -1.0 0.0)
    (shape-add-tex-vertex shape 1.0 0.0)
    (shape-add-vertex shape sx (- sy) sz)
    (shape-add-normal shape 0.0 -1.0 0.0)
    (shape-add-tex-vertex shape 1.0 1.0)
    (shape-add-vertex shape (- sx) (- sy) sz)
    (shape-add-normal shape 0.0 -1.0 0.0)
    (shape-add-tex-vertex shape 0.0 1.0)
    (shape-add-indices shape 0 1 2 0 2 3)
    ;; X+
    (shape-add-vertex shape sx (- sy) (- sz))
    (shape-add-normal shape 1.0 0.0 0.0)
    (shape-add-tex-vertex shape 0.0 0.0)
    (shape-add-vertex shape sx sy (- sz))
    (shape-add-normal shape 1.0 0.0 0.0)
    (shape-add-tex-vertex shape 1.0 0.0)
    (shape-add-vertex shape sx sy sz)
    (shape-add-normal shape 1.0 0.0 0.0)
    (shape-add-tex-vertex shape 1.0 1.0)
    (shape-add-vertex shape sx (- sy) sz)
    (shape-add-normal shape 1.0 0.0 0.0)
    (shape-add-tex-vertex shape 0.0 1.0)
    (shape-add-indices shape 4 5 6 4 6 7)
    ;; Y+
    (shape-add-vertex shape sx sy (- sz))
    (shape-add-normal shape 0.0 1.0 0.0)
    (shape-add-tex-vertex shape 0.0 0.0)
    (shape-add-vertex shape (- sx) sy (- sz))
    (shape-add-normal shape 0.0 1.0 0.0)
    (shape-add-tex-vertex shape 1.0 0.0)
    (shape-add-vertex shape (- sx) sy sz)
    (shape-add-normal shape 0.0 1.0 0.0)
    (shape-add-tex-vertex shape 1.0 1.0)
    (shape-add-vertex shape sx sy sz)
    (shape-add-normal shape 0.0 1.0 0.0)
    (shape-add-tex-vertex shape 0.0 1.0)
    (shape-add-indices shape 8 9 10 8 10 11)
    ;; X-
    (shape-add-vertex shape (- sx) sy (- sz))
    (shape-add-normal shape -1.0 0.0 0.0)
    (shape-add-tex-vertex shape 0.0 0.0)
    (shape-add-vertex shape (- sx) (- sy) (- sz))
    (shape-add-normal shape -1.0 0.0 0.0)
    (shape-add-tex-vertex shape 1.0 0.0)
    (shape-add-vertex shape (- sx) (- sy) sz)
    (shape-add-normal shape -1.0 0.0 0.0)
    (shape-add-tex-vertex shape 1.0 1.0)
    (shape-add-vertex shape (- sx) sy sz)
    (shape-add-normal shape -1.0 0.0 0.0)
    (shape-add-tex-vertex shape 0.0 1.0)
    (shape-add-indices shape 12 13 14 12 14 15)
    ;; Z+
    (shape-add-vertex shape sx (- sy) sz)
    (shape-add-normal shape 0.0 0.0 1.0)
    (shape-add-tex-vertex shape 0.0 0.0)
    (shape-add-vertex shape sx sy sz)
    (shape-add-normal shape 0.0 0.0 1.0)
    (shape-add-tex-vertex shape 1.0 0.0)
    (shape-add-vertex shape (- sx) sy sz)
    (shape-add-normal shape 0.0 0.0 1.0)
    (shape-add-tex-vertex shape 1.0 1.0)
    (shape-add-vertex shape (- sx) (- sy) sz)
    (shape-add-normal shape 0.0 0.0 1.0)
    (shape-add-tex-vertex shape 0.0 1.0)
    (shape-add-indices shape 16 17 18 16 18 19)
    ;; Z-
    (shape-add-vertex shape (- sx) (- sy) (- sz))
    (shape-add-normal shape 0.0 0.0 -1.0)
    (shape-add-tex-vertex shape 0.0 0.0)
    (shape-add-vertex shape (- sx) sy (- sz))
    (shape-add-normal shape 0.0 0.0 -1.0)
    (shape-add-tex-vertex shape 1.0 0.0)
    (shape-add-vertex shape sx sy (- sz))
    (shape-add-normal shape 0.0 0.0 -1.0)
    (shape-add-tex-vertex shape 1.0 1.0)
    (shape-add-vertex shape sx (- sy) (- sz))
    (shape-add-normal shape 0.0 0.0 -1.0)
    (shape-add-tex-vertex shape 0.0 1.0)
    (shape-add-indices shape 20 21 22 20 22 23)

    shape))
