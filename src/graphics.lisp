(in-package :glaw)

;; Framerate utils
;; TODO: add min/avg/max
(defstruct frame-counter
  (nb-frames 0 :type fixnum)
  (sample-size 10)
  (min most-positive-single-float)
  (max most-negative-single-float)
  (sum 0.0)
  (nb-samples 0)
  (rate 0.0)
  (last-render-time (get-internal-real-time)))

(defvar *frame-counter* (make-frame-counter)
  "Framerate counter.")

(defun update-fps ()
  "Updates framerate informations."
  (incf (frame-counter-nb-frames *frame-counter*))
  (when (> (frame-counter-nb-frames *frame-counter*) (frame-counter-sample-size *frame-counter*))
    (let* ((elapsed-time (- (get-internal-real-time)
                            (frame-counter-last-render-time *frame-counter*)))
           (fps (* (/ (frame-counter-nb-frames *frame-counter*)
                      (/ elapsed-time internal-time-units-per-second)) 1.0)))
      (incf (frame-counter-nb-samples *frame-counter*))
      (incf (frame-counter-sum *frame-counter*) fps)
      (when (< fps (frame-counter-min *frame-counter*))
        (setf (frame-counter-min *frame-counter*) fps))
      (when (> fps (frame-counter-max *frame-counter*))
        (setf (frame-counter-max *frame-counter*) fps))
      (setf (frame-counter-rate *frame-counter*) fps)
      (setf (frame-counter-last-render-time *frame-counter*)
            (get-internal-real-time))
      (setf (frame-counter-nb-frames *frame-counter*) 0))))

(defun current-fps ()
  (frame-counter-rate *frame-counter*))

(defun min-fps ()
  (frame-counter-min *frame-counter*))

(defun max-fps ()
  (frame-counter-max *frame-counter*))

(defun avg-fps ()
  (/ (frame-counter-sum *frame-counter*)
     (if (zerop (frame-counter-nb-samples *frame-counter*))
         1.0
         (frame-counter-nb-samples *frame-counter*))))

;;; General rendering
(defvar *display-width* 0)
(defvar *display-height* 0)

(defun setup-gl-defaults ()
  (gl:clear-color 0.3 0.3 0.3 0)
  (gl:disable :depth-test)
  (gl:disable :cull-face)
  (gl:disable :lighting)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :normalize)
  (gl:shade-model :smooth)
  (gl:enable :texture-2d))

(defun reshape (width height)
  ;; set viewport to full window
  (gl:viewport 0 0 width height)
  (setf *display-width* width)
  (setf *display-height* height))


(defun begin-draw ()
  (gl:clear :color-buffer))

(defun end-draw ()
  (update-fps)
  (gl:flush))

;;; 2D view
(defstruct 2d-view
  left right bottom top
  (zoom 1.0))

(defun create-2d-view (x y width height)
  (make-2d-view :left x :right (+ x width)
                :bottom y :top (+ y height)))

(defun 2d-view-width (view)
  (- (2d-view-right view) (2d-view-left view)))

(defmethod (setf 2d-view-width) (value (view 2d-view))
  (setf (2d-view-right view) (+ (2d-view-left view)
                                value)))

(defun 2d-view-height (view)
  (- (2d-view-top view) (2d-view-bottom view)))

(defmethod (setf 2d-view-height) (value (view 2d-view))
  (setf (2d-view-top view) (+ (2d-view-bottom view)
                                 value)))

(defun zoom-2d-view (view dfactor)
  (let ((width-diff (* dfactor (2d-view-width view)))
        (height-diff (* dfactor (2d-view-height view))))
    (decf (2d-view-left view) width-diff)
    (incf (2d-view-right view) width-diff)
    (decf (2d-view-bottom view) height-diff)
    (incf (2d-view-top view) height-diff)
    (incf (2d-view-zoom view) dfactor)))

(defun move-2d-view (view dx dy)
  (incf (2d-view-left view) dx)
  (incf (2d-view-right view) dx)
  (incf (2d-view-bottom view) dy)
  (incf (2d-view-top view) dy))

(defun update-2d-view (view x y width height)
  (setf (2d-view-left view) x)
  (setf (2d-view-bottom view) y)
  (setf (2d-view-right view) (+ x width))
  (setf (2d-view-top view) (+ y height)))

(defun set-view-2d (view)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho (2d-view-left view) (2d-view-right view)
            (2d-view-bottom view) (2d-view-top view)
            -1.0 1.0)
  (gl:matrix-mode :modelview)
  (gl:load-identity))

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

(defmacro with-2d-view-screen-coords (((x-sym x-val) (y-sym y-val)) view  &body body)
  `(multiple-value-bind (,x-sym ,y-sym)
       (screen-to-view ,x-val ,y-val ,view)
     ,@body))

(defmacro with-2d-screen-view-coords (((x-sym x-val) (y-sym y-val)) view  &body body)
  `(multiple-value-bind (,x-sym ,y-sym)
       (view-to-screen ,x-val ,y-val ,view)
     ,@body))

(defmacro with-2d-view-coords (((x-sym x-val) (y-sym y-val)) from-view to-view  &body body)
  `(multiple-value-bind (,x-sym ,y-sym)
       (view-to-view ,x-val ,y-val ,from-view ,to-view)
     ,@body))

;;; Colors helpers
(defstruct color
  (r 1.0)
  (g 1.0)
  (b 1.0)
  (a 1.0))

(declaim (inline set-color))
(defun set-color (col)
  (gl:color (color-r col) (color-g col) (color-b col) (color-a col)))

(declaim (inline set-color/rgb))
(defun set-color/rgb (r g b &optional (a 1.0))
  (gl:color r g b a))

(defun create-color (r g b &optional (a 1.0))
  (make-color :r r :g g :b b :a a))

(defun color-copy (src dest)
  (setf (color-r dest) (color-r src)
        (color-g dest) (color-g src)
        (color-b dest) (color-b src)
        (color-a dest) (color-a src)))

(defun mix-colors (color-1 color-2 value)
  (let ((r (+ (color-r color-1)
              (* (- (color-r color-2)
                    (color-r color-1))
                 value)))
        (g (+ (color-g color-1)
              (* (- (color-g color-2)
                    (color-g color-1))
                 value)))
        (b (+ (color-b color-1)
              (* (- (color-b color-2)
                    (color-b color-1))
                 value)))
        (a (+ (color-a color-1)
              (* (- (color-a color-2)
                    (color-a color-1))
                 value))))
    (make-color :r r :g g :b b :a a)))

(defun mix-colors/rgb (color-1 color-2 value)
  (let ((r (+ (color-r color-1)
              (* (- (color-r color-2)
                    (color-r color-1))
                 value)))
        (g (+ (color-g color-1)
              (* (- (color-g color-2)
                    (color-g color-1))
                 value)))
        (b (+ (color-b color-1)
              (* (- (color-b color-2)
                    (color-b color-1))
                 value)))
        (a (+ (color-a color-1)
              (* (- (color-a color-2)
                    (color-a color-1))
                 value))))
    (values r g b a)))

(defstruct color-gradient
  (start (make-color :r 0.0 :g 0.0 :b 0.0 :a 1.0))
  (end (make-color :r 1.0 :g 1.0 :b 1.0 :a 1.0)))

(defun create-color-gradient (start-r start-g start-b start-a
                              end-r end-g end-b end-a)
  (make-color-gradient
   :start (make-color :r start-r :g start-g :b start-b :a start-a)
   :end (make-color :r end-r :g end-g :b end-b :a end-a)))

(defun get-color-from-gradient (gradient value)
  (mix-colors (color-gradient-start gradient)
              (color-gradient-end gradient)
              value))

(defun get-color-from-gradient/rgb (gradient value)
  (mix-colors/rgb (color-gradient-start gradient)
                  (color-gradient-end gradient)
                  value))

(defun set-color-from-gradient (gradient value)
  (multiple-value-bind (r g b a) (get-color-from-gradient/rgb gradient value)
    (gl:color r g b a)))

;;; Image
(defstruct image
  width height bpp data)

(defun create-image (width height bpp)
  (make-image :width width :height height :bpp bpp
              :data (make-array (* width height bpp) :initial-element 255
                                :element-type '(unsigned-byte 8))))

(defun image-set-pixel (image x y r &optional (g 255) (b 255) (a 255))
  (let ((index (+ x (* y (image-width image)))))
    (image-set-pixel/index image index r g b a)))

(defun image-set-pixel/index (image index r &optional (g 255) (b 255) (a 255))
  (let ((bpp (image-bpp image)))
    (ecase bpp
      (1 (setf (aref (image-data image) (* index bpp)) r))
      (2 (setf (aref (image-data image) (* index bpp)) r
               (aref (image-data image) (+ 1 (* index bpp))) g))
      (3 (setf (aref (image-data image) (* index bpp)) r
               (aref (image-data image) (+ 1 (* index bpp))) g
               (aref (image-data image) (+ 2 (* index bpp))) b))
      (4 (setf (aref (image-data image) (* index bpp)) r
               (aref (image-data image) (+ 1 (* index bpp))) g
               (aref (image-data image) (+ 2 (* index bpp))) b
               (aref (image-data image) (+ 3 (* index bpp))) a)))))

;;; 2D Texture
(defstruct texture
  width height bpp
  index ;; GL texture index
  ;; GL texture parameters
  (internal-format :rgba)
  (min-filter :linear)
  (mag-filter :linear)
  ;; min-lod max-lod
  ;; base-level max-level
  ;; wrap-s wrap-t wrap-r
  priority)

(defun create-texture (width height bpp data &rest args)
  "Create a new GL texture. Pixels should be of the '(unsigned-byte 8)
   type."
  (let ((tex (apply 'make-texture :index (first (gl:gen-textures 1))
                                  :width width :height height
                                  :bpp bpp
                                  args)))
    (select-texture tex)
    (gl:tex-image-2d :texture-2d 0 (texture-internal-format tex)
                     (texture-width tex)
                     (texture-height tex) 0
                     (ecase (texture-bpp tex)
                       (1 :alpha)
                       (2 :luminance-alpha)
                       (3 :rgb)
                       (4 :rgba))
                     :unsigned-byte
                     data)
    (gl:tex-parameter :texture-2d :texture-min-filter (texture-min-filter tex))
    (gl:tex-parameter :texture-2d :texture-mag-filter (texture-mag-filter tex))
    ;; (gl:tex-parameter :texture-2d :texture-min-lod (texture-min-lod tex))
    ;; (gl:tex-parameter :texture-2d :texture-max-lod (texture-max-lod tex))
    ;; (gl:tex-parameter :texture-2d :texture-base-level (texture-base-level tex))
    ;; (gl:tex-parameter :texture-2d :texture-max-level (texture-max-level tex))
    ;; (gl:tex-parameter :texture-2d :texture-wrap-s (texture-wrap-s tex))
    ;; (gl:tex-parameter :texture-2d :texture-wrap-t (texture-wrap-t tex))
    ;; (gl:tex-parameter :texture-2d :texture-wrap-r (texture-wrap-r tex))
    tex))

(defun update-texture (tex data &optional (x 0) (y 0)
                       (width (texture-width tex)) (height (texture-height tex)))
  (select-texture tex)
  (gl:tex-sub-image-2d :texture-2d 0 x y width height
                   (ecase (texture-bpp tex)
                     (1 :alpha)
                     (2 :luminance-alpha)
                     (3 :rgb)
                     (4 :rgba))
                   :unsigned-byte
                   data))

(defun create-texture-from-image (image &rest args)
  (apply 'create-texture
         (image-width image) (image-height image) (image-bpp image) (image-data image)
         args))

(defun update-texture-from-image (tex image)
  (update-texture tex (image-data image)))

(defun destroy-texture (tex)
  (gl:delete-textures (list (texture-index tex))))

(defvar *selected-texture-index* nil
  "Current texture in GL context.")

(defun select-texture (tex &key (env-mode :replace))
  "Set TEX as the current gl texture if necessary."
  (if tex
      (progn (unless *selected-texture-index*
               (gl:enable :texture-2d)
               (setf *selected-texture-index* -1))
             (unless (= (texture-index tex) *selected-texture-index*)
               (gl:bind-texture :texture-2d (texture-index tex))
               (gl:tex-env :texture-env
                           :texture-env-mode env-mode)
               (setf *selected-texture-index* (texture-index tex))))
      (progn (gl:disable :texture-2d)
             (setf *selected-texture-index* nil))))


;;; Shapes management
(defstruct shape
  (primitive :triangle-strip)
  vertices   ;; x,y,z
  colors     ;; r,g,b,a
  tex-coords ;; u,v
  indices)

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

(defun shape-get-vertex (shape index)
  (values (aref (shape-vertices shape) (* index 3))
          (aref (shape-vertices shape) (+ 1 (* index 3)))
          (aref (shape-vertices shape) (+ 2 (* index 3)))))

(defun shape-get-index (shape index)
  (aref (shape-indices shape) index))

(defun shape-set-vertex (shape index x y &optional (z 0.0))
  (setf (aref (shape-vertices shape) (* index 3)) x
        (aref (shape-vertices shape) (+ 1 (* index 3))) y
        (aref (shape-vertices shape) (+ 2 (* index 3))) z))

(defun shape-set-color (shape index r g b &optional (a 0.0))
  (setf (aref (shape-colors shape) (* index 4)) r
        (aref (shape-colors shape) (+ 1 (* index 4))) g
        (aref (shape-colors shape) (+ 2 (* index 4))) b
        (aref (shape-colors shape) (+ 3 (* index 4))) a))

(defun shape-set-tex-coord (shape index u v)
  (setf (aref (shape-tex-coords shape) (* index 2)) u
        (aref (shape-tex-coords shape) (+ 1 (* index 2))) v))

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

(defun render-shape (shape &optional (primitive (shape-primitive shape)))
  (gl:begin primitive)
  (loop with dim = (length (shape-indices shape))
       for index below dim
       for i = (aref (shape-indices shape) index)
       when (shape-colors shape)
       do (gl:color  (aref (shape-colors shape) (* i 4))
                     (aref (shape-colors shape) (+ 1 (* i 4)))
                     (aref (shape-colors shape) (+ 2 (* i 4)))
                     (aref (shape-colors shape) (+ 3 (* i 4))))
       when (shape-tex-coords shape)
       do (gl:tex-coord  (aref (shape-tex-coords shape) (* i 2))
                         (aref (shape-tex-coords shape) (+ 1 (* i 2))))
       do (gl:vertex (aref (shape-vertices shape) (* i 3))
                     (aref (shape-vertices shape) (+ 1 (* i 3)))
                     (aref (shape-vertices shape) (+ 2 (* i 3)))))
  (gl:end))

(defun create-shape (nb-vertices nb-indices &key color texture
                                             (primitive :triangles))
  (make-shape :primitive primitive
              :vertices (make-array (* nb-vertices 3)
                                    ;;:element-type 'single-float
                                    :adjustable t
                                    :fill-pointer 0)
              :colors (when color
                        (make-array (* nb-vertices 4)
                                    ;;:element-type 'single-float
                                    :adjustable t
                                    :fill-pointer 0))
              :tex-coords (when texture
                            (make-array (* nb-vertices 2)
                                        ;;:element-type 'single-float
                                    :adjustable t
                                        :fill-pointer 0))
              :indices (make-array nb-indices
                                   :element-type 'unsigned-byte
                                   :adjustable t
                                   :fill-pointer 0)))

(defun shape-ensure-adjustable (shape)
  (when (shape-vertices shape)
    (setf (shape-vertices shape) (ensure-adjustable (shape-vertices shape))))
  (when (shape-colors shape)
    (setf (shape-colors shape) (ensure-adjustable (shape-colors shape))))
  (when (shape-tex-coords shape)
    (setf (shape-tex-coords shape) (ensure-adjustable (shape-tex-coords shape))))
  (when (shape-indices shape)
    (setf (shape-indices shape) (ensure-adjustable (shape-indices shape)))))

(defun shape-add-vertex (shape x y &optional (z 0.0))
  ;;(declare (type single-float x y z))
  (vector-push-extend x (shape-vertices shape))
  (vector-push-extend y (shape-vertices shape))
  (vector-push-extend z (shape-vertices shape)))

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

(defun create-rectangle-shape (left bottom right top &key (filled t))
  (let ((shape (create-shape 4 5
                             :color nil
                             :texture t
                             :primitive (if filled :quads :line-strip))))
  (shape-add-vertex/index shape left bottom)
  (shape-add-tex-vertex shape 0.0 1.0)
  (shape-add-vertex/index shape right bottom)
  (shape-add-tex-vertex shape 1.0 1.0)
  (shape-add-vertex/index shape right top)
  (shape-add-tex-vertex shape 1.0 0.0)
  (shape-add-vertex/index shape left top)
  (shape-add-tex-vertex shape 0.0 0.0)
  (shape-add-indices shape 0)
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

;;; Bounding box
(defstruct bbox
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
  (loop for i from 0 below (fill-pointer (shape-vertices shape)) by 3 do
       (bbox-update bbox (aref (shape-vertices shape) i)
                         (aref (shape-vertices shape) (+ i 1))
                         (aref (shape-vertices shape) (+ i 2)))))

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
