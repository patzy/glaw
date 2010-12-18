(in-package :glaw)

;; Framerate utils
(defstruct frame-counter
  (sample-size 2.0) ;; seconds
  (min most-positive-single-float)
  (max most-negative-single-float)
  (sum 0.0)
  (nb-frames 0)
  (average 0.0)
  (time-ratio 0.5)
  (last-render-time 1.0))

(defvar *frame-counter* (make-frame-counter)
  "Default framerate counter.")

(defun frame-counter-update (counter dt)
  "Updates framerate informations. DT is the elapsed time since last frame was rendered."
  (setf (frame-counter-last-render-time counter) (+ (* dt (frame-counter-time-ratio counter))
                                                    (* (frame-counter-last-render-time counter)
                                                       (- 1.0 (frame-counter-time-ratio counter)))))
  (incf (frame-counter-nb-frames counter))
  (incf (frame-counter-sum counter) dt)
  (when (>= (frame-counter-sum counter) (frame-counter-sample-size counter))
    (let ((fps (/ (frame-counter-nb-frames counter) (frame-counter-sum counter))))
      (when (< fps (frame-counter-min counter))
        (setf (frame-counter-min counter) fps))
      (when (> fps (frame-counter-max counter))
        (setf (frame-counter-max counter) fps))
      (setf (frame-counter-nb-frames counter) 0
            (frame-counter-sum counter) 0.0
            (frame-counter-average counter) fps))))

(defun frame-counter-current (counter)
  (/ 1.0 (frame-counter-last-render-time counter)))

(let ((last-fps-update (get-internal-real-time)))
  (defun update-fps ()
    (let ((dt (/ (- (get-internal-real-time) last-fps-update) internal-time-units-per-second)))
      (frame-counter-update *frame-counter* dt)
      (setf last-fps-update (get-internal-real-time)))))

(defun current-fps ()
  (frame-counter-current *frame-counter*))

(defun min-fps ()
  (frame-counter-min *frame-counter*))

(defun max-fps ()
  (frame-counter-max *frame-counter*))

(defun avg-fps ()
  (frame-counter-average *frame-counter*))

;;; General rendering
(defvar *display-width* 0)
(defvar *display-height* 0)

(defun setup-3d-defaults ()
  (gl:clear-color 0.3 0.3 0.3 0)
  (gl:enable :depth-test)
  (gl:depth-func :lequal)
  (gl:depth-mask :enable)
  (gl:disable :cull-face)
  (gl:cull-face :back)
  (gl:enable :lighting)
  (gl:enable :light0)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :normalize)
  (gl:shade-model :smooth)
  (gl:enable :texture-2d)
  ;; some default head light
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:light :light0 :position #(0.0 0.0 0.0 1.0))
  (gl:light :light0 :diffuse #(1.0 1.0 1.0 1.0))
  (gl:light :light0 :specular #(1.0 1.0 1.0 1.0))
  (gl:light :light0 :ambient #(1.0 1.0 1.0 1.0)))

(defun setup-2d-defaults ()
  (gl:clear-color 0.3 0.3 0.3 0)
  (gl:disable :depth-test)
  (gl:disable :cull-face)
  (gl:disable :lighting)
  (gl:disable :light0)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:enable :texture-2d))

(defun draw-origin (&optional (scale 20.0))
  (gl:with-primitive :lines
    (gl:color 1.0 0.0 0.0 1.0)
    (gl:vertex 0.0 0.0 0.0)
    (gl:vertex scale 0.0 0.0)
    (gl:color 0.0 1.0 0.0 1.0)
    (gl:vertex 0.0 0.0 0.0)
    (gl:vertex 0.0 scale 0.0)
    (gl:color 0.0 0.0 1.0 1.0)
    (gl:vertex 0.0 0.0 0.0)
    (gl:vertex 0.0 0.0 scale)))

(defun reshape (width height)
  ;; set viewport to full window
  (gl:viewport 0 0 width height)
  (setf *display-width* width)
  (setf *display-height* height))

(defun set-view (proj-mtx view-mtx)
  (gl:matrix-mode :projection)
  (gl:load-matrix proj-mtx)
  (gl:matrix-mode :modelview)
  (gl:load-matrix view-mtx))

(defun begin-draw ()
  (gl:clear :color-buffer :depth-buffer))

(defun end-draw ()
  (update-fps)
  (gl:flush))

;;; FBO
(defun create-gl-buffer (width height)
  (let ((w (min (nearest-power-of-two width)
                (gl:get-integer :max-texture-size)))
        (h (min (nearest-power-of-two height)
                (gl:get-integer :max-texture-size)))
        (framebuffer (first (gl:gen-framebuffers-ext 1)))
        (depthbuffer (first (gl:gen-renderbuffers-ext 1)))
        (texture (first (gl:gen-textures 1))))
    (gl:bind-framebuffer-ext :framebuffer-ext framebuffer)
    ;; setup and attach texture
    (gl:bind-texture :texture-2d texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-image-2d :texture-2d 0 :rgba w h 0 :rgba :unsigned-byte (cffi:null-pointer))
    (gl:generate-mipmap-ext :texture-2d)
    (gl:bind-texture :texture-2d 0)
    (gl:framebuffer-texture-2d-ext :framebuffer-ext
                                   :color-attachment0-ext
                                   :texture-2d
                                   texture
                                   0)
    ;; setup and attach depth buffer
    (gl:bind-renderbuffer-ext :renderbuffer-ext depthbuffer)
    (gl:renderbuffer-storage-ext :renderbuffer-ext :depth-component24 w h)
    (gl:framebuffer-renderbuffer-ext :framebuffer-ext
                                     :depth-attachment-ext
                                     :renderbuffer-ext
                                     depthbuffer)
    ;; validate framebuffer
    (let ((framebuffer-status (gl:check-framebuffer-status-ext :framebuffer-ext)))
      (unless (gl::enum= framebuffer-status :framebuffer-complete-ext)
        (error "Framebuffer not complete: ~A." framebuffer-status)))
    framebuffer))

;;; Colors helpers
(defstruct (color (:type (vector float)))
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
  "Simple image structure. Origin is generally top-left but depends on the loader
   you use."
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

(defun image-get-pixel (image x y)
  (let ((index (+ x (* y (image-width image)))))
    (image-get-pixel/index image index)))

(defun image-get-pixel/index (image index)
  (let ((bpp (image-bpp image)))
    (ecase bpp
      (1 (values (aref (image-data image) (* index bpp))
                 0 0 0))
      (2 (values (aref (image-data image) (* index bpp))
                 (aref (image-data image) (+ 1 (* index bpp)))
                 0 0))
      (3 (values (aref (image-data image) (* index bpp))
                 (aref (image-data image) (+ 1 (* index bpp)))
                 (aref (image-data image) (+ 2 (* index bpp)))
                 0))
      (4 (values (aref (image-data image) (* index bpp))
                 (aref (image-data image) (+ 1 (* index bpp)))
                 (aref (image-data image) (+ 2 (* index bpp)))
                 (aref (image-data image) (+ 3 (* index bpp))))))))

;;; 2D Texture
(defstruct texture
  width height bpp data
  index ;; GL texture index
  ;; GL texture parameters
  (internal-format :rgba)
  (min-filter :linear)
  (mag-filter :linear)
  ;; min-lod max-lod
  ;; base-level max-level
  (wrap-s :repeat)
  (wrap-t :repeat)
  (wrap-r :repeat)
  priority)

(defun create-texture (width height bpp data &rest args)
  "Create a new GL texture. Texture's origin is bottom-left."
  (let ((tex (apply 'make-texture :index (first (gl:gen-textures 1))
                                  :width width :height height
                                  :bpp bpp :data data
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
    (gl:tex-parameter :texture-2d :texture-wrap-s (texture-wrap-s tex))
    (gl:tex-parameter :texture-2d :texture-wrap-t (texture-wrap-t tex))
    (gl:tex-parameter :texture-2d :texture-wrap-r (texture-wrap-r tex))
    tex))

(defun update-texture (tex data &optional (x 0) (y 0)
                       (width (texture-width tex)) (height (texture-height tex)))
  (setf (texture-data tex) data)
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
  normals    ;; x y z
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

(defun scale-shape-2d (shape sx sy)
  (loop for i from 0 below (length (shape-vertices shape)) by 3 do
       (let ((x (aref (shape-vertices shape) i))
             (y (aref (shape-vertices shape) (+ i 1))))
         (setf (aref (shape-vertices shape) i) (* x sx)
               (aref (shape-vertices shape) (+ i 1)) (* y sy)))))

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

(defun create-shape (nb-vertices nb-indices &key color texture normals
                                             (primitive :triangles))
  (make-shape :primitive primitive
              :vertices (make-array (* nb-vertices 3)
                                    ;;:element-type 'single-float
                                    :adjustable t
                                    :fill-pointer 0)
              :normals (when normals
                         (make-array (* nb-vertices 3)
                                     ;;:element-type 'single-float
                                     :adjustable t
                                     :fill-pointer 0))
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
  (when (shape-normals shape)
    (setf (shape-normals shape) (ensure-adjustable (shape-normals shape))))
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

(defasset :shape '("shape")
  ;; load
  (lambda (filename)
    (with-open-file (in filename :direction :input)
      ;; FIXME: unable to modify loaded shape (no fill-pointer)
      (read in)))
  ;; unload
  (lambda (shape)
    ;; nothing to do
    (declare (ignore shape))
    (values)))


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
                                                          tex-width tex-height)
  (let ((shape (create-shape 4 5
                             :color nil
                             :texture t
                             :primitive (if filled :quads :line-strip)))
        (width (- right left))
        (height (- top bottom)))
  (shape-add-vertex/index shape left bottom)
  (shape-add-tex-vertex shape 0.0 0.0)
  (shape-add-vertex/index shape right bottom)
  (shape-add-tex-vertex shape (if tex-width (/ width tex-width) 1.0) 0.0)
  (shape-add-vertex/index shape right top)
  (shape-add-tex-vertex shape (if tex-width (/ width tex-width) 1.0)
                              (if tex-height (/ height tex-height) 1.0))
  (shape-add-vertex/index shape left top)
  (shape-add-tex-vertex shape 0.0 (if tex-height (/ height tex-height) 1.0))
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

(defun bbox-inside-p (bbox x y)
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
