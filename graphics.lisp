(in-package :glaw)

;; Framerate utils
;; TODO: add min/avg/max
(defstruct frame-counter
  (nb-frames 0 :type fixnum)
  (sample-size 100)
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
      (setf (frame-counter-rate *frame-counter*) fps)
      (format t "FPS: ~D~%" (frame-counter-rate *frame-counter*))
      (setf (frame-counter-last-render-time *frame-counter*)
            (get-internal-real-time))
      (setf (frame-counter-nb-frames *frame-counter*) 0))))

(defun current-fps ()
  (frame-counter-rate *frame-counter*))

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
  (glu:ortho-2d (2d-view-left view) (2d-view-right view)
                (2d-view-bottom view) (2d-view-top view))
  (gl:matrix-mode :modelview)
  (gl:load-identity))

(defun screen-to-view (x y view)
  (unless (or (zerop (2d-view-width view)) (zerop (2d-view-height view))
              (zerop *display-width*) (zerop *display-height*))
    (let ((width-factor (/ (2d-view-width view) *display-width*))
          (height-factor (/ (2d-view-height view) *display-height*)))
      (values (+ (2d-view-left view)
                 (* x width-factor))
              (+ (2d-view-bottom view)
                 (* (- *display-height* y) height-factor))))))


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

(defun copy-color (src dest)
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

;;; 2D Texture
(defstruct texture
  width height bpp
  index ;; GL texture index
  ;; GL texture parameters
  (min-filter :linear)
  (mag-filter :linear)
  ;; min-lod max-lod
  ;; base-level max-level
  ;; wrap-s wrap-t wrap-r
  priority)

(defun create-texture (width height bpp data &rest args)
  (let ((tex (apply 'make-texture :index (first (gl:gen-textures 1))
                                  :width width :height height
                                  :bpp bpp
                                  args)))
    (gl:bind-texture :texture-2d (texture-index tex))
    (gl:tex-image-2d :texture-2d 0 :rgba width height 0
                     (ecase bpp
                       (1 :luminance)
                       (2 :luminance-alpha)
                       (3 :rgb)
                       (4 :rgba))
                     :unsigned-byte
                     data)
    (gl:bind-texture :texture-2d (texture-index tex))
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

(defun create-texture-from-image (image &rest args)
  (apply 'create-texture
         (image-width image) (image-height image) (image-bpp image) (image-data image)
         args))

(defun destroy-texture (tex)
  (gl:delete-textures (list (texture-index tex))))

(defvar *selected-texture-index* -1
  "Current texture in GL context.")

(defun select-texture (tex &key (env-mode :replace))
  "Set TEX as the current gl texture if necessary."
  (unless (= (texture-index tex) *selected-texture-index*)
    (gl:bind-texture :texture-2d (texture-index tex))
    (gl:tex-env :texture-env
                :texture-env-mode env-mode)
    (setf *selected-texture-index* (texture-index tex))))


;;; Text rendering
(defstruct font
  texture base width height)

(defmethod create-bitmap-font (texture char-width char-height)
  ;; XXX: assume 16x16 character bitmap
  ;; TODO: use char-width and font-height when creating display lists
  (let ((fnt (make-font :width char-width :height char-height)))
    (setf (font-texture fnt) texture)
    (setf (font-base fnt) (gl:gen-lists 256))
    (select-texture (font-texture fnt) :env-mode :modulate)
    (loop for i from 0 to 256
       do (let ((cx (/ (mod i 16.0) 16.0))
                (cy (/ (truncate (/ i 16)) 16.0)))
            (gl:new-list (+ (font-base fnt) i) :compile)
            (gl:with-primitive :quads
              (gl:tex-coord cx (- 1.0 cy 0.0625))
              (gl:vertex 0 0)
              (gl:tex-coord (+ cx 0.0625) (- 1.0 cy 0.0625))
              (gl:vertex 16 0)
              (gl:tex-coord (+ cx 0.0625) (- 1.0 cy))
              (gl:vertex 16 16)
              (gl:tex-coord cx (- 1.0 cy))
              (gl:vertex 0 16))
            (gl:translate 13 0 0)
            (gl:end-list)))
    fnt))

(defun destroy-font (fnt)
  (destroy-texture (font-texture fnt))
  (gl:delete-lists (font-base fnt) 256))

(defun render-bitmap-string (x y text fnt)
  (gl:enable :texture-2d)
  (select-texture (font-texture fnt) :env-mode :modulate)
  (gl:with-pushed-matrix
    (gl:translate x y 0)
    (gl:list-base (font-base fnt))
    (let ((char-lst (loop for c across text
                       collect (char-code c))))
      (gl:call-lists char-lst)))
  (gl:disable :texture-2d))

(defun string-width (str fnt)
  (* (font-width fnt) (length str)))

(defun string-height (str fnt)
  (declare (ignore str))
  (font-height fnt))

(defmacro format-at (x y fnt fmt &rest values)
  `(render-bitmap-string ,x ,y (format nil ,fmt ,@values) ,fnt))


;;; Shapes management
(defstruct shape
  (primitive :triangle-strip)
  vertices   ;; x,y,z
  colors     ;; r,g,b
  tex-coords ;; u,v
  indices
  x-min y-min z-min
  x-max y-max z-max)

(defmacro with-shape-vertices (v-sym shape &body body)
  `(let ((,v-sym (shape-vertices ,shape)))
     ,@body))

(defmacro with-shape-colors (c-sym shape &body body)
  `(let ((,c-sym (shape-colors ,shape)))
     ,@body))

(defmacro with-shape-tex-coords (t-sym shape &body body)
  `(let ((,t-sym (shape-tex-coords ,shape)))
     ,@body))


(defun translate-shape (shape dx dy &optional (dz 0.0))
  (loop for i from 0 below (fill-pointer (shape-vertices shape)) by 3 do
       (incf (aref (shape-vertices shape) i) dx)
       (incf (aref (shape-vertices shape) (+ i 1)) dy)
       (incf (aref (shape-vertices shape) (+ i 2)) dz))
  (incf (shape-x-min shape) dx)
  (incf (shape-x-max shape) dx)
  (incf (shape-y-min shape) dy)
  (incf (shape-y-max shape) dy)
  (incf (shape-z-min shape) dz)
  (incf (shape-z-max shape) dz))

(defun render-shape (shape)
  (gl:begin (shape-primitive shape))
  (loop with dim = (fill-pointer (shape-indices shape))
       for index below dim
       for i = (aref (shape-indices shape) index)
       when (shape-colors shape)
       do (gl:color  (aref (shape-colors shape) (* i 3))
                     (aref (shape-colors shape) (+ 1 (* i 3)))
                     (aref (shape-colors shape) (+ 2 (* i 3)))
                     (aref (shape-colors shape) (+ 3 (* i 3))))
       when (shape-tex-coords shape)
       do (gl:tex-coord  (aref (shape-tex-coords shape) (* i 2))
                         (aref (shape-tex-coords shape) (+ 1 (* i 2))))
       do (gl:vertex (aref (shape-vertices shape) (* i 3))
                     (aref (shape-vertices shape) (+ 1 (* i 3)))
                     (aref (shape-vertices shape) (+ 2 (* i 3)))))
  (gl:end))

(defun render-bbox (shape)
  (gl:begin :line-strip)
  (gl:vertex (shape-x-min shape) (shape-y-max shape))
  (gl:vertex (shape-x-max shape) (shape-y-max shape))
  (gl:vertex (shape-x-max shape) (shape-y-min shape))
  (gl:vertex (shape-x-min shape) (shape-y-min shape))
  (gl:vertex (shape-x-min shape) (shape-y-max shape))
  (gl:end))

(defun coords-overlap-p (a b c d)
  (or (< c a d b) (< a c b d)
      (< a c d b) (< c a b d)))

(defun shape-intersect-p (shape-1 shape-2)
  (and (coords-overlap-p (shape-x-min shape-1) (shape-x-max shape-1)
                         (shape-x-min shape-2) (shape-x-max shape-2))
       (coords-overlap-p (shape-y-min shape-1) (shape-y-max shape-1)
                         (shape-y-min shape-2) (shape-y-max shape-2))))

(defun create-shape (nb-vertices nb-indices &key color texture
                                             (primitive :triangles))
  (make-shape :primitive primitive
              :vertices (make-array (* nb-vertices 3)
                                    :element-type 'single-float
                                    :fill-pointer 0)
              :colors (when color
                        (make-array (* nb-vertices 3)
                                    :element-type 'single-float
                                    :fill-pointer 0))
              :tex-coords (when texture
                            (make-array (* nb-vertices 2)
                                        :element-type 'single-float
                                        :fill-pointer 0))
              :indices (make-array nb-indices
                                   :element-type 'unsigned-byte
                                   :fill-pointer 0)))

(defun shape-update-bbox (shape x y &optional (z 0.0))
  (if (zerop (fill-pointer (shape-vertices shape)))
      (setf (shape-x-min shape) x
            (shape-y-min shape) y
            (shape-z-min shape) z
            (shape-x-max shape) x
            (shape-y-max shape) y
            (shape-z-max shape) z)
      (progn (when (< x (shape-x-min shape))
               (setf (shape-x-min shape) x))
             (when (< y (shape-y-min shape))
               (setf (shape-y-min shape) y))
             (when (< z (shape-z-min shape))
               (setf (shape-z-min shape) z))
             (when (> x (shape-x-max shape))
               (setf (shape-x-max shape) x))
             (when (> y (shape-y-max shape))
               (setf (shape-y-max shape) y))
             (when (> z (shape-z-max shape))
               (setf (shape-z-max shape) z)))))

(defun shape-add-vertex (shape x y &optional (z 0.0))
  (declare (type single-float x y z))
  (shape-update-bbox shape x y z)
  (vector-push x (shape-vertices shape))
  (vector-push y (shape-vertices shape))
  (vector-push z (shape-vertices shape)))

(defun shape-set-vertex (shape index x y &optional (z 0.0))
  (declare (type single-float x y z))
  (setf (aref (shape-vertices shape) (* index 3))
        x
        (aref (shape-vertices shape) (+ 1 (* index 3)))
        y
        (aref (shape-vertices shape) (+ 2 (* index 3)))
        z))

(defun shape-add-color (shape color)
  (declare (type color color))
  (vector-push (color-r color) (shape-colors shape))
  (vector-push (color-g color) (shape-colors shape))
  (vector-push (color-b color) (shape-colors shape))
  (vector-push (color-a color) (shape-colors shape)))

(defun shape-add-color/rgb (shape r g b &optional (a 1.0))
  (declare (type single-float r g b a))
  (vector-push r (shape-colors shape))
  (vector-push g (shape-colors shape))
  (vector-push b (shape-colors shape))
  (vector-push a (shape-colors shape)))

(defun shape-add-tex-vertex (shape u v)
  (declare (type single-float u v))
  (vector-push u (shape-tex-coords shape))
  (vector-push v (shape-tex-coords shape)))

(defun shape-add-indices (shape &rest indices)
  (dolist (i indices)
    (declare (type unsigned-byte i))
    (vector-push i (shape-indices shape))))

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
                             :primitive :triangles)))
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
    (loop for x from 0 below (1- width)
       do (loop for y from 0 below (1- height)
               for i = (+ x (* width y))
             do (shape-add-indices shape
                   i (+ i 1) (+ i width)
                   (+ i width 1) (+ i width) (+ i 1))))
    shape))

(defun create-circle-shape (x y radius &key (resolution 20) (filledp t))
  (let ((shape (create-shape (round (/ 360 resolution) 1.0)
                             (round (/ 360 resolution) 1.0)
                             :primitive (if filledp :triangle-fan :line-loop))))
    (loop for angle from 0 to 360 by resolution
       for radian = (deg->rad angle)
       do (shape-add-vertex/index shape
                                  (+ x (* radius (cos radian)))
                                  (+ y (* radius (sin radian)))))
    shape))

(defun create-triangle-shape (x0 y0 x1 y1 x2 y2)
  (let ((shape (create-shape 3
                             3
                             :primitive :triangles)))
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

(defun create-rectangle-shape (left bottom right top &key (filledp t))
  (let ((shape (create-shape 4 5
                             :color nil
                             :texture t
                             :primitive (if filledp :quads :line-strip))))
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

;;; Sprites are just textured quads
;; TODO: use point sprites when possible
;; TODO: implement sprite batches
(defstruct sprite
  shape
  texture
  (flip nil))      ;; :vertical, :horizontal or :both

(defun create-sprite (x y width height texture)
  (make-sprite :texture texture
               :shape (create-rectangle-shape x y (+ x width) (+ y height))))

(defun render-sprite (sp)
  (gl:enable :texture-2d)
  (select-texture (sprite-texture sp) :env-mode :modulate)
  (render-shape (sprite-shape sp)))


;; ;;; Tilemap
;; (defstruct tileset
;;   texture
;;   tilesize
;;   width height)

;; (defun create-tileset (image tilesize width height)
;;   (let ((tset (make-tileset :texture (create-texture image)
;;                             :tilesize tilesize
;;                             :width width
;;                             :height height)))
;;     tset))

;; (defun tile-tex-coords (tileset tile-index)
;;   "Returns tex-coord of top left corner of the tile designated by
;;    TILE-INDEX in the provided TILESET and tile's width/height in the
;;    texture world."
;;   (let ((tile-width (/ (tileset-tilesize tileset)
;;                        (* (tileset-width tileset)
;;                           (tileset-tilesize tileset))))
;;         (tile-height (/ (tileset-tilesize tileset)
;;                        (* (tileset-height tileset)
;;                           (tileset-tilesize tileset)))))
;;     (values (* tile-index tile-width) (* tile-index tile-height)
;;             tile-width tile-height)))

;; (defun destroy-tileset (tileset)
;;   (destroy-texture (tileset-texture tileset)))

;; (defstruct tilemap
;;   width height
;;   tiles
;;   tileset)

;; (defun create-tilemap (tileset width height)
;;   (let ((map (make-tilemap :tileset tileset
;;                            :width width
;;                            :height height
;;                            :tiles (make-array (* width height)
;;                                                :element-type 'integer))))
;;     map))

;; (defun destroy-tilemap (tilemap)
;;   (setf (tilemap-tiles tilemap) nil))

;; (defun set-tile (tilemap x y value)
;;   (setf (aref (tilemap-tiles tilemap) (+ x (* y (tilemap-width tilemap))))
;;         value))

;; (defun get-tile (tilemap x y)
;;   (aref (tilemap-tiles tilemap) (+ x (* y (tilemap-width tilemap)))))

;; (defun render-tilemap (tilemap)
;;   (let* ((tilesize (tileset-tilesize (tilemap-tileset tilemap)))
;;          (width (* (tilemap-width tilemap) tilesize))
;;          (height (* (tilemap-height tilemap) tilesize)))
;;     (select-texture (tileset-texture (tilemap-tileset tilemap)))
;;     (gl:begin :quads)
;;     (let ((tile-index 0))
;;       (loop for x from 0 upto (tilemap-width tilemap)
;;          do (loop for y from 0 upto (tilemap-height tilemap)
;;                do (multiple-value-bind (tex-x tex-y tex-width tex-height)
;;                       (tile-tex-coords (tilemap-tileset tilemap)
;;                                       (aref (tilemap-tiles tilemap) tile-index))
;;                     (gl:tex-coord tex-x tex-y)
;;                     (gl:vertex (* x tilesize) (* y tilesize))
;;                     (gl:tex-coord (+ tex-x tex-width) tex-y)
;;                     (gl:vertex (* (+ x 1) tilesize) (* y tilesize))
;;                     (gl:tex-coord (+ tex-x tex-width) (+ tex-y tex-height))
;;                     (gl:vertex (* (+ x 1) tilesize) (* (+ y 1) tilesize))
;;                     (gl:tex-coord tex-x (+ tex-y tex-height))
;;                     (gl:vertex (* x tilesize) (* (+ y 1) tilesize))
;;                     (incf tile-index)))))
;;     (gl:end)))