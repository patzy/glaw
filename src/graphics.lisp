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

(defun set-background-color (color)
  (gl:clear-color (color-r color)
                  (color-g color)
                  (color-b color)
                  0))

(defun set-background-color/rgb (r g b)
  (gl:clear-color r g b 0))

(defun setup-3d-defaults ()
  (set-background-color #(0.3 0.3 0.3 0.0))
  (set-render-state +default-3d-render-state+)
  ;; some default head light
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (gl:enable :light0)
  (gl:light :light0 :position #(0.0 0.0 0.0 1.0))
  (gl:light :light0 :ambient #(0.2 0.2 0.2 1.0))
  (gl:light :light0 :diffuse #(0.8 0.8 0.8 1.0))
  (gl:light :light0 :specular #(0.5 0.5 0.5 1.0)))

(defun setup-2d-defaults ()
  (set-background-color #(0.3 0.3 0.3 0.0))
  (set-render-state +default-2d-render-state+))

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
  ;; set viewport to full screen
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
                     (if data data (cffi::null-pointer)))
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

(defun select-texture (tex &key (env-mode :replace)
                                (unit :texture0))
  "Set TEX as the current gl texture if necessary."
  (gl:active-texture unit)
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

;;; Renderbuffer
(defstruct renderbuffer
  width
  height
  format
  index)

(defun create-renderbuffer (width height &optional (format :rgba))
  (let ((index (first (gl:gen-renderbuffers-ext 1))))
    (gl:bind-renderbuffer-ext :renderbuffer-ext index)
    (gl:renderbuffer-storage-ext :renderbuffer-ext format width height)
    (make-renderbuffer :width width :height height :format format :index index)))

(defun destroy-renderbuffer (rb)
  (gl:delete-renderbuffers-ext (renderbuffer-index rb)))

;;; Framebuffer
(defstruct framebuffer
  width
  height
  index
  colors
  depth
  stencil)

(defun %framebuffer-attach-texture (fb texture attach-point)
  (assert (and (= (texture-height texture) (framebuffer-height fb))
               (= (texture-width texture) (framebuffer-width fb))))
  (let ((tex-index (texture-index texture)))
    (gl:bind-texture :texture-2d tex-index)
    (gl:framebuffer-texture-2d-ext :framebuffer-ext
                                   attach-point
                                   :texture-2d
                                   tex-index
                                   0)))

(defun %framebuffer-attach-renderbuffer (fb rb attach-point)
  (assert (and (= (renderbuffer-height rb) (framebuffer-height fb))
               (= (renderbuffer-width rb) (framebuffer-width fb))))
  (let ((rb-index (renderbuffer-index rb)))
    (gl:bind-renderbuffer-ext :renderbuffer-ext rb-index)
    (gl:framebuffer-renderbuffer-ext :framebuffer-ext
                                     attach-point
                                     :renderbuffer-ext
                                     rb-index)))

(defmethod framebuffer-attach-color (fb (buf texture) &optional (index 0))
  (%framebuffer-attach-texture fb buf
                               (ecase index
                                 (0 :color-attachment0-ext)
                                 (1 :color-attachment1-ext)
                                 (2 :color-attachment2-ext)
                                 (3 :color-attachment3-ext)))
  (setf (aref (framebuffer-colors fb) index) buf))

(defmethod framebuffer-attach-color (fb (buf renderbuffer) &optional (index 0))
  (%framebuffer-attach-renderbuffer fb buf
                                    (ecase index
                                      (0 :color-attachment0-ext)
                                      (1 :color-attachment1-ext)
                                      (2 :color-attachment2-ext)
                                      (3 :color-attachment3-ext)))
  (setf (aref (framebuffer-colors fb) index) buf))

(defmethod framebuffer-attach-depth (fb (buf texture) &optional (index 0))
  (%framebuffer-attach-texture fb buf :depth-attachment-ext)
  (setf (framebuffer-depth fb) buf))

(defmethod framebuffer-attach-depth (fb (buf renderbuffer) &optional (index 0))
  (%framebuffer-attach-renderbuffer fb buf :depth-attachment-ext)
  (setf (framebuffer-depth fb) buf))

(defmethod framebuffer-attach-stencil (fb (buf texture) &optional (index 0))
  (%framebuffer-attach-texture fb buf :stencil-attachment-ext)
  (setf (framebuffer-stencil fb) buf))

(defmethod framebuffer-attach-color (fb (buf renderbuffer) &optional (index 0))
  (%framebuffer-attach-renderbuffer fb buf :stencil-attachment-ext)
  (setf (framebuffer-stencil fb) buf))

(defun create-framebuffer (width height &key (colors '())
                                             (depth nil)
                                             (stencil nil))
  (let ((w (min (nearest-power-of-two width)
                (gl:get-integer :max-texture-size)))
        (h (min (nearest-power-of-two height)
                (gl:get-integer :max-texture-size)))
        (framebuffer (first (gl:gen-framebuffers-ext 1))))
    (gl:bind-framebuffer-ext :framebuffer-ext framebuffer)
    ;; FIXME: get number of color attachment from OpenGL
    (make-framebuffer :index framebuffer :width w :height h
                      :colors (make-array 4) :depth nil :stencil nil)))

(defun destroy-framebuffer (fb)
  (gl:delete-framebuffers-ext 1 (framebuffer-index fb)))

(defvar *selected-framebuffer-index* nil
  "Currently selected framebuffer object index.")

(defun select-framebuffer (fb)
  (gl:bind-framebuffer-ext :framebuffer-ext (if fb (framebuffer-index fb) 0)))

;;; Primitive rendering
;; TODO: use vertex arrays etc
(defun render-primitive (indices vertices &key (primitive :triangles)
                                                colors tex-coords normals
                                                (start 0) (end (length indices)))
  (gl:with-primitive primitive
    ;; immediate mode
    (loop for index from start below end
       for i = (aref indices index)
       when colors
       do (gl:color (aref colors (* i 4))
                    (aref colors (+ 1 (* i 4)))
                    (aref colors (+ 2 (* i 4)))
                    (aref colors (+ 3 (* i 4))))
       when tex-coords
       do (gl:tex-coord  (aref tex-coords (* i 2))
                         (aref tex-coords (+ 1 (* i 2))))
       when normals
       do (gl:normal (aref normals (* i 3))
                     (aref normals (+ 1 (* i 3)))
                     (aref normals (+ 2 (* i 3))))
       do (gl:vertex (aref vertices (* i 3))
                     (aref vertices (+ 1 (* i 3)))
                     (aref vertices (+ 2 (* i 3)))))))
;;; Basic material
(defstruct material
  (ambient #(0.3 0.3 0.3 1.0))
  diffuse ;; nil means same as diffuse
  (specular #(1.0 1.0 1.0 1.0))
  (shininess 1.0)
  (emissive #(0.0 0.0 0.0 1.0)))

(defvar +default-material+
  (make-material))

(defun material-set-alpha (mat alpha)
  "Set material transparency."
  (setf (color-a (material-ambient mat)) alpha
        (color-a (material-diffuse mat)) alpha
        (color-a (material-specular mat)) alpha
        (color-a (material-emissive mat)) alpha))

(defun material-alpha (mat)
  (color-a (material-ambient mat)))

(defsetf material-alpha material-set-alpha)

;;; FIXME: check if material is not already set
;;; FIXME: use color material (supposed to be faster)
(defun set-material (mat)
  (when mat
    (if (material-diffuse mat)
        (progn (gl:material :front-and-back :ambient (material-ambient mat))
               (gl:material :front-and-back :diffuse (material-diffuse mat)))
        (gl:material :front-and-back :ambient-and-diffuse (material-ambient mat)))
    (gl:material :front-and-back :specular (material-specular mat))
    (gl:material :front-and-back :shininess (material-shininess mat))
    (gl:material :front-and-back :emission (material-emissive mat))))

;;; Shaders
(defstruct shader
  source
  id ;; GL id
  programs ;; programs this shader is attached to
  (needs-compile t))

(defun %shader-add-program (shader prg)
  (push prg (shader-programs shader)))

(defun %shader-remove-program (shader prg)
  (setf (shader-programs shader) (remove prg (shader-programs shader))))

(defun shader-compile (shader)
  (assert (and (shader-source shader)
               (shader-id shader)))
  (dformat "Compiling shader.~%")
  (gl:compile-shader (shader-id shader))
  (dformat "Shader compile log:~%~S~%" (gl:get-shader-info-log (shader-id shader)))
  (setf (shader-needs-compile shader) nil))

(defun %shader-set-source (shader source)
  (setf (shader-source shader) source
        (shader-needs-compile shader) t)
  (gl:shader-source (shader-id shader) (shader-source shader)))

(defun shader-set-source (shader source &optional compile)
  (%shader-set-source shader source)
  (when compile
    (shader-compile shader)))

(defun create-shader (type &optional source compile)
  (assert (or (eq type :vertex-shader) (eq type :fragment-shader)))
  (let ((sh (make-shader)))
    (setf (shader-id sh) (gl:create-shader type))
    (when source
      (shader-set-source sh source))
    (when compile
      (shader-compile sh))
    (dformat "Created shader: ~S~%" sh)
    sh))

(defun create-shader-from-file (type file &optional compile)
  (create-shader type (file->strings file) compile))

(defun create-vertex-shader (&optional source compile)
  (create-shader :vertex-shader source compile))

(defun create-vertex-shader-from-file (file &optional compile)
  (create-shader-from-file :vertex-shader file compile))

(defun create-fragment-shader (&optional source compile)
  (create-shader :fragment-shader source compile))

(defun create-fragment-shader-from-file (file &optional compile)
  (create-shader-from-file :fragment-shader file compile))

;; FIXME: detach before delete?
(defun destroy-shader (sh)
  (gl:delete-shader (shader-id sh)))

(defstruct shader-program
  vertex
  fragment
  id
  uniforms ;; TODO
  attribs
  (needs-link t))

(defun shader-program-attach-vertex (prg vtx-shader)
  (setf (shader-program-vertex prg) vtx-shader
        (shader-program-needs-link prg) t)
  (gl:attach-shader (shader-program-id prg) (shader-id vtx-shader))
  (%shader-add-program vtx-shader prg))

(defun shader-program-detach-vertex (prg)
  (gl:detach-shader (shader-program-id prg) (shader-id (shader-program-vertex prg)))
  (%shader-remove-program (shader-program-vertex prg) prg)
  (setf (shader-program-vertex prg) nil))

(defun shader-program-attach-fragment (prg frag-shader)
  (setf (shader-program-fragment prg) frag-shader
        (shader-program-needs-link prg) t)
  (gl:attach-shader (shader-program-id prg) (shader-id frag-shader))
  (%shader-add-program frag-shader prg))

(defun shader-program-detach-fragment (prg)
  (gl:detach-shader (shader-program-id prg) (shader-id (shader-program-fragment prg)))
  (%shader-remove-program (shader-program-fragment prg) prg)
  (setf (shader-program-fragment prg) nil))

(defun shader-program-link (prg)
  (assert (shader-program-id prg))
  (dformat "Linking shader program.~%")
  (setf (shader-program-needs-link prg) nil)
  (gl:link-program (shader-program-id prg))
  (dformat "Program info log:~%~S~%" (gl:get-program-info-log (shader-program-id prg))))

(defun set-shader-program (prg)
  (if prg
      (gl:use-program (shader-program-id prg))
      (gl:use-program 0)))

(setf *print-circle* t)

(defun create-shader-program (&optional vertex fragment link)
  (let ((prg (make-shader-program)))
    (setf (shader-program-id prg) (gl:create-program))
    (dformat "Created program with id: ~S~%" (shader-program-id prg))
    (when (and vertex fragment)
      (shader-program-attach-vertex prg vertex)
      (shader-program-attach-fragment prg fragment))
    (when (and vertex fragment link)
      (shader-program-link prg))
    (dformat "Created shader program: ~S~%" prg)
    prg))

(defun destroy-shader-program (prg)
  (when (shader-program-vertex prg)
    (shader-program-detach-vertex prg))
  (when (shader-program-fragment prg)
    (shader-program-detach-fragment prg))
  (gl:delete-program (shader-program-id prg)))

;;; Render state
(defstruct render-state
  (depth-func :lequal)
  (depth-write t)
  (cull-face :back)
  (blend-func '(:src-alpha :one-minus-src-alpha))
  (shade-model :smooth)
  (wireframe nil)
  (lighting t)
  (texturing t))

(defvar +default-3d-render-state+
  (make-render-state :depth-func :lequal
                     :depth-write t
                     :cull-face :back
                     :blend-func nil
                     :shade-model :smooth
                     :wireframe nil
                     :lighting t
                     :texturing nil))
(defvar +default-2d-render-state+
  (make-render-state :depth-func nil
                     :depth-write nil
                     :cull-face nil
                     :blend-func '(:src-alpha :one-minus-src-alpha)
                     :shade-model :smooth
                     :wireframe nil
                     :lighting nil
                     :texturing nil))

;; TODO: only apply differences from *current-render-state*
(defun set-render-state (rs)
  (if (render-state-depth-func rs)
      (progn (gl:enable :depth-test)
             (gl:depth-func (render-state-depth-func rs)))
      (gl:disable :depth-test))
  (gl:depth-mask (if (render-state-depth-write rs) :enable :disable))
  (if (render-state-cull-face rs)
      (progn (gl:enable :cull-face)
             (gl:cull-face (render-state-cull-face rs)))
      (gl:disable :cull-face))
  (if (render-state-blend-func rs)
      (progn (gl:enable :blend)
             (gl:blend-func (first (render-state-blend-func rs))
                           (second (render-state-blend-func rs))))
      (gl:disable :blend))
  (gl:shade-model (render-state-shade-model rs))
  (if (render-state-wireframe rs)
      (gl:polygon-mode :front-and-back :line)
      (gl:polygon-mode :front-and-back :fill))
  (if (render-state-lighting rs)
      (gl:enable :lighting)
      (gl:disable :lighting))
  (if (render-state-texturing rs)
      (gl:enable :texture-2d)
      (gl:disable :texture-2d)))

;;; Appearance
(defstruct appearance
  (material +default-material+)
  (textures '())
  (shader nil)
  (render-state +default-3d-render-state+)
  (next nil))
