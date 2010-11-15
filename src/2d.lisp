(in-package #:glaw)

;;; Sprite
(defstruct sprite
  "On screen image with transform and animation capabilities."
  x y width height
  bbox
  shape
  texture
  (flip :none))

(defun sprite-set-flip (it value)
  (case value
    (:vertical (setf (shape-tex-coords (sprite-shape it)) #(0.0 1.0 1.0 1.0 1.0 0.0 0.0 0.0)))
    (:horizontal (setf (shape-tex-coords (sprite-shape it)) #(1.0 0.0 0.0 0.0 0.0 1.0 1.0 1.0)))
    (:both (setf (shape-tex-coords (sprite-shape it)) #(1.0 1.0 0.0 1.0 0.0 0.0 1.0 0.0)))
    (:none (setf (shape-tex-coords (sprite-shape it)) #(0.0 0.0 1.0 0.0 1.0 1.0 0.0 1.0)))))

(defsetf sprite-flip sprite-set-flip)

(defun create-sprite (x y width height texture &key (bbox (make-bbox)) (flip :none)
                                                    (angle 0))
  (let ((sp (make-sprite :texture texture
                         :x x :y y :width width :height height
                         :bbox bbox :flip flip
                         :shape (create-rectangle-shape x y (+ x width) (+ y height)))))
    (setf (sprite-flip sp) flip)
    (rotate-sprite sp angle)
    (when bbox (bbox-overwrite/shape (sprite-bbox sp) (sprite-shape sp)))
    sp))

(defun render-sprite (sp)
  (select-texture (sprite-texture sp) :env-mode :modulate)
  (render-shape (sprite-shape sp)))

(define-anim-channels ((it sprite) data)
    (:texture (setf (sprite-texture it) data))
    (:tex-coords (setf (shape-tex-coords (sprite-shape it)) data))
    (:position  (move-sprite it (first data) (second data)))
    (:orientation (rotate-sprite it data)))

(defun translate-sprite (sp dx dy)
  (incf (sprite-x sp) dx)
  (incf (sprite-y sp) dy)
  (translate-shape (sprite-shape sp) dx dy)
  (when (sprite-bbox sp) (bbox-translate (sprite-bbox sp) dx dy)))

(defun move-sprite (sp x y)
  "Set sprite position."
  (let ((dx (- x (sprite-x sp)))
        (dy (- y (sprite-y sp))))
    (translate-sprite sp dx dy)))

(defun rotate-sprite (sp dangle)
  (rotate-shape-2d (sprite-shape sp) dangle (sprite-x sp) (sprite-y sp))
  (when (sprite-bbox sp) (bbox-overwrite/shape (sprite-bbox sp) (sprite-shape sp))))

;;; Tilemap
(defstruct tileset
  (start-index 0)
  texture
  tile-width tile-height
  (spacing 0)
  (margin 0)
  (cache nil))

(defun tileset-pixel-width (tileset)
  (texture-width (tileset-texture tileset)))

(defun tileset-pixel-height (tileset)
  (texture-height (tileset-texture tileset)))

(defun tileset-tiles-width (tileset)
  (1+ (truncate (/ (- (tileset-pixel-width tileset) (tileset-margin tileset))
                   (+ (tileset-tile-width tileset) (tileset-spacing tileset))))))

(defun tileset-tiles-height (tileset)
  (1+ (truncate (/ (- (tileset-pixel-height tileset) (tileset-margin tileset))
                   (+ (tileset-tile-height tileset) (tileset-spacing tileset))))))

(defun tileset-nb-tiles (tileset)
  (* (tileset-tiles-width tileset) (tileset-tiles-height tileset)))

(defun tileset-tile-tex-coords (tileset tile-index)
  "Returns tex-coord of top left corner and tile's width/height in the
   texture world of the tile designated by X,Y coordinates in the provided TILESET."
  (let* ((x (mod tile-index (tileset-tiles-width tileset)))
         (y (- (1- (tileset-tiles-height tileset))
               (truncate (/ (- tile-index x) (tileset-tiles-width tileset)))))
         (tile-tex-width (* 1.0 (/ (tileset-tile-width tileset) (tileset-pixel-width tileset))))
         (tile-tex-height (* 1.0 (/ (tileset-tile-height tileset) (tileset-pixel-height tileset))))
         (margin-tex-width (* 1.0 (/ (tileset-margin tileset) (tileset-pixel-width tileset))))
         (margin-tex-height (* 1.0 (/ (tileset-margin tileset) (tileset-pixel-height tileset))))
         (spacing-tex-width (* 1.0 (/ (tileset-spacing tileset) (tileset-pixel-width tileset))))
         (spacing-tex-height (* 1.0 (/ (tileset-spacing tileset) (tileset-pixel-height tileset)))))
    (values (+ (* x tile-tex-width) margin-tex-width (* x spacing-tex-width))
            (+ (* y tile-tex-height) margin-tex-height (* y spacing-tex-height))
            tile-tex-width tile-tex-height)))

(defun tileset-update-cache (tileset)
  (setf (tileset-cache tileset) (make-array (tileset-nb-tiles tileset) :initial-element nil))
  (loop for i below (tileset-nb-tiles tileset)
       do (multiple-value-bind (tex-x tex-y tex-width tex-height)
              (tileset-tile-tex-coords tileset i)
            (setf (aref (tileset-cache tileset) i) (list tex-x tex-y tex-width tex-height)))))

(defun tileset-tile-data (tileset tile-index)
  (unless (tileset-cache tileset)
    (tileset-update-cache tileset))
  (decf tile-index (tileset-start-index tileset))
  (let ((coords (aref (tileset-cache tileset) tile-index)))
        (values (first coords) (second coords) (third coords) (fourth coords))))

;; A tilemap layer
(defstruct tilemap
  width height
  tiles)

(defun tilemap-nb-tiles (tilemap)
  (* (tilemap-width tilemap) (tilemap-height tilemap)))

(defun create-tilemap (width height)
  (let ((map (make-tilemap :width width
                           :height height
                           :tiles (make-array (* width height) :element-type 'integer))))
    map))

(defun tilemap-fill (tilemap value)
  (loop for i below (tilemap-nb-tiles tilemap)
       do (setf (aref (tilemap-tiles tilemap) i) value)))

(defun tilemap-set-tile (tilemap x y value)
  (setf (aref (tilemap-tiles tilemap) (+ x (* y (tilemap-width tilemap)))) value))

(defun tilemap-get-tile (tilemap x y)
  (aref (tilemap-tiles tilemap) (+ x (* y (tilemap-width tilemap)))))

(defun render-tilemap (tilemap tileset)
  (let* ((tile-width (tileset-tile-width tileset))
         (tile-height (tileset-tile-height tileset)))
    (select-texture (tileset-texture tileset))
    (gl:begin :quads)
    (let ((tile-index 0))
      (loop for y below (tilemap-height tilemap)
         do (loop for x below (tilemap-width tilemap)
               do (multiple-value-bind (tex-x tex-y tex-width tex-height)
                      (tileset-tile-data tileset (aref (tilemap-tiles tilemap) tile-index))
                    (gl:tex-coord tex-x (+ tex-y tex-height))
                    (gl:vertex (* x tile-width) (* y tile-height))
                    (gl:tex-coord (+ tex-x tex-width) (+ tex-y tex-height))
                    (gl:vertex (* (+ x 1) tile-width) (* y tile-height))
                    (gl:tex-coord (+ tex-x tex-width) tex-y)
                    (gl:vertex (* (+ x 1) tile-width) (* (+ y 1) tile-height))
                    (gl:tex-coord tex-x tex-y)
                    (gl:vertex (* x tile-width) (* (+ y 1) tile-height))
                    (incf tile-index)))))
    (gl:end)))