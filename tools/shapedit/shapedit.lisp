(defpackage :shapedit
  (:use #:cl)
  (:export #:run))

(in-package #:shapedit)

(defvar *view* nil)
(defvar *font* nil)
(defvar *screens* nil)

(defstruct edition-screen
  (view (glaw:create-2d-view -512 -384 1024 768))
  (selection 0) ;; selected vertex index
  (selection-shape (glaw:create-rectangle-shape -5.0 -5.0 5.0 5.0)) ;; in screen view
  (selection-boxes '())
  (creation-boxes '())
  (edges '()) ;; indices pairs
  shape
  ;; input
  (moving nil))

(defun edition-screen-update-boxes (scr)
  ;; vertex translation
  (setf (edition-screen-selection-boxes scr) '())
  (loop for i below (glaw:shape-nb-vertices (edition-screen-shape scr)) do
       (let ((box (glaw:create-bbox-from-shape (edition-screen-selection-shape scr))))
         (multiple-value-bind (x y z) (glaw:shape-get-vertex (edition-screen-shape scr) i)
           (declare (ignore z))
           (multiple-value-bind (scr-x scr-y) (glaw:view-to-screen x y (edition-screen-view scr))
             (glaw:bbox-translate box scr-x scr-y)))
         (push box (edition-screen-selection-boxes scr))))
  (setf (edition-screen-selection-boxes scr) (reverse (edition-screen-selection-boxes scr)))
  ;; triangle creation
  (setf (edition-screen-creation-boxes scr) '())
  (setf (edition-screen-edges scr) '())
  (loop for index below (glaw:shape-nb-indices (edition-screen-shape scr)) by 3 do
       (let ((i (glaw:shape-get-index (edition-screen-shape scr) index))
             (j (glaw:shape-get-index (edition-screen-shape scr) (+ 1 index)))
             (k (glaw:shape-get-index (edition-screen-shape scr) (+ 2 index))))
         ;; i-j edge
         (let ((box (glaw:create-bbox-from-shape (edition-screen-selection-shape scr)))
               x1 y1 x2 y2)
           (multiple-value-bind (x y z) (glaw:shape-get-vertex (edition-screen-shape scr) i)
             (declare (ignore z))
             (multiple-value-bind (scr-x scr-y) (glaw:view-to-screen x y (edition-screen-view scr))
               (setf x1 scr-x y1 scr-y)))
           (multiple-value-bind (x y z) (glaw:shape-get-vertex (edition-screen-shape scr) j)
             (declare (ignore z))
             (multiple-value-bind (scr-x scr-y) (glaw:view-to-screen x y (edition-screen-view scr))
               (setf x2 scr-x y2 scr-y)))
           (glaw:bbox-translate box (* 0.5 (+ x1 x2)) (* 0.5 (+ y1 y2)))
           (push box (edition-screen-creation-boxes scr))
           (push (cons i j) (edition-screen-edges scr)))
         ;; j-k edge
         (let ((box (glaw:create-bbox-from-shape (edition-screen-selection-shape scr)))
               x1 y1 x2 y2)
           (multiple-value-bind (x y z) (glaw:shape-get-vertex (edition-screen-shape scr) j)
             (declare (ignore z))
             (multiple-value-bind (scr-x scr-y) (glaw:view-to-screen x y (edition-screen-view scr))
               (setf x1 scr-x y1 scr-y)))
           (multiple-value-bind (x y z) (glaw:shape-get-vertex (edition-screen-shape scr) k)
             (declare (ignore z))
             (multiple-value-bind (scr-x scr-y) (glaw:view-to-screen x y (edition-screen-view scr))
               (setf x2 scr-x y2 scr-y)))
           (glaw:bbox-translate box (* 0.5 (+ x1 x2)) (* 0.5 (+ y1 y2)))
           (push box (edition-screen-creation-boxes scr))
           (push (cons j k) (edition-screen-edges scr)))
         ;; k-i edge
         (let ((box (glaw:create-bbox-from-shape (edition-screen-selection-shape scr)))
               x1 y1 x2 y2)
           (multiple-value-bind (x y z) (glaw:shape-get-vertex (edition-screen-shape scr) k)
             (declare (ignore z))
             (multiple-value-bind (scr-x scr-y) (glaw:view-to-screen x y (edition-screen-view scr))
               (setf x1 scr-x y1 scr-y)))
           (multiple-value-bind (x y z) (glaw:shape-get-vertex (edition-screen-shape scr) i)
             (declare (ignore z))
             (multiple-value-bind (scr-x scr-y) (glaw:view-to-screen x y (edition-screen-view scr))
               (setf x2 scr-x y2 scr-y)))
           (glaw:bbox-translate box (* 0.5 (+ x1 x2)) (* 0.5 (+ y1 y2)))
           (push box (edition-screen-creation-boxes scr))
           (push (cons k i) (edition-screen-edges scr)))))
  (setf (edition-screen-creation-boxes scr) (reverse (edition-screen-creation-boxes scr)))
  (setf (edition-screen-edges scr) (reverse (edition-screen-edges scr))))

(defun edition-screen-render-selection-shape (scr index)
  (multiple-value-bind (x y z) (glaw:shape-get-vertex (edition-screen-shape scr) index)
    (declare (ignore z))
    (multiple-value-bind (scr-x scr-y) (glaw:view-to-screen x y (edition-screen-view scr))
      (if (= (edition-screen-selection scr) index)
          (glaw:set-color/rgb 1.0 0.0 0.0 1.0)
          (glaw:set-color/rgb 0.0 1.0 0.0 1.0))
      (gl:with-pushed-matrix
        (gl:translate scr-x scr-y 0)
        (glaw:render-shape (edition-screen-selection-shape scr)))
      (glaw:set-color/rgb 1.0 1.0 1.0)
      (when (nth index (edition-screen-selection-boxes scr))
        (glaw:render-bbox (nth index (edition-screen-selection-boxes scr))))
      (glaw:set-color/rgb 0.0 0.0 1.0 1.0)
      (glaw:format-at scr-x scr-y *font* "~D" index))))

(defun edition-screen-translate-vertex (scr index dx dy)
  (let ((shape (edition-screen-shape scr)))
    (multiple-value-bind (x y z) (glaw:shape-get-vertex shape index)
      (glaw:shape-set-vertex shape index (+ x dx) (+ y dy) z)))
  (edition-screen-update-boxes scr))

(defun edition-screen-add-vertex (scr x y prev-index next-index)
  (glaw:shape-add-vertex (edition-screen-shape scr) x y)
  (glaw:shape-add-indices (edition-screen-shape scr)
                          prev-index
                          (1- (glaw:shape-nb-vertices (edition-screen-shape scr)))
                          next-index)
  (edition-screen-update-boxes scr))

;; (defun edition-screen-remove-vertex (scr)
;;   (glaw:shape-add-vertex (edition-screen-shape scr) x y)
;;   (glaw:shape-add-indices (edition-screen-shape scr)
;;                           prev-index
;;                           (1- (glaw:shape-nb-vertices (edition-screen-shape scr)))
;;                           next-index)
;;   (setf (edition-screen-selection scr) 0)
;;   (edition-screen-update-boxes scr))

(glaw:button-handler (it edition-screen) :mouse (:wheel-up :press)
   (glaw:zoom-2d-view (edition-screen-view it) -0.01)
   (edition-screen-update-boxes it))

(glaw:button-handler (it edition-screen) :mouse (:wheel-down :press)
   (glaw:zoom-2d-view (edition-screen-view it) 0.01)
   (edition-screen-update-boxes it))

(glaw:key-handler (it edition-screen) (:w :press)
  (with-open-file (out #P"out.shape" :direction :output)
    (format out "~S" (edition-screen-shape it))))

(glaw:key-handler (it edition-screen) (:n :press)
  (incf (edition-screen-selection it))
  (when (>= (edition-screen-selection it) (glaw:shape-nb-vertices (edition-screen-shape it)))
    (setf (edition-screen-selection it) 0)))

(glaw:button-handler (it edition-screen) :mouse (:left-button :press)
  (let ((found (loop for i below (glaw:shape-nb-vertices (edition-screen-shape it))
                  for box = (nth i (edition-screen-selection-boxes it))
                  when (and box (glaw:bbox-inside-p box glaw:*mouse-x*
                                                    (- glaw:*display-height* glaw:*mouse-y*))) do
                    (setf (edition-screen-selection it) i)
                    (return box))))
    (if found
        (setf (edition-screen-moving it) t)
        (loop for box in (edition-screen-creation-boxes it)
           for edge in (edition-screen-edges it)
           when (and box (glaw:bbox-inside-p box glaw:*mouse-x*
                                             (- glaw:*display-height* glaw:*mouse-y*))) do
             (glaw:with-2d-coords-from-screen ((x glaw:*mouse-x*) (y glaw:*mouse-y*))
               (edition-screen-view it)
               (edition-screen-add-vertex it x y (car edge) (cdr edge)))
             (return)))))

;; (glaw:button-handler (it edition-screen) :mouse (:right-button :press)
;;   (let ((found (loop for i below (glaw:shape-nb-vertices (edition-screen-shape it))
;;                   for box = (nth i (edition-screen-selection-boxes it))
;;                   when (and box (glaw:bbox-inside-p box glaw:*mouse-x*
;;                                                     (- glaw:*display-height* glaw:*mouse-y*))) do
;;                     (setf (edition-screen-selection it) i)
;;                     (return box))))
;;     (when found
;;       (edition-screen-remove-vertex it))))

(glaw:button-handler (it edition-screen) :mouse (:left-button :release)
  (setf (edition-screen-moving it) nil))

(glaw:motion-handler (it edition-screen) :mouse (screen-dx screen-dy)
  (multiple-value-bind (dx dy) (glaw:view-to-view screen-dx (- screen-dy)
                                                  *view* (edition-screen-view it) nil)
    (when (edition-screen-moving it)
      (edition-screen-translate-vertex it (edition-screen-selection it) dx dy))))

(defmethod glaw:init-screen ((it edition-screen) &key)
  (setf (edition-screen-shape it) (glaw:use-resource "shape"))
  (edition-screen-update-boxes it)
  (glaw::shape-ensure-adjustable (edition-screen-shape it))
  (glaw:add-input-handler it))

(defmethod glaw:shutdown-screen ((it edition-screen))
  (glaw:drop-resource "shape")
  (glaw:remove-input-handler it))

(defmethod glaw:update-screen ((it edition-screen) dt)
  (declare (ignore dt)))

(defmethod glaw:render-screen ((it edition-screen))
  (glaw:set-view-2d (edition-screen-view it))
  (glaw:set-color/rgb 1.0 0.0 0.0 1.0)
  (gl:begin :lines)
  (gl:vertex 0 0 0)
  (gl:vertex 100 0 0)
  (gl:end)
  (glaw:set-color/rgb 0.0 1.0 0.0 1.0)
  (gl:begin :lines)
  (gl:vertex 0 0 0)
  (gl:vertex 0 100 0)
  (gl:end)
  (glaw:set-color/rgb 0.0 0.0 0.0 1.0)
  (glaw:render-shape (edition-screen-shape it) :triangles)
  (gl:polygon-mode :front-and-back :line)
  (glaw:set-color/rgb 1.0 1.0 1.0 1.0)
  (glaw:render-shape (edition-screen-shape it) :triangles)
  (gl:polygon-mode :front-and-back :fill)
  (glaw:set-view-2d *view*)
  (loop for i below (glaw:shape-nb-vertices (edition-screen-shape it)) do
       (edition-screen-render-selection-shape it i))
  (glaw:set-color/rgb 1.0 1.0 1.0 1.0)
  (loop for i in (edition-screen-creation-boxes it) do
       (glaw:render-bbox i))
  (glaw:set-color/rgb 1.0 1.0 1.0 1.0)
  (multiple-value-bind (x y z) (glaw:shape-get-vertex (edition-screen-shape it)
                                                      (edition-screen-selection it))
    (glaw:format-at 100 160 *font* "Selection: ~A;~A;~A" x y z)))

;;; Main code
(defun init (shape-file data-dir)
  (glaw:init-content-manager data-dir)
  (glaw:load-asset "dejavu-sans.fnt" :fonttool-bitmap-font "font")
  (glaw:load-asset shape-file :shape "shape")
  (setf *font* (glaw:use-resource "font")
        *view* (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*)
        *screens* (glaw:make-screen-stack))
  (glaw:push-screen (make-edition-screen) *screens*))

(defun shutdown ()
  (glaw:pop-screen *screens*)
  (glaw:dispose-asset "font")
  (glaw:dispose-asset "shape")
  (glaw:shutdown-content-manager))

(defun draw (window)
  (glaw:begin-draw)
  (glaw:render-screens *screens*)
  (glaw:set-view-2d *view*)
  (glaw:set-color/rgb 1.0 1.0 1.0)
  (glaw:format-at 100 100 *font* "FPS: ~A" (glaw:current-fps))
  (glaw:end-draw)
  (glop:swap-buffers window))

(defun update (dt)
  (glaw:update-screens *screens* dt))

(defmethod glop:on-key (window pressed keycode keysym string)
  (glaw:dispatch-key-event keysym (if pressed :press :release) keycode string)
  (when (eql keysym :escape)
    (glop:push-close-event window)))

(defmethod glop:on-close (window)
  (declare (ignore window))
  (shutdown))

(defmethod glop:on-button (window pressed button)
  (declare (ignore window))
  (glaw:dispatch-button-event :mouse (glaw:translate-mouse-button button)
                              (if pressed :press :release)))

(defmethod glop:on-mouse-motion (window x y dx dy)
  (declare (ignore window))
  (glaw:update-mouse-position x y)
  (glaw:dispatch-motion-event :mouse dx dy))

(defmethod glop:on-draw (window)
  (draw window))

(defmethod glop:on-resize (window w h)
  (glaw:update-2d-view *view* 0 0 w h)
  (glaw:reshape w h)
  (draw window))

(defun run (&optional (shape-file "default.shape") (data-dir #P"./"))
  (setf cl-opengl-bindings:*gl-get-proc-address* 'glop:gl-get-proc-address)
  (glop:with-window (win "Glaw Shape Editor" 800 600)
    (glaw:setup-gl-defaults)
    (glaw:reshape 800 600)
    (init shape-file data-dir)
    (let ((last-update-time (get-internal-real-time)))
      (loop while (glop:dispatch-events win :blocking nil) do
           (let* ((elapsed-time (- (get-internal-real-time)
                                   last-update-time))
                  (dt (/ (* elapsed-time 1.0)
                         internal-time-units-per-second)))
             (glaw:with-timestep (dt 0.001)
               (update dt)
               (draw win))
             (setf last-update-time (get-internal-real-time)))))))



