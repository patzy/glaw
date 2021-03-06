(defpackage :glaw-examples
  (:use #:cl)
  (:export #:run-example
           #:empty
           #:pathfinding #:particles #:sprites #:screens #:text #:texture #:tilemap
           #:sound #:skeletons #:input #:views
           #:shaders #:framebuffer #:console
           ;; TODO: those may be removed
            #:mesh-viewer  #:gui))

(in-package #:glaw-examples)

(defvar *current-example* nil)
(defvar *max-frame-time* 0.04) ;; lock at ~25 FPS
(defvar *render-timer* (glaw:make-frame-timer))
(defvar *render-stats-grapher* (glaw:make-frame-timer-grapher))
(defvar *show-render-stats* t)
(defvar *render-stats-view* (glaw:create-2d-view 0 0 800 600))

(defgeneric init-example (example))
(defgeneric shutdown-example (example))
(defgeneric render-example (example))
(defgeneric update-example (example dt))
(defgeneric reshape-example (example w h))

(defun start-example (expl)
  (let ((old-example *current-example*))
    (when old-example
      (shutdown-example old-example))
    (setf *current-example* (make-instance expl))
    (init-example *current-example*)))


(defun draw ()
  (let ((render-start-time (get-internal-real-time)))
    (glaw:begin-draw)
    (when *current-example*
      (render-example *current-example*))
    (let ((dt (/ (- (get-internal-real-time) render-start-time)
                 internal-time-units-per-second)))
      (glaw:frame-timer-update *render-timer* dt))
    (glaw:frame-timer-grapher-update *render-stats-grapher* *render-timer*)
    (when *show-render-stats*
      (glaw:set-view-2d *render-stats-view*)
      (glaw:select-texture nil)
      (glaw:setup-2d-defaults)
      (glaw:with-resources ((fnt "default-font"))
        (glaw:with-frame-timer *render-timer*
          (glaw::render-stats-grapher *render-stats-grapher* :x 100 :y 400)
          (glaw:set-color/rgb 1.0 1.0 1.0 1.0)
          (glaw:format-at 50 20 fnt "Instant framerate: ~a" (glaw:instant-fps))
          (glaw:format-at 50 40 fnt "Instant frametime: ~a" (glaw:instant-frame-time))
          (glaw:format-at 50 60 fnt "Instant framerate min.: ~a" (glaw:instant-min-fps))
          (glaw:format-at 50 80 fnt "Instant framerate max.: ~a" (glaw:instant-max-fps))
          (glaw:format-at 50 100 fnt "Average framerate: ~a" (glaw:avg-fps))
          (glaw:format-at 50 120 fnt "Average frametime: ~a" (glaw:avg-frame-time))
          (glaw:format-at 50 140 fnt "Average framerate min.: ~a" (glaw:avg-min-fps))
          (glaw:format-at 50 160 fnt "Average framerate max.: ~a" (glaw:avg-max-fps))
          (glaw:format-at 50 180 fnt "All-time framerate: ~a" (glaw:total-fps))
          (glaw:format-at 50 200 fnt "All-time frametime: ~a" (glaw:total-frame-time))
          (glaw:format-at 50 220 fnt "Total time: ~a" (glaw:total-timer-time))
          (glaw:format-at 50 240 fnt "Total # of frames: ~a" (glaw:total-timer-nb-frames)))))
    (glaw:end-draw)))

(defun update (dt)
  (when *current-example*
    (update-example *current-example* dt)))

;; Using GLOP
#+glaw-examples-glop
(let ((alt-pressed nil))
  (defmethod glop:on-key (window pressed keycode keysym string)
    (glaw:dispatch-key-event keysym (if pressed :press :release) keycode string)
    (when (or (eql keysym :alt-l)
              (eql keysym :alt-r))
      (setf alt-pressed pressed))
    (when (and pressed (eql keysym :s) alt-pressed)
      (setf *show-render-stats* (not *show-render-stats*)))
    (when (or (eql keysym :escape) (eql keysym :q))
      (glop:push-close-event window))))

#+glaw-examples-glop
(defmethod glop:on-close (window)
  (declare (ignore window))
  (shutdown-example *current-example*)
  (setf *current-example* nil))

#+glaw-examples-glop
(defmethod glop:on-button (window pressed button)
  (declare (ignore window))
  (glaw:dispatch-button-event :mouse (glaw:translate-mouse-button button)
                              (if pressed :press :release)))

#+glaw-examples-glop
(defmethod glop:on-mouse-motion (window x y dx dy)
  (declare (ignore window))
  (glaw:update-mouse-position x y)
  (glaw:dispatch-motion-event :mouse dx dy))

#+glaw-examples-glop
(defmethod glop:on-draw (window)
  (declare (ignore window)))

#+glaw-examples-glop
(defmethod glop:on-resize (window w h)
  (glaw:reshape w h)
  (reshape-example *current-example* w h))


#+glaw-examples-glop
(defun run-example (example-name)
  ;; how to get extensions
  (setf cl-opengl-bindings:*gl-get-proc-address* 'glop:gl-get-proc-address)
  (glaw:init-content-manager :root (asdf:system-relative-pathname :glaw "data/")
                             :config (asdf:system-relative-pathname :glaw "data/examples.assets"))
  (glop:with-window (win "Glaw examples" 800 600)
    (glaw::setup-2d-defaults)
    (glaw:reshape 800 600)
    (glaw:load-asset "default-font")
    (start-example example-name)
    (let ((last-update-time (get-internal-real-time)))
      (loop while (glop:dispatch-events win :blocking nil) do
           (let* ((elapsed-time (- (get-internal-real-time)
                                   last-update-time))
                  (dt (/ (* elapsed-time 1.0)
                         internal-time-units-per-second)))
             (setf last-update-time (get-internal-real-time))
             (draw)
             (glop:swap-buffers win)
             (glaw:with-timestep (dt *max-frame-time*)
               (update dt))))))
  (glaw:dispose-asset "default-font")
  (glaw:shutdown-content-manager))

;; Using SDL
#+glaw-examples-sdl
(glaw:key-handler :global (:escape :press)
  (shutdown-example *current-example*)
  (sdl:push-quit-event))

#+glaw-examples-sdl
(defun run-example (example-name)
  (sdl:with-init ()
    ;; how to get extensions
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (glaw:init-content-manager :root (asdf:system-relative-pathname :glaw "data/")
                               :config (asdf:system-relative-pathname :glaw "data/examples.assets"))
    (sdl:window 1024 768
                :bpp 32
                :flags '(sdl:sdl-opengl sdl:sdl-resizable
                         sdl:sdl-opengl sdl:sdl-hw-surface
                         sdl:sdl-doublebuf)
                :title-caption "Glaw Examples"
                :icon-caption "Glaw Examples")
    (setf (sdl:frame-rate) (/ 1.0 *max-frame-time*))
    (sdl:enable-unicode)
    (sdl:enable-key-repeat nil nil)
    (glaw:setup-2d-defaults)
    (glaw:reshape 800 600)
    (glaw:load-asset "default-font")
    (start-example example-name)
    (let ((last-update-time (get-internal-real-time)))
      (let* ((elapsed-time (- (get-internal-real-time)
                              last-update-time))
             (dt (/ (* elapsed-time 1.0)
                    internal-time-units-per-second)))
      (setf last-update-time (get-internal-real-time))
      (sdl:with-events (:poll)
        (:quit-event () t)
        (:key-down-event (:key key :unicode code :scancode scancode)
                         (glaw:dispatch-key-event (glaw-sdl:translate-keysym key)
                                                  :press scancode (code-char code)))
        (:key-up-event (:key key :unicode code :scancode scancode)
                       (glaw:dispatch-key-event (glaw-sdl:translate-keysym key)
                                                :release scancode (code-char code)))
        (:mouse-button-down-event (:button button)
                                  (glaw:dispatch-button-event :mouse
                                                              (glaw:translate-mouse-button button)
                                                              :press))
        (:mouse-button-up-event (:button button)
                                (glaw:dispatch-button-event :mouse
                                                            (glaw:translate-mouse-button button)
                                                            :release))
        (:mouse-motion-event (:x x :y y :x-rel x-rel :y-rel y-rel)
                             (glaw:update-mouse-position x y)
                             (glaw:dispatch-motion-event :mouse x-rel y-rel))
        (:video-expose-event ()
                             (draw)
                             (sdl:update-display))
        (:video-resize-event (:w w :h h)
                             (sdl:resize-window w h)
                             (glaw:reshape w h)
                             (reshape-example *current-example* w h)
                             (draw)
                             (sdl:update-display))
        (:idle ()
               (update dt)
               (draw)
               (sdl:update-display)))))
  (glaw:dispose-asset "default-font")
  (glaw:shutdown-content-manager)))
