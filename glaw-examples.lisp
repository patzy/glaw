(defpackage :glaw-examples
  (:use #:cl)
  (:export #:run-example))

(in-package #:glaw-examples)

(defvar *current-example* nil)

(defgeneric init-example (example))
(defgeneric shutdown-example (example))
(defgeneric render-example (example))
(defgeneric update-example (example dt))
(defgeneric reshape-example (example w h))

(glaw:key-handler :global (#\Esc :press)
  (shutdown-example *current-example*)
  (sdl:push-quit-event))

(defun start-example (expl)
  (let ((old-example *current-example*))
    (when old-example
      (shutdown-example old-example))
    (setf *current-example* (make-instance expl))
    (init-example *current-example*)))

(defun draw ()
  (glaw:begin-draw)
  (when *current-example*
    (render-example *current-example*))
  (glaw:end-draw))

(let ((last-update-time (get-internal-real-time)))
  (defun idle ()
    (let* ((elapsed-time (- (get-internal-real-time)
                            last-update-time))
           (dt (/ (* elapsed-time 1.0)
                  internal-time-units-per-second)))
      (when *current-example*
        (update-example *current-example* dt))
      (setf last-update-time (get-internal-real-time)))))

(defun run-example (example-name)
  (sdl:with-init ()
    ;; how to get extensions
    (setf cl-opengl-bindings:*gl-get-proc-address* #'sdl-cffi::sdl-gl-get-proc-address)
    (sdl:window 1024 768
                :bpp 32
                :flags '(sdl:sdl-opengl sdl:sdl-resizable
                         sdl:sdl-opengl sdl:sdl-hw-surface
                         sdl:sdl-doublebuf)
                :title-caption "GLAW - Examples"
                :icon-caption "GLAW - Examples")
    (setf (sdl:frame-rate) 120)
    (sdl:enable-unicode)
    (sdl:enable-key-repeat nil nil)
    (glaw:setup-gl-defaults)
    (glaw:reshape 800 600)
    (start-example example-name)
    (sdl:with-events (:poll)
      (:quit-event () t)
      (:key-down-event (:key key :unicode code)
          (glaw:dispatch-key-event (glaw-sdl:translate-key key code)
                                   :press))
      (:key-up-event (:key key :unicode code)
          (glaw:dispatch-key-event (glaw-sdl:translate-key key code)
                                   :release))
      (:mouse-button-down-event (:button button)
          (glaw:dispatch-button-event :mouse
                                      (glaw-sdl:translate-mouse-button button)
                                      :press))
      (:mouse-button-up-event (:button button)
          (glaw:dispatch-button-event :mouse
                                      (glaw-sdl:translate-mouse-button button)
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
          (idle)
          (draw)
          (sdl:update-display)))))

