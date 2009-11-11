(in-package #:glaw-examples)

(glaw:key-handler :global (#\Esc :press)
   (shutdown)
   (sdl:push-quit-event))

(defun init ()
  (setf *font* (glaw:create-font (glaw:create-resource "font.png" :image)
                                 13 16))
  (glaw:init-gui *font*)
  (let* ((window (glaw:create-widget 'glaw:gui-window nil
                                     :x 50 :y 50
                                     :width 300 :height 500
                                     :moveable nil
                                     :texture
                                     (glaw:create-texture-from-file
                                      "examples/starfield.jpg")
                                     :title "Static window"))
         (window2 (glaw:create-widget 'glaw:gui-window nil
                                     :x 100 :y 200
                                     :width 300 :height 500
                                     :moveable t
                                     :title "Moveable window"))
         (label (glaw:create-widget 'glaw:gui-label window
                                    :x 50 :y 100
                                    :width 50 :height 10
                                    :text "Some text label !")))
    (glaw:create-widget 'glaw:gui-button window
                                    :x 50 :y 50
                                    :width 50 :height 10
                                    :action (let ((nb-clicks 0))
                                              (lambda (self)
                                                (incf nb-clicks)
                                                (setf (glaw:text label)
                                                      (format nil
                                                              "Clics: ~d"
                                                              nb-clicks))))
                                    :text "Click me !")
    (glaw:create-widget 'glaw:gui-label window2
                                    :x 0 :y 50
                                    :width 50 :height 10
                                    :text "Input:")
    (glaw:create-widget 'glaw:gui-slider window2
                                    :x 10 :y 150
                                    :width 100 :height 10)
    (glaw:create-widget 'glaw:gui-text-input window2
                        :x 80 :y 50
                        :width 50 :height 10)))


(defun shutdown ()
  (glaw:shutdown-gui)
  (glaw:destroy-font *font*)
  (glaw:free-all-resources))

(defun draw ()
  (glaw:begin-draw)
  (glaw:render-gui)
  (glaw:end-draw))

(let ((last-update-time (get-internal-real-time)))
  (defun idle ()
    (let* ((elapsed-time (- (get-internal-real-time)
                            last-update-time))
           (dt (/ (* elapsed-time 1.0)
                  internal-time-units-per-second)))
      ;;(glaw:update-gui dt)
      (setf last-update-time (get-internal-real-time)))))

(defun run-gui ()
  (sdl:with-init (sdl:sdl-init-video)
    (sdl:window 800 600
                :bpp 32
                :flags '(sdl:sdl-opengl sdl:sdl-resizable
                         sdl:sdl-opengl sdl:sdl-hw-surface
                         sdl:sdl-doublebuf)
                :title-caption "Plantae"
                :icon-caption "Plantae")
    (setf (sdl:frame-rate) 120)
    (sdl:enable-unicode t)
    (sdl:enable-key-repeat nil nil)
    (glaw:setup-gl-defaults)
    (init)
    (glaw:reshape 800 600)
    (sdl:with-events (:poll)
      (:quit-event () (prog1 t
                        (shutdown)
                        ;; free remaining resources
                        (glaw:free-all-resources)))
      (:key-down-event (:key key :unicode code)
          (glaw:dispatch-key-event (glaw-sdl:translate-key key code)
                                   :press))
      (:key-up-event (:key key :unicode code)
          (glaw:dispatch-key-event (glaw-sdl:translate-key key code)
                                   :release))
      (:mouse-button-down-event (:button button :state state :x x :y y)
          (glaw:dispatch-button-event :mouse
                                      (glaw-sdl:translate-mouse-button button)
                                      :press))
      (:mouse-button-up-event (:button button :state state :x x :y y)
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
          (glaw:update-gui)
          (draw)
          (sdl:update-display))
      (:idle ()
          (idle)
          (draw)
          (sdl:update-display)))))

