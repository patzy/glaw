(in-package #:glaw-examples)

(defstruct particles
  (font nil)
  (view (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*))
  (systems '())
  (emitters '()))

(defmethod init-example ((it particles))
  (glaw:load-asset "font.png" :bitmap-font)
  (glaw:load-asset "particle.png" :texture)
  (glaw:load-asset "fire-particle.png" :texture)
  (setf (particles-font it) (glaw:use-resource "font.png"))
  (loop for i below 10
       do (if (oddp i)
              ;; water thing
              (let ((syst (glaw:create-particle-system)))
                (push syst (particles-systems it))
                (glaw::add-particle-emitter syst
                                            (glaw::make-particle-emitter
                                             :delay 0.00
                                             :rate 1.0
                                             :vx '(90.0 100.0)
                                             :vy '(19 20)
                                             :lifetime 8.0
                                             :color (glaw:create-color 0.3 0.3 1.0 1.0)
                                             :width 5.0
                                             :height 5.0
                                             :x (random glaw:*display-width*)
                                             :y (random glaw:*display-height*)
                                             :texture (glaw:use-resource "particle.png")))
                (glaw::add-particle-affector syst
                                             (glaw::make-gravity-affector
                                              :strength 50))
                (glaw::add-particle-affector syst
                                             (glaw::make-fading-affector
                                              :rate 0.3)))
              ;; fire thing
              (let ((syst (glaw:create-particle-system)))
                (push syst (particles-systems it))
                (glaw::add-particle-emitter syst
                                            (glaw::make-particle-emitter
                                             :delay 0.05
                                             :rate '(1.0 10.0)
                                             :vx 0.0
                                             :vy '(-20.0 40.0)
                                             :lifetime 2.0
                                             :color (glaw:create-color 1.0 0.4 0.15 1.0)
                                             :width 50.0
                                             :height 50.0
                                             :x (random glaw:*display-width*)
                                             :y (random glaw:*display-height*)
                                             :texture (glaw:use-resource "fire-particle.png")))
                (glaw::add-particle-affector syst
                                             (glaw::make-resistance-affector
                                              :value 0.5))
                (glaw::add-particle-affector syst
                                             (glaw::make-fading-affector
                                              :rate 2.0))))))

(defmethod shutdown-example ((it particles))
  (glaw:dispose-asset "font.png")
  (glaw:dispose-asset "particle.png")
  (glaw:dispose-asset "fire-particle.png"))

(defmethod render-example ((it particles))
  (glaw:begin-draw)
  (glaw:set-view-2d (particles-view it))
  (dolist (syst (particles-systems it))
    (glaw:render-particles syst))
  (glaw:set-color/rgb 1 1 1)
  (glaw:format-at 50 80  (particles-font it) "Particles: ~a" glaw::*nb-particles*)
  (glaw:format-at 50 100  (particles-font it) "FPS: ~a/~a/~a"
                  (glaw:min-fps) (glaw:current-fps) (glaw:max-fps))
  (glaw:end-draw))

(defmethod update-example ((it particles) dt)
  (dolist (syst (particles-systems it))
    (glaw:update-particles syst dt)))

(defmethod reshape-example ((it particles) w h)
  (glaw:update-2d-view (particles-view it) 0 0 w h))

