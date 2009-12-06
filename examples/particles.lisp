(in-package #:glaw-examples)

(defstruct particles
  (font nil)
  (view (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*))
  (systems '())
  (emitters '()))

(defmethod init-example ((it particles))
  (glaw:init-content-manager "data/")
  (glaw:load-asset "font.png" :texture)
  (glaw:load-asset "particle.png" :texture)
  (setf (particles-font it)
        (glaw:create-bitmap-font (glaw:use-resource "font.png") 13 16))
  (push (glaw:create-particle-system) (particles-systems it))
  (glaw::add-particle-emitter (first (particles-systems it))
                              (glaw::make-particle-emitter
                               :delay 0.2
                               :rate 10.0
                               :vx '(-10.0 10.0)
                               :vy '(0.0 100.0)
                               :lifetime 100.0
                               :color (glaw:create-color 0.3 0.6 1.0 1.0)
                               :width 10.0
                               :height 10.0
                               :x 500
                               :y 400
                               :texture (glaw:use-resource "particle.png"))
                              )
  (glaw::add-particle-affector (first (particles-systems it))
                              (glaw::make-gravity-affector)
                              )
  (glaw::add-particle-affector (first (particles-systems it))
                              (glaw::make-fading-affector)
                              )
  (format t "done~%"))

(defmethod shutdown-example ((it particles))
  (glaw:destroy-font (particles-font it))
  (glaw:shutdown-content-manager))

(defmethod render-example ((it particles))
  (glaw:begin-draw)
  (glaw:set-view-2d (particles-view it))
  (dolist (syst (particles-systems it))
    (glaw:render-particles syst))
  (glaw:set-color/rgb 1 1 1)
  (glaw:format-at 50 80  (particles-font it) "Particles: ~a" glaw::*nb-particles*)
  (glaw:format-at 50 100  (particles-font it) "FPS: ~a" (glaw:current-fps))
  (glaw:end-draw))

(defmethod update-example ((it particles) dt)
  (dolist (syst (particles-systems it))
    (glaw:update-particles syst dt)))

(defmethod reshape-example ((it particles) w h)
  (glaw:update-2d-view (particles-view it) 0 0 w h))

