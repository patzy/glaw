(in-package #:glaw-examples)

(defstruct particles
  view
  systems
  emitters)

(defmethod init-example ((it particles))
  (glaw:load-asset "particle.png" :texture)
  (glaw:load-asset "fire-particle.png" :texture)
  (setf (particles-view it) (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*))
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
                                             :spin (list 0 (glaw:deg->rad 60))
                                             :angle (list 0 (glaw:deg->rad 60))
                                             :rate '(1.0 10.0)
                                             :vx 0.0
                                             :vy '(100 200.0)
                                             :lifetime 2.0
                                             :color (glaw:create-color 1.0 0.4 0.15 1.0)
                                             :width 30.0
                                             :height 30.0
                                             :x (let ((x (random glaw:*display-width*)))
                                                  (list (- x 20) (+ x 20)))
                                             :y (random glaw:*display-height*)
                                             :texture (glaw:use-resource "fire-particle.png")))
                (glaw::add-particle-affector syst
                                             (glaw::make-resistance-affector
                                              :value 0.5))
                (glaw::add-particle-affector syst
                                             (glaw::make-fading-affector
                                              :rate 2.0))))))

(defmethod shutdown-example ((it particles))
  (glaw:dispose-asset "particle.png")
  (glaw:dispose-asset "fire-particle.png"))

(defmethod render-example ((it particles))
  (glaw:begin-draw)
  (glaw:set-view-2d (particles-view it))
  (let ((nb-particles 0))
    (dolist (syst (particles-systems it))
      (glaw:render-particles syst)
      (incf nb-particles (glaw:particle-system-nb-particles syst)))
    (glaw:set-color/rgb 1 1 1)
    (glaw:with-resources ((fnt "default-font"))
      (glaw:format-at 50 80 fnt "Particles: ~a" nb-particles)
      (glaw:format-at 50 100 fnt "FPS: ~a" (glaw:current-fps))))
  (glaw:end-draw))

(defmethod update-example ((it particles) dt)
  (dolist (syst (particles-systems it))
    (glaw:update-particles syst dt)))

(defmethod reshape-example ((it particles) w h)
  (glaw:update-2d-view (particles-view it) 0 0 w h))

