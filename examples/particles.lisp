(in-package #:glaw-examples)

(defstruct particles
  view
  systems
  emitters)

(defmethod init-example ((it particles))
  (glaw:load-asset "particle.png" :texture)
  (glaw:load-asset "fire-particle.png" :texture)
  (glaw:load-asset "smoke-particle.png" :texture)
  (glaw:load-asset "bubble.png" :texture)
  (setf (particles-view it) (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*))
  (loop for i below 10
       do (let ((syst (glaw:create-particle-system)))
            (push syst (particles-systems it))
            (case (mod i 4)
              (0 ;; water thing
               (glaw::add-particle-emitter syst
                                           (glaw::make-particle-emitter
                                            :rate 100.0
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
              (1 ;; blobby thing
               (let ((x (random glaw:*display-width*))
                     (y (random glaw:*display-height*)))
                 (glaw::add-particle-emitter syst
                                             (glaw::make-particle-emitter
                                              :rate 50.0
                                              :vx '(-5.0 5.0)
                                              :vy '(-5.0 5.0)
                                              :lifetime 1.0
                                              :color (glaw:create-color 0.3 0.3 1.0 0.3)
                                              :width '(50.0 100.0)
                                              :height '(50.0 100.0)
                                              :x (list (- x 10.0) (+ x 10.0))
                                              :y (list (- y 10.0) (+ y 10.0))
                                              :texture (glaw:use-resource "particle.png"))))
               (glaw::add-particle-affector syst
                                            (glaw::make-fading-affector
                                             :rate 0.1)))
              (2 ;; bubble thing
               (glaw::add-particle-emitter syst
                                           (glaw::make-particle-emitter
                                            :rate 3.0
                                            :vx '(-5.0 5.0)
                                            :vy '(19 20)
                                            :lifetime 8.0
                                            :color (glaw:create-color 1.0 1.0 1.0 0.7)
                                            :width 5.0
                                            :height 5.0
                                            :x (list 0 glaw:*display-width*)
                                            :y (list 0 10)
                                            :texture (glaw:use-resource "bubble.png"))))
              (3 ;; fire thing
               (let ((x (random glaw:*display-width*))
                     (y (random glaw:*display-height*)))
                 (glaw::add-particle-emitter syst
                                             (glaw::make-particle-emitter
                                              :rate 65.0
                                              :spin (list 0 (glaw:deg->rad 60))
                                              :angle (list 0 (glaw:deg->rad 60))
                                              :rate '(1.0 10.0)
                                              :vx 0.0
                                              :vy '(100 200.0)
                                              :lifetime 2.0
                                              :color (glaw:create-color 1.0 0.4 0.15 1.0)
                                              :width 30.0
                                              :height 30.0
                                              :x (list (- x 20.0) (+ x 20.0))
                                              :y y
                                              :texture (glaw:use-resource "fire-particle.png")))
               (glaw::add-particle-emitter syst
                                           (glaw::make-particle-emitter
                                            :rate 20.0
                                            :spin (list 0 (glaw:deg->rad 60))
                                            :angle (list 0 (glaw:deg->rad 60))
                                            :rate '(1.0 10.0)
                                            :vx 0.0
                                            :vy '(100 200.0)
                                            :lifetime 2.0
                                            :color (glaw:create-color .3 .3 .3 1.0)
                                            :width 30.0
                                            :height 30.0
                                            :x (list (- x 20.0) (+ x 20.0))
                                            :y (+ y 50.0)
                                            :texture (glaw:use-resource "smoke-particle.png"))))
               (glaw::add-particle-affector syst
                                            (glaw::make-resistance-affector
                                             :value 0.5))
               (glaw::add-particle-affector syst
                                            (glaw::make-fading-affector
                                             :rate 2.0)))))))

(defmethod shutdown-example ((it particles))
  (glaw:dispose-asset "particle.png")
  (glaw:dispose-asset "fire-particle.png"))

(defmethod render-example ((it particles))
  (glaw:set-view-2d (particles-view it))
  (let ((nb-particles 0))
    (dolist (syst (particles-systems it))
      (glaw:render-particles syst)
      (incf nb-particles (glaw:particle-system-nb-particles syst)))
    (glaw:set-color/rgb 1.0 1.0 1.0)
    (glaw:with-resources ((fnt "default-font"))
      (glaw:format-at 50 120 fnt "Particles: ~a" nb-particles))))

(defmethod update-example ((it particles) dt)
  (dolist (syst (particles-systems it))
    (glaw:update-particles syst dt)))

(defmethod reshape-example ((it particles) w h)
  (glaw:update-2d-view (particles-view it) 0 0 w h))

