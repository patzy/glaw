(in-package #:glaw)

(defstruct particle
  sprite
  (color (create-color 1.0 1.0 1.0))
  blend-mode
  (active-p nil)
  (vx 0.0) (vy 0.0) (vz 0.0)
  (spin 0.0)
  (age 0.0)
  (lifetime 10.0))

(defun particle-dead-p (particle)
  (or (> (particle-age particle) (particle-lifetime particle))
      (zerop (color-a (particle-color particle)))))

;; A particle system is a group of particles with same properties
;; This means all particles of a system are subjects to the same set of affectors
(defstruct particle-system
  max-particles
  (particles '())
  (affectors '())
  (emitters '()))

(defun add-particle-emitter (syst emitter)
  (push emitter (particle-system-emitters syst)))

(defun add-particle-affector (syst affector)
  (push affector (particle-system-affectors syst)))

(defun create-particle-system (&optional (max-particles 10000))
  (let ((syst (make-particle-system :max-particles max-particles)))
    syst))

(defun particle-system-nb-particles (syst)
  (length (particle-system-particles syst)))

(defun add-particle (p syst)
  (push p (particle-system-particles syst)))

(defun remove-particle (p syst)
  (setf (particle-system-particles syst)
        (remove p (particle-system-particles syst))))

(defmacro map-particles ((p-sym p-system) &body body)
  `(dolist (,p-sym (particle-system-particles ,p-system))
     ,@body))

(defun render-particles (s)
  (dolist (part (particle-system-particles s))
    (gl:blend-func (first (particle-blend-mode part))
                   (second (particle-blend-mode part)))
    (set-color (particle-color part))
    (render-sprite (particle-sprite part))))

(defun update-particles (s dt)
  ;; remove dead particles
  (map-particles (p s)
    (incf (particle-age p) dt)
    (when (particle-dead-p p)
      (remove-particle p s)))
  ;; emit new particles
  (dolist (e (particle-system-emitters s))
    (emit-particles e s dt))
  ;; affect particles
  (dolist (a (particle-system-affectors s))
    (affect-particles a s dt))
  ;; update particles position & orientation
  (map-particles (p s)
     (translate-sprite (particle-sprite p) (* (particle-vx p) dt) (* (particle-vy p) dt))
     (rotate-sprite (particle-sprite p) (* (particle-spin p) dt))))

;; This is what creates particles
(defstruct particle-emitter
  ;; emission management
  (acc 0.0)
  (rate 1.0)  ;; number of particles to emit per second
  ;; this may be put elsewhere
  (texture nil)
  (blend-mode '(:src-alpha :one))
  ;; emission parameters
  ;; each one may be a single value or a range (list)
  (lifetime  10)
  (color (create-color 0.75 0.6 0.3 1.0))
  (width 30)
  (height 30)
  (spin  0.0)
  (angle 0.0)
  (vx '(-10 10))
  (vy '(-10 10))
  (x 0)
  (y 0))

(defun emit-particles (em s dt)
  (flet ((range-or-value (param)
           (if (consp param)
               (random-between (first param) (second param))
               param)))
    (incf (particle-emitter-acc em) (* dt (particle-emitter-rate em)))
    (loop while (>= (particle-emitter-acc em) 1.0)
       do (let ((part (make-particle)))
              ;; finish particle creation
              (setf (particle-sprite part)
                    (create-sprite (float (range-or-value (particle-emitter-x em)))
                                   (float (range-or-value (particle-emitter-y em)))
                                   (float (range-or-value (particle-emitter-width em)))
                                   (float (range-or-value (particle-emitter-height em)))
                                   (particle-emitter-texture em)
                                   :bbox nil)
                    (particle-blend-mode part) (particle-emitter-blend-mode em))
              (rotate-sprite (particle-sprite part) (range-or-value (particle-emitter-angle em)))
              (color-copy (particle-emitter-color em) (particle-color part))
              ;; particle parameters init
              (setf (particle-lifetime part) (range-or-value (particle-emitter-lifetime em))
                    (particle-vx part) (range-or-value (particle-emitter-vx em))
                    (particle-vy part) (range-or-value (particle-emitter-vy em))
                    (particle-spin part)  (range-or-value (particle-emitter-spin em)))
              (add-particle part s))
         (decf (particle-emitter-acc em)))))

;; Particles state modification
(defgeneric affect-particles (affector system dt))

(defstruct gravity-affector
  (strength 100.0)
  (direction (make-vector-2d :x 0.0 :y -1.0)))

(defmethod affect-particles ((a gravity-affector) (s particle-system) dt)
  (let ((dx (* (gravity-affector-strength a) (vector-2d-x (gravity-affector-direction a)) dt))
        (dy (* (gravity-affector-strength a) (vector-2d-y (gravity-affector-direction a)) dt)))
    (map-particles (p s)
      (incf (particle-vx p) dx)
      (incf (particle-vy p) dy))))

(defstruct resistance-affector
  (value 0.9))

(defmethod affect-particles ((a resistance-affector) (s particle-system) dt)
  (let ((factor (* (resistance-affector-value a) dt)))
    (map-particles (p s)
      (decf (particle-vx p) (* (particle-vx p) factor))
      (decf (particle-vy p) (* (particle-vy p) factor)))))

(defstruct fading-affector
  (rate 0.5))

(defmethod affect-particles ((a fading-affector) (s particle-system) dt)
  (let ((df (* (fading-affector-rate a) dt)))
    (map-particles (p s)
      (if (> (color-a (particle-color p)) df)
         (decf (color-a (particle-color p)) df)
         (setf (color-a (particle-color p)) 0.0)))))
