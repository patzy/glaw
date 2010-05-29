(in-package #:glaw)

(defstruct anim-state
  animation
  time
  scale)

(defun anim-state-update (state dt)
  (when (or (= (animation-loops (anim-state-animation state)) 0)
            (< (anim-state-time state) (* (animation-loops (anim-state-animation state))
                                          (animation-duration (anim-state-animation state)))))
    (incf (anim-state-time state) (* dt (anim-state-scale state)))))

(defstruct animation
  frame-time
  start-frame
  nb-frames
  (loops 1))

(defun animation-end-frame (anim)
  (+ (animation-start-frame anim) (animation-nb-frames anim)))

(defun animation-frame-date (anim frame)
  (* (mod frame (animation-nb-frames anim)) (animation-frame-time anim)))

(defun animation-date-frame (anim date)
  (let* ((cursor (* (mod (/ date (animation-duration anim)) 1.0)
                    (- (animation-nb-frames anim) 1)))
         (index (floor cursor)))
    (if (< (- cursor index) 0.5)
        index
        (1+ index))))

(defun animation-duration (anim)
  (* (animation-nb-frames anim) (animation-frame-time anim)))

(defgeneric animation-apply-time (anim obj date))
(defgeneric animation-apply-frame (anim obj frame))

(defun anim-state-apply (state obj)
  (animation-apply-time (anim-state-animation state) obj (anim-state-time state)))

;;; Texture animation
(defstruct (texture-anim (:include animation))
  textures
  coords)

(defun texture-anim-frame (tex-anim frame)
  (let ((tex (if (listp (texture-anim-textures tex-anim))
                 (nth frame (texture-anim-textures tex-anim))
                 (texture-anim-textures tex-anim)))
        (coords (if (listp (texture-anim-coords tex-anim))
                    (nth frame (texture-anim-coords tex-anim))
                    (texture-anim-coords tex-anim))))
    (values tex coords)))

;; Sprite animation
(defmethod animation-apply-time ((it texture-anim) (sp sprite) date)
  (animation-apply-frame it sp (animation-date-frame it date)))

(defmethod animation-apply-frame ((it texture-anim) (sp sprite) (frame integer))
  (multiple-value-bind (tex coords) (texture-anim-frame it frame)
    (setf (sprite-texture sp) tex)
    (setf (shape-tex-coords (sprite-shape sp)) coords)))
