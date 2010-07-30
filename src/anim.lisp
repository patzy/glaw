(in-package #:glaw)

;;; Animation
(defgeneric animation-duration (anim)
  (:documentation "Returns total time length of ANIM."))

(defgeneric animation-apply (anim obj date)
  (:documentation "Apply animation ANIM at DATE to OBJ."))

(defstruct anim-state
  animation
  (time 0.0)
  (scale 1.0)
  (loops 0);;FIXME: we don't even know if we can loop here....
  )

(defun anim-state-update (state dt)
  (when (or (= (anim-state-loops state) 0)
            (< (anim-state-time state) (* (anim-state-loops state)
                                          (animation-duration (anim-state-animation state)))))
    (incf (anim-state-time state) (* dt (anim-state-scale state)))))

(defun anim-state-apply (state obj)
  (animation-apply (anim-state-animation state) obj (anim-state-time state)))

;;; Keyframed animation
;; Each channel contains frame data for a specific aspect of the object you want to apply anim to
;; TODO: interpolation support
(defstruct keyframe-anim
  (channels '())
  (hints '())
  frame-time
  start-frame
  nb-frames)

(defmethod animation-duration ((anim keyframe-anim))
  (* (keyframe-anim-nb-frames anim) (keyframe-anim-frame-time anim)))

(defun keyframe-anim-end-frame (anim)
  (+ (keyframe-anim-start-frame anim) (keyframe-anim-nb-frames anim)))

(defun keyframe-anim-frame-date (anim frame)
  (* (mod frame (keyframe-anim-nb-frames anim)) (keyframe-anim-frame-time anim)))

(defun keyframe-anim-date-frame (anim date)
  (let* ((cursor (* (mod (/ date (animation-duration anim)) 1.0)
                    (- (keyframe-anim-nb-frames anim) 1)))
         (index (floor cursor)))
    (if (< (- cursor index) 0.5)
        index
        (1+ index))))

(defun channel-frame-data (channel frame)
  (if (listp channel)
      (nth frame channel)
      (when (zerop frame)
        channel)))

(defgeneric frame-apply-channel (channel-type obj frame-data))

(defmethod animation-apply ((it keyframe-anim) obj date)
  (let ((frame (keyframe-anim-date-frame it date)))
    (loop for (type . index) in (keyframe-anim-hints it)
       for frame-data = (channel-frame-data (nth index (keyframe-anim-channels it)) frame)
       when frame-data
       do (frame-apply-channel type obj frame-data))))

(defmacro define-anim-channels (((obj-sym obj-type) data-sym) &rest channels)
  `(progn ,@(loop for ( type . body ) in channels
                 collect `(defmethod frame-apply-channel ((type (eql ,type))
                                                          (,obj-sym ,obj-type)
                                                          ,data-sym)
                            ,@body))))


