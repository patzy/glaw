(in-package #:glaw)


;; Framerate utils
(defstruct frame-counter
  (sample-size 2.0) ;; seconds
  (min most-positive-single-float)
  (max most-negative-single-float)
  (sum 0.0)
  (nb-frames 0)
  (average 0.0)
  (time-ratio 0.5)
  (last-render-time 1.0))

(defvar *frame-counter* (make-frame-counter)
  "Default framerate counter.")

(defun frame-counter-update (counter dt)
  "Updates framerate informations. DT is the elapsed time since last frame was rendered."
  (setf (frame-counter-last-render-time counter) (+ (* dt (frame-counter-time-ratio counter))
                                                    (* (frame-counter-last-render-time counter)
                                                       (- 1.0 (frame-counter-time-ratio counter)))))
  (incf (frame-counter-nb-frames counter))
  (incf (frame-counter-sum counter) dt)
  (when (>= (frame-counter-sum counter) (frame-counter-sample-size counter))
    (let ((fps (/ (frame-counter-nb-frames counter) (frame-counter-sum counter))))
      (when (< fps (frame-counter-min counter))
        (setf (frame-counter-min counter) fps))
      (when (> fps (frame-counter-max counter))
        (setf (frame-counter-max counter) fps))
      (setf (frame-counter-nb-frames counter) 0
            (frame-counter-sum counter) 0.0
            (frame-counter-average counter) fps))))

(defun frame-counter-current (counter)
  (/ 1.0 (frame-counter-last-render-time counter)))

(let ((last-fps-update (get-internal-real-time)))
  (defun update-fps ()
    (let ((dt (/ (- (get-internal-real-time) last-fps-update) internal-time-units-per-second)))
      (frame-counter-update *frame-counter* dt)
      (setf last-fps-update (get-internal-real-time)))))

(defun current-fps ()
  (frame-counter-current *frame-counter*))

(defun min-fps ()
  (frame-counter-min *frame-counter*))

(defun max-fps ()
  (frame-counter-max *frame-counter*))

(defun avg-fps ()
  (frame-counter-average *frame-counter*))

(defun frame-time ()
  (frame-counter-last-render-time *frame-counter*))