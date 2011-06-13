(in-package #:glaw)

;; Frame timer
(defstruct frame-timer
  ;; time window average
  (avg-sample-size 2.0) ;; in seconds
  (avg-min most-positive-single-float)
  (avg-max most-negative-single-float)
  (time-sum 0.0)
  (avg-frame-time 0.0)
  (avg-nb-frames 0)
  (avg-framerate 50.0)
  ;; instant
  (instant-min most-positive-single-float)
  (instant-max most-negative-single-float)
  (instant-framerate 0.0)
  (time-ratio 0.5)
  (instant-frame-time 50.0)
  ;; all time average
  (total-framerate 50.0)
  (total-frame-time 0.0)
  (total-time 0.0)
  (total-nb-frames 0))

(defun frame-timer-update (timer dt)
  "Updates framerate informations. DT is the elapsed time since last frame was rendered."
  ;; update all time average framerate data
  (incf (frame-timer-total-time timer) dt)
  (incf (frame-timer-total-nb-frames timer))
  (setf (frame-timer-total-framerate timer)
        (/ (frame-timer-total-nb-frames timer)
           (frame-timer-total-time timer)))
  (setf (frame-timer-total-frame-time timer)
        (/ (frame-timer-total-time timer)
           (frame-timer-total-nb-frames timer)))
  ;; update instantaneous framerate data
  (setf (frame-timer-instant-frame-time timer) (+ (* dt (frame-timer-time-ratio timer))
                                                   (* (frame-timer-instant-frame-time timer)
                                                      (- 1.0 (frame-timer-time-ratio timer)))))
  (let ((fps (/ 1.0 (frame-timer-instant-frame-time timer))))
    (setf (frame-timer-instant-framerate timer) fps)
    (when (< fps (frame-timer-instant-min timer))
      (setf (frame-timer-instant-min timer) fps))
    (when (> fps (frame-timer-instant-max timer))
      (setf (frame-timer-instant-max timer) fps)))
  ;; update average framerate data
  (incf (frame-timer-time-sum timer) dt)
  (incf (frame-timer-avg-nb-frames timer))
  (when (>= (frame-timer-time-sum timer) (frame-timer-avg-sample-size timer))
    (let ((fps (/ (frame-timer-avg-nb-frames timer) (frame-timer-time-sum timer)))
          (ftime (/ (frame-timer-time-sum timer) (frame-timer-avg-nb-frames timer))))
      (when (< fps (frame-timer-avg-min timer))
        (setf (frame-timer-avg-min timer) fps))
      (when (> fps (frame-timer-avg-max timer))
        (setf (frame-timer-avg-max timer) fps))
      (setf (frame-timer-avg-nb-frames timer) 0
            (frame-timer-time-sum timer) 0.0
            (frame-timer-avg-frame-time timer) ftime
            (frame-timer-avg-framerate timer) fps))))

;;; Frame timer singleton
(defvar %frame-timer% nil
  "Current frame timer.")

(defmacro with-frame-timer (timer &body body)
  `(let ((%frame-timer% ,timer))
     ,@body))

(defun update-fps (dt)
  (frame-timer-update %frame-timer% dt))

(defun instant-fps ()
  (frame-timer-instant-framerate %frame-timer%))

(defun instant-min-fps ()
  (frame-timer-instant-min %frame-timer%))

(defun instant-max-fps ()
  (frame-timer-instant-max %frame-timer%))

(defun instant-frame-time ()
  (frame-timer-instant-frame-time %frame-timer%))

(defun avg-fps ()
  (frame-timer-avg-framerate %frame-timer%))

(defun avg-frame-time ()
  (frame-timer-avg-frame-time %frame-timer%))

(defun avg-min-fps ()
  (frame-timer-avg-min %frame-timer%))

(defun avg-max-fps ()
  (frame-timer-avg-max %frame-timer%))

(defun total-fps ()
  (frame-timer-total-framerate %frame-timer%))

(defun total-frame-time ()
  (frame-timer-total-frame-time %frame-timer%))

(defun total-timer-time ()
  (frame-timer-total-time %frame-timer%))

(defun total-timer-nb-frames ()
  (frame-timer-total-nb-frames %frame-timer%))

;;; Stats graphs
(defstruct stats-graph-data
  (samples (list))
  (max-samples 500)
  (min most-positive-single-float)
  (max most-negative-single-float))

(defun stats-graph-data-update (data sample &key filter)
  (setf (stats-graph-data-samples data)
        (append (stats-graph-data-samples data) (list sample)))
  (when (> sample (stats-graph-data-max data))
    (setf (stats-graph-data-max data) sample))
  (when (< sample (stats-graph-data-min data))
    (setf (stats-graph-data-min data) sample))
  (when (> (length (stats-graph-data-samples data))
           (stats-graph-data-max-samples data))
    (pop (stats-graph-data-samples data))))


(defstruct stats-grapher
  (datas (list))
  (colors (list))
  (enabled (list))
  (width 500)
  (height 100)
  (bg-color (create-color 0.7 0.7 0.7 0.6)))

(defun stats-grapher-add-data (grapher &key (data (make-stats-graph-data))
                                            (color (make-random-color))
                                            (enable t))
  (push data (stats-grapher-datas grapher))
  (push color (stats-grapher-colors grapher))
  (push enable (stats-grapher-enabled grapher))
  data)

(defun stats-grapher-set-data-color (grapher index color)
  (setf (nth index (stats-grapher-colors grapher)) color))

(defun stats-grapher-update-data (grapher index sample)
  (stats-graph-data-update (nth index (stats-grapher-datas grapher)) sample))

(defun stats-grapher-enable-data (grapher index)
  (setf (nth index (stats-grapher-colors grapher)) t))

(defun stats-grapher-disable-data (grapher index)
  (setf (nth index (stats-grapher-colors grapher)) nil))

(defun render-stats-grapher (grapher &key (x 0) (y 0))
  (gl:with-pushed-matrix
      (gl:translate x y 0)
  (gl:with-primitive :quads
    (set-color (stats-grapher-bg-color grapher))
    (gl:vertex 0 0)
    (gl:vertex (stats-grapher-width grapher) 0)
    (gl:vertex (stats-grapher-width grapher)
               (stats-grapher-height grapher))
    (gl:vertex 0 (stats-grapher-height grapher)))
  (set-color/rgb 0.0 0.0 0.0)
  (gl:with-primitive :lines
    (gl:vertex 0 (/ (stats-grapher-height grapher) 2.0))
    (gl:vertex (stats-grapher-width grapher) (/ (stats-grapher-height grapher) 2.0)))
  (loop for data in (stats-grapher-datas grapher)
     for color in (stats-grapher-colors grapher)
     for enabled in (stats-grapher-enabled grapher)
     when enabled
     do (set-color color)
       (when (> (length (stats-graph-data-samples data)) 2)
         (gl:with-primitive :line-strip
           (loop with x-step = (float (/ (stats-grapher-width grapher)
                                         (stats-graph-data-max-samples data)))
              with x-pos = 0
              with max = (stats-graph-data-max data)
              with min = (stats-graph-data-min data)
              for sample in (stats-graph-data-samples data)
              with amplitude = (- max min)
              with center = (/ (+ max min) 2.0)
              for normalized-sample = (if (zerop amplitude)
                                          0.0
                                          (+ 0.5 (/ (- sample center) amplitude)))
              for y-pos = (* normalized-sample (stats-grapher-height grapher))
              do (gl:vertex x-pos y-pos 0)
                (incf x-pos x-step)))))))

;;; Frame timer grapher
;; Data are in order:
;; instant-fps, instant-frame-time
;; avg-fps, avg-frame-time
;; all-time-fps, all-time-frame-time
(define-constant +frame-timer-grapher-instant-fps+ 0)
(define-constant +frame-timer-grapher-instant-frame-time+ 1)
(define-constant +frame-timer-grapher-avg-fps+ 2)
(define-constant +frame-timer-grapher-avg-frame-time+ 3)
(define-constant +frame-timer-grapher-all-time-fps+ 4)
(define-constant +frame-timer-grapher-all-time-frame-time+ 5)

(defun make-frame-timer-grapher ()
  (let ((grapher (make-stats-grapher)))
    (stats-grapher-add-data grapher)
    (stats-grapher-add-data grapher)
    (stats-grapher-add-data grapher)
    (stats-grapher-add-data grapher)
    (stats-grapher-add-data grapher)
    (stats-grapher-add-data grapher)
    grapher))

(defun frame-timer-grapher-update (grapher timer)
  (stats-grapher-update-data grapher +frame-timer-grapher-instant-fps+
                             (frame-timer-instant-framerate timer))
  (stats-grapher-update-data grapher +frame-timer-grapher-instant-frame-time+
                             (frame-timer-instant-frame-time timer))
  (stats-grapher-update-data grapher +frame-timer-grapher-avg-fps+
                             (frame-timer-avg-framerate timer))
  (stats-grapher-update-data grapher +frame-timer-grapher-avg-frame-time+
                             (frame-timer-avg-frame-time timer))
  (stats-grapher-update-data grapher +frame-timer-grapher-all-time-fps+
                             (frame-timer-total-framerate timer))
  (stats-grapher-update-data grapher +frame-timer-grapher-all-time-frame-time+
                             (frame-timer-total-frame-time timer)))
