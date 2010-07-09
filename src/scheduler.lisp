(in-package #:glaw)

;; timer and scheduler stuff
(defstruct timer
  function
  args
  elapsed
  time
  repeat)

(defun timer-expired-p (timer)
  (>= (timer-elapsed timer) (timer-time timer)))

(defun update-timer (timer dt)
  "Update timer state and execute if expired.
   Returns updated timer or NIL if timer expired and repeat was not set."
  (incf (timer-elapsed timer) dt)
  (if (timer-expired-p timer)
    (progn (apply (timer-function timer) (timer-args timer))
           (when (timer-repeat timer)
             (setf (timer-time timer) (timer-repeat timer))
             (setf (timer-elapsed timer) 0.0)
             timer))
    timer))


(defun run-timers (timer-list dt)
  (loop for it in timer-list
       with remaining = '()
       do (when (update-timer it dt)
            (push it remaining))
       finally (return remaining)))


(defvar *timers* '())

(defun schedule (secs repeat function &rest args)
  (let ((timer (make-timer :function function
                           :args args
                           :elapsed 0.0
                           :time secs
                           :repeat repeat)))
    (push timer *timers*)
    timer))

(defun cancel-timer (timer)
  (setf *timers* (remove timer *timers*)))

(defun update-scheduler (dt &key (lock 0))
  (setf *timers* (run-timers *timers* dt)))

;; timestep lock
(defmacro with-timestep ((real expected) &body body)
  `(progn (when (< ,real ,expected)
            (sleep (- ,expected ,real)))
          ,@body))