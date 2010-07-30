;; Game screen management

(in-package #:glaw)

(defstruct screen-stack
  screens     ;; all pushed screens
  render      ;; screens that need to be rendered
  render-backup
  update      ;; screens that need to be updated
  update-backup
  )

(defgeneric init-screen (screen &rest initargs &key &allow-other-keys))
(defgeneric shutdown-screen (screen))
(defgeneric update-screen (screen dt))
(defgeneric render-screen (screen))
(defgeneric suspend-screen (screen))
(defgeneric resume-screen (screen))

(defmethod suspend-screen (screen)
  (declare (ignore screen)))
(defmethod resume-screen (screen)
  (declare (ignore screen)))

(defun current-screen (stack)
  "Returns current active game screen or NIL if there's no screen
   in the stack."
  (when (screen-stack-screens stack)
    (first (screen-stack-screens stack))))

(defun push-screen (screen stack &key propagate-rendering propagate-updating initargs)
  "Push SCREEN on top of STACK and then initialize SCREEN."
  (when (current-screen stack)
    (suspend-screen (current-screen stack)))
  (push screen (screen-stack-screens stack))
  (unless propagate-rendering
    (when (screen-stack-render stack)
      (push (screen-stack-render stack) (screen-stack-render-backup stack)))
    (setf (screen-stack-render stack) '()))
  (push screen (screen-stack-render stack))
  (unless propagate-updating
    (when (screen-stack-update stack)
      (push (screen-stack-update stack) (screen-stack-update-backup stack)))
    (setf (screen-stack-update stack) '()))
  (push screen (screen-stack-update stack))
  (apply 'init-screen screen initargs))

(defun pop-screen (stack)
  "Shutdown current screen and pop it from STACK."
  (let ((scr (current-screen stack)))
    (shutdown-screen scr)
    (pop (screen-stack-screens stack))
    (setf (screen-stack-render stack) (remove scr (screen-stack-render stack)))
    (when (and (not (screen-stack-render stack)) (screen-stack-render-backup stack))
      (setf (screen-stack-render stack) (pop (screen-stack-render-backup stack))))
    (setf (screen-stack-update stack) (remove scr (screen-stack-update stack)))
    (when (and (not (screen-stack-update stack)) (screen-stack-update-backup stack))
      (setf (screen-stack-update stack) (pop (screen-stack-update-backup stack)))))
  (when (current-screen stack)
    (resume-screen (current-screen stack))))


(defun replace-screen (stack screen)
  (pop-screen stack)
  (push-screen screen stack))

(defun empty-screen-stack (stack)
  (loop while (screen-stack-screens stack) do (pop-screen stack)))

(defun render-screens (stack)
  (dolist (scr (screen-stack-render stack))
    (render-screen scr)))

(defun update-screens (stack dt)
  (dolist (scr (screen-stack-update stack))
    (update-screen scr dt)))
