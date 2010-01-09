;; Game screen management

(in-package #:glaw)

(defstruct screen-stack
  screens     ;; all pushed screens
  render      ;; screens that need to be rendered
  update      ;; screens that need to be updated
  )

(defgeneric init-screen (screen &rest initargs &key &allow-other-keys))
(defgeneric shutdown-screen (screen))
(defgeneric update-screen (screen dt))
(defgeneric render-screen (screen))

(defun current-screen (stack)
  "Returns current active game screen or NIL if there's no screen
   in the stack."
  (when (screen-stack-screens stack)
    (first (screen-stack-screens stack))))

(defun push-screen (screen stack &key propagate-rendering propagate-update initargs)
  "Push SCREEN on top of STACK and then initialize SCREEN."
  (push screen (screen-stack-screens stack))
  (unless propagate-rendering
    (setf (screen-stack-render stack) '()))
  (push screen (screen-stack-render stack))
  (unless propagate-update
    (setf (screen-stack-update stack) '()))
  (push screen (screen-stack-update stack))
  (apply 'init-screen screen initargs))

(defun pop-screen (stack)
  "Shutdown current screen and pop it from STACK."
  (let ((scr (current-screen stack)))
    (shutdown-screen scr)
    (pop (screen-stack-screens stack))
    (setf (screen-stack-render stack) (remove scr (screen-stack-render stack)))
    (setf (screen-stack-update stack) (remove scr (screen-stack-update stack)))))

(defun render-screens (stack)
  (dolist (scr (screen-stack-render stack))
    (render-screen scr)))

(defun update-screens (stack dt)
  (dolist (scr (screen-stack-update stack))
    (update-screen scr dt)))
