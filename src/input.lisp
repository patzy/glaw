;;;; Input management

;; Abstract layer for different input handling methods (events, state-based)
;; Different methods should be usable together
;; Note that the actual input values (keysym, keycoe, buttons, state etc) depend on the application
;; and the windowing framework used, here we only provide different input handling methods and
;; helpers.

(in-package :glaw)

(defvar *mouse-x* 0)

(defvar *mouse-y* 0)

(defvar *input-stack* '())

(defvar *input-handlers* '(:global)
  "All active input handlers.")

(defun push-input-handlers ()
  (push *input-handlers* *input-stack*)
  (setf *input-handlers* '(:global)))

(defun pop-input-handlers ()
  (setf *input-handlers* (pop *input-stack*)))

(defun dispatch-key-event (keysym state keycode string)
  "Dispatch keyboard events."
  (dolist (h *input-handlers*)
    (on-key h keysym state keycode string)))

(defun dispatch-button-event (device btn state)
  "Dispatch digital button events."
  (dolist (h *input-handlers*)
    (on-button h device btn state)))

(defun dispatch-motion-event (device dx dy)
  "Dispatch relative axis device events."
  (dolist (h *input-handlers*)
    (on-motion h device dx dy)))

(defun update-mouse-position (x y)
  (setf *mouse-x* x
        *mouse-y* y))

;; Callback based input handling
(defun add-input-handler (handler)
  (push handler *input-handlers*))

(defun remove-input-handler (handler)
  (setf *input-handlers* (remove handler *input-handlers*)))

(defun clear-input-handlers ()
  (setf *input-handlers* '(:global)))

(defgeneric on-key (input-object keysym key-state keycode string)
  (:documentation
   "Key press/release callback."))

(defgeneric on-button (input-object input-device btn btn-state)
  (:documentation
   "Buttons press/release callback. For buttons not on the keyboard..."))

(defgeneric on-motion (input-object input-device dx dy)
  (:documentation
   "Motion callback, may be used for any 2d motion input device."))

(defmethod on-key (input-object key key-state keycode string)
  (declare (ignore input-object key key-state keycode string))
  (values))

(defmethod on-button (input-object input-device btn btn-state)
  (declare (ignore input-object input-device btn btn-state))
  (values))

(defmethod on-motion (input-object input-device dx dy)
  (declare (ignore input-object input-device dx dy))
  (values))

(defmacro key-handler (class (keysym key-state) &body body)
  `(defmethod on-key (,(if (eq class :global)
                           `(it (eql :global))
                           class)
                      ,(if keysym
                           `(keysym (eql ,keysym))
                           `key)
                      (key-state (eql ,key-state)) keycode string)
     (declare (ignore keycode string))
     ,@body))

(defmacro button-handler (class device (btn btn-state) &body body)
  `(defmethod on-button (,(if (eq class :global)
                           `(it (eql :global))
                           class)
                         ,(if device
                              `(device (eql ,device))
                              `device)
                         ,(if btn
                              `(btn (eql ,btn))
                              `btn)
                         (btn-state (eql ,btn-state)))
       ,@body))

(defmacro motion-handler (class device &body body)
  `(defmethod on-motion (,(if (eq class :global)
                           `(it (eql :global))
                           class)
                         ,(if device
                              `(device (eql ,device))
                              `device)
                      dx dy)
     ,@body))


;; Key sequences management
(defstruct (input-sequence
             (:constructor make-input-sequence (&key keysyms delay duration (remaining keysyms))))
  keysyms    ;; keysyms list
  delay      ;; delay between each key-press
  duration   ;; total sequence maximum time
  remaining)
