;;;; Input management
;; TODO: handle key modifiers
(in-package :glaw)

(defvar *mouse-x* 0)
(defvar *mouse-y* 0)

(defvar *input-handlers* '()
  "All active input handlers.")

(defun add-input-handler (handler)
  (push handler *input-handlers*))

(defun remove-input-handler (handler)
  (setf *input-handlers* (remove handler *input-handlers*)))

(defun update-mouse-position (x y)
  (setf *mouse-x* x)
  (setf *mouse-y* y))

(defgeneric on-key (input-object key key-state)
  (:documentation
   "Key press/release callback. This includes mouse clicks."))

(defmethod on-key (input-object key key-state)
  nil)

(defgeneric on-button (input-object input-device btn btn-state)
  (:documentation
   "Buttons press/release callback. For buttons not on the keyboard..."))

(defmethod on-button (input-object input-device dx dy)
  nil)

(defgeneric on-motion (input-object input-device dx dy)
  (:documentation
   "Motion callback, may be used for any 2d motion input device."))

(defmethod on-motion (input-object input-device dx dy)
  nil)

(defun dispatch-key-event (key state)
  (dolist (h *input-handlers*)
    (on-key h key state))
  (on-key :global key state))

(defun dispatch-button-event (device btn state)
  (dolist (h *input-handlers*)
    (on-button h device btn state))
  (on-button :global device btn state))

(defun dispatch-motion-event (device dx dy)
  (dolist (h *input-handlers*)
    (on-motion h device dx dy))
  (on-motion :global device dx dy))

(defmacro key-handler (class (key key-state) &body body)
  `(defmethod on-key (,(if (eq class :global)
                           `(it (eql :global))
                           `(it ,class))
                      ,(if key
                           `(key (eql ,key))
                           `key)
                      (key-state (eql ,key-state)))
     ,@body))

(defmacro button-handler (class device (btn btn-state) &body body)
  `(defmethod on-button (,(if (eq class :global)
                           `(it (eql :global))
                           `(it ,class))
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
                           `(it ,class))
                         ,(if device
                              `(device (eql ,device))
                              `device)
                      dx dy)
     ,@body))