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

;; Basic handling
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

(defmacro motion-handler (class device (dx-sym dy-sym) &body body)
  `(defmethod on-motion (,(if (eq class :global)
                           `(it (eql :global))
                           class)
                         ,(if device
                              `(device (eql ,device))
                              `device)
                      ,dx-sym ,dy-sym)
     ,@body))


;; Input processing
;; FIXME: handle motion events ?
(defstruct input-processor
  (output :input-action))

(defgeneric input-processor-reset (processor)
  (:documentation "Cancel current processor state."))

(defgeneric input-processor-update (processor input state)
  (:documentation "Update processor internal state."))

(defgeneric input-process-valid-p (processor)
  (:documentation "Return T if processor's output should be triggered, NIL otherwise."))

(defgeneric on-input (input-object input)
  (:documentation "Generic input event.")
  (:method (input-object input)
    (declare (ignore input-object input))
    (values)))

(defmacro input-handler (class input &body body)
  `(defmethod on-input (,(if (eq class :global)
                           `(it (eql :global))
                           class)
                      ,(if input
                           `(input (eql ,input))
                           `input))
     ,@body))

(defmethod on-key ((proc input-processor) keysym key-state keycode string)
  (input-processor-update proc keysym key-state)
  (when (input-processor-valid-p proc)
    (dolist (h *input-handlers*)
      (unless (eq h proc)
        (on-input h (input-processor-output proc))))))

(defmethod on-button ((proc input-processor) device btn btn-state)
  (input-processor-update proc btn btn-state)
  (when (input-processor-valid-p proc)
    (dolist (h *input-handlers*)
      (unless (eq h proc)
        (on-input h (input-processor-output proc))))))

;; repeat
(defstruct (input-repeat (:include input-processor)
                         (:constructor make-input-repeat (&key input delay (output :input-action)
                                            (auto-reset nil)
                                            (max-delay (* delay internal-time-units-per-second)))))
  input
  max-delay
  old-press
  last-press)

(defmethod input-processor-reset ((it input-repeat))
  (setf (input-repeat-last-press it) (-  (input-repeat-max-delay it))
        (input-repeat-old-press it) nil))

(defmethod input-processor-update ((it input-repeat) input state)
  (when (eq state :press)
    (if (eq input (input-repeat-input it))
        (let ((date (get-internal-real-time)))
          (when (> (- date (input-repeat-last-press it)) (input-repeat-max-delay it))
            (input-processor-reset it))
          (unless (= (input-repeat-last-press it) (- (input-repeat-max-delay it)))
            (setf (input-repeat-old-press it) (input-repeat-last-press it)))
          (setf (input-repeat-last-press it) date))
        (input-processor-reset it))))

(defmethod input-processor-valid-p ((it input-repeat))
  (and (input-repeat-input it) (input-repeat-old-press it)
       (< (- (get-internal-real-time) (input-repeat-last-press it)) (input-repeat-max-delay it))))

;; sequence
(defstruct (input-sequence (:include input-processor)
             (:constructor make-input-sequence (&key inputs delay (output :input-action)
                                                     (max-delay (* delay
                                                                   internal-time-units-per-second))
                                                     (remaining-inputs inputs))))
  inputs
  max-delay
  remaining-inputs
  last-press)

(defmethod input-processor-reset ((it input-sequence))
  (setf (input-sequence-remaining-inputs it) (input-sequence-inputs it)
        (input-sequence-last-press it) (- (input-sequence-max-delay it))))

(defmethod input-processor-update ((it input-sequence) input state)
  (when (and (input-sequence-remaining-inputs it) (eq state :press))
    (if (eq (first (input-sequence-remaining-inputs it)) input) ;; reset on bad key
        (progn (pop (input-sequence-remaining-inputs it))
               (setf (input-sequence-last-press it) (get-internal-real-time)))
        (input-processor-reset it))))

(defmethod input-processor-valid-p ((it input-sequence))
  (unless (input-sequence-remaining-inputs it)
    (if (> (- (get-internal-real-time) (input-sequence-last-press it))
           (input-sequence-max-delay it))
        (progn (input-processor-reset it)
               nil)
        t)))


;; chord
(defstruct (input-chord (:include input-processor)
             (:constructor make-input-chord (&key inputs delay (output :input-action)
                                                  (max-delay (* delay
                                                                internal-time-units-per-second))
                                                  (pressed '()))))
  inputs
  max-delay
  pressed
  first-press)

(defmethod input-processor-reset ((it input-chord))
  (setf (input-chord-pressed it) '()
        (input-chord-first-press it) (- (input-chord-max-delay it))))

(defmethod input-processor-update ((it input-chord) input state)
  (when (and (eq state :press) (member input (input-chord-inputs it)))
    (when (not (input-chord-pressed it)) ;; first key
      (setf (input-chord-first-press it) (get-internal-real-time)))
    (if (< (- (get-internal-real-time) (input-chord-first-press it))
           (input-chord-max-delay it))
        (unless (member input (input-chord-pressed it))
          (push input (input-chord-pressed it)))
        (input-processor-reset it))))

(defmethod input-processor-valid-p ((it input-chord))
  (when (equal (input-chord-inputs it) (input-chord-pressed it))
    (if (> (- (get-internal-real-time) (input-chord-first-press it))
           (input-chord-max-delay it))
        (progn (input-processor-reset it)
               nil)
        t)))
