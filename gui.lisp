(in-package #:glaw)

;;; GUI
(defvar *gui* nil)
(defvar *gui-view* nil)

(defstruct gui
  (font nil)
  (widgets '())) ;; last one has focus

(defun init-gui (font)
  (setf *gui* (make-gui :font font))
  (setf *gui-view* (create-2d-view 0 0 *display-width* *display-height*))
  (add-input-handler *gui*))

(defun shutdown-gui ()
  (remove-input-handler *gui*)
  (setf *gui* nil))

(defun render-gui ()
  (set-view-2d *gui-view*)
  (dolist (w (reverse (gui-widgets *gui*)))
    (render-widget w)
    (when (focused w)
      (gl:disable :texture-2d)
      (gl:color 1 1 1 1)
      (gl:with-primitive :line-loop
        (gl:vertex  (gl-x w)
                    (gl-y w))
        (gl:vertex  (+ (pos-x w) (width w))
                    (gl-y w))
        (gl:vertex  (+ (pos-x w) (width w))
                    (- (gl-y w) (height w)))
        (gl:vertex  (pos-x w)
                    (- (gl-y w) (height w)))))))


(defun update-gui (&optional (width *display-width*)
                             (height *display-height*))
  (update-2d-view *gui-view* 0 0 width height))

(defun create-widget (widget-type parent-widget &rest initargs)
  (let ((created-widget (apply 'make-instance widget-type initargs)))
    (format t "Created widget: ~S~%" created-widget)
    (add-child-widget parent-widget created-widget)
    created-widget))

;; focus management
(defmethod gui-focus ((it gui))
  (first (gui-widgets it)))

(defmethod gui-focus-next ((it gui))
  (when (gui-focus it)
    (unfocus (gui-focus it))
    (setf (gui-widgets it) (rotate-list-left (gui-widgets it) 1))
    (focus (gui-focus it))))

(defmethod gui-focus-prev ((it gui))
  (when (gui-focus it)
    (unfocus (gui-focus it))
    (setf (gui-widgets it) (rotate-list-right (gui-widgets it) 1))
    (focus (gui-focus it))))

;; input handling
(key-handler gui (#\Tab :press)
     (when (gui-focus it)
       (gui-focus-next it)))

(defun gui-widget-at (gui x y)
  (loop for w in (gui-widgets gui)
     for found = (gui-widget-child-at w x y)
     when found
     return found))

(defmethod on-button ((it gui) (device (eql :mouse))
                      btn (btn-state (eql :press)))
  (let ((widget (gui-widget-at it *mouse-x* *mouse-y*)))
    (when widget
      (gui-widget-mouse-down widget))))

(defmethod on-button ((it gui) (device (eql :mouse))
                      btn (btn-state (eql :release)))
  (let ((widget (gui-widget-at it *mouse-x* *mouse-y*)))
    (when widget
      (gui-widget-mouse-up widget))))

(defmethod on-motion ((it gui) (device (eql :mouse)) dx dy)
  (let ((widget (gui-widget-at it *mouse-x* *mouse-y*)))
    (when widget
      (gui-widget-mouse-move widget dx dy))))

(defclass gui-widget ()
   ((x :accessor pos-x :initform 0 :initarg :x)  ;; screen coordinates
    (y :accessor pos-y :initform 0 :initarg :y)
    (width :accessor width :initform 1 :initarg :width)
    (height :accessor height :initform 1 :initarg :height)
    (color :accessor gui-widget-color
           :initform (create-color 0.35 0.35 0.35 0.5)
           :initarg :color)
    (texture :accessor gui-widget-texture :initform nil :initarg :texture)
    ;;(stick :reader stick :initform nil :initarg :stick)
    (visible :accessor visible :initform t)
    (focused :accessor focused :initform t)
    (parent-widget :accessor parent-widget :initform nil)
    (children :accessor children :initform '() :initarg :children))
  (:documentation "Base class for all gui components."))

(defmethod gui-widget-mouse-down (gui-widget)
  (format t "Widget mouse down: ~S~%" gui-widget))
(defmethod gui-widget-mouse-up (gui-widget)
  (format t "Widget mouse up: ~S~%" gui-widget))
(defmethod gui-widget-mouse-move (gui-widget dx dy))

;; Find the lowest widget in the tree at (x;y)
;; (x;y) are relative to the provided widget
(defun gui-widget-child-at (w x y)
  (dolist (c (children w))
    (let ((found (gui-widget-child-at c (- x (pos-x w)) (- y (pos-y w)))))
      (when found
        (return-from gui-widget-child-at found))))
  (when (and (> x (pos-x w)) (< x (+ (pos-x w) (width w)))
             (> y (pos-y w)) (< y (+ (pos-y w) (height w))))
    w))

;; gui Y-axis is inverted (top left origin instead of OGL bottom left)
;; pos-x and pos-y are stored in GUI coordinate system
;; this requires to use gl-x and gl-y accessors from rendering functions
(defmethod gl-x ((w gui-widget))
  (pos-x w))

(defmethod gl-y ((w gui-widget))
  (- *display-height* (pos-y w)))

(defmethod resize ((o gui-widget) w h)
  (setf (width o) w)
  (setf (height o) h))

(defmethod focus :around ((w gui-widget))
  (call-next-method)
  (dolist (c (children w))
    (focus c)))

(defmethod unfocus :around ((w gui-widget))
  (call-next-method)
  (dolist (c (children w))
    (unfocus c)))

(defmethod focus ((w gui-widget))
  (setf (focused w) t)
  (add-input-handler w))

(defmethod unfocus ((w gui-widget))
  (setf (focused w) nil)
  (remove-input-handler w))

(defmethod add-child-widget ((p (eql nil)) (c gui-widget))
  (format t "Adding root widget ~S~%" c)
  (when (gui-focus *gui*)
    (unfocus (gui-focus *gui*)))
  (push c (gui-widgets *gui*))
  (focus c))

(defmethod add-child-widget ((p gui) (c gui-widget))
  (when (gui-focus p)
    (unfocus (gui-focus p)))
  (push c (gui-widgets p))
  (focus c))

(defmethod add-child-widget ((e gui-widget) (c gui-widget))
  (setf (parent-widget c) e)
  (push c (children e)))

(defgeneric render-widget (gui-widget)
  (:documentation "Renders the provided widget on screen."))

(defmethod render-widget ((w gui-widget))
  (set-color (gui-widget-color w))
  (if (gui-widget-texture w)
      (progn (gl:enable :texture-2d)
             (select-texture (gui-widget-texture w)))
      (gl:disable :texture-2d))
  (gl:with-primitive :quads
    (gl:tex-coord 0 0)
    (gl:vertex  (gl-x w)
                (gl-y w))
    (gl:tex-coord 1 0)
    (gl:vertex  (+ (pos-x w) (width w))
                (gl-y w))
    (gl:tex-coord 1 1)
    (gl:vertex  (+ (pos-x w) (width w))
                (- (gl-y w) (height w)))
    (gl:tex-coord 0 1)
    (gl:vertex  (pos-x w)
                (- (gl-y w) (height w)))))

(defmethod render-widget :around ((w gui-widget))
  (when (visible w)
    (call-next-method)
    (gl:with-pushed-matrix
        (gl:translate (pos-x w) (- (pos-y w)) 0)
      (dolist (c (children w))
        (render-widget c)))))

(defgeneric update-widget (gui-widget)
  (:documentation "Do whatever needs to be done to update the state of the
                   widget."))

(defmethod update-widget ((w gui-widget))
  (declare (ignore w)))

(defmethod show ((e gui-widget))
  (unless (visible e)
    (setf (visible e) t)
    (add-input-handler e)))

(defmethod hide ((e gui-widget))
  (when (visible e)
    (setf (visible e) nil)
    (remove-input-handler e)))

;; Some widget definitions
(defclass gui-window (gui-widget)
  ((moveable :accessor gui-window-moveable :initform nil :initarg :moveable)
   (moving :accessor gui-window-moving :initform nil)
   (title :reader title :initarg :title))
  (:default-initargs))

(defmethod gui-widget-mouse-down ((it gui-window))
  (setf (gui-window-moving it) t))

(defmethod gui-widget-mouse-up ((it gui-window))
  (setf (gui-window-moving it) nil))

(defmethod gui-widget-mouse-move ((it gui-window) dx dy)
  (when (and (gui-window-moveable it) (gui-window-moving it))
    (incf (pos-x it) dx)
    (incf (pos-y it) dy)))

(defmethod render-widget ((w gui-window))
  (call-next-method)
  (set-color/rgb 1 1 1)
  (render-bitmap-string (gl-x w) (- (gl-y w) 3
                                    (string-height (title w)
                                                   (gui-font *gui*)))
                        (title w) (gui-font *gui*))
  (gl:with-primitive :lines
    (gl:vertex (gl-x w)
               (- (gl-y w) 6 (string-height (title w)
                                            (gui-font *gui*))))
    (gl:vertex (+ (gl-x w) (width w))
               (- (gl-y w) 6 (string-height (title w)
                                           (gui-font *gui*))))))


(defclass gui-label (gui-widget)
  ((text :accessor text :initarg :text :initform "Some text?"))
  (:documentation "One line text"))

(defmethod initialize-instance :after ((w gui-label) &key)
  (update-dimensions w))

(defmethod update-dimensions ((w gui-label))
  (setf (width w) (string-width (text w) (gui-font *gui*)))
  (setf (height w) (string-height (text w) (gui-font *gui*))))

(defmethod (setf text) ((txt string) (w gui-label))
  (setf (slot-value w 'text) txt)
 (update-dimensions w))

(defmethod render-widget ((w gui-label))
  (render-bitmap-string (pos-x w) (- (gl-y w) (height w))
                        (text w) (gui-font *gui*)))


(defclass gui-text-input (gui-widget)
  ((input :accessor input :initform '() :initarg :input)
   (action :accessor action :initform nil :initarg :action)
   (prefix :accessor prefix :initform "" :initarg :prefix))
  (:default-initargs))

(defmethod initialize-instance :after ((w gui-text-input) &key)
  (setf (height w) (string-height (prefix w) (gui-font *gui*)))
  (setf (width w) (string-width (prefix (gui-font *gui*)))))

;; setup keymap
(key-handler gui-text-input (#\Return :press)
    (when (action it)
      (funcall (action it) it)))
(key-handler gui-text-input (#\Backspace :press)
    (when (input it)
      (decf (width it) (font-width (gui-font *gui*)))
      (pop (input it))))
(key-handler gui-text-input (nil :press)
    (incf (width it) (font-width (gui-font *gui*)))
    (push key (input it)))

(defmethod show ((w gui-text-input))
  (add-input-handler w)
  (call-next-method))

(defmethod hide ((w gui-text-input))
  (remove-input-handler w)
  (call-next-method))

(defun clear-text-input (w)
  (setf (input w) '())
  (setf (height w) (string-height (prefix w)))
  (setf (width w) (string-width (prefix w))))

(defun string-text-input (w)
  (coerce (reverse (input w)) 'string))

(defmethod render-widget ((w gui-text-input))
  (gl:color 1 1 1 1)
  (render-bitmap-string (pos-x w) (- (gl-y w) (height w))
       (concatenate 'string (prefix w) (coerce (reverse (input w)) 'string))))

(defclass gui-button (gui-widget)
  ((text :accessor text :initform '() :initarg :text)
   (action :accessor action :initform nil :initarg :action)
   (pressed :accessor gui-button-pressed :initform nil))
  (:default-initargs
    :color (create-color 1 1 1)
    :texture (create-texture-from-file "button.png"
                                       :env-mode :replace)))

(defmethod initialize-instance :after ((w gui-button) &key action)
  (format t "Creating gui-button with :action ~S~%"
          action)
  (setf (height w) (string-height (text w) (gui-font *gui*)))
  (setf (width w) (string-width (text w) (gui-font *gui*))))

(key-handler gui-button (#\Return :press)
    (when (action it)
      (funcall (action it) it)))

(defmethod gui-widget-mouse-down ((it gui-button))
  (setf (gui-button-pressed it) t))

(defmethod gui-widget-mouse-up ((it gui-button))
  (when (and (gui-button-pressed it) (action it))
    (funcall (action it) it))
  (setf (gui-button-pressed it) nil))

(defmethod show ((w gui-button))
  (add-input-handler w)
  (call-next-method))

(defmethod hide ((w gui-button))
  (remove-input-handler w)
  (call-next-method))

(defmethod render-widget ((w gui-button))
  (call-next-method)
  (render-bitmap-string (pos-x w) (- (gl-y w) (height w))
                        (text w) (gui-font *gui*)))

(defclass gui-multiline-text (gui-label)
  ((nb-lines :accessor nb-lines :initform 10 :initarg :nb-lines)
   (expandable :accessor expandable :initform t :initarg :expandable))
   (:default-initargs :text '()))

(defmethod update-dimensions ((w gui-multiline-text))
  (setf (height w) (* (length (text w)) (string-height "")))
  (setf (width w)
        (loop for l in (text w)
             maximize (string-width l))))

(defmethod add-line ((w gui-multiline-text) (line string))
  (push line (text w))
  (unless (expandable w)
    (when (> (length (text w)) (nb-lines w))
      (setf (text w) (butlast (text w)))))
  (update-dimensions w))

(defmethod render-widget ((w gui-multiline-text))
  (gl:color (first (color w))
            (second (color w))
            (third (color w))
            (fourth (color w)))
  (let ((x (pos-x w))
        (y (- (gl-y w) (string-height ""))))
  (dolist (txt (reverse (text w)))
    (render-bitmap-string x y txt)
    (decf y (string-height "")))))


(defclass gui-slider (gui-widget)
  ((min :accessor gui-slider-min :initform 0 :initarg :min)
   (max :accessor gui-slider-max :initform 100 :initarg :max)
   (step :accessor gui-slider-step :initform 1 :initarg :step)
   (value :accessor gui-slider-value :initform 50 :initarg :value)))

(defmethod gui-slider-step-up ((w gui-slider))
  (when (> (incf (gui-slider-value w) (gui-slider-step w)) (gui-slider-max w))
    (setf (gui-slider-value w) (gui-slider-max w))))

(defmethod gui-slider-step-down ((w gui-slider))
  (when (< (decf (gui-slider-value w) (gui-slider-step w)) (gui-slider-min w))
    (setf (gui-slider-value w) (gui-slider-min w))))


(defmethod render-widget ((w gui-slider))
  (gl:color 1 1 1 1)
  (gl:begin :lines)
  (gl:vertex (pos-x w)
             (- (gl-y w) (/ (height w) 2.0)))
  (gl:vertex (+ (pos-x w) (width w))
             (- (gl-y w) (/ (height w) 2.0)))
  (let ((slider-scale (/ (width w) (- (gui-slider-max w) (gui-slider-min w)))))
    (gl:vertex (+ (pos-x w) (* slider-scale (gui-slider-value w)))
               (gl-y w))
    (gl:vertex (+ (pos-x w) (* slider-scale (gui-slider-value w)))
               (- (gl-y w) (height w))))
  (gl:end))