(in-package #:glaw)

;;; GUI
;; FIXME: this gl-x gl-y thing is kinda ugly...change this !
(defvar *gui* nil)
(defvar *gui-view* nil)

(defstruct gui
  (selected-widget 0)
  (widgets '()))

(defun init-gui ()
  (setf *gui* (make-gui))
  (setf *gui-view* (create-2d-view 0 0 *display-width* *display-height*))
  (add-input-handler *gui*))

(defun shutdown-gui ()
  (remove-input-handler *gui*)
  (setf *gui* nil))

(defun render-gui ()
  (set-view-2d *gui-view*)
  (mapc (lambda (widget)
          (render-widget widget)) (gui-widgets *gui*)))


(defun update-gui (&optional (width *display-width*)
                             (height *display-height*))
  (update-2d-view *gui-view* 0 0 width height))

(defun create-widget (widget-type parent-widget &rest initargs)
  (let ((created-widget (apply 'make-instance widget-type initargs)))
    (format t "Created widget: ~S~%" created-widget)
    (add-child-widget parent-widget created-widget)
    created-widget))

;; gui focus management
(defmethod gui-focus ((it gui))
  (nth (gui-selected-widget it) (gui-widgets it)))

(defmethod gui-focus-next ((it gui))
  (when (gui-focus it)
    (unfocus (gui-focus it))
    (setf (gui-selected-widget it) (mod (incf (gui-selected-widget it))
                                       (length (gui-widgets it))))
    (focus (gui-focus it))))

(defmethod gui-focus-prev ((it gui))
  (when (gui-focus it)
    (unfocus (gui-focus it))
    (setf (gui-selected-widget it) (mod (decf (gui-selected-widget it))
                                       (length (gui-widgets it))))
    (focus (gui-focus it))))

;; gui default keymap
(key-handler gui (#\Tab :press)
     (when (gui-focus it)
       (gui-focus-next it)))

;; (defgeneric gui-layout-arrange (layout widget))

;; (defclass gui-stack-layout ()
;;   ((direction :accessor gui-stack-layout-direction :initform :vertical
;;               :initarg :direction)))

;; (defmethod gui-layout-arrange ((layout gui-stack-layout) widget)
;;   (let ((coord (ecase 
;;   (dolist (w (children widget))
;;     (ecase (gui-stack-layout-direction layout)
;;       (:vertical 


(defclass gui-widget ()
   ((x :accessor pos-x :initform 0 :initarg :x)
    (y :accessor pos-y :initform 0 :initarg :y)
    (width :accessor width :initform 1 :initarg :width)
    (height :accessor height :initform 1 :initarg :height)
    (moveable :accessor moveable :initform nil :initarg :moveable)
    (moving :reader moving :initform nil)
    ;;(stick :reader stick :initform nil :initarg :stick)
    (visible :accessor visible :initform t)
    (focused :accessor focused :initform t)
    (parent-widget :accessor parent-widget :initform nil)
    (children :accessor children :initform '() :initarg :children))
  (:documentation "Base class for all gui components."))

(button-handler gui-widget :mouse (:left-button :press)
    (when (moveable it)
      (setf (slot-value it 'moving) t)))
(button-handler gui-widget :mouse (:left-button :release)
    (when (moveable it)
      (setf (slot-value it 'moving) nil)))
(motion-handler gui-widget :mouse
  (when (and (moveable it)
             (eq (slot-value it 'moving) t))
    (incf (pos-x it) dx)
    (incf (pos-y it) dy)))

;; gui Y-axis is inverted (top left origin instead of OGL bottom left)
;; pos-x and pos-y are stored in GUI coordinate system
;; this requires to use gl-x and gl-y accessors from rendering functions
(defmethod gl-x ((w gui-widget))
  (pos-x w))

(defmethod gl-y ((w gui-widget))
  (- *display-height* (pos-y w)))

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
  ;; update widget hierarchy
  (setf (parent-widget c) e)
  (push c (children e)))

(defgeneric render-widget (gui-widget)
  (:documentation "Renders the provided widget on screen."))

(defmethod render-widget :around ((w gui-widget))
  (when (visible w)
    ;; outline the focused widget
    (when (focused w)
      (gl:color 1 1 1 1)
      (gl:disable :texture-2d)
      (gl:with-primitive :line-loop
        (gl:vertex  (gl-x w)
                    (gl-y w))
        (gl:tex-coord 0 0)
        (gl:vertex  (+ (pos-x w) (width w))
                    (gl-y w))
        (gl:tex-coord 1 0)
        (gl:vertex  (+ (pos-x w) (width w))
                    (- (gl-y w) (height w)))
        (gl:tex-coord 1 1)
        (gl:vertex  (pos-x w)
                    (- (gl-y w) (height w)))))
      ;; render current widget
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

;;, Some widget definitions
(defclass gui-label (gui-widget)
  ((text :accessor text :initarg :text :initform "Some text?")
   (font :accessor font :initarg :font)
   (color :accessor color :initform '(1 1 1 1) :initarg :color))
  (:documentation "One line text"))

(defmethod initialize-instance :after ((w gui-label) &key)
  (update-dimensions w))

(defmethod update-dimensions ((w gui-label))
  (setf (width w) (string-width (text w) (font w)))
  (setf (height w) (string-height (text w) (font w))))

(defmethod (setf text) ((txt string) (w gui-label))
  (setf (slot-value w 'text) txt)
 (update-dimensions w))

(defmethod render-widget ((w gui-label))
  (gl:color (first (color w))
            (second (color w))
            (third (color w))
            (fourth (color w)))
  (render-bitmap-string (pos-x w) (- (gl-y w) (height w))
                        (text w) (font w)))

(defclass gui-dynamic-label (gui-label)
  ((update-fun :reader update-fun :initarg :update-fun :initform nil)
   (fmt :reader fmt :initarg :format :initform "~S")
   (update-args :reader update-args :initarg :update-args :initform nil)))

(defmethod update-widget ((w gui-dynamic-label))
  (when (update-fun w)
    (setf (text w) (format nil (fmt w)
                           (apply (update-fun w) (update-args w))))))

(defclass gui-window (gui-widget)
  ((title :reader title :initarg :title))
  (:default-initargs))


(defmethod resize ((o gui-window) w h)
  (setf (width o) w)
  (setf (height o) h))

(defmethod render-widget ((w gui-window))
  (if (equal (gui-focus *gui*) w)
      (gl:color 0 0 0 0.75)
      (gl:color 0 0 0 0.6))
  (gl:disable :texture-2d)
  (gl:with-primitive :quads
    (gl:tex-coord 0 1)
    (gl:vertex  (pos-x w)
                (gl-y w))
    (gl:tex-coord 0 0)
    (gl:vertex  (+ (pos-x w) (width w))
                (gl-y w))
    (gl:tex-coord 1 0)
    (gl:vertex  (+ (pos-x w) (width w))
                (- (gl-y w) (height w)))
    (gl:tex-coord 1 1)
    (gl:vertex  (pos-x w)
                (- (gl-y w) (height w)))))

(defclass gui-text-input (gui-widget)
  ((input :accessor input :initform '() :initarg :input)
   (action :accessor action :initform nil :initarg :action)
   (prefix :accessor prefix :initform "" :initarg :prefix))
  (:default-initargs))

(defmethod initialize-instance :after ((w gui-text-input) &key)
  (setf (height w) (string-height (prefix w)))
  (setf (width w) (string-width (prefix w))))

;; setup keymap
(key-handler gui-text-input (#\Return :press)
    (when (action it)
      (funcall (action it) it)))
(key-handler gui-text-input (#\Backspace :press)
    (when (input it)
      (decf (width it) *font-width*)
      (pop (input it))))
(key-handler gui-text-input (nil :press)
    (incf (width it) *font-width*)
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
   (texture :accessor gui-button-texture :initform nil :initarg :texture)
   (action :accessor action :initform nil :initarg :action))
  (:default-initargs))

(defmethod initialize-instance :after ((w gui-button) &key action)
  (format t "Creating gui-button with :action ~S~%"
          action)
  (setf (height w) (string-height (text w)))
  (setf (width w) (string-width (text w))))

(key-handler gui-button (#\Return :press)
    (when (action it)
      (funcall (action it) it)))

(button-handler gui-button :mouse (:left-button :press)
    (when (action it)
      (funcall (action it) it)))

(defmethod show ((w gui-button))
  (add-input-handler w)
  (call-next-method))

(defmethod hide ((w gui-button))
  (remove-input-handler w)
  (call-next-method))

(defmethod render-widget ((w gui-button))
  (gl:color 1 1 1 1)
  (render-bitmap-string (pos-x w) (- (gl-y w) (height w))
                        (text w)))

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