(in-package #:glaw)

;;; Input compatibility layer
;; Since we're defining some default input handlers for the GUI stuff
;; we need to make sure received input values are the right ones.
;; Use the following translation function for mouse button events.
;; Also, make sure to provide :press or :release for key and button states
(defun translate-mouse-button (button)
  (case button
    (1 :left-button)
    (2 :middle-button)
    (3 :right-button)
    (4 :wheel-up)
    (5 :wheel-down)
    (otherwise button)))

(defstruct gui
  (default-font nil)
  (view (create-2d-view 0 0 *display-width* *display-height*))
  (widgets '())) ;; last one has focus

(defun create-gui (default-font)
  (let ((gui (make-gui :default-font default-font)))
    (add-input-handler gui)
    gui))

(defun destroy-gui (gui)
  (remove-input-handler gui)
  (setf gui nil))

;; GUI singleton
(defvar *gui* nil)

(defun init-gui (font)
  "Init GUI singleton using the provided bitmap font."
  (setf *gui* (create-gui font)))

(defun shutdown-gui ()
  "Shutdown GUI singleton."
  (destroy-gui *gui*))

(defun render-gui (&optional (gui *gui*))
  (set-view-2d (gui-view gui))
  (dolist (w (reverse (gui-widgets gui)))
    (render-widget w)
    (when (and (gui-widget-visible w) (focused w))
      (select-texture nil)
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


(defun update-gui (&optional (gui *gui*)
                             (width *display-width*)
                             (height *display-height*))
  (update-2d-view (gui-view gui) 0 0 width height)
  (dolist (w (gui-widgets gui))
    (apply-layout w)))

(defun create-widget (widget-type parent-widget &rest initargs)
  (let ((created-widget (apply 'make-instance widget-type initargs)))
    (if parent-widget
        (add-child-widget parent-widget created-widget)
        (add-child-widget *gui* created-widget))
    created-widget))

(defmethod gui-focus ((it gui))
  (first (gui-widgets it)))

(defmethod (setf gui-focus) (value (it gui))
  (when (gui-focus it)
    (unfocus (gui-focus it)))
  (setf (gui-widgets it)
        (append (list value) (butlast (gui-widgets it))))
  (focus value))

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

(defun gui-widget-at (gui x y)
  (loop for w in (gui-widgets gui)
     for found = (gui-widget-child-at w x y)
     when found
     return found))

(defmethod on-button ((it gui) (device (eql :mouse))
                      btn (btn-state (eql :press)))
  (let ((widget (gui-widget-at it *mouse-x* *mouse-y*)))
    (when widget
      (gui-widget-mouse-down widget btn))))

(defmethod on-button ((it gui) (device (eql :mouse))
                      btn (btn-state (eql :release)))
  (let ((widget (gui-widget-at it *mouse-x* *mouse-y*)))
    (when widget
      (gui-widget-mouse-up widget btn))))

(let ((entered-widget nil))
  (defmethod on-motion ((it gui) (device (eql :mouse)) dx dy)
    (let ((widget (gui-widget-at it *mouse-x* *mouse-y*)))
      (when widget
        (unless (eq entered-widget widget)
          (when entered-widget
            (gui-widget-mouse-leave entered-widget))
          (gui-widget-mouse-enter widget)
          (setf entered-widget widget))
        (gui-widget-mouse-move widget dx dy)))))

;; GUI widgets
(defclass gui-widget ()
  (;; relative coordinates for absolute position
   ;; may be overriden by layout if not :absolute
   (x :accessor pos-x :initform 0 :initarg :x)
   (y :accessor pos-y :initform 0 :initarg :y)
   ;; [0.0;1.0]: proportional / integer: fixed
   (width :initform 1.0 :initarg :width)
   (height :initform 1.0 :initarg :height)
   ;; client area offset
   (x-off :accessor gui-widget-x-off :initform 0 :initarg :x-off)
   (y-off :accessor gui-widget-y-off :initform 0 :initarg :y-off)
   ;; children layout (:vertical, :horizontal or :absolute)
   (layout :accessor gui-widget-layout :initform :absolute :initarg :layout)
   ;; visual appearance
   (color :accessor gui-widget-color
          :initform (create-color 0.35 0.35 0.35 0.5)
          :initarg :color)
   (texture :accessor gui-widget-texture :initform nil :initarg :texture)
   (visible :accessor gui-widget-visible :initform t :initarg :visible)
   (font :accessor gui-widget-font :initform nil :initarg :font) ;; XXX: move this?
   (focused :accessor focused :initform t)
   (parent :accessor parent-widget :initform nil)
   (children :accessor children :initform '()))
  (:documentation "Base class for all gui components."))

(defmethod width ((it gui-widget))
  (if (<= 0.0 (slot-value it 'width) 1.0)
      (* (slot-value it 'width) (if (parent-widget it)
                                    (- (width (parent-widget it))
                                       (gui-widget-x-off (parent-widget it)))
                                    *display-width*))
      (slot-value it 'width)))

(defmethod (setf width) (value (it gui-widget))
  (setf (slot-value it 'width) value))

(defmethod height ((it gui-widget))
  (if (<= 0.0 (slot-value it 'height) 1.0)
      (* (slot-value it 'height) (if (parent-widget it)
                                     (- (height (parent-widget it))
                                        (gui-widget-y-off (parent-widget it)))
                                     *display-height*))
      (slot-value it 'height)))

(defmethod (setf height) (value (it gui-widget))
  (setf (slot-value it 'height) value))

(defmethod move-widget ((it gui-widget) x y)
  (setf (pos-x it) x (pos-y it) y))

;; Default input methods for static widgets
;; simply forward to parent widget
;; Interactive widgets should override these to provided specific behavior
(defmethod gui-widget-mouse-down ((it gui-widget) btn)
  (when (parent-widget it)
    (gui-widget-mouse-down (parent-widget it) btn)))

(defmethod gui-widget-mouse-up ((it gui-widget) btn)
  (when (parent-widget it)
    (gui-widget-mouse-up (parent-widget it) btn)))

(defmethod gui-widget-mouse-move ((it gui-widget) dx dy)
  (when (parent-widget it)
    (gui-widget-mouse-move (parent-widget it) dx dy)))

(defmethod gui-widget-mouse-enter ((it gui-widget)))
(defmethod gui-widget-mouse-leave ((it gui-widget)))

;; Find the lowest widget in the tree at (x;y)
;; (x;y) are relative to the provided widget
(defun gui-widget-child-at (w x y)
  (dolist (c (children w))
    (let ((found (gui-widget-child-at
                  c
                  (- x (pos-x w) (gui-widget-x-off w))
                  (- y (pos-y w) (gui-widget-y-off w)))))
      (when found
        (return-from gui-widget-child-at found))))
  (when (and (gui-widget-visible w)
             (< (pos-x w) x (+ (pos-x w) (width w)))
             (< (pos-y w) y (+ (pos-y w) (height w))))
    w))

;; gui Y-axis is inverted (top left origin instead of OGL bottom left)
;; widget dimensions are relative to the parent widget in screen coordinates
;; this requires to use gl-x and gl-y accessors from rendering functions
(defmethod gl-x ((w gui-widget))
  (pos-x w))

(defmethod gl-y ((w gui-widget))
  (- *display-height* (pos-y w)))

(defmethod resize ((it gui-widget) w h)
  ;; update widget dimensions
  (setf (width it) w
        (height it) h)
  (apply-layout it))

(defmethod apply-layout ((it gui-widget))
  ;; apply layout of children
  (dolist (c (children it))
    (when (children c)
      (apply-layout c)))
  ;; layout our own children
  (unless (eq (gui-widget-layout it) :absolute)
    (let ((origin 0))
      (dolist (c (children it))
        (when (children c)
          (apply-layout c))
        (case (gui-widget-layout it)
          (:horizontal (progn (setf (pos-x c) origin)
                              ;;(setf (pos-y c) align)
                              (incf origin (width c))))
          (:vertical (progn (setf (pos-y c) origin)
                            ;;(setf (pos-x c) align)
                            (incf origin (height c))))
          (otherwise (error "Invalid layout specification: ~S~%"
                            (gui-widget-layout it))))))))

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

(defmethod add-child-widget ((p gui) (c gui-widget))
  (unless (gui-widget-font c)
    (setf (gui-widget-font c) (gui-default-font p)))
  (when (gui-focus p)
    (unfocus (gui-focus p)))
  (push c (gui-widgets p))
  (focus c))

(defmethod add-child-widget ((e gui-widget) (c gui-widget))
  (unless (gui-widget-font c)
    (setf (gui-widget-font c) (gui-widget-font e)))
  (setf (parent-widget c) e)
  (setf (children e) (append (children e) (list c)))
  (apply-layout e))

(defgeneric render-widget (gui-widget)
  (:documentation "Renders the provided widget on screen."))

(defmethod render-widget ((w gui-widget))
  (set-color (gui-widget-color w))
  (select-texture (gui-widget-texture w))
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
  (when (gui-widget-visible w)
    (call-next-method)
    (gl:with-pushed-matrix
        (gl:translate (+ (gui-widget-x-off w) (pos-x w))
                      (- (+ (gui-widget-y-off w) (pos-y w))) 0)
      (dolist (c (children w))
        (render-widget c)))))

(defgeneric update-widget (gui-widget)
  (:documentation "Do whatever needs to be done to update the state of the
                   widget."))

(defmethod update-widget ((w gui-widget))
  (declare (ignore w)))

(defmethod show ((e gui-widget))
  (unless (gui-widget-visible e)
    (setf (gui-widget-visible e) t)
    (add-input-handler e)))

(defmethod hide ((e gui-widget))
  (when (gui-widget-visible e)
    (setf (gui-widget-visible e) nil)
    (remove-input-handler e)))

;; Some widget definitions
(defclass gui-window (gui-widget)
  ((moveable :accessor gui-window-moveable :initform nil :initarg :moveable)
   (moving :accessor gui-window-moving :initform nil)
   (title :reader gui-window-title :initarg :title))
  (:default-initargs))

(defmethod update-dimensions ((it gui-window))
  (when (and (gui-window-title it) (gui-widget-font it))
    (setf (gui-widget-y-off it) (+ 6 (string-height (gui-widget-font it) (gui-window-title it))))))

(defmethod gui-widget-mouse-down ((it gui-window) (btn (eql :left-button)))
  (setf (gui-window-moving it) t))

(defmethod gui-widget-mouse-up ((it gui-window) (btn (eql :left-button)))
  (setf (gui-window-moving it) nil))

(defmethod gui-widget-mouse-move ((it gui-window) dx dy)
  (when (and (gui-window-moveable it) (gui-window-moving it))
    (incf (pos-x it) dx)
    (incf (pos-y it) dy)))

(defmethod gui-widget-mouse-leave ((it gui-window))
  (setf (gui-window-moving it) nil))

(defmethod render-widget ((w gui-window))
  (call-next-method)
  (set-color/rgb 1 1 1)
  (update-dimensions w) ;; FIXME: move this
  (render-string (gl-x w) (- (gl-y w) 3
                             (string-height (gui-widget-font w)
                                                   (gui-window-title w)))
                         (gui-widget-font w) (gui-window-title w))
  (gl:with-primitive :lines
    (gl:vertex (gl-x w)
               (- (gl-y w) 6 (string-height (gui-widget-font w)
                                            (gui-window-title w))))
    (gl:vertex (+ (gl-x w) (width w))
               (- (gl-y w) 6 (string-height (gui-widget-font w)
                                            (gui-window-title w))))))

(defclass gui-label (gui-widget)
  ((text :accessor text :initarg :text :initform "Some text?")
   (text-color :accessor text-color :initarg :text-color
               :initform (create-color 1 1 1)))
  (:documentation "One line text"))

(defmethod update-dimensions ((w gui-label))
  (setf (width w) (string-width (gui-widget-font w) (text w)))
  (setf (height w) (string-height (gui-widget-font w) (text w))))

(defmethod (setf text) ((txt string) (w gui-label))
  (setf (slot-value w 'text) txt)
 (update-dimensions w))

(defmethod render-widget ((w gui-label))
  (set-color (text-color w))
  (update-dimensions w) ;; FIXME: move this
  (render-string (pos-x w) (- (gl-y w) (height w))
                        (gui-widget-font w) (text w)))

(defclass gui-text-input (gui-widget)
  ((input :accessor input :initform '() :initarg :input)
   (action :accessor action :initform nil :initarg :action))
  (:default-initargs))

(defmethod text ((it gui-widget))
  (coerce (reverse (input it)) 'string))

(defmethod initialize-instance :after ((w gui-text-input) &key)
  (when (and (text w) (gui-widget-font w))
    (setf (height w) (string-height (gui-widget-font w) (text w)))
    (setf (width w) (string-width (gui-widget-font w) (text w)))))

;; setup keymap
;; (key-handler (it gui-text-input) (:return :press)
;;     (when (action it)
;;       (funcall (action it) it)))
;; (key-handler (it gui-text-input) (:backspace :press)
;;     (when (input it)
;;       (decf (width it) (font-width (gui-font *gui*)))
;;       (pop (input it))))
;; (key-handler (it gui-text-input) (nil :press)
;;     (incf (width it) (font-width (gui-font *gui*)))
;;     (push key (input it)))

(defmethod show ((w gui-text-input))
  (add-input-handler w)
  (call-next-method))

(defmethod hide ((w gui-text-input))
  (remove-input-handler w)
  (call-next-method))

(defmethod clear ((w gui-text-input))
  (setf (input w) '())
  (setf (height w) 1)
  (setf (width w) 1))

(defmethod render-widget ((w gui-text-input))
  (gl:color 1 1 1 1)
  (render-string (pos-x w) (- (gl-y w) (height w)) (gui-widget-font w) (text w)))

(defclass gui-button (gui-widget)
  ((text :accessor text :initform "Some text?" :initarg :text)
   (action :accessor action :initform nil :initarg :action)
   (args :accessor gui-button-args :initform nil :initarg :args)
   (pressed :accessor gui-button-pressed :initform nil)
   (pressed-texture :accessor gui-button-pressed-texture :initform nil
                    :initarg :pressed-texture))
  (:default-initargs
    :color (create-color 1 1 1 0.3)))

(defmethod gui-widget-mouse-down ((it gui-button) (btn (eql :left-button)))
  (setf (gui-button-pressed it) t)
  (when (gui-button-pressed-texture it)
    (let ((tex (gui-widget-texture it)))
      (setf (gui-widget-texture it) (gui-button-pressed-texture it))
      (setf (gui-button-pressed-texture it) tex))))

(defmethod gui-widget-mouse-up ((it gui-button) (btn (eql :left-button)))
  (when (and (gui-button-pressed it) (action it))
    (apply (action it) (gui-button-args it)))
  (setf (gui-button-pressed it) nil)
  (when (gui-button-pressed-texture it)
    (let ((tex (gui-widget-texture it)))
      (setf (gui-widget-texture it) (gui-button-pressed-texture it))
      (setf (gui-button-pressed-texture it) tex))))

(defmethod show ((w gui-button))
  (add-input-handler w)
  (call-next-method))

(defmethod hide ((w gui-button))
  (remove-input-handler w)
  (call-next-method))

(defmethod render-widget ((w gui-button))
  (call-next-method)
  (set-color/rgb 1 1 1)
  (render-string (- (+ (pos-x w)
                              (/ (width w) 2.0))
                           (/ (string-width (gui-widget-font w) (text w)) 2.0))
                        (- (gl-y w)
                           (/ (height w) 2.0)
                           (/ (string-height (gui-widget-font w) (text w)) 2.0))
                         (gui-widget-font w) (text w)))

;; (defclass gui-multiline-text (gui-label)
;;   ((nb-lines :accessor nb-lines :initform 10 :initarg :nb-lines)
;;    (expandable :accessor expandable :initform t :initarg :expandable))
;;    (:default-initargs :text '()))

;; (defmethod update-dimensions ((w gui-multiline-text))
;;   (setf (height w) (* (length (text w)) (string-height "" (gui-font *gui*))))
;;   (setf (width w)
;;         (loop for l in (text w)
;;              maximize (string-width l (gui-font *gui*)))))

;; (defmethod add-line ((w gui-multiline-text) (line string))
;;   (push line (text w))
;;   (unless (expandable w)
;;     (when (> (length (text w)) (nb-lines w))
;;       (setf (text w) (butlast (text w)))))
;;   (update-dimensions w))

;; (defmethod render-widget ((w gui-multiline-text))
;;   (let ((x (gl-x w))
;;         (y (- (gl-y w) (string-height "" (gui-font *gui*)))))
;;   (dolist (txt (reverse (text w)))
;;     (render-string x y txt)
;;     (decf y (string-height "" (gui-font *gui*))))))


(defclass gui-slider (gui-widget)
  ((min :accessor gui-slider-min :initform 0 :initarg :min)
   (max :accessor gui-slider-max :initform 100 :initarg :max)
   (step :accessor gui-slider-step :initform 1 :initarg :step)
   (value :accessor gui-slider-value :initform 50 :initarg :value)
   (sliding :accessor gui-slider-sliding :initform nil)))

(defmethod gui-slider-step-up ((w gui-slider))
  (when (> (incf (gui-slider-value w) (gui-slider-step w)) (gui-slider-max w))
    (setf (gui-slider-value w) (gui-slider-max w))))

(defmethod gui-slider-step-down ((w gui-slider))
  (when (< (decf (gui-slider-value w) (gui-slider-step w)) (gui-slider-min w))
    (setf (gui-slider-value w) (gui-slider-min w))))


(defmethod gui-slider-incf ((w gui-slider) value)
  (when (> (incf (gui-slider-value w) value) (gui-slider-max w))
    (setf (gui-slider-value w) (gui-slider-max w))))

(defmethod gui-slider-decf ((w gui-slider) value)
  (when (< (decf (gui-slider-value w) value) (gui-slider-min w))
    (setf (gui-slider-value w) (gui-slider-min w))))


(defmethod render-widget ((w gui-slider))
  (gl:with-primitive :lines
    (gl:vertex (gl-x w)
               (- (gl-y w) (/ (height w) 2.0)))
    (gl:vertex (+ (gl-x w) (width w))
               (- (gl-y w) (/ (height w) 2.0)))
    (let ((slider-scale (/ (width w) (- (gui-slider-max w)
                                        (gui-slider-min w)))))
      (gl:vertex (+ (gl-x w) (* slider-scale (gui-slider-value w)))
                 (gl-y w))
      (gl:vertex (+ (gl-x w) (* slider-scale (gui-slider-value w)))
                 (- (gl-y w) (height w))))))

(defmethod gui-widget-mouse-down ((it gui-slider) (btn (eql :left-button)))
  (setf (gui-slider-sliding it) t))

(defmethod gui-widget-mouse-up ((it gui-slider) (btn (eql :left-button)))
  (setf (gui-slider-sliding it) nil))

(defmethod gui-widget-mouse-move ((it gui-slider) dx dy)
  (declare (ignore dy))
  (when (gui-slider-sliding it)
    (gui-slider-incf it dx)))

(defmethod gui-widget-mouse-leave ((it gui-slider))
  (setf (gui-slider-sliding it) nil))


(defclass gui-gauge (gui-widget)
  ((value :accessor gui-gauge-value :initform 0.5 :initarg :value)
   (fill-color :accessor gui-gauge-fill-color
               :initform (create-color 1.0 1.0 1.0 0.75) :initarg :fill-color)))

(defmethod render-widget ((w gui-gauge))
  (call-next-method)
  (set-color (gui-gauge-fill-color w))
  (gl:with-primitive :quads
    (gl:vertex (gl-x w) (gl-y w))
    (gl:vertex (+ (* (gui-gauge-value w) (width w)) (gl-x w))
               (gl-y w))
    (gl:vertex (+ (* (gui-gauge-value w) (width w)) (gl-x w))
               (- (gl-y w) (height w)))
    (gl:vertex (gl-x w) (- (gl-y w) (height w)))))
