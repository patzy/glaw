(in-package #:glaw)

;;; Input console

(defclass console ()
  ((input :accessor input :initform '() :initarg :input)
   (height :accessor height :initform 10);; in number of lines
   (buffer :accessor buffer :initform '("Input console:"))))

(key-handler console (#\Return :press)
   (let* ((text (char-list->string (input it))))
     (push text (buffer it))
     (setf (input it) '())
     (push (format nil "~S" (handler-case (eval (read-from-string text))
                              (error (e) (format nil "~A" e))))
           (buffer it))))

(key-handler console (#\Esc :press)
   (toggle-console))

(key-handler console (#\Backspace :press)
    (pop (input it)))

(key-handler console (nil :press)
    (push key (input it)))

(defun char-list->string (char-list)
  (coerce (reverse char-list)
          'string))

(defun string->char-list (string)
  (reverse (loop for c across string
              collect c)))

(defmethod render-widget ((co console))
  (gl:color 0 0 0 0.6)
  (gl:disable :texture-2d)
  (gl:with-primitive :quads
    (gl:vertex 0
               (- *display-height*
                  (* (height co) 16))
               0)
    (gl:vertex *display-width*
               (- *display-height*
                  (* (height co) 16))
               0)
    (gl:vertex *display-width*
               *display-height*
               0)
    (gl:vertex 0
               *display-height*
               0))
  (gl:color 1 1 1 1)
  (format-at  (- *display-width* 150)
              (- *display-height* 25)
              "~D" (current-fps))
  (gl:with-pushed-matrix
      (gl:translate 0
                    (- *display-height*
                       (* (height co) 16))
                     0)
    (gl:color 1 1 1 1)
    ;; Render buffer content
    ;; one line per item
    (let ((x 0)
          (y 16))
      (loop for i in (buffer co)
         do (progn (render-bitmap-string x y i)
                   (incf y 16))
         finally (render-bitmap-string
                  x 0
                  (concatenate 'string "=> "
                               (char-list->string (append (input co)))))))))
