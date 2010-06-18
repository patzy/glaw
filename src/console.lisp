(in-package #:glaw)

;;; In-game console

(defun char-list->string (char-list)
  (apply #'concatenate 'string (reverse char-list)))

(defun string->char-list (string)
  (reverse (loop for c across string
              collect c)))

(defstruct console
  (input '())
  (prompt "> ")
  (buffer '("Input console:"))
  (parser (lambda (text)
            (handler-case (format nil "~S" (eval (read-from-string text)))
              (error (e) (format nil "ERROR" e))))))

(defmethod on-key ((it console) (key (eql :return)) (key-state (eql :press)) keycode string)
  (let ((text (char-list->string (console-input it))))
    (setf (console-input it) '())
    (push text (console-buffer it))
    (push (concatenate 'string "=> " (funcall (console-parser it) text)) (console-buffer it))))

(defmethod on-key ((it console) (key (eql :backspace)) (key-state (eql :press)) keycode string)
  (pop (console-input it)))

(defmethod on-key ((it console) key (key-state (eql :press)) keycode string)
  (push string (console-input it)))

(defun render-console (console font &optional (x 0) (y 0) (w *display-width*) (h *display-height*))
  (gl:color 0 0 0 0.6)
  (gl:with-primitive :quads
    (gl:vertex x y)
    (gl:vertex (+ x w) y)
    (gl:vertex (+ x w) (+ y h))
    (gl:vertex x (+ y h)))
  (gl:color .75 .75 .75 1)
  (let ((nb-lines (round (/ h (font-line-height font))))
        (lines (mapcan (lambda (item) (string-wrap font item w)) (console-buffer console)))
        (line-x x)
        (line-y (- (+ y h) (font-line-height font))))
    (loop for i in (last (reverse lines) (1- nb-lines)) do
         (setf line-y (render-wrapped-string line-x line-y w font i))
       finally (gl:color 1 1 1 1)
         (render-wrapped-string line-x line-y w font
                      (concatenate 'string (console-prompt console)
                                   (char-list->string (console-input console)))))))

