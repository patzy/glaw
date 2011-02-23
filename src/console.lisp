(in-package #:glaw)

;;; Interpreter
(defgeneric interpreter-eval (terp input-str)
  (:documentation
   "Evaluate the provided input string, returns T if input is incomplete, NIL otherwise."))

(defgeneric interpreter-push (terp line)
  (:documentation
     "Adds line to current interpreter input.
Returns T if more input is required, NIL if input was processed in some way."))

(defgeneric interpreter-reset-input (terp)
  (:documentation "Cancel any pending input."))

(defgeneric interpreter-output (terp)
  (:documentation "Returns latest interpreter output string."))

;; Commands + CL multi-line input interpreter
(defclass interpreter ()
    ((output :accessor interpreter-output :initform "")
     (input-lines :accessor interpreter-input-lines :initform (list))
     (allow-lisp :accessor interpreter-allow-lisp :initform nil :initarg :allow-lisp)
     (commands :accessor interpreter-commands :initform (make-hash-table :test #'equal))))

(defun make-command (&key func args (help "No help available"))
  (list func args help))

(defun command-func (cmd)
  (first cmd))

(defun command-args (cmd)
  (second cmd))

(defun command-help (cmd)
  (third cmd))

(defmethod interpreter-command (terp name)
  (gethash name (interpreter-commands terp)))

(defmethod interpreter-add-command (terp name func &rest args)
  (setf (gethash name (interpreter-commands terp))
        (make-command :func func :args args)))

(defmethod interpreter-input-string (terp)
  (reduce (lambda (x y) (concatenate 'string x y)) (interpreter-input-lines terp)))

(defmethod interpreter-reset-input (terp)
  (setf (interpreter-input-lines terp) (list)))

(defmethod interpreter-eval-command (terp cmd &rest args)
  (let ((cmd (interpreter-command terp cmd)))
    (if cmd
        (prog1 t (apply (command-func cmd) (or args (command-args cmd))))
        (format t "~A: no such command.~%" cmd))))

(defmethod interpreter-eval (terp input-string)
  (case (aref input-string 0)
    (#\( (if (interpreter-allow-lisp terp)
             (let ((obj (handler-case (read-from-string input-string)
                          (end-of-file (err) nil)
                          (error (err) nil))))
               (if obj
                   (handler-case (format t "~A~%" (eval obj))
                     (error (err) (format t "Error: ~A~%" err)))
                   t))
             (format t "Lisp code not allowed.~%")))
    (t (apply #'interpreter-eval-command terp (split-string input-string #\Space)))))

(defmethod interpreter-push (terp line)
  (let ((res nil))
    (setf (interpreter-input-lines terp)
          (append (interpreter-input-lines terp) (list line)))
    (setf (interpreter-output terp)
          (with-output-to-string (s)
            (let ((*standard-output* s))
              (if (interpreter-eval terp (interpreter-input-string terp))
                  (setf res t)
                  (progn (interpreter-reset-input terp)
                         (setf res nil))))))
    res))


;;; Basic text console
(defstruct (text-console
             (:constructor make-text-console (&key (terp (make-instance 'interpreter))
                                                   (ps1 "> ")
                                                   (ps2 "... ")
                                                   (prompt ps1))))
  terp
  ps1 ps2 prompt)

(defun text-console-repl (console)
  (loop while t
       do (let* ((line (read-line))
                 (res (interpreter-push (text-console-terp console) line)))
            (if res
                (setf (text-console-prompt console) (text-console-ps2 console))
                (setf (text-console-prompt console) (text-console-ps1 console)))
            (format t "~A" (interpreter-output (text-console-terp console))))))

;;; In-game console
(defun char-list->string (char-list)
  (apply #'concatenate 'string (reverse char-list)))

(defun string->char-list (string)
  (reverse (loop for c across string
              collect c)))

(defstruct graphic-console
  (input (list))
  (prompt "> ")
  (buffer (list))
  (terp (make-instance 'interpreter :allow-lisp t)))

(defmethod on-key ((it graphic-console) (key (eql :return)) (key-state (eql :press)) keycode string)
  (let* ((text (char-list->string (graphic-console-input it)))
         (res (interpreter-push (graphic-console-terp it) text)))
    (setf (graphic-console-input it) '())
    (push text (graphic-console-buffer it))
    (if res
        (setf (graphic-console-prompt it) "... ")
        (progn (setf (graphic-console-prompt it) "> ")
               (push (concatenate 'string "=> "
                                  (interpreter-output (graphic-console-terp it)))
                     (graphic-console-buffer it))))))

(defmethod on-key ((it graphic-console) (key (eql :backspace)) (key-state (eql :press)) keycode
                   string)
  (pop (graphic-console-input it)))

(defmethod on-key ((it graphic-console) key (key-state (eql :press)) keycode string)
  (push string (graphic-console-input it)))

(defun render-console (console font &optional (x 0) (y 0) (w *display-width*) (h *display-height*))
  (gl:color 0 0 0 0.6)
  (gl:with-primitive :quads
    (gl:vertex x y)
    (gl:vertex (+ x w) y)
    (gl:vertex (+ x w) (+ y h))
    (gl:vertex x (+ y h)))
  (gl:color .75 .75 .75 1)
  (let ((nb-lines (round (/ h (font-line-height font))))
        (lines (mapcan (lambda (item) (string-wrap font item w)) (graphic-console-buffer console)))
        (line-x x)
        (line-y (- (+ y h) (font-line-height font))))
    (loop for i in (last (reverse lines) (1- nb-lines)) do
         (setf line-y (render-wrapped-string line-x line-y w font i))
       finally (gl:color 1 1 1 1)
         (render-wrapped-string line-x line-y w font
                      (concatenate 'string (graphic-console-prompt console)
                                   (char-list->string (graphic-console-input console)))))))

