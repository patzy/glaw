(in-package #:glaw)

(defmacro define-constant (name value &optional doc)
  `(defconstant ,name (if (boundp ',name) (symbol-value ',name) ,value)
     ,@(when doc (list doc))))

(defun ensure-adjustable (vec)
  (make-array (length vec) :fill-pointer (length vec) :initial-contents vec
              :adjustable t))

(defun nearest-power-of-two (size)
  (let ((next-size 1))
    ;; compute next power of two
    (loop while (< next-size size)
       do (setf next-size (ash next-size 1)))
    ;; compute previous one
    (let ((prev-size (ash next-size -1)))
      ;; take the nearest
      (if (< (abs (- next-size size))
             (abs (- prev-size size)))
          next-size
          prev-size))))

(defun elapsed-time-since (start-time &optional (resolution-unit nil))
  "Return elapsed time between provided start time and now."
    (let ((time-units (- (get-internal-real-time)
                         start-time)))
      (if resolution-unit
          (convert-time time-units (or resolution-unit 1.0))
          time-units)))

(defun convert-time (time &optional (resolution-unit 1.0))
  "Converts realtime to seconds with optional scale factor."
  (/ (/ time internal-time-units-per-second) resolution-unit))

(defun sqr-dist (x0 y0 x1 y1)
  (+ (* (- x1 x0) (- x1 x0))
     (* (- y1 y0) (- y1 y0))))

(defun coords-overlap-p (a b c d)
  (or (= a c) (= b d)
      (< c a d b) (< a c b d)
      (< a c d b) (< c a b d)))

;;; Random
(defun random-between (min max)
  (+ min (random (- max min))))

(defun random-nth (seq)
  (elt seq (random-between 0 (length seq))))

(defun nshuffle (seq)
  (loop for i downfrom (1- (length seq)) to 1
        do (rotatef (elt seq (random (1+ i)))
                    (elt seq i)))
  seq)

(defun shuffle (seq)
  (let ((seq-copy (subseq seq 0)))
    (nshuffle seq-copy)))

;;; List manipulation
(defun rotate-list-right (lst n)
   (if (zerop n)
       lst
       (rotate-list-right (append (last lst) (butlast lst))
                          (- n 1))))

(defun rotate-list-left (lst n)
   (if (zerop n)
       lst
       (rotate-list-left (append (rest lst) (list (first lst)))
                         (- n 1))))

(defun rotate-list (lst &key (direction :left) (distance 1))
  ;; check distance is positive
  (when (< distance 0)
    (if (eq direction :left)
        (setf direction :right)
        (setf direction :left))
    (setf distance (- distance)))
  ;; rotate the list
  (if (eq direction :left)
      (rotate-list-left  lst distance)
      (rotate-list-right lst distance)))

(defun list-insert (lst1 lst2 insert-pos)
  (let ((lst-start (butlast lst1 (- (length lst1) insert-pos)))
        (lst-end (nthcdr insert-pos lst1)))
    (append lst-start
            lst2
            lst-end)))

(defun list-from (item nb-items)
  (loop for i from 0 to nb-items
       collect item))

;; profiling
(defmacro with-profiling (fmt &body body)
  `(let ((start-time (get-internal-real-time)))
     ,@body
     (format t ,fmt (* 1.0 ( / (- (get-internal-real-time) start-time)
                              internal-time-units-per-second)))))

;; binary files (stolen from imago)
(defun read-integer (stream size &optional big-endian)
  (loop with number = 0
        for i below size
        as byte = (read-byte stream)
        do (if big-endian
               (setf number (logior (ash number 8) byte))
               (incf number (ash byte (* i 8))))
        finally (return number)))

;; string manipulation
(defun split-string (string delim)
  "Split the provided string and returns a list."
  (loop for i = 0 then (1+ j)
        as j = (position delim string :start i)
        when (not (zerop (length (subseq string i j)))) ;remove last elt
        collect (subseq string i j)
        while j))

(defun file->strings (path)
  (with-open-file (s path)
    (loop for line = (read-line s nil nil)
         while line
         collect line)))

;; pathnames (from PCL: http://gigamonkeys.com/book/practical-a-portable-pathname-library.html)
(defun component-present-p (value)
  (and value (not (eql value :unspecific))))

(defun directory-pathname-p  (p)
  (and
   (not (component-present-p (pathname-name p)))
   (not (component-present-p (pathname-type p)))
   p))

(defun pathname-as-directory (name)
  (let ((pathname (pathname name)))
    (when (wild-pathname-p pathname)
      (error "Can't reliably convert wild pathnames."))
    (if (not (directory-pathname-p name))
      (make-pathname
       :directory (append (or (pathname-directory pathname) (list :relative))
                          (list (file-namestring pathname)))
       :name      nil
       :type      nil
       :defaults pathname)
      pathname)))

(defun file-exists-p (pathname)
  #+(or sbcl lispworks openmcl)
  (probe-file pathname)

  #+(or allegro cmu)
  (or (probe-file (pathname-as-directory pathname))
      (probe-file pathname))

  #+clisp
  (or (ignore-errors
        (probe-file (pathname-as-file pathname)))
      (ignore-errors
        (let ((directory-form (pathname-as-directory pathname)))
          (when (ext:probe-directory directory-form)
            directory-form))))

  #-(or sbcl cmu lispworks openmcl allegro clisp)
  (error "file-exists-p not implemented"))

;; misc.
(defun key-value (key lst)
  (cadr (member key lst)))

(defun set-key-value (key lst value)
  (setf (cadr (member key lst)) value))

;; Debugging
(defvar *debug-stream* *standard-output*)

(defun dformat (fmt &rest args)
  (apply #'format *debug-stream* fmt args))

;; Conditions
(define-condition glaw-error (error)
  () (:documentation "Any glaw specific error should inherit this."))

(define-condition not-implemented (glaw-error)
  () (:documentation "Unimplemented."))

;; Executable creation
(defun make-executable (name startup-fun &optional (documentation name))
  #+clisp
  (ext:saveinitmem name :init-function startup-fun :executable t
                   :keep-global-handlers t :norc t :documentation documentation)
  #+sbcl
  (sb-ext:save-lisp-and-die name :toplevel startup-fun :executable t)
  #+ccl
  (ccl:save-application name :toplevel-function startup-fun
                        :prepend-kernel t)
  #-(or clisp sbcl ccl)(error 'not-implemented)
)