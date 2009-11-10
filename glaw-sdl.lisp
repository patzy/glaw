(defpackage :glaw-sdl
  (:use #:cl)
  (:export
     #:translate-key
     #:translate-mouse-button))

(in-package #:glaw-sdl)

;; input
(defun translate-key (key code)
  (case key
    (:sdl-key-up :key-up)
    (:sdl-key-down :key-down)
    (:sdl-key-left :key-left)
    (:sdl-key-right :key-right)
    (:sdl-key-tab #\Tab)
    (:sdl-key-return #\Return)
    (:sdl-key-escape #\Esc)
    (:sdl-key-space #\Space)
    (:sdl-key-backspace #\Backspace)
    (otherwise (coerce code 'character))))

(defun translate-mouse-button (button)
  (case button
    (1 :left-button)
    (2 :middle-button)
    (3 :right-button)
    (4 :wheel-up)
    (5 :wheel-down)))

;; image-resource
(defmethod glaw:load-resource ((type (eql :image)) filename)
  ;; FIXME: missing resize to next power of two
  (let ((image (sdl-image:load-image filename)))
    (sdl-base::with-pixel (pix (sdl:fp image))
      (glaw:make-image-resource :identifier filename
                                :data (sdl-base::pixel-data pix)
                                :bpp (sdl-base::pixel-bpp pix)
                                :width (sdl:width image)
                                :height (sdl:height image)))))

(defmethod glaw:free-resource ((resource glaw:image-resource))
  ;; TODO: free resource
  nil)
