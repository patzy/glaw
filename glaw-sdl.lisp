(defpackage :glaw-sdl
  (:use #:cl #:glaw)
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
    (otherwise (unless (zerop code) (coerce code 'character)))))

(defun translate-mouse-button (button)
  (case button
    (1 :left-button)
    (2 :middle-button)
    (3 :right-button)
    (4 :wheel-up)
    (5 :wheel-down)))

;; image asset
(defasset :image
  ;; load
  (lambda (filename)
    (let ((img-surf (sdl-image:load-image filename)))
      (sdl-base::with-pixel (pix (sdl:fp img-surf))
        (make-image :data (sdl-base::pixel-data pix)
                    :bpp (sdl-base::pixel-bpp pix)
                    :width (sdl:width img-surf)
                    :height (sdl:height img-surf)))))
  ;; unload
  'sdl::free-surface)


;; texture asset
(defasset :texture
  ;; load
  (lambda (filename)
    (let ((img-surf (sdl-image:load-image filename)))
      (sdl-base::with-pixel (pix (sdl:fp img-surf))
        (create-texture (sdl:width img-surf) (sdl:height img-surf)
                        (sdl-base::pixel-bpp pix) (sdl-base::pixel-data pix)))))
  ;; unload
  'glaw:destroy-texture)