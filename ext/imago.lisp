(defpackage :glaw-imago
  (:use #:cl #:glaw)
  (:export))

(in-package #:glaw-imago)

(defun translate-image-pixels (img)
  "Translate IMAGO two-dimensional pixel array into a single dimension one."
  (let* ((width (imago:image-width img))
         (height (imago:image-height img))
         (pixels (imago:image-pixels img))
         ;;(bpp (imago::pixel-size img))
         (res (make-array (* width height 4) :fill-pointer 0 :element-type '(unsigned-byte 8))))
    (loop for y below height do
         (loop for x below width do
              (let ((pixel (aref pixels y x)))
                (vector-push (imago:color-red pixel) res)
                (vector-push (imago:color-green pixel) res)
                (vector-push (imago:color-blue pixel) res)
                (vector-push (imago:color-alpha pixel) res))))
    res))

;; image asset
(defasset :image
  ;; load
  (lambda (filename)
    (let ((img (imago:read-image (namestring filename))))
      (make-image :data (translate-image-pixels img)
                  :bpp (imago::pixel-size img)
                  :width (imago:image-width img)
                  :height (imago:image-height img))))
  ;; unload not needed?
)


;; texture asset
(defasset :texture
  ;; load
  (lambda (filename)
    ;; XXX: textures are bottom-left origin
    (let ((img (imago:flip nil (imago:read-image (namestring filename)) :horizontal)))
      (create-texture (imago:image-width img) (imago:image-height img)
                      (imago::pixel-size img) (translate-image-pixels img))))
  ;; unload
  'glaw:destroy-texture)


;; font asset
(defasset :fixed-bitmap-font
  ;; load
  (lambda (filename)
    (let ((img (imago:read-image (namestring filename))))
      (let* ((tex (create-texture (imago:image-width img) (imago:image-height img)
                                  (imago::pixel-size img) (translate-image-pixels img)))
             (fnt (create-font tex)))
        (loop for i below 256
             for cx = (/ (mod i 16.0) 16.0)
             for cy = (/ (truncate (/ i 16)) 16.0) do
             (font-set-glyph-data fnt i cx cy 0.0625 0.0625 16))
        (font-build-cache fnt)
        fnt)))
  ;; unload
  (lambda (font)
    (destroy-texture (glaw::font-texture font))
    (destroy-font font)))
