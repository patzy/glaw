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
      (make-image :data (imago:image-pixels img)
                  :bpp (imago::pixel-size img)
                  :width (imago:image-width img)
                  :height (imago:image-height img))))
  ;; unload not needed?
)


;; texture asset
(defasset :texture
  ;; load
  (lambda (filename)
    (let ((img (imago:read-image (namestring filename))))
      (format t "Loaded: ~S~%" img)
      (create-texture (imago:image-width img) (imago:image-height img)
                      (imago::pixel-size img) (translate-image-pixels img))))
  ;; unload
  'glaw:destroy-texture)


;; font asset
;; XXX: assumes 256x256 image
(defasset :bitmap-font
  ;; load
  (lambda (filename)
    (let ((img (imago:read-image (namestring filename))))
      (format t "Loaded: ~S~%" img)
      (let ((tex (create-texture (imago:image-width img) (imago:image-height img)
                                 (imago::pixel-size img) (translate-image-pixels img))))
        (create-bitmap-font tex 13 16))))
  ;; unload
  (lambda (font)
    (glaw:destroy-texture (glaw::font-texture font))
    (glaw:destroy-bitmap-font font)))
