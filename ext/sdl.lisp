(defpackage :glaw-sdl
  (:use #:cl #:glaw)
  (:export
     #:translate-keysym))

(in-package #:glaw-sdl)

;; input
(defun translate-keysym (keysym)
  "Translate a subset of SDL keysym to GLAW compatible keysyms."
  (case keysym
    (:sdl-key-up :up)
    (:sdl-key-down :down)
    (:sdl-key-left :left)
    (:sdl-key-right :right)
    (:sdl-key-tab :tab)
    (:sdl-key-return :return)
    (:sdl-key-escape :escape)
    (:sdl-key-space :space)
    (:sdl-key-backspace :backspace)
    (:sdl-key-a :a)
    (:sdl-key-b :b)
    (:sdl-key-c :c)
    (:sdl-key-d :d)
    (:sdl-key-e :e)
    (:sdl-key-f :f)
    (:sdl-key-g :g)
    (:sdl-key-h :h)
    (:sdl-key-i :i)
    (:sdl-key-j :j)
    (:sdl-key-k :k)
    (:sdl-key-l :l)
    (:sdl-key-m :m)
    (:sdl-key-n :n)
    (:sdl-key-o :o)
    (:sdl-key-p :p)
    (:sdl-key-q :q)
    (:sdl-key-r :r)
    (:sdl-key-s :s)
    (:sdl-key-t :t)
    (:sdl-key-u :u)
    (:sdl-key-v :v)
    (:sdl-key-w :w)
    (:sdl-key-x :x)
    (:sdl-key-y :y)
    (:sdl-key-z :z)
    (:sdl-key-1 :1)
    (:sdl-key-2 :2)
    (:sdl-key-3 :3)
    (:sdl-key-4 :4)
    (:sdl-key-5 :5)
    (:sdl-key-6 :6)
    (:sdl-key-7 :7)
    (:sdl-key-8 :8)
    (:sdl-key-9 :9)))

;; image asset
(defasset :image '("png" "jpg" "bmp" "gif" "tga" "pnm" "pbm"
                   "pgm" "ppm" "xpm" "xcf" "pcx" "tif" "lbm")
  ;; load
  (lambda (&key filename &allow-other-keys)
    (sdl:with-init (sdl:sdl-init-video)
      (let ((img-surf (sdl-image:load-image filename)))
        (sdl-base::with-pixel (pix (sdl:fp img-surf))
          (make-image :data (cffi::foreign-array-to-lisp (sdl-base::pixel-data pix)
                                  (list :array :unsigned-char (* (sdl-base::pixel-bpp pix)
                                                                 (sdl:width img-surf)
                                                                 (sdl:height img-surf))))
                      :bpp (sdl-base::pixel-bpp pix)
                      :width (sdl:width img-surf)
                      :height (sdl:height img-surf))))))
  ;; unload
)


;; texture asset
(defasset :texture '("png" "jpg" "bmp" "gif" "tga" "pnm" "pbm"
                     "pgm" "ppm" "xpm" "xcf" "pcx" "tif" "lbm")
  ;; load
  (lambda (&key filename &allow-other-keys)
    (sdl:with-init (sdl:sdl-init-video)
      (let ((img-surf (sdl-image:load-image filename)))
        (sdl-base::with-pixel (pix (sdl:fp img-surf))
          ;; XXX: loaded data is top-left origin while textures are bottom-left
          (loop with start = 0
               with end = (1- (sdl:height img-surf))
               with bpp = (sdl-base::pixel-bpp pix)
               with width = (sdl:width img-surf)
               while (< start end)
               do (progn (loop for x below (* width bpp)
                              with color = (random 255)
                            do (let* ((start-index (+ x (* start width bpp)))
                                      (end-index (+ x (* end width bpp)))
                                      (data (sdl-base::pixel-data pix))
                                      (tmp (cffi:mem-aref data :unsigned-char start-index)))
                                 (setf (cffi:mem-aref data :unsigned-char start-index)
                                       (cffi:mem-aref data :unsigned-char end-index)
                                       (cffi:mem-aref data :unsigned-char end-index)
                                       tmp)))
                         (incf start)
                         (decf end)))
          (create-texture (sdl:width img-surf) (sdl:height img-surf)
                          (sdl-base::pixel-bpp pix)
                          (cffi::foreign-array-to-lisp (sdl-base::pixel-data pix)
                                (list :array :unsigned-char (* (sdl-base::pixel-bpp pix)
                                                               (sdl:width img-surf)
                                                               (sdl:height img-surf)))))))))
  ;; unload
  'glaw:destroy-texture)


;; font asset
;; XXX: assumes 256x256 image
(defasset :font '("png" "jpg" "bmp" "gif" "tga" "pnm" "pbm"
                  "pgm" "ppm" "xpm" "xcf" "pcx" "tif" "lbm")
  ;; load
  (lambda (filename &rest props)
    (sdl:with-init (sdl:sdl-init-video)
      (let ((img-surf (sdl-image:load-image filename)))
        (sdl-base::with-pixel (pix (sdl:fp img-surf))
          (let* ((tex (create-texture (sdl:width img-surf) (sdl:height img-surf)
                                      (sdl-base::pixel-bpp pix) (sdl-base::pixel-data pix)))
                 (fnt (create-font tex)))
            (loop for i below 256
               for cx = (/ (mod i 16.0) 16.0)
               for cy = (/ (truncate (/ i 16)) 16.0) do
                 (font-set-glyph-data fnt i cx cy 0.0625 0.0625 16))
            (font-build-cache fnt)
            fnt)))))
  ;; unload
  (lambda (font)
    (glaw:destroy-texture (glaw::font-texture font))
    (glaw:destroy-font font)))