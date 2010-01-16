(defpackage :glaw-sdl
  (:use #:cl #:glaw)
  (:export
     #:translate-key
     #:translate-mouse-button))

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


;; font asset
;; XXX: assumes 256x256 image
(defasset :bitmap-font
  ;; load
  (lambda (filename)
    (let ((img-surf (sdl-image:load-image filename)))
      (sdl-base::with-pixel (pix (sdl:fp img-surf))
        (let ((tex (create-texture (sdl:width img-surf) (sdl:height img-surf)
                                   (sdl-base::pixel-bpp pix) (sdl-base::pixel-data pix))))
          (create-bitmap-font tex 13 16)))))
  ;; unload
  'glaw:destroy-bitmap-font)