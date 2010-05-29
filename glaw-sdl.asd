;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem glaw-sdl
  :depends-on (:glaw :lispbuilder-sdl :lispbuilder-sdl-image)
  :components
  ((:module "ext"
            :components ((:file "sdl")))))

