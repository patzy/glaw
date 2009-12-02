;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem glaw-examples
  :depends-on (glaw glaw-sdl)
  :serial t
  :components
  ((:file "glaw-examples")
   (:module "examples"
            :components ((:file "gui")
                         (:file "sprites")))))
