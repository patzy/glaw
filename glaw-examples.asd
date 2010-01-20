;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem glaw-examples
  :depends-on (glaw glop glaw-imago)
  ;;:depends-on (glaw glaw-sdl)
  :serial t
  :components
  ((:file "glaw-examples")
   (:module "examples"
            :components ((:file "gui")
                         (:file "sprites")
                         (:file "screens")
                         (:file "text")
                         (:file "particles")))))
