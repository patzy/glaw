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
                         (:file "texture")
                         (:file "screens")
                         (:file "text")
                         (:file "tilemap")
                         (:file "particles")
                         (:file "pathfinding")))))
