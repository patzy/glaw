;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem glaw-examples
  :depends-on (:glaw :glop :glaw-imago)
  ;;:depends-on (glaw glaw-sdl)
  :components
  ((:module "examples"
            :serial t
            :components ((:file "examples")
                         (:file "gui")
                         (:file "sprites")
                         (:file "texture")
                         (:file "screens")
                         (:file "text")
                         (:file "tilemap")
                         (:file "particles")
                         (:file "pathfinding")
                         (:file "sound")
                         (:file "skeleton")
                         (:file "console")
                         (:file "input")))))


;; Use GLOP for input and window/context management
(push :glaw-examples-glop *features*)
;; Or SDL
;; (push :glaw-examples-sdl *features*)
