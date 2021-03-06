;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;; Use GLOP for input and window/context management
(push :glaw-examples-glop *features*)
;; Or SDL
;;(push :glaw-examples-sdl *features*)

(asdf:defsystem glaw-examples
  :depends-on (:glaw :glop :glaw-imago)
  :license "MIT"
  :version "git"
  :description "Game programming utilities examples"
  :author "Morgan Veyret <patzy at oxyde dot org>"
  :components
  ((:module "examples"
            :serial t
            :components ((:file "examples")
                         (:file "empty")
                         (:file "views")
                         (:file "gui")
                         (:file "sprites")
                         (:file "framebuffer")
                         (:file "mesh")
                         (:file "shaders")
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

