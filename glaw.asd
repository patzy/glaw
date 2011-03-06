;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem glaw
  :license "MIT"
  :version "git"
  :description "Game programming utilities"
  :depends-on (:cl-opengl :cl-openal :cl-alc)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "utils")
             (:file "math")
             (:file "resource")
             (:file "assets")
             (:file "input")
             (:file "stats")
             (:file "graphics")
             (:file "shape")
             (:file "bbox")
             (:file "anim")
             (:file "2d")
             (:file "3d")
             (:file "gui")
             (:file "font")
             (:file "particles")
             (:file "screen")
             (:file "scheduler")
             (:file "navmesh")
             (:file "skeleton")
             (:file "sound")
             (:file "console")))))

