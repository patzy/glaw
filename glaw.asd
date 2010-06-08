;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem glaw
  :license "MIT"
  :version "git"
  :description "Game programming utilities"
  :depends-on (:cl-opengl :cl-glu :cl-openal)
  :components
  ((:module "src"
            :serial t
            :components
            ((:file "package")
             (:file "utils")
             (:file "resource")
             (:file "input")
             (:file "graphics")
             (:file "anim")
             (:file "2d")
             (:file "gui")
             (:file "assets")
             (:file "font")
             (:file "particles")
             (:file "screen")
             (:file "scheduler")
             (:file "navmesh")
             (:file "sound")
             ;;(:file "console")
             (:file "glaw")))))

