;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem glaw
  :depends-on (cl-opengl cl-glu)
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "resource")
   (:file "input")
   (:file "graphics")
   (:file "gui")
   (:file "assets")
   (:file "font")
   (:file "particles")
   (:file "screen")
   (:file "scheduler")
   ;;(:file "console")
   (:file "glaw")))

