;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defsystem glaw-examples
  :depends-on (glaw glaw-sdl)
  :components
  ((:module "examples"
            :components ((:file "gui")))))

(defpackage :glaw-examples
  (:use #:cl)
  (:export #:run-gui))

(in-package #:glaw-examples)
