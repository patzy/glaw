;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem glaw-imago
  :depends-on (:glaw :imago)
  :components
  ((:module "ext"
            :components ((:file "imago")))))

