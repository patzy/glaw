;;; Example template
(in-package #:glaw-examples)

(defstruct empty)

(defmethod init-example ((it empty)))

(defmethod shutdown-example ((it empty)))

(defmethod render-example ((it empty)))

(defmethod update-example ((it empty) dt))

(defmethod reshape-example ((it empty) w h))

