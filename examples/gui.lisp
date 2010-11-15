(in-package #:glaw-examples)

(defstruct gui)

(defmethod init-example ((it gui))
  (declare (ignore it))
  (glaw:load-asset "dejavu-sans.fnt" :font)
  (glaw:load-asset "starfield.png" :texture)
  (glaw:load-asset "button.png" :texture)
  (glaw:init-gui (glaw:use-resource "default-font"))
  (let* ((window (glaw:create-widget 'glaw:gui-window nil
                                     :x 50 :y 50
                                     :width 300 :height 500
                                     :texture (glaw:use-resource "starfield.png")
                                     :title "Static window"))
         (window2 (glaw:create-widget 'glaw:gui-window nil
                                     :x 10 :y 10
                                     :width 300 :height 500
                                     :moveable t
                                     :title "Moveable window"))
         (window3 (glaw:create-widget 'glaw:gui-window nil
                                     :x 100 :y 200
                                     :width 300 :height 500
                                     :layout :vertical
                                     :moveable t
                                     :title "Layouts"))
         (panes (glaw:create-widget 'glaw:gui-widget window3
                                    :height 0.7
                                    :layout :horizontal))
         (left-pane (glaw:create-widget 'glaw:gui-widget panes
                                     :width 0.5
                                     :color (glaw:create-color 1 0 0)
                                     :layout :vertical))
         (right-pane (glaw:create-widget 'glaw:gui-widget panes
                                     :width 0.5
                                     :color (glaw:create-color 0 1 0)
                                     :layout :vertical))
         (bottom-pane (glaw:create-widget 'glaw:gui-widget window3
                                     :width 1.0 :height 0.3
                                     :color (glaw:create-color 0 0 1)
                                     :layout :horizontal))
         (label (glaw:create-widget 'glaw:gui-label window
                                    :x 50 :y 200
                                    :width 50 :height 10
                                    :text "Some text label !")))
    (glaw:create-widget 'glaw:gui-button window
                                    :x 50 :y 50
                                    :width 100 :height 50
                                    :texture (glaw:use-resource "button.png")
                                    :action (let ((nb-clicks 0))
                                              (lambda ()
                                                (declare (ignore self))
                                                (incf nb-clicks)
                                                (setf (glaw:text label)
                                                      (format nil
                                                              "Clics: ~d"
                                                              nb-clicks))))
                                    :text "Click me !")
    (glaw:create-widget 'glaw:gui-slider window2
                                    :x 10 :y 150
                                    :width 100 :height 10)
    (glaw:create-widget 'glaw:gui-gauge window2
                                    :x 10 :y 200
                                    :color (glaw:create-color 0 0 0 1)
                                    :width 100 :height 10)
    (glaw:create-widget 'glaw:gui-label window2
                        :x 20 :y 50
                        :width 50 :height 10
                        :text "Input:")
    (glaw:create-widget 'glaw:gui-text-input window2
                        :x 50 :y 50
                        :width 50 :height 10)
    (loop for i below 5
         do (glaw:create-widget 'glaw:gui-button bottom-pane
                                :texture (glaw:use-resource "button.png")
                                :width 0.2 :height 1.0))
    (loop for i upto 10
         do (glaw:create-widget 'glaw:gui-label left-pane
                                :width 30 :height 30
                                :text (format nil "Label ~d" i)))
    (loop for i upto 10
         do (glaw:create-widget 'glaw:gui-button right-pane
                                :texture (glaw:use-resource "button.png")
                                :width 150 :height 30
                                :text (format nil "button ~d" i)))))

(defmethod shutdown-example ((it gui))
  (declare (ignore it))
  (glaw:dispose-asset "dejavu-sans.fnt")
  (glaw:dispose-asset "starfield.png")
  (glaw:dispose-asset "button.png")
  (glaw:shutdown-gui))

(defmethod render-example ((it gui))
  (declare (ignore it))
  (glaw:begin-draw)
  (glaw:render-gui)
  (glaw:end-draw))

(defmethod update-example ((it gui) dt)
  (declare (ignore it dt)))

(defmethod reshape-example ((it gui) w h)
  (declare (ignore it w h))
  (glaw:update-gui))

