(in-package #:glaw-examples)

(defstruct text
  (font1 nil)
  (font2 nil)
  (font3 nil)
  (view (glaw:create-2d-view 0 0 glaw:*display-width* glaw:*display-height*)))

(defmethod init-example ((it text))
  (glaw:init-content-manager "data/")
  (glaw:load-asset "font.png" :fixed-bitmap-font)
  (glaw:load-asset "dejavu-sans.fnt" :fonttool-bitmap-font)
  (glaw:load-asset "liberation-serif-italic.fnt" :fonttool-bitmap-font)
  (setf (text-font1 it) (glaw:use-resource "font.png"))
  (setf (text-font2 it) (glaw:use-resource "dejavu-sans.fnt"))
  (setf (text-font3 it) (glaw:use-resource "liberation-serif-italic.fnt"))
  (glaw:add-input-handler it))

(defmethod shutdown-example ((it text))
  (glaw:dispose-asset "font.png")
  (glaw:dispose-asset "dejavu-sans.fnt")
  (glaw:dispose-asset "liberation-serif-italic.fnt")
  (glaw:remove-input-handler it))

(defmethod render-example ((it text))
  (glaw:begin-draw)
  (glaw:set-view-2d (text-view it))
  (let ((line 400))
    (glaw:format-at 50 line (text-font1 it) "abcdefghijklmnopqrstuvwxyz")
    (incf line (glaw:font-line-height (text-font1 it)))
    (glaw:format-at 50 line (text-font1 it) "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    (incf line (glaw:font-line-height (text-font1 it)))
    (glaw:format-at 50 line (text-font1 it) "Hello world..."))

  (let ((line 200))
    (glaw:format-at 50 line (text-font2 it) "abcdefghijklmnopqrstuvwxyz")
    (incf line (glaw:font-line-height (text-font2 it)))
    (glaw:format-at 50 line (text-font2 it) "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    (incf line (glaw:font-line-height (text-font2 it))))

  (glaw:render-wrapped-string 350 300 200 (text-font2 it)
                              "Hello world...This system works, and is quite easy to use. The reason the tool to create font files is a separate program is that this way you won't have to link FreeType with your main program, and loading a font is very fast. If you want your program to be able to load and use arbitrary true type fonts you will have to integrate the functionality of fonttool into your main program.")

  (glaw:render-wrapped-string 650 600 200 (text-font2 it)
                              "Hello world...This system works, and is quite easy to use. The reason the tool to create font files is a separate program is that this way you won't have to link FreeType with your main program, and loading a font is very fast. If you want your program to be able to load and use arbitrary true type fonts you will have to integrate the functionality of fonttool into your main program."
                              :justify :right)

  (glaw:render-wrapped-string 650 200 200 (text-font2 it)
                              "Hello world...This system works, and is quite easy to use. The reason the tool to create font files is a separate program is that this way you won't have to link FreeType with your main program, and loading a font is very fast. If you want your program to be able to load and use arbitrary true type fonts you will have to integrate the functionality of fonttool into your main program."
                              :justify :center)

  (let ((line 600))
    (glaw:format-at 250 line (text-font3 it) "abcdefghijklmnopqrstuvwxyz")
    (incf line (glaw:font-line-height (text-font3 it)))
    (glaw:format-at 250 line (text-font3 it) "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    (incf line (glaw:font-line-height (text-font3 it)))
    (glaw:format-at 250 line (text-font3 it)
                    "Hello world...This system works, and is quite easy to use. The reason the tool to create font files is a separate program is that this way you won't have to link FreeType with your main program, and loading a font is very fast. If you want your program to be able to load and use arbitrary true type fonts you will have to integrate the functionality of fonttool into your main program."))

  (glaw:format-at 50 100 (text-font2 it) "FPS: ~a" (glaw:current-fps))
  (glaw:end-draw))

(defmethod update-example ((it text) dt)
  (declare (ignore it dt)))

(defmethod reshape-example ((it text) w h)
  (glaw:update-2d-view (text-view it) 0 0 w h))

