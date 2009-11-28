(defpackage :glaw
  (:use #:cl)
  (:export
   ;; resources
   #:create-resource #:destroy-resource #:free-all-resources
   #:load-resource #:free-resource
   #:image-resource #:sound-resource
   #:make-image-resource #:make-sound-resource
   ;; graphics
   #:create-texture #:create-texture-from-file
   #:create-color #:mix-colors #:create-color-gradient
   #:set-color #:set-color/rgb
   #:set-color-from-gradient #:get-color-from-gradient
   #:get-color-from-gradient/rgb
   #:create-texture #:destroy-texture #:select-texture
   #:create-font #:destroy-font
   #:render-bitmap-string #:string-width #:string-height
   #:format-at
   #:fps-counter #:update-fps #:current-fps
   #:*display-width* #:*display-height* #:set-view-2d
   #:create-2d-view #:zoom-2d-view #:2d-view-zoom #:move-2d-view
   #:update-2d-view #:screen-to-view
   #:begin-draw #:end-draw #:reshape #:setup-gl-defaults
   ;; shapes
   #:create-shape #:render-shape #:render-bbox
   #:shape-x-min #:shape-x-max #:shape-y-min #:shape-y-max
   #:coords-overlap-p
   #:shape-intersect-p
   #:translate-shape
   #:shape-add-vertex  #:shape-add-vertex/index #:shape-set-vertex
   #:shape-add-color #:shape-add-color/rgb
   #:shape-add-tex-vertex
   #:create-grid-shape #:create-cross-shape #:create-rectangle-shape
   #:create-line-shape #:create-circle-shape #:create-triangle-shape
   ;; input management
   #:add-input-handler #:remove-input-handler
   #:dispatch-key-event #:dispatch-button-event #:dispatch-motion-event
   #:key-handler #:button-handler #:motion-handler
   #:*mouse-x* #:*mouse-y* #:update-mouse-position
   ;; gui
   #:init-gui #:shutdown-gui #:render-gui #:update-gui
   #:gui-focus #:gui-focus-next #:gui-focus-prev
   #:create-widget
   #:text
   ;; gui widgets
   #:gui-widget
   #:gui-label #:gui-dynamic-label #:gui-window #:gui-text-input #:gui-button
   #:gui-multiline-text #:gui-slider #:gui-slider-step-up #:gui-slider-step-down
   #:gui-gauge
))

(in-package #:glaw)
