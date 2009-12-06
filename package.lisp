(defpackage :glaw
  (:use #:cl)
  (:export
   ;; resources
   #:create-resource #:use-resource #:get-resource #:drop-resource #:destroy-resource
   #:create-resource-manager #:destroy-resource-manager
   #:width-resource-manager #:with-resource
   ;; assets
   #:init-content-manager #:shutdown-content-manager #:load-asset #:dispose-asset #:defasset
   ;; colors
   #:create-color #:mix-colors #:create-color-gradient
   #:copy-color
   #:set-color #:set-color/rgb
   #:set-color-from-gradient #:get-color-from-gradient
   #:get-color-from-gradient/rgb
   ;; images
   #:create-image-from-file #:destroy-image #:make-image
   ;; textures
   #:create-texture #:create-texture-from-image #:create-texture-from-file
   #:destroy-texture #:select-texture
   #:create-bitmap-font #:destroy-font
   #:render-bitmap-string #:string-width #:string-height
   #:format-at
   #:fps-counter #:update-fps #:current-fps
   ;; view
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
   ;; sprites
   #:create-sprite #:render-sprite
   ;; particles
   #:create-particle-system #:update-particles #:render-particles
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
