(defpackage :glaw
  (:use #:cl)
  (:export
   ;; utils
   #:random-between #:random-nth
   #:schedule #:update-timer #:run-timers #:update-scheduler #:cancel-timer
   ;; resources
   #:use-resource #:get-resource #:drop-resource
   #:create-resource-manager #:destroy-resource-manager
   #:with-resource-manager #:with-resource
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
   ;; text rendering
   #:create-font #:destroy-font
   #:font-set-glyph-data #:font-build-cache #:font-line-height #:char-width
   #:string-width #:string-height #:string-wrap
   #:render-string #:render-wrapped-string #:format-at
   ;; fps
   #:fps-counter #:update-fps #:current-fps #:min-fps #:max-fps #:avg-fps
   ;; view
   #:*display-width* #:*display-height* #:set-view-2d
   #:create-2d-view #:zoom-2d-view #:2d-view-zoom #:move-2d-view
   #:update-2d-view #:screen-to-view #:with-2d-view-coords
   #:begin-draw #:end-draw #:reshape #:setup-gl-defaults
   ;; shapes
   #:create-shape #:render-shape #:render-bbox
   #:shape-x-min #:shape-x-max #:shape-y-min #:shape-y-max
   #:coords-overlap-p
   #:shape-intersect-p
   #:translate-shape
   #:shape-set-vertex #:shape-set-color #:shape-set-tex-coord
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
   #:translate-mouse-button
   ;; game screens
   #:make-screen-stack #:current-screen #:push-screen #:pop-screen #:render-screens #:update-screens
   #:init-screen #:shutdown-screen #:render-screen #:update-screen
   ;; gui
   #:init-gui #:shutdown-gui #:render-gui #:update-gui #:gui-view
   #:gui-focus #:gui-focus-next #:gui-focus-prev
   #:create-widget #:gui-widget-visible #:show-widget #:hide-widget
   #:text #:move-widget
   ;; gui widgets
   #:gui-widget
   #:gui-label #:gui-dynamic-label #:gui-window #:gui-text-input #:gui-button
   #:gui-multiline-text #:gui-slider #:gui-slider-step-up #:gui-slider-step-down
   #:gui-gauge
))

(in-package #:glaw)
