(defpackage :glaw
  (:use #:cl)
  (:export
   ;; utils
   #:random-between #:random-nth
   #:make-vector-2d #:vec-from-lst #:vec-from-coords #:vec-slope #:vec-dot-product
   #:vec-perp-dot-product #:vec-mag #:vec-normalize #:vec-null-p
   #:ver-perp #:vec-opposite #:vec-rotate #:vec-angle #:vec-add #:vec-sum #:vec-sub #:vec-diff
   #:vec-scale #:vector-2d-x #:vector-2d-y
   #:deg->rad #:rad->deg #:coords-overlap-p
   ;; timing
   #:schedule #:update-timer #:run-timers #:update-scheduler #:cancel-timer
   #:with-timestep
   ;; resources
   #:use-resource #:get-resource #:drop-resource
   #:use-resources #:drop-resources
   #:create-resource-manager #:destroy-resource-manager
   #:with-resource-manager #:with-resources
   ;; assets
   #:init-content-manager #:shutdown-content-manager #:load-asset #:dispose-asset #:defasset
   #:supported-assets
   ;; colors
   #:create-color #:mix-colors #:create-color-gradient
   #:color-copy
   #:set-color #:set-color/rgb
   #:set-color-from-gradient #:get-color-from-gradient
   #:get-color-from-gradient/rgb
   ;; images
   #:create-image #:image-set-pixel #:image-set-pixel/index #:image-width #:image-height #:image-bpp
   #:make-image #:image-get-pixel #:image-get-pixel/index
   ;; textures
   #:create-texture #:create-texture-from-image #:create-texture-from-file
   #:destroy-texture #:select-texture #:update-texture #:update-texture-from-image
   ;; text rendering
   #:create-font #:destroy-font
   #:font-set-glyph-data #:font-build-cache #:font-line-height #:char-width
   #:string-width #:string-height #:string-wrap
   #:render-string #:render-wrapped-string #:format-at
   ;; fps
   #:fps-counter #:update-fps #:current-fps #:min-fps #:max-fps #:avg-fps
   ;; view
   #:*display-width* #:*display-height* #:set-view-2d
   #:create-2d-view #:zoom-2d-view #:2d-view-zoom #:move-2d-view #:update-2d-view
   #:screen-to-view #:view-to-screen #:with-2d-coords-from-screen #:with-2d-coords-to-screen
   #:view-to-view #:with-2d-view-coords #:with-2d-view-deltas #:with-2d-screen-deltas
   #:2d-view-left #:2d-view-bottom #:2d-view-right #:2d-view-top #:2d-view-width #:2d-view-height
   #:begin-draw #:end-draw #:reshape #:setup-gl-defaults
   ;; shapes
   #:create-shape #:render-shape #:shape-nb-vertices #:shape-nb-indices #:shape-get-vertex
   #:shape-get-index
   #:shape-ensure-adjustable
   #:translate-shape #:rotate-shape-2d
   #:shape-set-vertex #:shape-set-color #:shape-set-tex-coord
   #:shape-add-vertex  #:shape-add-vertex/index #:shape-set-vertex
   #:shape-add-color #:shape-add-color/rgb
   #:shape-add-tex-vertex
   #:shape-add-indices
   #:create-grid-shape #:create-cross-shape #:create-rectangle-shape
   #:create-line-shape #:create-circle-shape #:create-triangle-shape #:create-polygon-shape
   ;; bbox
   #:make-bbox #:bbox-invalidate #:bbox-intersect-p #:bbox-update #:bbox-update/shape
   #:bbox-visible-p
   #:bbox-overwrite/shape #:bbox-translate #:bbox-inside-p
   #:bbox-x-min #:bbox-x-max #:bbox-y-min #:bbox-y-max #:render-bbox
   #:create-bbox-from-shape
   ;; sprites
   #:create-sprite #:render-sprite
   #:create-tilemap #:render-tilemap #:tilemap-nb-tiles #:make-tileset
   #:tileset-pixel-width #:tileset-pixel-height #:tileset-tiles-width #:tileset-tiles-height
   #:tileset-spacing #:tileset-margin #:tileset-start-index
   ;; animation
   #:make-anim-state #:make-keyframe-anim #:animation-apply
   #:anim-state-update #:anim-state-apply
   ;; pathfinding
   #:create-grid-navmesh #:navmesh-cell #:navmesh-remove-cell-at #:connect-grid-navmesh
   #:simplify-navmesh #:render-navmesh #:navcell-vertices #:navmesh-nb-cells
   #:find-path/cells #:find-path #:make-navcell #:navcell-add-vertex #:make-navmesh
   #:navcells-connected-p #:navmesh-containing-cell #:navcell-center
   ;; particles
   #:create-particle-system #:particle-system-nb-particles #:update-particles #:render-particles
   ;; input management
   #:add-input-handler #:remove-input-handler #:push-input-handlers #:pop-input-handlers
   #:dispatch-key-event #:dispatch-button-event #:dispatch-motion-event
   #:key-handler #:button-handler #:motion-handler
   #:*mouse-x* #:*mouse-y* #:update-mouse-position
   #:translate-mouse-button
   #:input-handler #:make-input-repeat #:make-input-chord #:make-input-sequence
   #:input-processor-reset #:input-processor-valid-p
   ;; game screens
   #:make-screen-stack #:current-screen #:push-screen #:pop-screen #:empty-screen-stack
   #:has-screens
   #:render-screens #:update-screens
   #:replace-screen #:suspend-screen #:resume-screen
   #:init-screen #:shutdown-screen #:render-screen #:update-screen
   ;; sound
   #:init-sound #:shutdown-sound #:update-sound #:sound-nb-channels #:sound-nb-free-channels
   #:sound-nb-used-channels #:play-sound #:stop-sound #:play-channel #:stop-channel
   #:channel-volume #:main-volume #:set-main-volume
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
