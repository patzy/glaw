(defpackage :glaw
  (:use #:cl)
  (:export
   ;; utils
   #:random-between #:random-nth #:nshuffle #:shuffle
   ;; math
   #:deg->rad #:rad->deg #:coords-overlap-p
   #:make-vector-2d #:make-vector-2d-from-list #:make-vector-2d-from-coords
   #:vector-2d-x #:vector-2d-y
   #:vector-2d-slope #:vector-2d-dot-product
   #:vector-2d-perp-dot-product #:vector-2d-mag #:vector-2d-normalize
   #:vector-2d-null-p #:vector-2d-perp #:vector-2d-opposite #:vector-2d-rotate
   #:vector-2d-angle #:vector-2d-add #:vector-2d-sum #:vector-2d-sub #:vector-2d-diff
   #:vector-2d-scale #:vector-3d-nscale #:vector-2d-x #:vector-2d-y
   #:make-point-2d #:make-point-2d-from-polar #:point-2d-angle #:point-2d-distance
   #:make-vector-3d #:make-vector-3d-from-list #:make-vector-3d-from-coords
   #:vector-3d-x #:vector-3d-y #:vector-3d-z
   #:vector-3d-dot-product #:vector-3d-cross-product
   #:vector-3d-mag #:vector-3d-normalize
   #:vector-3d-null-p #:vector-3d-opposite
   #:vector-3d-add #:vector-3d-sum #:vector-3d-sub #:vector-3d-diff
   #:vector-3d-scale #:vector-3d-nscale #:vector-3d-x #:vector-3d-y #:vector-3d-z
   #:vector-3d-project-xy #:vector-3d-project-xz #:vector-3d-project-yz
   #:make-point-3d-from-spherical #:point-3d-distance #:point-3d-angles
   #:make-vector-4d #:make-vector-4d-from-3d
   #:vector-4d-x #:vector-4d-y #:vector-4d-z #:vector-4d-w
   #:make-orientation #:orientation-roll #:orientation-pitch #:orientation-yaw
   #:make-orientation-from-vector-3d
   #:make-axis #:+x-axis+ #:+y-axis+ #:+z-axis+
   #:make-quaternion #:make-quaternion-from-axis-angle #:make-quaternion-from-angles
   #:make-quaternion-from-orientation
   #:quaternion-x #:quaternion-y #:quaternion-z #:quaternion-w
   #:quaternion-mag #:quaternion-scale #:quaternion-nscale #:quaternion-normalize
   #:quaternion-sum #:quaternion-add #:quaternion-diff #:quaternion-sub
   #:quaternion-mult
   #:make-matrix #:+matrix-identity+
   #:make-basis
   #:basis-local-x #:basis-local-y #:basis-local-z
   #:basis-position #:basis-translate #:basis-axis-angle
   #:basis-roll #:basis-pitch #:basis-yaw #:basis-cancel-rotation
   #:basis-zyx-orientation #:basis-xyz-orientation
   #:matrix-set-ortho #:matrix-set-frustum
   #:make-ortho-matrix #:make-frustum-matrix
   #:make-perspective
   #:perspective-fov #:perspective-ratio #:perspective-apply
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
   ;; graphics
   #:*display-width* #:*display-height*
   #:begin-draw #:end-draw #:reshape #:setup-2d-defaults #:setup-3d-defaults
   #:draw-origin
   #:create-color #:mix-colors #:create-color-gradient
   #:color-copy
   #:set-color #:set-color/rgb
   #:set-color-from-gradient #:get-color-from-gradient
   #:get-color-from-gradient/rgb
   #:create-image #:image-set-pixel #:image-set-pixel/index #:image-width #:image-height #:image-bpp
   #:make-image #:image-get-pixel #:image-get-pixel/index
   #:create-texture #:create-texture-from-image #:create-texture-from-file
   #:destroy-texture #:select-texture #:update-texture #:update-texture-from-image
   ;; text rendering
   #:create-font #:destroy-font
   #:font-set-glyph-data #:font-build-cache #:font-line-height #:char-width
   #:string-width #:string-height #:string-wrap
   #:render-string #:render-wrapped-string #:format-at
   ;; fps
   #:fps-counter #:update-fps #:current-fps #:min-fps #:max-fps #:avg-fps
   ;; 2d view
   #:set-view-2d
   #:create-2d-view #:zoom-2d-view #:2d-view-zoom #:move-2d-view #:update-2d-view
   #:screen-to-view #:view-to-screen #:with-2d-coords-from-screen #:with-2d-coords-to-screen
   #:view-to-view #:with-2d-view-coords #:with-2d-view-deltas #:with-2d-screen-deltas
   #:2d-view-left #:2d-view-bottom #:2d-view-right #:2d-view-top #:2d-view-width #:2d-view-height
   ;; 3d view
   #:set-view-3d
   #:make-3d-view
   #:3d-view-foward #:3d-view-side #:3d-view-up
   #:3d-view-fov #:3d-view-ratio #:3d-view-near #:3d-view-far #:3d-view-position
   #:3d-view-translate #:3d-view-roll #:3d-view-pitch #:3d-view-yaw
   #:3d-view-orientation #:3d-view-point-at #:3d-view-look-at
   ;; shapes
   #:create-shape #:render-shape #:shape-nb-vertices #:shape-nb-indices #:shape-get-vertex
   #:shape-get-index
   #:shape-ensure-adjustable
   #:translate-shape #:rotate-shape-2d
   #:shape-set-vertex #:shape-set-color #:shape-set-tex-coord
   #:shape-add-vertex  #:shape-add-vertex/index #:shape-set-vertex
   #:shape-add-normal
   #:shape-add-color #:shape-add-color/rgb
   #:shape-add-tex-vertex
   #:shape-add-indices
   #:create-grid-shape #:create-cross-shape #:create-rectangle-shape
   #:create-line-shape #:create-circle-shape #:create-triangle-shape #:create-polygon-shape
   #:create-box-shape
   ;; bbox
   #:make-bbox #:bbox-invalidate #:bbox-intersect-p #:bbox-update #:bbox-update/shape
   #:bbox-visible-p
   #:bbox-overwrite/shape #:bbox-translate #:bbox-inside-p
   #:bbox-x-min #:bbox-x-max #:bbox-y-min #:bbox-y-max #:render-bbox
   #:create-bbox-from-shape
   ;; sprites
   #:create-sprite #:render-sprite #:move-sprite #:translate-sprite #:rotate-sprite
   #:create-tilemap #:render-tilemap #:tilemap-nb-tiles #:make-tileset
   #:tileset-pixel-width #:tileset-pixel-height #:tileset-tiles-width #:tileset-tiles-height
   #:tileset-spacing #:tileset-margin #:tileset-start-index
   ;; animation
   #:make-anim-state #:make-keyframe-anim #:animation-apply
   #:anim-state-update #:anim-state-apply
   ;; pathfinding
   #:find-path/nodes #:find-path
   #:make-navmesh
   #:navmesh-cells #:make-navmesh-cell #:navmesh-cell-add-vertex
   #:navmesh-remove-cell-at #:navmesh-cell
   #:simplify-navmesh #:render-navmesh #:navmesh-cell-vertices #:navmesh-nb-cells
   #:create-grid-navmesh #:connect-grid-navmesh
   ;; particles
   #:create-particle-system #:particle-system-nb-particles #:update-particles #:render-particles
   ;; input management
   #:add-input-handler #:remove-input-handler #:push-input-handlers #:pop-input-handlers
   #:clear-input-handlers
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
