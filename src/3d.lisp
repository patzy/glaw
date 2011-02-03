(in-package #:glaw)

;;; 3D view
;; forward: X
;; left: Y
;; up: Z
(defstruct (3d-view (:constructor %make-3d-view))
  perspective
  basis
  eye-mtx
  proj-mtx)

(defun %3d-view-update-proj-matrix (view)
  (perspective-apply (3d-view-perspective view)
                     (3d-view-proj-mtx view)))

(defun %3d-view-update-eye-matrix (view)
  (with-slots (eye-mtx basis) view
    ;; first apply rotations
    (setf (matrix-r00 eye-mtx) (- (basis-r01 basis)) ;; -y --> x
          (matrix-r01 eye-mtx) (- (basis-r11 basis))
          (matrix-r02 eye-mtx) (- (basis-r21 basis))
          (matrix-r10 eye-mtx) (basis-r02 basis)     ;; z --> y
          (matrix-r11 eye-mtx) (basis-r12 basis)
          (matrix-r12 eye-mtx) (basis-r22 basis)
          (matrix-r20 eye-mtx) (- (basis-r00 basis)) ;; -x --> z
          (matrix-r21 eye-mtx) (- (basis-r10 basis))
          (matrix-r22 eye-mtx) (- (basis-r20 basis)))
    ;; then translations
    (let ((eye-x (- (basis-tx basis)))
          (eye-y (- (basis-ty basis)))
          (eye-z (- (basis-tz basis))))
      (setf (matrix-tx eye-mtx) (+ (* eye-x (matrix-r00 eye-mtx))
                                   (* eye-y (matrix-r01 eye-mtx))
                                   (* eye-z (matrix-r02 eye-mtx)))
            (matrix-ty eye-mtx) (+ (* eye-x (matrix-r10 eye-mtx))
                                   (* eye-y (matrix-r11 eye-mtx))
                                   (* eye-z (matrix-r12 eye-mtx)))
            (matrix-tz eye-mtx) (+ (* eye-x (matrix-r20 eye-mtx))
                                   (* eye-y (matrix-r21 eye-mtx))
                                   (* eye-z (matrix-r22 eye-mtx)))))))

(defun make-3d-view (&key (fov (/ pi 3.0)) (ratio 1.0) (near 0.1) (far 100.0)
                          (position (make-point-3d))
                          (orientation (make-orientation)))
  (let ((view (%make-3d-view)))
    (setf (3d-view-perspective view)
          (make-perspective :fov fov :ratio ratio :near near :far far)
          (3d-view-basis view) (make-basis)
          (3d-view-eye-mtx view) (make-matrix)
          (3d-view-proj-mtx view) (make-matrix))
    (%3d-view-update-proj-matrix view)
    (%3d-view-update-eye-matrix view)
    view))

(defun set-view-3d (view)
  (set-view (3d-view-proj-mtx view) (3d-view-eye-mtx view)))

(defun 3d-view-forward (view)
  (basis-local-x (3d-view-basis view)))

(defun 3d-view-side (view)
  (basis-local-y (3d-view-basis view)))

(defun 3d-view-up (view)
  (basis-local-z (3d-view-basis view)))

(defun 3d-view-fov (v)
  (perspective-fov (3d-view-perspective v)))

(defun 3d-view-set-fov (v fov)
  (setf (perspective-fov (3d-view-perspective v)) fov)
  (%3d-view-update-proj-matrix v))

(defsetf 3d-view-fov 3d-view-set-fov)

(defun 3d-view-ratio (v)
  (perspective-ratio (3d-view-perspective v)))

(defun 3d-view-set-ratio (v ratio)
  (setf (perspective-ratio (3d-view-perspective v)) ratio)
  (%3d-view-update-proj-matrix v))

(defsetf 3d-view-ratio 3d-view-set-ratio)

(defun 3d-view-near (v)
  (perspective-near (3d-view-perspective v)))

(defun 3d-view-set-near (v near)
  (setf (perspective-near (3d-view-perspective v)) near)
  (%3d-view-update-proj-matrix v))

(defsetf 3d-view-near 3d-view-set-near)

(defun 3d-view-far (v)
  (perspective-far (3d-view-perspective v)))

(defun 3d-view-set-far (v far)
  (setf (perspective-far (3d-view-perspective v)) far)
  (%3d-view-update-proj-matrix v))

(defsetf 3d-view-far 3d-view-set-far)

(defun 3d-view-position (v)
  (basis-position (3d-view-basis v)))

(defun 3d-view-set-position (v pos)
  (basis-set-position (3d-view-basis v) pos)
  (%3d-view-update-eye-matrix v))

(defsetf 3d-view-position 3d-view-set-position)

(defun 3d-view-translate (v dx dy dz)
  (basis-translate (3d-view-basis v) dx dy dz)
  (%3d-view-update-eye-matrix v))

(defun 3d-view-orientation (v)
  (basis-zyx-orientation (3d-view-basis v))
  (%3d-view-update-eye-matrix v))

(defun 3d-view-set-orientation (v orientation)
  (basis-roll (3d-view-basis v) (orientation-roll orientation))
  (basis-pitch (3d-view-basis v) (orientation-pitch orientation))
  (basis-yaw (3d-view-basis v) (orientation-yaw orientation))
  (%3d-view-update-eye-matrix v))

(defun 3d-view-roll (v droll)
  (basis-roll (3d-view-basis v) droll)
  (%3d-view-update-eye-matrix v))

(defun 3d-view-pitch (v dpitch)
  (basis-pitch (3d-view-basis v) dpitch)
  (%3d-view-update-eye-matrix v))

(defun 3d-view-yaw (v dyaw)
  (basis-yaw (3d-view-basis v) dyaw)
  (%3d-view-update-eye-matrix v))

(defun 3d-view-point-at (v target)
  (let* ((dir (vector-3d-diff target (basis-position (3d-view-basis v))))
         (orientation (make-orientation-from-vector-3d dir)))
    (basis-cancel-rotation (3d-view-basis v))
    (setf (basis-zyx-orientation (3d-view-basis v)) orientation))
  (%3d-view-update-eye-matrix v))

(defun 3d-view-look-at (v eye target)
  (basis-set-position (3d-view-basis v) eye)
  (3d-view-point-at v target))

;;; Mesh
(defstruct mesh
  names
  shapes
  materials)

(defun create-mesh (&optional (nb-parts 0))
  (make-mesh :shapes (make-array nb-parts :adjustable t :fill-pointer t :element-type 'shape)
             :materials (make-array nb-parts :adjustable t :fill-pointer t)
             :names (make-array nb-parts :adjustable t :fill-pointer t :element-type 'string)))

(defun mesh-nb-parts (mesh)
  (length (mesh-shapes mesh)))

(defun mesh-shape (mesh &optional (part-index (1- (length (mesh-shapes mesh)))))
  (assert (< part-index (mesh-nb-parts mesh)))
  (aref (mesh-shapes mesh) part-index))

(defun mesh-material (mesh &optional (part-index (1- (length (mesh-shapes mesh)))))
  (assert (< part-index (mesh-nb-parts mesh)))
  (aref (mesh-materials mesh) part-index))

(defun mesh-part (mesh &optional (part-index (1- (length (mesh-shapes mesh)))))
  (values (mesh-shape mesh) (mesh-material mesh)))

;; FIXME: use resource manager here
(defun mesh-add-part (mesh &key (shape (create-shape 0 0)) (material +default-material+)
                                (name (symbol-name (gensym "PART-"))))
  (vector-push-extend shape (mesh-shapes mesh))
  (vector-push-extend material (mesh-materials mesh)))

(defun render-mesh (mesh)
  (loop for i below (mesh-nb-parts mesh)
       for shape = (mesh-shape mesh i)
       for mat = (mesh-material mesh i)
       do (set-material mat)
          (render-shape shape)))


;;; Wavefront OBJ :mesh asset
;; http://www.martinreddy.net/gfx/3d/OBJ.spec
(defasset :mesh '("obj")
  ;; load
  (lambda (filename)
    (with-open-file (in filename :direction :input)
      (let ((mesh (create-mesh)))
        (loop for line = (read-line in nil)
           while line
           with group = nil
           and material = +default-material+
           and vertices = nil
           and normals = nil
           and tex-coords = nil
           and vertex-indices = nil
           and tex-vertex-indices = nil
           and normal-indices = nil
           when (string/= line "")
           do (case (aref line 0)
                (#\# (dformat "Wavefront comment: ~S~%" line))
                (#\g (when group ;; store last group read
                       (%mesh-add-wavefront-part mesh group material
                                                 vertices tex-coords normals
                                                 vertex-indices tex-vertex-indices normal-indices))
                     (setf vertices nil
                           tex-coords nil
                           normals nil
                           vertex-indices nil
                           tex-vertex-indices nil
                           normal-indices nil
                           group (subseq line 2)))
                (#\v (case (aref line 1)
                       (#\t (unless tex-coords
                              (setf tex-coords (make-array 0 :adjustable t :fill-pointer t)))
                            (read-wavefront-vertex line tex-coords))
                       (#\n (unless normals
                              (setf normals (make-array 0 :adjustable t :fill-pointer t)))
                            (read-wavefront-vertex line normals))
                       (otherwise
                        (unless group
                          (setf group (symbol-name (gensym "MESH-"))))
                        (unless vertices
                          (setf vertices (make-array 0 :adjustable t :fill-pointer t)))
                        (read-wavefront-vertex line vertices))))
                (#\f (unless (or vertex-indices (not vertices))
                       (setf vertex-indices (make-array 0 :adjustable t :fill-pointer t)))
                     (unless (or normal-indices (not normals))
                       (setf normal-indices (make-array 0 :adjustable t :fill-pointer t)))
                     (unless (or tex-vertex-indices (not tex-coords))
                       (setf tex-vertex-indices (make-array 0 :adjustable t :fill-pointer t)))
                     (read-wavefront-face line vertex-indices tex-vertex-indices normal-indices))
                (otherwise (dformat "Wavefront, ignoring ~S~%" line)))
             finally (when group ;; store last group read
                       (%mesh-add-wavefront-part mesh group material
                                                 vertices tex-coords normals
                                                 vertex-indices tex-vertex-indices normal-indices)))
        mesh)))
  ;; unload
  (lambda (mesh)
    (values)))

(defun read-wavefront-vertex (line data)
  (let ((datums (split-string (subseq line 2) #\Space)))
    (loop for d in datums do (vector-push-extend (read-from-string d) data))))

(defun read-wavefront-face (line vertex-indices tex-vertex-indices normal-indices)
  (let ((datums (split-string (subseq line 2) #\Space)))
    (loop for d in datums
       do (let* ((indices (mapcar #'1- (mapcar #'read-from-string (split-string d #\/))))
                 (types (case (+ (loop for c across d when (char= c #\/) sum 1)
                                 (length indices))
                          (1 :vertex)
                          (3 :vertex-texture)
                          (4 :vertex-normal)
                          (5 :vertex-texture-normal))))
           (case types
             (:vertex (vector-push-extend (first indices) vertex-indices))
             (:vertex-texture
              (vector-push-extend (first indices) vertex-indices)
              (vector-push-extend (second indices) tex-vertex-indices))
             (:vertex-normal
              (vector-push-extend (first indices) vertex-indices)
              (vector-push-extend (second indices) normal-indices))
             (:vertex-texture-normal
              (vector-push-extend (first indices) vertex-indices)
              (vector-push-extend (second indices) tex-vertex-indices)
              (vector-push-extend (third indices) normal-indices)))))))

(defun %mesh-add-wavefront-part (mesh name material vertices tex-coords normals
                                 vertex-indices tex-vertex-indices normal-indices)
  (let ((indices (make-array (length vertex-indices))))
    (loop for i below (length vertex-indices)
         do (setf (aref indices i) i))
    (let ((shape (create-shape-from-arrays
                  indices
                  (%vertex-data-from-face-data vertices vertex-indices)
                  nil
                  (%vertex-data-from-face-data tex-coords tex-vertex-indices)
                  (%vertex-data-from-face-data normals normal-indices))))
      (unless (and normals (not (zerop (length normal-indices))))
        (shape-compute-normals shape))
      (mesh-add-part mesh :shape shape :material material :name name))))

;; ;; Wavefront OBJ :material-lib asset
;; (defasset :material-lib '("mtl")
;;   ;; load
;;   (lambda (filename)
    
;;   ;; unload
;;   (lambda (material)
;;     (declare (ignore material))
;;     (values)))