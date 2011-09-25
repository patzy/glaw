(in-package #:glaw)

;;; Mesh
(defstruct mesh
  names
  shapes
  dlists ;; TESTING: rendering performance
  materials)

(defun create-mesh (&optional (nb-parts 0))
  (make-mesh :shapes (make-array nb-parts :adjustable t :fill-pointer t :element-type 'shape)
             :materials (make-array nb-parts :adjustable t :fill-pointer t)
             :dlists (make-array nb-parts :adjustable t :fill-pointer t :element-type 'display-list)
             :names (make-array nb-parts :adjustable t :fill-pointer t :element-type 'string)))

(defun mesh-nb-parts (mesh)
  (length (mesh-shapes mesh)))

(defun mesh-shape (mesh &optional (part-index (1- (length (mesh-shapes mesh)))))
  (assert (< part-index (mesh-nb-parts mesh)))
  (aref (mesh-shapes mesh) part-index))

(defun mesh-material (mesh &optional (part-index (1- (length (mesh-shapes mesh)))))
  (assert (< part-index (mesh-nb-parts mesh)))
  (aref (mesh-materials mesh) part-index))

(defun mesh-name (mesh &optional (part-index (1- (length (mesh-shapes mesh)))))
  (assert (< part-index (mesh-nb-parts mesh)))
  (aref (mesh-names mesh) part-index))

(defun mesh-part (mesh &optional (part-index (1- (length (mesh-shapes mesh)))))
  (values (mesh-shape mesh part-index)
          (mesh-material mesh part-index)
          (mesh-name mesh part-index)))

(defun mesh-add-part (mesh &key (shape (create-shape 0 0)) (material +default-material+)
                                (name (symbol-name (gensym "PART-"))))
  (vector-push-extend shape (mesh-shapes mesh))
  (vector-push-extend (load-primitive (shape-indices shape)
                                      (shape-vertices shape)
                                      :primitive (shape-primitive shape)
                                      :colors (shape-colors shape)
                                      :tex-coords (shape-tex-coords shape)
                                      :normals (shape-normals shape)
                                      :use-buffers t)
                      (mesh-dlists mesh))
  (vector-push-extend material (mesh-materials mesh))
  (vector-push-extend name (mesh-names mesh)))

(defun dprint-mesh (mesh)
  (dformat "Mesh (~S parts)~%" (mesh-nb-parts mesh))
  (loop for i below (mesh-nb-parts mesh)
       do (dformat "Part: ~S~%" (mesh-name mesh i))))

(defun render-mesh (mesh)
  (loop for i below (mesh-nb-parts mesh)
       for shape = (mesh-shape mesh i)
       for dl = (aref (mesh-dlists mesh) i)
       for mat = (mesh-material mesh i)
       do (set-material mat)
       (call-primitive dl)
       ;;(render-shape shape)
       ))

;;; Wavefront OBJ :mesh asset
;; http://www.martinreddy.net/gfx/3d/OBJ.spec
;; Object and groups are considered the same
;; On part per material is created upon loading
;; when a single object contains multiple materials
;; then one part per material is created, all parts having the same name (object's name)
(defasset :mesh '("obj")
  ;; load
  (lambda (&key filename &allow-other-keys)
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
                (#\m (let ((words (split-string line #\Space)))
                       (when (string= (first words) "mtllib")
                         (dformat "Waveront material library: ~S~%" (second words))
                         (load-asset (second words) :material-lib (second words)))))
                (#\u (let ((words (split-string line #\Space)))
                       (when (string= (first words) "usemtl")
                         ;; new material, store what we read so far
                         (when vertex-indices
                           (%mesh-add-wavefront-part mesh group material
                                                   vertices tex-coords normals
                                                   vertex-indices tex-vertex-indices normal-indices)
                           ;; reset indices
                           (setf vertex-indices nil
                                 tex-vertex-indices nil
                                 normal-indices nil))
                         ;; retrieve proper material
                         (unless (string= (second words) "(null)") ;; is this blender only?
                           (dformat "Waveront using material: ~S~%" (second words))
                           (setf material (use-asset (second words)))))))
                ((or #\o #\g)
                 (let ((words (split-string line #\Space)))
                   (dformat "Wavefront group: ~S~%" (second words))
                   ;; new group, store what we read so far
                   (when vertex-indices
                     (%mesh-add-wavefront-part mesh group material
                                               vertices tex-coords normals
                                               vertex-indices tex-vertex-indices normal-indices)
                     ;; reset indices
                     (setf vertex-indices nil
                           tex-vertex-indices nil
                           normal-indices nil))
                   (setf group (second words))))
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
                finally (when vertex-indices ;; store remaining data
                          (%mesh-add-wavefront-part mesh group material
                                                 vertices tex-coords normals
                                                 vertex-indices tex-vertex-indices normal-indices)))
        (dprint-mesh mesh)
        mesh)))
  ;; unload
  (lambda (mesh)
    (values)))

(defun read-wavefront-vertex (line data)
  (let ((datums (split-string (subseq line 2) #\Space)))
    (loop for d in datums do (vector-push-extend (read-from-string d) data))))


(defun read-wavefront-face (line vertex-indices tex-vertex-indices normal-indices)
  (let ((datums (split-string (subseq line 2) #\Space)))
    (unless (= (length datums) 3)
      (error "Only triangle mesh loading is supported for Wavefront file."))
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
              ;;(vector-push-extend (second indices) tex-vertex-indices)
              (vector-push-extend (third indices) normal-indices)))))))

(defun %mesh-add-wavefront-part (mesh name material vertices tex-coords normals
                                 vertex-indices tex-vertex-indices normal-indices)
  (dformat "Wavefront adding mesh part: ~S ~S~%" name material)
  (let ((indices (make-array (length vertex-indices))))
    (loop for i below (length vertex-indices)
         do (setf (aref indices i) i))
    (let ((shape (create-shape-from-arrays
                  indices
                  (%vertex-data-from-face-data vertices vertex-indices)
                  nil
                  nil;;(%vertex-data-from-face-data tex-coords tex-vertex-indices)
                  (%vertex-data-from-face-data normals normal-indices))))
      (unless (and normals (not (zerop (length normal-indices))))
        (shape-compute-normals shape))
      (mesh-add-part mesh :shape shape :material material :name name))))

;; Wavefront OBJ :material-lib asset
;; A material library contains multiple materiels
;; Loading such lib will just create a new material asset with
;; its name prefixed by the library name (e.g. matlib:some_material)
(defasset :material-lib '("mtl")
  ;; load
  (lambda (&key filename &allow-other-keys)
    (with-open-file (in filename :direction :input)
      (let ((mat nil)
            (name (symbol-name (gensym "MAT-"))))
        (loop for line = (read-line in nil)
           while line
           when (string/= line "")
           do (case (aref line 0)
                (#\# (dformat "Wavefront material comment: ~S~%" line))
                (otherwise
                 (let ((words (split-string line #\Space)))
                   (cond
                     ((string= (first words) "newmtl")
                      (dformat "Wavefront material: ~S~%" (second words))
                      (when mat
                        (use-asset name mat))
                      (setf mat (make-material)
                            name (second words)))
                     ((string= (first words) "Ns")
                      (setf (material-shininess mat) (read-from-string (second words))))
                     ((string= (first words) "Ka")
                      (setf (material-ambient mat)
                            (make-color :r (read-from-string (second words))
                                        :g (read-from-string (third words))
                                        :b (read-from-string (fourth words))
                                        :a 1.0)))
                     ((string= (first words) "Kd")
                      (setf (material-diffuse mat)
                            (make-color :r (read-from-string (second words))
                                        :g (read-from-string (third words))
                                        :b (read-from-string (fourth words))
                                        :a 1.0)))
                     ((string= (first words) "Ks")
                      (setf (material-specular mat)
                            (make-color :r (read-from-string (second words))
                                        :g (read-from-string (third words))
                                        :b (read-from-string (fourth words))
                                        :a 1.0)))
                     ((string= (first words) "d")
                      (setf (material-alpha mat) (read-from-string (second words))))
                     (t (dformat "Wavefront material ignoring: ~S~%" line)))))))
        (when mat
          (use-asset name mat)))))

  ;; unload
  (lambda (material)
    (declare (ignore material))
    (values)))