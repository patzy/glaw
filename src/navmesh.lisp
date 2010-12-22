(in-package #:glaw)

;;; Navigation structure protocol
(defgeneric navstruct-nodes-dist (nv start end)
  (:documentation "Computes distance between START and END nodes in whatever
way seems appropriate."))

(defgeneric navstruct-node-at (nv x y)
  (:documentation "Returns the navigation structure node at the specified position."))

(defgeneric navstruct-node-neighbors (nv node)
  (:documentation "Returns a list of nodes connected to the provided NODE."))

(defgeneric navstruct-nodes-connected-p (nv node-1 node-2)
  (:documentation "Returns NIL if there's no connection from NODE-1 to NODE-2."))

;;; A* pathfinding
;;; Requires an implementation of the navstruct protocol
(defun %lowest-cost-node (node-list costs-table)
  (let ((node (first node-list))
        (cost (gethash (first node-list) costs-table)))
    (loop for n in (cdr node-list)
         when (< (gethash n costs-table) cost)
         do (setf cost (gethash n costs-table)
                  node n))
    node))

(defun %reconstruct-path (current-node prevs)
  (if (gethash current-node prevs)
      (append (list current-node) (%reconstruct-path (gethash current-node prevs) prevs))
      (list current-node)))

(defun find-path (nv start-x start-y end-x end-y &key (g-func #'navstruct-nodes-dist)
                                                      (h-func #'navstruct-nodes-dist))
  "Find a path from (START-X;START-Y) to (END-X;END-Y) if possible."
  (let ((start-node (navstruct-node-at nv start-x start-y))
        (end-node (navstruct-node-at nv end-x end-y)))
    (assert (and start-node end-node))
    (find-path/nodes nv start-node end-node g-func h-func)))

(defun find-path/nodes (nv start-node end-node &optional (g-func #'navstruct-nodes-dist)
                                                         (h-func #'navstruct-nodes-dist))
  "Find a path from START-NODE to END-NODE using A star algorithm on the provided navigation mesh."
  (let* ((closed '())
         (opened '())
         (prevs (make-hash-table :test #'eq))
         (f (make-hash-table :test #'eq))
         (g (make-hash-table :test #'eq))
         (h (make-hash-table :test #'eq)))
    (push start-node opened)
    (setf (gethash start-node prevs) nil
          (gethash start-node g) 0
          (gethash start-node h) (funcall h-func nv start-node end-node)
          (gethash start-node f) (funcall h-func nv start-node end-node))
    (loop while opened
       do (let ((current-node (%lowest-cost-node opened f)))
            (when (eq current-node end-node)
              (return-from find-path/nodes (%reconstruct-path end-node prevs)))
            (setf opened (remove current-node opened))
            (push current-node closed)
            (loop for n in (navstruct-node-neighbors nv current-node)
               when (not (member n closed))
               do (let ((tentative-g (+ (gethash current-node g)
                                        (funcall g-func nv current-node n)))
                        (tentative-better nil))
                    (cond
                      ((and (not (member n closed)) (not (member n opened)))
                       (push n opened)
                       (setf tentative-better t))
                      ((< tentative-g (+ (gethash (find n opened) g)
                                         (funcall g-func nv current-node n)))
                       (setf tentative-better t))
                      (t  (setf tentative-better nil)))
                    (when tentative-better
                      (setf (gethash n prevs) current-node
                            (gethash n g) tentative-g
                            (gethash n h) (funcall h-func nv n end-node)
                            (gethash n f) (+ tentative-g (funcall h-func nv n end-node)))))))))
  ;; no path
  nil)

;;; Navigation mesh
(defstruct (navmesh-cell (:constructor %make-navmesh-cell))
  neighbors
  polygon)

(defun make-navmesh-cell ()
  (%make-navmesh-cell :neighbors '()
                      :polygon (make-polygon)))

(defun navmesh-cell-add-vertex (cell x y)
  (polygon-add-vertex (navmesh-cell-polygon cell) (make-point-2d :x x :y y)))

(defun navmesh-cell-add-vertices (cell &rest verts)
  (apply #'polygon-add-vertices (navmesh-cell-polygon cell)
         (loop for x in verts by #'cddr
            for y in (cdr verts) by #'cddr
            collect (make-point-2d :x x :y y))))

(defun navmesh-cell-edges (cell)
  (polygon-edges (navmesh-cell-polygon cell)))

(defun navmesh-cell-center (cell)
  (polygon-centroid (navmesh-cell-polygon cell)))

(defun navmesh-cells-connected-p (cell-1 cell-2)
  "Returns T if you can go from CELL-1 to CELL-2."
  (find cell-2 (navmesh-cell-neighbors cell-1)))

(defun navmesh-cell-inside-p (c x y)
  (polygon-point-inside-p (navmesh-cell-polygon c) (make-point-2d :x x :y y)))

(defstruct navmesh
  cells)

(defmethod navstruct-node-neighbors ((nv navmesh) (node navmesh-cell))
  (declare (ignore nv))
  (navmesh-cell-neighbors node))

(defun navmesh-cell (nv index)
  (nth index (navmesh-cells nv)))

(defun navmesh-nb-cells (nv)
  (length (navmesh-cells nv)))

(defun navmesh-remove-cell-at (nv x y)
  (setf (navmesh-cells nv)
        (remove-if (lambda (cell)
                     (navmesh-cell-inside-p cell x y))
                   (navmesh-cells nv))))

(defmethod navstruct-node-at ((nv navmesh) x y)
  (find-if (lambda (cell)
             (navmesh-cell-inside-p cell x y)) (navmesh-cells nv)))

(defmethod navstruct-nodes-dist ((nv navmesh) start-cell end-cell)
  (declare (ignore nv))
  (let ((start-cell-pos (navmesh-cell-center start-cell))
        (end-cell-pos (navmesh-cell-center end-cell)))
    (vector-2d-mag (vector-2d-diff end-cell-pos start-cell-pos))))

(defun navmesh-connect-cells (cell-1 cell-2 &optional bi-directional)
    (push cell-2 (navmesh-cell-neighbors cell-1))
    (when bi-directional
      (push cell-1 (navmesh-cell-neighbors cell-2))))

(defun adjacent-neighbor (cell)
  (loop for c in (navmesh-cell-neighbors cell)
         when (polygons-adjacent-p (navmesh-cell-polygon cell)
                                   (navmesh-cell-polygon c))
         return c))

(defun find-adjacent-cells (nv)
  "Returns first pair of adjacent cells found."
  (loop for c1 in (navmesh-cells nv)
       when (adjacent-neighbor c1)
       return (values c1 (adjacent-neighbor c1))))

(defun navmesh-cells-merge (cell-1 cell-2)
  "Merge two adjacent cells and return the resulting cell."
  (let ((merged-cell (copy-navmesh-cell cell-1))
        (merged-poly (polygons-merge (navmesh-cell-polygon cell-1) (navmesh-cell-polygon cell-2))))
    (unless merged-poly
      (error "Can't merge cells."))
    (setf (navmesh-cell-polygon merged-cell) merged-poly)
    ;; remove merged cells from neighbors list
    ;; don't forget to add the merged cell as a new neighbor
    (mapcar (lambda (item)
              (push merged-cell (navmesh-cell-neighbors item))
              (setf (navmesh-cell-neighbors item)
                    (remove cell-1 (navmesh-cell-neighbors item))))
            (navmesh-cell-neighbors cell-1))
    (mapcar (lambda (item)
              (push merged-cell (navmesh-cell-neighbors item))
              (setf (navmesh-cell-neighbors item)
                    (remove cell-2 (navmesh-cell-neighbors item))))
            (navmesh-cell-neighbors cell-2))
    ;; set merged cell neighbors correctly
    ;; this means removing cell-2 from the current neighbors list
    ;; and add all cell-2's neighbors except cell-1
    (setf (navmesh-cell-neighbors merged-cell)
          (remove cell-2 (navmesh-cell-neighbors merged-cell)))
    (loop for n in (navmesh-cell-neighbors cell-2)
       unless (eq n cell-1)
       do (push n (navmesh-cell-neighbors merged-cell)))
    merged-cell))

(defun simplify-navmesh (nv)
  "Simplify the provided navigation mesh trying to merge adjacent cells."
  ;; try to find a pair of adjacent cells that may be merged
  (dformat "Merging adjacent cells")
  (let ((nb-merged 0))
    (loop with continue? = t
       while continue?
       do (multiple-value-bind (cell-1 cell-2)
              (find-adjacent-cells nv)
            (format t ".")
            (if (and cell-1 cell-2)
                (let ((merged-cell (navmesh-cells-merge cell-1 cell-2)))
                  (when  (polygon-convex-p (navmesh-cell-polygon merged-cell))
                    (incf nb-merged)
                    (setf (navmesh-cells nv) (remove cell-1 (navmesh-cells nv)))
                    (setf (navmesh-cells nv) (remove cell-2 (navmesh-cells nv)))
                    (push merged-cell (navmesh-cells nv))))
                (setf continue? nil)))))
  (dformat "done~%"))

;; Simple grid navigation mesh
(defun create-grid-navmesh (width height cell-size)
  "Creates naive grid plane navigation mesh with its origin at (0;0)."
  (let ((nv (make-navmesh)))
    (loop for y below (* height cell-size) by cell-size
         do (loop for x below (* width cell-size) by cell-size
                 do (let ((cell (make-navmesh-cell)))
                      (navmesh-cell-add-vertices cell x y
                                                      (+ x cell-size) y
                                                      (+ x cell-size) (+ y cell-size)
                                                      x (+ y cell-size))
                      (push cell (navmesh-cells nv)))))
    nv))

(defun connect-grid-navmesh (nv cell-size)
  "Make bi-directional connections between all cells of a complete grid navmesh."
  (dformat "Connecting grid navigation mesh")
  (loop for c in (navmesh-cells nv)
       do (let* ((center (navmesh-cell-center c))
                 (center-x (point-2d-x center))
                 (center-y (point-2d-y center))
                 (w (navstruct-node-at nv (- center-x cell-size)
                                             center-y))
                 (n (navstruct-node-at nv center-x
                                       (- center-y cell-size)))
                 (s (navstruct-node-at nv center-x
                                       (+ center-y cell-size)))
                 (e (navstruct-node-at nv (+ center-x cell-size)
                                       center-y)))
            (dformat ".")
            (when w
              (navmesh-connect-cells c w))
            (when n
              (navmesh-connect-cells c n))
            (when e
              (navmesh-connect-cells c e))
            (when s
              (navmesh-connect-cells c s))))
  (dformat"done~%"))


