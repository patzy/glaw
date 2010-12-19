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
(defstruct navmesh-cell
  neighbors
  vertices)

(defun navmesh-cell-add-vertex (cell x y)
  (push (list x y) (navmesh-cell-vertices cell)))

(defun navmesh-cell-edges (cell)
  (loop for v1 in (navmesh-cell-vertices cell) by #'cdr
       for v2 in (rotate-list (navmesh-cell-vertices cell)) by #'cdr
       collect (list v1 v2)))

(defun navmesh-cell-center (c)
  ;; barycenter of the polygon
  (let ((x-list (mapcar #'first (navmesh-cell-vertices c)))
        (y-list (mapcar #'second (navmesh-cell-vertices c))))
    (list (/ (reduce '+ x-list) (length x-list))
          (/ (reduce '+ y-list) (length y-list)))))

(defun edge-equal-p (edge-1 edge-2)
  "Compare contents of edges regardless of the vertices order."
  (or (and (equal (first edge-1) (first edge-2))
           (equal (second edge-1) (second edge-2)))
      (and (equal (first edge-1) (second edge-2))
           (equal (second edge-1) (first edge-2)))))

(defun adjacent-edges (cell-1 cell-2)
  "Returns the list of shared edges between cell-1 and cell-2."
  (let ((edges1 (navmesh-cell-edges cell-1))
        (edges2 (navmesh-cell-edges cell-2)))
    ;; compare every cell to each other
    (loop for e1 in edges1
       when (find-if (lambda (item)
                       (edge-equal-p e1 item))
                     edges2)
       collect e1)))

(defun adjacent-p (cell-1 cell-2)
  (not (null (adjacent-edges cell-1 cell-2))))

(defun merge-cells (cell-1 cell-2)
  "Merge two adjacent cells and return the resulting cell."
  ;; test:
  ;; '((10 30) (20 40) (30 40) (40 30) (40 20) (20 20))
  ;; '((20 20) (40 20) (40 10) (30 10))
  (assert (adjacent-p cell-1 cell-2))
  ;; XXX: we need to be sure there's only one edge !!!!
  (let ((shared-edge (first (adjacent-edges cell-1 cell-2))))
    (let ((merged-cell (copy-navmesh-cell cell-1))
          (first-vertex (first shared-edge))
          (last-vertex (second shared-edge)))
      ;; shared edge is from cell-1 (see adjacent-edges func)
      ;; this means we just have to insert all vertices from cell-2
      ;; to merged-cell (cell-1's copy) right after last-vertex
      ;; except for vertices in shared-edge
      (let ((vertices-to-add (rotate-list (navmesh-cell-vertices cell-2)
                                    :distance (- (position first-vertex
                                                           (navmesh-cell-vertices cell-2)
                                                           :test 'equal)
                                                 1))))
        ;; remove the common vertices from vertices-to-add
        (setf vertices-to-add (cddr vertices-to-add))
        ;; merge the right vertices at the right place
        (setf (navmesh-cell-vertices merged-cell)
              (list-insert (navmesh-cell-vertices merged-cell)
                           vertices-to-add
                           (position last-vertex (navmesh-cell-vertices merged-cell)
                                     :test 'equal)))
        ;; remove vertices on straight lines etc...
        (setf (navmesh-cell-vertices merged-cell)
              (simplify-vertices (navmesh-cell-vertices merged-cell)))
        ;; update cell-1 and cell-2 neighborhood
        (setf (navmesh-cell-neighbors cell-1) (remove cell-2 (navmesh-cell-neighbors cell-1)))
        (setf (navmesh-cell-neighbors cell-2) (remove cell-1 (navmesh-cell-neighbors cell-2)))
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
        ;; set neighbors correctly
        ;; this means removing cell-2 from the current neighbors list
        ;; and add all cell-2's neighbors except cell-1
        (setf (navmesh-cell-neighbors merged-cell)
              (remove cell-2 (navmesh-cell-neighbors merged-cell)))
        (loop for n in (navmesh-cell-neighbors cell-2)
             unless (eq n cell-1)
             do (push n (navmesh-cell-neighbors merged-cell))))
      merged-cell)))

(defun edge-vectors (vertices)
  (let ((edge-vectors (loop for v1 in vertices by #'cdr
                         for v2 in (cdr vertices) by #'cdr
                         collect (vector-2d-diff (make-vector-2d-from-list v1)
                                       (make-vector-2d-from-list v2)))))
    (push (vector-2d-diff (make-vector-2d-from-list (first (last vertices)))
                (make-vector-2d-from-list (first vertices))) edge-vectors)
    edge-vectors))

(defun edges-perp-products (edge-vectors)
  (loop for v1 in edge-vectors by #'cdr
     for v2 in (rotate-list edge-vectors)
     collect (vector-2d-perp-dot-product v1 v2)))

(defun simplify-vertices (vertices)
  (let* ((vecs (edge-vectors vertices))
         (perp-products (edges-perp-products vecs))
         (res-vertices '()))
    (loop for p in perp-products
         with current-pos = 0
         do (progn (unless (zerop p)
                     (push (nth current-pos vertices)
                           res-vertices))
                   (incf current-pos)))
    (setf res-vertices (reverse res-vertices))
    res-vertices))

(defun convex-p (vertices)
  ;; using formulas described here:
  ;; http://mathworld.wolfram.com/ConvexPolygon.html
  ;; e.g. perp-dot-product between two consecutive
  ;; edge vectors
  ;; tests (NIL T):
  ;; '((100 100) (100 200) (200 300) (300 200) (200 200) (200 100))
  ;; '((100 100) (100 200) (200 300) (300 200) (200 100))
  ;; XXX: this may be written better
  (let*  ((vecs (edge-vectors vertices))
          (perp-products (edges-perp-products vecs))
          (first-sign (sign-of (first perp-products))))
    (every (lambda (item)
             (or (eq (sign-of item) first-sign)
                 (zerop item)))
           perp-products)))

(defun navmesh-cell-inside-p (c x y)
  ;; FIXME: only square cell here
  (let ((xmin (loop for v in (navmesh-cell-vertices c)
                   minimize (first v)))
        (ymin (loop for v in (navmesh-cell-vertices c)
                   minimize (second v)))
        (xmax (loop for v in (navmesh-cell-vertices c)
                   maximize (first v)))
        (ymax (loop for v in (navmesh-cell-vertices c)
                   maximize (second v))))
  (and (< x xmax) (< y ymax) (> x xmin) (> y ymin))))

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
  (let* ((start-coords (navmesh-cell-center start-cell))
         (end-coords (navmesh-cell-center end-cell))
         (dx (- (first end-coords) (first start-coords)))
         (dy (- (second end-coords) (second start-coords))))
  (sqrt (+ (* dx dx) (* dy dy)))))

(defun navmesh-connect-cells (cell-1 cell-2 &optional bi-directional)
    (push cell-2 (navmesh-cell-neighbors cell-1))
    (when bi-directional
      (push cell-1 (navmesh-cell-neighbors cell-2))))

(defun navmesh-cells-connected-p (cell-1 cell-2)
  "Returns T if you can go from CELL-1 to CELL-2."
  (find cell-2 (navmesh-cell-neighbors cell-1)))

(defun create-grid-navmesh (width height cell-size)
  "Creates naive grid navigation mesh."
  (let ((nv (make-navmesh)))
    (loop for y below (* height cell-size) by cell-size
         do (loop for x below (* width cell-size) by cell-size
                 do (let ((cell (make-navmesh-cell)))
                      (navmesh-cell-add-vertex cell x y)
                      (navmesh-cell-add-vertex cell (+ x cell-size) y)
                      (navmesh-cell-add-vertex cell (+ x cell-size) (+ y cell-size))
                      (navmesh-cell-add-vertex cell x (+ y cell-size))
                      (setf (navmesh-cell-vertices cell) (reverse (navmesh-cell-vertices cell)))
                      (push cell (navmesh-cells nv)))))
    nv))

(defun connect-grid-navmesh (nv cell-size)
  "Make bi-directional connections between all cells of a complete grid navmesh."
  (format t "Connecting grid navigation mesh")
  (loop for c in (navmesh-cells nv)
       do (let* ((center (navmesh-cell-center c))
                 (w (navstruct-node-at nv (- (first center) cell-size)
                                             (second center)))
                 (n (navstruct-node-at nv (first center)
                                             (- (second center) cell-size)))
                 (s (navstruct-node-at nv (first center)
                                             (+ (second center) cell-size)))
                 (e (navstruct-node-at nv (+ (first center) cell-size)
                                             (second center))))
            (format t ".")
            (when w
              (navmesh-connect-cells c w))
            (when n
              (navmesh-connect-cells c n))
            (when e
              (navmesh-connect-cells c e))
            (when s
              (navmesh-connect-cells c s))))
  (format t "done~%"))


(defun adjacent-neighbor (cell)
  (loop for c in (navmesh-cell-neighbors cell)
         when (adjacent-p cell c)
         return c))

(defun find-adjacent-cells (nv)
  (loop for c1 in (navmesh-cells nv)
       when (adjacent-neighbor c1)
       return (values c1 (adjacent-neighbor c1))))


(defun simplify-navmesh (nv)
  "Simplify the provided navigation mesh trying to merge adjacent cells."
  ;; try to find a pair of adjacent cells that may be merged
  (format t "Merging adjacent cells")
  (let ((nb-merged 0))
    (loop with continue? = t
       while continue?
       do (multiple-value-bind (cell-1 cell-2)
              (find-adjacent-cells nv)
            (format t ".")
            (if (and cell-1 cell-2)
                (let ((merged-cell (merge-cells cell-1 cell-2)))
                  ;;(format t "Merged cell: ~S~%" merged-cell)
                  (when  (convex-p (navmesh-cell-vertices merged-cell))
                    (incf nb-merged)
                    (setf (navmesh-cells nv) (remove cell-1 (navmesh-cells nv)))
                    (setf (navmesh-cells nv) (remove cell-2 (navmesh-cells nv)))
                    (push merged-cell (navmesh-cells nv))))
                (setf continue? nil)))))
  (format t "done~%"))


(defun render-navmesh-cell (c)
  (select-texture nil)
  (gl:color 1 1 1)
  (gl:with-primitive :line-loop
    (loop for v in (navmesh-cell-vertices c)
       do (gl:vertex (first v)
                     (second v))))
  (let ((center (navmesh-cell-center c)))
    ;; WTF? :line works on the laptop but not on the desktop !!!
    (gl:with-primitive :lines
      (loop for n in (navmesh-cell-neighbors c)
         do (let ((center-2 (navmesh-cell-center n)))
              (gl:color 1 0 0)
              (gl:vertex (first center) (second center))
              (gl:vertex (first center-2) (second center-2)))))))

(defun render-navmesh (nv)
  (loop for c in (navmesh-cells nv)
       do (render-navmesh-cell c)))

