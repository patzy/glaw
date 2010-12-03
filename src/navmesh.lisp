(in-package #:glaw)

(defstruct navcell
  (neighbors '())
  (vertices '()))

(defun navcell-add-vertex (cell x y)
  (push (list x y) (navcell-vertices cell)))

(defun navcell-edges (cell)
  (loop for v1 in (navcell-vertices cell) by #'cdr
       for v2 in (rotate-list (navcell-vertices cell)) by #'cdr
       collect (list v1 v2)))

(defun navcell-center (c)
  ;; barycenter of the polygon
  (let ((x-list (mapcar #'first (navcell-vertices c)))
        (y-list (mapcar #'second (navcell-vertices c))))
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
  (let ((edges1 (navcell-edges cell-1))
        (edges2 (navcell-edges cell-2)))
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
  ;;(format t "Merging ~S and ~S~%" (vertices cell-1) (vertices cell-2))
  ;; XXX: we need to be sure there's only one edge !!!!
  (let ((shared-edge (first (adjacent-edges cell-1 cell-2))))
    ;;(format t "Shared edge: ~S~%" shared-edge)
    (let ((merged-cell (copy-navcell cell-1))
          (first-vertex (first shared-edge))
          (last-vertex (second shared-edge)))
      ;; shared edge is from cell-1 (see adjacent-edges func)
      ;; this means we just have to insert all vertices from cell-2
      ;; to merged-cell (cell-1's copy) right after last-vertex
      ;; except for vertices in shared-edge
      ;;(format t "First shared vertex: ~S~%" first-vertex)
      ;;(format t "Last shared vertex: ~S~%" last-vertex)
      ;;(format t "Rotate distance: ~S~%" (position first-vertex
      ;;                                              (vertices cell-2)
      ;;                                              :test 'equal))
      ;;(format t "Vertices to rotate: ~S~%" (vertices cell-2))
      (let ((vertices-to-add (rotate-list (navcell-vertices cell-2)
                                    :distance (- (position first-vertex
                                                           (navcell-vertices cell-2)
                                                           :test 'equal)
                                                 1))))
        ;; remove the common vertices from vertices-to-add
        (setf vertices-to-add (cddr vertices-to-add))
        ;;(format t "Vertices from cell-2 to add: ~S~%" vertices-to-add)
        ;; merge the right vertices at the right place
        (setf (navcell-vertices merged-cell)
              (list-insert (navcell-vertices merged-cell)
                           vertices-to-add
                           (position last-vertex (navcell-vertices merged-cell)
                                     :test 'equal)))
        ;; remove vertices on straight lines etc...
        (setf (navcell-vertices merged-cell)
              (simplify-vertices (navcell-vertices merged-cell)))
        ;;(format t "~S's neighbors: ~S~%" cell-1 (neighbors cell-1))
        ;;(format t "~S's neighbors: ~S~%" cell-2 (neighbors cell-2))
        ;; update cell-1 and cell-2 neighborhood
        (setf (navcell-neighbors cell-1) (remove cell-2 (navcell-neighbors cell-1)))
        (setf (navcell-neighbors cell-2) (remove cell-1 (navcell-neighbors cell-2)))
        ;; remove merged cells from neighbors list
        ;; don't forget to add the merged cell as a new neighbor
        (mapcar (lambda (item)
                  (push merged-cell (navcell-neighbors item))
                  (setf (navcell-neighbors item)
                        (remove cell-1 (navcell-neighbors item))))
                (navcell-neighbors cell-1))
        (mapcar (lambda (item)
                  (push merged-cell (navcell-neighbors item))
                  (setf (navcell-neighbors item)
                        (remove cell-2 (navcell-neighbors item))))
                (navcell-neighbors cell-2))
        ;;(format t "~S's neighbors to merge: ~S~%" cell-1 (neighbors cell-1))
        ;;(format t "~S's neighbors to merge: ~S~%" cell-2 (neighbors cell-2))
        ;; set neighbors correctly
        ;; this means removing cell-2 from the current neighbors list
        ;; and add all cell-2's neighbors except cell-1
        (setf (navcell-neighbors merged-cell) (remove cell-2 (navcell-neighbors merged-cell)))
        (loop for n in (navcell-neighbors cell-2)
             unless (eq n cell-1)
             do (push n (navcell-neighbors merged-cell)))
        ;;(format t "Merged vertices: ~S~%" (vertices merged-cell))
        ;;(format t "Merged neighbors: ~S~%" (neighbors merged-cell))
)
      ;;(format t "Convex result? ~S~%" (convex-p (vertices merged-cell)))
      merged-cell)))

(defun edge-vectors (vertices)
  (let ((edge-vectors (loop for v1 in vertices by #'cdr
                         for v2 in (cdr vertices) by #'cdr
                         collect (vec-diff (vec-from-lst v1)
                                       (vec-from-lst v2)))))
    (push (vec-diff (vec-from-lst (first (last vertices)))
                (vec-from-lst (first vertices))) edge-vectors)
    edge-vectors))

(defun edges-perp-products (edge-vectors)
  (loop for v1 in edge-vectors by #'cdr
     for v2 in (rotate-list edge-vectors)
     collect (vec-perp-dot-product v1 v2)))

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
    ;;    (format t "Simplified vertices: ~S~%" res-vertices)
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
;;    (format t "Perp products: ~S~%" perp-products)
    (every (lambda (item)
             (or (eq (sign-of item) first-sign)
                 (zerop item)))
           perp-products)))

(defun navcell-inside-p (c x y)
  ;; FIXME: only square cell here
  (let ((xmin (loop for v in (navcell-vertices c)
                   minimize (first v)))
        (ymin (loop for v in (navcell-vertices c)
                   minimize (second v)))
        (xmax (loop for v in (navcell-vertices c)
                   maximize (first v)))
        (ymax (loop for v in (navcell-vertices c)
                   maximize (second v))))
  (and (< x xmax) (< y ymax) (> x xmin) (> y ymin))))

(defstruct navmesh
  (cells '()))

(defun navmesh-cell (nv index)
  (nth index (navmesh-cells nv)))

(defun navmesh-nb-cells (nv)
  (length (navmesh-cells nv)))

(defun navmesh-remove-cell-at (nv x y)
  (format t "Removing cell containing ~S; ~S~%" x y)
  (setf (navmesh-cells nv)
        (remove-if (lambda (cell)
                     (navcell-inside-p cell x y))
                   (navmesh-cells nv))))

(defun navmesh-containing-cell (nv x y)
  (find-if (lambda (cell)
             (navcell-inside-p cell x y)) (navmesh-cells nv)))

(defun navmesh-cell-exist-p (nv x y)
  (navmesh-containing-cell nv x y))

(defun navmesh-cells-dist (nv start-cell end-cell)
  (declare (ignore nv))
  (let* ((start-coords (navcell-center start-cell))
         (end-coords (navcell-center end-cell))
         (dx (- (first end-coords) (first start-coords)))
         (dy (- (second end-coords) (second start-coords))))
  (sqrt (+ (* dx dx) (* dy dy)))))

(defun connect-cells (cell-1 cell-2 &optional bi-directional)
;;;     (format t "Connecting cells: ~S ; ~S (bi-directional? ~S)~%"
;;;             cell-1 cell-2 bi-directional)
    (push cell-2 (navcell-neighbors cell-1))
    (when bi-directional
      (push cell-1 (navcell-neighbors cell-2))))

(defun navcells-connected-p (cell-1 cell-2)
  "Returns T if cell-1 is connected to cell-2."
  (find cell-2 (navcell-neighbors cell-1)))

(defun create-grid-navmesh (width height cell-size)
  (let ((nv (make-navmesh)))
    (loop for y below (* height cell-size) by cell-size
         do (loop for x below (* width cell-size) by cell-size
                 do (let ((cell (make-navcell)))
                      (navcell-add-vertex cell x y)
                      (navcell-add-vertex cell (+ x cell-size) y)
                      (navcell-add-vertex cell (+ x cell-size) (+ y cell-size))
                      (navcell-add-vertex cell x (+ y cell-size))
                      (setf (navcell-vertices cell) (reverse (navcell-vertices cell)))
                      (push cell (navmesh-cells nv)))))
    nv))

(defun connect-grid-navmesh (nv cell-size)
  "Make bi-directional connections between cells in a grid-like navmesh."
  (format t "Connecting grid navigation mesh")
  (loop for c in (navmesh-cells nv)
       do (let* ((center (navcell-center c))
                 (w (navmesh-containing-cell nv (- (first center) cell-size)
                                             (second center)))
                 (n (navmesh-containing-cell nv (first center)
                                             (- (second center) cell-size)))
                 (s (navmesh-containing-cell nv (first center)
                                             (+ (second center) cell-size)))
                 (e (navmesh-containing-cell nv (+ (first center) cell-size)
                                             (second center))))
            (format t ".")
            (when w
              (connect-cells c w))
            (when n
              (connect-cells c n))
            (when e
              (connect-cells c e))
            (when s
              (connect-cells c s))))
  (format t "done~%"))


(defun adjacent-neighbor (cell)
  (loop for c in (navcell-neighbors cell)
         when (adjacent-p cell c)
         return c))

(defun find-adjacent-cells (nv)
  (loop for c1 in (navmesh-cells nv)
       when (adjacent-neighbor c1)
       return (values c1 (adjacent-neighbor c1))))


(defun simplify-navmesh (nv)
  "Simplify the provided navigation mesh trying to merge adjacent cells."
  ;; try to find a pair of adjacent cells that may be merged
  ;;(format t "Cells:~S~%" (find-adjacent-cells nv))
  (format t "Merging adjacent cells")
  (let ((nb-merged 0))
    (loop with continue? = t
       while continue?
       do (multiple-value-bind (cell-1 cell-2)
              (find-adjacent-cells nv)
            (format t ".")
            ;;(format t "Cells to merge: ~S and ~S~%" cell-1 cell-2)
            (if (and cell-1 cell-2)
                (let ((merged-cell (merge-cells cell-1 cell-2)))
                  ;;(format t "Merged cell: ~S~%" merged-cell)
                  (when  (convex-p (navcell-vertices merged-cell))
                    ;;(format t "Merged ~S and ~S~%" cell-1 cell-2)
                    (incf nb-merged)
                    (setf (navmesh-cells nv) (remove cell-1 (navmesh-cells nv)))
                    (setf (navmesh-cells nv) (remove cell-2 (navmesh-cells nv)))
                    (push merged-cell (navmesh-cells nv))))
                (setf continue? nil)))))
  (format t "done~%"))


(defun render-navcell (c)
  (select-texture nil)
  (gl:color 1 1 1)
  (gl:with-primitive :line-loop
    (loop for v in (navcell-vertices c)
       do (gl:vertex (first v)
                     (second v))))
  (let ((center (navcell-center c)))
    ;; WTF? :line works on the laptop but not on the desktop !!!
    (gl:with-primitive :lines
      (loop for n in (navcell-neighbors c)
         do (let ((center-2 (navcell-center n)))
              (gl:color 1 0 0)
              (gl:vertex (first center) (second center))
              (gl:vertex (first center-2) (second center-2)))))))

(defun render-navmesh (nv)
  (loop for c in (navmesh-cells nv)
       do (render-navcell c)))


;;; pathfinding
(defun %lowest-cost-cell (cell-list costs)
  (let ((cell (first cell-list))
        (cost (gethash (first cell-list) costs)))
    (loop for c in (cdr cell-list)
         when (< (gethash c costs) cost)
         do (setf cost (gethash c costs)
                  cell c))
    cell))

(defun %reconstruct-path (current-cell prevs)
  (if (gethash current-cell prevs)
      (append (list current-cell) (%reconstruct-path (gethash current-cell prevs) prevs))
      (list current-cell)))

(defun find-path (nv start-x start-y end-x end-y &optional (g-func #'navmesh-cells-dist)
                                                           (h-func #'navmesh-cells-dist))
  (let ((start-cell (navmesh-containing-cell nv start-x start-y))
        (end-cell (navmesh-containing-cell nv end-x end-y)))
    (assert (and start-cell end-cell))
    (find-path/cells nv start-cell end-cell g-func h-func)))

(defun find-path/cells (nv start-cell end-cell &optional (g-func #'navmesh-cells-dist)
                                                         (h-func #'navmesh-cells-dist))
  "Find a path from start to end using A star algorithm on the provided navigation mesh."
  (let* ((closed '())
         (opened '())
         (prevs (make-hash-table :test #'eq))
         (f (make-hash-table :test #'eq))
         (g (make-hash-table :test #'eq))
         (h (make-hash-table :test #'eq)))
    (push start-cell opened)
    (setf (gethash start-cell prevs) nil
          (gethash start-cell g) 0
          (gethash start-cell h) (funcall h-func nv start-cell end-cell)
          (gethash start-cell f) (funcall h-func nv start-cell end-cell))
    (loop while opened
       do (let ((current-cell (%lowest-cost-cell opened f)))
            (when (eq current-cell end-cell)
              (return-from find-path/cells (%reconstruct-path end-cell prevs)))
            (setf opened (remove current-cell opened))
            (push current-cell closed)
            (loop for n in (navcell-neighbors current-cell)
               when (not (member n closed))
               do (let ((tentative-g (+ (gethash current-cell g)
                                        (funcall g-func nv current-cell n)))
                        (tentative-better nil))
                    (cond
                      ((and (not (member n closed)) (not (member n opened)))
                       (push n opened)
                       (setf tentative-better t))
                      ((< tentative-g (+ (gethash (find n opened) g)
                                         (funcall g-func nv current-cell n)))
                       (setf tentative-better t))
                      (t  (setf tentative-better nil)))
                    (when tentative-better
                      (setf (gethash n prevs) current-cell
                            (gethash n g) tentative-g
                            (gethash n h) (funcall h-func nv n end-cell)
                            (gethash n f) (+ tentative-g (funcall h-func nv n end-cell)))))))))
  ;; no path
  nil)