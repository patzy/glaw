(in-package #:glaw)

;; skeleton
(defstruct bone
  (angle 0.0 :type short-float)
  (length 0.0 :type float)
  (parent nil :type unsigned-byte)
  (children '()))

;; (defun bone-nb-children (bone)
;;   (length (bone-children bone)))

(define-anim-channels ((it bone) data)
  (:angle (setf (bone-angle it) data))
  (:length (setf (bone-length it) data)))

(defstruct skeleton
  (bones (make-array 1 :fill-pointer t :adjustable t :initial-element (make-bone))
         :type (vector bone))
  (names (make-array 1 :fill-pointer t :adjustable t :initial-element "root")
         :type (vector string)))

(defun skeleton-nb-bones (skel)
  (fill-pointer (skeleton-bones skel)))

(defun find-bone (skel name)
  (loop for i below (fill-pointer (skeleton-bones skel))
     when (string= (aref (skeleton-names skel) i) name)
     return (aref (skeleton-bones skel) i)))

(defun skeleton-add-bone (skel bone &key parent-name (parent-index 0)
                                         (name (format nil "bone-~d" (skeleton-nb-bones skel))))
  (format t "Adding bone ~S to ~S~%" bone (or parent-name parent-index))
  (let ((parent (if parent-name
                    (find-bone skel parent-name)
                    (aref (skeleton-bones skel) parent-index)))
        (index (vector-push-extend bone (skeleton-bones skel))))
    (push index (bone-children parent))
    (setf (bone-parent bone) parent))
  (vector-push-extend name (skeleton-names skel)))

(defun render-skeleton (skel &optional (root-index 0))
  (let ((node (aref (skeleton-bones skel) root-index)))
    (gl:push-matrix)
    (gl:rotate (bone-angle node) 0 0 1)
    (gl:begin :lines)
    (gl:vertex 0.0 0.0)
    (gl:vertex (bone-length node) 0.0)
    (gl:end)
    (gl:translate (bone-length node) 0 0)
    (loop for i in (bone-children node) do
       (render-skeleton skel i))
    (gl:pop-matrix)))

