(in-package #:glaw)

;; Game asset are loaded from files
(defvar *content-manager* nil)
(defvar *content-directory* nil)

(defun init-content-manager (&optional (content-dir ""))
  (setf *content-manager* (create-resource-manager))
  (setf *content-directory* content-dir))

(defun shutdown-content-manager ()
  (destroy-resource-manager *content-manager*)
  (setf *content-directory* nil))

(defvar *asset-loaders* (make-hash-table))

(defun load-asset (filename type &optional (auto-use nil))
  (format t "Loading asset of type ~S from ~S~%" type filename)
  (with-resource-manager *content-manager*
    (let ((loader (gethash type *asset-loaders* nil))
          (pathname (merge-pathnames *content-directory* filename)))
      (if loader
          (use-resource filename
                        (funcall (first loader) pathname) (second loader))
          (error "No asset loader defined for ~S~%" type)))))

(defun dispose-asset (filename)
  (with-resource-manager *content-manager*
    (drop-resource filename)))


(defun defasset (asset-type load &optional unload)
  (setf (gethash asset-type *asset-loaders*) (list load unload)))