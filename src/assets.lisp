(in-package #:glaw)

#+clisp ;; why ansi isn"t default behavior?
(setf custom:*merge-pathnames-ansi* t)

;; Game asset are loaded from files
(defvar *content-manager* nil)
(defvar *content-directory* nil)

(defun init-content-manager (&optional (content-dir ""))
  (format t "Initializing content manager with ~S~%" content-dir)
  (setf *content-manager* (create-resource-manager))
  (setf *content-directory* content-dir))

(defun shutdown-content-manager ()
  (destroy-resource-manager *content-manager*)
  (setf *content-directory* nil))

(defvar *asset-loaders* (make-hash-table))

(defun load-asset (filename type &optional (identifier filename))
  (format t "Loading asset of type ~S from ~S~%" type filename)
  (with-resource-manager *content-manager*
    (let ((loader (gethash type *asset-loaders* nil))
          (pathname (merge-pathnames filename *content-directory*)))
      (if loader
          (use-resource identifier (funcall (first loader) pathname) (second loader))
          (error "No asset loader defined for ~S~%" type)))))

(defun dispose-asset (identifier)
  "Dispose designated asset regardless of its current users."
  (format t "Disposing of asset: ~S~%" identifier)
  (remove-resource *content-manager* identifier))

(defun dispose-assets (&rest ids)
  (dolist (it ids)
    (dispose-asset it)))

(defun defasset (asset-type load &optional unload)
  (setf (gethash asset-type *asset-loaders*) (list load unload)))