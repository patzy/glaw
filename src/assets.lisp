(in-package #:glaw)

#+clisp ;; why ansi isn't default behavior?
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

(defun asset-loader-load (loader)
  (first loader))

(defun asset-loader-unload (loader)
  (second loader))

(defun asset-loader-extensions (loader)
  (third loader))

(defun asset-loader-extension-supported-p (loader extension)
  (or (eq :any (asset-loader-extensions loader))
      (member (string-upcase extension) (asset-loader-extensions loader)
              :test #'string-equal)))

(defun make-asset-loader (&key (load nil) (unload nil) (extensions :any))
  (list load unload (if (listp extensions)
                        (mapcar #'string-upcase extensions)
                        extensions)))

(defun supported-assets ()
  "Return list of supported assets types."
  (loop for k being the hash-keys of *asset-loaders* collect k))

(defun %asset-loader (type filename)
  (let ((loader (loop for l in (gethash type *asset-loaders* nil)
                   when (asset-loader-extension-supported-p l (pathname-type filename))
                   return l)))
    (unless loader
      (error "No asset loader defined for loading ~S as a ~S~%" filename type))
    loader))

(defun load-asset (filename type &optional (identifier filename))
  (format t "Loading asset of type ~S from ~S as ~S~%" type filename identifier)
  (with-resource-manager *content-manager*
    (when (existing-resource-p identifier)
      (warn "Asset already loaded or identifier conflict for ~A." identifier))
    (let* ((pathname (merge-pathnames filename *content-directory*))
           (loader (%asset-loader type pathname)))
      (if loader
          (use-resource identifier (funcall (asset-loader-load loader) pathname)
                        (asset-loader-unload loader))
          (error "No asset loader defined for ~S~%" type)))))

(defun dispose-asset (identifier)
  "Dispose designated asset regardless of its current users."
  (format t "Disposing of asset: ~S~%" identifier)
  (remove-resource *content-manager* identifier))

(defun dispose-assets (&rest ids)
  (dolist (it ids)
    (dispose-asset it)))

(defun defasset (asset-type load &optional unload (extensions :any))
  (push (make-asset-loader :load load :unload unload
                           :extensions extensions)
        (gethash asset-type *asset-loaders*)))
