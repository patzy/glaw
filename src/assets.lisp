(in-package #:glaw)

#+clisp ;; why ansi isn't default behavior?
(setf custom:*merge-pathnames-ansi* t)

;;; Asset loaders
(defvar *asset-loaders* (make-hash-table))

(defun make-asset-loader (&key (load nil) (unload nil) (extensions :any))
  "Returns a new asset loader."
  (list load unload (if (listp extensions)
                        (mapcar #'string-upcase extensions)
                        extensions)))

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

(defun supported-assets ()
  "Return list of supported assets types."
  (loop for k being the hash-keys of *asset-loaders* collect k))

(defun %asset-loader (type filename)
  "Try to find a valid asset loader for the specified TYPE and FILENAME."
  (let ((loader (loop for l in (gethash type *asset-loaders* nil)
                   when (asset-loader-extension-supported-p l (pathname-type filename))
                   return l)))
    (unless loader
      (error "No asset loader defined for loading ~S as a ~S~%" filename type))
    loader))

(defun defasset (asset-type (extensions :any) load &optional unload)
  "Defines an asset loader for ASSET-TYPE."
  (push (make-asset-loader :load load :unload unload
                           :extensions extensions)
        (gethash asset-type *asset-loaders*)))

;;; Content manager singleton
(defvar *content-manager* nil)
(defvar *content-directory* nil)
(defvar *assets* nil)

(defstruct asset
  type
  name
  filename
  properties)

(defun find-asset-declaration (name)
  (find-if (lambda (it)
             (equal (asset-name it) name))
           *assets*))

(defun content-manager-parse-asset (decl)
  (let ((type (first decl))
        (props (rest decl)))
    (push (make-asset :type type
                      :name (key-value :name props)
                      :filename (key-value :file props)
                      :properties props)
          *assets*)))

(defun configure-content-manager (assets-lst)
  (loop for it in assets-lst
       do (content-manager-parse-asset it)))

(defun configure-content-manager-from-file (filename)
  (with-open-file (file filename :direction :input)
    (configure-content-manager (read file))))

(defun init-content-manager (&key (root (pathname-as-directory #P".")) config)
  (when *content-manager*
    (error "Content manager already initialized.~%"))
  (format t "Initializing content manager with ~S~%" root)
  (when config
    (format t "Using content manager config: ~S~%" config))
  (setf *content-manager* (create-resource-manager)
        *content-directory* root)
  (when config
    (if (pathnamep config)
        (configure-content-manager-from-file config)
        (configure-content-manager config))))

(defun shutdown-content-manager ()
  (destroy-resource-manager *content-manager*)
  (setf *content-directory* nil
        *content-manager* nil
        *assets* nil))

(defun load-asset (name &optional type (identifier name))
  "Load the specified asset from disk using whatever valid loader exist.
If the optional argument TYPE is provided then NAME is used as the filename to load from.
Otherwise an asset declaration for the provided NAME is searched. If such a declaration
exists then the corresponding asset is loaded, otherwise an error occurs."
  (format t "Loading asset ~S~%" name)
  (with-resource-manager *content-manager*
    (when (existing-resource-p identifier)
      (warn "Asset already loaded or identifier conflict for ~A." identifier))
    (let* ((pathname (merge-pathnames (if type
                                          name
                                          (asset-filename (find-asset-declaration name)))
                                      *content-directory*))
           (loader (%asset-loader (if type type
                                      (asset-type (find-asset-declaration name)))
                                      pathname)))
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
