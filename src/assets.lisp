(in-package #:glaw)

#+clisp ;; why ansi isn't default behavior?
(setf custom:*merge-pathnames-ansi* t)

;;; Asset loaders
;; An asset loader is required to load an asset resource from disk
;; It should provide functions for load and unload as well
;; as a list of supported extensions (or :any)
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

(defun %asset-loader (type extension)
  "Try to find a valid asset loader for the specified TYPE and EXTENSION."
  (let ((loader (loop for l in (gethash type *asset-loaders* nil)
                   when (asset-loader-extension-supported-p l extension)
                   return l)))
    (unless loader
      (error "No asset loader defined for loading ~S as a ~S~%" extension type))
    loader))

(defun defasset (asset-type extensions load &optional unload)
  "Defines an asset loader for ASSET-TYPE. Extension check will be done upon loading
unless EXTENSIONS is :ANY."
  (push (make-asset-loader :load load :unload unload
                           :extensions extensions)
        (gethash asset-type *asset-loaders*)))

;;; Content manager singleton
(defvar *content-manager* nil)
(defvar *content-directory* nil)
(defvar *assets* nil)

(defstruct asset
  "Holds assets meta-data."
  type
  name
  properties)

(defun find-asset (name)
  (find-if (lambda (it)
             (equal (asset-name it) name))
           *assets*))

(defun %register-asset (asset)
  (push asset *assets*))

(defun declare-asset (type name props)
  (dformat "Registering asset ~S of type ~S: ~S~%" name type props)
  (%register-asset (make-asset :type type
                               :name name
                               :properties props)))

(defun content-manager-parse-asset (decl)
  (let ((type (first decl))
        (props (rest decl)))
    (%register-asset (make-asset :type type
                                 :name (or (key-value :name props) (key-value :file props)
                                           (symbol-name (gensym "ASSET-")))
                                 :properties props))))

(defun configure-content-manager (assets-lst)
  (loop for it in assets-lst
       do (content-manager-parse-asset it)))

(defun configure-content-manager-from-file (filename)
  (with-open-file (file filename :direction :input)
    (configure-content-manager (read file))))

(defun init-content-manager (&key (root (pathname-as-directory #P".")) config)
  (when *content-manager*
    (warn "Content manager already initialized, replacing instance.~%"))
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

(defun %loaded-asset-p (asset)
  (with-resource-manager *content-manager*
    (existing-resource-p (asset-name asset))))

(defun %load-asset (asset &optional (root-path *content-directory*))
  (assert (or (key-value :file (asset-properties asset))
              (key-value :format (asset-properties asset))))
  (format t "Loading ~S asset ~S~%" (asset-type asset) (asset-name asset))
  (let ((props (asset-properties asset))
        (format (key-value :format (asset-properties asset))))
    (when (key-value :file (asset-properties asset))
      (let ((pathname (merge-pathnames (key-value :file (asset-properties asset))
                                       root-path)))
        (setf format (pathname-type pathname))
        (setf props (append props (list :filename pathname)))))
    (with-resource-manager *content-manager*
      (let ((loader (%asset-loader (asset-type asset) format)))
        (if loader
            (use-resource (asset-name asset)
                          (apply (asset-loader-load loader) props)
                          (asset-loader-unload loader))
            (error "No asset loader defined for type ~S (~S)~%"
                   (asset-type asset) (asset-name asset)))))))


(defun load-asset (name &optional type (identifier name) properties)
  "Load the specified asset from disk using whatever valid loader exist.
If the optional argument TYPE is provided then NAME is used as the filename to load from.
Otherwise an asset declaration for the provided NAME is searched. If such a declaration
exists then the corresponding asset is loaded and returned, otherwise an error occurs.
If load is called directly with a filename, a new ASSET is created and stored in *ASSETS*.
When no identifier is specified, the filename is used."
  (let ((asset (if type
                   (make-asset :type type :name identifier
                               :properties (append (list :file name)
                                                   properties))
                   (find-asset name))))
    (unless asset
      (error "Invalid asset, can't load ~S." name))
    ;; Handle alias loading, this only makes sense with declared assets
    ;; XXX: we only support one alias depth level (i.e. no aliases' aliases)
    (if (eq :alias (asset-type asset))
        (let* ((aliased-res-name (key-value :resource (asset-properties asset)))
               (aliased-asset (find-asset aliased-res-name)))
          (unless aliased-asset
            (error "Can't find aliased asset ~S~%" aliased-asset-name))
          (let ((aliased-res (if (%loaded-asset-p aliased-asset)
                                 (use-asset (asset-name aliased-asset))
                                 (%load-asset aliased-asset))))
            (with-resource-manager *content-manager*
              (alias-resource (asset-name aliased-asset) (asset-name asset))
              aliased-res)))
        (%load-asset asset))))

(defun alias-asset (identifier alias)
  (with-resource-manager *content-manager*
    (alias-resource identifier alias)))

(defun use-asset (identifier &optional res)
  (with-resource-manager *content-manager*
    (unless (or res (existing-resource-p identifier))
      (load-asset identifier))
    (use-resource identifier res)))

(defun dispose-asset (identifier)
  "Dispose designated asset regardless of its current users."
  (format t "Disposing of asset: ~S~%" identifier)
  (remove-resource *content-manager* identifier))

(defun dispose-assets (&rest ids)
  (dolist (it ids)
    (dispose-asset it)))
