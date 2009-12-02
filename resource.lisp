(in-package :glaw)

;; Resource management
;; A resource is something that may be shared between multiple users.
;; We don't store the resource but resource holders.
;; You may use different resources managers at the same time.
;; Create or retrieve a resource with USE-RESOURCE
;; when you don't need the resource anymore call DROP-RESOURCE

(defstruct resource-manager
  (resources (make-hash-table :test 'equal)))

(defstruct resource-holder
  "Value holder for an actual resource object."
  data finalizer (users 0 :type 'integer))

;; TODO: add some conditions/restarts when overwriting an existing resource
(defun add-resource (mgr id res &optional finalizer)
  "Create a new RESOURCE-HOLDER for RES designated by ID."
  (setf (gethash id (resource-manager-resources mgr))
        (make-resource-holder :data res :finalizer finalizer))
  res)

;; TODO: add some conditions/restarts when trying to remove a non-existing resource
(defun remove-resource (mgr id)
  "Remove RESOURCE-HOLDER designated by ID from the manager calling associated finalizer if any."
  (let ((holder (gethash id (resource-manager-resources mgr))))
    (when (resource-holder-finalizer holder)
        (funcall (resource-holder-finalizer holder) (resource-holder-data holder)))
    (remhash id (resource-manager-resources mgr))))

(defun remove-all-resources (mgr)
  (when (resource-manager-resources mgr)
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (resource-holder-finalizer value)
                 (funcall (resource-holder-finalizer value) (resource-holder-data value))))
             (resource-manager-resources mgr))
    (setf (resource-manager-resources mgr) (make-hash-table :test 'equal))))

(defun get-resource-holder (mgr id)
  "Returns the actual resource and its associated value-holder as values."
  (gethash id (resource-manager-resources mgr) nil))

;; Some funcs to help working with a resource-manager
(defvar %resource-manager% nil)

(defmacro with-resource-manager (mgr &body body)
  `(let ((%resource-manager% ,mgr))
     ,@body))

;; This is a macro so you can pass code creating the resource as the RES argument
;; and this only get evaluated if a new resource is created.
(defmacro use-resource (id &optional res finalizer)
  "Use or create a resource designated by ID.
   Resource creation occurs iff ID doesn't already exist in %RESOURCE-MANAGER%
   and requires RES to be specified. FINALIZER is optional but if provided should be a function
   taking the resource to finalize as its only argument."
  `(let ((holder (get-resource-holder %resource-manager% ,id)))
     (format t "Found holder: ~S~%" holder)
     (if holder
         (progn (incf (resource-holder-users holder))
                (resource-holder-data holder))
         (progn (format t "Creating a new resource with id: ~S~%" ,id)
                (add-resource %resource-manager% ,id ,res ,finalizer)))))

(defun drop-resource (id)
  "Stop using the designated resource."
  (let ((holder (get-resource-holder %resource-manager% id)))
    (when holder
      (decf (resource-holder-users holder))
      (when (zerop (resource-holder-users holder))
        (remove-resource %resource-manager% id)))))

(defun create-resource-manager (&optional (keep-current nil))
  "Make a new resource manager, maybe binds %RESOURCE-MANAGER% and returns it."
  (let ((mgr (make-resource-manager)))
    (unless keep-current
      (setf %resource-manager% mgr))
    mgr))

(defun destroy-resource-manager (&optional (mgr %resource-manager%))
  "Destroy specified resource manager, removing all managed resources before.
   Defaults to %RESOURCE-MANAGER%."
  (remove-all-resources mgr)
  (setf mgr nil))

