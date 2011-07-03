(in-package :glaw)

;; Resource management
;; A resource is something that may be shared between multiple users.
;; We don't store the resource but resource holders.
;; You may use different resources managers at the same time.
;; Create or retrieve a resource with USE-RESOURCE
;; when you don't need the resource anymore call DROP-RESOURCE
;; Make resource aliases using ALIAS-RESOURCE
;; and remove all aliases with UNALIAS-RESOURCE
;; and remove a specific alias with DROP-ALIAS

(defstruct resource-manager
  (resources (make-hash-table :test 'equal))
  (aliases (make-hash-table :test 'equal)))

(defstruct resource-holder
  "Value holder for an actual resource object."
  data finalizer (users 1))

(defun add-resource (mgr id res &optional finalizer)
  "Create a new RESOURCE-HOLDER for RES designated by ID."
  (if (get-resource-holder mgr id)
      (error "Resource ~S already exist~%" id)
      (setf (gethash id (resource-manager-resources mgr))
            (make-resource-holder :data res :finalizer finalizer)))
  res)

(defun add-resource-alias (mgr res-id alias-id)
  (assert (get-resource-holder mgr res-id)
          (alias-id res-id)
          "Resource must exist to create an alias (~S -> ~S)~%" alias-id res-id)
  (if (gethash alias-id (resource-manager-aliases mgr) nil)
      (error "Alias ~S for ~S already exists~%" alias-id res-id)
      (progn (dformat "Adding resource alias ~S -> ~S~%" alias-id res-id)
             (setf (gethash alias-id (resource-manager-aliases mgr)) res-id))))

(defun get-resource-aliases (mgr res-id)
  (let ((aliases (list)))
    (maphash (lambda (key value)
               (when (string-equal value res-id)
                 (push key aliases)))
             (resource-manager-aliases mgr))
    aliases))

(defun remove-all-resource-aliases (mgr res-id)
  (dolist (alias (get-resource-aliases mgr res-id))
    (remove-resource-alias mgr alias)))

(defun remove-resource-alias (mgr alias-id)
  (remhash alias-id (resource-manager-aliases mgr)))

(defun remove-resource (mgr id)
  "Remove RESOURCE-HOLDER designated by ID from the manager calling associated finalizer if any."
  (let ((holder (gethash id (resource-manager-resources mgr))))
    (if holder
      (progn (remove-all-resource-aliases mgr id)
             (when (resource-holder-finalizer holder)
               (funcall (resource-holder-finalizer holder) (resource-holder-data holder)))
             (remhash id (resource-manager-resources mgr)))
      (error "Can't remove non-existing resource ~S~%" id))))

(defun remove-all-resources (mgr)
  (when (resource-manager-resources mgr)
    (maphash (lambda (key value)
               (declare (ignore key))
               (when (resource-holder-finalizer value)
                 (funcall (resource-holder-finalizer value) (resource-holder-data value))))
             (resource-manager-resources mgr))
    (setf (resource-manager-resources mgr) (make-hash-table :test 'equal)))
  (when (resource-manager-aliases mgr)
    (setf (resource-manager-resources mgr) (make-hash-table :test 'equal))))


(defun get-resource-holder (mgr id)
  "Returns the actual resource and its associated value-holder as values."
  (let ((alias (gethash id (resource-manager-aliases mgr) nil)))
    (gethash (or alias id) (resource-manager-resources mgr) nil)))

;; Some funcs to help working with a current resource-manager
(defvar %resource-manager% nil)

(defmacro with-resource-manager (mgr &body body)
  "Bind MGR as the current resource manager."
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
     (if holder
         (progn (incf (resource-holder-users holder))
                (resource-holder-data holder))
         (add-resource %resource-manager% ,id ,res ,finalizer))))

(defun existing-resource-p (id)
  "Returns T if there's a resource with the provided ID, NIL otherwise."
  (if (get-resource-holder %resource-manager% id) t nil))

(defun drop-resource (id)
  "Stop using the designated resource."
  (let ((holder (get-resource-holder %resource-manager% id)))
    (when holder
      (decf (resource-holder-users holder))
      (when (zerop (resource-holder-users holder))
        (remove-resource %resource-manager% id)))))

(defun alias-resource (res-id alias-id)
  (add-resource-alias %resource-manager% res-id alias-id))

(defun resource-aliases (res-id)
  (get-resource-aliases %resource-manager% res-id))

(defun unalias-resource (res-id)
  (remove-all-resource-aliases %resource-manager% res-id))

(defun drop-alias (alias-id)
  (remove-resource-alias %resource-manager% alias-id))

(defun use-resources (&rest res-ids)
  (loop for id in res-ids collect (use-resource id)))

(defun drop-resources (&rest res-ids)
  (loop for id in res-ids do (drop-resource id)))

(defmacro with-resources (resources &body body)
  (let ((res (gensym)))
    `(let (,@(loop for (sym id) in resources collect `(,sym (use-resource ,id))))
       (let ((,res (progn ,@body)))
         (drop-resources ,@(loop for r in resources collect (cadr r)))
         ,res))))

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

