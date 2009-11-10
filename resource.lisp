(in-package :glaw)

(defvar *resources* (make-hash-table :test 'equal))

(defstruct resource
  identifier
  (users 1 :type 'integer)
  type
  data)

(defun get-resource (identifier)
  (gethash identifier *resources*))

(defun create-resource (filename type)
  "Returns resource identifier"
  (format t "Creating resource of type ~S from ~S~%"
          type filename)
  (let* ((identifier filename)
         (resource (get-resource identifier)))
    (if resource
        (progn (incf (resource-users resource))
               resource)
        (setf (gethash identifier *resources*)
              (load-resource type filename)))))

(defun destroy-resource (identifier)
  (let ((resource (get-resource identifier)))
    (decf (resource-users resource))
    (when (zerop (resource-users resource))
      (free-resource resource))))


(defun free-all-resources ()
  "Free al allocated resources regardless of current users number."
  (when *resources*
    (maphash (lambda (key value)
               (declare (ignore key))
               (free-resource value)) *resources*)))

(defgeneric load-resource (type filename)
  (:documentation "Returns a new resource"))

(defgeneric free-resource (resource)
  (:documentation "Free data allocated for resource"))

;; Image resource
(defstruct (image-resource (:include resource
                                     (type :image)))
  width height bpp)

;; Sound resource
(defstruct (sound-resource (:include resource
                                     (type :sample)))
  format freq loop)
