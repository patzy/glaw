(in-package #:glaw)

;; TODO: remove alut dependency?

(defvar *free-sources* '())
(defvar *used-sources* '())

(defun add-source (src)
  (push src *free-sources*))

(defun use-source (src)
  (push src *used-sources*)
  (setf *free-sources* (remove src *free-sources*)))

(defun init-sound (&optional (max-channels 64))
  (alut:init)
  (loop for src = (al:gen-source)
       until (or (zerop src) (= max-channels (sound-nb-channels)))
       do (add-source src)))

(defun shutdown-sound ()
  (alut:exit))

(defun sound-nb-channels ()
  (+ (length *free-sources*) (length *used-sources*)))

(defstruct sound
  buffer-id
  (position #(0 0 0))
  (velocity #(0 0 0))
  (direction #(0 0 0))
  source-relative)

(defun play-sound (snd &optional (channel-policy :free))
  
