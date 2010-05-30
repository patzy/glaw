(in-package #:glaw)

;; TODO: remove alut dependency?

(defvar *free-sources* '())
(defvar *used-sources* '())
(defvar *sounds* '())

(defun add-source (src)
  (push src *free-sources*))

(defun use-source (src)
  (push src *used-sources*)
  (setf *free-sources* (remove src *free-sources*)))

(defun free-source (src)
  (push src *free-sources*)
  (setf *used-sources* (remove src *used-sources*)))

(defun init-sound (&optional (max-channels 64))
  (alut:init)
  (loop for src = (al:gen-source)
       until (or (zerop src) (= max-channels (sound-nb-channels)))
       do (add-source src)))

(defun shutdown-sound ()
  (al:delete-sources *free-sources*)
  (al:delete-sources *used-sources*)
  (alut:exit)
  (setf *free-sources* '() *used-sources*'()))

(defun update-sound ()
  (dolist (src *used-sources*)
    (unless (eq (al:get-source src :source-state) :playing)
      (free-source src)
      (dolist (snd *sounds*) ;; FIXME: this is just wrong
        (when (member src (sound-channels snd))
          (setf (sound-channels snd) (remove src (sound-channels snd))))))))

(defun sound-find-channel ()
  (when *free-sources*
    (let ((chan (first *free-sources*)))
      (use-source chan)
      chan)))

(defun sound-nb-channels ()
  (+ (length *free-sources*) (length *used-sources*)))

(defun sound-nb-free-channels ()
  (length *free-sources*))

(defun sound-nb-used-channels ()
  (length *used-sources*))

(defun play-channel (chan)
  (al:source-play chan))

(defun stop-channel (chan)
  (al:source-stop chan))

(defstruct sound
  (channels '()) ;; all channels playing this sound
  buffer-id
  (position #(0 0 0))
  (velocity #(0 0 0))
  (direction #(0 0 0))
  relative-p)

(defun play-sound (snd &key loop (volume 1.0))
  (let ((chan (sound-find-channel)))
    (when chan
      (al:source chan :buffer (sound-buffer-id snd))
      (al:source chan :position (sound-position snd))
      (al:source chan :velocity (sound-velocity snd))
      (al:source chan :direction (sound-direction snd))
      (al:source chan :source-relative (sound-relative-p snd))
      (al:source chan :looping loop)
      (al:source chan :gain volume)
      (push chan (sound-channels snd))
      (push snd *sounds*)
      (play-channel chan)
      chan)))

(defun stop-sound (snd)
  (dolist (chan (sound-channels snd))
    (stop-channel chan))
  (setf (sound-channels snd) '()))

(defasset :sound
    ;; load
    (lambda (filename)
      (let ((buff (alut:create-buffer-from-file filename)))
        (make-sound :buffer-id buff)))
    ;; unload
    (lambda (snd)
      (al:delete-buffer (sound-buffer-id snd))))
