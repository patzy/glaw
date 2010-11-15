(in-package #:glaw)

(defvar *free-sources* '())
(defvar *used-sources* '())
(defvar *sounds* '())

(defstruct sound
  (channels '()) ;; all channels playing this sound
  buffer-id
  (position #(0 0 0))
  (velocity #(0 0 0))
  (direction #(0 0 0))
  relative-p)

(defun add-source (src)
  (push src *free-sources*))

(defun use-source (src)
  (push src *used-sources*)
  (setf *free-sources* (remove src *free-sources*)))

(defun free-source (src)
  (push src *free-sources*)
  (setf *used-sources* (remove src *used-sources*)))

(defun init-sound (&optional (max-channels 64))
  (let ((device (alc:open-device)))
    (unless device
      (error "Can't open sound device."))
    (let ((ctx (alc:create-context device)));; FIXME: missing attributes
      (unless ctx
        (alc:close-device device)
        (error "Unable to create OpenAL context."))
      (unless (alc:make-context-current ctx)
        (alc:destroy-context ctx)
        (alc:close-device device)
        (error "Unable to make OpenAL context current."))))
  (loop for src = (al:gen-source)
       until (or (zerop src) (= max-channels (sound-nb-channels)))
       do (add-source src)))

(defun shutdown-sound ()
  (al:delete-sources *free-sources*)
  (al:delete-sources *used-sources*)
  (let* ((ctx (alc:get-current-context))
         (device (alc:get-contexts-device ctx)))
    (alc:destroy-context ctx)
    (alc:close-device device))
  (setf *free-sources* '() *used-sources*'()))

(defun update-sound ()
  (dolist (src *used-sources*)
    (unless (eq (al:get-source src :source-state) :playing)
      (free-source src)
      (dolist (snd *sounds*) ;; FIXME: this is just wrong
        (when (member src (sound-channels snd))
          (setf (sound-channels snd) (remove src (sound-channels snd))))))))

(defun main-volume ()
  (al:get-listener :gain))

(defun set-main-volume (val)
  (al:listener :gain val))

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

(defun channel-volume (chan)
  (al:get-source chan :gain))

(defun set-channel-volume (chan value)
  (al:source chan :gain value))

(defsetf channel-volume set-channel-volume)

(defun channel-pitch (chan)
  (al:get-source :pitch))

(defun set-channel-pitch (chan value)
  (al:source chan :pitch value))

(defsetf channel-pitch set-channel-pitch)

(defun play-sound (snd &key loop (volume 1.0) (pitch 1.0))
  (let ((chan (sound-find-channel)))
    (when chan
      (al:source chan :buffer (sound-buffer-id snd))
      (al:source chan :position (sound-position snd))
      (al:source chan :velocity (sound-velocity snd))
      (al:source chan :direction (sound-direction snd))
      (al:source chan :source-relative (sound-relative-p snd))
      (al:source chan :looping loop)
      (al:source chan :gain volume)
      (al:source chan :pitch pitch)
      (push chan (sound-channels snd))
      (push snd *sounds*)
      (play-channel chan)
      chan)))

(defun stop-sound (snd)
  (dolist (chan (sound-channels snd))
    (stop-channel chan))
  (setf (sound-channels snd) '()))

;; TODO: need support for extended formats (see alure.cpp:391)
(defun al-sample-format (chans bits)
  (case bits
    (8 (case chans
         (1 :mono8)
         (2 :stereo8)
         (t (error "Extended formats unsupported."))))
    (16 (case chans
         (1 :mono16)
         (2 :stereo16)
         (t (error "Extended formats unsupported."))))
    (t (error "Unhandled bit depth."))))

(defasset :sound '("wav")
    ;; load
    (lambda (filename)
      (with-open-file (in filename :direction :input :element-type '(unsigned-byte 8))
        (let ((riff-header (read-integer in 4 t));; big-endian
              (total-length (read-integer in 4)) ;; total data length
              (wave-header (read-integer in 4 t)))
          (unless (and (= riff-header #x52494646) (= wave-header #x57415645)) ;; "RIFF" & "WAVE"
            (error "Not a valid wave file."))
          ;; proper riff/wave file, going on...
          (let ((magic (read-integer in 4 t))
                (chunk-length (read-integer in 4)))
            (unless (= magic #x666d7420) ;; "fmt "
              (error "Missing fmt section."))
            (unless (= chunk-length 16)
              (error "Only PCM format is supported."))
            (let ((audio-format (read-integer in 2))
                  (num-channels (read-integer in 2))
                  (sample-freq  (read-integer in 4))
                  (byte-rate    (read-integer in 4))
                  (block-align  (read-integer in 2))
                  (bits-per-sample (read-integer in 2)))
              (unless (= audio-format 1)
                (error "Only uncompressed wave files are supported."))
              (setf magic (read-integer in 4 t)
                    chunk-length (read-integer in 4))
              (unless (= magic #x64617461) ;; "data"
                (error "Problem reading data."))
              ;; data may be quit big so we want toa void stack allocation
              ;; that may occur with cffi:with-foreign-object
              ;; it actually occurs with clisp
              (let ((data (cffi:foreign-alloc :unsigned-char :count chunk-length)))
                (loop for i below chunk-length
                   do (setf (cffi:mem-aref data :unsigned-char i)
                            (read-integer in 1)))
                (let ((buff (al:gen-buffer)))
                  (al:buffer-data buff (al-sample-format num-channels bits-per-sample)
                                  data chunk-length sample-freq)
                  (cffi:foreign-free data)
                  (make-sound :buffer-id buff))))))))
    ;; unload
    (lambda (snd)
      (al:delete-buffer (sound-buffer-id snd))))


;; streaming: http://kcat.strangesoft.net/openal-tutorial.html
;; synth: http://www.noeska.com/doal/lesson11.aspx
