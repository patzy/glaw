(in-package #:glaw)

;;; Text rendering
;; texture mapped fonts

(defstruct glyph
  u v du dv   ;; texture coords for the glyph
  (advance 0) ;; glyph width
  id          ;; char code
)

(defstruct font
  texture
  (base -1)   ;; display list base
  glyphs      ;; array of all glyphs
  line-height ;; height for all glyphs
)

(defun char-width (fnt char)
  (glyph-advance (aref (font-glyphs fnt) (char-code char))))

(defun string-width (fnt str)
  (loop for c across str
       sum (char-width fnt c)))

(defun string-height (fnt &optional str)
  (declare (ignore str))
  (font-line-height fnt))

(defun string-wrap (fnt str max-width)
  "Wrap the provided string at MAX-WIDTH and returns a list of string, one for each line of the
wrapped text."
  (let ((words (split-string str #\Space))
        (lines '()))
    (loop with current-width = 0
       with line = ""
       for w in words
       for word-width = (string-width fnt w)
       do
         (if (< (+ current-width word-width) max-width)
             (progn  (incf current-width word-width)
                     (if (string-equal line "")
                         (setf line w)
                         (setf line (concatenate 'string line " " w))))
             (progn (push line lines)
                    (setf line w)
                    (setf current-width 0)))
         finally (unless (string-equal line "") (push line lines)))
    (reverse lines)))

(defun create-font (texture &key (line-height 16) (nb-glyphs 256))
  "Create a new font object that can be used to render text using the provided texture."
  (let ((fnt (make-font :texture texture :line-height line-height
                        :glyphs (make-array nb-glyphs :element-type 'glyph :fill-pointer 0))))
    (loop for i below 256 do (setf (aref (font-glyphs fnt) i) (make-glyph)))
    fnt))

(defun font-build-cache (fnt)
  (when (/= (font-base fnt) -1)
    (gl:delete-lists (font-base fnt) 256))
  (setf (font-base fnt) (gl:gen-lists 256))
  (loop for i below 256 do
       (gl:new-list (+ (font-base fnt) i) :compile)
       (font-render-glyph fnt i)
       (when (glyph-id (aref (font-glyphs fnt) i))
         (gl:translate (glyph-advance (aref (font-glyphs fnt) i)) 0 0))
       (gl:end-list)))

(defun font-set-glyph-data (fnt index u v du dv advance)
  (let ((g (aref (font-glyphs fnt) index)))
    (setf (glyph-id g) index)
    (setf (glyph-u g) u (glyph-v g) v (glyph-du g) du (glyph-dv g) dv
          (glyph-advance g) advance)))

(defun font-render-glyph (fnt index)
  (let ((g (aref (font-glyphs fnt) index)))
    (when (glyph-id g)
      (select-texture (font-texture fnt) :env-mode :modulate)
      (gl:with-primitive :quads
        (gl:tex-coord (glyph-u g) (+ (glyph-v g) (glyph-dv g)))
        (gl:vertex 0 0)
        (gl:tex-coord (+ (glyph-u g) (glyph-du g)) (+ (glyph-v g) (glyph-dv g)))
        (gl:vertex (glyph-advance g) 0)
        (gl:tex-coord (+ (glyph-u g) (glyph-du g)) (glyph-v g))
        (gl:vertex (glyph-advance g) (font-line-height fnt))
        (gl:tex-coord (glyph-u g) (glyph-v g))
        (gl:vertex 0 (font-line-height fnt))))))

(defun destroy-font (fnt)
  (when (/= (font-base fnt) -1)
    (gl:delete-lists (font-base fnt) 256)))

(defun render-string (x y fnt str)
  (let ((char-lst (loop for c across str
                     collect (char-code c))))
    (select-texture (font-texture fnt) :env-mode :modulate)
    (gl:with-pushed-matrix
        (gl:translate x y 0)
      (if (= (font-base fnt) -1)
          (loop for c across str do
               (font-render-glyph fnt (char-code c))
               (gl:translate (char-width fnt c) 0 0))
          (progn (gl:list-base (font-base fnt))
                 (gl:call-lists char-lst))))
    (select-texture nil)))

(defun render-wrapped-string (x y wdth fnt str &key (justify :left))
  (let ((line-x x)
        (line-y y)
        (lines (glaw:string-wrap fnt str wdth)))
    (loop for l in lines do
         (case justify
           (:right (setf line-x (+ x (- wdth (string-width fnt l)))))
           (:center (setf line-x (+ x (round (- (/ wdth 2.0) (/ (string-width fnt l) 2.0)))))))
         (render-string line-x line-y fnt l)
         (decf line-y (font-line-height fnt)))
    line-y))

(defmacro format-at (x y fnt fmt &rest values)
  `(render-string ,x ,y ,fnt (format nil ,fmt ,@values)))

;; loader for fonttool generated fonts
;; see: http://gpwiki.org/index.php/C:OpenGL_Font_System
;; actually we use a slightly modified version using uint32_t instead of size_t which may change
;; see tools/fonttool.c
(defasset :fonttool-bitmap-font
  ;; load
  (lambda (filename)
    (with-open-file (in filename :direction :input :element-type '(unsigned-byte 8))
      (unless (and (= (read-byte in) 70) (= (read-byte in) 48))
        (error "Not a valid fonttool file (header mismatch)~%"))
      (let* ((tex-width (read-integer in 4))
             (tex-height (read-integer in 4))
             (tex-data (make-array (* tex-width tex-height) :element-type '(unsigned-byte 8)))
             (line-height (read-integer in 4))
             (nb-chars (read-integer in 4))
             (chars '())
             (tex nil)
             (fnt nil))
          (loop for i below nb-chars do
               (let* ((ascii-code (read-integer in 1))
                      (width (read-integer in 1))
                      (x (read-integer in 2))
                      (y (read-integer in 2)))
                 (push (list ascii-code x y width) chars)))
          (read-sequence tex-data in)
          (setf tex (create-texture tex-width tex-height 1 tex-data :internal-format :alpha)
                fnt (create-font tex :line-height line-height))
          (loop for i in chars do
               (font-set-glyph-data fnt (first i) (/ (second i) tex-width)
                                    (/ (third i) tex-height) (/ (fourth i) tex-width)
                                    (/ line-height tex-height) (fourth i)))
          (font-build-cache fnt)
          fnt)))
  ;; unload
  (lambda (font)
    (destroy-texture (glaw::font-texture font))
    (destroy-font font)))

