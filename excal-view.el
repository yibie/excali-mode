;;; excal-view.el --- Rendering, presentation backends and view navigation  -*- lexical-binding: t; -*-

;;; Commentary:

;; Turns the scene into pixels and puts them on screen: the offscreen
;; framebuffer, the canvas/tiles/layer backends, damage tracking, scroll
;; reuse, panning, zooming and export.

;;; Code:

(require 'excal-core)
(require 'excal-text)
(require 'excal-image)

(declare-function excal--update-pointer "excal-edit")
(declare-function excal--sync-cursor-view "excal-cursor")
(declare-function excal--hide-cursor-view "excal-cursor")
(declare-function excal--overlay-natives "excal-handles")
(declare-function excal--erase-opacity-for "excal-erase")
(declare-function excal--text-native-extras "excal-text")
(declare-function excal-native-fb-copy "excal-module")
(declare-function excal-native-fb-zoom-preview "excal-module")
(declare-function excal-native-fb-mean-diff "excal-module")

(defun excal--flat-points (points)
  "Convert JSON POINTS array of [x y] into a flat float vector."
  (when (vectorp points)
    (let ((flat (make-vector (* 2 (length points)) 0.0)) (i 0))
      (seq-doseq (p points)
        (aset flat i (float (aref p 0)))
        (aset flat (1+ i) (float (aref p 1)))
        (cl-incf i 2))
      flat)))

(defun excal--native-element (element)
  "Return the cached native vector for ELEMENT."
  (let ((native
         (or (gethash element excal--native-cache)
             (puthash
              element
              (vector (excal--get element 'type)
                      (excal--get element 'x) (excal--get element 'y)
                      (excal--get element 'width) (excal--get element 'height)
                      (excal--get element 'angle)
                      (excal--get element 'strokeColor)
                      (excal--get element 'backgroundColor)
                      (excal--get element 'fillStyle)
                      (excal--get element 'strokeWidth)
                      (excal--get element 'roughness)
                      (excal--get element 'seed)
                      (excal--flat-points (excal--get element 'points))
                      (excal--get element 'text)
                      (excal--get element 'fontSize)
                      (excal--get element 'opacity)
                      nil
                      (excal--get element 'strokeStyle)
                      (excal--get element 'fontFamily)
                      (excal--get element 'textAlign)
                      (excal--get element 'lineHeight)
                      (and (excal--get element 'roundness) t)
                      (excal--get element 'startArrowhead)
                      (excal--get element 'endArrowhead)
                      (excal--native-shape-extras element)
                      (excal--native-text-extras element)
                      ;; Optional last slot, see `SLOT_MEDIA_EXTRAS'.
                      (excal--native-media-extras element))
              excal--native-cache))))
    (when (fboundp 'excal--erase-opacity-for)
      ;; Elements marked by the eraser fade; recomputed every frame.
      (aset native 15 (excal--erase-opacity-for element (excal--get element 'opacity))))
    native))

(defun excal--native-shape-extras (element)
  "Return extra shape rendering properties of ELEMENT for the module.
The result is a vector [KEY VALUE ...] with string keys, read in C by
`get_extra_*' in excal-module.c.  Keys: \"roundnessType\",
\"roundnessValue\", \"elbowed\", and for freedraw \"pressures\",
\"simulatePressure\" (1, 0, or absent), \"strokeVariability\" and
\"streamline\", and for sticky notes \"stickyFooter\"."
  (let ((roundness (excal--get element 'roundness))
        (extras nil))
    (when (consp roundness)
      (when-let* ((type (excal--get roundness 'type)))
        (push "roundnessType" extras) (push type extras))
      (when-let* ((value (excal--get roundness 'value)))
        (push "roundnessValue" extras) (push value extras)))
    (when (excal--get element 'elbowed)
      (push "elbowed" extras) (push t extras))
    (when (equal (excal--get element 'type) "freedraw")
      (when-let* ((pressures (excal--get element 'pressures)))
        (push "pressures" extras) (push (vconcat pressures) extras))
      (let ((cell (assq 'simulatePressure element)))
        (when cell
          (push "simulatePressure" extras)
          (push (if (memq (cdr cell) '(nil :null :false)) 0 1) extras)))
      (when-let* ((options (excal--get element 'strokeOptions))
                  ((consp options)))
        (when-let* ((variability (excal--get options 'variability)))
          (push "strokeVariability" extras) (push variability extras))
        (when-let* ((streamline (excal--get options 'streamline)))
          (push "streamline" extras) (push streamline extras))))
    (when (equal (excal--get element 'type) "stickynote")
      (when-let* ((footer (excal--sticky-footer element)))
        (push "stickyFooter" extras) (push footer extras)))
    (vconcat (nreverse extras))))

(defun excal--sticky-footer (note)
  "Return NOTE's date label: \"27 Sep\", with the year when it is not this
year and the body is wide enough (upstream sticky note footer)."
  (let ((created (or (excal--get note 'created) (excal--get note 'updated))))
    (when (numberp created)
      (let* ((time (decode-time (/ created 1000.0)))
             (months ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])
             (label (format "%d %s" (decoded-time-day time)
                            (aref months (1- (decoded-time-month time))))))
        (if (and (/= (decoded-time-year time) (decoded-time-year (decode-time)))
                 (>= (- (or (excal--get note 'width) 0) 32) 80))
            (format "%s %d" label (decoded-time-year time))
          label)))))

(defun excal--native-text-extras (element)
  "Return extra text rendering properties of ELEMENT for the module.
Same format as `excal--native-shape-extras'.  Text elements get
\"vertical-offset\", the first baseline below their top; arrows with a
label get \"label-hole\" [X Y W H], the box cut out of the stroke.
See `excal--text-native-extras' in excal-text.el."
  (excal--text-native-extras element))

(defun excal--visible-elements ()
  "Return live elements, then editor overlays, as a native vector."
  (vconcat (delq nil (mapcar (lambda (e)
                               (unless (excal--get e 'isDeleted)
                                 (excal--native-element e)))
                             excal--elements))
           (excal--overlay-natives)))

(defcustom excal-backend 'auto
  "How rendered pixels reach the screen.
`canvas' copies each frame into one Canvas image.  `tiles' splits the
window into Canvas tiles and refreshes only those whose pixels changed.
`layer' (macOS only) shows frames in a CoreAnimation overlay above the
Emacs view, bypassing `canvas-refresh' entirely.  `auto' uses `layer'
on graphical macOS frames when the module provides it, else `tiles'."
  :type '(choice (const auto) (const canvas) (const tiles) (const layer))
  :group 'excal)

(defun excal--layer-frame-p (frame)
  "Return non-nil if FRAME is a graphical macOS frame."
  (eq (framep frame) 'ns))

(defun excal--resolve-backend (&optional frame)
  "Return the backend a new buffer on FRAME should use.
FRAME defaults to the selected frame.  Resolve `auto' in
`excal-backend', and fall back to `tiles' where `layer' is unavailable."
  (let ((layer (fboundp 'excal-native-layer-create)))
    (pcase excal-backend
      ('auto (if (and layer (excal--layer-frame-p (or frame (selected-frame))))
                 'layer
               'tiles))
      ('layer (if layer 'layer 'tiles))
      (backend backend))))

(defcustom excal-tile-size 256
  "Maximum tile edge in logical pixels for the `tiles' backend."
  :type 'integer
  :group 'excal)

(defvar-local excal--backend nil "Backend used by this buffer.")
(defvar-local excal--fb nil "Module-owned offscreen framebuffer.")
(defvar-local excal--tiles nil "Vector of [CANVAS X Y WIDTH HEIGHT] tiles.")
(defvar-local excal--layer nil "Overlay layer handle for `layer'.")
(defvar-local excal--last-stats nil "Plist describing the last frame.")

(defun excal--view-origin ()
  "Return (ZOOM PIXEL-SCALE X Y): the scene origin in device pixels."
  (let ((scale (* excal--zoom excal--pixel-scale)))
    (list excal--zoom excal--pixel-scale
          (* scale excal--scroll-x) (* scale excal--scroll-y))))

(defun excal--integral-p (x)
  "Return non-nil if X is within rounding error of an integer."
  (< (abs (- x (round x))) 1e-6))

(defun excal--plan-repaint (damage)
  "Prepare the framebuffer for DAMAGE and return what to repaint.
Return nil to repaint everything, `none' when nothing changed, or a
vector of native [X Y W H] rectangles.  If the view only moved by whole
device pixels since the last render, the framebuffer is shifted in place
so only the newly exposed strips need painting."
  (let ((old excal--rendered-origin)
        (new (excal--view-origin)))
    (setq excal--rendered-origin new)
    (when (and damage (not (eq damage 'full)) old
               (= (nth 0 old) (nth 0 new)) (= (nth 1 old) (nth 1 new)))
      (let ((dx (- (nth 2 new) (nth 2 old)))
            (dy (- (nth 3 new) (nth 3 old)))
            (w (car excal--canvas-size))
            (h (cdr excal--canvas-size))
            (rects nil))
        (when (and (excal--integral-p dx) (excal--integral-p dy)
                   (< (abs dx) w) (< (abs dy) h))
          (setq dx (round dx) dy (round dy))
          (unless (and (zerop dx) (zerop dy))
            (excal-native-fb-scroll excal--fb dx dy)
            (cond ((> dx 0) (push (vector 0 0 dx h) rects))
                  ((< dx 0) (push (vector (+ w dx) 0 (- dx) h) rects)))
            (cond ((> dy 0) (push (vector 0 0 w dy) rects))
                  ((< dy 0) (push (vector 0 (+ h dy) w (- dy)) rects))))
          (when (consp damage)
            (push (excal--damage-vector damage) rects))
          (if rects (vconcat rects) 'none))))))

(defun excal-toggle-theme ()
  "Switch the canvas between the light and dark themes."
  (interactive)
  (setq excal--theme (if (eq excal--theme 'dark) 'light 'dark))
  (when excal--native-cache (clrhash excal--native-cache))
  (excal--render)
  (message "Theme: %s" excal--theme))

(defun excal--canvas-color ()
  "Return the scene's background color, `viewBackgroundColor', or nil."
  (let ((color (alist-get 'viewBackgroundColor (alist-get 'appState excal--doc))))
    (and (stringp color) (string-prefix-p "#" color) color)))

(defun excal--render (&optional damage)
  "Render the scene and present it.
DAMAGE nil repaints everything.  `scroll' means only the view moved; a
device rectangle (X1 Y1 X2 Y2) marks changed scene content.  See
`excal--plan-repaint' for how moved views reuse existing pixels.  A
zoom preview on screen is replaced by a full render."
  (excal--cancel-preview)
  (when excal--fb
    (let* ((t0 (float-time))
           (plan (excal--plan-repaint damage))
           (drawn (if (eq plan 'none)
                      0
                    (excal-native-fb-render
                     excal--fb excal--pixel-scale excal--zoom
                     excal--scroll-x excal--scroll-y
                     (excal--visible-elements) plan
                     (excal--canvas-color) (eq excal--theme 'dark)))))
      (excal--present-frame t0 drawn nil))))

(defun excal--present-frame (start drawn preview)
  "Present the framebuffer and record stats for a frame begun at START.
DRAWN is the number of elements rendered; PREVIEW is non-nil when the
frame is a zoom preview."
  (let* ((t1 (float-time))
         (refreshed (excal--present))
         (t2 (float-time)))
    (setq excal--last-render-time (- t1 start)
          excal--last-stats (list :drawn drawn
                                  :render-ms (* 1000 (- t1 start))
                                  :present-ms (* 1000 (- t2 t1))
                                  :refreshed refreshed
                                  :preview preview))))

;;;; Zoom preview

(defcustom excal-zoom-preview-delay 0.1
  "Seconds without zoom input before a zoom preview is rendered crisply.
While zooming, each step scales the pixels of the last full render
instead of rendering the scene again; once zooming pauses this long a
full render replaces the preview.  nil renders every zoom step fully."
  :type '(choice (const :tag "Render every step" nil) number)
  :group 'excal)

(defcustom excal-zoom-preview-limit 4.0
  "Largest factor a zoom preview may scale the last full render by.
A zoom step that goes further (in or out) renders fully, and later
steps preview from that render instead."
  :type 'number
  :group 'excal)

(defvar-local excal--preview-snapshot nil
  "Framebuffer holding a copy of the full render being previewed.")
(defvar-local excal--preview-origin nil
  "View origin of `excal--preview-snapshot' while a preview is shown.
nil when no preview is shown; see `excal--view-origin'.")
(defvar-local excal--preview-timer nil
  "Timer that replaces the zoom preview with a full render, or nil.")

(defun excal--cancel-preview ()
  "Cancel any pending preview render and forget the preview origin.
The framebuffer may still hold preview pixels; `excal--rendered-origin'
is nil then, so the next render repaints everything."
  (when excal--preview-timer
    (cancel-timer excal--preview-timer)
    (setq excal--preview-timer nil))
  (setq excal--preview-origin nil))

(defun excal--finish-preview (buffer)
  "Replace the zoom preview in BUFFER with a full render.
This runs from `excal--preview-timer'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      ;; `excal--render' cancels the timer too.
      (if excal--preview-origin
          (excal--render)
        (excal--cancel-preview)))))

(defun excal--snapshot-framebuffer ()
  "Copy the framebuffer into `excal--preview-snapshot'."
  (unless (and excal--preview-snapshot
               (excal-native-fb-copy excal--fb excal--preview-snapshot))
    (setq excal--preview-snapshot
          (excal-native-fb-create (car excal--canvas-size)
                                  (cdr excal--canvas-size)))
    (excal-native-fb-copy excal--fb excal--preview-snapshot)))

(defun excal--preview-factor (base)
  "Return the scale from view origin BASE to the current view, or nil.
nil means the current view cannot be previewed from BASE."
  (let ((new (excal--view-origin)))
    (when (and base (= (nth 1 base) (nth 1 new)))
      (let ((factor (/ (nth 0 new) (float (nth 0 base)))))
        (when (<= (/ 1.0 excal-zoom-preview-limit) factor
                  excal-zoom-preview-limit)
          factor)))))

(defun excal--render-preview ()
  "Show the current view by transforming the last full render.
The first preview after a full render keeps a pristine copy of it, and
every later step scales that copy, so steps never compound blur or
rounding.  Fall back to `excal--render' when previews are disabled, the
framebuffer holds no exact render, or the view moved too far from it.
A full render follows once no step came for `excal-zoom-preview-delay'."
  (let* ((base (or excal--preview-origin excal--rendered-origin))
         (factor (and excal-zoom-preview-delay excal--fb
                      (excal--preview-factor base))))
    (if (not factor)
        (excal--render)
      (let ((t0 (float-time))
            (new (excal--view-origin)))
        (unless excal--preview-origin
          (excal--snapshot-framebuffer)
          (setq excal--preview-origin base))
        ;; A scene point at device pixel D in the snapshot is now at
        ;; FACTOR * D + NEW - FACTOR * BASE.
        (excal-native-fb-zoom-preview
         excal--fb excal--preview-snapshot factor
         (- (nth 2 new) (* factor (nth 2 base)))
         (- (nth 3 new) (* factor (nth 3 base))))
        ;; The pixels match no exact origin now: never scroll-reuse them.
        (setq excal--rendered-origin nil)
        (excal--present-frame t0 0 t)
        (when excal--preview-timer
          (cancel-timer excal--preview-timer))
        (setq excal--preview-timer
              (run-with-timer excal-zoom-preview-delay nil
                              #'excal--finish-preview (current-buffer)))
        (add-hook 'kill-buffer-hook #'excal--cancel-preview nil t)))))

(defun excal--present ()
  "Push the framebuffer to the screen; return the surfaces refreshed."
  (pcase excal--backend
    ('canvas
     (excal-native-fb-present-canvas excal--fb excal--canvas)
     (canvas-refresh excal--canvas)
     1)
    ('tiles
     (let ((dirty (excal-native-fb-present-tiles excal--fb excal--tiles)))
       (dolist (i dirty)
         (canvas-refresh (aref (aref excal--tiles i) 0)))
       (length dirty)))
    ('layer
     (excal-native-layer-present excal--layer excal--fb)
     1)))

(defun excal--guess-pixel-scale ()
  "Return the device pixel ratio of the selected frame."
  (or excal-pixel-scale
      (let ((scale (and (fboundp 'frame-scale-factor) (frame-scale-factor))))
        (if (and (numberp scale) (> scale 0)) (float scale) 1.0))))

(defun excal--make-canvas (width height)
  "Return a WIDTH by HEIGHT device-pixel canvas for the current scale."
  (list 'image :type 'canvas :id (gensym "excal-canvas-")
        :data-width width :data-height height
        :scale (/ 1.0 excal--pixel-scale) :ascent 'center))

(defun excal--split (total size)
  "Split TOTAL into near-equal parts no longer than SIZE.
Return a list of (OFFSET . LENGTH)."
  (let* ((n (max 1 (ceiling total (float size))))
         (done 0)
         parts)
    (dotimes (i n)
      (let ((len (- (round (* (1+ i) (/ (float total) n))) done)))
        (push (cons done len) parts)
        (cl-incf done len)))
    (nreverse parts)))

(defun excal--insert-tiles (width height)
  "Insert Canvas tiles covering WIDTH by HEIGHT logical pixels."
  (let ((scale excal--pixel-scale)
        (rows (excal--split height excal-tile-size))
        tiles)
    (dolist (row rows)
      (dolist (col (excal--split width excal-tile-size))
        (let* ((x (round (* scale (car col)))) (w (round (* scale (cdr col))))
               (y (round (* scale (car row)))) (h (round (* scale (cdr row))))
               (canvas (excal--make-canvas w h)))
          (push (vector canvas x y w h) tiles)
          (insert (propertize " " 'display canvas))))
      (unless (eq row (car (last rows)))
        (insert "\n")))
    (setq excal--tiles (vconcat (nreverse tiles)))))

(defun excal--sync-layer (window width height)
  "Attach and place the overlay layer over WINDOW's WIDTH by HEIGHT body."
  (unless excal--layer
    (pcase-let ((`(,left ,top ,right ,bottom)
                 (frame-edges (window-frame window) 'native-edges)))
      (setq excal--layer (excal-native-layer-create
                          left top (- right left) (- bottom top))))
    (unless excal--layer
      (error "No Emacs view found for the overlay layer")))
  (pcase-let ((`(,x ,y . ,_) (window-inside-pixel-edges window)))
    (excal-native-layer-set-geometry excal--layer x y width height
                                     excal--pixel-scale t)))

(defun excal--hide-layer ()
  "Hide this buffer's overlay layer, if any."
  (when excal--layer
    (excal-native-layer-set-geometry excal--layer 0 0 1 1 1.0 nil)))

(defun excal--sync-canvas (&optional window)
  "Size the display surfaces to WINDOW's body and render."
  (let* ((window (or window (get-buffer-window (current-buffer))))
         (width (max 1 (window-body-width window t)))
         (height (max 1 (window-body-height window t)))
         (dw (round (* width excal--pixel-scale)))
         (dh (round (* height excal--pixel-scale))))
    (unless (equal excal--canvas-size (cons dw dh))
      (setq excal--canvas-size (cons dw dh)
            excal--fb (excal-native-fb-create dw dh)
            excal--rendered-origin nil
            excal--canvas nil
            excal--tiles nil)
      (let ((inhibit-read-only t)
            (buffer-undo-list t)
            (point (point)))
        (delete-region (or excal--canvas-start (point-min)) (point-max))
        (goto-char (point-max))
        (pcase excal--backend
          ('canvas
           (setq excal--canvas (excal--make-canvas dw dh))
           (insert (propertize " " 'display excal--canvas)))
          ('tiles (excal--insert-tiles width height))
          ('layer
           ;; Reserve the area so mouse events land in the text area.
           (insert (propertize " " 'display
                               `(space :width (,width) :height (,height))))))
        (goto-char (if excal--canvas-start point (point-min)))
        (setq excal--pointer nil))
      ;; Canvas pixel buffers exist only once the images are displayed.
      (redisplay t))
    (if (eq excal--backend 'layer)
        (excal--sync-layer window width height)
      (excal--hide-layer))
    (excal--sync-cursor-view window width height)
    (excal--render)
    (excal--update-pointer)))

(defun excal--window-size-change (frame)
  "Resize or hide the surfaces of excal buffers after FRAME changed."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'excal-mode)
        (if-let* ((window (get-buffer-window buffer frame)))
            (excal--sync-canvas window)
          (excal--hide-layer)
          (excal--hide-cursor-view))))))

(defun excal-cycle-backend ()
  "Switch to the next presentation backend."
  (interactive)
  (let* ((backends (if (fboundp 'excal-native-layer-create)
                       '(canvas tiles layer)
                     '(canvas tiles)))
         (next (or (cadr (memq excal--backend backends)) (car backends))))
    (excal--use-backend next)
    (message "Backend: %s" next)))

(defun excal--use-backend (backend)
  "Switch this buffer to BACKEND and redraw."
  (unless (eq backend 'layer) (excal--hide-layer))
  (setq excal--backend backend excal--canvas-size nil)
  (excal--sync-canvas))

(defun excal--device-rect (element)
  "Return ELEMENT's padded bounds as (X1 Y1 X2 Y2) in device pixels."
  (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) (excal--bounds element))
               (scale (* excal--zoom excal--pixel-scale))
               (pad (+ 40 (* 2 (or (excal--get element 'strokeWidth) 2))
                       (* 6 (or (excal--get element 'roughness) 1)))))
    (when (and (numberp (excal--get element 'angle))
               (/= 0 (excal--get element 'angle)))
      (let ((cx (/ (+ x1 x2) 2.0)) (cy (/ (+ y1 y2) 2.0))
            (r (/ (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))) 2.0)))
        (setq x1 (- cx r) x2 (+ cx r) y1 (- cy r) y2 (+ cy r))))
    (list (- (floor (* scale (+ x1 excal--scroll-x (- pad)))) 20)
          (- (floor (* scale (+ y1 excal--scroll-y (- pad)))) 20)
          (+ (ceiling (* scale (+ x2 excal--scroll-x pad))) 20)
          (+ (ceiling (* scale (+ y2 excal--scroll-y pad))) 20))))

(defun excal--damage-union (a b)
  "Union of damage A and B.
`full' absorbs everything; `scroll' adds nothing beyond the view move,
which `excal--plan-repaint' detects by itself."
  (cond ((or (eq a 'full) (eq b 'full)) 'full)
        ((memq a '(nil scroll)) (or b a))
        ((memq b '(nil scroll)) a)
        (t (list (min (nth 0 a) (nth 0 b)) (min (nth 1 a) (nth 1 b))
                 (max (nth 2 a) (nth 2 b)) (max (nth 3 a) (nth 3 b))))))

(defun excal--damage-vector (damage)
  "Convert DAMAGE to the native [X Y W H] form, or nil for full."
  (when (consp damage)
    (vector (nth 0 damage) (nth 1 damage)
            (- (nth 2 damage) (nth 0 damage))
            (- (nth 3 damage) (nth 1 damage)))))

(defun excal--scene-rect-damage (rect)
  "Return the device-pixel damage covering scene RECT and its outline."
  (pcase-let ((`(,x1 ,y1 ,x2 ,y2) rect)
              (scale (* excal--zoom excal--pixel-scale)))
    (list (- (floor (* scale (+ x1 excal--scroll-x))) 20)
          (- (floor (* scale (+ y1 excal--scroll-y))) 20)
          (+ (ceiling (* scale (+ x2 excal--scroll-x))) 20)
          (+ (ceiling (* scale (+ y2 excal--scroll-y))) 20))))

(defun excal--elements-damage (elements)
  "Return the damage covering ELEMENTS as currently drawn."
  (let (damage)
    (dolist (e elements damage)
      (setq damage (excal--damage-union damage (excal--device-rect e))))))

(defmacro excal--with-elements-damage (elements &rest body)
  "Run BODY and return the damage caused by changing ELEMENTS."
  (declare (indent 1))
  (let ((els (make-symbol "elements")) (before (make-symbol "before")))
    `(let* ((,els ,elements) (,before (excal--elements-damage ,els)))
       ,@body
       (excal--damage-union ,before (excal--elements-damage ,els)))))

(defmacro excal--with-damage (element &rest body)
  "Run BODY and return the damage caused by changing ELEMENT."
  (declare (indent 1))
  (let ((el (make-symbol "element")) (before (make-symbol "before")))
    `(let* ((,el ,element) (,before (excal--device-rect ,el)))
       ,@body
       (excal--damage-union ,before (excal--device-rect ,el)))))

(defun excal--event-window-xy (event)
  "Return EVENT's position relative to the canvas window's text area.
Positions over the mode line or outside the window are not relative to
the text area, so fall back to the absolute pointer position."
  (let* ((posn (event-end event))
         (window (get-buffer-window (current-buffer))))
    (if (and (eq (posn-window posn) window) (null (posn-area posn)))
        (posn-x-y posn)
      (let ((pointer (mouse-absolute-pixel-position))
            (edges (window-inside-absolute-pixel-edges window)))
        (cons (- (car pointer) (nth 0 edges))
              (- (cdr pointer) (nth 1 edges)))))))

(defun excal--event-scene-xy (event)
  "Return EVENT's position as scene coordinates (X . Y)."
  (let* ((xy (excal--event-window-xy event))
         (x (/ (float (car xy)) excal--zoom))
         (y (/ (float (cdr xy)) excal--zoom)))
    (cons (- x excal--scroll-x) (- y excal--scroll-y))))

(defun excal-wheel (event)
  "Pan on plain wheel EVENT, zoom with control."
  (interactive "e")
  (let* ((raw (nth 4 event))
         (basic (event-basic-type event))
         (step (if (and (consp raw) (numberp (cdr raw))) nil 40.0))
         (dx (cond (step (pcase basic ('wheel-left step) ('wheel-right (- step)) (_ 0.0)))
                   ((numberp (car raw)) (- (float (car raw))))
                   (t 0.0)))
         (dy (cond (step (pcase basic ('wheel-up step) ('wheel-down (- step)) (_ 0.0)))
                   (t (- (float (cdr raw)))))))
    (if (memq 'control (event-modifiers event))
        (excal--zoom-at (if (memq basic '(wheel-up)) 1.1 (/ 1 1.1))
                        (posn-x-y (event-start event)))
      (excal--pan dx dy))))

(defun excal--pan (dx dy)
  "Scroll the view by DX, DY logical pixels.
Fractional deltas (trackpads) accumulate until they reach whole pixels,
so panning keeps reusing the framebuffer instead of repainting it."
  (let* ((x (+ dx (car excal--pan-remainder)))
         (y (+ dy (cdr excal--pan-remainder)))
         (ix (truncate x)) (iy (truncate y)))
    (setq excal--pan-remainder (cons (- x ix) (- y iy)))
    (unless (and (zerop ix) (zerop iy))
      (cl-incf excal--scroll-x (/ (float ix) excal--zoom))
      (cl-incf excal--scroll-y (/ (float iy) excal--zoom))
      ;; Pans mixed into a zoom gesture keep previewing.
      (if excal--preview-origin
          (excal--render-preview)
        (excal--render 'scroll)))))

(defun excal-pinch (event)
  "Zoom on pinch EVENT."
  (interactive "e")
  (let ((scale (nth 4 event)))
    (when (numberp scale)
      (excal--zoom-at (/ scale (or (get 'excal-pinch 'last) 1.0))
                      (posn-x-y (nth 1 event)))
      (put 'excal-pinch 'last scale))))

(defun excal--zoom-view (factor xy)
  "Multiply zoom by FACTOR keeping window point XY fixed, without drawing."
  (let* ((old excal--zoom)
         (new (max 0.1 (min 30.0 (* old factor))))
         (x (float (car xy))) (y (float (cdr xy))))
    (setq excal--scroll-x (+ excal--scroll-x (- (/ x new) (/ x old)))
          excal--scroll-y (+ excal--scroll-y (- (/ y new) (/ y old)))
          excal--zoom new)))

(defun excal--zoom-at (factor xy)
  "Multiply zoom by FACTOR keeping window point XY fixed.
The step is shown as a preview; see `excal--render-preview'."
  (excal--zoom-view factor xy)
  (excal--render-preview)
  (message "Zoom %d%%" (round (* 100 excal--zoom))))

(defun excal-zoom-in () "Zoom in." (interactive) (excal--zoom-at 1.25 '(0 . 0)))
(defun excal-zoom-out () "Zoom out." (interactive) (excal--zoom-at 0.8 '(0 . 0)))
(defun excal-zoom-reset ()
  "Reset zoom and scroll."
  (interactive)
  (setq excal--zoom 1.0 excal--scroll-x 0.0 excal--scroll-y 0.0)
  (excal--render))

(defun excal-toggle-pixel-scale ()
  "Toggle between 1x and 2x canvas resolution to compare sharpness."
  (interactive)
  (setq excal--pixel-scale (if (> excal--pixel-scale 1.0) 1.0 2.0)
        excal--canvas-size nil)
  (excal--sync-canvas)
  (message "Canvas pixel scale %.1fx (%dx%d device px)"
           excal--pixel-scale (car excal--canvas-size) (cdr excal--canvas-size)))

(defun excal-write-framebuffer-png (file)
  "Write the current framebuffer pixels to FILE, for debugging.
See `excal-export-png' for exporting the scene."
  (interactive "FWrite framebuffer PNG: ")
  (excal-native-fb-write-png excal--fb (expand-file-name file)))

(provide 'excal-view)
;;; excal-view.el ends here
