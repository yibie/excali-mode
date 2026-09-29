;;; excali-view.el --- Rendering, presentation backends and view navigation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Turns the scene into pixels and puts them on screen: the offscreen
;; framebuffer, the canvas/tiles/layer backends, damage tracking, scroll
;; reuse, panning, zooming and export.

;;; Code:

(require 'excali-core)
(require 'excali-text)
(require 'excali-image)

(declare-function excali--update-pointer "excali-cursor")
(defvar excali--pointer-surfaces)
(declare-function excali--sync-cursor-view "excali-cursor")
(declare-function excali--hide-cursor-view "excali-cursor")
(declare-function excali--overlay-natives "excali-handles")
(declare-function excali--erase-opacity-for "excali-erase")
(declare-function excali--text-native-extras "excali-text")
(declare-function excali-native-fb-copy "excali-module")
(declare-function excali-native-fb-zoom-preview "excali-module")
(declare-function excali-native-fb-mean-diff "excali-module")

(defun excali--flat-points (points)
  "Convert JSON POINTS array of [x y] into a flat float vector."
  (when (vectorp points)
    (let ((flat (make-vector (* 2 (length points)) 0.0)) (i 0))
      (seq-doseq (p points)
        (aset flat i (float (aref p 0)))
        (aset flat (1+ i) (float (aref p 1)))
        (cl-incf i 2))
      flat)))

(defun excali--native-element (element)
  "Return the cached native vector for ELEMENT."
  (let ((native
         (or (gethash element excali--native-cache)
             (puthash
              element
              (vector (excali--get element 'type)
                      (excali--get element 'x) (excali--get element 'y)
                      (excali--get element 'width) (excali--get element 'height)
                      (excali--get element 'angle)
                      (excali--get element 'strokeColor)
                      (excali--get element 'backgroundColor)
                      (excali--get element 'fillStyle)
                      (excali--get element 'strokeWidth)
                      (excali--get element 'roughness)
                      (excali--get element 'seed)
                      (excali--flat-points (excali--get element 'points))
                      (excali--get element 'text)
                      (excali--get element 'fontSize)
                      (excali--get element 'opacity)
                      nil
                      (excali--get element 'strokeStyle)
                      (excali--get element 'fontFamily)
                      (excali--get element 'textAlign)
                      (excali--get element 'lineHeight)
                      (and (excali--get element 'roundness) t)
                      (excali--get element 'startArrowhead)
                      (excali--get element 'endArrowhead)
                      (excali--native-shape-extras element)
                      (excali--native-text-extras element)
                      ;; Optional last slot, see `SLOT_MEDIA_EXTRAS'.
                      (excali--native-media-extras element))
              excali--native-cache))))
    (when (fboundp 'excali--erase-opacity-for)
      ;; Elements marked by the eraser fade; recomputed every frame.
      (aset native 15 (excali--erase-opacity-for element (excali--get element 'opacity))))
    native))

(defun excali--native-shape-extras (element)
  "Return extra shape rendering properties of ELEMENT for the module.
The result is a vector [KEY VALUE ...] with string keys, read in C by
`get_extra_*' in excali-module.c.  Keys: \"roundnessType\",
\"roundnessValue\", \"elbowed\", and for freedraw \"pressures\",
\"simulatePressure\" (1, 0, or absent), \"strokeVariability\" and
\"streamline\", and for sticky notes \"stickyFooter\"."
  (let ((roundness (excali--get element 'roundness))
        (extras nil))
    (when (consp roundness)
      (when-let* ((type (excali--get roundness 'type)))
        (push "roundnessType" extras) (push type extras))
      (when-let* ((value (excali--get roundness 'value)))
        (push "roundnessValue" extras) (push value extras)))
    (when (excali--get element 'elbowed)
      (push "elbowed" extras) (push t extras))
    (when (equal (excali--get element 'type) "freedraw")
      (when-let* ((pressures (excali--get element 'pressures)))
        (push "pressures" extras) (push (vconcat pressures) extras))
      (let ((cell (assq 'simulatePressure element)))
        (when cell
          (push "simulatePressure" extras)
          (push (if (memq (cdr cell) '(nil :null :false)) 0 1) extras)))
      (when-let* ((options (excali--get element 'strokeOptions))
                  ((consp options)))
        (when-let* ((variability (excali--get options 'variability)))
          (push "strokeVariability" extras) (push variability extras))
        (when-let* ((streamline (excali--get options 'streamline)))
          (push "streamline" extras) (push streamline extras))))
    (when (equal (excali--get element 'type) "stickynote")
      (when-let* ((footer (excali--sticky-footer element)))
        (push "stickyFooter" extras) (push footer extras)))
    (vconcat (nreverse extras))))

(defun excali--sticky-footer (note)
  "Return NOTE's date label: \"27 Sep\", with the year when it is not this
year and the body is wide enough (upstream sticky note footer)."
  (let ((created (or (excali--get note 'created) (excali--get note 'updated))))
    (when (numberp created)
      (let* ((time (decode-time (/ created 1000.0)))
             (months ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"])
             (label (format "%d %s" (decoded-time-day time)
                            (aref months (1- (decoded-time-month time))))))
        (if (and (/= (decoded-time-year time) (decoded-time-year (decode-time)))
                 (>= (- (or (excali--get note 'width) 0) 32) 80))
            (format "%s %d" label (decoded-time-year time))
          label)))))

(defun excali--native-text-extras (element)
  "Return extra text rendering properties of ELEMENT for the module.
Same format as `excali--native-shape-extras'.  Text elements get
\"vertical-offset\", the first baseline below their top; arrows with a
label get \"label-hole\" [X Y W H], the box cut out of the stroke.
See `excali--text-native-extras' in excali-text.el."
  (excali--text-native-extras element))

(defun excali--visible-elements ()
  "Return live elements, then editor overlays, as a native vector."
  (vconcat (delq nil (mapcar (lambda (e)
                               (unless (excali--get e 'isDeleted)
                                 (excali--native-element e)))
                             excali--elements))
           (excali--overlay-natives)))

(defcustom excali-backend 'auto
  "How rendered pixels reach the screen.
`canvas' copies each frame into one Canvas image.  `tiles' splits the
window into Canvas tiles and refreshes only those whose pixels changed.
`layer' (macOS only) shows frames in a CoreAnimation overlay above the
Emacs view, bypassing `canvas-refresh' entirely.  `auto' uses `layer'
on graphical macOS frames when the module provides it, else `tiles'."
  :type '(choice (const auto) (const canvas) (const tiles) (const layer))
  :group 'excali)

(defun excali--layer-frame-p (frame)
  "Return non-nil if FRAME is a graphical macOS frame."
  (eq (framep frame) 'ns))

(defun excali--resolve-backend (&optional frame)
  "Return the backend a new buffer on FRAME should use.
FRAME defaults to the selected frame.  Resolve `auto' in
`excali-backend', and fall back to `tiles' where `layer' is unavailable."
  (let ((layer (fboundp 'excali-native-layer-create)))
    (pcase excali-backend
      ('auto (if (and layer (excali--layer-frame-p (or frame (selected-frame))))
                 'layer
               'tiles))
      ('layer (if layer 'layer 'tiles))
      (backend backend))))

(defcustom excali-tile-size 256
  "Maximum tile edge in logical pixels for the `tiles' backend."
  :type 'integer
  :group 'excali)

(defvar-local excali--backend nil "Backend used by this buffer.")
(defvar-local excali--fb nil "Module-owned offscreen framebuffer.")
(defvar-local excali--tiles nil "Vector of [CANVAS X Y WIDTH HEIGHT] tiles.")
(defvar-local excali--layer nil "Overlay layer handle for `layer'.")
(defvar-local excali--last-stats nil "Plist describing the last frame.")

;;;; Views
;;
;; A buffer shown in several windows gives each its own view: zoom,
;; scroll, framebuffer, display surfaces, layer and pointer map.  The
;; view variables below always hold the view of `excali--view-window';
;; the others wait in `excali--views'.  `excali--with-view' swaps a
;; window's view in for the duration of its body.
;;
;; The buffer holds one placeholder character.  Each window hides it and
;; shows its own surfaces through an overlay of its own: `display' ""
;; replaces the character and `before-string' carries the images (a
;; display string cannot hold images of its own, an overlay string can).

(defvar-local excali--view-overlay nil "The window's overlay showing its surfaces.")
(defvar-local excali--view-stamp nil
  "`excali--scene-stamp' of the scene the view last rendered.")

(defconst excali--view-variables
  '(excali--zoom excali--scroll-x excali--scroll-y excali--pixel-scale
    excali--canvas excali--canvas-size excali--fb excali--tiles excali--layer
    excali--rendered-origin excali--pan-remainder excali--last-render-time
    excali--last-stats excali--preview-snapshot excali--preview-origin
    excali--preview-timer excali--pointer-surfaces excali--pointer-stamp
    excali--cursor excali--cursor-view excali--cursor-view-shown
    excali--view-overlay excali--view-stamp)
  "Buffer-local variables that belong to one window's view.")

(defvar-local excali--view-window nil
  "The window whose view the view variables hold, or nil before the first.")
(defvar-local excali--views nil
  "Hash table from windows to the saved views of the other windows.
A saved view is an alist of `excali--view-variables' and their values.")

(defun excali--view-values ()
  "Return the view variables' values as an alist."
  (mapcar (lambda (var) (cons var (symbol-value var))) excali--view-variables))

(defun excali--new-view (window)
  "Return a view for WINDOW that looks where the current one does."
  (append (list (cons 'excali--zoom excali--zoom)
                (cons 'excali--scroll-x excali--scroll-x)
                (cons 'excali--scroll-y excali--scroll-y)
                (cons 'excali--pixel-scale (excali--guess-pixel-scale (window-frame window)))
                (cons 'excali--pan-remainder (cons 0.0 0.0)))
          (mapcar (lambda (var) (cons var nil))
                  (seq-difference excali--view-variables
                                  '(excali--zoom excali--scroll-x excali--scroll-y
                                    excali--pixel-scale excali--pan-remainder)))))

(defun excali--use-view (window)
  "Make WINDOW's view the one the view variables hold.
The first window adopts the buffer's state as its view; a window
without a view yet gets one showing what the current view shows."
  (unless (or (eq window excali--view-window) (null window))
    (unless excali--views
      (setq excali--views (make-hash-table :test #'eq)))
    (when excali--view-window
      (let ((saved (gethash window excali--views)))
        (puthash excali--view-window (excali--view-values) excali--views)
        (remhash window excali--views)
        (dolist (cell (or saved (excali--new-view window)))
          (set (car cell) (cdr cell)))))
    (setq excali--view-window window)))

(defmacro excali--with-view (window &rest body)
  "Run BODY with WINDOW's view in the view variables, then swap back."
  (declare (indent 1))
  (let ((old (make-symbol "old")))
    `(let ((,old excali--view-window))
       (excali--use-view ,window)
       (unwind-protect (progn ,@body)
         (when (window-live-p ,old)
           (excali--use-view ,old))))))

(defun excali--view-window ()
  "Return the window of the current view, else one showing the buffer."
  (if (window-live-p excali--view-window)
      excali--view-window
    (get-buffer-window (current-buffer))))

(defun excali--view-value (window var)
  "Return the value of view variable VAR in WINDOW's view."
  (let ((saved (and excali--views (not (eq window excali--view-window))
                    (gethash window excali--views))))
    (if saved (alist-get var saved) (symbol-value var))))

(defun excali--view-windows ()
  "Return the live windows showing this buffer, on any frame."
  (get-buffer-window-list (current-buffer) 'nomini t))

(defun excali--other-views ()
  "Return the windows other than `excali--view-window' that have a view."
  (and excali--views
       (let (windows)
         (maphash (lambda (window _) (push window windows)) excali--views)
         windows)))

(defun excali--release-view (window)
  "Forget WINDOW's view: hide its layer and cursor view, drop its overlay."
  (excali--with-view window
    (excali--cancel-preview)
    (excali--hide-layer)
    (when (fboundp 'excali--hide-cursor-view) (excali--hide-cursor-view))
    (when (overlayp excali--view-overlay) (delete-overlay excali--view-overlay))
    (setq excali--view-overlay nil excali--canvas-size nil excali--fb nil
          excali--canvas nil excali--tiles nil excali--pointer-surfaces nil))
  (if (eq window excali--view-window)
      ;; The current view goes: take any other one.
      (let ((next (car (excali--other-views))))
        (if next
            (progn (excali--use-view next) (remhash window excali--views))
          (setq excali--view-window nil)))
    (remhash window excali--views)))

(defun excali--release-views ()
  "Release every view of this buffer, as it is killed."
  (dolist (window (cons excali--view-window (excali--other-views)))
    (when window (excali--release-view window))))

(defun excali--prune-views ()
  "Release the views of windows that no longer show this buffer."
  (let ((showing (excali--view-windows)))
    (dolist (window (cons excali--view-window (excali--other-views)))
      (when (and window (not (memq window showing)))
        (excali--release-view window)))))

(defun excali--scene-stamp ()
  "Return what every view of the scene depends on."
  (let ((h 0))
    (dolist (e excali--elements)
      (setq h (logand (+ (* h 31) (or (excali--get e 'versionNonce) 0))
                      most-positive-fixnum)))
    (list h (length excali--elements)
          (mapcar (lambda (e) (excali--get e 'id)) excali--selection)
          (and excali--editing-linear (excali--get excali--editing-linear 'id))
          excali--editing-group excali--theme (bound-and-true-p excali--grid-enabled)
          (excali--canvas-color))))

(defun excali--command-window ()
  "Return the window the current command acts in, if it shows this buffer.
Mouse events act where they happen, other input in the selected window."
  (let* ((event last-input-event)
         (window (if (and (consp event) (consp (cdr event)) (consp (cadr event))
                          (windowp (posn-window (event-start event))))
                     (posn-window (event-start event))
                   (selected-window))))
    (and (window-live-p window) (eq (window-buffer window) (current-buffer))
         window)))

(defun excali--select-view ()
  "Before a command, give it the view of the window it acts in.
A press in another window showing the buffer selects that window, as
clicks do in Emacs."
  (when-let* ((window (excali--command-window)))
    (when (and (not (eq window (selected-window)))
               (memq 'down (event-modifiers last-input-event)))
      (select-window window))
    (excali--use-view window)))

(defun excali--sync-views ()
  "After a command, redraw the other windows' views if the scene changed."
  (when (excali--other-views)
    (let ((stamp (excali--scene-stamp)))
      (setq excali--view-stamp stamp)
      (dolist (window (excali--other-views))
        (when (and (window-live-p window)
                   (not (equal stamp (excali--view-value window 'excali--view-stamp))))
          (excali--with-view window
            (excali--render)
            (setq excali--view-stamp stamp)))))))

(defun excali--view-origin ()
  "Return (ZOOM PIXEL-SCALE X Y): the scene origin in device pixels."
  (let ((scale (* excali--zoom excali--pixel-scale)))
    (list excali--zoom excali--pixel-scale
          (* scale excali--scroll-x) (* scale excali--scroll-y))))

(defun excali--integral-p (x)
  "Return non-nil if X is within rounding error of an integer."
  (< (abs (- x (round x))) 1e-6))

(defun excali--plan-repaint (damage)
  "Prepare the framebuffer for DAMAGE and return what to repaint.
Return nil to repaint everything, `none' when nothing changed, or a
vector of native [X Y W H] rectangles.  If the view only moved by whole
device pixels since the last render, the framebuffer is shifted in place
so only the newly exposed strips need painting."
  (let ((old excali--rendered-origin)
        (new (excali--view-origin)))
    (setq excali--rendered-origin new)
    (when (and damage (not (eq damage 'full)) old
               (= (nth 0 old) (nth 0 new)) (= (nth 1 old) (nth 1 new)))
      (let ((dx (- (nth 2 new) (nth 2 old)))
            (dy (- (nth 3 new) (nth 3 old)))
            (w (car excali--canvas-size))
            (h (cdr excali--canvas-size))
            (rects nil))
        (when (and (excali--integral-p dx) (excali--integral-p dy)
                   (< (abs dx) w) (< (abs dy) h))
          (setq dx (round dx) dy (round dy))
          (unless (and (zerop dx) (zerop dy))
            (excali-native-fb-scroll excali--fb dx dy)
            (cond ((> dx 0) (push (vector 0 0 dx h) rects))
                  ((< dx 0) (push (vector (+ w dx) 0 (- dx) h) rects)))
            (cond ((> dy 0) (push (vector 0 0 w dy) rects))
                  ((< dy 0) (push (vector 0 (+ h dy) w (- dy)) rects))))
          (when (consp damage)
            (push (excali--damage-vector damage) rects))
          (if rects (vconcat rects) 'none))))))

(defun excali-toggle-theme ()
  "Switch the canvas between the light and dark themes."
  (interactive)
  (setq excali--theme (if (eq excali--theme 'dark) 'light 'dark))
  (when excali--native-cache (clrhash excali--native-cache))
  (excali--render)
  (message "Theme: %s" excali--theme))

(defun excali--canvas-color ()
  "Return the scene's background color, `viewBackgroundColor', or nil."
  (let ((color (alist-get 'viewBackgroundColor (alist-get 'appState excali--doc))))
    (and (stringp color) (string-prefix-p "#" color) color)))

(defun excali--render (&optional damage)
  "Render the scene and present it.
DAMAGE nil repaints everything.  `scroll' means only the view moved; a
device rectangle (X1 Y1 X2 Y2) marks changed scene content.  See
`excali--plan-repaint' for how moved views reuse existing pixels.  A
zoom preview on screen is replaced by a full render."
  (excali--cancel-preview)
  (when excali--fb
    (let* ((t0 (float-time))
           (plan (excali--plan-repaint damage))
           (drawn (if (eq plan 'none)
                      0
                    (excali-native-fb-render
                     excali--fb excali--pixel-scale excali--zoom
                     excali--scroll-x excali--scroll-y
                     (excali--visible-elements) plan
                     (excali--canvas-color) (eq excali--theme 'dark)))))
      (excali--present-frame t0 drawn nil))))

(defun excali--present-frame (start drawn preview)
  "Present the framebuffer and record stats for a frame begun at START.
DRAWN is the number of elements rendered; PREVIEW is non-nil when the
frame is a zoom preview."
  (let* ((t1 (float-time))
         (refreshed (excali--present))
         (t2 (float-time)))
    (setq excali--last-render-time (- t1 start)
          excali--last-stats (list :drawn drawn
                                  :render-ms (* 1000 (- t1 start))
                                  :present-ms (* 1000 (- t2 t1))
                                  :refreshed refreshed
                                  :preview preview))))

;;;; Zoom preview

(defcustom excali-zoom-preview-delay 0.1
  "Seconds without zoom input before a zoom preview is rendered crisply.
While zooming, each step scales the pixels of the last full render
instead of rendering the scene again; once zooming pauses this long a
full render replaces the preview.  nil renders every zoom step fully."
  :type '(choice (const :tag "Render every step" nil) number)
  :group 'excali)

(defcustom excali-zoom-preview-limit 4.0
  "Largest factor a zoom preview may scale the last full render by.
A zoom step that goes further (in or out) renders fully, and later
steps preview from that render instead."
  :type 'number
  :group 'excali)

(defvar-local excali--preview-snapshot nil
  "Framebuffer holding a copy of the full render being previewed.")
(defvar-local excali--preview-origin nil
  "View origin of `excali--preview-snapshot' while a preview is shown.
nil when no preview is shown; see `excali--view-origin'.")
(defvar-local excali--preview-timer nil
  "Timer that replaces the zoom preview with a full render, or nil.")

(defun excali--cancel-preview ()
  "Cancel any pending preview render and forget the preview origin.
The framebuffer may still hold preview pixels; `excali--rendered-origin'
is nil then, so the next render repaints everything."
  (when excali--preview-timer
    (cancel-timer excali--preview-timer)
    (setq excali--preview-timer nil))
  (setq excali--preview-origin nil))

(defun excali--finish-preview (buffer &optional window)
  "Replace the zoom preview in BUFFER with a full render.
WINDOW is the window whose view shows the preview, if the buffer has
views.  This runs from `excali--preview-timer'."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (excali--with-view (if (window-live-p window) window excali--view-window)
        ;; `excali--render' cancels the timer too.
        (if excali--preview-origin
            (excali--render)
          (excali--cancel-preview))))))

(defun excali--snapshot-framebuffer ()
  "Copy the framebuffer into `excali--preview-snapshot'."
  (unless (and excali--preview-snapshot
               (excali-native-fb-copy excali--fb excali--preview-snapshot))
    (setq excali--preview-snapshot
          (excali-native-fb-create (car excali--canvas-size)
                                  (cdr excali--canvas-size)))
    (excali-native-fb-copy excali--fb excali--preview-snapshot)))

(defun excali--preview-factor (base)
  "Return the scale from view origin BASE to the current view, or nil.
nil means the current view cannot be previewed from BASE."
  (let ((new (excali--view-origin)))
    (when (and base (= (nth 1 base) (nth 1 new)))
      (let ((factor (/ (nth 0 new) (float (nth 0 base)))))
        (when (<= (/ 1.0 excali-zoom-preview-limit) factor
                  excali-zoom-preview-limit)
          factor)))))

(defun excali--render-preview ()
  "Show the current view by transforming the last full render.
The first preview after a full render keeps a pristine copy of it, and
every later step scales that copy, so steps never compound blur or
rounding.  Fall back to `excali--render' when previews are disabled, the
framebuffer holds no exact render, or the view moved too far from it.
A full render follows once no step came for `excali-zoom-preview-delay'."
  (let* ((base (or excali--preview-origin excali--rendered-origin))
         (factor (and excali-zoom-preview-delay excali--fb
                      (excali--preview-factor base))))
    (if (not factor)
        (excali--render)
      (let ((t0 (float-time))
            (new (excali--view-origin)))
        (unless excali--preview-origin
          (excali--snapshot-framebuffer)
          (setq excali--preview-origin base))
        ;; A scene point at device pixel D in the snapshot is now at
        ;; FACTOR * D + NEW - FACTOR * BASE.
        (excali-native-fb-zoom-preview
         excali--fb excali--preview-snapshot factor
         (- (nth 2 new) (* factor (nth 2 base)))
         (- (nth 3 new) (* factor (nth 3 base))))
        ;; The pixels match no exact origin now: never scroll-reuse them.
        (setq excali--rendered-origin nil)
        (excali--present-frame t0 0 t)
        (when excali--preview-timer
          (cancel-timer excali--preview-timer))
        (setq excali--preview-timer
              (apply #'run-with-timer excali-zoom-preview-delay nil
                     #'excali--finish-preview (current-buffer)
                     (and excali--view-window (list excali--view-window))))
        (add-hook 'kill-buffer-hook #'excali--cancel-preview nil t)))))

(defun excali--present ()
  "Push the framebuffer to the screen; return the surfaces refreshed."
  (pcase excali--backend
    ('canvas
     (excali-native-fb-present-canvas excali--fb excali--canvas)
     (canvas-refresh excali--canvas)
     1)
    ('tiles
     (let ((dirty (excali-native-fb-present-tiles excali--fb excali--tiles)))
       (dolist (i dirty)
         (canvas-refresh (aref (aref excali--tiles i) 0)))
       (length dirty)))
    ('layer
     (excali-native-layer-present excali--layer excali--fb)
     1)))

(defun excali--guess-pixel-scale (&optional frame)
  "Return the device pixel ratio of FRAME, by default the selected one."
  (or excali-pixel-scale
      (let ((scale (and (fboundp 'frame-scale-factor) (frame-scale-factor frame))))
        (if (and (numberp scale) (> scale 0)) (float scale) 1.0))))

(defun excali--make-canvas (width height)
  "Return a WIDTH by HEIGHT device-pixel canvas for the current scale.
`:map' comes first so that changing the pointer map in place leaves the
image cache's hash alone (see excali-cursor.el)."
  (list 'image :map nil :type 'canvas :id (gensym "excali-canvas-")
        :data-width width :data-height height
        :scale (/ 1.0 excali--pixel-scale) :ascent 'center))

(defun excali--split (total size)
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

(defun excali--make-tiles (width height)
  "Make Canvas tiles covering WIDTH by HEIGHT logical pixels.
Set `excali--tiles' and return the string showing them, rows of tiles
separated by newlines."
  (let ((scale excali--pixel-scale)
        (rows (excali--split height excali-tile-size))
        tiles parts)
    (dolist (row rows)
      (dolist (col (excali--split width excali-tile-size))
        (let* ((x (round (* scale (car col)))) (w (round (* scale (cdr col))))
               (y (round (* scale (car row)))) (h (round (* scale (cdr row))))
               (canvas (excali--make-canvas w h)))
          (push (vector canvas x y w h) tiles)
          (push (propertize " " 'display canvas) parts)))
      (unless (eq row (car (last rows)))
        (push "\n" parts)))
    (setq excali--tiles (vconcat (nreverse tiles)))
    (apply #'concat (nreverse parts))))

(defun excali--sync-layer (window width height)
  "Attach and place the overlay layer over WINDOW's WIDTH by HEIGHT body."
  (unless excali--layer
    (pcase-let ((`(,left ,top ,right ,bottom)
                 (frame-edges (window-frame window) 'native-edges)))
      (setq excali--layer (excali-native-layer-create
                          left top (- right left) (- bottom top))))
    (unless excali--layer
      (error "No Emacs view found for the overlay layer")))
  (pcase-let ((`(,x ,y . ,_) (window-inside-pixel-edges window)))
    (excali-native-layer-set-geometry excali--layer x y width height
                                     excali--pixel-scale t)))

(defun excali--hide-layer ()
  "Hide this buffer's overlay layer, if any."
  (when excali--layer
    (excali-native-layer-set-geometry excali--layer 0 0 1 1 1.0 nil)))

(defun excali--ensure-placeholder ()
  "Make the buffer hold just the placeholder character the views replace."
  (unless (equal (buffer-substring-no-properties (point-min) (point-max)) " ")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert " ")
      (goto-char (point-min)))))

(defun excali--make-surfaces (width height)
  "Make the view's display surfaces for WIDTH by HEIGHT logical pixels.
Return the string that shows them in the window."
  (pcase excali--backend
    ('canvas
     (setq excali--canvas (excali--make-canvas (car excali--canvas-size)
                                              (cdr excali--canvas-size)))
     (propertize " " 'display excali--canvas))
    ('tiles (excali--make-tiles width height))
    ('layer
     ;; The layer shows the pixels; underneath, a 1x1 canvas stretched
     ;; over the window takes mouse events and carries the pointer map.
     (setq excali--canvas
           (list 'image :map nil :type 'canvas :id (gensym "excali-pointer-")
                 :data-width 1 :data-height 1 :width width :height height
                 :scale 1 :ascent 'center))
     (propertize " " 'display excali--canvas))))

(defun excali--sync-canvas (&optional window)
  "Size WINDOW's display surfaces to its body and render its view.
WINDOW defaults to the window of the current view, else the selected one."
  (interactive)
  (let ((window (or window
                    (and (window-live-p excali--view-window) excali--view-window)
                    (get-buffer-window (current-buffer)))))
    (excali--with-view window
      (let* ((width (max 1 (window-body-width window t)))
             (height (max 1 (window-body-height window t)))
             (dw (round (* width excali--pixel-scale)))
             (dh (round (* height excali--pixel-scale))))
        (unless (and (equal excali--canvas-size (cons dw dh))
                     (overlayp excali--view-overlay)
                     (overlay-buffer excali--view-overlay))
          (setq excali--canvas-size (cons dw dh)
                excali--fb (excali-native-fb-create dw dh)
                excali--rendered-origin nil
                excali--canvas nil
                excali--tiles nil)
          (excali--ensure-placeholder)
          (unless (and (overlayp excali--view-overlay) (overlay-buffer excali--view-overlay))
            (setq excali--view-overlay (make-overlay (point-min) (point-max) nil nil t))
            (overlay-put excali--view-overlay 'window window)
            (overlay-put excali--view-overlay 'display ""))
          (move-overlay excali--view-overlay (point-min) (point-max))
          (overlay-put excali--view-overlay 'before-string
                       (excali--make-surfaces width height))
          (setq excali--pointer-surfaces
                (if (eq excali--backend 'tiles)
                    (mapcar (lambda (tile)
                              (cons (aref tile 0)
                                    (cons (round (/ (aref tile 1) excali--pixel-scale))
                                          (round (/ (aref tile 2) excali--pixel-scale)))))
                            excali--tiles)
                  (list (cons excali--canvas (cons 0 0)))))
          ;; Canvas pixel buffers exist only once the images are displayed.
          (redisplay t))
        (if (eq excali--backend 'layer)
            (excali--sync-layer window width height)
          (excali--hide-layer))
        (excali--sync-cursor-view window width height)
        (excali--render)
        (setq excali--view-stamp (excali--scene-stamp))
        (excali--update-pointer t)))))

(defun excali--window-size-change (frame)
  "Resize the views of excali buffers shown on FRAME; drop gone ones."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'excali-mode)
        (excali--prune-views)
        (dolist (window (excali--view-windows))
          (when (eq (window-frame window) frame)
            (excali--sync-canvas window)))))))

(defun excali-cycle-backend ()
  "Switch to the next presentation backend."
  (interactive)
  (let* ((backends (if (fboundp 'excali-native-layer-create)
                       '(canvas tiles layer)
                     '(canvas tiles)))
         (next (or (cadr (memq excali--backend backends)) (car backends))))
    (excali--use-backend next)
    (message "Backend: %s" next)))

(defun excali--use-backend (backend)
  "Switch this buffer to BACKEND and redraw every view."
  (setq excali--backend backend)
  (dolist (window (or (excali--view-windows) (list (selected-window))))
    (excali--with-view window
      (unless (eq backend 'layer) (excali--hide-layer))
      (setq excali--canvas-size nil))
    (excali--sync-canvas window)))

(defun excali--device-rect (element)
  "Return ELEMENT's padded bounds as (X1 Y1 X2 Y2) in device pixels."
  (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) (excali--bounds element))
               (scale (* excali--zoom excali--pixel-scale))
               (pad (+ 40 (* 2 (or (excali--get element 'strokeWidth) 2))
                       (* 6 (or (excali--get element 'roughness) 1)))))
    (when (and (numberp (excali--get element 'angle))
               (/= 0 (excali--get element 'angle)))
      (let ((cx (/ (+ x1 x2) 2.0)) (cy (/ (+ y1 y2) 2.0))
            (r (/ (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))) 2.0)))
        (setq x1 (- cx r) x2 (+ cx r) y1 (- cy r) y2 (+ cy r))))
    (list (- (floor (* scale (+ x1 excali--scroll-x (- pad)))) 20)
          (- (floor (* scale (+ y1 excali--scroll-y (- pad)))) 20)
          (+ (ceiling (* scale (+ x2 excali--scroll-x pad))) 20)
          (+ (ceiling (* scale (+ y2 excali--scroll-y pad))) 20))))

(defun excali--damage-union (a b)
  "Union of damage A and B.
`full' absorbs everything; `scroll' adds nothing beyond the view move,
which `excali--plan-repaint' detects by itself."
  (cond ((or (eq a 'full) (eq b 'full)) 'full)
        ((memq a '(nil scroll)) (or b a))
        ((memq b '(nil scroll)) a)
        (t (list (min (nth 0 a) (nth 0 b)) (min (nth 1 a) (nth 1 b))
                 (max (nth 2 a) (nth 2 b)) (max (nth 3 a) (nth 3 b))))))

(defun excali--damage-vector (damage)
  "Convert DAMAGE to the native [X Y W H] form, or nil for full."
  (when (consp damage)
    (vector (nth 0 damage) (nth 1 damage)
            (- (nth 2 damage) (nth 0 damage))
            (- (nth 3 damage) (nth 1 damage)))))

(defun excali--scene-rect-damage (rect)
  "Return the device-pixel damage covering scene RECT and its outline."
  (pcase-let ((`(,x1 ,y1 ,x2 ,y2) rect)
              (scale (* excali--zoom excali--pixel-scale)))
    (list (- (floor (* scale (+ x1 excali--scroll-x))) 20)
          (- (floor (* scale (+ y1 excali--scroll-y))) 20)
          (+ (ceiling (* scale (+ x2 excali--scroll-x))) 20)
          (+ (ceiling (* scale (+ y2 excali--scroll-y))) 20))))

(defun excali--elements-damage (elements)
  "Return the damage covering ELEMENTS as currently drawn."
  (let (damage)
    (dolist (e elements damage)
      (setq damage (excali--damage-union damage (excali--device-rect e))))))

(defmacro excali--with-elements-damage (elements &rest body)
  "Run BODY and return the damage caused by changing ELEMENTS."
  (declare (indent 1))
  (let ((els (make-symbol "elements")) (before (make-symbol "before")))
    `(let* ((,els ,elements) (,before (excali--elements-damage ,els)))
       ,@body
       (excali--damage-union ,before (excali--elements-damage ,els)))))

(defmacro excali--with-damage (element &rest body)
  "Run BODY and return the damage caused by changing ELEMENT."
  (declare (indent 1))
  (let ((el (make-symbol "element")) (before (make-symbol "before")))
    `(let* ((,el ,element) (,before (excali--device-rect ,el)))
       ,@body
       (excali--damage-union ,before (excali--device-rect ,el)))))

(defun excali--canvas-area-p (posn)
  "Return non-nil if POSN is over the canvas: the text area or a hot spot."
  (memq (posn-area posn) '(nil excali-canvas)))

(defun excali--event-window-xy (event)
  "Return EVENT's position relative to the view's window's text area.
Positions over the mode line or outside the window are not relative to
the text area, so fall back to the absolute pointer position."
  (let* ((posn (event-end event))
         (window (excali--view-window)))
    (if (and (eq (posn-window posn) window) (excali--canvas-area-p posn))
        (posn-x-y posn)
      (let ((pointer (mouse-absolute-pixel-position))
            (edges (window-inside-absolute-pixel-edges window)))
        (cons (- (car pointer) (nth 0 edges))
              (- (cdr pointer) (nth 1 edges)))))))

(defun excali--event-scene-xy (event)
  "Return EVENT's position as scene coordinates (X . Y)."
  (let* ((xy (excali--event-window-xy event))
         (x (/ (float (car xy)) excali--zoom))
         (y (/ (float (cdr xy)) excali--zoom)))
    (cons (- x excali--scroll-x) (- y excali--scroll-y))))

(defcustom excali-wheel-step 40
  "Logical pixels one wheel notch pans when its event carries no pixel delta."
  :type 'number
  :group 'excali)

(defun excali--wheel-delta (event)
  "Return (DX . DY) to pan the view by for wheel EVENT.
The canvas moves the way Emacs scrolls a buffer on the same system:
`wheel-up' moves it down and `wheel-down' up, as `mwheel-scroll' moves
text, and `wheel-left' moves it left and `wheel-right' right, swapped by
`mouse-wheel-flip-direction'.  With shift, a vertical wheel pans
sideways, as upstream does.  The direction comes from the event's name:
window systems disagree on the sign of the pixel delta, and Windows
reports horizontal amounts on its y part, so the delta only says how
far."
  (let* ((raw (nth 4 event))
         (basic (event-basic-type event))
         (horizontal (memq basic '(wheel-left wheel-right)))
         (delta (and (consp raw)
                     (seq-find (lambda (v) (and (numberp v) (not (zerop v))))
                               (if horizontal (list (car raw) (cdr raw))
                                 (list (cdr raw) (car raw))))))
         (amount (if delta (abs (float delta)) (float excali-wheel-step)))
         (flip (if mouse-wheel-flip-direction -1 1))
         (d (pcase basic
              ('wheel-up (cons 0.0 amount))
              ('wheel-down (cons 0.0 (- amount)))
              ('wheel-left (cons (* flip (- amount)) 0.0))
              ('wheel-right (cons (* flip amount) 0.0))
              (_ (cons 0.0 0.0)))))
    (if (and (memq 'shift (event-modifiers event)) (not horizontal))
        (cons (cdr d) 0.0)
      d)))

(defun excali-wheel (event)
  "Pan on wheel EVENT (sideways with shift), zoom with control."
  (interactive "e")
  (if (memq 'control (event-modifiers event))
      (excali--zoom-at (if (eq (event-basic-type event) 'wheel-up) 1.1 (/ 1 1.1))
                       (posn-x-y (event-start event)))
    (let ((d (excali--wheel-delta event)))
      (excali--pan (car d) (cdr d)))))

(defun excali--pan (dx dy)
  "Scroll the view by DX, DY logical pixels.
Fractional deltas (trackpads) accumulate until they reach whole pixels,
so panning keeps reusing the framebuffer instead of repainting it."
  (let* ((x (+ dx (car excali--pan-remainder)))
         (y (+ dy (cdr excali--pan-remainder)))
         (ix (truncate x)) (iy (truncate y)))
    (setq excali--pan-remainder (cons (- x ix) (- y iy)))
    (unless (and (zerop ix) (zerop iy))
      (cl-incf excali--scroll-x (/ (float ix) excali--zoom))
      (cl-incf excali--scroll-y (/ (float iy) excali--zoom))
      ;; Pans mixed into a zoom gesture keep previewing.
      (if excali--preview-origin
          (excali--render-preview)
        (excali--render 'scroll)))))

(defun excali-pinch (event)
  "Zoom on pinch EVENT."
  (interactive "e")
  (let ((scale (nth 4 event)))
    (when (numberp scale)
      (excali--zoom-at (/ scale (or (get 'excali-pinch 'last) 1.0))
                      (posn-x-y (nth 1 event)))
      (put 'excali-pinch 'last scale))))

(defun excali--zoom-view (factor xy)
  "Multiply zoom by FACTOR keeping window point XY fixed, without drawing."
  (let* ((old excali--zoom)
         (new (max 0.1 (min 30.0 (* old factor))))
         (x (float (car xy))) (y (float (cdr xy))))
    (setq excali--scroll-x (+ excali--scroll-x (- (/ x new) (/ x old)))
          excali--scroll-y (+ excali--scroll-y (- (/ y new) (/ y old)))
          excali--zoom new)))

(defun excali--zoom-at (factor xy)
  "Multiply zoom by FACTOR keeping window point XY fixed.
The step is shown as a preview; see `excali--render-preview'."
  (excali--zoom-view factor xy)
  (excali--render-preview)
  (message "Zoom %d%%" (round (* 100 excali--zoom))))

(defun excali-zoom-in () "Zoom in." (interactive) (excali--zoom-at 1.25 '(0 . 0)))
(defun excali-zoom-out () "Zoom out." (interactive) (excali--zoom-at 0.8 '(0 . 0)))
(defun excali-zoom-reset ()
  "Reset zoom and scroll."
  (interactive)
  (setq excali--zoom 1.0 excali--scroll-x 0.0 excali--scroll-y 0.0)
  (excali--render))

(defun excali-toggle-pixel-scale ()
  "Toggle between 1x and 2x canvas resolution to compare sharpness."
  (interactive)
  (setq excali--pixel-scale (if (> excali--pixel-scale 1.0) 1.0 2.0)
        excali--canvas-size nil)
  (excali--sync-canvas)
  (message "Canvas pixel scale %.1fx (%dx%d device px)"
           excali--pixel-scale (car excali--canvas-size) (cdr excali--canvas-size)))

(defun excali-write-framebuffer-png (file)
  "Write the current framebuffer pixels to FILE, for debugging.
See `excali-export-png' for exporting the scene."
  (interactive "FWrite framebuffer PNG: ")
  (excali-native-fb-write-png excali--fb (expand-file-name file)))

(provide 'excali-view)
;;; excali-view.el ends here
