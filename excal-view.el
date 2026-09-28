;;; excal-view.el --- Rendering, presentation backends and view navigation  -*- lexical-binding: t; -*-

;;; Commentary:

;; Turns the scene into pixels and puts them on screen: the offscreen
;; framebuffer, the canvas/tiles/layer backends, damage tracking, scroll
;; reuse, panning, zooming and export.

;;; Code:

(require 'excal-core)

(declare-function excal--update-pointer "excal-edit")

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
                      (and (excal--get element 'roundness) t))
              excal--native-cache))))
    (aset native 16 (eq element excal--selected))
    native))

(defun excal--visible-elements ()
  "Return live elements as a native vector."
  (vconcat (delq nil (mapcar (lambda (e)
                               (unless (excal--get e 'isDeleted)
                                 (excal--native-element e)))
                             excal--elements))))

(defcustom excal-backend 'tiles
  "How rendered pixels reach the screen.
`canvas' copies each frame into one Canvas image.  `tiles' splits the
window into Canvas tiles and refreshes only those whose pixels changed.
`layer' (macOS only) shows frames in a CoreAnimation overlay above the
Emacs view, bypassing `canvas-refresh' entirely."
  :type '(choice (const canvas) (const tiles) (const layer))
  :group 'excal)

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

(defun excal--render (&optional damage)
  "Render the scene and present it.
DAMAGE nil repaints everything.  `scroll' means only the view moved; a
device rectangle (X1 Y1 X2 Y2) marks changed scene content.  See
`excal--plan-repaint' for how moved views reuse existing pixels."
  (when excal--fb
    (let* ((t0 (float-time))
           (plan (excal--plan-repaint damage))
           (drawn (if (eq plan 'none)
                      0
                    (excal-native-fb-render
                     excal--fb excal--pixel-scale excal--zoom
                     excal--scroll-x excal--scroll-y
                     (excal--visible-elements) plan)))
           (t1 (float-time))
           (refreshed (excal--present))
           (t2 (float-time)))
      (setq excal--last-render-time (- t1 t0)
            excal--last-stats (list :drawn drawn
                                    :render-ms (* 1000 (- t1 t0))
                                    :present-ms (* 1000 (- t2 t1))
                                    :refreshed refreshed)))))

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
      (let ((inhibit-read-only t))
        (erase-buffer)
        (pcase excal--backend
          ('canvas
           (setq excal--canvas (excal--make-canvas dw dh))
           (insert (propertize " " 'display excal--canvas)))
          ('tiles (excal--insert-tiles width height))
          ('layer
           ;; Reserve the area so mouse events land in the text area.
           (insert (propertize " " 'display
                               `(space :width (,width) :height (,height))))))
        (goto-char (point-min))
        (setq excal--pointer nil))
      ;; Canvas pixel buffers exist only once the images are displayed.
      (redisplay t))
    (if (eq excal--backend 'layer)
        (excal--sync-layer window width height)
      (excal--hide-layer))
    (excal--render)
    (excal--update-pointer)))

(defun excal--window-size-change (frame)
  "Resize or hide the surfaces of excal buffers after FRAME changed."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'excal-mode)
        (if-let* ((window (get-buffer-window buffer frame)))
            (excal--sync-canvas window)
          (excal--hide-layer))))))

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
      (excal--render 'scroll))))

(defun excal-pinch (event)
  "Zoom on pinch EVENT."
  (interactive "e")
  (let ((scale (nth 4 event)))
    (when (numberp scale)
      (excal--zoom-at (/ scale (or (get 'excal-pinch 'last) 1.0))
                      (posn-x-y (nth 1 event)))
      (put 'excal-pinch 'last scale))))

(defun excal--zoom-at (factor xy)
  "Multiply zoom by FACTOR keeping window point XY fixed."
  (let* ((old excal--zoom)
         (new (max 0.1 (min 30.0 (* old factor))))
         (x (float (car xy))) (y (float (cdr xy))))
    (setq excal--scroll-x (+ excal--scroll-x (- (/ x new) (/ x old)))
          excal--scroll-y (+ excal--scroll-y (- (/ y new) (/ y old)))
          excal--zoom new)
    (excal--render)
    (message "Zoom %d%%" (round (* 100 new)))))

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

(defun excal-export-png (file)
  "Write the current canvas pixels to FILE."
  (interactive "FExport PNG: ")
  (excal-native-fb-write-png excal--fb (expand-file-name file)))

(provide 'excal-view)
;;; excal-view.el ends here
