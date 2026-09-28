;;; excal.el --- Excalidraw scenes on Emacs Canvas (spike)  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "32.0"))

;;; Commentary:

;; Path-exploration spike: read and write .excalidraw files, render them
;; through a Cairo/Pango module into a Canvas image, and support basic
;; drawing, selection, panning and zooming.
;;
;; Entry points: `excal-open', `excal-new', `excal-bench'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst excal--directory
  (file-name-directory (or load-file-name buffer-file-name default-directory)))

(unless (featurep 'excal-module)
  (module-load (expand-file-name (concat "excal-module" module-file-suffix)
                                 excal--directory)))

(declare-function excal-native-render "excal-module")
(declare-function excal-native-measure-text "excal-module")
(declare-function excal-native-write-png "excal-module")
(declare-function excal-native-fb-create "excal-module")
(declare-function excal-native-fb-render "excal-module")
(declare-function excal-native-fb-present-canvas "excal-module")
(declare-function excal-native-fb-present-tiles "excal-module")
(declare-function excal-native-fb-write-png "excal-module")
(declare-function excal-native-fb-diff "excal-module")
(declare-function excal-native-fb-scroll "excal-module")
(declare-function excal-native-layer-create "excal-module")
(declare-function excal-native-layer-set-geometry "excal-module")
(declare-function excal-native-layer-present "excal-module")
(declare-function excal-native-layer-flush "excal-module")

(defgroup excal nil
  "Excalidraw scenes on Emacs Canvas."
  :group 'multimedia)

(defcustom excal-pixel-scale nil
  "Device pixels per logical pixel, or nil to guess from the frame."
  :type '(choice (const :tag "Auto" nil) number))

;;;; Document model

(defvar-local excal--file nil "File the scene is saved to.")
(defvar-local excal--doc nil "Top-level .excalidraw alist.")
(defvar-local excal--elements nil "Element alists in z-order.")
(defvar-local excal--canvas nil "Canvas image spec.")
(defvar-local excal--canvas-size nil "(WIDTH . HEIGHT) in device pixels.")
(defvar-local excal--pixel-scale 1.0)
(defvar-local excal--zoom 1.0)
(defvar-local excal--scroll-x 0.0)
(defvar-local excal--scroll-y 0.0)
(defvar-local excal--tool 'select)
(defvar-local excal--selected nil "Selected element alist, or nil.")
(defvar-local excal--pointer nil "Pointer shape currently shown over the canvas.")
(defvar-local excal--rendered-origin nil
  "View origin of the framebuffer's contents; see `excal--view-origin'.")
(defvar-local excal--pan-remainder '(0.0 . 0.0)
  "Sub-pixel pan distance not yet applied; see `excal--pan'.")
(defvar-local excal--native-cache nil
  "Hash table mapping element alists to native vectors.")
(defvar-local excal--last-render-time nil
  "Seconds spent in the last native render.")

(defun excal--get (element key)
  "Return KEY of ELEMENT, mapping JSON null and false to nil."
  (let ((value (alist-get key element)))
    (if (memq value '(:null :false)) nil value)))

(defun excal--put (element key value)
  "Destructively set KEY of ELEMENT to VALUE."
  (if-let* ((cell (assq key element)))
      (setcdr cell value)
    (nconc element (list (cons key value))))
  value)

(defun excal--touch (element)
  "Invalidate ELEMENT's native cache and bump its version."
  (remhash element excal--native-cache)
  (excal--put element 'version (1+ (or (excal--get element 'version) 0)))
  (excal--put element 'versionNonce (random (ash 1 31)))
  (excal--put element 'updated (truncate (* 1000 (float-time)))))

(defun excal--read-file (file)
  "Parse .excalidraw FILE into an alist."
  (with-temp-buffer
    (insert-file-contents file)
    (json-parse-buffer :object-type 'alist :array-type 'array
                       :null-object :null :false-object :false)))

(defun excal--empty-doc ()
  "Return an empty .excalidraw document."
  (list (cons 'type "excalidraw")
        (cons 'version 2)
        (cons 'source "https://excalidraw.com")
        (cons 'elements [])
        (cons 'appState (list (cons 'viewBackgroundColor "#ffffff")
                              (cons 'gridSize :null)))
        (cons 'files (list))))

(defun excal-save ()
  "Write the scene back to its .excalidraw file."
  (interactive)
  (unless excal--file
    (setq excal--file (read-file-name "Save scene to: " nil nil nil
                                      "untitled.excalidraw")))
  (let ((doc (copy-alist excal--doc)))
    (setf (alist-get 'elements doc) (vconcat excal--elements))
    (unless (alist-get 'files doc) (setf (alist-get 'files doc) (list)))
    (with-temp-file excal--file
      (setq buffer-file-coding-system 'utf-8-unix)
      (json-insert doc :null-object :null :false-object :false)))
  (message "Saved %s" excal--file))

(defun excal--new-id ()
  "Return a random element id."
  (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (apply #'string (cl-loop repeat 20 collect (aref chars (random (length chars)))))))

(defun excal--make-element (type x y &rest props)
  "Return a new element alist of TYPE at X, Y with extra PROPS alist."
  (let ((element
         (list (cons 'id (excal--new-id)) (cons 'type type)
               (cons 'x (float x)) (cons 'y (float y))
               (cons 'width 0.0) (cons 'height 0.0) (cons 'angle 0)
               (cons 'strokeColor "#1e1e1e") (cons 'backgroundColor "transparent")
               (cons 'fillStyle "solid") (cons 'strokeWidth 2)
               (cons 'strokeStyle "solid") (cons 'roughness 1)
               (cons 'opacity 100) (cons 'groupIds []) (cons 'frameId :null)
               (cons 'roundness :null) (cons 'seed (1+ (random (1- (ash 1 31)))))
               (cons 'version 1) (cons 'versionNonce (random (ash 1 31)))
               (cons 'isDeleted :false) (cons 'boundElements :null)
               (cons 'updated (truncate (* 1000 (float-time))))
               (cons 'link :null) (cons 'locked :false))))
    (dolist (prop props element)
      (excal--put element (car prop) (cdr prop)))))

;;;; Rendering

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

;;;; Presentation backends

(defcustom excal-backend 'tiles
  "How rendered pixels reach the screen.
`canvas' copies each frame into one Canvas image.  `tiles' splits the
window into Canvas tiles and refreshes only those whose pixels changed.
`layer' (macOS only) shows frames in a CoreAnimation overlay above the
Emacs view, bypassing `canvas-refresh' entirely."
  :type '(choice (const canvas) (const tiles) (const layer)))

(defcustom excal-tile-size 256
  "Maximum tile edge in logical pixels for the `tiles' backend."
  :type 'integer)

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

;;;; Damage

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

;;;; Coordinates and hit testing

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

(defun excal--bounds (element)
  "Return (X1 Y1 X2 Y2) of ELEMENT, ignoring rotation."
  (let ((x (excal--get element 'x)) (y (excal--get element 'y))
        (w (excal--get element 'width)) (h (excal--get element 'height)))
    (if-let* ((points (excal--get element 'points))
              ((> (length points) 0)))
        (let ((xs (mapcar (lambda (p) (+ x (aref p 0))) points))
              (ys (mapcar (lambda (p) (+ y (aref p 1))) points)))
          (list (apply #'min xs) (apply #'min ys)
                (apply #'max xs) (apply #'max ys)))
      (list (min x (+ x w)) (min y (+ y h)) (max x (+ x w)) (max y (+ y h))))))

(defun excal--hit (scene-xy)
  "Return the topmost element under SCENE-XY."
  (let ((tolerance (/ 8.0 excal--zoom)))
    (cl-find-if (lambda (element)
                  (and (not (excal--get element 'isDeleted))
                       (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excal--bounds element)))
                         (and (<= (- x1 tolerance) (car scene-xy) (+ x2 tolerance))
                              (<= (- y1 tolerance) (cdr scene-xy) (+ y2 tolerance))))))
                (reverse excal--elements))))

;;;; Resizing

(defun excal--rotated-p (element)
  "Return non-nil if ELEMENT is rotated."
  (let ((angle (excal--get element 'angle)))
    (and (numberp angle) (/= angle 0))))

(defun excal--handles (element)
  "Return ELEMENT's resize handles as ((NAME . (X . Y)) ...) in scene units.
Keep the layout in sync with `draw_selection' in excal-render.c."
  (unless (excal--rotated-p element)
    (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) (excal--bounds element))
                 (pad (/ 6.0 excal--zoom))
                 (l (- x1 pad)) (tp (- y1 pad)) (r (+ x2 pad)) (b (+ y2 pad))
                 (mx (/ (+ l r) 2.0)) (my (/ (+ tp b) 2.0))
                 (all `((nw ,l . ,tp) (ne ,r . ,tp) (sw ,l . ,b) (se ,r . ,b)
                        (n ,mx . ,tp) (s ,mx . ,b) (w ,l . ,my) (e ,r . ,my))))
      (if (equal (excal--get element 'type) "text") (seq-take all 4) all))))

(defun excal--hit-handle (element scene-xy)
  "Return the name of ELEMENT's handle under SCENE-XY, or nil."
  (let ((radius (/ 8.0 excal--zoom)))
    (car (cl-find-if (lambda (handle)
                       (and (<= (abs (- (cadr handle) (car scene-xy))) radius)
                            (<= (abs (- (cddr handle) (cdr scene-xy))) radius)))
                     (excal--handles element)))))

(defun excal--geometry (element)
  "Snapshot ELEMENT's geometry so a resize can be computed from it."
  (list :bounds (excal--bounds element)
        :x (excal--get element 'x) :y (excal--get element 'y)
        :points (mapcar #'copy-sequence (excal--get element 'points))
        :font-size (excal--get element 'fontSize)))

(defun excal--resize (element handle geometry dx dy)
  "Resize ELEMENT by dragging HANDLE DX, DY scene units from GEOMETRY."
  (pcase-let* ((`(,ox1 ,oy1 ,ox2 ,oy2) (plist-get geometry :bounds))
               (x1 (if (memq handle '(nw w sw)) (+ ox1 dx) ox1))
               (x2 (if (memq handle '(ne e se)) (+ ox2 dx) ox2))
               (y1 (if (memq handle '(nw n ne)) (+ oy1 dy) oy1))
               (y2 (if (memq handle '(sw s se)) (+ oy2 dy) oy2))
               (ow (- ox2 ox1)) (oh (- oy2 oy1)))
    (pcase (excal--get element 'type)
      ("text"
       ;; Scale the font uniformly, anchored at the opposite corner.
       (let* ((scale (max (if (> ow 0) (/ (abs (- x2 x1)) ow) 0)
                          (if (> oh 0) (/ (abs (- y2 y1)) oh) 0)
                          0.05))
              (size (max 1.0 (* scale (plist-get geometry :font-size)))))
         (excal--put element 'fontSize size)
         (excal--measure-text element)
         (let ((w (excal--get element 'width)) (h (excal--get element 'height)))
           (excal--put element 'x (float (if (memq handle '(nw sw)) (- ox2 w) ox1)))
           (excal--put element 'y (float (if (memq handle '(nw ne)) (- oy2 h) oy1))))))
      ((or "line" "arrow" "freedraw")
       ;; Map every absolute point from the old bounds onto the new ones;
       ;; a negative scale mirrors the shape when dragged past the edge.
       (let* ((ox (plist-get geometry :x)) (oy (plist-get geometry :y))
              (map-x (lambda (ax) (if (> ow 0) (+ x1 (* (- ax ox1) (/ (- x2 x1) ow)))
                                    (+ ax (- x1 ox1)))))
              (map-y (lambda (ay) (if (> oh 0) (+ y1 (* (- ay oy1) (/ (- y2 y1) oh)))
                                    (+ ay (- y1 oy1)))))
              (points (plist-get geometry :points))
              (fx (funcall map-x (+ ox (aref (car points) 0))))
              (fy (funcall map-y (+ oy (aref (car points) 1)))))
         (excal--put element 'x (float fx))
         (excal--put element 'y (float fy))
         (excal--put element 'points
                     (vconcat (mapcar (lambda (p)
                                        (vector (- (funcall map-x (+ ox (aref p 0))) fx)
                                                (- (funcall map-y (+ oy (aref p 1))) fy)))
                                      points)))
         (excal--linear-extent element)))
      (_
       (excal--put element 'x (float (min x1 x2)))
       (excal--put element 'y (float (min y1 y2)))
       (excal--put element 'width (float (max 1 (abs (- x2 x1)))))
       (excal--put element 'height (float (max 1 (abs (- y2 y1)))))))
    (excal--touch element)))

;;;; Pointer shape

(defun excal--pointer-at (scene-xy)
  "Return the pointer shape for SCENE-XY given the current tool.
Emacs only offers a few portable shapes: there is no diagonal resize,
move or crosshair pointer, so corners use `hdrag' and elements `hand'."
  (pcase excal--tool
    ('select
     (let ((handle (and excal--selected
                        (excal--hit-handle excal--selected scene-xy))))
       (cond ((memq handle '(n s)) 'nhdrag)
             (handle 'hdrag)
             ((excal--hit scene-xy) 'hand)
             (t 'arrow))))
    ('text 'text)
    (_ 'arrow)))

(defun excal--set-pointer (pointer)
  "Show POINTER over the canvas.
The shape is a text property, so changing it touches neither the image
cache nor the canvas pixels."
  (unless (eq pointer excal--pointer)
    (setq excal--pointer pointer)
    (with-silent-modifications
      (put-text-property (point-min) (point-max) 'pointer pointer))))

(defun excal--update-pointer ()
  "Recompute the pointer shape at the current mouse position."
  (let* ((window (and (display-graphic-p) (get-buffer-window (current-buffer))))
         (pointer (and window (mouse-absolute-pixel-position)))
         (edges (and window (window-inside-absolute-pixel-edges window))))
    (when (and edges
               (<= (nth 0 edges) (car pointer) (1- (nth 2 edges)))
               (<= (nth 1 edges) (cdr pointer) (1- (nth 3 edges))))
      (excal--set-pointer
       (excal--pointer-at
        (cons (- (/ (float (- (car pointer) (nth 0 edges))) excal--zoom)
                 excal--scroll-x)
              (- (/ (float (- (cdr pointer) (nth 1 edges))) excal--zoom)
                 excal--scroll-y)))))))

(defun excal-mouse-move (event)
  "Update the pointer shape for mouse movement EVENT."
  (interactive "e")
  (let ((posn (event-start event)))
    (when (and (eq (posn-window posn) (get-buffer-window (current-buffer)))
               (null (posn-area posn)))
      (excal--set-pointer (excal--pointer-at (excal--event-scene-xy event))))))

;;;; Interaction

(defun excal--drag-loop (on-move)
  "Track the mouse, calling ON-MOVE with each movement event until release.
ON-MOVE returns the damage it caused: a device rectangle (X1 Y1 X2 Y2)
or `full'.  Damage from frames skipped under pending input accumulates."
  (let ((pending nil))
    (track-mouse
      (setq track-mouse 'dragging)
      (catch 'done
        (while t
          (let ((event (read--potential-mouse-event)))
            (cond
             ((mouse-movement-p event)
              (setq pending (excal--damage-union pending (funcall on-move event)))
              (unless (input-pending-p)
                (excal--render pending)
                (setq pending nil)
                (redisplay)))
             ((memq (event-basic-type event) '(mouse-1))
              (throw 'done event))
             (t
              (push event unread-command-events)
              (throw 'done nil)))))))))

(defun excal--normalize-box (element)
  "Make ELEMENT's width and height non-negative."
  (let ((w (excal--get element 'width)) (h (excal--get element 'height)))
    (when (< w 0)
      (excal--put element 'x (+ (excal--get element 'x) w))
      (excal--put element 'width (- w)))
    (when (< h 0)
      (excal--put element 'y (+ (excal--get element 'y) h))
      (excal--put element 'height (- h)))))

(defun excal--linear-extent (element)
  "Recompute ELEMENT's width and height from its points."
  (let ((points (excal--get element 'points)))
    (excal--put element 'width
                (float (- (seq-max (seq-map (lambda (p) (aref p 0)) points))
                          (seq-min (seq-map (lambda (p) (aref p 0)) points)))))
    (excal--put element 'height
                (float (- (seq-max (seq-map (lambda (p) (aref p 1)) points))
                          (seq-min (seq-map (lambda (p) (aref p 1)) points)))))))

(defun excal-mouse-down (event)
  "Start the current tool's drag at EVENT."
  (interactive "e")
  (let* ((start (excal--event-scene-xy event))
         (sx (car start)) (sy (cdr start)))
    (pcase excal--tool
      ((and 'select
            (guard excal--selected)
            (let handle (excal--hit-handle excal--selected start))
            (guard handle))
       (let ((element excal--selected)
             (geometry (excal--geometry excal--selected)))
         (excal--drag-loop
          (lambda (ev)
            (let ((p (excal--event-scene-xy ev)))
              (excal--with-damage element
                (excal--resize element handle geometry
                               (- (car p) sx) (- (cdr p) sy))))))))
      ('select
       (let ((hit (excal--hit start)))
         (setq excal--selected hit)
         (excal--render)
         (if hit
             (let ((ox (excal--get hit 'x)) (oy (excal--get hit 'y)))
               (excal--drag-loop
                (lambda (ev)
                  (let ((p (excal--event-scene-xy ev)))
                    (excal--with-damage hit
                      (excal--put hit 'x (+ ox (- (car p) sx)))
                      (excal--put hit 'y (+ oy (- (cdr p) sy)))
                      (excal--touch hit))))))
           (let ((last (excal--event-window-xy event)))
             (excal--drag-loop
              (lambda (ev)
                (let ((xy (excal--event-window-xy ev)))
                  (cl-incf excal--scroll-x (/ (- (car xy) (car last)) excal--zoom))
                  (cl-incf excal--scroll-y (/ (- (cdr xy) (cdr last)) excal--zoom))
                  (setq last xy)
                  'scroll)))))))
      ((and tool (or 'rectangle 'ellipse 'diamond))
       (let ((element (excal--make-element (symbol-name tool) sx sy)))
         (when (eq tool 'rectangle)
           (excal--put element 'roundness (list (cons 'type 3))))
         (setq excal--elements (append excal--elements (list element))
               excal--selected element)
         (excal--drag-loop
          (lambda (ev)
            (let ((p (excal--event-scene-xy ev)))
              (excal--with-damage element
                (excal--put element 'x (float (min sx (car p))))
                (excal--put element 'y (float (min sy (cdr p))))
                (excal--put element 'width (float (abs (- (car p) sx))))
                (excal--put element 'height (float (abs (- (cdr p) sy))))
                (excal--touch element)))))
         (setq excal--tool 'select)))
      ((and tool (or 'arrow 'line))
       (let ((element (excal--make-element
                       (symbol-name tool) sx sy
                       (cons 'points (vector [0.0 0.0] [0.0 0.0]))
                       (cons 'roundness (list (cons 'type 2)))
                       (cons 'startBinding :null) (cons 'endBinding :null)
                       (cons 'startArrowhead :null)
                       (cons 'endArrowhead (if (eq tool 'arrow) "arrow" :null)))))
         (setq excal--elements (append excal--elements (list element))
               excal--selected element)
         (excal--drag-loop
          (lambda (ev)
            (let ((p (excal--event-scene-xy ev)))
              (excal--with-damage element
                (aset (excal--get element 'points) 1
                      (vector (- (car p) sx) (- (cdr p) sy)))
                (excal--linear-extent element)
                (excal--touch element)))))
         (setq excal--tool 'select)))
      ('freedraw
       (let* ((points (list [0.0 0.0]))
              (element (excal--make-element
                        "freedraw" sx sy (cons 'points (vconcat points))
                        (cons 'pressures []) (cons 'simulatePressure t))))
         (setq excal--elements (append excal--elements (list element))
               excal--selected nil)
         (excal--drag-loop
          (lambda (ev)
            (let ((p (excal--event-scene-xy ev)))
              (excal--with-damage element
                (push (vector (- (car p) sx) (- (cdr p) sy)) points)
                (excal--put element 'points (vconcat (reverse points)))
                (excal--touch element)))))
         (excal--linear-extent element)))
      ('text
       (excal--await-release)
       (setq excal--tool 'select)
       (excal--insert-text sx sy)))
    (excal--render)
    (excal--update-pointer)))

(defun excal--await-release ()
  "Consume input until the mouse button that started this command is released.
Prompting from a `down-mouse-1' command before this would let the release
event reach the minibuffer."
  (catch 'done
    (while t
      (let ((event (read-event)))
        (cond
         ((and (eq (event-basic-type event) 'mouse-1)
               (not (memq 'down (event-modifiers event))))
          (throw 'done event))
         ((mouse-movement-p event))
         (t
          (push event unread-command-events)
          (throw 'done nil)))))))

(defun excal-double-click (event)
  "Edit the text under EVENT, or create a text element there."
  (interactive "e")
  (excal--await-release)
  (let* ((xy (excal--event-scene-xy event))
         (hit (excal--hit xy)))
    (if (equal (excal--get hit 'type) "text")
        (progn (setq excal--selected hit)
               (excal-edit-text))
      (setq excal--tool 'select)
      (excal--insert-text (car xy) (cdr xy)))
    (excal--render)
    (excal--update-pointer)))

(defun excal--measure-text (element)
  "Set ELEMENT's width and height from its text and font."
  (let ((size (excal-native-measure-text
               (or (excal--get element 'text) "")
               (or (excal--get element 'fontSize) 20)
               (or (excal--get element 'fontFamily) 5)
               (or (excal--get element 'lineHeight) 1.25))))
    (excal--put element 'width (car size))
    (excal--put element 'height (cdr size))))

(defun excal--set-text (element text)
  "Set ELEMENT's TEXT and resize it to fit."
  (excal--put element 'text text)
  (excal--put element 'originalText text)
  (excal--measure-text element)
  (excal--touch element))

(defvar-keymap excal-text-minibuffer-map
  :parent minibuffer-local-map
  :doc "Keymap for editing text elements in the minibuffer."
  "C-j" #'newline
  "S-<return>" #'newline)

(defun excal--edit-text-live (element)
  "Edit ELEMENT's text in the minibuffer, previewing every change.
Return the confirmed text, or nil when the edit was aborted; an abort
restores the original text."
  (let* ((buffer (current-buffer))
         (original (or (excal--get element 'text) ""))
         (preview (lambda (&rest _)
                    (let ((text (minibuffer-contents-no-properties)))
                      (with-current-buffer buffer
                        (excal--set-text element text)
                        (excal--render)))))
         (confirmed nil))
    (unwind-protect
        (setq confirmed
              (minibuffer-with-setup-hook
                  (lambda () (add-hook 'after-change-functions preview nil t))
                (read-from-minibuffer "Text (C-j newline, RET done): "
                                      original excal-text-minibuffer-map)))
      (with-current-buffer buffer
        (excal--set-text element (or confirmed original))))
    confirmed))

(defun excal--insert-text (x y)
  "Create a text element at scene X, Y and edit it in place."
  (let ((element (excal--make-element
                  "text" x y
                  (cons 'text "") (cons 'originalText "")
                  (cons 'fontSize 20) (cons 'fontFamily 5)
                  (cons 'textAlign "left") (cons 'verticalAlign "top")
                  (cons 'containerId :null) (cons 'autoResize t)
                  (cons 'lineHeight 1.25))))
    (setq excal--elements (append excal--elements (list element))
          excal--selected element)
    (excal--measure-text element)
    (excal--render)
    (let ((text (condition-case nil (excal--edit-text-live element) (quit nil))))
      (when (or (null text) (string-empty-p text))
        (setq excal--elements (delq element excal--elements)
              excal--selected nil)))
    (excal--render)))

(defun excal-edit-text ()
  "Edit the selected text element, previewing changes on the canvas."
  (interactive)
  (when (equal (excal--get excal--selected 'type) "text")
    (condition-case nil (excal--edit-text-live excal--selected) (quit nil))
    (excal--render)))

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

(defun excal-delete-selected ()
  "Delete the selected element."
  (interactive)
  (when excal--selected
    (excal--put excal--selected 'isDeleted t)
    (excal--touch excal--selected)
    (setq excal--selected nil)
    (excal--render)))

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

(defmacro excal--tool-command (tool)
  "Return a command selecting TOOL."
  `(lambda ()
     (interactive)
     (setq excal--tool ',tool)
     (excal--update-pointer)
     (message "Tool: %s" ',tool)))

(defvar-keymap excal-mode-map
  "<down-mouse-1>" #'excal-mouse-down
  "<double-down-mouse-1>" #'excal-double-click
  "<mouse-movement>" #'excal-mouse-move
  "<wheel-up>" #'excal-wheel "<wheel-down>" #'excal-wheel
  "<wheel-left>" #'excal-wheel "<wheel-right>" #'excal-wheel
  "C-<wheel-up>" #'excal-wheel "C-<wheel-down>" #'excal-wheel
  "<pinch>" #'excal-pinch
  "v" (excal--tool-command select)
  "r" (excal--tool-command rectangle)
  "o" (excal--tool-command ellipse)
  "d" (excal--tool-command diamond)
  "a" (excal--tool-command arrow)
  "l" (excal--tool-command line)
  "p" (excal--tool-command freedraw)
  "t" (excal--tool-command text)
  "e" #'excal-edit-text "RET" #'excal-edit-text
  "<delete>" #'excal-delete-selected "DEL" #'excal-delete-selected
  "=" #'excal-zoom-in "-" #'excal-zoom-out "0" #'excal-zoom-reset
  "H" #'excal-toggle-pixel-scale
  "b" #'excal-cycle-backend
  "g" #'excal--sync-canvas
  "C-x C-s" #'excal-save
  "B" #'excal-bench)

(define-derived-mode excal-mode special-mode "Excal"
  "Major mode for editing Excalidraw scenes on a Canvas image."
  (setq-local cursor-type nil
              ;; Report plain mouse motion so the pointer can follow the
              ;; scene; see `excal-mouse-move'.
              track-mouse t
              truncate-lines t
              line-spacing nil
              mode-line-process '(:eval (format " %s %s %d%%" excal--backend
                                                excal--tool
                                                (round (* 100 excal--zoom)))))
  (setq excal--native-cache (make-hash-table :test #'eq :weakness 'key)
        excal--pixel-scale (excal--guess-pixel-scale)
        excal--backend (if (and (eq excal-backend 'layer)
                                (not (fboundp 'excal-native-layer-create)))
                           'tiles
                         excal-backend))
  (add-hook 'kill-buffer-hook #'excal--hide-layer nil t)
  (add-hook 'window-size-change-functions #'excal--window-size-change)
  (add-hook 'window-buffer-change-functions #'excal--window-size-change))

(defun excal--open (doc file name)
  "Show DOC saved to FILE in a buffer called NAME."
  (unless (and (display-graphic-p) (image-type-available-p 'canvas))
    (error "excal needs a graphical Emacs with Canvas images"))
  (let ((buffer (generate-new-buffer name)))
    (pop-to-buffer-same-window buffer)
    (excal-mode)
    (setq excal--file file
          excal--doc doc
          excal--elements (append (alist-get 'elements doc) nil))
    (excal--sync-canvas (selected-window))
    buffer))

;;;###autoload
(defun excal-open (file)
  "Open .excalidraw FILE."
  (interactive "fExcalidraw file: ")
  (let ((file (expand-file-name file)))
    (excal--open (excal--read-file file) file
                 (format "*excal %s*" (file-name-nondirectory file)))))

;;;###autoload
(defun excal-new ()
  "Start an empty scene."
  (interactive)
  (excal--open (excal--empty-doc) nil "*excal new*"))

;;;; Benchmark

(defun excal--stress-elements (count)
  "Return COUNT random elements for benchmarking."
  (cl-loop for i below count
           collect
           (let ((type (nth (% i 5) '("rectangle" "ellipse" "diamond" "arrow" "text")))
                 (x (float (random 2000))) (y (float (random 1400))))
             (pcase type
               ("arrow" (excal--make-element
                         type x y (cons 'points (vector [0 0] [80 30] [160 -20]))
                         (cons 'roundness (list (cons 'type 2)))))
               ("text" (excal--make-element
                        type x y (cons 'text "手绘 Excalidraw")
                        (cons 'fontSize 20) (cons 'fontFamily 5)
                        (cons 'width 150.0) (cons 'height 25.0)))
               (_ (excal--make-element
                   type x y (cons 'width 120.0) (cons 'height 80.0)
                   (cons 'backgroundColor "#a5d8ff")
                   (cons 'fillStyle (if (cl-evenp i) "hachure" "solid"))))))))

(defun excal--bench-frame (damage)
  "Render one benchmark frame with DAMAGE and push it to the screen."
  (excal--render damage)
  (redisplay t)
  (when (eq excal--backend 'layer)
    (excal-native-layer-flush)))

(defun excal--bench-run (frames step)
  "Time FRAMES frames produced by calling STEP with the frame index.
STEP returns the frame's damage."
  (let ((render 0.0) (present 0.0) (refreshed 0)
        (start (float-time)))
    (dotimes (i frames)
      (excal--bench-frame (funcall step i))
      (cl-incf render (plist-get excal--last-stats :render-ms))
      (cl-incf present (plist-get excal--last-stats :present-ms))
      (cl-incf refreshed (plist-get excal--last-stats :refreshed)))
    (let ((total (- (float-time) start)))
      (list :render-ms (/ render frames)
            :present-ms (/ present frames)
            :frame-ms (/ (* 1000 total) frames)
            :fps (/ frames total)
            :surfaces (/ (float refreshed) frames)))))

(defun excal-bench (&optional frames)
  "Measure panning and dragging over FRAMES frames with the current backend."
  (interactive)
  (let* ((frames (or frames 60))
         (target (or excal--selected
                     (cl-find-if (lambda (e) (not (excal--get e 'isDeleted)))
                                 excal--elements)))
         (step (lambda (i) (if (< i (/ frames 2)) 1 -1)))
         (pan-full (excal--bench-run
                    frames
                    (lambda (i)
                      (cl-incf excal--scroll-x (* 3.0 (funcall step i)))
                      (cl-incf excal--scroll-y (* 2.0 (funcall step i)))
                      'full)))
         (pan (excal--bench-run
               frames
               (lambda (i)
                 (cl-incf excal--scroll-x (* 3.0 (funcall step i)))
                 (cl-incf excal--scroll-y (* 2.0 (funcall step i)))
                 'scroll)))
         (drag (and target
                    (excal--bench-run
                     frames
                     (lambda (i)
                       (excal--with-damage target
                         (excal--put target 'x (+ (excal--get target 'x)
                                                  (if (< i (/ frames 2)) 3.0 -3.0)))
                         (excal--touch target))))))
         (result (list :backend excal--backend
                       :elements (length excal--elements)
                       :canvas excal--canvas-size
                       :pixel-scale excal--pixel-scale
                       :pan-full pan-full :pan pan :drag drag)))
    (message "excal-bench: %S" result)
    result))

(defun excal-bench-backends (&optional frames show)
  "Benchmark every backend on this scene and on a 500-element stress scene.
Results go to the *excal-bench* buffer, which is displayed when SHOW is
non-nil (as it is interactively), and are returned as a list.  Showing it
from a script would split the window and change the measured canvas."
  (interactive (list nil t))
  (let* ((frames (or frames 60))
         (backends (if (fboundp 'excal-native-layer-create)
                       '(canvas tiles layer)
                     '(canvas tiles)))
         (original excal--backend)
         (elements excal--elements)
         (results nil))
    (unwind-protect
        (dolist (scene '(sample stress))
          (when (eq scene 'stress)
            (setq excal--elements (append elements (excal--stress-elements 500))))
          (dolist (backend backends)
            (excal--use-backend backend)
            (redisplay t)
            (push (cons scene (excal-bench frames)) results)))
      (setq excal--elements elements)
      (excal--use-backend original))
    (setq results (nreverse results))
    ;; Read these before leaving the excal buffer: they are buffer-local.
    (let ((size excal--canvas-size) (scale excal--pixel-scale))
     (with-current-buffer (get-buffer-create "*excal-bench*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "canvas %S, pixel scale %.1f, %d frames\n\n"
                        size scale frames))
        (insert (format "%-7s %-7s %-8s %9s %10s %9s %6s %9s\n"
                        "scene" "backend" "op" "render" "present" "frame" "fps"
                        "surfaces"))
        (dolist (r results)
          (dolist (op '(:pan-full :pan :drag))
            (when-let* ((m (plist-get (cdr r) op)))
              (insert (format "%-7s %-7s %-8s %7.2fms %8.2fms %7.2fms %6.1f %9.1f\n"
                              (car r) (plist-get (cdr r) :backend)
                              (substring (symbol-name op) 1)
                              (plist-get m :render-ms) (plist-get m :present-ms)
                              (plist-get m :frame-ms) (plist-get m :fps)
                              (plist-get m :surfaces)))))))
      (special-mode)
      (when show (display-buffer (current-buffer)))))
    results))

(defun excal-bench-elisp-fill ()
  "Measure filling the whole canvas pixel by pixel from Elisp."
  (let* ((w (car excal--canvas-size)) (h (cdr excal--canvas-size))
         (data (make-vector (* w h) #xFFFFFFFF))
         (canvas (list 'image :type 'canvas :id (gensym "excal-elisp-")
                       :data-width w :data-height h :data data))
         (start (float-time)))
    (dotimes (i (* w h)) (aset data i (logior #xFF000000 (logand i #xFFFFFF))))
    (let ((fill (- (float-time) start)))
      (setq start (float-time))
      (canvas-refresh canvas 'reload-data)
      (list :elisp-fill-ms (* 1000 fill)
            :reload-ms (* 1000 (- (float-time) start))))))

(provide 'excal)
;;; excal.el ends here
