;;; excali-cursor.el --- Pointer shapes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Which pointer shape the canvas shows, following upstream's cursor.ts
;; (`setCursorForShape', `getCursorForResizingElement') and the hover
;; logic of App.tsx `handleCanvasPointerMove'.  Shapes are named like the
;; CSS cursors upstream sets: `default', `pointer', `move', `grab',
;; `grabbing', `crosshair', `text', `ns-resize', `ew-resize',
;; `nwse-resize', `nesw-resize', `not-allowed', and the custom `eraser'
;; (`eraser-dark' in the dark theme).
;;
;; Emacs shows them through the canvas image's `:map', as video.el
;; does: every command rebuilds a list of hot spots (handles, link icons,
;; line points, elements and the selection box, each with the nearest
;; shape Emacs' `pointer' offers), and Emacs itself switches the pointer
;; as the mouse crosses them, with no Lisp round trip.  Hot spots carry
;; no id, so clicks on them stay ordinary text-area events.  The map is
;; changed in place and put first in the image spec, where it does not
;; enter the image cache's hash, so updating it never re-creates the
;; canvas image.
;;
;; `excali-native-cursors' (off by default) instead lets the module show
;; every upstream shape on macOS (src/excali-cursor.m) through a view
;; over the canvas.

;;; Code:

(require 'excali-core)
(require 'excali-select)
(require 'excali-handles)
(require 'excali-hit)

(declare-function excali--in-selection-box-p "excali-edit")
(declare-function excali--link-at "excali-erase")
(declare-function excali--linear-target "excali-linear")
(declare-function excali--point-at "excali-linear")
(declare-function excali--midpoint-at "excali-linear")
(declare-function excali--elbow-p "excali-elbow")
(declare-function excali--elbow-end-at "excali-elbow")
(declare-function excali--elbow-midpoint-at "excali-elbow")
(declare-function excali-native-cursor-view-create "excali-module")
(declare-function excali-native-cursor-view-set-geometry "excali-module")
(declare-function excali-native-cursor-set "excali-module")
(declare-function excali--link-icon-box "excali-erase")
(declare-function excali--linear-scene-points "excali-linear")
(declare-function excali--segment-midpoints "excali-linear")
(declare-function excali--elbow-scene-points "excali-elbow")
(declare-function excali--elbow-midpoints "excali-elbow")
(declare-function excali--frame-p "excali-frame")
(declare-function excali--frame-name-bounds "excali-frame-render")
(defvar excali--canvas-size)
(defvar excali--pixel-scale)
(defvar excali--point-hit-size)
(defvar excali--multi-element)
(defvar excali--editing-linear)

(defcustom excali-native-cursors nil
  "Non-nil means let the module show Excalidraw's own pointer shapes.
Experimental and macOS only: a module view over the canvas owns the
pointer there.  Otherwise the canvas image's hot spots show the nearest
shapes Emacs offers."
  :type 'boolean
  :group 'excali)

(defvar-local excali--cursor nil "Pointer shape last chosen for the canvas.")
(defvar-local excali--cursor-view nil "Module cursor view over the canvas, or nil.")
(defvar-local excali--cursor-view-shown nil "Non-nil while the cursor view is shown.")
(defvar-local excali--pointer-surfaces nil
  "Images carrying the pointer map, as (SPEC X . Y).
X and Y are the image's offset in the window, in logical pixels.")
(defvar-local excali--pointer-stamp nil "What the pointer map was built for.")
(defvar-local excali--pointer-timer nil "Idle timer that will rebuild the pointer map.")

(defcustom excali-pointer-map-delay 0.1
  "Idle seconds after a command before the pointer map is rebuilt.
Commands in quick succession, such as the steps of a pan, rebuild it
once when they stop."
  :type 'number
  :group 'excali)

;;;; Choosing

(defconst excali--resize-cursors ["ns" "nesw" "ew" "nwse"]
  "RESIZE_CURSORS, in the order rotation steps through them.")

(defun excali--resize-cursor (handle element)
  "Return the cursor for transform HANDLE of ELEMENT.
ELEMENT is nil for the common box of several elements.  Port of
`getCursorForResizingElement': mirrored elements swap the diagonals, and
rotation turns the cursor in 45 degree steps."
  (if (eq handle 'rotation)
      'grab
    (let* ((swap (and element
                      (< (* (cl-signum (or (excali--get element 'width) 0))
                            (cl-signum (or (excali--get element 'height) 0)))
                         0)))
           (base (pcase handle
                   ((or 'n 's) "ns")
                   ((or 'e 'w) "ew")
                   ((or 'nw 'se) (if swap "nesw" "nwse"))
                   (_ (if swap "nwse" "nesw"))))
           (steps (if element (round (/ (excali--element-angle element) (/ float-pi 4))) 0))
           (index (mod (+ (cl-position base excali--resize-cursors :test #'equal) steps)
                       (length excali--resize-cursors))))
      (intern (concat (aref excali--resize-cursors index) "-resize")))))

(defun excali--select-cursor-at (scene-xy)
  "Return the selection tool's cursor at SCENE-XY.
The checks follow `excali-mouse-down', so the cursor tells what a press
there would do."
  (let* ((single (excali--single-selection))
         (linear (and (fboundp 'excali--linear-target) (excali--linear-target)))
         (handle (excali--handle-at scene-xy)))
    (cond
     ((and (fboundp 'excali--link-at) (excali--link-at scene-xy)) 'pointer)
     ((and single (fboundp 'excali--elbow-p) (excali--elbow-p single)
           (or (excali--elbow-midpoint-at single scene-xy)
               (excali--elbow-end-at single scene-xy)))
      'pointer)
     ((and linear (not (and (fboundp 'excali--elbow-p) (excali--elbow-p linear)))
           (or (excali--point-at linear scene-xy)
               (excali--midpoint-at linear scene-xy)))
      'pointer)
     (handle
      (excali--resize-cursor handle (and single (not (cdr excali--selection)) single)))
     ((or (excali--hit scene-xy)
          (and (fboundp 'excali--in-selection-box-p)
               (excali--in-selection-box-p scene-xy)))
      'move)
     (t 'default))))

(defun excali--cursor-at (scene-xy)
  "Return the pointer shape for SCENE-XY given the current tool.
`setCursorForShape' plus the hover rules of `handleCanvasPointerMove'."
  (cond
   ((bound-and-true-p excali--multi-element) 'crosshair)
   (t
    (pcase excali--tool
      ('select (excali--select-cursor-at scene-xy))
      ('hand 'grab)
      ('eraser (if (eq excali--theme 'dark) 'eraser-dark 'eraser))
      ('text (if (equal (excali--get (excali--hit scene-xy) 'type) "text") 'text 'crosshair))
      (_ 'crosshair)))))

;;;; Showing

(defconst excali--cursor-fallbacks
  '((default . arrow) (auto . arrow) (pointer . hand) (move . hand)
    (grab . hand) (grabbing . hand) (text . text) (crosshair . arrow)
    (ns-resize . nhdrag) (ew-resize . hdrag) (nwse-resize . hdrag)
    (nesw-resize . hdrag) (not-allowed . arrow) (eraser . arrow)
    (eraser-dark . arrow))
  "The `pointer' shape standing in for each cursor.
Emacs offers no diagonal resize, move, crosshair or rotate pointer.")

(defun excali--emacs-pointer (cursor)
  "Return the `pointer' shape nearest CURSOR."
  (alist-get cursor excali--cursor-fallbacks 'arrow))

;;;;; Hot spot geometry, in window pixels

(defun excali--window-xy (point)
  "Return scene POINT in whole window pixels."
  (cons (round (* (+ (car point) excali--scroll-x) excali--zoom))
        (round (* (+ (cdr point) excali--scroll-y) excali--zoom))))

(defun excali--hot-spot (area cursor)
  "Return a map entry showing CURSOR over AREA.
The id is nil so that clicks there stay plain text-area events."
  (list area nil (list 'pointer (excali--emacs-pointer cursor))))

(defun excali--hot-rect (x1 y1 x2 y2)
  "Return the area of the scene rectangle X1 Y1 X2 Y2."
  (cons 'rect (cons (excali--window-xy (cons x1 y1)) (excali--window-xy (cons x2 y2)))))

(defun excali--hot-circle (center radius)
  "Return the area of a circle at scene CENTER with RADIUS screen px."
  (cons 'circle (cons (excali--window-xy center) (float radius))))

(defun excali--hot-poly (points)
  "Return the area of the scene polygon POINTS."
  (cons 'poly (vconcat (mapcan (lambda (p) (let ((w (excali--window-xy p)))
                                             (list (car w) (cdr w))))
                               points))))

(defun excali--segment-quad (a b h &optional flat)
  "Return the corners of segment A-B grown by H on every side.
With FLAT, the ends are not extended."
  (let* ((dx (- (car b) (car a))) (dy (- (cdr b) (cdr a)))
         (len (sqrt (+ (* dx dx) (* dy dy))))
         (ux (if (zerop len) 1.0 (/ dx len))) (uy (if (zerop len) 0.0 (/ dy len)))
         (nx (* (- uy) h)) (ny (* ux h))
         (ex (if flat 0.0 (* ux h))) (ey (if flat 0.0 (* uy h))))
    (list (cons (+ (car a) (- ex) nx) (+ (cdr a) (- ey) ny))
          (cons (+ (car b) ex nx) (+ (cdr b) ey ny))
          (cons (- (+ (car b) ex) nx) (- (+ (cdr b) ey) ny))
          (cons (- (- (car a) ex) nx) (- (- (cdr a) ey) ny)))))

(defun excali--thin-polyline (points)
  "Return POINTS without those closer than 6 screen px to the last kept."
  (let ((min (/ 6.0 excali--zoom)) kept)
    (dolist (p points)
      (when (or (null kept) (null (cdr points))
                (>= (sqrt (+ (expt (- (car p) (caar kept)) 2)
                             (expt (- (cdr p) (cdar kept)) 2)))
                    min))
        (push p kept)))
    (unless (equal (car kept) (car (last points)))
      (push (car (last points)) kept))
    (nreverse kept)))

(defun excali--band-areas (points closed h)
  "Return areas covering the points within H of the polyline POINTS.
With CLOSED, the last point joins the first.  Each segment is a quad
and each corner a circle, so together they are exactly that band."
  (let ((segments (cl-loop for (a b) on (if closed (append points (list (car points))) points)
                           while b collect (cons a b))))
    (append (mapcar (lambda (s) (excali--hot-poly (excali--segment-quad (car s) (cdr s) h t)))
                    segments)
            (mapcar (lambda (p) (excali--hot-circle p (* h excali--zoom))) points))))

(defun excali--element-hot-areas (element)
  "Return map areas where a press hits ELEMENT.
They follow `excali--hit-element-p': filled shapes, text and images
anywhere inside, and every element within its hit distance of its
outline or path."
  (let* ((type (excali--get element 'type))
         (pad (+ (excali--hit-threshold element)
                 (if (equal type "freedraw") (* 2 (or (excali--get element 'strokeWidth) 1)) 0)))
         (box (excali--element-box element))
         (center (excali--box-center box))
         (angle (excali--element-angle element))
         (turn (lambda (points)
                 (if (zerop angle) points
                   (mapcar (lambda (p) (excali--rotate-point p center angle)) points))))
         (outline (excali--outline element))
         (closed (car outline))
         (points (excali--thin-polyline (cdr outline))))
    (cond
     ((member type '("text" "image"))
      (pcase-let ((`(,x1 ,y1 ,x2 ,y2) box))
        (list (excali--hot-poly
               (funcall turn (list (cons (- x1 pad) (- y1 pad)) (cons (+ x2 pad) (- y1 pad))
                                   (cons (+ x2 pad) (+ y2 pad)) (cons (- x1 pad) (+ y2 pad))))))))
     (t
      (append (and (excali--test-inside-p element) (cddr points)
                   (list (excali--hot-poly (funcall turn points))))
              (excali--band-areas (funcall turn points) closed pad))))))

;;;;; The map

(defun excali--visible-box ()
  "Return the scene box the window shows, or nil without a canvas."
  (when excali--canvas-size
    (let ((w (/ (car excali--canvas-size) excali--pixel-scale excali--zoom))
          (h (/ (cdr excali--canvas-size) excali--pixel-scale excali--zoom)))
      (list (- excali--scroll-x) (- excali--scroll-y)
            (- w excali--scroll-x) (- h excali--scroll-y)))))

(defun excali--roughly-visible-p (element view)
  "Return non-nil if ELEMENT may show in the scene box VIEW."
  (or (null view)
      (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) (excali--element-box element))
                   (m (+ (max (- x2 x1) (- y2 y1)) 20)))
        (and (< (- x1 m) (nth 2 view)) (> (+ x2 m) (nth 0 view))
             (< (- y1 m) (nth 3 view)) (> (+ y2 m) (nth 1 view))))))

(defun excali--handle-hot-spots ()
  "Return hot spots for the transform handles.
They come in the order `excali--handle-at' checks."
  (when-let* ((target (excali--transform-target)))
    (let* ((single (excali--single-selection))
           (owner (and single (not (cdr excali--selection)) single))
           (box (plist-get target :box))
           (angle (plist-get target :angle))
           (handles (excali--transform-handles box angle (plist-get target :margin)
                                               (plist-get target :spacing)
                                               (not (plist-get target :rotation))))
           (square (lambda (h)
                     (pcase-let ((`(,name ,x ,y ,w ,hh) h))
                       (excali--hot-spot (excali--hot-rect x y (+ x w) (+ y hh))
                                         (excali--resize-cursor name owner))))))
      (append
       (mapcar square (seq-filter (lambda (h) (eq (car h) 'rotation)) handles))
       (mapcar square (seq-remove (lambda (h) (eq (car h) 'rotation)) handles))
       (when (plist-get target :sides)
         (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) box)
                      (s (/ (float (if (eql (plist-get target :spacing) 0) 0
                                     excali--side-threshold))
                            excali--zoom))
                      (c (excali--box-center box))
                      (corner (lambda (x y) (excali--rotate-point (cons x y) c angle)))
                      (tl (funcall corner (- x1 s) (- y1 s)))
                      (tr (funcall corner (+ x2 s) (- y1 s)))
                      (br (funcall corner (+ x2 s) (+ y2 s)))
                      (bl (funcall corner (- x1 s) (+ y2 s)))
                      (h (/ (float excali--side-threshold) excali--zoom)))
           (cl-loop for (name a b) in `((n ,tl ,tr) (e ,tr ,br) (s ,br ,bl) (w ,bl ,tl))
                    collect (excali--hot-spot
                             (excali--hot-poly (excali--segment-quad a b h))
                             (excali--resize-cursor name owner)))))))))

(defun excali--select-hot-spots ()
  "Return the selection tool's hot spots.
They come in the order `excali--select-cursor-at' checks."
  (let* ((single (excali--single-selection))
         (linear (excali--linear-target))
         (view (excali--visible-box))
         (elements (seq-filter (lambda (e) (and (not (excali--get e 'locked))
                                                (excali--roughly-visible-p e view)))
                               (reverse (excali--live-elements)))))
    (append
     ;; Link icons of unselected elements.
     (let ((tolerance (/ 4.0 excali--zoom)))
       (cl-loop for e in elements
                when (and (stringp (excali--get e 'link)) (not (excali--selected-p e)))
                collect (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excali--link-icon-box e)))
                          (excali--hot-spot
                           (excali--hot-rect (- x1 tolerance) (- y1 tolerance)
                                             (+ x2 tolerance) (+ y2 tolerance))
                           'pointer))))
     ;; Elbow arrow ends and segment midpoints.
     (when (and single (excali--elbow-p single))
       (let ((points (excali--elbow-scene-points single)))
         (mapcar (lambda (p) (excali--hot-spot (excali--hot-circle p 11) 'pointer))
                 (append (list (car points) (car (last points)))
                         (mapcar #'cdr (excali--elbow-midpoints single))))))
     ;; Points and midpoints of the line or arrow being edited.
     (when (and linear (not (excali--elbow-p linear)))
       (mapcar (lambda (p) (excali--hot-spot (excali--hot-circle p excali--point-hit-size)
                                             'pointer))
               (append (excali--linear-scene-points linear)
                       (mapcar #'cdr (excali--segment-midpoints
                                      linear (eq linear excali--editing-linear))))))
     (excali--handle-hot-spots)
     ;; Frame names, elements and the selection box: a press drags.
     (cl-loop for e in elements
              when (and (excali--frame-p e) (fboundp 'excali--frame-name-bounds))
              collect (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excali--frame-name-bounds e)))
                        (excali--hot-spot (excali--hot-rect x1 y1 x2 y2) 'move)))
     (cl-loop for e in elements
              append (mapcar (lambda (area) (excali--hot-spot area 'move))
                             (excali--element-hot-areas e)))
     (when-let* ((bounds (excali--selection-bounds)))
       (pcase-let ((`(,x1 ,y1 ,x2 ,y2) bounds)
                   (pad (/ 6.0 excali--zoom)))
         (list (excali--hot-spot (excali--hot-rect (- x1 pad) (- y1 pad) (+ x2 pad) (+ y2 pad))
                                 'move)))))))

(defconst excali--whole-canvas '(rect . ((0 . 0) . (100000 . 100000)))
  "An area covering any canvas.")

(defun excali--pointer-map ()
  "Return the image map giving the canvas its pointer shapes.
The last entry covers everything with the current tool's shape."
  (let ((default (lambda (cursor) (list (excali--hot-spot excali--whole-canvas cursor)))))
    (cond
     (excali--cursor-view-shown (funcall default 'default))
     ((bound-and-true-p excali--multi-element) (funcall default 'crosshair))
     (t
      (pcase excali--tool
        ('select (append (excali--select-hot-spots) (funcall default 'default)))
        ('text
         (append (cl-loop for e in (reverse (excali--live-elements))
                          when (equal (excali--get e 'type) "text")
                          append (mapcar (lambda (area) (excali--hot-spot area 'text))
                                         (excali--element-hot-areas e)))
                 (funcall default 'crosshair)))
        (_ (funcall default (excali--cursor-at '(0.0 . 0.0)))))))))

(defun excali--translate-area (area dx dy)
  "Return AREA moved by DX, DY pixels."
  (let ((move (lambda (p) (cons (+ (car p) dx) (+ (cdr p) dy)))))
    (pcase (car area)
      ('rect (cons 'rect (cons (funcall move (cadr area)) (funcall move (cddr area)))))
      ('circle (cons 'circle (cons (funcall move (cadr area)) (cddr area))))
      ('poly (cons 'poly (vconcat (cl-loop for v across (cdr area) for i from 0
                                           collect (+ v (if (cl-evenp i) dx dy))))))
      (_ area))))

(defun excali--pointer-stamp ()
  "Return what the pointer map depends on."
  (let ((h 0))
    (dolist (e excali--elements)
      (setq h (logand (+ (* h 31) (or (excali--get e 'versionNonce) 0)) most-positive-fixnum)))
    (list h (length excali--elements) excali--tool excali--zoom
          excali--scroll-x excali--scroll-y
          (mapcar (lambda (e) (excali--get e 'id)) excali--selection)
          (and excali--editing-linear (excali--get excali--editing-linear 'id))
          (and (bound-and-true-p excali--multi-element) t)
          excali--cursor-view-shown excali--theme excali--pointer-surfaces)))

(defun excali--schedule-pointer-update ()
  "Rebuild the pointer map once Emacs has been idle a moment."
  (unless excali--pointer-timer
    (let ((buffer (current-buffer)))
      (setq excali--pointer-timer
            (run-with-idle-timer
             excali-pointer-map-delay nil
             (lambda ()
               (when (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (setq excali--pointer-timer nil)
                   (excali--update-pointer)))))))))

(defun excali--update-pointer (&optional force)
  "Rebuild the canvas pointer map if the scene or view changed.
With FORCE, rebuild it anyway.  The map is set in place in each image
spec, so Emacs picks it up at the next mouse motion without a redisplay."
  (when excali--pointer-surfaces
    (let ((stamp (excali--pointer-stamp)))
      (when (or force (not (equal stamp excali--pointer-stamp)))
        (setq excali--pointer-stamp stamp)
        (let ((map (excali--pointer-map)))
          (dolist (surface excali--pointer-surfaces)
            (pcase-let ((`(,spec ,x . ,y) surface))
              (plist-put (cdr spec) :map
                         (if (and (zerop x) (zerop y))
                             map
                           (mapcar (lambda (entry)
                                     (cons (excali--translate-area (car entry) (- x) (- y))
                                           (cdr entry)))
                                   map))))))))))

;;;;; The module's own cursors (`excali-native-cursors')

(defun excali--native-cursor-p (&optional frame)
  "Return non-nil if the module should show cursors on FRAME."
  (and excali-native-cursors
       (fboundp 'excali-native-cursor-view-create)
       (eq (framep (or frame (selected-frame))) 'ns)))

(defun excali--sync-cursor-view (window width height)
  "Place the cursor view over WINDOW's WIDTH by HEIGHT body, if enabled."
  (if (not (excali--native-cursor-p (window-frame window)))
      (excali--hide-cursor-view)
    (unless excali--cursor-view
      (pcase-let ((`(,left ,top ,right ,bottom)
                   (frame-edges (window-frame window) 'native-edges)))
        (setq excali--cursor-view (excali-native-cursor-view-create
                                  left top (- right left) (- bottom top)))))
    (when excali--cursor-view
      (pcase-let ((`(,x ,y . ,_) (window-inside-pixel-edges window)))
        (excali-native-cursor-view-set-geometry excali--cursor-view x y width height t))
      (setq excali--cursor-view-shown t))))

(defun excali--hide-cursor-view ()
  "Hide this buffer's cursor view, if any, handing the pointer to Emacs."
  (when excali--cursor-view
    (excali-native-cursor-view-set-geometry excali--cursor-view 0 0 1 1 nil))
  (setq excali--cursor-view-shown nil))

(defun excali--set-pointer (cursor)
  "Record CURSOR as the canvas pointer shape and show it through the module.
Without the module view, the pointer map shows it already."
  (setq excali--cursor cursor)
  (when excali--cursor-view-shown
    (excali-native-cursor-set excali--cursor-view (symbol-name cursor))))

(provide 'excali-cursor)
;;; excali-cursor.el ends here
