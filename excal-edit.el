;;; excal-edit.el --- Tools, transforms and editing commands  -*- lexical-binding: t; -*-

;;; Commentary:

;; Hit testing, resize handles, pointer shapes, the drawing tools, moving
;; and resizing the selection, text editing, grouping, z-order and
;; deletion.  The selection model itself lives in excal-select.el.

;;; Code:

(require 'excal-core)
(require 'excal-view)
(require 'excal-select)

(defcustom excal-nudge-step 1
  "Scene units moved by the arrow keys."
  :type 'number
  :group 'excal)

(defcustom excal-nudge-large-step 5
  "Scene units moved by the arrow keys with shift."
  :type 'number
  :group 'excal)

;;;; Hit testing

(defun excal--hit (scene-xy)
  "Return the topmost live element under SCENE-XY."
  (let ((tolerance (/ 8.0 excal--zoom)))
    (cl-find-if (lambda (element)
                  (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excal--bounds element)))
                    (and (<= (- x1 tolerance) (car scene-xy) (+ x2 tolerance))
                         (<= (- y1 tolerance) (cdr scene-xy) (+ y2 tolerance)))))
                (reverse (excal--live-elements)))))

(defun excal--rotated-p (element)
  "Return non-nil if ELEMENT is rotated."
  (let ((angle (excal--get element 'angle)))
    (and (numberp angle) (/= angle 0))))

(defun excal--box-handles (bounds &optional corners-only)
  "Return resize handles around BOUNDS as ((NAME . (X . Y)) ...).
With CORNERS-ONLY, return just the four corners.  Keep the layout in sync
with `draw_selection' in excal-render.c."
  (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) bounds)
               (pad (/ 6.0 excal--zoom))
               (l (- x1 pad)) (tp (- y1 pad)) (r (+ x2 pad)) (b (+ y2 pad))
               (mx (/ (+ l r) 2.0)) (my (/ (+ tp b) 2.0))
               (all `((nw ,l . ,tp) (ne ,r . ,tp) (sw ,l . ,b) (se ,r . ,b)
                      (n ,mx . ,tp) (s ,mx . ,b) (w ,l . ,my) (e ,r . ,my))))
    (if corners-only (seq-take all 4) all)))

(defun excal--handles (element)
  "Return ELEMENT's resize handles, or nil if it cannot be resized."
  (unless (excal--rotated-p element)
    (excal--box-handles (excal--bounds element)
                        (equal (excal--get element 'type) "text"))))

(defun excal--selection-handles ()
  "Return the resize handles of the selection."
  (if-let* ((single (excal--single-selection)))
      (excal--handles single)
    (when (and excal--selection
               (not (seq-some #'excal--rotated-p excal--selection)))
      (excal--box-handles (excal--selection-bounds)))))

(defun excal--hit-handle (scene-xy)
  "Return the name of the selection handle under SCENE-XY, or nil."
  (let ((radius (/ 8.0 excal--zoom)))
    (car (cl-find-if (lambda (handle)
                       (and (<= (abs (- (cadr handle) (car scene-xy))) radius)
                            (<= (abs (- (cddr handle) (cdr scene-xy))) radius)))
                     (excal--selection-handles)))))

;;;; Damage helpers

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

;;;; Resizing

(defun excal--geometry (element)
  "Snapshot ELEMENT's geometry so a resize can be computed from it."
  (list :bounds (excal--bounds element)
        :x (excal--get element 'x) :y (excal--get element 'y)
        :points (mapcar #'copy-sequence (excal--get element 'points))
        :font-size (excal--get element 'fontSize)))

(defun excal--dragged-bounds (bounds handle dx dy)
  "Return BOUNDS with the edges grabbed by HANDLE moved by DX, DY."
  (pcase-let ((`(,x1 ,y1 ,x2 ,y2) bounds))
    (list (if (memq handle '(nw w sw)) (+ x1 dx) x1)
          (if (memq handle '(nw n ne)) (+ y1 dy) y1)
          (if (memq handle '(ne e se)) (+ x2 dx) x2)
          (if (memq handle '(sw s se)) (+ y2 dy) y2))))

(defun excal--map-geometry (element geometry from to)
  "Place ELEMENT, snapshotted as GEOMETRY, by mapping rectangle FROM onto TO.
TO may be inverted, which mirrors point-based elements."
  (pcase-let* ((`(,fx1 ,fy1 ,fx2 ,fy2) from)
               (`(,tx1 ,ty1 ,tx2 ,ty2) to)
               (fw (- fx2 fx1)) (fh (- fy2 fy1))
               (map-x (lambda (x) (if (> fw 0) (+ tx1 (* (- x fx1) (/ (- tx2 tx1) fw)))
                                    (+ x (- tx1 fx1)))))
               (map-y (lambda (y) (if (> fh 0) (+ ty1 (* (- y fy1) (/ (- ty2 ty1) fh)))
                                    (+ y (- ty1 fy1)))))
               (`(,ex1 ,ey1 ,ex2 ,ey2) (plist-get geometry :bounds)))
    (pcase (excal--get element 'type)
      ("text"
       ;; Text scales its font uniformly; its top-left corner follows the map.
       ;; An unchanged axis has scale 1, so dragging one edge still works.
       (let* ((sx (if (> fw 0) (abs (/ (- tx2 tx1) fw)) 1.0))
              (sy (if (> fh 0) (abs (/ (- ty2 ty1) fh)) 1.0))
              ;; Follow whichever axis is dragged furthest.
              (scale (max 0.05 sx sy)))
         (excal--put element 'fontSize
                     (max 1.0 (* scale (plist-get geometry :font-size))))
         (excal--measure-text element)
         (excal--put element 'x (float (min (funcall map-x ex1) (funcall map-x ex2))))
         (excal--put element 'y (float (min (funcall map-y ey1) (funcall map-y ey2))))))
      ((or "line" "arrow" "freedraw")
       (let* ((ox (plist-get geometry :x)) (oy (plist-get geometry :y))
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
       (let ((x1 (funcall map-x ex1)) (x2 (funcall map-x ex2))
             (y1 (funcall map-y ey1)) (y2 (funcall map-y ey2)))
         (excal--put element 'x (float (min x1 x2)))
         (excal--put element 'y (float (min y1 y2)))
         (excal--put element 'width (float (max 1 (abs (- x2 x1)))))
         (excal--put element 'height (float (max 1 (abs (- y2 y1))))))))
    (excal--touch element)))

(defun excal--resize (element handle geometry dx dy)
  "Resize a lone ELEMENT by dragging HANDLE DX, DY scene units from GEOMETRY."
  (let* ((from (plist-get geometry :bounds))
         (to (excal--dragged-bounds from handle dx dy)))
    (if (equal (excal--get element 'type) "text")
        ;; Corner-resized text keeps the opposite corner fixed.
        (pcase-let ((`(,ox1 ,oy1 ,ox2 ,oy2) from))
          (excal--map-geometry element geometry from to)
          (let ((w (excal--get element 'width)) (h (excal--get element 'height)))
            (excal--put element 'x (float (if (memq handle '(nw sw)) (- ox2 w) ox1)))
            (excal--put element 'y (float (if (memq handle '(nw ne)) (- oy2 h) oy1)))
            (excal--touch element)))
      (excal--map-geometry element geometry from to))))

(defun excal--resize-selection (handle geometries bounds dx dy)
  "Resize the selection by dragging HANDLE DX, DY from its original BOUNDS.
GEOMETRIES is an alist of (ELEMENT . GEOMETRY) snapshots."
  (if (null (cdr geometries))
      (excal--resize (caar geometries) handle (cdar geometries) dx dy)
    (let ((to (excal--dragged-bounds bounds handle dx dy)))
      (pcase-dolist (`(,element . ,geometry) geometries)
        (excal--map-geometry element geometry bounds to)))))

;;;; Pointer shape

(defun excal--pointer-at (scene-xy)
  "Return the pointer shape for SCENE-XY given the current tool.
Emacs only offers a few portable shapes: there is no diagonal resize,
move or crosshair pointer, so corners use `hdrag' and elements `hand'."
  (pcase excal--tool
    ('select
     (let ((handle (excal--hit-handle scene-xy)))
       (cond ((memq handle '(n s)) 'nhdrag)
             (handle 'hdrag)
             ((excal--hit scene-xy) 'hand)
             (t 'arrow))))
    ('hand 'hand)
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
  (when-let* ((xy (excal--mouse-scene-xy)))
    (excal--set-pointer (excal--pointer-at xy))))

(defun excal-mouse-move (event)
  "Update the pointer shape for mouse movement EVENT."
  (interactive "e")
  (let ((posn (event-start event)))
    (when (and (eq (posn-window posn) (get-buffer-window (current-buffer)))
               (null (posn-area posn)))
      (excal--set-pointer (excal--pointer-at (excal--event-scene-xy event))))))

;;;; Dragging

(defun excal--drag-loop (on-move &optional button)
  "Track the mouse, calling ON-MOVE with each movement event until release.
BUTTON is the mouse button being held, `mouse-1' by default.  ON-MOVE
returns the damage it caused: a device rectangle (X1 Y1 X2 Y2), `scroll'
or `full'.  Damage from frames skipped under pending input accumulates.
Return the release event, or nil if another event ended the drag."
  (let ((pending nil)
        (button (or button 'mouse-1)))
    (track-mouse
      (setq track-mouse 'dragging)
      (catch 'done
        (while t
          (let ((event (read--potential-mouse-event)))
            (cond
             ((mouse-movement-p event)
              (setq pending (excal--damage-union pending (funcall on-move event)))
              (unless (or (null pending) (input-pending-p))
                (excal--render pending)
                (setq pending nil)
                (redisplay)))
             ((eq (event-basic-type event) button)
              (when pending (excal--render pending))
              (throw 'done event))
             (t
              (when pending (excal--render pending))
              (push event unread-command-events)
              (throw 'done nil)))))))))

(defun excal--pan-drag (event button)
  "Pan the view while BUTTON, pressed at EVENT, is held."
  (let ((last (excal--event-window-xy event)))
    (excal--drag-loop
     (lambda (ev)
       (let ((xy (excal--event-window-xy ev)))
         ;; Pointer positions are whole pixels, so this keeps scroll reuse.
         (cl-incf excal--scroll-x (/ (float (- (car xy) (car last))) excal--zoom))
         (cl-incf excal--scroll-y (/ (float (- (cdr xy) (cdr last))) excal--zoom))
         (setq last xy)
         'scroll))
     button)))

(defun excal--move-drag (start)
  "Move the selection with the mouse from scene point START."
  (let* ((elements excal--selection)
         (origins (mapcar (lambda (e) (cons (excal--get e 'x) (excal--get e 'y)))
                          elements)))
    (excal--drag-loop
     (lambda (ev)
       (let* ((p (excal--event-scene-xy ev))
              (dx (- (car p) (car start))) (dy (- (cdr p) (cdr start))))
         (excal--with-elements-damage elements
           (cl-mapc (lambda (e origin)
                      (excal--put e 'x (float (+ (car origin) dx)))
                      (excal--put e 'y (float (+ (cdr origin) dy)))
                      (excal--touch e))
                    elements origins)))))))

(defun excal--resize-drag (handle start)
  "Resize the selection by dragging HANDLE from scene point START."
  (let ((geometries (mapcar (lambda (e) (cons e (excal--geometry e)))
                            excal--selection))
        (bounds (excal--selection-bounds)))
    (excal--drag-loop
     (lambda (ev)
       (let ((p (excal--event-scene-xy ev)))
         (excal--with-elements-damage (mapcar #'car geometries)
           (excal--resize-selection handle geometries bounds
                                    (- (car p) (car start))
                                    (- (cdr p) (cdr start)))))))))

(defun excal--marquee-drag (start add)
  "Select by dragging a box from scene point START.
With ADD, extend the existing selection instead of replacing it."
  (let ((base (and add excal--selection)))
    (setq excal--marquee (list (car start) (cdr start) (car start) (cdr start)))
    (unwind-protect
        (excal--drag-loop
         (lambda (ev)
           (let* ((p (excal--event-scene-xy ev))
                  (old-rect excal--marquee)
                  (old-selection excal--selection)
                  (rect (excal--normalize-rect (car start) (cdr start)
                                               (car p) (cdr p))))
             (setq excal--marquee rect
                   excal--selection nil)
             (excal--select (append base (excal--marquee-selection rect)))
             ;; Repaint both boxes; when the selection changed, also the
             ;; old and new selected elements, which covers their boxes
             ;; and the overall selection box.
             (excal--damage-union
              (excal--damage-union (excal--scene-rect-damage old-rect)
                                   (excal--scene-rect-damage rect))
              (unless (equal old-selection excal--selection)
                (excal--elements-damage (append old-selection excal--selection)))))))
      (setq excal--marquee nil))))

;;;; Mouse commands

(defun excal-mouse-down (event)
  "Start the current tool's drag at EVENT.
With shift, clicking toggles elements in the selection and box selection
adds to it."
  (interactive "e")
  (let* ((start (excal--event-scene-xy event))
         (shift (memq 'shift (event-modifiers event)))
         (sx (car start)) (sy (cdr start)))
    (pcase excal--tool
      ((and 'select (let handle (and (not shift) (excal--hit-handle start)))
            (guard handle))
       (excal--resize-drag handle start))
      ('select
       (let ((hit (excal--hit start)))
         (cond
          ((and hit shift)
           (excal--toggle-unit hit)
           (excal--render)
           (when (excal--selected-p hit)
             (excal--move-drag start)))
          (hit
           (unless (excal--selected-p hit)
             ;; Clicking outside the entered group leaves it.
             (unless (and excal--editing-group
                          (seq-contains-p (excal--get hit 'groupIds)
                                          excal--editing-group))
               (setq excal--editing-group nil))
             (setq excal--selection nil)
             (excal--select (excal--unit hit)))
           (excal--render)
           (excal--move-drag start))
          (t
           (unless shift (excal--deselect))
           (excal--render)
           (excal--marquee-drag start shift)))))
      ('hand
       (excal--pan-drag event 'mouse-1))
      ((and tool (or 'rectangle 'ellipse 'diamond))
       (let ((element (excal--make-element (symbol-name tool) sx sy)))
         (when (eq tool 'rectangle)
           (excal--put element 'roundness (list (cons 'type 3))))
         (setq excal--elements (append excal--elements (list element)))
         (excal--deselect)
         (excal--select (list element))
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
         (setq excal--elements (append excal--elements (list element)))
         (excal--deselect)
         (excal--select (list element))
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
         (setq excal--elements (append excal--elements (list element)))
         (excal--deselect)
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

(defun excal-mouse-pan (event)
  "Pan the view while the middle button, pressed at EVENT, is held."
  (interactive "e")
  (excal--pan-drag event 'mouse-2))

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
  "Edit text, enter a group, or add text at EVENT.
Double-clicking a grouped element enters its group so that its members
can be selected one by one; double-clicking text edits it; elsewhere a
new text element is created."
  (interactive "e")
  (excal--await-release)
  (let* ((xy (excal--event-scene-xy event))
         (hit (excal--hit xy))
         (group (and hit (excal--unit-group hit))))
    (cond
     (group
      (setq excal--editing-group group
            excal--selection nil)
      (excal--select (excal--unit hit)))
     ((equal (excal--get hit 'type) "text")
      (excal--deselect)
      (excal--select (list hit))
      (excal-edit-text))
     (t
      (setq excal--tool 'select)
      (excal--insert-text (car xy) (cdr xy))))
    (excal--render)
    (excal--update-pointer)))

;;;; Text

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
  (let ((element (excal--make-text-element x y "")))
    (setq excal--elements (append excal--elements (list element)))
    (excal--deselect)
    (excal--select (list element))
    (excal--render)
    (let ((text (condition-case nil (excal--edit-text-live element) (quit nil))))
      (when (or (null text) (string-empty-p text))
        (setq excal--elements (delq element excal--elements))
        (excal--deselect)))
    (excal--render)))

(defun excal-edit-text ()
  "Edit the selected text element, previewing changes on the canvas."
  (interactive)
  (let ((element (excal--single-selection)))
    (when (equal (excal--get element 'type) "text")
      (condition-case nil (excal--edit-text-live element) (quit nil))
      (excal--render))))

;;;; Selection commands

(defun excal-select-all ()
  "Select every element."
  (interactive)
  (setq excal--editing-group nil)
  (excal--select (excal--live-elements))
  (excal--render))

(defun excal-escape ()
  "Leave the entered group, or clear the selection."
  (interactive)
  (if-let* ((group excal--editing-group))
      (let ((members (excal--group-members group)))
        (setq excal--editing-group nil
              excal--selection nil)
        (excal--select (if members (excal--unit (car members)) nil)))
    (excal--deselect))
  (excal--render))

(defun excal-delete-selected ()
  "Delete the selected elements."
  (interactive)
  (when excal--selection
    (dolist (e excal--selection)
      (excal--put e 'isDeleted t)
      (excal--touch e))
    (excal--deselect)
    (excal--render)))

(defun excal--nudge (dx dy)
  "Move the selection by DX, DY scene units."
  (when excal--selection
    (excal--render
     (excal--with-elements-damage excal--selection
       (dolist (e excal--selection)
         (excal--put e 'x (float (+ (excal--get e 'x) dx)))
         (excal--put e 'y (float (+ (excal--get e 'y) dy)))
         (excal--touch e))))))

(defmacro excal--define-nudge (name dx dy large)
  "Define command NAME nudging the selection by DX, DY steps.
LARGE selects `excal-nudge-large-step' instead of `excal-nudge-step'."
  `(defun ,name ()
     ,(format "Move the selection %s by `%s'."
              (cond ((< dx 0) "left") ((> dx 0) "right") ((< dy 0) "up") (t "down"))
              (if large "excal-nudge-large-step" "excal-nudge-step"))
     (interactive)
     (let ((step ,(if large 'excal-nudge-large-step 'excal-nudge-step)))
       (excal--nudge (* ,dx step) (* ,dy step)))))

(excal--define-nudge excal-nudge-left -1 0 nil)
(excal--define-nudge excal-nudge-right 1 0 nil)
(excal--define-nudge excal-nudge-up 0 -1 nil)
(excal--define-nudge excal-nudge-down 0 1 nil)
(excal--define-nudge excal-nudge-left-large -1 0 t)
(excal--define-nudge excal-nudge-right-large 1 0 t)
(excal--define-nudge excal-nudge-up-large 0 -1 t)
(excal--define-nudge excal-nudge-down-large 0 1 t)

;;;; Groups

(defun excal-group ()
  "Group the selected elements.
The new group becomes the outermost group of every selected element."
  (interactive)
  (if (not (cdr excal--selection))
      (message "Select at least two elements to group")
    (let ((group (excal--new-id)))
      (dolist (e excal--selection)
        (excal--put e 'groupIds
                    (vconcat (excal--get e 'groupIds) (vector group)))
        (excal--touch e)))
    (excal--render)))

(defun excal-ungroup ()
  "Remove the outermost selected group of the selected elements."
  (interactive)
  (let ((groups (delete-dups (delq nil (mapcar #'excal--unit-group
                                               excal--selection)))))
    (if (null groups)
        (message "Selection is not grouped")
      (dolist (e excal--selection)
        (let ((ids (excal--get e 'groupIds)))
          (when (seq-some (lambda (g) (member g groups)) ids)
            (excal--put e 'groupIds
                        (vconcat (seq-remove (lambda (g) (member g groups)) ids)))
            (excal--touch e))))
      (excal--render))))

;;;; Z-order

(defun excal--reorder (fn)
  "Reorder the scene: FN takes the element list and the selected set.
Selected elements keep their relative order."
  (when excal--selection
    (setq excal--elements (funcall fn excal--elements excal--selection))
    ;; Keep the selection in z-order, and let history see the change.
    (excal--select excal--selection)
    (dolist (e excal--selection) (excal--touch e))
    (excal--render)))

(defun excal--step-selected (elements selected)
  "Move each SELECTED element one place later in ELEMENTS.
Scanning from the top, a selected element swaps with the unselected one
above it, so runs of selected elements move up as a block."
  (let ((result (vconcat elements)))
    (cl-loop for i from (- (length result) 2) downto 0
             when (and (memq (aref result i) selected)
                       (not (memq (aref result (1+ i)) selected)))
             do (cl-rotatef (aref result i) (aref result (1+ i))))
    (append result nil)))

(defun excal-bring-forward ()
  "Move the selection one step up in z-order."
  (interactive)
  (excal--reorder #'excal--step-selected))

(defun excal-send-backward ()
  "Move the selection one step down in z-order."
  (interactive)
  (excal--reorder (lambda (elements selected)
                    (nreverse (excal--step-selected (reverse elements) selected)))))

(defun excal-bring-to-front ()
  "Move the selection to the top of the z-order."
  (interactive)
  (excal--reorder (lambda (elements selected)
                    (append (seq-remove (lambda (e) (memq e selected)) elements)
                            (seq-filter (lambda (e) (memq e selected)) elements)))))

(defun excal-send-to-back ()
  "Move the selection to the bottom of the z-order."
  (interactive)
  (excal--reorder (lambda (elements selected)
                    (append (seq-filter (lambda (e) (memq e selected)) elements)
                            (seq-remove (lambda (e) (memq e selected)) elements)))))

(provide 'excal-edit)
;;; excal-edit.el ends here
