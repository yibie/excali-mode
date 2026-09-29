;;; excali-edit.el --- Tools, transforms and editing commands  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Hit testing, resize handles, pointer shapes, the drawing tools, moving
;; and resizing the selection, text editing, grouping, z-order and
;; deletion.  The selection model itself lives in excali-select.el.

;;; Code:

(require 'excali-core)
(require 'excali-view)
(require 'excali-select)
(require 'excali-style)
(require 'excali-handles)
(require 'excali-transform)
(require 'excali-hit)
(require 'excali-create)
(require 'excali-binding)
(require 'excali-linear)
(require 'excali-snap)
(require 'excali-frame)
(require 'excali-erase)
(require 'excali-index)
(require 'excali-elbow)
(require 'excali-cursor)

(defcustom excali-nudge-step 1
  "Scene units moved by the arrow keys."
  :type 'number
  :group 'excali)

(defcustom excali-nudge-large-step 5
  "Scene units moved by the arrow keys with shift."
  :type 'number
  :group 'excali)

;;;; Hit testing

(defun excali--in-selection-box-p (scene-xy)
  "Return non-nil if SCENE-XY lies inside the drawn selection box.
The box is the selection bounds padded like `draw_selection'."
  (when-let* ((bounds (excali--selection-bounds)))
    (pcase-let ((`(,x1 ,y1 ,x2 ,y2) bounds)
                (pad (/ 6.0 excali--zoom)))
      (and (<= (- x1 pad) (car scene-xy) (+ x2 pad))
           (<= (- y1 pad) (cdr scene-xy) (+ y2 pad))))))

;;;; Pointer shape

(defun excali-mouse-move (event)
  "Follow mouse movement EVENT: hover effects and multi-point lines."
  (interactive "e")
  (let ((posn (event-start event)))
    (when (and (eq (posn-window posn) (excali--view-window))
               (excali--canvas-area-p posn))
      (let ((xy (excali--event-scene-xy event)))
        (when excali--multi-element
          (excali--multi-move xy))
        (when-let* ((damage (excali--elbow-track-hover xy)))
          (excali--render damage))
        ;; The pointer map switches shapes itself; only the module's
        ;; view needs telling (`excali-native-cursors').
        (when excali--cursor-view-shown
          (excali--set-pointer (excali--cursor-at xy)))))))

;;;; Dragging

(defvar excali--last-release nil
  "Scene position of the last drag release, as a one-element list.")

(defun excali--drag-loop (on-move &optional button)
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
              (setq pending (excali--damage-union pending (funcall on-move event)))
              (unless (or (null pending) (input-pending-p))
                (excali--render pending)
                (setq pending nil)
                (redisplay)))
             ((eq (event-basic-type event) button)
              (when pending (excali--render pending))
              (setq excali--last-release (list (excali--event-scene-xy event)))
              (throw 'done event))
             (t
              (when pending (excali--render pending))
              (push event unread-command-events)
              (throw 'done nil)))))))))

(defun excali--pan-drag (event button)
  "Pan the view while BUTTON, pressed at EVENT, is held."
  (let ((last (excali--event-window-xy event)))
    (excali--set-pointer 'grabbing)
    (excali--drag-loop
     (lambda (ev)
       (let ((xy (excali--event-window-xy ev)))
         ;; Pointer positions are whole pixels, so this keeps scroll reuse.
         (cl-incf excali--scroll-x (/ (float (- (car xy) (car last))) excali--zoom))
         (cl-incf excali--scroll-y (/ (float (- (cdr xy) (cdr last))) excali--zoom))
         (setq last xy)
         'scroll))
     button)))


(defun excali--with-bound-arrows (elements)
  "Return ELEMENTS plus everything that follows them: bound arrows, labels."
  (excali--dependents elements))

(defun excali--move-drag (start &optional super)
  "Move the selection with the mouse from scene point START.
Arrows bound to moved shapes follow.  A lone bound arrow stays until it
is dragged past `excali--dragging-threshold', so clicking it does not
unbind it; moved arrows let go of shapes that stay behind.  The grid
snaps the top-left of the moved bounds; object snapping aligns with
other elements.  SUPER, held at the press, suppresses the grid and
inverts object snapping."
  ;; Bound elbow arrows only move with both of their shapes.
  (let* ((elements (excali--elbow-movable (excali--with-frame-children excali--selection)))
         (top-left (let ((b (excali--elements-bounds excali--selection)))
                     (cons (nth 0 b) (nth 1 b))))
         (affected (excali--with-bound-arrows elements))
         (origins (mapcar (lambda (e) (cons (excali--get e 'x) (excali--get e 'y)))
                          elements))
         (hold (and (null (cdr elements))
                    (equal (excali--get (car elements) 'type) "arrow")
                    (or (excali--get (car elements) 'startBinding)
                        (excali--get (car elements) 'endBinding))))
         (moved nil))
    (excali--drag-loop
     (lambda (ev)
       (when elements
         (let* ((p (excali--event-scene-xy ev))
                (dx (- (car p) (car start))) (dy (- (cdr p) (cdr start)))
                (corner (excali--grid-point (cons (+ (car top-left) dx) (+ (cdr top-left) dy))
                                           super))
                (dx (- (car corner) (car top-left))) (dy (- (cdr corner) (cdr top-left)))
                (snapped (if (excali--grid-active-p super)
                             (cons dx dy)
                           (excali--snap-move elements dx dy super)))
                (dx (car snapped)) (dy (cdr snapped)))
           (when (or moved (not hold)
                     (> (max (abs dx) (abs dy)) excali--dragging-threshold))
             (setq moved t)
             (excali--with-elements-damage affected
               (cl-mapc (lambda (e origin)
                          (excali--put e 'x (float (+ (car origin) dx)))
                          (excali--put e 'y (float (+ (cdr origin) dy)))
                          (excali--touch e))
                        elements origins)
               (excali--follow elements elements)))))))
    (setq excali--snap-lines nil)
    (when moved
      (excali--release-moved-arrows elements)
      (when-let* ((release (car excali--last-release)))
        (excali--update-frame-membership excali--selection release)))))

(defun excali--handle-reference (target handle)
  "Return the scene point of TARGET's box that HANDLE drags.
That is the corner, or the middle of the edge, rotated with the box."
  (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) (plist-get target :box))
               (x (cond ((memq handle '(nw w sw)) x1)
                        ((memq handle '(ne e se)) x2)
                        (t (/ (+ x1 x2) 2.0))))
               (y (cond ((memq handle '(nw n ne)) y1)
                        ((memq handle '(sw s se)) y2)
                        (t (/ (+ y1 y2) 2.0)))))
    (excali--rotate-point (cons x y) (excali--box-center (plist-get target :box))
                         (plist-get target :angle))))

(defun excali--transform-drag (handle start &optional shift alt)
  "Transform the selection by dragging HANDLE from scene point START.
SHIFT keeps the aspect ratio, or snaps rotation to 15 degrees; ALT
resizes about the center.  Like upstream, the offset between START and
the handle's reference point is kept so the shape does not jump."
  (let* ((target (excali--transform-target))
         (geometries (mapcar (lambda (e) (cons e (excali--snapshot-geometry e)))
                             excali--selection))
         (selected (mapcar #'car geometries))
         (elements (excali--with-bound-arrows selected))
         (box (plist-get target :box))
         (reference (and (not (eq handle 'rotation))
                         (excali--handle-reference target handle)))
         (offset (if reference
                     (cons (- (car reference) (car start)) (- (cdr reference) (cdr start)))
                   '(0 . 0))))
    (excali--drag-loop
     (lambda (ev)
       (let* ((p (excali--event-scene-xy ev))
              (pointer (excali--grid-point (cons (+ (car p) (car offset))
                                                (+ (cdr p) (cdr offset))))))
         (excali--with-elements-damage elements
           (cond
            ((and (eq handle 'rotation) (cdr geometries))
             (excali--rotate-multiple geometries (excali--box-center box) start p shift))
            ((eq handle 'rotation)
             (excali--rotate-single (caar geometries) (cdar geometries) p shift))
            ((cdr geometries)
             (excali--resize-multiple geometries box handle pointer shift alt))
            (t
             (excali--resize-single (caar geometries) (cdar geometries)
                                   handle pointer shift alt)))
           (if (eq handle 'rotation)
               (excali--follow selected selected)
             (excali--follow selected selected handle shift alt))))))
    (unless (eq handle 'rotation)
      (excali--update-resized-frames (seq-filter #'excali--frame-p selected)))))

(defun excali--marquee-drag (start add)
  "Select by dragging a box from scene point START.
With ADD, extend the existing selection instead of replacing it."
  (let ((base (and add excali--selection)))
    (setq excali--marquee (list (car start) (cdr start) (car start) (cdr start)))
    (unwind-protect
        (excali--drag-loop
         (lambda (ev)
           (let* ((p (excali--event-scene-xy ev))
                  (old-rect excali--marquee)
                  (old-selection excali--selection)
                  (rect (excali--normalize-rect (car start) (cdr start)
                                               (car p) (cdr p))))
             (setq excali--marquee rect
                   excali--selection nil)
             (excali--select (append base (excali--marquee-selection rect)))
             ;; Repaint both boxes; when the selection changed, also the
             ;; old and new selected elements, which covers their boxes
             ;; and the overall selection box.
             (excali--damage-union
              (excali--damage-union (excali--scene-rect-damage old-rect)
                                   (excali--scene-rect-damage rect))
              (unless (equal old-selection excali--selection)
                (excali--elements-damage (append old-selection excali--selection)))))))
      (setq excali--marquee nil))))

;;;; Mouse commands

(declare-function excali--lasso-drag "excali-tools")
(declare-function excali--laser-drag "excali-tools")
(declare-function excali--bucket-click "excali-bucket")

(defun excali-mouse-down (event)
  "Start the current tool's drag at EVENT.
With shift, clicking toggles elements in the selection and box selection
adds to it."
  (interactive "e")
  (let* ((start (excali--event-scene-xy event))
         (mods (event-modifiers event))
         (shift (memq 'shift mods))
         ;; Mod+Alt from the selection tool, or the lasso tool off any
         ;; element and handle, draws a lasso.
         (lasso (and (null excali--multi-element)
                     (or (and (memq excali--tool '(select lasso)) (memq 'meta mods)
                              (or (memq 'control mods) (memq 'super mods)))
                         (and (eq excali--tool 'lasso)
                              (not (or (excali--handle-at start) (excali--hit start)
                                       (excali--in-selection-box-p start)
                                       (excali--link-at start))))))))
    ;; Off the lasso path, the lasso tool acts as the selection tool.
    (pcase (if (eq excali--tool 'lasso) 'select excali--tool)
      ((guard lasso)
       (unless shift (excali--deselect))
       (excali--render)
       (excali--lasso-drag start shift))
      ('laser (excali--laser-drag start))
      ('bucketfill
       (excali--await-release)
       (excali--bucket-click start (memq 'meta mods)))
      ((guard excali--multi-element)
       (excali--await-release)
       (excali--multi-click start))
      ((and 'select (let linked (excali--link-at start)) (guard linked))
       (excali--await-release)
       (excali-follow-link linked))
      ('eraser
       (excali--erase-drag start (memq 'meta (event-modifiers event))))
      ;; A selected elbow arrow's ends and segment midpoints.
      ((and 'select (guard (excali--elbow-mouse-down start))))
      ((and 'select (guard (excali--linear-mouse-down event start))))
      ((and 'select (let handle (excali--handle-at start)) (guard handle))
       (excali--transform-drag handle start shift
                              (memq 'meta (event-modifiers event))))
      ('select
       (let ((hit (excali--hit start)))
         (cond
          ((and hit shift)
           (excali--toggle-unit hit)
           (excali--render)
           (when (excali--selected-p hit)
             (excali--move-drag start (memq 'super (event-modifiers event)))))
          ((and (not shift)
                (or (null hit) (excali--selected-p hit))
                (excali--in-selection-box-p start))
           ;; Like Excalidraw: pressing anywhere inside the selection box,
           ;; including the gaps between elements, drags the selection.
           (excali--move-drag start (memq 'super (event-modifiers event))))
          (hit
           (unless (excali--selected-p hit)
             ;; Clicking outside the entered group leaves it.
             (unless (and excali--editing-group
                          (seq-contains-p (excali--get hit 'groupIds)
                                          excali--editing-group))
               (setq excali--editing-group nil))
             (setq excali--selection nil)
             (excali--select (excali--unit hit)))
           (excali--render)
           (excali--move-drag start (memq 'super (event-modifiers event))))
          (t
           (unless shift (excali--deselect))
           (excali--render)
           (excali--marquee-drag start shift)))))
      ('hand
       (excali--pan-drag event 'mouse-1))
      (tool (excali--create tool event start)))
    (excali--render)))

(defun excali-mouse-pan (event)
  "Pan the view while the button pressed at EVENT is held.
The middle and right buttons pan."
  (interactive "e")
  (excali--pan-drag event (event-basic-type event)))

(defun excali--await-release ()
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

(defun excali-double-click (event)
  "Edit text, enter a group, or add text at EVENT.
Double-clicking a grouped element enters its group so that its members
can be selected one by one; double-clicking text edits it, and a shape
that can hold text gets its label edited or added; elsewhere a new text
element is created."
  (interactive "e")
  (excali--await-release)
  (let* ((xy (excali--event-scene-xy event))
         (hit (excali--hit xy))
         (group (and hit (excali--unit-group hit))))
    (cond
     ;; While drawing points a double click is just another click.
     (excali--multi-element
      (excali--multi-click xy))
     ;; On a selected elbow arrow's segment midpoint: release the segment.
     ((let ((single (excali--single-selection)))
        (and (excali--elbow-p single) (excali--elbow-double-click single xy))))
     ;; Double-clicking a line edits its points; with super, arrows too.
     ((and hit (or (equal (excali--get hit 'type) "line")
                   (and (memq 'super (event-modifiers event))
                        (excali--linear-p hit)
                        (not (excali--elbow-p hit)))))
      (excali--deselect)
      (excali--select (list hit))
      (excali-edit-linear t))
     (group
      (setq excali--editing-group group
            excali--selection nil)
      (excali--select (excali--unit hit)))
     ((equal (excali--get hit 'type) "text")
      (excali--deselect)
      (excali--select (list (or (excali--container-of hit) hit)))
      (if (excali--container-of hit)
          (excali--edit-container-text (excali--container-of hit))
        (excali-edit-text)))
     ((let ((single (excali--single-selection)))
        (and (excali--text-container-p single)
             (or (eq single hit) (null hit))))
      (excali--edit-container-text (excali--single-selection)))
     ((and (excali--text-container-p hit) (excali--binds-text-at-p hit xy))
      (excali--edit-container-text hit))
     (t
      (setq excali--tool 'select)
      (excali--insert-text (car xy) (cdr xy))))
    (excali--render)))

;;;; Text

(defvar-keymap excali-text-minibuffer-map
  :parent minibuffer-local-map
  :doc "Keymap for editing text elements in the minibuffer."
  "C-j" #'newline
  "S-<return>" #'newline)

(defun excali--container-geometry (container)
  "Snapshot CONTAINER's box so a text edit can restore or shrink it."
  (and container
       (mapcar (lambda (key) (cons key (excali--get container key)))
               '(x y width height))))

(defun excali--restore-geometry (container geometry)
  "Put CONTAINER's box back to GEOMETRY from `excali--container-geometry'."
  (pcase-dolist (`(,key . ,value) geometry)
    (excali--put container key value))
  (excali--touch container))

(defun excali--preview-text (element container geometry text)
  "Show TEXT in ELEMENT while editing.
A CONTAINER grows to fit and shrinks back, but never below its height
in GEOMETRY, as in Excalidraw's editor."
  (when container
    (excali--put container 'height (alist-get 'height geometry)))
  (excali--set-text element text))

(defun excali--edit-text-live (element)
  "Edit ELEMENT's text in the minibuffer, previewing every change.
Return the confirmed text, or nil when the edit was aborted; an abort
restores the original text and container size."
  (let* ((buffer (current-buffer))
         (container (excali--container-of element))
         (geometry (excali--container-geometry container))
         (original (or (excali--get element 'originalText)
                       (excali--get element 'text) ""))
         (preview (lambda (&rest _)
                    (let ((text (minibuffer-contents-no-properties)))
                      (with-current-buffer buffer
                        (excali--preview-text element container geometry text)
                        (excali--render)))))
         (confirmed nil))
    (unwind-protect
        (setq confirmed
              (minibuffer-with-setup-hook
                  (lambda () (add-hook 'after-change-functions preview nil t))
                (read-from-minibuffer "Text (C-j newline, RET done): "
                                      original excali-text-minibuffer-map)))
      (with-current-buffer buffer
        (if confirmed
            (excali--preview-text element container geometry confirmed)
          (when container (excali--restore-geometry container geometry))
          (excali--set-text element original))))
    confirmed))

(defun excali--finish-text-edit (element text)
  "Delete ELEMENT if the edit left TEXT blank; return non-nil if kept."
  (if (and text (not (string-empty-p (string-trim text))))
      t
    (if-let* ((container (excali--container-of element)))
        (excali--remove-bound-text container element)
      (setq excali--elements (delq element excali--elements)))
    (excali--deselect)
    nil))

(defun excali--insert-text (x y)
  "Create a text element at scene X, Y and edit it in place."
  (let ((element (excali--apply-current-style (excali--make-text-element x y ""))))
    (setq excali--elements (append excali--elements (list element)))
    (excali--deselect)
    (excali--select (list element))
    (excali--render)
    (let ((text (condition-case nil (excali--edit-text-live element) (quit nil))))
      (excali--finish-text-edit element text))
    (excali--render)))

(defun excali--edit-container-text (container)
  "Edit CONTAINER's label, creating it if CONTAINER has none."
  (let* ((existing (excali--bound-text-of container))
         (bound (alist-get 'boundElements container :null))
         (geometry (excali--container-geometry container))
         (text (or existing
                   (let ((label (excali--add-bound-text container)))
                     (excali--apply-current-style label)
                     (excali--put label 'textAlign "center")
                     (excali--put label 'verticalAlign "middle")
                     (excali--put label 'lineHeight
                                 (excali--line-height (excali--get label 'fontFamily)))
                     (excali--redraw-text label container)))))
    (excali--deselect)
    (excali--select (list container))
    (excali--render)
    (let ((result (condition-case nil (excali--edit-text-live text) (quit nil))))
      (unless (or (and existing (null result)) ; Aborted: keep the label.
                  (excali--finish-text-edit text result))
        (unless existing
          ;; Leave a cancelled new label no trace in the container.
          (excali--put container 'boundElements bound)
          (excali--restore-geometry container geometry))
        (excali--select (list container))))
    (excali--render)))

(defun excali--binds-text-at-p (container scene-xy)
  "Return non-nil if double-clicking SCENE-XY on CONTAINER edits its label.
Like upstream, a transparent shape only takes a new label when the
click is near its outline; filled shapes, arrows and shapes that already
have a label take it anywhere."
  (or (excali--arrow-p container)
      (excali--bound-text-of container)
      (not (member (excali--get container 'backgroundColor) '(nil "transparent")))
      (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excali--bounds container))
                  (tolerance (/ 10.0 excali--zoom))
                  (`(,px . ,py) scene-xy))
        (or (< (- px x1) tolerance) (< (- x2 px) tolerance)
            (< (- py y1) tolerance) (< (- y2 py) tolerance)))))

(defun excali-edit-text ()
  "Edit the selected text element or the label of the selected shape.
Changes are previewed on the canvas; a shape without a label gets one."
  (interactive)
  (let ((element (excali--single-selection)))
    (cond
     ((equal (excali--get element 'type) "text")
      (let ((text (condition-case nil (excali--edit-text-live element) (quit nil))))
        (when text (excali--finish-text-edit element text)))
      (excali--render))
     ((excali--text-container-p element)
      (excali--edit-container-text element)))))

;;;; Selection commands

(defun excali-select-all ()
  "Select every element."
  (interactive)
  (setq excali--editing-group nil)
  (excali--select (excali--live-elements))
  (excali--render))

(defun excali-escape ()
  "Leave the entered group, or clear the selection."
  (interactive)
  (if-let* ((group excali--editing-group))
      (let ((members (excali--group-members group)))
        (setq excali--editing-group nil
              excali--selection nil)
        (excali--select (if members (excali--unit (car members)) nil)))
    (excali--deselect))
  (excali--render))

(defun excali-delete-selected ()
  "Delete the selected points in the point editor, else the selected elements."
  (interactive)
  (if (and excali--editing-linear excali--selected-points)
      (excali-delete-points)
   (when excali--selection
    ;; Labels go with their containers; frame children are released.
    (let ((doomed (seq-union excali--selection (excali--labels-of excali--selection)))
          (released (excali--release-frame-children
                     (seq-filter #'excali--frame-p excali--selection))))
      (excali--forget-bindings-to doomed)
      (dolist (e doomed)
        (when (equal (excali--get e 'type) "arrow")
          (excali--unbind-end e 'start)
          (excali--unbind-end e 'end))
        (excali--put e 'isDeleted t)
        (excali--touch e))
      (excali--deselect)
      (excali--select (seq-remove (lambda (e) (memq e doomed)) released)))
    (excali--render))))

(defun excali--nudge (dx dy)
  "Move the selection by DX, DY scene units."
  (when excali--selection
    ;; Arrows bound to shapes outside the selection stay attached and are
    ;; re-routed instead of moved, as upstream.
    (let* ((moved (seq-remove
                   (lambda (e)
                     (seq-some (lambda (key)
                                 (when-let* ((b (excali--get e key)))
                                   (not (memq (excali--live-element-by-id (alist-get 'elementId b))
                                              excali--selection))))
                               '(startBinding endBinding)))
                   excali--selection)))
      (setq moved (excali--with-frame-children moved))
      (excali--render
       (excali--with-elements-damage (excali--with-bound-arrows moved)
         (dolist (e moved)
           (excali--put e 'x (float (+ (excali--get e 'x) dx)))
           (excali--put e 'y (float (+ (excali--get e 'y) dy)))
           (excali--touch e))
         (excali--follow moved moved))))))

(defmacro excali--define-nudge (name dx dy large)
  "Define command NAME nudging the selection by DX, DY steps.
LARGE selects `excali-nudge-large-step' instead of `excali-nudge-step'."
  `(defun ,name ()
     ,(format "Move the selection %s by `%s'."
              (cond ((< dx 0) "left") ((> dx 0) "right") ((< dy 0) "up") (t "down"))
              (if large "excali-nudge-large-step" "excali-nudge-step"))
     (interactive)
     ;; With the grid on, plain arrows step by the grid and shift by 1.
     (let ((step (if excali--grid-enabled
                     ,(if large 1 'excali--grid-size)
                   ,(if large 'excali-nudge-large-step 'excali-nudge-step))))
       (excali--nudge (* ,dx step) (* ,dy step)))))

(excali--define-nudge excali-nudge-left -1 0 nil)
(excali--define-nudge excali-nudge-right 1 0 nil)
(excali--define-nudge excali-nudge-up 0 -1 nil)
(excali--define-nudge excali-nudge-down 0 1 nil)
(excali--define-nudge excali-nudge-left-large -1 0 t)
(excali--define-nudge excali-nudge-right-large 1 0 t)
(excali--define-nudge excali-nudge-up-large 0 -1 t)
(excali--define-nudge excali-nudge-down-large 0 1 t)

;;;; Groups

(defun excali-group ()
  "Group the selected elements.
The new group becomes the outermost group of every selected element."
  (interactive)
  (if (not (cdr excali--selection))
      (message "Select at least two elements to group")
    (let ((group (excali--new-id)))
      (dolist (e excali--selection)
        (excali--put e 'groupIds
                    (vconcat (excali--get e 'groupIds) (vector group)))
        (excali--touch e)))
    (excali--render)))

(defun excali-ungroup ()
  "Remove the outermost selected group of the selected elements."
  (interactive)
  (let ((groups (delete-dups (delq nil (mapcar #'excali--unit-group
                                               excali--selection)))))
    (if (null groups)
        (message "Selection is not grouped")
      (dolist (e excali--selection)
        (let ((ids (excali--get e 'groupIds)))
          (when (seq-some (lambda (g) (member g groups)) ids)
            (excali--put e 'groupIds
                        (vconcat (seq-remove (lambda (g) (member g groups)) ids)))
            (excali--touch e))))
      (excali--render))))

;;;; Z-order

(defun excali--reorder (fn)
  "Reorder the scene: FN takes the element list and the selected set.
Selected elements keep their relative order."
  (when excali--selection
    (setq excali--elements (funcall fn excali--elements excali--selection))
    (excali--sync-moved-indices excali--selection)
    ;; Keep the selection in z-order, and let history see the change.
    (excali--select excali--selection)
    (dolist (e excali--selection) (excali--touch e))
    (excali--render)))

(defun excali--step-selected (elements selected)
  "Move each SELECTED element one place later in ELEMENTS.
Scanning from the top, a selected element swaps with the unselected one
above it, so runs of selected elements move up as a block."
  (let ((result (vconcat elements)))
    (cl-loop for i from (- (length result) 2) downto 0
             when (and (memq (aref result i) selected)
                       (not (memq (aref result (1+ i)) selected)))
             do (cl-rotatef (aref result i) (aref result (1+ i))))
    (append result nil)))

(defun excali-bring-forward ()
  "Move the selection one step up in z-order."
  (interactive)
  (excali--reorder #'excali--step-selected))

(defun excali-send-backward ()
  "Move the selection one step down in z-order."
  (interactive)
  (excali--reorder (lambda (elements selected)
                    (nreverse (excali--step-selected (reverse elements) selected)))))

(defun excali-bring-to-front ()
  "Move the selection to the top of the z-order."
  (interactive)
  (excali--reorder (lambda (elements selected)
                    (append (seq-remove (lambda (e) (memq e selected)) elements)
                            (seq-filter (lambda (e) (memq e selected)) elements)))))

(defun excali-send-to-back ()
  "Move the selection to the bottom of the z-order."
  (interactive)
  (excali--reorder (lambda (elements selected)
                    (append (seq-filter (lambda (e) (memq e selected)) elements)
                            (seq-remove (lambda (e) (memq e selected)) elements)))))

(provide 'excali-edit)
;;; excali-edit.el ends here
