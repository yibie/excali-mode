;;; excal-edit.el --- Tools, transforms and editing commands  -*- lexical-binding: t; -*-

;;; Commentary:

;; Hit testing, resize handles, pointer shapes, the drawing tools, moving
;; and resizing the selection, text editing, grouping, z-order and
;; deletion.  The selection model itself lives in excal-select.el.

;;; Code:

(require 'excal-core)
(require 'excal-view)
(require 'excal-select)
(require 'excal-style)
(require 'excal-handles)
(require 'excal-transform)
(require 'excal-hit)
(require 'excal-create)

(defcustom excal-nudge-step 1
  "Scene units moved by the arrow keys."
  :type 'number
  :group 'excal)

(defcustom excal-nudge-large-step 5
  "Scene units moved by the arrow keys with shift."
  :type 'number
  :group 'excal)

;;;; Hit testing

(defun excal--in-selection-box-p (scene-xy)
  "Return non-nil if SCENE-XY lies inside the drawn selection box.
The box is the selection bounds padded like `draw_selection'."
  (when-let* ((bounds (excal--selection-bounds)))
    (pcase-let ((`(,x1 ,y1 ,x2 ,y2) bounds)
                (pad (/ 6.0 excal--zoom)))
      (and (<= (- x1 pad) (car scene-xy) (+ x2 pad))
           (<= (- y1 pad) (cdr scene-xy) (+ y2 pad))))))

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

;;;; Pointer shape

(defun excal--pointer-at (scene-xy)
  "Return the pointer shape for SCENE-XY given the current tool.
Emacs only offers a few portable shapes: there is no diagonal resize,
rotate, move or crosshair pointer, so corners use `hdrag', and the
rotation handle and elements `hand'."
  (pcase excal--tool
    ('select
     (let ((handle (excal--handle-at scene-xy)))
       (cond ((eq handle 'rotation) 'hand)
             ((memq handle '(n s)) 'nhdrag)
             (handle 'hdrag)
             ((or (excal--hit scene-xy) (excal--in-selection-box-p scene-xy))
              'hand)
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
      (let ((xy (excal--event-scene-xy event)))
        (when excal--multi-element
          (excal--multi-move xy))
        (excal--set-pointer (excal--pointer-at xy))))))

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

(defun excal--handle-reference (target handle)
  "Return the scene point of TARGET's box that HANDLE drags.
That is the corner, or the middle of the edge, rotated with the box."
  (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) (plist-get target :box))
               (x (cond ((memq handle '(nw w sw)) x1)
                        ((memq handle '(ne e se)) x2)
                        (t (/ (+ x1 x2) 2.0))))
               (y (cond ((memq handle '(nw n ne)) y1)
                        ((memq handle '(sw s se)) y2)
                        (t (/ (+ y1 y2) 2.0)))))
    (excal--rotate-point (cons x y) (excal--box-center (plist-get target :box))
                         (plist-get target :angle))))

(defun excal--transform-drag (handle start &optional shift alt)
  "Transform the selection by dragging HANDLE from scene point START.
SHIFT keeps the aspect ratio, or snaps rotation to 15 degrees; ALT
resizes about the center.  Like upstream, the offset between START and
the handle's reference point is kept so the shape does not jump."
  (let* ((target (excal--transform-target))
         (geometries (mapcar (lambda (e) (cons e (excal--snapshot-geometry e)))
                             excal--selection))
         (elements (mapcar #'car geometries))
         (box (plist-get target :box))
         (reference (and (not (eq handle 'rotation))
                         (excal--handle-reference target handle)))
         (offset (if reference
                     (cons (- (car reference) (car start)) (- (cdr reference) (cdr start)))
                   '(0 . 0))))
    (excal--drag-loop
     (lambda (ev)
       (let* ((p (excal--event-scene-xy ev))
              (pointer (cons (+ (car p) (car offset)) (+ (cdr p) (cdr offset)))))
         (excal--with-elements-damage elements
           (cond
            ((and (eq handle 'rotation) (cdr geometries))
             (excal--rotate-multiple geometries (excal--box-center box) start p shift))
            ((eq handle 'rotation)
             (excal--rotate-single (caar geometries) (cdar geometries) p shift))
            ((cdr geometries)
             (excal--resize-multiple geometries box handle pointer shift alt))
            (t
             (excal--resize-single (caar geometries) (cdar geometries)
                                   handle pointer shift alt)))))))))

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
         (shift (memq 'shift (event-modifiers event))))
    (pcase excal--tool
      ((guard excal--multi-element)
       (excal--await-release)
       (excal--multi-click start))
      ((and 'select (let handle (excal--handle-at start)) (guard handle))
       (excal--transform-drag handle start shift
                              (memq 'meta (event-modifiers event))))
      ('select
       (let ((hit (excal--hit start)))
         (cond
          ((and hit shift)
           (excal--toggle-unit hit)
           (excal--render)
           (when (excal--selected-p hit)
             (excal--move-drag start)))
          ((and (not shift)
                (or (null hit) (excal--selected-p hit))
                (excal--in-selection-box-p start))
           ;; Like Excalidraw: pressing anywhere inside the selection box,
           ;; including the gaps between elements, drags the selection.
           (excal--move-drag start))
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
      (tool (excal--create tool event start)))
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
     ;; While drawing points a double click is just another click.
     (excal--multi-element
      (excal--multi-click xy))
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
  (let ((element (excal--apply-current-style (excal--make-text-element x y ""))))
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
