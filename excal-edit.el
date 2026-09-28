;;; excal-edit.el --- Selection, tools and editing  -*- lexical-binding: t; -*-

;;; Commentary:

;; Hit testing, resize handles, pointer shapes, the drawing tools and
;; text editing.

;;; Code:

(require 'excal-core)
(require 'excal-view)

(defun excal--hit (scene-xy)
  "Return the topmost element under SCENE-XY."
  (let ((tolerance (/ 8.0 excal--zoom)))
    (cl-find-if (lambda (element)
                  (and (not (excal--get element 'isDeleted))
                       (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excal--bounds element)))
                         (and (<= (- x1 tolerance) (car scene-xy) (+ x2 tolerance))
                              (<= (- y1 tolerance) (cdr scene-xy) (+ y2 tolerance))))))
                (reverse excal--elements))))

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

(defun excal-delete-selected ()
  "Delete the selected element."
  (interactive)
  (when excal--selected
    (excal--put excal--selected 'isDeleted t)
    (excal--touch excal--selected)
    (setq excal--selected nil)
    (excal--render)))

(provide 'excal-edit)
;;; excal-edit.el ends here
