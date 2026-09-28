;;; excali-create.el --- Creating elements with the drawing tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Element creation following Excalidraw's pointer handling
;; (docs/excalidraw-spec.md §3b.1):
;;
;; - shapes are dragged out; a plain click creates nothing; shift makes a
;;   square, meta (Alt) draws from the center;
;; - lines and arrows follow the pointer with their second point, shift
;;   locks the angle to 15 degrees; a drag shorter than 20 screen px
;;   starts click-click mode, where each click commits a point and a
;;   floating point follows the mouse until the last point is clicked
;;   again, RET or ESC, or a line closes on its start;
;; - after creating, the tool returns to selection with the new element
;;   selected, unless the tool is locked (`q'); freedraw stays active.
;;
;; Modifiers are read from the press, since Emacs motion events carry
;; none.

;;; Code:

(require 'excali-core)
(require 'excali-view)
(require 'excali-select)
(require 'excali-style)
(require 'excali-hit)
(require 'excali-transform)
(require 'excali-binding)
(require 'excali-snap)
(require 'excali-frame)
(require 'excali-elbow)

(defconst excali--minimum-arrow-size 20
  "MINIMUM_ARROW_SIZE, screen px: shorter linear drags start click-click mode.")

(defvar-local excali--tool-locked nil
  "Non-nil keeps the drawing tool active after creating an element.")

(defvar-local excali--multi-element nil
  "Line or arrow being drawn click by click; its last point floats.")

(defvar-local excali--new-arrow-start nil
  "Element the start of the arrow being drawn binds to, or nil.")

(defvar-local excali--binding-hover-since nil
  "When the arrow end being drawn began hovering `excali--binding-highlight'.")

(defvar-local excali--new-arrow-inside nil
  "Non-nil when the arrow being drawn binds \"inside\" (Alt at the press).")

(declare-function excali--drag-loop "excali-edit")
(declare-function excali--insert-text "excali-edit")
(declare-function excali--await-release "excali-edit")

;;;; Finishing

(defun excali--created (element)
  "Finish creating ELEMENT: select it and return to the selection tool.
With the tool locked, the tool stays and nothing is selected; freedraw
always keeps its tool and selects nothing, as upstream."
  (unless (or excali--tool-locked (equal (excali--get element 'type) "freedraw"))
    (excali--deselect)
    (excali--select (list element))
    (setq excali--tool excali--preferred-selection-tool)))

(defun excali--discard (element)
  "Remove ELEMENT, which was never finished, from the scene."
  (setq excali--elements (delq element excali--elements))
  (excali--deselect))

(defun excali--add-new (element)
  "Put the new ELEMENT on top of the scene."
  (setq excali--elements (append excali--elements (list element))))

;;;; Shapes

(defun excali--drag-box (sx sy px py square from-center)
  "Return (X Y W H) for a shape dragged from SX,SY to PX,PY.
SQUARE makes width and height equal; FROM-CENTER grows about SX,SY."
  (let* ((w (abs (- px sx))) (h (abs (- py sy))))
    (when square (setq w (max w h) h w))
    (if from-center
        (list (- sx w) (- sy h) (* 2 w) (* 2 h))
      (list (if (< px sx) (- sx w) sx) (if (< py sy) (- sy h) sy) w h))))

(defun excali--create-shape (type start square from-center)
  "Drag out a new shape of TYPE from scene point START.
See `excali--drag-box' for SQUARE and FROM-CENTER.  A new frame adopts
the elements it encloses."
  (let ((element (if (equal type "frame")
                     (excali--new-frame (car start) (cdr start))
                   (excali--apply-current-style
                    (excali--make-element type (car start) (cdr start))))))
    (excali--add-new element)
    (excali--deselect)
    (excali--drag-loop
     (lambda (ev)
       (let ((p (excali--grid-point (excali--event-scene-xy ev))))
         (excali--with-damage element
           (pcase-let ((`(,x ,y ,w ,h) (excali--drag-box (car start) (cdr start)
                                                        (car p) (cdr p)
                                                        square from-center)))
             (excali--put element 'x (float x))
             (excali--put element 'y (float y))
             (excali--put element 'width (float w))
             (excali--put element 'height (float h))
             (excali--touch element))))))
    (if (and (zerop (excali--get element 'width)) (zerop (excali--get element 'height)))
        (excali--discard element)
      (when (excali--frame-p element)
        (excali--adopt-into-frame element))
      (excali--created element))))

;;;; Lines and arrows

(defun excali--lock-angle (dx dy)
  "Return DX, DY snapped to the nearest 15-degree direction, as (DX . DY).
The pointer is projected onto that ray, as `getLockedLinearCursorAlignSize'."
  (let* ((angle (atan dy dx))
         (locked (* (round angle excali--shift-locking-angle) excali--shift-locking-angle))
         (ux (cos locked)) (uy (sin locked))
         (len (+ (* dx ux) (* dy uy))))
    (cons (* len ux) (* len uy))))

(defun excali--track-binding (element point)
  "Highlight what the end of new arrow ELEMENT at POINT would bind to.
Return the damage of the highlight change.  Lines never bind."
  (let ((old excali--binding-highlight))
    (setq excali--binding-highlight
          (and (equal (excali--get element 'type) "arrow")
               (excali--binding-candidate point (list element))))
    (unless (eq old excali--binding-highlight)
      (setq excali--binding-hover-since (float-time))
      (excali--elements-damage (delq nil (list old excali--binding-highlight))))))

(defun excali--bind-new-arrow (arrow)
  "Bind the ends of the new ARROW and snap them to the bound outlines."
  (setq excali--binding-highlight nil)
  (cond
   ((excali--elbow-p arrow)
    ;; Elbow arrows bind with outline-snapped fixed points and re-route.
    (excali--elbow-finish-new arrow excali--new-arrow-start))
   ((equal (excali--get arrow 'type) "arrow")
    (let* ((n (length (excali--get arrow 'points)))
           (start (excali--arrow-point arrow 0))
           (end (excali--arrow-point arrow (1- n)))
           (end-target (excali--binding-candidate end (list arrow))))
      (when excali--new-arrow-start
        (excali--bind-end arrow 'start excali--new-arrow-start start excali--new-arrow-inside))
      (when end-target
        (excali--bind-end arrow 'end end-target end
                         (or excali--new-arrow-inside
                             (and excali--binding-hover-since
                                  (>= (- (float-time) excali--binding-hover-since)
                                      excali--bind-mode-timeout)))))
      (excali--update-arrow arrow))))
  (setq excali--new-arrow-start nil))

(defun excali--set-last-point (element dx dy)
  "Move ELEMENT's last point to DX, DY relative to its origin."
  (let ((points (copy-sequence (excali--get element 'points))))
    (aset points (1- (length points)) (vector (float dx) (float dy)))
    (excali--put element 'points points)
    (excali--linear-extent element)
    (excali--touch element)))

(defun excali--new-linear (type start)
  "Return a new line or arrow of TYPE starting at scene point START.
With the elbow arrow type, arrows are elbow arrows."
  (let ((element (excali--apply-current-style
                  (excali--make-element type (car start) (cdr start)
                                       (cons 'points (vector [0.0 0.0] [0.0 0.0]))
                                       (cons 'startBinding :null) (cons 'endBinding :null)
                                       (cons 'startArrowhead :null) (cons 'endArrowhead :null)))))
    (when (and (equal type "arrow") (equal (excali--style-value 'arrowType) "elbow"))
      (excali--elbow-make element))
    element))

(defun excali--create-linear (type start lock-angle &optional inside)
  "Drag out a new line or arrow of TYPE from scene point START.
LOCK-ANGLE snaps the direction to 15 degrees; INSIDE binds arrow ends
inside shapes rather than on their outline.  A short drag switches to
click-click mode instead of finishing."
  (setq excali--new-arrow-inside inside excali--binding-hover-since nil)
  (let ((element (excali--new-linear type start))
        (last-d '(0 . 0)))
    (setq excali--new-arrow-start (and (equal type "arrow")
                                      (excali--binding-candidate start (list element))))
    (excali--add-new element)
    (excali--deselect)
    (excali--drag-loop
     (lambda (ev)
       (let* ((p (excali--grid-point (excali--event-scene-xy ev)))
              (d (cons (- (car p) (car start)) (- (cdr p) (cdr start))))
              (d (if (and lock-angle (not (excali--elbow-p element)))
                     (excali--lock-angle (car d) (cdr d))
                   d)))
         (setq last-d d)
         (excali--damage-union
          (excali--with-damage element
            (if (excali--elbow-p element)
                (excali--elbow-drag-to element (cons (+ (car start) (car d))
                                                    (+ (cdr start) (cdr d))))
              (excali--set-last-point element (car d) (cdr d))))
          (excali--track-binding element (cons (+ (car start) (car d))
                                              (+ (cdr start) (cdr d))))))))
    (let* ((length (* excali--zoom (sqrt (+ (expt (car last-d) 2) (expt (cdr last-d) 2))))))
      (if (< length excali--minimum-arrow-size)
          (setq excali--multi-element element)
        (excali--bind-new-arrow element)
        (excali--created element)))))

(defun excali--multi-point-scene (element index)
  "Return ELEMENT's point INDEX in scene coordinates."
  (let ((p (aref (excali--get element 'points) index)))
    (cons (+ (excali--get element 'x) (aref p 0)) (+ (excali--get element 'y) (aref p 1)))))

(defun excali--multi-move (scene-xy)
  "Let the floating point of the element being drawn follow SCENE-XY."
  (let ((element excali--multi-element))
    (excali--render
     (excali--damage-union
      (excali--with-damage element
        (if (excali--elbow-p element)
            (excali--elbow-drag-to element scene-xy)
          (excali--set-last-point element
                                 (- (car scene-xy) (excali--get element 'x))
                                 (- (cdr scene-xy) (excali--get element 'y)))))
      (excali--track-binding element scene-xy)))))

(defun excali--multi-click (scene-xy)
  "Handle a click at SCENE-XY while drawing a line or arrow point by point.
Clicking the last committed point again finishes; a line whose new point
lands on its start closes and finishes; otherwise the point is committed
and a new floating point follows the mouse."
  (let* ((element excali--multi-element)
         (points (excali--get element 'points))
         (n (length points))
         (committed (excali--multi-point-scene element (- n 2)))
         (close (/ (float excali--line-confirm-threshold) excali--zoom))
         (near (lambda (a b) (<= (sqrt (+ (expt (- (car a) (car b)) 2)
                                          (expt (- (cdr a) (cdr b)) 2)))
                                 close))))
    (cond
     ((excali--elbow-p element)
      ;; Elbow arrows have only a start and an end: this click ends it.
      (excali--elbow-drag-to element scene-xy)
      (excali-finish-multi-point))
     ((and (> n 2) (funcall near scene-xy committed))
      (excali-finish-multi-point))
     ((and (equal (excali--get element 'type) "line") (>= n 3)
           (funcall near scene-xy (excali--multi-point-scene element 0)))
      ;; Close the loop exactly on the first point.
      (excali--set-last-point element 0 0)
      (setq excali--multi-element nil)
      (excali--created element))
     (t
      (excali--set-last-point element (- (car scene-xy) (excali--get element 'x))
                             (- (cdr scene-xy) (excali--get element 'y)))
      (excali--put element 'points
                  (vconcat (excali--get element 'points)
                           (vector (copy-sequence (aref (excali--get element 'points)
                                                        (1- n)))))))))
  (excali--render))

(defun excali-finish-multi-point ()
  "Finish the line or arrow being drawn point by point.
The floating point is dropped; an element left with fewer than two
points is discarded."
  (interactive)
  (when-let* ((element excali--multi-element))
    (setq excali--multi-element nil)
    (let ((points (excali--get element 'points)))
      (cond
       ((excali--elbow-p element)
        ;; The route to the floating end is the arrow.
        (if (and (zerop (excali--get element 'width)) (zerop (excali--get element 'height)))
            (progn (setq excali--binding-highlight nil excali--new-arrow-start nil)
                   (excali--discard element))
          (excali--bind-new-arrow element)
          (excali--created element)))
       ((< (length points) 3)
        (setq excali--binding-highlight nil excali--new-arrow-start nil)
        (excali--discard element))
       (t
        (excali--put element 'points (seq-take points (1- (length points))))
        (excali--linear-extent element)
        (excali--touch element)
        (excali--bind-new-arrow element)
        (excali--created element))))
    (excali--render)))

;;;; Freedraw

(defun excali--create-freedraw (start)
  "Draw a freehand stroke from scene point START."
  (let* ((points (list [0.0 0.0]))
         (element (excali--apply-current-style
                   (excali--make-element
                    "freedraw" (car start) (cdr start)
                    (cons 'points (vconcat points))
                    (cons 'pressures []) (cons 'simulatePressure t)))))
    (excali--add-new element)
    (excali--deselect)
    (excali--drag-loop
     (lambda (ev)
       (let* ((p (excali--event-scene-xy ev))
              (point (vector (- (car p) (car start)) (- (cdr p) (cdr start)))))
         ;; Motion that does not move adds nothing to the stroke.
         (unless (equal point (car points))
           (excali--with-damage element
             (push point points)
             (excali--put element 'points (vconcat (reverse points)))
             (excali--touch element))))))
    ;; A click leaves a dot: upstream nudges the final point to allow it.
    (when (null (cdr points))
      (excali--put element 'points (vector [0.0 0.0] [0.0001 0.0001])))
    (excali--linear-extent element)
    (excali--touch element)
    (excali--created element)))

;;;; Sticky notes

(defconst excali--sticky-note-size 250 "DEFAULT_STICKY_NOTE_SIZE.")
(defconst excali--sticky-note-min-size 75 "STICKY_NOTE_MIN_SIZE.")

(defun excali--create-sticky-note (start)
  "Create a sticky note from scene point START and edit its text.
A click places a default-sized note centered on START; a drag sizes it,
no smaller than `excali--sticky-note-min-size'."
  (let* ((now (truncate (* 1000 (float-time))))
         (note (excali--make-element
                "stickynote" (car start) (cdr start)
                (cons 'strokeColor "#1e1e1e") (cons 'backgroundColor "#ffdf6b")
                (cons 'fillStyle "solid") (cons 'strokeWidth 1)
                (cons 'roughness (or (excali--style-value 'roughness) 1))
                (cons 'roundness '((type . 2))) (cons 'created now)
                (cons 'baseHeight excali--sticky-note-size)))
         (dragged nil))
    (excali--add-new note)
    (excali--deselect)
    (excali--drag-loop
     (lambda (ev)
       (let ((p (excali--grid-point (excali--event-scene-xy ev))))
         (when (> (max (abs (- (car p) (car start))) (abs (- (cdr p) (cdr start))))
                  excali--dragging-threshold)
           (setq dragged t))
         (when dragged
           (excali--with-damage note
             (pcase-let ((`(,x ,y ,w ,h) (excali--drag-box (car start) (cdr start)
                                                          (car p) (cdr p) nil nil)))
               (excali--put note 'x (float x)) (excali--put note 'y (float y))
               (excali--put note 'width (float w)) (excali--put note 'height (float h))
               (excali--touch note)))))))
    (if dragged
        (let ((size-w (max excali--sticky-note-min-size (excali--get note 'width)))
              (size-h (max excali--sticky-note-min-size (excali--get note 'height))))
          (excali--put note 'width (float size-w))
          (excali--put note 'height (float size-h)))
      (let ((s excali--sticky-note-size))
        (excali--put note 'x (float (- (car start) (/ s 2))))
        (excali--put note 'y (float (- (cdr start) (/ s 2))))
        (excali--put note 'width (float s))
        (excali--put note 'height (float s))))
    (excali--put note 'baseHeight (excali--get note 'height))
    (excali--touch note)
    (excali--deselect)
    (excali--select (list note))
    (unless excali--tool-locked (setq excali--tool excali--preferred-selection-tool))
    (excali--render)
    (when (fboundp 'excali-edit-text)
      (excali-edit-text))))

;;;; Dispatch

(defun excali--create (tool event start)
  "Create an element with TOOL for the press EVENT at scene point START.
With the grid on the start snaps to it, unless super is held."
  (let* ((mods (event-modifiers event))
         (start (if (memq tool '(freedraw autoshape)) start
                  (excali--grid-point start (memq 'super mods)))))
    (pcase tool
      ((or 'rectangle 'ellipse 'diamond 'frame)
       (excali--create-shape (symbol-name tool) start (memq 'shift mods) (memq 'meta mods)))
      ((or 'arrow 'line)
       (excali--create-linear (symbol-name tool) start (memq 'shift mods)
                             (memq 'meta mods)))
      ('freedraw (excali--create-freedraw start))
      ('autoshape (excali--autoshape-drag start))
      ('stickynote (excali--create-sticky-note start))
      ('text
       (excali--await-release)
       (excali--insert-text (car start) (cdr start))
       (unless excali--tool-locked (setq excali--tool excali--preferred-selection-tool))))
    ;; Elements drawn inside a frame belong to it.
    (unless (eq tool 'frame)
      (when-let* ((new (car (last excali--elements)))
                  ((not (excali--get new 'isDeleted)))
                  ((not (excali--frame-p new)))
                  ((null (excali--get new 'frameId)))
                  (frame (excali--frame-at start (list new)))
                  ((> (length (member frame excali--elements))
                      (length (member new excali--elements)))))
        ;; Only an element created by this press, above the frame.
        (excali--set-frame (list new) frame)))))

;;;; Tool commands

(defun excali-toggle-tool-lock ()
  "Toggle keeping the drawing tool after creating an element."
  (interactive)
  (setq excali--tool-locked (not excali--tool-locked))
  (message "Tool lock %s" (if excali--tool-locked "on" "off")))

(declare-function excali-bucket-cycle-color "excali-bucket")
(declare-function excali--autoshape-drag "excali-tools")

(defvar-local excali--previous-tool 'select
  "Tool to return to when a toggle tool is chosen again.")

(defun excali-select-tool (tool)
  "Make TOOL current.
Choosing the arrow tool again cycles the arrow type sharp, round,
elbow (`currentItemArrowType'); choosing the eraser or hand again goes
back to the previous tool, and the bucket fill again cycles its color.
The selection tool is the preferred one, box or lasso."
  (when (and (eq tool 'arrow) (eq excali--tool 'arrow))
    (excali-set-style 'arrowType
                     (pcase (excali--style-value 'arrowType)
                       ("sharp" "round") ("round" "elbow") (_ "sharp")))
    (message "Arrow type: %s" (excali--style-value 'arrowType)))
  (when (eq tool 'select)
    (setq tool excali--preferred-selection-tool))
  (when excali--multi-element (excali-finish-multi-point))
  (cond
   ((and (eq tool 'bucketfill) (eq excali--tool 'bucketfill))
    (excali-bucket-cycle-color))
   ((and (memq tool '(eraser hand)) (eq excali--tool tool))
    (setq excali--tool excali--previous-tool))
   (t
    (unless (eq tool excali--tool)
      (setq excali--previous-tool excali--tool))
    (setq excali--tool tool))))

(provide 'excali-create)
;;; excali-create.el ends here
