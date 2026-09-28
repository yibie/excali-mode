;;; excal-create.el --- Creating elements with the drawing tools  -*- lexical-binding: t; -*-

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

(require 'excal-core)
(require 'excal-view)
(require 'excal-select)
(require 'excal-style)
(require 'excal-hit)
(require 'excal-transform)
(require 'excal-binding)
(require 'excal-snap)
(require 'excal-frame)

(defconst excal--minimum-arrow-size 20
  "MINIMUM_ARROW_SIZE, screen px: shorter linear drags start click-click mode.")

(defvar-local excal--tool-locked nil
  "Non-nil keeps the drawing tool active after creating an element.")

(defvar-local excal--multi-element nil
  "Line or arrow being drawn click by click; its last point floats.")

(defvar-local excal--new-arrow-start nil
  "Element the start of the arrow being drawn binds to, or nil.")

(defvar-local excal--binding-hover-since nil
  "When the arrow end being drawn began hovering `excal--binding-highlight'.")

(defvar-local excal--new-arrow-inside nil
  "Non-nil when the arrow being drawn binds \"inside\" (Alt at the press).")

(declare-function excal--drag-loop "excal-edit")
(declare-function excal--insert-text "excal-edit")
(declare-function excal--await-release "excal-edit")

;;;; Finishing

(defun excal--created (element)
  "Finish creating ELEMENT: select it and return to the selection tool.
With the tool locked, the tool stays and nothing is selected; freedraw
always keeps its tool and selects nothing, as upstream."
  (unless (or excal--tool-locked (equal (excal--get element 'type) "freedraw"))
    (excal--deselect)
    (excal--select (list element))
    (setq excal--tool 'select)))

(defun excal--discard (element)
  "Remove ELEMENT, which was never finished, from the scene."
  (setq excal--elements (delq element excal--elements))
  (excal--deselect))

(defun excal--add-new (element)
  "Put the new ELEMENT on top of the scene."
  (setq excal--elements (append excal--elements (list element))))

;;;; Shapes

(defun excal--drag-box (sx sy px py square from-center)
  "Return (X Y W H) for a shape dragged from SX,SY to PX,PY.
SQUARE makes width and height equal; FROM-CENTER grows about SX,SY."
  (let* ((w (abs (- px sx))) (h (abs (- py sy))))
    (when square (setq w (max w h) h w))
    (if from-center
        (list (- sx w) (- sy h) (* 2 w) (* 2 h))
      (list (if (< px sx) (- sx w) sx) (if (< py sy) (- sy h) sy) w h))))

(defun excal--create-shape (type start square from-center)
  "Drag out a new shape of TYPE from scene point START.
See `excal--drag-box' for SQUARE and FROM-CENTER.  A new frame adopts
the elements it encloses."
  (let ((element (if (equal type "frame")
                     (excal--new-frame (car start) (cdr start))
                   (excal--apply-current-style
                    (excal--make-element type (car start) (cdr start))))))
    (excal--add-new element)
    (excal--deselect)
    (excal--drag-loop
     (lambda (ev)
       (let ((p (excal--grid-point (excal--event-scene-xy ev))))
         (excal--with-damage element
           (pcase-let ((`(,x ,y ,w ,h) (excal--drag-box (car start) (cdr start)
                                                        (car p) (cdr p)
                                                        square from-center)))
             (excal--put element 'x (float x))
             (excal--put element 'y (float y))
             (excal--put element 'width (float w))
             (excal--put element 'height (float h))
             (excal--touch element))))))
    (if (and (zerop (excal--get element 'width)) (zerop (excal--get element 'height)))
        (excal--discard element)
      (when (excal--frame-p element)
        (excal--adopt-into-frame element))
      (excal--created element))))

;;;; Lines and arrows

(defun excal--lock-angle (dx dy)
  "Return DX, DY snapped to the nearest 15-degree direction, as (DX . DY).
The pointer is projected onto that ray, as `getLockedLinearCursorAlignSize'."
  (let* ((angle (atan dy dx))
         (locked (* (round angle excal--shift-locking-angle) excal--shift-locking-angle))
         (ux (cos locked)) (uy (sin locked))
         (len (+ (* dx ux) (* dy uy))))
    (cons (* len ux) (* len uy))))

(defun excal--track-binding (element point)
  "Highlight what the end of new arrow ELEMENT at POINT would bind to.
Return the damage of the highlight change.  Lines never bind."
  (let ((old excal--binding-highlight))
    (setq excal--binding-highlight
          (and (equal (excal--get element 'type) "arrow")
               (excal--binding-candidate point (list element))))
    (unless (eq old excal--binding-highlight)
      (setq excal--binding-hover-since (float-time))
      (excal--elements-damage (delq nil (list old excal--binding-highlight))))))

(defun excal--bind-new-arrow (arrow)
  "Bind the ends of the new ARROW and snap them to the bound outlines."
  (setq excal--binding-highlight nil)
  (when (equal (excal--get arrow 'type) "arrow")
    (let* ((n (length (excal--get arrow 'points)))
           (start (excal--arrow-point arrow 0))
           (end (excal--arrow-point arrow (1- n)))
           (end-target (excal--binding-candidate end (list arrow))))
      (when excal--new-arrow-start
        (excal--bind-end arrow 'start excal--new-arrow-start start excal--new-arrow-inside))
      (when end-target
        (excal--bind-end arrow 'end end-target end
                         (or excal--new-arrow-inside
                             (and excal--binding-hover-since
                                  (>= (- (float-time) excal--binding-hover-since)
                                      excal--bind-mode-timeout)))))
      (excal--update-arrow arrow)))
  (setq excal--new-arrow-start nil))

(defun excal--set-last-point (element dx dy)
  "Move ELEMENT's last point to DX, DY relative to its origin."
  (let ((points (copy-sequence (excal--get element 'points))))
    (aset points (1- (length points)) (vector (float dx) (float dy)))
    (excal--put element 'points points)
    (excal--linear-extent element)
    (excal--touch element)))

(defun excal--new-linear (type start)
  "Return a new line or arrow of TYPE starting at scene point START."
  (excal--apply-current-style
   (excal--make-element type (car start) (cdr start)
                        (cons 'points (vector [0.0 0.0] [0.0 0.0]))
                        (cons 'startBinding :null) (cons 'endBinding :null)
                        (cons 'startArrowhead :null) (cons 'endArrowhead :null))))

(defun excal--create-linear (type start lock-angle &optional inside)
  "Drag out a new line or arrow of TYPE from scene point START.
LOCK-ANGLE snaps the direction to 15 degrees; INSIDE binds arrow ends
inside shapes rather than on their outline.  A short drag switches to
click-click mode instead of finishing."
  (setq excal--new-arrow-inside inside excal--binding-hover-since nil)
  (let ((element (excal--new-linear type start)))
    (setq excal--new-arrow-start (and (equal type "arrow")
                                      (excal--binding-candidate start (list element))))
    (excal--add-new element)
    (excal--deselect)
    (excal--drag-loop
     (lambda (ev)
       (let* ((p (excal--grid-point (excal--event-scene-xy ev)))
              (d (cons (- (car p) (car start)) (- (cdr p) (cdr start))))
              (d (if lock-angle (excal--lock-angle (car d) (cdr d)) d)))
         (excal--damage-union
          (excal--with-damage element
            (excal--set-last-point element (car d) (cdr d)))
          (excal--track-binding element (cons (+ (car start) (car d))
                                              (+ (cdr start) (cdr d))))))))
    (let* ((last (aref (excal--get element 'points) 1))
           (length (* excal--zoom (sqrt (+ (expt (aref last 0) 2) (expt (aref last 1) 2))))))
      (if (< length excal--minimum-arrow-size)
          (setq excal--multi-element element)
        (excal--bind-new-arrow element)
        (excal--created element)))))

(defun excal--multi-point-scene (element index)
  "Return ELEMENT's point INDEX in scene coordinates."
  (let ((p (aref (excal--get element 'points) index)))
    (cons (+ (excal--get element 'x) (aref p 0)) (+ (excal--get element 'y) (aref p 1)))))

(defun excal--multi-move (scene-xy)
  "Let the floating point of the element being drawn follow SCENE-XY."
  (let ((element excal--multi-element))
    (excal--render
     (excal--damage-union
      (excal--with-damage element
        (excal--set-last-point element
                               (- (car scene-xy) (excal--get element 'x))
                               (- (cdr scene-xy) (excal--get element 'y))))
      (excal--track-binding element scene-xy)))))

(defun excal--multi-click (scene-xy)
  "Handle a click at SCENE-XY while drawing a line or arrow point by point.
Clicking the last committed point again finishes; a line whose new point
lands on its start closes and finishes; otherwise the point is committed
and a new floating point follows the mouse."
  (let* ((element excal--multi-element)
         (points (excal--get element 'points))
         (n (length points))
         (committed (excal--multi-point-scene element (- n 2)))
         (close (/ (float excal--line-confirm-threshold) excal--zoom))
         (near (lambda (a b) (<= (sqrt (+ (expt (- (car a) (car b)) 2)
                                          (expt (- (cdr a) (cdr b)) 2)))
                                 close))))
    (cond
     ((and (> n 2) (funcall near scene-xy committed))
      (excal-finish-multi-point))
     ((and (equal (excal--get element 'type) "line") (>= n 3)
           (funcall near scene-xy (excal--multi-point-scene element 0)))
      ;; Close the loop exactly on the first point.
      (excal--set-last-point element 0 0)
      (setq excal--multi-element nil)
      (excal--created element))
     (t
      (excal--set-last-point element (- (car scene-xy) (excal--get element 'x))
                             (- (cdr scene-xy) (excal--get element 'y)))
      (excal--put element 'points
                  (vconcat (excal--get element 'points)
                           (vector (copy-sequence (aref (excal--get element 'points)
                                                        (1- n)))))))))
  (excal--render))

(defun excal-finish-multi-point ()
  "Finish the line or arrow being drawn point by point.
The floating point is dropped; an element left with fewer than two
points is discarded."
  (interactive)
  (when-let* ((element excal--multi-element))
    (setq excal--multi-element nil)
    (let ((points (excal--get element 'points)))
      (if (< (length points) 3)
          (progn (setq excal--binding-highlight nil excal--new-arrow-start nil)
                 (excal--discard element))
        (excal--put element 'points (seq-take points (1- (length points))))
        (excal--linear-extent element)
        (excal--touch element)
        (excal--bind-new-arrow element)
        (excal--created element)))
    (excal--render)))

;;;; Freedraw

(defun excal--create-freedraw (start)
  "Draw a freehand stroke from scene point START."
  (let* ((points (list [0.0 0.0]))
         (element (excal--apply-current-style
                   (excal--make-element
                    "freedraw" (car start) (cdr start)
                    (cons 'points (vconcat points))
                    (cons 'pressures []) (cons 'simulatePressure t)))))
    (excal--add-new element)
    (excal--deselect)
    (excal--drag-loop
     (lambda (ev)
       (let* ((p (excal--event-scene-xy ev))
              (point (vector (- (car p) (car start)) (- (cdr p) (cdr start)))))
         ;; Motion that does not move adds nothing to the stroke.
         (unless (equal point (car points))
           (excal--with-damage element
             (push point points)
             (excal--put element 'points (vconcat (reverse points)))
             (excal--touch element))))))
    ;; A click leaves a dot: upstream nudges the final point to allow it.
    (when (null (cdr points))
      (excal--put element 'points (vector [0.0 0.0] [0.0001 0.0001])))
    (excal--linear-extent element)
    (excal--touch element)
    (excal--created element)))

;;;; Dispatch

(defun excal--create (tool event start)
  "Create an element with TOOL for the press EVENT at scene point START.
With the grid on the start snaps to it, unless super is held."
  (let* ((mods (event-modifiers event))
         (start (if (eq tool 'freedraw) start
                  (excal--grid-point start (memq 'super mods)))))
    (pcase tool
      ((or 'rectangle 'ellipse 'diamond 'frame)
       (excal--create-shape (symbol-name tool) start (memq 'shift mods) (memq 'meta mods)))
      ((or 'arrow 'line)
       (excal--create-linear (symbol-name tool) start (memq 'shift mods)
                             (memq 'meta mods)))
      ('freedraw (excal--create-freedraw start))
      ('text
       (excal--await-release)
       (excal--insert-text (car start) (cdr start))
       (unless excal--tool-locked (setq excal--tool 'select))))
    ;; Elements drawn inside a frame belong to it.
    (unless (eq tool 'frame)
      (when-let* ((new (car (last excal--elements)))
                  ((not (excal--get new 'isDeleted)))
                  ((not (excal--frame-p new)))
                  ((null (excal--get new 'frameId)))
                  (frame (excal--frame-at start (list new)))
                  ((> (length (member frame excal--elements))
                      (length (member new excal--elements)))))
        ;; Only an element created by this press, above the frame.
        (excal--set-frame (list new) frame)))))

;;;; Tool commands

(defun excal-toggle-tool-lock ()
  "Toggle keeping the drawing tool after creating an element."
  (interactive)
  (setq excal--tool-locked (not excal--tool-locked))
  (message "Tool lock %s" (if excal--tool-locked "on" "off")))

(defvar-local excal--previous-tool 'select
  "Tool to return to when a toggle tool is chosen again.")

(defun excal-select-tool (tool)
  "Make TOOL current.
Choosing the arrow tool again cycles the arrow type between sharp and
round (`currentItemArrowType'); choosing the eraser or hand again goes
back to the previous tool."
  (when (and (eq tool 'arrow) (eq excal--tool 'arrow))
    (excal-set-style 'arrowType
                     (if (equal (excal--style-value 'arrowType) "round") "sharp" "round"))
    (message "Arrow type: %s" (excal--style-value 'arrowType)))
  (when excal--multi-element (excal-finish-multi-point))
  (if (and (memq tool '(eraser hand)) (eq excal--tool tool))
      (setq excal--tool excal--previous-tool)
    (unless (eq tool excal--tool)
      (setq excal--previous-tool excal--tool))
    (setq excal--tool tool)))

(provide 'excal-create)
;;; excal-create.el ends here
