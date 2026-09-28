;;; excal-actions.el --- Editor actions: flip, align, distribute, lock, zoom to fit  -*- lexical-binding: t; -*-

;;; Commentary:

;; Commands matching Excalidraw's actions (docs/excalidraw-spec.md §3.A.3,
;; §3.A.7): flipping, aligning and distributing the selection, locking
;; elements, copying and pasting styles, font size steps, converting
;; between shape types, zooming to fit and page scrolling.  Alignment and
;; distribution treat each selected group as one unit.

;;; Code:

(require 'excal-core)
(require 'excal-view)
(require 'excal-select)
(require 'excal-style)
(require 'excal-handles)
(require 'excal-transform)
(require 'excal-binding)

;;;; Units of the selection

(defun excal--selection-units ()
  "Return the selection split into units: groups, or lone elements."
  (let (units)
    (dolist (e excal--selection)
      (unless (seq-some (lambda (u) (memq e u)) units)
        (push (seq-filter #'excal--selected-p (excal--unit e)) units)))
    (nreverse units)))

(defun excal--move-elements (elements dx dy)
  "Move ELEMENTS by DX, DY scene units, re-routing arrows bound to them."
  (dolist (e elements)
    (excal--put e 'x (float (+ (excal--get e 'x) dx)))
    (excal--put e 'y (float (+ (excal--get e 'y) dy)))
    (excal--touch e))
  (excal--update-bound-arrows elements excal--selection))

;;;; Flip

(defun excal--flip (horizontal)
  "Mirror the selection about its center, HORIZONTAL or vertically.
Each element's geometry is mirrored in its own frame about its center,
which moves to its mirrored place; rotated elements negate their angle.
Text is moved but not mirrored."
  (when-let* ((box (excal--selection-bounds)))
    (let ((center (excal--box-center box)))
      (dolist (e excal--selection)
        (let* ((g (excal--snapshot-geometry e))
               (old (excal--box-center (plist-get g :box)))
               (new (if horizontal
                        (cons (- (* 2 (car center)) (car old)) (cdr old))
                      (cons (car old) (- (* 2 (cdr center)) (cdr old))))))
          (if (equal (excal--get e 'type) "text")
              (excal--move-elements (list e) (- (car new) (car old)) (- (cdr new) (cdr old)))
            (excal--place e g
                          (lambda (p)
                            (if horizontal
                                (cons (- (* 2 (car old)) (car p)) (cdr p))
                              (cons (car p) (- (* 2 (cdr old)) (cdr p)))))
                          (cons (- (car new) (car old)) (- (cdr new) (cdr old)))
                          (- (plist-get g :angle))))))
      (excal--update-bound-arrows excal--selection excal--selection))
    (excal--render)))

(defun excal-flip-horizontal ()
  "Mirror the selection left to right."
  (interactive)
  (excal--flip t))

(defun excal-flip-vertical ()
  "Mirror the selection top to bottom."
  (interactive)
  (excal--flip nil))

;;;; Align and distribute

(defun excal--align (edge)
  "Align the selected units to EDGE of the selection box.
EDGE is one of left, right, top, bottom, hcenter and vcenter."
  (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (or (excal--selection-bounds) '(0 0 0 0))))
    (when (cdr (excal--selection-units))
      (dolist (unit (excal--selection-units))
        (pcase-let ((`(,ux1 ,uy1 ,ux2 ,uy2) (excal--elements-bounds unit)))
          (excal--move-elements
           unit
           (pcase edge
             ('left (- x1 ux1)) ('right (- x2 ux2))
             ('hcenter (- (/ (+ x1 x2) 2.0) (/ (+ ux1 ux2) 2.0)))
             (_ 0))
           (pcase edge
             ('top (- y1 uy1)) ('bottom (- y2 uy2))
             ('vcenter (- (/ (+ y1 y2) 2.0) (/ (+ uy1 uy2) 2.0)))
             (_ 0)))))
      (excal--render))))

(defmacro excal--define-align (edge)
  "Define `excal-align-EDGE'."
  `(defun ,(intern (format "excal-align-%s" edge)) ()
     ,(format "Align the selected units to the %s of the selection." edge)
     (interactive)
     (excal--align ',edge)))

(excal--define-align left)
(excal--define-align right)
(excal--define-align top)
(excal--define-align bottom)
(excal--define-align hcenter)
(excal--define-align vcenter)

(defun excal--distribute (horizontal)
  "Space the selected units evenly, HORIZONTAL or vertically.
The outermost units stay; the gaps between neighbours become equal."
  (let* ((axis (if horizontal 0 1))
         (units (sort (mapcar (lambda (u) (cons u (excal--elements-bounds u)))
                              (excal--selection-units))
                      (lambda (a b) (< (+ (nth axis (cdr a)) (nth (+ axis 2) (cdr a)))
                                       (+ (nth axis (cdr b)) (nth (+ axis 2) (cdr b))))))))
    (when (> (length units) 2)
      (let* ((lo (apply #'min (mapcar (lambda (u) (nth axis (cdr u))) units)))
             (hi (apply #'max (mapcar (lambda (u) (nth (+ axis 2) (cdr u))) units)))
             (sizes (apply #'+ (mapcar (lambda (u) (- (nth (+ axis 2) (cdr u))
                                                      (nth axis (cdr u))))
                                       units)))
             (gap (/ (- hi lo sizes) (float (1- (length units)))))
             (pos lo))
        (dolist (u units)
          (let ((delta (- pos (nth axis (cdr u)))))
            (excal--move-elements (car u) (if horizontal delta 0) (if horizontal 0 delta))
            (setq pos (+ pos (- (nth (+ axis 2) (cdr u)) (nth axis (cdr u))) gap))))
        (excal--render)))))

(defun excal-distribute-horizontally ()
  "Space the selected units evenly from left to right."
  (interactive)
  (excal--distribute t))

(defun excal-distribute-vertically ()
  "Space the selected units evenly from top to bottom."
  (interactive)
  (excal--distribute nil))

;;;; Locking

(defun excal-toggle-lock ()
  "Lock the selection, or unlock it if all of it is locked.
Locked elements cannot be selected by clicking or box selection."
  (interactive)
  (when excal--selection
    (let ((lock (not (seq-every-p (lambda (e) (excal--get e 'locked)) excal--selection))))
      (dolist (e excal--selection)
        (excal--put e 'locked (if lock t :false))
        (excal--touch e))
      (when lock (excal--deselect))
      (message (if lock "Locked" "Unlocked"))
      (excal--render))))

(defun excal-unlock-all ()
  "Unlock every locked element and select them."
  (interactive)
  (let ((locked (seq-filter (lambda (e) (excal--get e 'locked)) (excal--live-elements))))
    (dolist (e locked)
      (excal--put e 'locked :false)
      (excal--touch e))
    (excal--deselect)
    (excal--select locked)
    (excal--render)))

;;;; Styles and fonts

(defvar excal--copied-style nil
  "Style properties copied by `excal-copy-styles', an alist.")

(defun excal-copy-styles ()
  "Remember the style of the first selected element."
  (interactive)
  (when-let* ((e (car excal--selection)))
    (setq excal--copied-style
          (delq nil (mapcar (lambda (entry)
                              (let ((p (car entry)))
                                (when (excal--style-applies-p p e)
                                  (cons p (excal--element-style-value p e)))))
                            excal-style-properties)))
    (message "Copied styles")))

(defun excal-paste-styles ()
  "Apply the copied style to the selected elements where it applies."
  (interactive)
  (when excal--copied-style
    (dolist (e excal--selection)
      (pcase-dolist (`(,property . ,value) excal--copied-style)
        (when (excal--style-applies-p property e)
          (excal--set-element-style e property value))))
    (excal--render)))

(defun excal--step-font-size (factor)
  "Multiply the font size of selected text by FACTOR, rounding."
  (dolist (e excal--selection)
    (when (equal (excal--get e 'type) "text")
      (excal--set-element-style
       e 'fontSize (max 1 (round (* factor (excal--get e 'fontSize)))))))
  (excal--render))

(defun excal-increase-font-size ()
  "Grow selected text by 10%."
  (interactive)
  (excal--step-font-size 1.1))

(defun excal-decrease-font-size ()
  "Shrink selected text by 10%."
  (interactive)
  (excal--step-font-size (/ 1 1.1)))

(defun excal-convert-type (&optional backward)
  "Cycle selected shapes through rectangle, diamond and ellipse.
With BACKWARD, cycle the other way."
  (interactive)
  (let ((cycle (if backward '("rectangle" "ellipse" "diamond")
                 '("rectangle" "diamond" "ellipse"))))
    (dolist (e excal--selection)
      (when-let* ((tail (member (excal--get e 'type) cycle)))
        (let ((next (or (cadr tail) (car cycle))))
          (excal--put e 'type next)
          (excal--put e 'roundness (if (and (excal--get e 'roundness)
                                            (not (equal next "ellipse")))
                                       '((type . 3))
                                     :null))
          (excal--touch e))))
    (excal--render)))

(defun excal-convert-type-backward ()
  "Cycle selected shapes backward; see `excal-convert-type'."
  (interactive)
  (excal-convert-type t))

;;;; Zoom to fit and page scrolling

(defun excal--zoom-to (box mode)
  "Zoom and scroll so BOX fits the view.
MODE `scale-down' never zooms in beyond 100%; `contain' may."
  (when (and box excal--canvas-size)
    (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) box)
                 (vw (/ (car excal--canvas-size) excal--pixel-scale))
                 (vh (/ (cdr excal--canvas-size) excal--pixel-scale))
                 (w (max 1 (- x2 x1))) (h (max 1 (- y2 y1)))
                 (fit (* 0.9 (min (/ vw w) (/ vh h))))
                 (zoom (max 0.1 (min 30.0 (if (eq mode 'scale-down) (min 1.0 fit) fit)))))
      (setq excal--zoom zoom
            excal--scroll-x (- (/ vw 2 zoom) (/ (+ x1 x2) 2.0))
            excal--scroll-y (- (/ vh 2 zoom) (/ (+ y1 y2) 2.0)))
      (excal--render))))

(defun excal-zoom-to-fit ()
  "Show every element, zooming out if needed."
  (interactive)
  (excal--zoom-to (excal--elements-bounds (excal--live-elements)) 'scale-down))

(defun excal-zoom-to-fit-selection-in-viewport ()
  "Show the selection, zooming out if needed."
  (interactive)
  (excal--zoom-to (excal--selection-bounds) 'scale-down))

(defun excal-zoom-to-fit-selection ()
  "Fill the view with the selection."
  (interactive)
  (excal--zoom-to (excal--selection-bounds) 'contain))

(defun excal--page (horizontal sign)
  "Scroll by one view, HORIZONTAL or vertically, in direction SIGN."
  (when excal--canvas-size
    (let ((span (/ (if horizontal (car excal--canvas-size) (cdr excal--canvas-size))
                   (* excal--pixel-scale excal--zoom))))
      (if horizontal
          (cl-incf excal--scroll-x (* sign span))
        (cl-incf excal--scroll-y (* sign span)))
      (excal--render 'scroll))))

(defun excal-page-up () "Scroll up one view." (interactive) (excal--page nil 1))
(defun excal-page-down () "Scroll down one view." (interactive) (excal--page nil -1))
(defun excal-page-left () "Scroll left one view." (interactive) (excal--page t 1))
(defun excal-page-right () "Scroll right one view." (interactive) (excal--page t -1))

(provide 'excal-actions)
;;; excal-actions.el ends here
