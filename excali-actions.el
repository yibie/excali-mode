;;; excali-actions.el --- Editor actions: flip, align, distribute, lock, zoom to fit  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Commands matching Excalidraw's actions (docs/excalidraw-spec.md §3.A.3,
;; §3.A.7): flipping, aligning and distributing the selection, locking
;; elements, copying and pasting styles, font size steps, converting
;; between shape types, zooming to fit and page scrolling.  Alignment and
;; distribution treat each selected group as one unit.

;;; Code:

(require 'excali-core)
(require 'excali-view)
(require 'excali-select)
(require 'excali-style)
(require 'excali-handles)
(require 'excali-transform)
(require 'excali-binding)
(require 'excali-frame)

(declare-function excali--elbow-p "excali-elbow")
(declare-function excali--elbow-transformed "excali-elbow")

;;;; Units of the selection

(defun excali--selection-units ()
  "Return the selection split into units: groups, or lone elements."
  (let (units)
    (dolist (e excali--selection)
      (unless (seq-some (lambda (u) (memq e u)) units)
        (push (seq-filter #'excali--selected-p (excali--unit e)) units)))
    (nreverse units)))

(defun excali--move-elements (elements dx dy)
  "Move ELEMENTS and their frame children by DX, DY, re-routing arrows."
  (setq elements (excali--with-frame-children elements))
  (dolist (e elements)
    (excali--put e 'x (float (+ (excali--get e 'x) dx)))
    (excali--put e 'y (float (+ (excali--get e 'y) dy)))
    (excali--touch e))
  (excali--follow elements excali--selection))

;;;; Flip

(defun excali--flip (horizontal)
  "Mirror the selection about its center, HORIZONTAL or vertically.
Each element's geometry is mirrored in its own frame about its center,
which moves to its mirrored place; rotated elements negate their angle.
Text is moved but not mirrored."
  (when-let* ((box (excali--selection-bounds)))
    (let ((center (excali--box-center box))
          (elbows nil))
      (dolist (e excali--selection)
        (let* ((g (excali--snapshot-geometry e))
               (old (excali--box-center (plist-get g :box)))
               (new (if horizontal
                        (cons (- (* 2 (car center)) (car old)) (cdr old))
                      (cons (car old) (- (* 2 (cdr center)) (cdr old))))))
          (if (equal (excali--get e 'type) "text")
              (excali--move-elements (list e) (- (car new) (car old)) (- (cdr new) (cdr old)))
            (let* ((dx (- (car new) (car old))) (dy (- (cdr new) (cdr old)))
                   (mirror (lambda (p)
                             (if horizontal
                                 (cons (- (* 2 (car old)) (car p)) (cdr p))
                               (cons (car p) (- (* 2 (cdr old)) (cdr p)))))))
              (excali--place e g mirror (cons dx dy) (- (plist-get g :angle)))
              (when (excali--elbow-p e)
                (push (list e g (lambda (p)
                                  (let ((q (funcall mirror p)))
                                    (cons (+ (car q) dx) (+ (cdr q) dy)))))
                      elbows))))))
      ;; Elbow arrows re-bind where their mirrored ends landed and route
      ;; once every shape is in place.
      (pcase-dolist (`(,arrow ,geometry ,map) (nreverse elbows))
        (excali--elbow-transformed arrow geometry map t))
      (excali--follow excali--selection excali--selection))
    (excali--render)))

(defun excali-flip-horizontal ()
  "Mirror the selection left to right."
  (interactive)
  (excali--flip t))

(defun excali-flip-vertical ()
  "Mirror the selection top to bottom."
  (interactive)
  (excali--flip nil))

;;;; Align and distribute

(defun excali--align (edge)
  "Align the selected units to EDGE of the selection box.
EDGE is one of left, right, top, bottom, hcenter and vcenter."
  (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (or (excali--selection-bounds) '(0 0 0 0))))
    (when (cdr (excali--selection-units))
      (dolist (unit (excali--selection-units))
        (pcase-let ((`(,ux1 ,uy1 ,ux2 ,uy2) (excali--elements-bounds unit)))
          (excali--move-elements
           unit
           (pcase edge
             ('left (- x1 ux1)) ('right (- x2 ux2))
             ('hcenter (- (/ (+ x1 x2) 2.0) (/ (+ ux1 ux2) 2.0)))
             (_ 0))
           (pcase edge
             ('top (- y1 uy1)) ('bottom (- y2 uy2))
             ('vcenter (- (/ (+ y1 y2) 2.0) (/ (+ uy1 uy2) 2.0)))
             (_ 0)))))
      (excali--render))))

(defmacro excali--define-align (edge)
  "Define `excali-align-EDGE'."
  `(defun ,(intern (format "excali-align-%s" edge)) ()
     ,(format "Align the selected units to the %s of the selection." edge)
     (interactive)
     (excali--align ',edge)))

(excali--define-align left)
(excali--define-align right)
(excali--define-align top)
(excali--define-align bottom)
(excali--define-align hcenter)
(excali--define-align vcenter)

(defun excali--distribute (horizontal)
  "Space the selected units evenly, HORIZONTAL or vertically.
The outermost units stay; the gaps between neighbours become equal."
  (let* ((axis (if horizontal 0 1))
         (units (sort (mapcar (lambda (u) (cons u (excali--elements-bounds u)))
                              (excali--selection-units))
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
            (excali--move-elements (car u) (if horizontal delta 0) (if horizontal 0 delta))
            (setq pos (+ pos (- (nth (+ axis 2) (cdr u)) (nth axis (cdr u))) gap))))
        (excali--render)))))

(defun excali-distribute-horizontally ()
  "Space the selected units evenly from left to right."
  (interactive)
  (excali--distribute t))

(defun excali-distribute-vertically ()
  "Space the selected units evenly from top to bottom."
  (interactive)
  (excali--distribute nil))

;;;; Locking

(defun excali-toggle-lock ()
  "Lock the selection, or unlock it if all of it is locked.
Locked elements cannot be selected by clicking or box selection."
  (interactive)
  (when excali--selection
    (let ((lock (not (seq-every-p (lambda (e) (excali--get e 'locked)) excali--selection))))
      (dolist (e excali--selection)
        (excali--put e 'locked (if lock t :false))
        (excali--touch e))
      (when lock (excali--deselect))
      (message (if lock "Locked" "Unlocked"))
      (excali--render))))

(defun excali-unlock-all ()
  "Unlock every locked element and select them."
  (interactive)
  (let ((locked (seq-filter (lambda (e) (excali--get e 'locked)) (excali--live-elements))))
    (dolist (e locked)
      (excali--put e 'locked :false)
      (excali--touch e))
    (excali--deselect)
    (excali--select locked)
    (excali--render)))

;;;; Styles and fonts

(defvar excali--copied-style nil
  "Style properties copied by `excali-copy-styles', an alist.")

(defun excali-copy-styles ()
  "Remember the style of the first selected element."
  (interactive)
  (when-let* ((e (car excali--selection)))
    (setq excali--copied-style
          (delq nil (mapcar (lambda (entry)
                              (let ((p (car entry)))
                                (when (excali--style-applies-p p e)
                                  (cons p (excali--element-style-value p e)))))
                            excali-style-properties)))
    (message "Copied styles")))

(defun excali-paste-styles ()
  "Apply the copied style to the selected elements where it applies."
  (interactive)
  (when excali--copied-style
    (dolist (e excali--selection)
      (pcase-dolist (`(,property . ,value) excali--copied-style)
        (when (excali--style-applies-p property e)
          (excali--set-element-style e property value))))
    (excali--render)))

(defun excali--step-font-size (factor)
  "Multiply the font size of selected text by FACTOR, rounding."
  (dolist (e excali--selection)
    (when (equal (excali--get e 'type) "text")
      (excali--set-element-style
       e 'fontSize (max 1 (round (* factor (excali--get e 'fontSize)))))))
  (excali--render))

(defun excali-increase-font-size ()
  "Grow selected text by 10%."
  (interactive)
  (excali--step-font-size 1.1))

(defun excali-decrease-font-size ()
  "Shrink selected text by 10%."
  (interactive)
  (excali--step-font-size (/ 1 1.1)))

(defun excali-convert-type (&optional backward)
  "Cycle selected shapes through rectangle, diamond and ellipse.
With BACKWARD, cycle the other way."
  (interactive)
  (let ((cycle (if backward '("rectangle" "ellipse" "diamond")
                 '("rectangle" "diamond" "ellipse"))))
    (dolist (e excali--selection)
      (when-let* ((tail (member (excali--get e 'type) cycle)))
        (let ((next (or (cadr tail) (car cycle))))
          (excali--put e 'type next)
          (excali--put e 'roundness (if (and (excali--get e 'roundness)
                                            (not (equal next "ellipse")))
                                       '((type . 3))
                                     :null))
          (excali--touch e))))
    (excali--render)))

(defun excali-convert-type-backward ()
  "Cycle selected shapes backward; see `excali-convert-type'."
  (interactive)
  (excali-convert-type t))

;;;; Zoom to fit and page scrolling

(defun excali--zoom-to (box mode)
  "Zoom and scroll so BOX fits the view.
MODE `scale-down' never zooms in beyond 100%; `contain' may."
  (when (and box excali--canvas-size)
    (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) box)
                 (vw (/ (car excali--canvas-size) excali--pixel-scale))
                 (vh (/ (cdr excali--canvas-size) excali--pixel-scale))
                 (w (max 1 (- x2 x1))) (h (max 1 (- y2 y1)))
                 (fit (* 0.9 (min (/ vw w) (/ vh h))))
                 (zoom (max 0.1 (min 30.0 (if (eq mode 'scale-down) (min 1.0 fit) fit)))))
      (setq excali--zoom zoom
            excali--scroll-x (- (/ vw 2 zoom) (/ (+ x1 x2) 2.0))
            excali--scroll-y (- (/ vh 2 zoom) (/ (+ y1 y2) 2.0)))
      (excali--render))))

(defun excali-zoom-to-fit ()
  "Show every element, zooming out if needed."
  (interactive)
  (excali--zoom-to (excali--elements-bounds (excali--live-elements)) 'scale-down))

(defun excali-zoom-to-fit-selection-in-viewport ()
  "Show the selection, zooming out if needed."
  (interactive)
  (excali--zoom-to (excali--selection-bounds) 'scale-down))

(defun excali-zoom-to-fit-selection ()
  "Fill the view with the selection."
  (interactive)
  (excali--zoom-to (excali--selection-bounds) 'contain))

(defun excali--page (horizontal sign)
  "Scroll by one view, HORIZONTAL or vertically, in direction SIGN."
  (when excali--canvas-size
    (let ((span (/ (if horizontal (car excali--canvas-size) (cdr excali--canvas-size))
                   (* excali--pixel-scale excali--zoom))))
      (if horizontal
          (cl-incf excali--scroll-x (* sign span))
        (cl-incf excali--scroll-y (* sign span)))
      (excali--render 'scroll))))

(defun excali-page-up () "Scroll up one view." (interactive) (excali--page nil 1))
(defun excali-page-down () "Scroll down one view." (interactive) (excali--page nil -1))
(defun excali-page-left () "Scroll left one view." (interactive) (excali--page t 1))
(defun excali-page-right () "Scroll right one view." (interactive) (excali--page t -1))

(provide 'excali-actions)
;;; excali-actions.el ends here
