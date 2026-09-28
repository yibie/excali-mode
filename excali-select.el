;;; excali-select.el --- Selection model  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The selection is a list of element alists.  Elements are selected in
;; units: an element that belongs to groups is selected together with the
;; members of its outermost group, unless that group has been entered by
;; double-clicking (`excali--editing-group'), in which case the next group
;; inward is the unit.  `groupIds' are ordered innermost first.

;;; Code:

(require 'excali-core)

;;;; Queries

(defun excali--live-elements ()
  "Return the elements that are not deleted, in z-order."
  (seq-remove (lambda (e) (excali--get e 'isDeleted)) excali--elements))

(defun excali--live-element-by-id (id)
  "Return the live element with ID, or nil."
  (and (stringp id)
       (cl-find-if (lambda (e) (equal (excali--get e 'id) id)) (excali--live-elements))))

(defun excali--selected-p (element)
  "Return non-nil if ELEMENT is selected."
  (memq element excali--selection))

(defun excali--unit-group (element)
  "Return the group id that selects ELEMENT as a unit, or nil.
That is the outermost group of ELEMENT, or, while a group is being
edited, the group just inside it."
  (let* ((groups (append (excali--get element 'groupIds) nil))
         (inside (and excali--editing-group
                      (cl-position excali--editing-group groups :test #'equal))))
    (cond ((null groups) nil)
          (inside (and (> inside 0) (nth (1- inside) groups)))
          (t (car (last groups))))))

(defun excali--group-members (group)
  "Return the live elements whose groups include GROUP, in z-order."
  (seq-filter (lambda (e)
                (seq-contains-p (excali--get e 'groupIds) group))
              (excali--live-elements)))

(defun excali--unit (element)
  "Return the elements selected together with ELEMENT."
  (if-let* ((group (excali--unit-group element)))
      (excali--group-members group)
    (list element)))

(defun excali--elements-bounds (elements)
  "Return the union (X1 Y1 X2 Y2) of ELEMENTS' bounds, or nil."
  (when elements
    (let ((bounds (mapcar #'excali--bounds elements)))
      (list (apply #'min (mapcar #'car bounds))
            (apply #'min (mapcar #'cadr bounds))
            (apply #'max (mapcar #'caddr bounds))
            (apply #'max (mapcar #'cadddr bounds))))))

(defun excali--selection-bounds ()
  "Return the bounds of the selection, or nil."
  (excali--elements-bounds excali--selection))

(defun excali--single-selection ()
  "Return the selected element if exactly one is selected."
  (and excali--selection (null (cdr excali--selection)) (car excali--selection)))

;;;; Pointer position

(defun excali--mouse-scene-xy ()
  "Return the mouse position in scene units, or nil if it is not over the canvas."
  (let* ((window (and (display-graphic-p) (get-buffer-window (current-buffer))))
         (pointer (and window (mouse-absolute-pixel-position)))
         (edges (and window (window-inside-absolute-pixel-edges window))))
    (when (and edges
               (<= (nth 0 edges) (car pointer) (1- (nth 2 edges)))
               (<= (nth 1 edges) (cdr pointer) (1- (nth 3 edges))))
      (cons (- (/ (float (- (car pointer) (nth 0 edges))) excali--zoom)
               excali--scroll-x)
            (- (/ (float (- (cdr pointer) (nth 1 edges))) excali--zoom)
               excali--scroll-y)))))

(defun excali--view-center ()
  "Return the scene point at the center of the canvas."
  (let ((size (or excali--canvas-size '(0 . 0)))
        (scale (* excali--zoom excali--pixel-scale)))
    (cons (- (/ (car size) 2.0 scale) excali--scroll-x)
          (- (/ (cdr size) 2.0 scale) excali--scroll-y))))

;;;; Changing the selection

(declare-function excali--drop-frame-children "excali-frame")

(defun excali--select (elements &optional add)
  "Select ELEMENTS, keeping the current selection when ADD is non-nil.
The selection is kept in z-order and without duplicates, and never holds
a frame together with its children."
  (let* ((wanted (append (and add excali--selection) elements))
         (wanted (if (fboundp 'excali--drop-frame-children)
                     (excali--drop-frame-children wanted)
                   wanted)))
    (setq excali--selection
          (seq-filter (lambda (e) (memq e wanted)) (excali--live-elements)))))

(defun excali--toggle-unit (element)
  "Add ELEMENT's unit to the selection, or remove it if already selected."
  (let ((unit (excali--unit element)))
    (if (excali--selected-p element)
        (setq excali--selection
              (seq-remove (lambda (e) (memq e unit)) excali--selection))
      (excali--select unit t))))

(defun excali--deselect ()
  "Clear the selection, leaving any entered group and the point editor."
  (setq excali--selection nil
        excali--editing-group nil
        excali--editing-linear nil
        excali--selected-points nil))

(defun excali--inside-p (inner outer)
  "Return non-nil if rectangle INNER lies within rectangle OUTER."
  (pcase-let ((`(,ix1 ,iy1 ,ix2 ,iy2) inner)
              (`(,ox1 ,oy1 ,ox2 ,oy2) outer))
    (and (<= ox1 ix1) (<= oy1 iy1) (>= ox2 ix2) (>= oy2 iy2))))

(defun excali--marquee-selection (rect)
  "Return the elements whose whole unit lies inside RECT (X1 Y1 X2 Y2).
The result is in z-order."
  (let ((units nil))
    (dolist (element (excali--live-elements))
      ;; Bound text is never selected on its own; it follows its container.
      (unless (or (excali--get element 'locked)
                  (and (equal (excali--get element 'type) "text")
                       (stringp (excali--get element 'containerId))))
       (let ((unit (excali--unit element)))
        (unless (assoc unit units)
          (push (cons unit (excali--inside-p (excali--elements-bounds unit) rect))
                units)))))
    (apply #'append (mapcar #'car (seq-filter #'cdr (nreverse units))))))

(defun excali--normalize-rect (x1 y1 x2 y2)
  "Return the rectangle spanned by corners X1,Y1 and X2,Y2."
  (list (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)))

(provide 'excali-select)
;;; excali-select.el ends here
