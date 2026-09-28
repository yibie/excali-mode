;;; excal-select.el --- Selection model  -*- lexical-binding: t; -*-

;;; Commentary:

;; The selection is a list of element alists.  Elements are selected in
;; units: an element that belongs to groups is selected together with the
;; members of its outermost group, unless that group has been entered by
;; double-clicking (`excal--editing-group'), in which case the next group
;; inward is the unit.  `groupIds' are ordered innermost first.

;;; Code:

(require 'excal-core)

;;;; Queries

(defun excal--live-elements ()
  "Return the elements that are not deleted, in z-order."
  (seq-remove (lambda (e) (excal--get e 'isDeleted)) excal--elements))

(defun excal--selected-p (element)
  "Return non-nil if ELEMENT is selected."
  (memq element excal--selection))

(defun excal--unit-group (element)
  "Return the group id that selects ELEMENT as a unit, or nil.
That is the outermost group of ELEMENT, or, while a group is being
edited, the group just inside it."
  (let* ((groups (append (excal--get element 'groupIds) nil))
         (inside (and excal--editing-group
                      (cl-position excal--editing-group groups :test #'equal))))
    (cond ((null groups) nil)
          (inside (and (> inside 0) (nth (1- inside) groups)))
          (t (car (last groups))))))

(defun excal--group-members (group)
  "Return the live elements whose groups include GROUP, in z-order."
  (seq-filter (lambda (e)
                (seq-contains-p (excal--get e 'groupIds) group))
              (excal--live-elements)))

(defun excal--unit (element)
  "Return the elements selected together with ELEMENT."
  (if-let* ((group (excal--unit-group element)))
      (excal--group-members group)
    (list element)))

(defun excal--elements-bounds (elements)
  "Return the union (X1 Y1 X2 Y2) of ELEMENTS' bounds, or nil."
  (when elements
    (let ((bounds (mapcar #'excal--bounds elements)))
      (list (apply #'min (mapcar #'car bounds))
            (apply #'min (mapcar #'cadr bounds))
            (apply #'max (mapcar #'caddr bounds))
            (apply #'max (mapcar #'cadddr bounds))))))

(defun excal--selection-bounds ()
  "Return the bounds of the selection, or nil."
  (excal--elements-bounds excal--selection))

(defun excal--single-selection ()
  "Return the selected element if exactly one is selected."
  (and excal--selection (null (cdr excal--selection)) (car excal--selection)))

;;;; Pointer position

(defun excal--mouse-scene-xy ()
  "Return the mouse position in scene units, or nil if it is not over the canvas."
  (let* ((window (and (display-graphic-p) (get-buffer-window (current-buffer))))
         (pointer (and window (mouse-absolute-pixel-position)))
         (edges (and window (window-inside-absolute-pixel-edges window))))
    (when (and edges
               (<= (nth 0 edges) (car pointer) (1- (nth 2 edges)))
               (<= (nth 1 edges) (cdr pointer) (1- (nth 3 edges))))
      (cons (- (/ (float (- (car pointer) (nth 0 edges))) excal--zoom)
               excal--scroll-x)
            (- (/ (float (- (cdr pointer) (nth 1 edges))) excal--zoom)
               excal--scroll-y)))))

(defun excal--view-center ()
  "Return the scene point at the center of the canvas."
  (let ((size (or excal--canvas-size '(0 . 0)))
        (scale (* excal--zoom excal--pixel-scale)))
    (cons (- (/ (car size) 2.0 scale) excal--scroll-x)
          (- (/ (cdr size) 2.0 scale) excal--scroll-y))))

;;;; Changing the selection

(defun excal--select (elements &optional add)
  "Select ELEMENTS, keeping the current selection when ADD is non-nil.
The selection is kept in z-order and without duplicates."
  (let ((wanted (append (and add excal--selection) elements)))
    (setq excal--selection
          (seq-filter (lambda (e) (memq e wanted)) (excal--live-elements)))))

(defun excal--toggle-unit (element)
  "Add ELEMENT's unit to the selection, or remove it if already selected."
  (let ((unit (excal--unit element)))
    (if (excal--selected-p element)
        (setq excal--selection
              (seq-remove (lambda (e) (memq e unit)) excal--selection))
      (excal--select unit t))))

(defun excal--deselect ()
  "Clear the selection and leave any entered group."
  (setq excal--selection nil
        excal--editing-group nil))

(defun excal--inside-p (inner outer)
  "Return non-nil if rectangle INNER lies within rectangle OUTER."
  (pcase-let ((`(,ix1 ,iy1 ,ix2 ,iy2) inner)
              (`(,ox1 ,oy1 ,ox2 ,oy2) outer))
    (and (<= ox1 ix1) (<= oy1 iy1) (>= ox2 ix2) (>= oy2 iy2))))

(defun excal--marquee-selection (rect)
  "Return the elements whose whole unit lies inside RECT (X1 Y1 X2 Y2).
The result is in z-order."
  (let ((units nil))
    (dolist (element (excal--live-elements))
      (let ((unit (excal--unit element)))
        (unless (assoc unit units)
          (push (cons unit (excal--inside-p (excal--elements-bounds unit) rect))
                units))))
    (apply #'append (mapcar #'car (seq-filter #'cdr (nreverse units))))))

(defun excal--normalize-rect (x1 y1 x2 y2)
  "Return the rectangle spanned by corners X1,Y1 and X2,Y2."
  (list (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)))

(provide 'excal-select)
;;; excal-select.el ends here
