;;; excal-snap.el --- Grid and object snapping  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Grid mode and object snapping (docs/excalidraw-spec.md §2c.3, §2c.11,
;; §3b.8).  With the grid on, new elements, drags (by the top-left of
;; the moved bounds), resizes and line points snap to multiples of the
;; grid size, and the arrow keys step by it.  Object snapping aligns the
;; corners and center of a moved selection with those of other visible
;; elements within 8 screen px, drawing red snap lines.  Holding super
;; (Cmd) at the press turns grid snapping off for that gesture and
;; inverts object snapping, as Ctrl/Cmd does upstream.

;;; Code:

(require 'excal-core)
(require 'excal-view)
(require 'excal-select)
(require 'excal-handles)
(require 'excal-hit)

(defconst excal--snap-distance 8 "SNAP_DISTANCE, screen px.")
(defconst excal--snap-color "#ff6b6b" "SNAP_COLOR_LIGHT.")

(defvar-local excal--grid-enabled nil "Non-nil when grid mode is on.")
(defvar-local excal--grid-size 20 "Grid spacing in scene units.")
(defvar-local excal--grid-step 5 "Every this many grid lines is bold.")
(defvar-local excal--objects-snap-enabled nil "Non-nil when object snapping is on.")
(defvar-local excal--snap-lines nil
  "Snap lines to draw, each a list of scene points (X . Y).")

;;;; State

(defun excal--load-grid-state (app-state)
  "Initialize grid and snapping from APP-STATE."
  (let ((size (alist-get 'gridSize app-state))
        (step (alist-get 'gridStep app-state)))
    (setq excal--grid-enabled (eq (alist-get 'gridModeEnabled app-state) t)
          excal--grid-size (if (numberp size) (max 1 (min 100 (round size))) 20)
          excal--grid-step (if (numberp step) (max 1 (min 100 (round step))) 5)
          excal--objects-snap-enabled (eq (alist-get 'objectsSnapModeEnabled app-state) t))))

(defun excal--store-app-state (key value)
  "Record app-state KEY as VALUE in the document so saving keeps it."
  (when excal--doc
    (let ((state (copy-alist (alist-get 'appState excal--doc))))
      (setf (alist-get key state) value)
      (setf (alist-get 'appState excal--doc) state))))

(defun excal-toggle-grid ()
  "Toggle grid mode."
  (interactive)
  (setq excal--grid-enabled (not excal--grid-enabled))
  (excal--store-app-state 'gridModeEnabled (if excal--grid-enabled t :false))
  (when excal--grid-enabled
    (excal--store-app-state 'gridSize excal--grid-size)
    (excal--store-app-state 'gridStep excal--grid-step))
  (message "Grid %s" (if excal--grid-enabled "on" "off"))
  (excal--render))

(defun excal-toggle-objects-snap ()
  "Toggle snapping to other elements."
  (interactive)
  (setq excal--objects-snap-enabled (not excal--objects-snap-enabled))
  (message "Object snapping %s" (if excal--objects-snap-enabled "on" "off")))

;;;; Grid

(defun excal--grid-active-p (&optional suppress)
  "Return non-nil if grid snapping applies; SUPPRESS turns it off."
  (and excal--grid-enabled (not suppress)))

(defun excal--grid-value (v)
  "Round V to the nearest grid line."
  (* excal--grid-size (round v excal--grid-size)))

(defun excal--grid-point (point &optional suppress)
  "Return scene POINT snapped to the grid when it applies.
SUPPRESS, the super modifier, turns snapping off."
  (if (excal--grid-active-p suppress)
      (cons (float (excal--grid-value (car point))) (float (excal--grid-value (cdr point))))
    point))

(defun excal--view-rect ()
  "Return the visible scene rectangle (X1 Y1 X2 Y2), or nil."
  (when excal--canvas-size
    (let ((scale (* excal--zoom excal--pixel-scale)))
      (list (- excal--scroll-x) (- excal--scroll-y)
            (- (/ (car excal--canvas-size) scale) excal--scroll-x)
            (- (/ (cdr excal--canvas-size) scale) excal--scroll-y)))))

(defun excal--grid-native ()
  "Return the grid overlay over the visible scene, or nil."
  (when-let* ((excal--grid-enabled)
              (rect (excal--view-rect)))
    (pcase-let ((`(,x1 ,y1 ,x2 ,y2) rect))
      (let ((ov (excal--ov "ov-grid" x1 y1 (- x2 x1) (- y2 y1))))
        (aset ov 9 excal--grid-size)
        (aset ov 14 excal--grid-step)
        ov))))

;;;; Object snapping

(defun excal--objects-snap-active-p (invert)
  "Return non-nil if object snapping applies; INVERT flips the setting.
Upstream cannot turn it on by inverting while the grid is on."
  (if invert
      (and (not excal--objects-snap-enabled) (not excal--grid-enabled))
    excal--objects-snap-enabled))

(defun excal--snap-points (elements)
  "Return the snap points of ELEMENTS: box corners and center.
A lone element uses its rotated corners, or edge midpoints for diamonds
and ellipses; several use their common box."
  (if (and elements (null (cdr elements)))
      (let* ((e (car elements))
             (box (excal--element-box e))
             (center (excal--box-center box))
             (angle (excal--element-angle e)))
        (pcase-let ((`(,x1 ,y1 ,x2 ,y2) box))
          (cons center
                (mapcar (lambda (p) (excal--rotate-point p center angle))
                        (if (member (excal--get e 'type) '("diamond" "ellipse"))
                            (list (cons (car center) y1) (cons x2 (cdr center))
                                  (cons (car center) y2) (cons x1 (cdr center)))
                          (list (cons x1 y1) (cons x2 y1) (cons x2 y2) (cons x1 y2)))))))
    (when-let* ((box (excal--elements-bounds elements)))
      (pcase-let ((`(,x1 ,y1 ,x2 ,y2) box))
        (list (excal--box-center box) (cons x1 y1) (cons x2 y1)
              (cons x2 y2) (cons x1 y2))))))

(defun excal--reference-snap-points (exclude)
  "Return snap points of visible elements not in EXCLUDE."
  (let ((view (excal--view-rect)) points)
    (dolist (e (excal--live-elements))
      (unless (or (memq e exclude) (excal--bound-text-p e))
        (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excal--element-box e)))
          (when (or (null view)
                    (and (<= (nth 0 view) x2) (<= x1 (nth 2 view))
                         (<= (nth 1 view) y2) (<= y1 (nth 3 view))))
            (setq points (append (excal--snap-points (list e)) points))))))
    points))

(defun excal--snap-axis (moving references axis)
  "Return the best snap on AXIS (car or cdr) as (DELTA . PAIRS), or nil.
MOVING and REFERENCES are point lists; DELTA is what moves MOVING onto a
reference within `excal--snap-distance', and PAIRS the matched points."
  (let ((limit (/ (float excal--snap-distance) excal--zoom))
        (best nil))
    (dolist (m moving)
      (dolist (r references)
        (let ((d (- (funcall axis r) (funcall axis m))))
          (when (and (<= (abs d) limit)
                     (or (null best) (< (abs d) (abs (car best)))))
            (setq best (list d))))))
    (when best
      (let ((delta (car best)) pairs)
        (dolist (m moving)
          (dolist (r references)
            (when (< (abs (- (funcall axis r) (+ (funcall axis m) delta))) 1e-6)
              (push (cons m r) pairs))))
        (cons delta pairs)))))

(defun excal--snap-move (elements dx dy invert)
  "Return (DX . DY) adjusted so moving ELEMENTS snaps to other elements.
INVERT flips the object-snap setting.  Sets `excal--snap-lines'."
  (setq excal--snap-lines nil)
  (if (not (excal--objects-snap-active-p invert))
      (cons dx dy)
    (let* ((moving (mapcar (lambda (p) (cons (+ (car p) dx) (+ (cdr p) dy)))
                           (excal--snap-points elements)))
           (references (excal--reference-snap-points elements))
           (sx (excal--snap-axis moving references #'car))
           (sy (excal--snap-axis moving references #'cdr))
           (ndx (+ dx (if sx (car sx) 0)))
           (ndy (+ dy (if sy (car sy) 0))))
      ;; One line through every aligned point per axis and value.
      (dolist (entry (list (cons sx #'car) (cons sy #'cdr)))
        (when-let* ((snap (car entry)))
          (let ((axis (cdr entry)) (groups nil))
            (pcase-dolist (`(,m . ,r) (cdr snap))
              (let* ((moved (cons (+ (car m) (if sx (car sx) 0)) (+ (cdr m) (if sy (car sy) 0))))
                     (key (funcall axis r))
                     (cell (assoc key groups)))
                (if cell
                    (setcdr cell (append (list moved r) (cdr cell)))
                  (push (list key moved r) groups))))
            (dolist (g groups)
              (push (sort (delete-dups (cdr g))
                          (lambda (a b) (< (funcall (if (eq axis #'car) #'cdr #'car) a)
                                           (funcall (if (eq axis #'car) #'cdr #'car) b))))
                    excal--snap-lines)))))
      (cons ndx ndy))))

(defun excal--snap-line-natives ()
  "Return overlays for `excal--snap-lines': a line plus a cross per point."
  (let ((cross (/ 2.0 excal--zoom)) natives)
    (dolist (line excal--snap-lines)
      (let ((origin (car line)))
        (let ((ov (excal--ov "ov-poly" (car origin) (cdr origin) 0 0
                             :stroke (if (eq excal--theme 'dark) "#ff9090" excal--snap-color))))
          (aset ov 12 (vconcat (apply #'append
                                      (mapcar (lambda (p) (list (- (car p) (car origin))
                                                                (- (cdr p) (cdr origin))))
                                              line))))
          (push ov natives))
        (dolist (p line)
          (dolist (d (list (list (- cross) (- cross) cross cross)
                           (list (- cross) cross cross (- cross))))
            (let ((ov (excal--ov "ov-poly" (+ (car p) (nth 0 d)) (+ (cdr p) (nth 1 d)) 0 0
                                 :stroke (if (eq excal--theme 'dark) "#ff9090" excal--snap-color))))
              (aset ov 12 (vector 0.0 0.0 (- (nth 2 d) (nth 0 d)) (- (nth 3 d) (nth 1 d))))
              (push ov natives))))))
    natives))

(provide 'excal-snap)
;;; excal-snap.el ends here
