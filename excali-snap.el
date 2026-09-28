;;; excali-snap.el --- Grid and object snapping  -*- lexical-binding: t; -*-

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

(require 'excali-core)
(require 'excali-view)
(require 'excali-select)
(require 'excali-handles)
(require 'excali-hit)

(defconst excali--snap-distance 8 "SNAP_DISTANCE, screen px.")
(defconst excali--snap-color "#ff6b6b" "SNAP_COLOR_LIGHT.")

(defvar-local excali--grid-enabled nil "Non-nil when grid mode is on.")
(defvar-local excali--grid-size 20 "Grid spacing in scene units.")
(defvar-local excali--grid-step 5 "Every this many grid lines is bold.")
(defvar-local excali--objects-snap-enabled nil "Non-nil when object snapping is on.")
(defvar-local excali--snap-lines nil
  "Snap lines to draw, each a list of scene points (X . Y).")

;;;; State

(defun excali--load-grid-state (app-state)
  "Initialize grid and snapping from APP-STATE."
  (let ((size (alist-get 'gridSize app-state))
        (step (alist-get 'gridStep app-state)))
    (setq excali--grid-enabled (eq (alist-get 'gridModeEnabled app-state) t)
          excali--grid-size (if (numberp size) (max 1 (min 100 (round size))) 20)
          excali--grid-step (if (numberp step) (max 1 (min 100 (round step))) 5)
          excali--objects-snap-enabled (eq (alist-get 'objectsSnapModeEnabled app-state) t))))

(defun excali--store-app-state (key value)
  "Record app-state KEY as VALUE in the document so saving keeps it."
  (when excali--doc
    (let ((state (copy-alist (alist-get 'appState excali--doc))))
      (setf (alist-get key state) value)
      (setf (alist-get 'appState excali--doc) state))))

(defun excali-toggle-grid ()
  "Toggle grid mode."
  (interactive)
  (setq excali--grid-enabled (not excali--grid-enabled))
  (excali--store-app-state 'gridModeEnabled (if excali--grid-enabled t :false))
  (when excali--grid-enabled
    (excali--store-app-state 'gridSize excali--grid-size)
    (excali--store-app-state 'gridStep excali--grid-step))
  (message "Grid %s" (if excali--grid-enabled "on" "off"))
  (excali--render))

(defun excali-toggle-objects-snap ()
  "Toggle snapping to other elements."
  (interactive)
  (setq excali--objects-snap-enabled (not excali--objects-snap-enabled))
  (message "Object snapping %s" (if excali--objects-snap-enabled "on" "off")))

;;;; Grid

(defun excali--grid-active-p (&optional suppress)
  "Return non-nil if grid snapping applies; SUPPRESS turns it off."
  (and excali--grid-enabled (not suppress)))

(defun excali--grid-value (v)
  "Round V to the nearest grid line."
  (* excali--grid-size (round v excali--grid-size)))

(defun excali--grid-point (point &optional suppress)
  "Return scene POINT snapped to the grid when it applies.
SUPPRESS, the super modifier, turns snapping off."
  (if (excali--grid-active-p suppress)
      (cons (float (excali--grid-value (car point))) (float (excali--grid-value (cdr point))))
    point))

(defun excali--view-rect ()
  "Return the visible scene rectangle (X1 Y1 X2 Y2), or nil."
  (when excali--canvas-size
    (let ((scale (* excali--zoom excali--pixel-scale)))
      (list (- excali--scroll-x) (- excali--scroll-y)
            (- (/ (car excali--canvas-size) scale) excali--scroll-x)
            (- (/ (cdr excali--canvas-size) scale) excali--scroll-y)))))

(defun excali--grid-native ()
  "Return the grid overlay over the visible scene, or nil."
  (when-let* ((excali--grid-enabled)
              (rect (excali--view-rect)))
    (pcase-let ((`(,x1 ,y1 ,x2 ,y2) rect))
      (let ((ov (excali--ov "ov-grid" x1 y1 (- x2 x1) (- y2 y1))))
        (aset ov 9 excali--grid-size)
        (aset ov 14 excali--grid-step)
        ov))))

;;;; Object snapping

(defun excali--objects-snap-active-p (invert)
  "Return non-nil if object snapping applies; INVERT flips the setting.
Upstream cannot turn it on by inverting while the grid is on."
  (if invert
      (and (not excali--objects-snap-enabled) (not excali--grid-enabled))
    excali--objects-snap-enabled))

(defun excali--snap-points (elements)
  "Return the snap points of ELEMENTS: box corners and center.
A lone element uses its rotated corners, or edge midpoints for diamonds
and ellipses; several use their common box."
  (if (and elements (null (cdr elements)))
      (let* ((e (car elements))
             (box (excali--element-box e))
             (center (excali--box-center box))
             (angle (excali--element-angle e)))
        (pcase-let ((`(,x1 ,y1 ,x2 ,y2) box))
          (cons center
                (mapcar (lambda (p) (excali--rotate-point p center angle))
                        (if (member (excali--get e 'type) '("diamond" "ellipse"))
                            (list (cons (car center) y1) (cons x2 (cdr center))
                                  (cons (car center) y2) (cons x1 (cdr center)))
                          (list (cons x1 y1) (cons x2 y1) (cons x2 y2) (cons x1 y2)))))))
    (when-let* ((box (excali--elements-bounds elements)))
      (pcase-let ((`(,x1 ,y1 ,x2 ,y2) box))
        (list (excali--box-center box) (cons x1 y1) (cons x2 y1)
              (cons x2 y2) (cons x1 y2))))))

(defun excali--reference-snap-points (exclude)
  "Return snap points of visible elements not in EXCLUDE."
  (let ((view (excali--view-rect)) points)
    (dolist (e (excali--live-elements))
      (unless (or (memq e exclude) (excali--bound-text-p e))
        (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excali--element-box e)))
          (when (or (null view)
                    (and (<= (nth 0 view) x2) (<= x1 (nth 2 view))
                         (<= (nth 1 view) y2) (<= y1 (nth 3 view))))
            (setq points (append (excali--snap-points (list e)) points))))))
    points))

(defun excali--snap-axis (moving references axis)
  "Return the best snap on AXIS (car or cdr) as (DELTA . PAIRS), or nil.
MOVING and REFERENCES are point lists; DELTA is what moves MOVING onto a
reference within `excali--snap-distance', and PAIRS the matched points."
  (let ((limit (/ (float excali--snap-distance) excali--zoom))
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

(defun excali--snap-move (elements dx dy invert)
  "Return (DX . DY) adjusted so moving ELEMENTS snaps to other elements.
INVERT flips the object-snap setting.  Sets `excali--snap-lines'."
  (setq excali--snap-lines nil)
  (if (not (excali--objects-snap-active-p invert))
      (cons dx dy)
    (let* ((moving (mapcar (lambda (p) (cons (+ (car p) dx) (+ (cdr p) dy)))
                           (excali--snap-points elements)))
           (references (excali--reference-snap-points elements))
           (sx (excali--snap-axis moving references #'car))
           (sy (excali--snap-axis moving references #'cdr))
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
                    excali--snap-lines)))))
      (cons ndx ndy))))

(defun excali--snap-line-natives ()
  "Return overlays for `excali--snap-lines': a line plus a cross per point."
  (let ((cross (/ 2.0 excali--zoom)) natives)
    (dolist (line excali--snap-lines)
      (let ((origin (car line)))
        (let ((ov (excali--ov "ov-poly" (car origin) (cdr origin) 0 0
                             :stroke (if (eq excali--theme 'dark) "#ff9090" excali--snap-color))))
          (aset ov 12 (vconcat (apply #'append
                                      (mapcar (lambda (p) (list (- (car p) (car origin))
                                                                (- (cdr p) (cdr origin))))
                                              line))))
          (push ov natives))
        (dolist (p line)
          (dolist (d (list (list (- cross) (- cross) cross cross)
                           (list (- cross) cross cross (- cross))))
            (let ((ov (excali--ov "ov-poly" (+ (car p) (nth 0 d)) (+ (cdr p) (nth 1 d)) 0 0
                                 :stroke (if (eq excali--theme 'dark) "#ff9090" excali--snap-color))))
              (aset ov 12 (vector 0.0 0.0 (- (nth 2 d) (nth 0 d)) (- (nth 3 d) (nth 1 d))))
              (push ov natives))))))
    natives))

(provide 'excali-snap)
;;; excali-snap.el ends here
