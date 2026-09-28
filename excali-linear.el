;;; excali-linear.el --- Point editor for lines and arrows  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Editing the points of lines and arrows, following Excalidraw's
;; LinearElementEditor (docs/excalidraw-spec.md §3b.2, §2c.9).
;;
;; A lone selected line or arrow shows its points and can have them
;; dragged; a two-point one also offers its segment midpoint.  In edit
;; mode (double-click a line, RET on a line, s-RET on any linear
;; element) points are larger, every long enough segment offers a
;; midpoint that inserts a point when dragged, shift-click toggles point
;; selection, meta-click appends a point and DEL deletes selected points.
;; Dragging an arrow end binds it like drawing does.

;;; Code:

(require 'excali-core)
(require 'excali-view)
(require 'excali-select)
(require 'excali-handles)
(require 'excali-transform)
(require 'excali-binding)
(require 'excali-create)
(require 'excali-elbow)

(defconst excali--point-hit-size 11
  "POINT_HANDLE_SIZE + 1: screen px within which a point is grabbed.")


(declare-function excali--drag-loop "excali-edit")

;;;; Geometry

(defun excali--linear-scene-points (element)
  "Return ELEMENT's points in scene coordinates, rotation applied."
  (let ((center (excali--box-center (excali--element-box element)))
        (angle (excali--element-angle element)))
    (mapcar (lambda (p) (excali--rotate-point p center angle))
            (excali--absolute-points element))))

(defun excali--set-linear-scene-points (element points)
  "Give ELEMENT the scene POINTS, keeping its rotation.
The points are taken into the element's unrotated frame about its old
center; the new box center then gets the usual compensation so the
points land exactly where given."
  (let* ((old-center (excali--box-center (excali--element-box element)))
         (angle (excali--element-angle element))
         (local (mapcar (lambda (p) (excali--rotate-point p old-center (- angle))) points))
         (xs (mapcar #'car local)) (ys (mapcar #'cdr local))
         (new-center (cons (/ (+ (apply #'min xs) (apply #'max xs)) 2.0)
                           (/ (+ (apply #'min ys) (apply #'max ys)) 2.0)))
         (shift (excali--rotation-compensation old-center new-center angle))
         (first (car local))
         (ox (+ (car first) (car shift))) (oy (+ (cdr first) (cdr shift))))
    (excali--put element 'x (float ox))
    (excali--put element 'y (float oy))
    (excali--put element 'points
                (vconcat (mapcar (lambda (p) (vector (- (+ (car p) (car shift)) ox)
                                                     (- (+ (cdr p) (cdr shift)) oy)))
                                 local)))
    (excali--linear-extent element)
    (excali--touch element)))

(defun excali--linear-target ()
  "Return the line or arrow whose points are shown, or nil."
  (or excali--editing-linear
      (let ((single (excali--single-selection)))
        (and single (excali--linear-p single) single))))

(defun excali--segment-midpoints (element &optional all)
  "Return (INDEX . SCENE-POINT) midpoints of ELEMENT's segments.
INDEX is the position a new point would take.  Segments shorter than 40
screen px are skipped; unless ALL, only two-point elements offer one."
  (let ((points (excali--linear-scene-points element)))
    (when (or all (= (length points) 2))
      (cl-loop for (a b) on points
               for i from 1
               while b
               when (>= (* excali--zoom (sqrt (+ (expt (- (car b) (car a)) 2)
                                                (expt (- (cdr b) (cdr a)) 2))))
                        (* 4 excali--point-handle-size))
               collect (cons i (cons (/ (+ (car a) (car b)) 2.0)
                                     (/ (+ (cdr a) (cdr b)) 2.0)))))))

(defun excali--point-at (element scene-xy)
  "Return the index of ELEMENT's point under SCENE-XY, or nil."
  (let ((limit (/ (float excali--point-hit-size) excali--zoom)))
    (cl-position-if (lambda (p) (< (sqrt (+ (expt (- (car p) (car scene-xy)) 2)
                                            (expt (- (cdr p) (cdr scene-xy)) 2)))
                                   limit))
                    (excali--linear-scene-points element))))

(defun excali--midpoint-at (element scene-xy)
  "Return the midpoint (INDEX . POINT) of ELEMENT under SCENE-XY, or nil."
  (let ((limit (/ (float excali--point-hit-size) excali--zoom)))
    (cl-find-if (lambda (m) (< (sqrt (+ (expt (- (cadr m) (car scene-xy)) 2)
                                        (expt (- (cddr m) (cdr scene-xy)) 2)))
                               limit))
                (excali--segment-midpoints element (eq element excali--editing-linear)))))

;;;; Overlays

(defun excali--linear-editor-overlays ()
  "Return point and midpoint overlays for the shown line or arrow."
  (when-let* ((element (excali--linear-target)))
    (if (excali--elbow-p element)
        ;; Elbow arrows: end points and segment midpoints only.
        (excali--elbow-overlays element)
      (let* ((editing (eq element excali--editing-linear))
             (diameter (/ (* 2.0 (if editing excali--point-handle-size
                                   (/ excali--point-handle-size 2.0)))
                          excali--zoom))
             (mid (/ (* 2.0 5) excali--zoom))
             (i -1))
        (append
         (mapcar (lambda (m)
                   (excali--ov "ov-circle" (- (cadr m) (/ mid 2)) (- (cddr m) (/ mid 2)) mid mid
                              :fill "#b197fcb3"))
                 (excali--segment-midpoints element editing))
         (mapcar (lambda (p)
                   (cl-incf i)
                   (excali--ov "ov-circle" (- (car p) (/ diameter 2)) (- (cdr p) (/ diameter 2))
                              diameter diameter :stroke "#5e5ad8"
                              :fill (if (and editing (memq i excali--selected-points))
                                        "#8683e2e6" "#ffffffe6")))
                 (excali--linear-scene-points element)))))))

;;;; Editing

(defun excali-edit-linear (&optional any)
  "Edit the points of the selected line.
With ANY, or interactively with a prefix argument, arrows qualify too."
  (interactive "P")
  (let ((element (excali--single-selection)))
    (when (and element
               (not (excali--elbow-p element)) ; Elbow arrows have no point editor.
               (if any (excali--linear-p element)
                 (equal (excali--get element 'type) "line")))
      (setq excali--editing-linear element
            excali--selected-points nil)
      (excali--render))))

(defun excali-edit-linear-any ()
  "Edit the points of the selected line or arrow."
  (interactive)
  (excali-edit-linear t))

(defun excali-stop-editing-linear ()
  "Leave point-edit mode."
  (interactive)
  (setq excali--editing-linear nil
        excali--selected-points nil)
  (excali--render))

(defun excali--arrow-end-for (element index)
  "Return `start' or `end' if INDEX is an end of arrow ELEMENT, else nil."
  (when (equal (excali--get element 'type) "arrow")
    (cond ((= index 0) 'start)
          ((= index (1- (length (excali--get element 'points)))) 'end))))

(defun excali--drag-points (element indices start &optional lock-angle)
  "Drag ELEMENT's points INDICES by the mouse from scene point START.
With LOCK-ANGLE and a single point, its segment keeps a 15-degree step
from the neighbouring point.  Dragged arrow ends bind like drawing does."
  (let* ((origin (excali--linear-scene-points element))
         (ends (delq nil (mapcar (lambda (i) (cons (excali--arrow-end-for element i) i))
                                 indices)))
         (ends (seq-filter #'car ends)))
    (excali--drag-loop
     (lambda (ev)
       (let* ((p (excali--event-scene-xy ev))
              (dx (- (car p) (car start))) (dy (- (cdr p) (cdr start)))
              (points (copy-sequence origin)))
         (dolist (i indices)
           (let ((o (nth i origin)))
             (setf (nth i points)
                   (if (and lock-angle (null (cdr indices)))
                       (let* ((pivot (nth (if (> i 0) (1- i) 1) origin))
                              (d (excali--lock-angle (- (+ (car o) dx) (car pivot))
                                                    (- (+ (cdr o) dy) (cdr pivot)))))
                         (cons (+ (car pivot) (car d)) (+ (cdr pivot) (cdr d))))
                     (cons (+ (car o) dx) (+ (cdr o) dy))))))
         (excali--damage-union
          (excali--with-damage element
            (excali--set-linear-scene-points element points))
          (when ends
            (let ((old excali--binding-highlight))
              (setq excali--binding-highlight
                    (excali--binding-candidate (nth (cdar ends) points) (list element)))
              (unless (eq old excali--binding-highlight)
                (excali--elements-damage (delq nil (list old excali--binding-highlight))))))))))
    ;; Bind or release dragged arrow ends, then snap bound ends.
    (pcase-dolist (`(,end . ,i) ends)
      (let* ((point (nth i (excali--linear-scene-points element)))
             (target (excali--binding-candidate point (list element))))
        (if target
            (excali--bind-end element end target point)
          (excali--unbind-end element end))))
    (setq excali--binding-highlight nil)
    (when ends (excali--update-arrow element))
    (excali--refresh-bound-text element)))

(defun excali--linear-mouse-down (event start)
  "Handle a press at START on the shown line or arrow's points.
Return non-nil if the press was handled."
  (when-let* ((element (excali--linear-target))
              ;; Elbow arrows have their own handles: `excali--elbow-mouse-down'.
              ((not (excali--elbow-p element))))
    (let* ((mods (event-modifiers event))
           (editing (eq element excali--editing-linear))
           (index (excali--point-at element start))
           (mid (and (not index) (excali--midpoint-at element start))))
      (cond
       (index
        (cond ((and editing (memq 'shift mods))
               (setq excali--selected-points
                     (if (memq index excali--selected-points)
                         (delq index excali--selected-points)
                       (cons index excali--selected-points))))
              ((not (memq index excali--selected-points))
               (setq excali--selected-points (list index))))
        (excali--render)
        (excali--drag-points element (if editing excali--selected-points (list index))
                            start (and (not editing) (memq 'shift mods)))
        t)
       (mid
        ;; Insert a point at the midpoint and drag it.
        (let ((points (excali--linear-scene-points element)))
          (excali--set-linear-scene-points
           element (append (seq-take points (car mid)) (list (cdr mid))
                           (nthcdr (car mid) points)))
          (setq excali--selected-points (list (car mid)))
          (excali--drag-points element (list (car mid)) start))
        t)
       ((and editing (memq 'meta mods))
        (excali--set-linear-scene-points
         element (append (excali--linear-scene-points element) (list start)))
        (setq excali--selected-points
              (list (1- (length (excali--get element 'points)))))
        (excali--render)
        t)
       (editing
        ;; A press anywhere else leaves the editor and is handled normally.
        (setq excali--editing-linear nil excali--selected-points nil)
        nil)))))

(defun excali-delete-points ()
  "Delete the selected points of the edited line or arrow.
Deleting all but one point deletes the element."
  (interactive)
  (when-let* ((element excali--editing-linear)
              (selected excali--selected-points))
    (let* ((points (excali--linear-scene-points element))
           (kept (cl-loop for p in points for i from 0
                          unless (memq i selected) collect p)))
      (setq excali--selected-points nil)
      (if (< (length kept) 2)
          (progn (excali--put element 'isDeleted t)
                 (excali--touch element)
                 (setq excali--editing-linear nil)
                 (excali--deselect))
        (excali--set-linear-scene-points element kept)
        (when (equal (excali--get element 'type) "arrow")
          (excali--update-arrow element)))
      (excali--render))))

(provide 'excali-linear)
;;; excali-linear.el ends here
