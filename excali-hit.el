;;; excali-hit.el --- Hit testing  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Point-on-element tests following Excalidraw's collision.ts
;; (docs/excalidraw-spec.md §3b.3): a transparent unfilled shape is hit
;; only near its stroke; filled shapes, text, images and shapes holding
;; bound text anywhere inside; lines and freedraw inside only when their
;; path is a loop; arrows only on their stroke.  The test runs in the
;; element's unrotated frame against a sampled outline.

;;; Code:

(require 'excali-core)
(require 'excali-select)
(require 'excali-handles)

(defconst excali--collision-threshold (- (* 2 4) 0.00001)
  "DEFAULT_COLLISION_THRESHOLD, screen px.")

(defconst excali--line-confirm-threshold 8
  "LINE_CONFIRM_THRESHOLD, screen px; also bounds loops in `isPathALoop'.")

(defun excali--hit-threshold (element)
  "Return the hit distance for ELEMENT in scene units."
  (max (+ (/ (float (or (excali--get element 'strokeWidth) 1)) 2) 0.1)
       (* 0.85 (/ excali--collision-threshold excali--zoom))))

;;;; Outlines

(defun excali--ellipse-outline (box &optional steps)
  "Return STEPS points (default 48) around the ellipse inscribed in BOX."
  (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) box)
               (cx (/ (+ x1 x2) 2.0)) (cy (/ (+ y1 y2) 2.0))
               (rx (/ (- x2 x1) 2.0)) (ry (/ (- y2 y1) 2.0))
               (n (or steps 48)))
    (cl-loop for i below n
             for a = (/ (* 2 float-pi i) n)
             collect (cons (+ cx (* rx (cos a))) (+ cy (* ry (sin a)))))))

(defun excali--catmull-rom (points &optional samples)
  "Return POINTS joined by a Catmull-Rom curve, SAMPLES per segment.
This is the curve roughjs draws through a rounded line's points."
  (let* ((v (vconcat points))
         (n (length v))
         (samples (or samples 8))
         (result (list (aref v 0))))
    (dotimes (i (1- n))
      (let ((p0 (aref v (max 0 (1- i)))) (p1 (aref v i))
            (p2 (aref v (1+ i))) (p3 (aref v (min (1- n) (+ i 2)))))
        (dotimes (k samples)
          (let* ((u (/ (float (1+ k)) samples))
                 (u2 (* u u)) (u3 (* u2 u)))
            (push (cons (* 0.5 (+ (* 2 (car p1)) (* (- (car p2) (car p0)) u)
                                  (* (+ (* 2 (car p0)) (* -5 (car p1)) (* 4 (car p2)) (- (car p3))) u2)
                                  (* (+ (- (car p0)) (* 3 (car p1)) (* -3 (car p2)) (car p3)) u3)))
                        (* 0.5 (+ (* 2 (cdr p1)) (* (- (cdr p2) (cdr p0)) u)
                                  (* (+ (* 2 (cdr p0)) (* -5 (cdr p1)) (* 4 (cdr p2)) (- (cdr p3))) u2)
                                  (* (+ (- (cdr p0)) (* 3 (cdr p1)) (* -3 (cdr p2)) (cdr p3)) u3))))
                  result)))))
    (nreverse result)))

(defun excali--absolute-points (element)
  "Return ELEMENT's points as absolute unrotated (X . Y) conses."
  (let ((x (excali--get element 'x)) (y (excali--get element 'y)))
    (mapcar (lambda (p) (cons (+ x (aref p 0)) (+ y (aref p 1))))
            (excali--get element 'points))))

(defun excali--outline (element)
  "Return ELEMENT's outline in its unrotated frame as (CLOSED . POINTS)."
  (let ((box (excali--element-box element)))
    (pcase (excali--get element 'type)
      ("ellipse" (cons t (excali--ellipse-outline box)))
      ("diamond"
       (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) box)
                    (cx (/ (+ x1 x2) 2.0)) (cy (/ (+ y1 y2) 2.0)))
         (cons t (list (cons cx y1) (cons x2 cy) (cons cx y2) (cons x1 cy)))))
      ((or "line" "arrow" "freedraw")
       (let ((points (excali--absolute-points element)))
         (cons nil (if (and (excali--get element 'roundness) (cddr points)
                            (not (equal (excali--get element 'type) "freedraw")))
                       (excali--catmull-rom points)
                     points))))
      (_
       (pcase-let ((`(,x1 ,y1 ,x2 ,y2) box))
         (cons t (list (cons x1 y1) (cons x2 y1) (cons x2 y2) (cons x1 y2))))))))

;;;; Tests

(defun excali--near-polyline-p (point points closed threshold)
  "Return non-nil if POINT is within THRESHOLD of the polyline POINTS.
With CLOSED, the last point joins the first."
  (let ((segments (cl-mapcar #'cons points (append (cdr points)
                                                   (and closed (list (car points)))))))
    (or (and (null (cdr points)) points
             (<= (sqrt (+ (expt (- (car point) (caar points)) 2)
                          (expt (- (cdr point) (cdar points)) 2)))
                 threshold))
        (seq-some (lambda (s) (excali--point-on-segment-p point (car s) (cdr s) threshold))
                  segments))))

(defun excali--point-in-polygon-p (point polygon)
  "Return non-nil if POINT lies inside POLYGON, by ray casting."
  (let ((inside nil)
        (px (car point)) (py (cdr point))
        (prev (car (last polygon))))
    (dolist (p polygon inside)
      (when (and (not (eq (> (cdr p) py) (> (cdr prev) py)))
                 (< px (+ (car p) (/ (* (- (car prev) (car p)) (- py (cdr p)))
                                     (- (cdr prev) (cdr p))))))
        (setq inside (not inside)))
      (setq prev p))))

(defun excali--path-loop-p (points)
  "Return non-nil if POINTS close on themselves (upstream `isPathALoop')."
  (and (>= (length points) 3)
       (let ((a (car points)) (b (car (last points))))
         (<= (sqrt (+ (expt (- (car a) (car b)) 2) (expt (- (cdr a) (cdr b)) 2)))
             (/ (float excali--line-confirm-threshold) excali--zoom)))))

(defun excali--bound-text-p (element)
  "Return non-nil if ELEMENT is text bound to a live container."
  (and (equal (excali--get element 'type) "text")
       (stringp (excali--get element 'containerId))))

(defun excali--has-bound-text-p (element)
  "Return non-nil if ELEMENT lists a bound text element."
  (seq-some (lambda (b) (equal (alist-get 'type b) "text"))
            (excali--get element 'boundElements)))

(defun excali--test-inside-p (element)
  "Return non-nil if a press inside ELEMENT hits it (`shouldTestInside')."
  (let* ((type (excali--get element 'type))
         (background (excali--get element 'backgroundColor))
         (draggable (or (and (member type '("rectangle" "ellipse" "diamond" "line"
                                            "freedraw" "stickynote" "frame"))
                             background (not (equal background "transparent")))
                        (excali--has-bound-text-p element)
                        (equal type "text"))))
    (cond ((equal type "arrow") nil)
          ((member type '("line" "freedraw"))
           (and draggable (excali--path-loop-p (excali--absolute-points element))))
          (t (or draggable (equal type "image"))))))

(defun excali--hit-element-p (element point &optional threshold)
  "Return non-nil if scene POINT hits ELEMENT within THRESHOLD."
  (let* ((threshold (or threshold (excali--hit-threshold element)))
         (box (excali--element-box element))
         (local (excali--rotate-point point (excali--box-center box)
                                     (- (excali--element-angle element))))
         (pad (+ threshold (if (equal (excali--get element 'type) "freedraw")
                               (* 2 (or (excali--get element 'strokeWidth) 1))
                             0))))
    (pcase-let ((`(,x1 ,y1 ,x2 ,y2) box))
      (and (<= (- x1 pad) (car local) (+ x2 pad))
           (<= (- y1 pad) (cdr local) (+ y2 pad))
           (let ((outline (excali--outline element)))
             (or (and (excali--test-inside-p element)
                      (or (member (excali--get element 'type) '("text" "image"))
                          (excali--point-in-polygon-p local (cdr outline))))
                 (excali--near-polyline-p local (cdr outline) (car outline) pad)))))))

(defun excali--hit-container (element)
  "Return the live container of bound text ELEMENT, or nil."
  (when-let* ((id (and (excali--bound-text-p element) (excali--get element 'containerId))))
    (cl-find-if (lambda (e) (equal (excali--get e 'id) id)) (excali--live-elements))))

(defun excali--hit (scene-xy)
  "Return the topmost element hit at SCENE-XY, or nil.
Bound text counts as its container.  When several elements are hit, the
topmost one must also pass half the threshold, as upstream does, so a
stroke right next to another element does not steal the press."
  (or (excali--hit-frame-name scene-xy)
   (let ((hits nil))
    (dolist (e (reverse (excali--live-elements)))
      ;; Locked elements cannot be picked; a press on them selects by box.
      (when (and (not (excali--get e 'locked))
                 (excali--hit-element-p e scene-xy))
        (push (or (excali--hit-container e) e) hits)))
    (setq hits (delete-dups (nreverse hits)))
    (if (and (cdr hits)
             (not (excali--hit-element-p (car hits) scene-xy
                                        (/ (excali--hit-threshold (car hits)) 2))))
        (cadr hits)
      (car hits)))))

(declare-function excali--frame-name-bounds "excali-frame-render")

(defun excali--hit-frame-name (scene-xy)
  "Return the topmost unlocked frame whose name label is at SCENE-XY."
  (when (fboundp 'excali--frame-name-bounds)
    (let ((tolerance (/ (excali--hit-threshold '((strokeWidth . 1))) 1.0)))
      (cl-find-if (lambda (e)
                    (and (member (excali--get e 'type) '("frame" "magicframe"))
                         (not (excali--get e 'locked))
                         (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excali--frame-name-bounds e)))
                           (and (<= (- x1 tolerance) (car scene-xy) (+ x2 tolerance))
                                (<= (- y1 tolerance) (cdr scene-xy) (+ y2 tolerance))))))
                  (reverse (excali--live-elements))))))

(provide 'excali-hit)
;;; excali-hit.el ends here
