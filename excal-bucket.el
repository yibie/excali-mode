;;; excal-bucket.el --- Bucket fill  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The bucket-fill tool (`b'; docs/excalidraw-spec.md, bucketFill.ts).
;; A click finds the smallest closed region around it from the element
;; outlines, bridging gaps up to `excal--bucket-gap-tolerance', and fills
;; it with a `line' element: `polygon' true, an opaque background, a
;; transparent stroke.  Islands inside the region become holes joined to
;; its outline by keyhole bridges (src/excal-fill.c does the search on a
;; raster).
;;
;; Fills carry no marker: a region matching an existing fill, within 5%
;; of its area and `excal--bucket-region-match-tolerance' of its box, is
;; recoloured instead of filled again.  A region matching a closed
;; shape's own inside gives that shape the background instead (not in
;; the spec; it keeps the shape's stroke on top of its fill).
;;
;; Pressing `b' again cycles the fill color through
;; BUCKET_FILL_BACKGROUND_PICKS; meta+click picks it from the canvas,
;; like upstream's temporary eye dropper under Alt.  The tool stays
;; active after filling.

;;; Code:

(require 'excal-core)
(require 'excal-view)
(require 'excal-select)
(require 'excal-handles)
(require 'excal-hit)
(require 'excal-style)
(require 'excal-frame)

(declare-function excal--pixel-color "excal-tools")
(declare-function excal-native-fill-region "excal-module")

(defconst excal--bucket-gap-tolerance 6 "BUCKET_FILL_GAP_TOLERANCE, screen px.")
(defconst excal--bucket-curve-max-deviation 0.5 "BUCKET_FILL_CURVE_MAX_DEVIATION.")
(defconst excal--bucket-region-match-tolerance 2 "BUCKET_FILL_REGION_MATCH_TOLERANCE.")
(defconst excal--bucket-area-tolerance 0.05 "Area difference for matching regions.")
(defconst excal--bucket-background-picks
  '("#ffffff" "#ffc9c9" "#b2f2bb" "#a5d8ff" "#ffec99")
  "BUCKET_FILL_BACKGROUND_PICKS.")
(defconst excal--bucket-max-cells 3000000 "Largest search grid, in cells.")

(defvar-local excal--bucket-color nil
  "The bucket-fill color, or nil for the current background.")

(defun excal--bucket-fill-color ()
  "Return the color the bucket fills with.
Without a chosen color, the current background, or the first
non-white pick when that is transparent."
  (or excal--bucket-color
      (let ((bg (excal--style-value 'backgroundColor)))
        (and (stringp bg) (not (equal bg "transparent")) bg))
      (cadr excal--bucket-background-picks)))

(defun excal-bucket-cycle-color ()
  "Switch the bucket fill to the next of the background picks."
  (interactive)
  (let* ((current (excal--bucket-fill-color))
         (tail (cdr (member current excal--bucket-background-picks))))
    (setq excal--bucket-color (or (car tail) (car excal--bucket-background-picks)))
    (message "Bucket fill: %s" excal--bucket-color)))

;;;; Walls

(defun excal--bucket-fill-p (element)
  "Return non-nil if ELEMENT looks like a bucket fill."
  (and (equal (excal--get element 'type) "line")
       (eq (excal--get element 'polygon) t)
       (member (excal--get element 'strokeColor) '("transparent" nil))))

(defun excal--ellipse-steps (box)
  "Return how many points keep BOX's ellipse within the curve deviation."
  (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) box)
               (r (max 1.0 (/ (max (- x2 x1) (- y2 y1)) 2.0))))
    (max 16 (min 720 (ceiling (/ float-pi (acos (max -1.0 (- 1 (/ excal--bucket-curve-max-deviation
                                                                  r))))))))))

(defun excal--bucket-outline (element)
  "Return ELEMENT's outline for the fill search as (CLOSED . POINTS).
POINTS are scene (X . Y), rotation applied."
  (let* ((box (excal--element-box element))
         (outline (if (equal (excal--get element 'type) "ellipse")
                      (cons t (excal--ellipse-outline box (excal--ellipse-steps box)))
                    (excal--outline element)))
         (center (excal--box-center box))
         (angle (excal--element-angle element))
         (points (mapcar (lambda (p) (excal--rotate-point p center angle)) (cdr outline)))
         (closed (or (car outline)
                     (and (member (excal--get element 'type) '("line" "freedraw"))
                          (excal--path-loop-p (excal--absolute-points element))))))
    (cons closed points)))

(defun excal--bucket-walls ()
  "Return (WALLS . ELEMENTS) for the fill search.
WALLS is a vector of [CLOSED X0 Y0 ...]; ELEMENTS pairs each wall with
its element, in the same order."
  (let (walls elements)
    (dolist (e (excal--live-elements))
      (unless (excal--bucket-fill-p e)
        (let ((outline (excal--bucket-outline e)))
          (when (cdr outline)
            (push (vconcat (list (car outline))
                           (apply #'append (mapcar (lambda (p) (list (float (car p))
                                                                     (float (cdr p))))
                                                   (cdr outline))))
                  walls)
            (push e elements)))))
    (cons (vconcat (nreverse walls)) (nreverse elements))))

;;;; Region

(defun excal--polygon-area (points)
  "Return the unsigned area of the polygon POINTS ((X . Y) ...)."
  (abs (/ (cl-loop for (a . rest) on points
                   for b = (or (car rest) (car points))
                   sum (- (* (car a) (cdr b)) (* (car b) (cdr a))))
          2.0)))

(defun excal--points-box (points)
  "Return the box (X1 Y1 X2 Y2) around POINTS."
  (let ((xs (mapcar #'car points)) (ys (mapcar #'cdr points)))
    (list (apply #'min xs) (apply #'min ys) (apply #'max xs) (apply #'max ys))))

(defun excal--bucket-region (point)
  "Return the region around scene POINT as a list of (X . Y), or a reason.
The reason is the symbol `on-wall' or `unbounded'."
  (let* ((walls (car (excal--bucket-walls)))
         (bounds (excal--elements-bounds (excal--live-elements)))
         (pad 20.0)
         (x1 (- (min (nth 0 bounds) (car point)) pad))
         (y1 (- (min (nth 1 bounds) (cdr point)) pad))
         (x2 (+ (max (nth 2 bounds) (car point)) pad))
         (y2 (+ (max (nth 3 bounds) (cdr point)) pad))
         ;; Half a screen px, coarser only for huge scenes.
         (cell (max (/ 0.5 excal--zoom)
                    (sqrt (/ (* (- x2 x1) (- y2 y1)) excal--bucket-max-cells))))
         (gw (ceiling (/ (- x2 x1) cell))) (gh (ceiling (/ (- y2 y1) cell)))
         (result (excal-native-fill-region
                  walls x1 y1 cell gw gh (float (car point)) (float (cdr point))
                  (/ (float excal--bucket-gap-tolerance) excal--zoom)
                  (max excal--bucket-curve-max-deviation cell))))
    (if (vectorp result)
        (cl-loop for i from 0 below (length result) by 2
                 collect (cons (aref result i) (aref result (1+ i))))
      result)))

(defun excal--bucket-matches-p (region points)
  "Return non-nil if REGION and the polygon POINTS are the same area.
Areas within 5%, boxes within the region match tolerance."
  (let ((a (excal--polygon-area region)) (b (excal--polygon-area points))
        (tol (max excal--bucket-region-match-tolerance
                  (/ (* 2.0 excal--bucket-region-match-tolerance) excal--zoom))))
    (and (> b 0)
         (<= (abs (- a b)) (* excal--bucket-area-tolerance (max a b)))
         (cl-every (lambda (u v) (<= (abs (- u v)) tol))
                   (excal--points-box region) (excal--points-box points)))))

(defun excal--bucket-closed-shape-p (element)
  "Return non-nil if ELEMENT encloses an area a fill could match."
  (or (member (excal--get element 'type) '("rectangle" "ellipse" "diamond" "stickynote"))
      (and (member (excal--get element 'type) '("line" "freedraw"))
           (car (excal--bucket-outline element)))))

(defun excal--bucket-match (region)
  "Return the fill or closed shape whose inside is REGION, or nil.
Fills are preferred, and the topmost of each."
  (let ((candidates (reverse (excal--live-elements))))
    (or (seq-find (lambda (e) (and (excal--bucket-fill-p e)
                                   (excal--bucket-matches-p region (excal--absolute-points e))))
                  candidates)
        (seq-find (lambda (e) (and (not (excal--get e 'locked))
                                   (excal--bucket-closed-shape-p e)
                                   (excal--bucket-matches-p region
                                                            (cdr (excal--bucket-outline e)))))
                  candidates))))

(defun excal--bucket-insert-position (point)
  "Return the index to insert a fill around scene POINT at.
Just above the topmost closed element enclosing POINT, so the walls of
the other elements stay above the fill; the bottom when none does."
  (let ((index 0) (i 0))
    (dolist (e excal--elements index)
      (setq i (1+ i))
      (when (and (not (excal--get e 'isDeleted))
                 (not (excal--bucket-fill-p e))
                 (let ((outline (excal--bucket-outline e)))
                   (and (car outline) (excal--point-in-polygon-p point (cdr outline)))))
        (setq index i)))))

(defun excal-bucket-fill-at (point)
  "Fill the closed region around scene POINT with the bucket color.
Return the element filled or created, or nil."
  (let ((region (excal--bucket-region point))
        (color (excal--bucket-fill-color)))
    (pcase region
      ('on-wall (message "Click inside an area to fill it") nil)
      ('unbounded (message "No closed area here") nil)
      ('nil (message "Could not find the area to fill") nil)
      (_
       (if-let* ((match (excal--bucket-match region)))
           (progn
             (excal--put match 'backgroundColor color)
             (when (and (not (excal--bucket-fill-p match))
                        (member (excal--get match 'fillStyle) '(nil "")))
               (excal--put match 'fillStyle "solid"))
             (excal--touch match)
             (excal--render)
             match)
         (let* ((origin (car region))
                (points (vconcat (mapcar (lambda (p) (vector (- (car p) (car origin))
                                                             (- (cdr p) (cdr origin))))
                                         (append region (list origin)))))
                (fill (excal--make-element
                       "line" (car origin) (cdr origin)
                       (cons 'points points)
                       (cons 'strokeColor "transparent") (cons 'backgroundColor color)
                       (cons 'fillStyle "solid") (cons 'strokeWidth 1)
                       (cons 'roughness 0) (cons 'polygon t)
                       (cons 'startBinding :null) (cons 'endBinding :null)
                       (cons 'startArrowhead :null) (cons 'endArrowhead :null)))
                (at (excal--bucket-insert-position point)))
           (excal--linear-extent fill)
           (setq excal--elements (append (seq-take excal--elements at) (list fill)
                                         (nthcdr at excal--elements)))
           (when-let* ((frame (excal--frame-at point (list fill))))
             (excal--set-frame (list fill) frame))
           (excal--render)
           fill))))))

(defun excal--bucket-click (start pick)
  "Handle a bucket-fill press at scene point START.
With PICK, take the fill color from the canvas instead."
  (if pick
      (when-let* ((color (excal--pixel-color start)))
        (setq excal--bucket-color color)
        (message "Bucket fill: %s" color))
    (excal-bucket-fill-at start)))

(provide 'excal-bucket)
;;; excal-bucket.el ends here
