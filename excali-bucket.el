;;; excali-bucket.el --- Bucket fill  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The bucket-fill tool (`b'; docs/excalidraw-spec.md, bucketFill.ts).
;; A click finds the smallest closed region around it from the element
;; outlines, bridging gaps up to `excali--bucket-gap-tolerance', and fills
;; it with a `line' element: `polygon' true, an opaque background, a
;; transparent stroke.  Islands inside the region become holes joined to
;; its outline by keyhole bridges (src/excali-fill.c does the search on a
;; raster).
;;
;; Fills carry no marker: a region matching an existing fill, within 5%
;; of its area and `excali--bucket-region-match-tolerance' of its box, is
;; recoloured instead of filled again.  A region matching a closed
;; shape's own inside gives that shape the background instead (not in
;; the spec; it keeps the shape's stroke on top of its fill).
;;
;; Pressing `b' again cycles the fill color through
;; BUCKET_FILL_BACKGROUND_PICKS; meta+click picks it from the canvas,
;; like upstream's temporary eye dropper under Alt.  The tool stays
;; active after filling.

;;; Code:

(require 'excali-core)
(require 'excali-view)
(require 'excali-select)
(require 'excali-handles)
(require 'excali-hit)
(require 'excali-style)
(require 'excali-frame)

(declare-function excali--pixel-color "excali-tools")
(declare-function excali-native-fill-region "excali-module")

(defconst excali--bucket-gap-tolerance 6 "BUCKET_FILL_GAP_TOLERANCE, screen px.")
(defconst excali--bucket-curve-max-deviation 0.5 "BUCKET_FILL_CURVE_MAX_DEVIATION.")
(defconst excali--bucket-region-match-tolerance 2 "BUCKET_FILL_REGION_MATCH_TOLERANCE.")
(defconst excali--bucket-area-tolerance 0.05 "Area difference for matching regions.")
(defconst excali--bucket-background-picks
  '("#ffffff" "#ffc9c9" "#b2f2bb" "#a5d8ff" "#ffec99")
  "BUCKET_FILL_BACKGROUND_PICKS.")
(defconst excali--bucket-max-cells 3000000 "Largest search grid, in cells.")

(defvar-local excali--bucket-color nil
  "The bucket-fill color, or nil for the current background.")

(defun excali--bucket-fill-color ()
  "Return the color the bucket fills with.
Without a chosen color, the current background, or the first
non-white pick when that is transparent."
  (or excali--bucket-color
      (let ((bg (excali--style-value 'backgroundColor)))
        (and (stringp bg) (not (equal bg "transparent")) bg))
      (cadr excali--bucket-background-picks)))

(defun excali-bucket-cycle-color ()
  "Switch the bucket fill to the next of the background picks."
  (interactive)
  (let* ((current (excali--bucket-fill-color))
         (tail (cdr (member current excali--bucket-background-picks))))
    (setq excali--bucket-color (or (car tail) (car excali--bucket-background-picks)))
    (message "Bucket fill: %s" excali--bucket-color)))

;;;; Walls

(defun excali--bucket-fill-p (element)
  "Return non-nil if ELEMENT looks like a bucket fill."
  (and (equal (excali--get element 'type) "line")
       (eq (excali--get element 'polygon) t)
       (member (excali--get element 'strokeColor) '("transparent" nil))))

(defun excali--ellipse-steps (box)
  "Return how many points keep BOX's ellipse within the curve deviation."
  (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) box)
               (r (max 1.0 (/ (max (- x2 x1) (- y2 y1)) 2.0))))
    (max 16 (min 720 (ceiling (/ float-pi (acos (max -1.0 (- 1 (/ excali--bucket-curve-max-deviation
                                                                  r))))))))))

(defun excali--bucket-outline (element)
  "Return ELEMENT's outline for the fill search as (CLOSED . POINTS).
POINTS are scene (X . Y), rotation applied."
  (let* ((box (excali--element-box element))
         (outline (if (equal (excali--get element 'type) "ellipse")
                      (cons t (excali--ellipse-outline box (excali--ellipse-steps box)))
                    (excali--outline element)))
         (center (excali--box-center box))
         (angle (excali--element-angle element))
         (points (mapcar (lambda (p) (excali--rotate-point p center angle)) (cdr outline)))
         (closed (or (car outline)
                     (and (member (excali--get element 'type) '("line" "freedraw"))
                          (excali--path-loop-p (excali--absolute-points element))))))
    (cons closed points)))

(defun excali--bucket-walls ()
  "Return (WALLS . ELEMENTS) for the fill search.
WALLS is a vector of [CLOSED X0 Y0 ...]; ELEMENTS pairs each wall with
its element, in the same order."
  (let (walls elements)
    (dolist (e (excali--live-elements))
      (unless (excali--bucket-fill-p e)
        (let ((outline (excali--bucket-outline e)))
          (when (cdr outline)
            (push (vconcat (list (car outline))
                           (apply #'append (mapcar (lambda (p) (list (float (car p))
                                                                     (float (cdr p))))
                                                   (cdr outline))))
                  walls)
            (push e elements)))))
    (cons (vconcat (nreverse walls)) (nreverse elements))))

;;;; Region

(defun excali--polygon-area (points)
  "Return the unsigned area of the polygon POINTS ((X . Y) ...)."
  (abs (/ (cl-loop for (a . rest) on points
                   for b = (or (car rest) (car points))
                   sum (- (* (car a) (cdr b)) (* (car b) (cdr a))))
          2.0)))

(defun excali--points-box (points)
  "Return the box (X1 Y1 X2 Y2) around POINTS."
  (let ((xs (mapcar #'car points)) (ys (mapcar #'cdr points)))
    (list (apply #'min xs) (apply #'min ys) (apply #'max xs) (apply #'max ys))))

(defun excali--bucket-region (point)
  "Return the region around scene POINT as a list of (X . Y), or a reason.
The reason is the symbol `on-wall' or `unbounded'."
  (let* ((walls (car (excali--bucket-walls)))
         (bounds (excali--elements-bounds (excali--live-elements)))
         (pad 20.0)
         (x1 (- (min (nth 0 bounds) (car point)) pad))
         (y1 (- (min (nth 1 bounds) (cdr point)) pad))
         (x2 (+ (max (nth 2 bounds) (car point)) pad))
         (y2 (+ (max (nth 3 bounds) (cdr point)) pad))
         ;; Half a screen px, coarser only for huge scenes.
         (cell (max (/ 0.5 excali--zoom)
                    (sqrt (/ (* (- x2 x1) (- y2 y1)) excali--bucket-max-cells))))
         (gw (ceiling (/ (- x2 x1) cell))) (gh (ceiling (/ (- y2 y1) cell)))
         (result (excali-native-fill-region
                  walls x1 y1 cell gw gh (float (car point)) (float (cdr point))
                  (/ (float excali--bucket-gap-tolerance) excali--zoom)
                  (max excali--bucket-curve-max-deviation cell))))
    (if (vectorp result)
        (cl-loop for i from 0 below (length result) by 2
                 collect (cons (aref result i) (aref result (1+ i))))
      result)))

(defun excali--bucket-matches-p (region points)
  "Return non-nil if REGION and the polygon POINTS are the same area.
Areas within 5%, boxes within the region match tolerance."
  (let ((a (excali--polygon-area region)) (b (excali--polygon-area points))
        (tol (max excali--bucket-region-match-tolerance
                  (/ (* 2.0 excali--bucket-region-match-tolerance) excali--zoom))))
    (and (> b 0)
         (<= (abs (- a b)) (* excali--bucket-area-tolerance (max a b)))
         (cl-every (lambda (u v) (<= (abs (- u v)) tol))
                   (excali--points-box region) (excali--points-box points)))))

(defun excali--bucket-closed-shape-p (element)
  "Return non-nil if ELEMENT encloses an area a fill could match."
  (or (member (excali--get element 'type) '("rectangle" "ellipse" "diamond" "stickynote"))
      (and (member (excali--get element 'type) '("line" "freedraw"))
           (car (excali--bucket-outline element)))))

(defun excali--bucket-match (region)
  "Return the fill or closed shape whose inside is REGION, or nil.
Fills are preferred, and the topmost of each."
  (let ((candidates (reverse (excali--live-elements))))
    (or (seq-find (lambda (e) (and (excali--bucket-fill-p e)
                                   (excali--bucket-matches-p region (excali--absolute-points e))))
                  candidates)
        (seq-find (lambda (e) (and (not (excali--get e 'locked))
                                   (excali--bucket-closed-shape-p e)
                                   (excali--bucket-matches-p region
                                                            (cdr (excali--bucket-outline e)))))
                  candidates))))

(defun excali--bucket-insert-position (point)
  "Return the index to insert a fill around scene POINT at.
Just above the topmost closed element enclosing POINT, so the walls of
the other elements stay above the fill; the bottom when none does."
  (let ((index 0) (i 0))
    (dolist (e excali--elements index)
      (setq i (1+ i))
      (when (and (not (excali--get e 'isDeleted))
                 (not (excali--bucket-fill-p e))
                 (let ((outline (excali--bucket-outline e)))
                   (and (car outline) (excali--point-in-polygon-p point (cdr outline)))))
        (setq index i)))))

(defun excali-bucket-fill-at (point)
  "Fill the closed region around scene POINT with the bucket color.
Return the element filled or created, or nil."
  (let ((region (excali--bucket-region point))
        (color (excali--bucket-fill-color)))
    (pcase region
      ('on-wall (message "Click inside an area to fill it") nil)
      ('unbounded (message "No closed area here") nil)
      ('nil (message "Could not find the area to fill") nil)
      (_
       (if-let* ((match (excali--bucket-match region)))
           (progn
             (excali--put match 'backgroundColor color)
             (when (and (not (excali--bucket-fill-p match))
                        (member (excali--get match 'fillStyle) '(nil "")))
               (excali--put match 'fillStyle "solid"))
             (excali--touch match)
             (excali--render)
             match)
         (let* ((origin (car region))
                (points (vconcat (mapcar (lambda (p) (vector (- (car p) (car origin))
                                                             (- (cdr p) (cdr origin))))
                                         (append region (list origin)))))
                (fill (excali--make-element
                       "line" (car origin) (cdr origin)
                       (cons 'points points)
                       (cons 'strokeColor "transparent") (cons 'backgroundColor color)
                       (cons 'fillStyle "solid") (cons 'strokeWidth 1)
                       (cons 'roughness 0) (cons 'polygon t)
                       (cons 'startBinding :null) (cons 'endBinding :null)
                       (cons 'startArrowhead :null) (cons 'endArrowhead :null)))
                (at (excali--bucket-insert-position point)))
           (excali--linear-extent fill)
           (setq excali--elements (append (seq-take excali--elements at) (list fill)
                                         (nthcdr at excali--elements)))
           (when-let* ((frame (excali--frame-at point (list fill))))
             (excali--set-frame (list fill) frame))
           (excali--render)
           fill))))))

(defun excali--bucket-click (start pick)
  "Handle a bucket-fill press at scene point START.
With PICK, take the fill color from the canvas instead."
  (if pick
      (when-let* ((color (excali--pixel-color start)))
        (setq excali--bucket-color color)
        (message "Bucket fill: %s" color))
    (excali-bucket-fill-at start)))

(provide 'excali-bucket)
;;; excali-bucket.el ends here
