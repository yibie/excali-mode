;;; excal-transform.el --- Resizing and rotating elements  -*- lexical-binding: t; -*-

;;; Commentary:

;; Resize and rotation math following Excalidraw's resizeElements.ts
;; (docs/excalidraw-spec.md §3b.5).
;;
;; A lone element is resized in its own unrotated frame: the grabbed
;; edges follow the pointer (rotated into that frame), the geometry is
;; mapped from the old box to the new one, and the result is shifted so
;; that the opposite corner stays put on screen even though the element
;; rotates about its new center.  Dragging past the opposite edge flips.
;; Several elements are scaled about the common box, uniformly when any
;; of them is rotated, text or grouped.  Rotation is absolute for a lone
;; element and relative for several; shift snaps to 15 degrees.

;;; Code:

(require 'excal-core)
(require 'excal-select)
(require 'excal-handles)
(require 'excal-text)

(declare-function excal--elbow-p "excal-elbow")
(declare-function excal--elbow-transformed "excal-elbow")

(defconst excal--shift-locking-angle (/ float-pi 12)
  "SHIFT_LOCKING_ANGLE: rotation snaps to multiples of this with shift.")

(defun excal--snapshot-geometry (element)
  "Return the geometry of ELEMENT needed to transform it from scratch."
  (list :box (excal--element-box element)
        :angle (excal--element-angle element)
        :x (excal--get element 'x) :y (excal--get element 'y)
        :width (excal--get element 'width) :height (excal--get element 'height)
        :points (mapcar #'copy-sequence (excal--get element 'points))
        :fixed (copy-tree (excal--get element 'fixedSegments) t)
        :font-size (excal--get element 'fontSize)))

(defun excal--normalize-angle (angle)
  "Return ANGLE reduced to [0, 2π)."
  (let ((a (mod angle (* 2 float-pi))))
    (if (< a 0) (+ a (* 2 float-pi)) a)))

;;;; Placing mapped geometry

(defun excal--place (element geometry map center-shift &optional angle)
  "Rebuild ELEMENT from GEOMETRY through MAP, then shift by CENTER-SHIFT.
MAP takes an unrotated scene point (X . Y) and returns its new place.
CENTER-SHIFT is added to every point afterwards; ANGLE, if non-nil,
becomes the element's rotation."
  (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) (plist-get geometry :box))
               (shift (lambda (p) (cons (+ (car p) (car center-shift))
                                        (+ (cdr p) (cdr center-shift))))))
    (pcase (excal--get element 'type)
      ((or "line" "arrow" "freedraw")
       (let* ((ox (plist-get geometry :x)) (oy (plist-get geometry :y))
              (moved (mapcar (lambda (p)
                               (funcall shift (funcall map (cons (+ ox (aref p 0))
                                                                 (+ oy (aref p 1))))))
                             (plist-get geometry :points)))
              (first (car moved)))
         (excal--put element 'x (float (car first)))
         (excal--put element 'y (float (cdr first)))
         (excal--put element 'points
                     (vconcat (mapcar (lambda (p) (vector (- (car p) (car first))
                                                          (- (cdr p) (cdr first))))
                                      moved)))
         (excal--linear-extent element)))
      (_
       (let ((a (funcall shift (funcall map (cons x1 y1))))
             (b (funcall shift (funcall map (cons x2 y2)))))
         (excal--put element 'x (float (min (car a) (car b))))
         (excal--put element 'y (float (min (cdr a) (cdr b))))
         (excal--put element 'width (float (abs (- (car b) (car a)))))
         (excal--put element 'height (float (abs (- (cdr b) (cdr a))))))))
    (when angle (excal--put element 'angle (excal--normalize-angle angle)))
    (excal--touch element)))

(defun excal--rotation-compensation (old-center new-center angle)
  "Return the shift keeping a resized element in place on screen.
The element was rotated by ANGLE about OLD-CENTER; after resizing in its
unrotated frame its center is NEW-CENTER, about which it now rotates.
The shift is R(NEW-CENTER - OLD-CENTER) + OLD-CENTER - NEW-CENTER."
  (let ((rotated (excal--rotate-point new-center old-center angle)))
    (cons (- (car rotated) (car new-center))
          (- (cdr rotated) (cdr new-center)))))

;;;; Resizing a lone element

(defun excal--resized-box (box handle pointer keep-aspect from-center)
  "Return BOX with HANDLE dragged to POINTER, both in the unrotated frame.
KEEP-ASPECT preserves the width/height ratio using the larger ratio;
FROM-CENTER mirrors the change about the center.  The box may be
inverted (flipped)."
  (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) box)
               (w (- x2 x1)) (h (- y2 y1))
               (cx (/ (+ x1 x2) 2.0)) (cy (/ (+ y1 y2) 2.0))
               (px (car pointer)) (py (cdr pointer))
               (nx1 x1) (ny1 y1) (nx2 x2) (ny2 y2))
    (when (memq handle '(nw w sw)) (setq nx1 px))
    (when (memq handle '(ne e se)) (setq nx2 px))
    (when (memq handle '(nw n ne)) (setq ny1 py))
    (when (memq handle '(sw s se)) (setq ny2 py))
    (when from-center
      (when (memq handle '(nw w sw)) (setq nx2 (- (* 2 cx) nx1)))
      (when (memq handle '(ne e se)) (setq nx1 (- (* 2 cx) nx2)))
      (when (memq handle '(nw n ne)) (setq ny2 (- (* 2 cy) ny1)))
      (when (memq handle '(sw s se)) (setq ny1 (- (* 2 cy) ny2))))
    (when (and keep-aspect (> (abs w) 0) (> (abs h) 0))
      (let* ((rw (/ (- nx2 nx1) w)) (rh (/ (- ny2 ny1) h))
             (horizontal (memq handle '(e w)))
             (vertical (memq handle '(n s)))
             (ratio (cond (horizontal (abs rw)) (vertical (abs rh))
                          (t (max (abs rw) (abs rh)))))
             (nw (* w ratio (if (and (not vertical) (< rw 0)) -1 1)))
             (nh (* h ratio (if (and (not horizontal) (< rh 0)) -1 1))))
        ;; Keep the anchor: the opposite corner or edge, or the center.
        (cond
         (from-center
          (setq nx1 (- cx (/ nw 2)) nx2 (+ cx (/ nw 2))
                ny1 (- cy (/ nh 2)) ny2 (+ cy (/ nh 2))))
         (t
          (cond ((memq handle '(nw w sw)) (setq nx1 (- x2 nw)))
                ((memq handle '(ne e se)) (setq nx2 (+ x1 nw)))
                (t (setq nx1 (- cx (/ nw 2)) nx2 (+ cx (/ nw 2)))))
          (cond ((memq handle '(nw n ne)) (setq ny1 (- y2 nh)))
                ((memq handle '(sw s se)) (setq ny2 (+ y1 nh)))
                (t (setq ny1 (- cy (/ nh 2)) ny2 (+ cy (/ nh 2)))))))))
    (list nx1 ny1 nx2 ny2)))

(defun excal--box-map (from to)
  "Return a function mapping points of box FROM onto box TO."
  (pcase-let* ((`(,fx1 ,fy1 ,fx2 ,fy2) from)
               (`(,tx1 ,ty1 ,tx2 ,ty2) to)
               (fw (- fx2 fx1)) (fh (- fy2 fy1)))
    (lambda (p)
      (cons (if (/= fw 0) (+ tx1 (* (- (car p) fx1) (/ (- tx2 tx1) fw)))
              (+ (car p) (- tx1 fx1)))
            (if (/= fh 0) (+ ty1 (* (- (cdr p) fy1) (/ (- ty2 ty1) fh)))
              (+ (cdr p) (- ty1 fy1)))))))

(defun excal--resize-text (element geometry handle to)
  "Resize text ELEMENT from GEOMETRY so HANDLE's box becomes TO.
Handles with a vertical component scale the font, keeping the aspect
ratio; side handles e and w re-wrap the text to the new width.  The
anchor is the opposite corner or edge.  Scaling always starts from the
snapshot, so a drag does not compound."
  (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) (plist-get geometry :box))
               (`(,tx1 ,ty1 ,tx2 ,ty2) to)
               (h (- y2 y1)))
    (if (memq handle '(e w))
        (excal--text-set-width element (abs (- tx2 tx1)))
      (excal--put element 'fontSize (plist-get geometry :font-size))
      (excal--put element 'width (plist-get geometry :width))
      (excal--put element 'height (plist-get geometry :height))
      (excal--text-scale element (if (> h 0) (abs (/ (- ty2 ty1) h)) 1.0)))
    (let* ((nw (excal--get element 'width)) (nh (excal--get element 'height))
           (nx (cond ((memq handle '(nw w sw)) (- x2 nw))
                     ((memq handle '(ne e se)) x1)
                     (t (- (/ (+ x1 x2) 2.0) (/ nw 2)))))
           (ny (cond ((memq handle '(nw n ne)) (- y2 nh))
                     ((memq handle '(sw s se)) y1)
                     (t (- (/ (+ y1 y2) 2.0) (/ nh 2)))))
           (old-center (excal--box-center (plist-get geometry :box)))
           (shift (excal--rotation-compensation
                   old-center (cons (+ nx (/ nw 2)) (+ ny (/ nh 2)))
                   (plist-get geometry :angle))))
      (excal--put element 'x (float (+ nx (car shift))))
      (excal--put element 'y (float (+ ny (cdr shift))))
      (excal--touch element))))

(defun excal--resize-single (element geometry handle pointer
                                     &optional keep-aspect from-center)
  "Resize ELEMENT, snapshotted as GEOMETRY, dragging HANDLE to POINTER.
POINTER is in scene coordinates; see `excal--resized-box' for
KEEP-ASPECT and FROM-CENTER."
  (let* ((box (plist-get geometry :box))
         (angle (plist-get geometry :angle))
         (center (excal--box-center box))
         (local (excal--rotate-point pointer center (- angle)))
         (text (equal (excal--get element 'type) "text"))
         (to (excal--resized-box box handle local (or keep-aspect text) from-center)))
    (if text
        (excal--resize-text element geometry handle to)
      (excal--place element geometry (excal--box-map box to)
                    (excal--rotation-compensation
                     center (excal--box-center to) angle)))))

;;;; Resizing several elements

(defun excal--resize-multiple (geometries box handle pointer
                                          &optional keep-aspect from-center)
  "Resize the elements in GEOMETRIES, an alist of (ELEMENT . GEOMETRY).
BOX is their common box at the start; HANDLE is dragged to POINTER.  The
scale is uniform when KEEP-ASPECT is set or any element is rotated, text
or grouped.  FROM-CENTER scales about the box center."
  (pcase-let* ((elements (mapcar #'car geometries))
               (uniform (or keep-aspect
                            (seq-some (lambda (e) (or (excal--rotated-p e)
                                                      (equal (excal--get e 'type) "text")
                                                      (> (length (excal--get e 'groupIds)) 0)))
                                      elements)))
               (to (excal--resized-box box handle pointer uniform from-center))
               (`(,x1 ,y1 ,x2 ,y2) box)
               (`(,tx1 ,ty1 ,tx2 ,ty2) to)
               (w (- x2 x1)) (h (- y2 y1))
               (sx (if (> w 0) (/ (- tx2 tx1) w) 1.0))
               (sy (if (> h 0) (/ (- ty2 ty1) h) 1.0))
               (map (excal--box-map box to))
               (elbows nil))
    (pcase-dolist (`(,element . ,geometry) geometries)
      (let* ((ebox (plist-get geometry :box))
             (angle (plist-get geometry :angle))
             (center (excal--box-center ebox))
             (new-center (funcall map center))
             (flipped (< (* sx sy) 0)))
        (if (equal (excal--get element 'type) "text")
            (progn
              (excal--put element 'fontSize (plist-get geometry :font-size))
              (excal--put element 'width (plist-get geometry :width))
              (excal--put element 'height (plist-get geometry :height))
              (excal--text-scale element (abs sx))
              (excal--put element 'x (float (- (car new-center)
                                               (/ (excal--get element 'width) 2.0))))
              (excal--put element 'y (float (- (cdr new-center)
                                               (/ (excal--get element 'height) 2.0))))
              (excal--touch element))
          ;; Scale the unrotated geometry about its own center, then move
          ;; the center; mirrored rotated elements negate their angle.
          (let ((map (lambda (p)
                       (cons (+ (car new-center) (* (- (car p) (car center)) sx))
                             (+ (cdr new-center) (* (- (cdr p) (cdr center)) sy))))))
            (excal--place element geometry map '(0 . 0) (if flipped (- angle) angle))
            (when (excal--elbow-p element)
              (push (list element geometry map) elbows))))))
    ;; Elbow arrows route once every shape is in place.
    (pcase-dolist (`(,arrow ,geometry ,map) (nreverse elbows))
      (excal--elbow-transformed arrow geometry map))))

;;;; Rotation

(defun excal--snap-angle (angle)
  "Round ANGLE to the nearest `excal--shift-locking-angle'."
  (let ((a (+ angle (/ excal--shift-locking-angle 2))))
    (- a (mod a excal--shift-locking-angle))))

(defun excal--rotate-single (element geometry pointer &optional snap)
  "Rotate ELEMENT so its rotation handle points at POINTER.
With SNAP, round to 15 degrees.  Upstream: angle = 5π/2 + atan2."
  (let* ((center (excal--box-center (plist-get geometry :box)))
         (angle (+ (* 2.5 float-pi)
                   (atan (- (cdr pointer) (cdr center))
                         (- (car pointer) (car center))))))
    (excal--put element 'angle
                (excal--normalize-angle (if snap (excal--snap-angle angle) angle)))
    (excal--touch element)))

(defun excal--rotate-multiple (geometries center start pointer &optional snap)
  "Rotate GEOMETRIES about CENTER by the angle from START to POINTER.
GEOMETRIES is an alist of (ELEMENT . GEOMETRY); SNAP rounds the delta."
  (let* ((delta (- (atan (- (cdr pointer) (cdr center)) (- (car pointer) (car center)))
                   (atan (- (cdr start) (cdr center)) (- (car start) (car center)))))
         (delta (if snap (excal--snap-angle delta) delta)))
    (pcase-dolist (`(,element . ,geometry) geometries)
      (let* ((ebox (plist-get geometry :box))
             (old (excal--box-center ebox))
             (new (excal--rotate-point old center delta)))
        (excal--place element geometry
                      (lambda (p) p)
                      (cons (- (car new) (car old)) (- (cdr new) (cdr old)))
                      (+ (plist-get geometry :angle) delta))))))

(provide 'excal-transform)
;;; excal-transform.el ends here
