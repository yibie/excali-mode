;;; excali-elbow.el --- Elbow arrows: routing and editing  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Elbow arrows (`elbowed: true') have their points routed automatically
;; as horizontal and vertical segments between their ends, around the
;; shapes they are bound to.  This ports Excalidraw's
;; packages/element/src/elbowArrow.ts and heading.ts closely, function by
;; function, so that the same inputs give the same points
;; (docs/excalidraw-spec.md §2a.12):
;;
;; - `excali--elbow-update-points' is `updateElbowArrowPoints': it routes
;;   the arrow, or with fixed segments renormalizes, releases, moves a
;;   segment or drags an end (`handleSegment*', `handleEndpointDrag');
;; - `excali--elbow-data' is `getElbowArrowData' (headings, obstacle boxes,
;;   dynamic AABBs, dongles), `excali--elbow-route' `routeElbowArrow', and
;;   `excali--elbow-astar' the A* search over the dynamic grid, with the
;;   same binary heap so that ties break the same way;
;; - the elbow parts of binding.ts: headings for bound ends
;;   (`getHeadingForElbowArrowSnap'), snapping ends to outlines
;;   (`bindPointToSnapToElementOutline', midpoint snapping,
;;   `avoidRectangularCorner') and fixed points
;;   (`calculateFixedPointForElbowArrowBinding').
;;
;; Points are [X Y] float vectors, bounds [X1 Y1 X2 Y2] vectors and
;; headings the symbols `up', `right', `down' and `left'.  Upstream
;; mutates point arrays in places; where that matters the port shares the
;; same vectors.
;;
;; The rest is the editor side (§3b.2, §3b.5–3b.7): a selected elbow
;; arrow shows only its end points and a midpoint per segment; dragging a
;; midpoint fixes and moves that segment, double-clicking it releases it;
;; dragging an end re-routes and binds like upstream's
;; `bindingStrategyForElbowArrowEndpointDragging'.
;;
;; Approximations: outlines used for snapping and intersection ignore
;; rounded corners (rectangles and diamonds are grown as polygons,
;; ellipses exactly), diamond edge midpoints are the vertices rather than
;; the midpoints of their rounded-corner curves, and the distance to an
;; element is `excali--outline-distance'.

;;; Code:

(require 'cl-lib)
(require 'excali-core)
(require 'excali-view)
(require 'excali-select)
(require 'excali-handles)
(require 'excali-binding)

(defconst excali--elbow-base-padding 40 "BASE_PADDING.")
(defconst excali--elbow-dedup-threshold 1 "DEDUP_TRESHOLD.")
(defconst excali--elbow-max-pos 1e6 "MAX_POS.")
(defconst excali--elbow-precision 1e-4 "PRECISION of @excalidraw/math.")
(defconst excali--fixed-point-bound 10 "FIXED_POINT_BOUND.")

(defun excali--elbow-p (element)
  "Return non-nil if ELEMENT is an elbow arrow."
  (and element (equal (excali--get element 'type) "arrow")
       (excali--get element 'elbowed) t))

;;;; Points and vectors

(defsubst excali--ep (x y)
  "Return the point [X Y] with float coordinates."
  (vector (float x) (float y)))

(defsubst excali--ex (p) "Return P's x." (aref p 0))
(defsubst excali--ey (p) "Return P's y." (aref p 1))

(defun excali--ep-equal (a b)
  "Return non-nil if points A and B are equal."
  (and (= (aref a 0) (aref b 0)) (= (aref a 1) (aref b 1))))

(defun excali--ep-distance (a b)
  "Return the distance between points A and B."
  (sqrt (+ (expt (- (aref a 0) (aref b 0)) 2) (expt (- (aref a 1) (aref b 1)) 2))))

(defun excali--ep-distance-sq (a b)
  "Return the squared distance between points A and B."
  (+ (expt (- (aref a 0) (aref b 0)) 2) (expt (- (aref a 1) (aref b 1)) 2)))

(defun excali--ep-rotate (p center angle)
  "Rotate point P about CENTER by ANGLE radians (`pointRotateRads')."
  (if (zerop angle)
      (excali--ep (aref p 0) (aref p 1))
    (let* ((dx (- (aref p 0) (aref center 0))) (dy (- (aref p 1) (aref center 1)))
           (c (cos angle)) (s (sin angle)))
      (excali--ep (+ (- (* dx c) (* dy s)) (aref center 0))
                 (+ (* dx s) (* dy c) (aref center 1))))))

(defun excali--ev-cross (a b)
  "Return the cross product of vectors A and B (`vectorCross')."
  (- (* (aref a 0) (aref b 1)) (* (aref b 0) (aref a 1))))

(defun excali--ev-from (p o)
  "Return the vector from O to P (`vectorFromPoint')."
  (excali--ep (- (aref p 0) (aref o 0)) (- (aref p 1) (aref o 1))))

(defun excali--ep-scale-from (p mid factor)
  "Return P scaled about MID by FACTOR (`pointScaleFromOrigin')."
  (excali--ep (+ (aref mid 0) (* (- (aref p 0) (aref mid 0)) factor))
             (+ (aref mid 1) (* (- (aref p 1) (aref mid 1)) factor))))

(defun excali--clamp (value low high)
  "Return VALUE clamped to LOW..HIGH, as upstream `clamp'."
  (float (min (max value low) high)))

(defun excali--m-dist (a b)
  "Return the Manhattan distance between A and B (`m_dist')."
  (+ (abs (- (aref a 0) (aref b 0))) (abs (- (aref a 1) (aref b 1)))))

;;;; Headings (heading.ts)

(defun excali--vector-to-heading (x y)
  "Return the heading of the vector X, Y (`vectorToHeading')."
  (let ((ax (abs x)) (ay (abs y)))
    (cond ((> x ay) 'right)
          ((<= x (- ay)) 'left)
          ((> y ax) 'down)
          (t 'up))))

(defun excali--heading-for-point (p o)
  "Return the heading from O to P (`headingForPoint')."
  (excali--vector-to-heading (- (aref p 0) (aref o 0)) (- (aref p 1) (aref o 1))))

(defun excali--heading-horizontal-p (heading)
  "Return non-nil if HEADING is `left' or `right'."
  (memq heading '(left right)))

(defun excali--heading-for-point-horizontal-p (p o)
  "Return non-nil if the heading from O to P is horizontal."
  (excali--heading-horizontal-p (excali--heading-for-point p o)))

(defun excali--flip-heading (heading)
  "Return the heading opposite HEADING."
  (pcase heading ('up 'down) ('down 'up) ('left 'right) (_ 'left)))

(defun excali--triangle-includes-p (a b c p)
  "Return non-nil if P lies in triangle A B C (`triangleIncludesPoint')."
  (let* ((sign (lambda (p1 p2 p3)
                 (- (* (- (aref p1 0) (aref p3 0)) (- (aref p2 1) (aref p3 1)))
                    (* (- (aref p2 0) (aref p3 0)) (- (aref p1 1) (aref p3 1))))))
         (d1 (funcall sign p a b))
         (d2 (funcall sign p b c))
         (d3 (funcall sign p c a))
         (neg (or (< d1 0) (< d2 0) (< d3 0)))
         (pos (or (> d1 0) (> d2 0) (> d3 0))))
    (not (and neg pos))))

;;;; Bindable element geometry

(defun excali--el-x (e) "Return E's x." (float (excali--get e 'x)))
(defun excali--el-y (e) "Return E's y." (float (excali--get e 'y)))
(defun excali--el-w (e) "Return E's width." (float (or (excali--get e 'width) 0)))
(defun excali--el-h (e) "Return E's height." (float (or (excali--get e 'height) 0)))

(defun excali--el-center (e)
  "Return the center of bindable element E (`elementCenterPoint')."
  (excali--ep (+ (excali--el-x e) (/ (excali--el-w e) 2))
             (+ (excali--el-y e) (/ (excali--el-h e) 2))))

(defun excali--aabb-for-element (e &optional offset)
  "Return E's rotated axis-aligned box, grown by OFFSET (`aabbForElement').
OFFSET is (TOP RIGHT DOWN LEFT)."
  (let* ((x (excali--el-x e)) (y (excali--el-y e))
         (x2 (+ x (excali--el-w e))) (y2 (+ y (excali--el-h e)))
         (center (excali--el-center e))
         (angle (excali--element-angle e))
         (corners (mapcar (lambda (p) (excali--ep-rotate p center angle))
                          (list (excali--ep x y) (excali--ep x2 y)
                                (excali--ep x2 y2) (excali--ep x y2))))
         (xs (mapcar #'excali--ex corners)) (ys (mapcar #'excali--ey corners))
         (bounds (vector (apply #'min xs) (apply #'min ys)
                         (apply #'max xs) (apply #'max ys))))
    (if offset
        (pcase-let ((`(,top ,right ,down ,left) offset))
          (vector (- (aref bounds 0) left) (- (aref bounds 1) top)
                  (+ (aref bounds 2) right) (+ (aref bounds 3) down)))
      bounds)))

(defun excali--bounds-center (b)
  "Return the center of bounds B (`getCenterForBounds')."
  (excali--ep (+ (aref b 0) (/ (- (aref b 2) (aref b 0)) 2))
             (+ (aref b 1) (/ (- (aref b 3) (aref b 1)) 2))))

(defun excali--point-inside-bounds-p (p b)
  "Return non-nil if P lies strictly inside bounds B (`pointInsideBounds')."
  (and (> (aref p 0) (aref b 0)) (< (aref p 0) (aref b 2))
       (> (aref p 1) (aref b 1)) (< (aref p 1) (aref b 3))))

(defun excali--elbow-binding-reach ()
  "Return `maxBindingDistance_simple' at the default zoom.
Updates through `mutateElement' carry no zoom, so upstream routes with
the zoom-1 binding distance."
  (let ((excali--zoom 1.0)) (excali--max-binding-distance)))

(defun excali--distance-to-element (e p)
  "Return the distance from point P to E's outline (`distanceToElement')."
  (car (excali--outline-distance e (cons (aref p 0) (aref p 1)))))

(defun excali--rectanguloid-p (e)
  "Return non-nil if E is rectangle-like (`isRectanguloidElement')."
  (not (member (excali--get e 'type) '("ellipse" "diamond"))))

(defun excali--elbow-outline (e gap)
  "Return E's outline grown by GAP as an unrotated polygon, or nil for ellipses."
  (let* ((x1 (excali--el-x e)) (y1 (excali--el-y e))
         (x2 (+ x1 (excali--el-w e))) (y2 (+ y1 (excali--el-h e)))
         (cx (/ (+ x1 x2) 2)) (cy (/ (+ y1 y2) 2))
         (a (/ (- x2 x1) 2)) (b (/ (- y2 y1) 2)))
    (pcase (excali--get e 'type)
      ("ellipse" nil)
      ("diamond"
       (let* ((edge (sqrt (+ (* a a) (* b b))))
              (a2 (+ a (if (> b 0) (/ (* gap edge) b) gap)))
              (b2 (+ b (if (> a 0) (/ (* gap edge) a) gap))))
         (list (excali--ep cx (- cy b2)) (excali--ep (+ cx a2) cy)
               (excali--ep cx (+ cy b2)) (excali--ep (- cx a2) cy))))
      (_ (list (excali--ep (- x1 gap) (- y1 gap)) (excali--ep (+ x2 gap) (- y1 gap))
               (excali--ep (+ x2 gap) (+ y2 gap)) (excali--ep (- x1 gap) (+ y2 gap)))))))

(defun excali--intersect-element-segment (e a b gap)
  "Return where segment A-B crosses E's outline grown by GAP.
This is `intersectElementWithLineSegment'; see the commentary for how
outlines are approximated."
  (let* ((center (excali--el-center e))
         (angle (excali--element-angle e))
         (la (excali--ep-rotate a center (- angle)))
         (lb (excali--ep-rotate b center (- angle)))
         (outline (excali--elbow-outline e gap))
         (hits nil))
    (if outline
        (cl-loop for (p . rest) on outline
                 for q = (or (car rest) (car outline))
                 do (when-let* ((hit (excali--segment-intersection
                                      (cons (aref la 0) (aref la 1))
                                      (cons (aref lb 0) (aref lb 1))
                                      (cons (aref p 0) (aref p 1))
                                      (cons (aref q 0) (aref q 1)))))
                      (push (excali--ep (car hit) (cdr hit)) hits)))
      ;; Ellipse with semi-axes grown by GAP.
      (let* ((rx (+ (/ (excali--el-w e) 2) gap)) (ry (+ (/ (excali--el-h e) 2) gap))
             (ox (- (aref la 0) (aref center 0))) (oy (- (aref la 1) (aref center 1)))
             (dx (- (aref lb 0) (aref la 0))) (dy (- (aref lb 1) (aref la 1)))
             (qa (+ (/ (* dx dx) (* rx rx)) (/ (* dy dy) (* ry ry))))
             (qb (* 2 (+ (/ (* ox dx) (* rx rx)) (/ (* oy dy) (* ry ry)))))
             (qc (- (+ (/ (* ox ox) (* rx rx)) (/ (* oy oy) (* ry ry))) 1))
             (disc (- (* qb qb) (* 4 qa qc))))
        (when (and (> qa 0) (>= disc 0))
          (dolist (tt (delete-dups (list (/ (- (- qb) (sqrt disc)) (* 2 qa))
                                         (/ (+ (- qb) (sqrt disc)) (* 2 qa)))))
            (when (<= 0 tt 1)
              (push (excali--ep (+ (aref la 0) (* tt dx)) (+ (aref la 1) (* tt dy))) hits))))))
    (mapcar (lambda (p) (excali--ep-rotate p center angle)) (nreverse hits))))

(defun excali--normalize-fixed-point (fixed)
  "Return FIXED clamped and kept off exactly 0.5 (`normalizeFixedPoint')."
  (if (not (and (sequencep fixed) (= (length fixed) 2)
                (seq-every-p (lambda (v) (and (numberp v) (not (isnan (float v)))
                                              (< (abs v) 1.0e+INF)))
                             fixed)))
      (vector 0.5001 0.5001)
    (let ((clamped (mapcar (lambda (r) (excali--clamp (float r) (- excali--fixed-point-bound)
                                                     excali--fixed-point-bound))
                           fixed)))
      (vconcat (if (seq-some (lambda (r) (< (abs (- r 0.5)) 0.0001)) clamped)
                   (mapcar (lambda (r) (if (< (abs (- r 0.5)) 0.0001) 0.5001 r)) clamped)
                 clamped)))))

(defun excali--elbow-global-fixed-point (fixed e)
  "Return the scene point for ratios FIXED of E's box.
This is `getGlobalFixedPointForBindableElement'."
  (let ((f (excali--normalize-fixed-point fixed)))
    (excali--ep-rotate (excali--ep (+ (excali--el-x e) (* (excali--el-w e) (aref f 0)))
                                 (+ (excali--el-y e) (* (excali--el-h e) (aref f 1))))
                      (excali--el-center e) (excali--element-angle e))))

;;;; Headings of bound ends

(defun excali--heading-from-diamond (e aabb p)
  "Return the heading of P off diamond E (`headingForPointFromDiamondElement')."
  (let* ((mid (excali--bounds-center aabb))
         (x (excali--el-x e)) (y (excali--el-y e))
         (w (excali--el-w e)) (h (excali--el-h e))
         (angle (excali--element-angle e))
         (shrink 0.95)
         (corner (lambda (cx cy)
                   (excali--ep-scale-from (excali--ep-rotate (excali--ep cx cy) mid angle)
                                         mid shrink)))
         (top (funcall corner (+ x (/ w 2)) y))
         (right (funcall corner (+ x w) (+ y (/ h 2))))
         (bottom (funcall corner (+ x (/ w 2)) (+ y h)))
         (left (funcall corner x (+ y (/ h 2))))
         (cross (lambda (a o b ob)
                  (excali--ev-cross (excali--ev-from a o) (excali--ev-from b ob)))))
    (cond
     ;; Corners
     ((and (<= (funcall cross p top top right) 0) (> (funcall cross p top top left) 0))
      (excali--heading-for-point top mid))
     ((and (<= (funcall cross p right right bottom) 0)
           (> (funcall cross p right right top) 0))
      (excali--heading-for-point right mid))
     ((and (<= (funcall cross p bottom bottom left) 0)
           (> (funcall cross p bottom bottom right) 0))
      (excali--heading-for-point bottom mid))
     ((and (<= (funcall cross p left left top) 0) (> (funcall cross p left left bottom) 0))
      (excali--heading-for-point left mid))
     ;; Sides
     ((and (<= (funcall cross p mid top mid) 0) (> (funcall cross p mid right mid) 0))
      (excali--heading-for-point (if (> w h) top right) mid))
     ((and (<= (funcall cross p mid right mid) 0) (> (funcall cross p mid bottom mid) 0))
      (excali--heading-for-point (if (> w h) bottom right) mid))
     ((and (<= (funcall cross p mid bottom mid) 0) (> (funcall cross p mid left mid) 0))
      (excali--heading-for-point (if (> w h) bottom left) mid))
     (t (excali--heading-for-point (if (> w h) top left) mid)))))

(defun excali--heading-from-element (e aabb p)
  "Return the side of E that P lies off (`headingForPointFromElement').
AABB is E's box; P is tested against four search cones from its center."
  (if (equal (excali--get e 'type) "diamond")
      (excali--heading-from-diamond e aabb p)
    (let* ((mid (excali--bounds-center aabb))
           (scale (lambda (x y) (excali--ep-scale-from (excali--ep x y) mid 2)))
           (tl (funcall scale (aref aabb 0) (aref aabb 1)))
           (tr (funcall scale (aref aabb 2) (aref aabb 1)))
           (bl (funcall scale (aref aabb 0) (aref aabb 3)))
           (br (funcall scale (aref aabb 2) (aref aabb 3))))
      (cond ((excali--triangle-includes-p tl tr mid p) 'up)
            ((excali--triangle-includes-p tr br mid p) 'right)
            ((excali--triangle-includes-p br bl mid p) 'down)
            (t 'left)))))

(defun excali--heading-for-elbow-snap (p other e aabb orig)
  "Return the heading of end P toward OTHER, bound to E with box AABB.
ORIG is the unsnapped end.  This is `getHeadingForElbowArrowSnap'."
  (if (not (and e aabb))
      (excali--vector-to-heading (- (aref other 0) (aref p 0)) (- (aref other 1) (aref p 1)))
    (let ((distance (excali--distance-to-element e orig)))
      ;; Upstream tests `!distance', so a distance of 0 counts as none.
      (if (or (> distance (excali--elbow-binding-reach)) (zerop distance))
          (let ((c (excali--el-center e)))
            (excali--vector-to-heading (- (aref p 0) (aref c 0)) (- (aref p 1) (aref c 1))))
        (excali--heading-from-element e aabb p)))))

(defun excali--bind-point-heading (p other e orig)
  "Return the heading of end P toward OTHER bound to E (`getBindPointHeading').
ORIG is the unsnapped end."
  (excali--heading-for-elbow-snap
   p other e
   (and e (let ((d (excali--distance-to-element e p)))
            (excali--aabb-for-element e (list d d d d))))
   orig))

;;;; Snapping ends to outlines

(defun excali--all-midpoints (e)
  "Return E's edge midpoints: right, bottom, left, top (`getAllMidpoints').
Diamonds use their vertices."
  (let ((x (excali--el-x e)) (y (excali--el-y e)) (w (excali--el-w e)) (h (excali--el-h e))
        (center (excali--el-center e)) (angle (excali--element-angle e)))
    (mapcar (lambda (p) (excali--ep-rotate (excali--ep (+ x (car p)) (+ y (cdr p))) center angle))
            (list (cons w (/ h 2)) (cons (/ w 2) h) (cons 0 (/ h 2)) (cons (/ w 2) 0)))))

(defun excali--elbow-snap-midpoint (point e)
  "Return (POINT . ON-AXIS) for POINT snapped to a midpoint of E, or nil.
This is `getElbowArrowSnapMidPoint' with `getSnappedMidpointForElbowArrow'."
  (let* ((x (excali--el-x e)) (y (excali--el-y e)) (w (excali--el-w e)) (h (excali--el-h e))
         (angle (excali--element-angle e))
         (center (excali--el-center e))
         (max-distance (+ (excali--elbow-binding-reach)
                          (/ (float (or (excali--get e 'strokeWidth) 1)) 2)))
         (hth (excali--clamp (* 0.05 w) 5 max-distance))
         (vth (excali--clamp (* 0.05 h) 5 max-distance))
         (nr (excali--ep-rotate point center (- angle)))
         (gap (excali--binding-gap e))
         (nx (aref nr 0)) (ny (aref nr 1))
         (cx (aref center 0)) (cy (aref center 1)))
    (unless (< (excali--ep-distance center nr) gap)
      (pcase-let ((`(,right ,bottom ,left ,top) (excali--all-midpoints e)))
        (cond
         ((and (<= nx (+ x (/ w 2))) (> ny (- cy vth)) (< ny (+ cy vth))) (cons left t))
         ((and (<= ny (+ y (/ h 2))) (> nx (- cx hth)) (< nx (+ cx hth))) (cons top t))
         ((and (>= nx (+ x (/ w 2))) (> ny (- cy vth)) (< ny (+ cy vth))) (cons right t))
         ((and (>= ny (+ y (/ h 2))) (> nx (- cx hth)) (< nx (+ cx hth))) (cons bottom t))
         ((equal (excali--get e 'type) "diamond")
          (let ((threshold (max hth vth)))
            (cl-loop for (ex ey dx dy) in (list (list (+ x (/ w 4)) (+ y (/ h 4)) -1 -1)
                                                (list (+ x (/ (* 3 w) 4)) (+ y (/ h 4)) 1 -1)
                                                (list (+ x (/ w 4)) (+ y (/ (* 3 h) 4)) -1 1)
                                                (list (+ x (/ (* 3 w) 4)) (+ y (/ (* 3 h) 4)) 1 1))
                     when (< (excali--ep-distance (excali--ep (+ ex (* dx gap)) (+ ey (* dy gap))) nr)
                             threshold)
                     return (cons (excali--ep-rotate (excali--ep ex ey) center angle) nil)))))))))

(defun excali--avoid-rectangular-corner (e p)
  "Move P off E's corners onto the nearest side (`avoidRectangularCorner')."
  (let* ((center (excali--el-center e))
         (angle (excali--element-angle e))
         (nr (excali--ep-rotate p center (- angle)))
         (x (excali--el-x e)) (y (excali--el-y e)) (w (excali--el-w e)) (h (excali--el-h e))
         (gap (excali--binding-gap e))
         (rot (lambda (px py) (excali--ep-rotate (excali--ep px py) center angle)))
         (nx (aref nr 0)) (ny (aref nr 1)))
    (cond
     ((and (< nx x) (< ny y))           ; Top left
      (if (> (- ny y) (- gap)) (funcall rot (- x gap) y) (funcall rot x (- y gap))))
     ((and (< nx x) (> ny (+ y h)))     ; Bottom left
      (if (> (- nx x) (- gap)) (funcall rot x (+ y h gap)) (funcall rot (- x gap) (+ y h))))
     ((and (> nx (+ x w)) (> ny (+ y h))) ; Bottom right
      (if (< (- nx x) (+ w gap)) (funcall rot (+ x w) (+ y h gap)) (funcall rot (+ x w gap) (+ y h))))
     ((and (> nx (+ x w)) (< ny y))     ; Top right
      (if (< (- nx x) (+ w gap)) (funcall rot (+ x w) (- y gap)) (funcall rot (+ x w gap) y)))
     (t p))))

(defun excali--ev-normalize (v)
  "Return V scaled to unit length, or [0 0] (`vectorNormalize')."
  (let ((m (sqrt (+ (expt (aref v 0) 2) (expt (aref v 1) 2)))))
    (if (zerop m) (excali--ep 0 0) (excali--ep (/ (aref v 0) m) (/ (aref v 1) m)))))

(defun excali--elbow-snap-to-outline (e point)
  "Return where an elbow arrow end at POINT lands on E's outline.
This is the elbow branch of `bindPointToSnapToElementOutline': the end
snaps to an edge midpoint when close, then is projected along E's axis
onto the outline grown by the binding gap."
  (let* ((edge (if (excali--rectanguloid-p e) (excali--avoid-rectangular-corner e point) point))
         (gap (excali--binding-gap e))
         (aabb (excali--aabb-for-element e))
         (center (excali--bounds-center aabb))
         (snap (excali--elbow-snap-midpoint edge e))
         (resolved (if snap (car snap) point))
         (horizontal (excali--heading-horizontal-p
                      (if (cdr snap)
                          (excali--heading-for-point (car snap) center)
                        (excali--heading-from-element e aabb point))))
         (reach (* 2 (max (excali--el-w e) (excali--el-h e))))
         (ray (lambda (from g)
                (let ((dir (excali--ev-normalize (excali--ev-from resolved from))))
                  (sort (excali--intersect-element-segment
                         e from (excali--ep (+ (aref from 0) (* reach (aref dir 0)))
                                           (+ (aref from 1) (* reach (aref dir 1))))
                         g)
                        (lambda (a b) (< (excali--ep-distance-sq a resolved)
                                         (excali--ep-distance-sq b resolved)))))))
         (intersection
          (or (car (funcall ray (excali--ep (if horizontal (aref center 0) (aref resolved 0))
                                           (if horizontal (aref resolved 1) (aref center 1)))
                            gap))
              (car (funcall ray (excali--ep (if horizontal (aref resolved 0) (aref center 0))
                                           (if horizontal (aref center 1) (aref resolved 1)))
                            excali--base-binding-gap)))))
    (if (or (null intersection)
            (< (excali--ep-distance-sq edge intersection) excali--elbow-precision))
        edge
      intersection)))

(defun excali--elbow-fixed-point (e point)
  "Return the fixed point binding an elbow arrow end at POINT to E.
This is `calculateFixedPointForElbowArrowBinding' with outline snapping."
  (let* ((snapped (excali--elbow-snap-to-outline e point))
         (nr (excali--ep-rotate snapped (excali--el-center e) (- (excali--element-angle e))))
         (gap (excali--binding-gap e)))
    (if (or (< (excali--el-w e) 1) (< (excali--el-h e) 1))
        (excali--normalize-fixed-point [0.5 0.5])
      (excali--normalize-fixed-point
       (vector (/ (- (aref nr 0) (excali--el-x e)) (max (excali--el-w e) gap))
               (/ (- (aref nr 1) (excali--el-y e)) (max (excali--el-h e) gap)))))))

;;;; Arrow snapshots

;; The router works on a snapshot plist of the arrow: :x :y :points (list
;; of local points), :fixed (list of segments), :start-binding
;; :end-binding (alists or nil), :start-arrowhead :end-arrowhead,
;; :start-special :end-special (t, :false or :null) and :element.

(cl-defstruct (excali--eseg (:constructor excali--eseg-create (index start end)))
  "A fixed segment: INDEX of its end point, START and END local points."
  index start end)

(defun excali--elbow-read-segments (value)
  "Return fixed segments from the JSON VALUE, as fresh `excali--eseg's."
  (when (sequencep value)
    (sort (mapcar (lambda (s)
                    (let ((start (alist-get 'start s)) (end (alist-get 'end s)))
                      (excali--eseg-create (alist-get 'index s)
                                          (excali--ep (elt start 0) (elt start 1))
                                          (excali--ep (elt end 0) (elt end 1)))))
                  value)
          (lambda (a b) (< (excali--eseg-index a) (excali--eseg-index b))))))

(defun excali--elbow-write-segments (segments)
  "Return SEGMENTS as JSON, :null when there are none."
  (if (null segments) :null
    (vconcat (mapcar (lambda (s)
                       (list (cons 'start (excali--ep (aref (excali--eseg-start s) 0)
                                                     (aref (excali--eseg-start s) 1)))
                             (cons 'end (excali--ep (aref (excali--eseg-end s) 0)
                                                   (aref (excali--eseg-end s) 1)))
                             (cons 'index (excali--eseg-index s))))
                     segments))))

(defun excali--copy-segments (segments)
  "Return fresh copies of SEGMENTS."
  (mapcar (lambda (s) (excali--eseg-create (excali--eseg-index s)
                                          (copy-sequence (excali--eseg-start s))
                                          (copy-sequence (excali--eseg-end s))))
          segments))

(defun excali--elbow-special (value)
  "Return the JSON VALUE of startIsSpecial/endIsSpecial as t, :false or :null."
  (cond ((eq value t) t) ((eq value :false) :false) (t :null)))

(defun excali--elbow-snapshot (arrow)
  "Return the router's snapshot plist of ARROW."
  (list :x (excali--el-x arrow) :y (excali--el-y arrow)
        :points (mapcar (lambda (p) (excali--ep (elt p 0) (elt p 1))) (excali--get arrow 'points))
        :fixed (excali--elbow-read-segments (excali--get arrow 'fixedSegments))
        :start-binding (excali--get arrow 'startBinding)
        :end-binding (excali--get arrow 'endBinding)
        :start-arrowhead (excali--get arrow 'startArrowhead)
        :end-arrowhead (excali--get arrow 'endArrowhead)
        :start-special (excali--elbow-special (alist-get 'startIsSpecial arrow))
        :end-special (excali--elbow-special (alist-get 'endIsSpecial arrow))
        :element arrow))

(defun excali--elbow-bound-element (binding)
  "Return the live bindable element BINDING refers to, or nil."
  (when binding
    (let ((e (excali--live-element-by-id (alist-get 'elementId binding))))
      (and e (excali--bindable-p e) e))))

;;;; Elbow arrow data

(defun excali--offset-from-heading (heading head side)
  "Return (TOP RIGHT DOWN LEFT) with HEAD on HEADING's side, SIDE elsewhere."
  (pcase heading
    ('up (list head side side side))
    ('right (list side head side side))
    ('down (list side side head side))
    (_ (list side side side head))))

(defun excali--common-aabb (boxes)
  "Return the bounds of BOXES (`commonAABB')."
  (vector (apply #'min (mapcar (lambda (b) (aref b 0)) boxes))
          (apply #'min (mapcar (lambda (b) (aref b 1)) boxes))
          (apply #'max (mapcar (lambda (b) (aref b 2)) boxes))
          (apply #'max (mapcar (lambda (b) (aref b 3)) boxes))))

(defun excali--elbow-global-point (e fixed initial dragging)
  "Return where an end at INITIAL goes (`getGlobalPoint').
E is the element it is bound to or hovers, FIXED the binding's ratios;
DRAGGING snaps to E's outline instead of using FIXED."
  (cond (dragging (if e (excali--elbow-snap-to-outline e initial) initial))
        (e (excali--elbow-global-fixed-point (or fixed [0 0]) e))
        (t initial)))

(defun excali--elbow-data (state next-points &optional dragging)
  "Return `getElbowArrowData' for STATE routed through NEXT-POINTS.
STATE is a snapshot plist (see `excali--elbow-snapshot'); only its :x
:y, bindings and arrowheads matter.  DRAGGING finds the elements under
the ends instead of using the bindings, and snaps to their outlines."
  (let* ((x (plist-get state :x)) (y (plist-get state :y))
         (first (car next-points)) (last (car (last next-points)))
         (orig-start (excali--ep (+ (aref first 0) x) (+ (aref first 1) y)))
         (orig-end (excali--ep (+ (aref last 0) x) (+ (aref last 1) y)))
         (self (plist-get state :element))
         (sb (plist-get state :start-binding)) (eb (plist-get state :end-binding))
         (hovered-start (if dragging
                            (let ((excali--zoom 1.0))
                              (excali--binding-candidate (cons (aref orig-start 0) (aref orig-start 1))
                                                        (list self)))
                          (excali--elbow-bound-element sb)))
         (hovered-end (if dragging
                          (let ((excali--zoom 1.0))
                            (excali--binding-candidate (cons (aref orig-end 0) (aref orig-end 1))
                                                      (list self)))
                        (excali--elbow-bound-element eb)))
         (start (excali--elbow-global-point hovered-start (alist-get 'fixedPoint sb)
                                           orig-start dragging))
         (end (excali--elbow-global-point hovered-end (alist-get 'fixedPoint eb)
                                         orig-end dragging))
         (start-heading (excali--bind-point-heading start end hovered-start orig-start))
         (end-heading (excali--bind-point-heading end start hovered-end orig-end))
         (point-box (lambda (p) (vector (- (aref p 0) 2) (- (aref p 1) 2)
                                        (+ (aref p 0) 2) (+ (aref p 1) 2))))
         (start-point-bounds (funcall point-box start))
         (end-point-bounds (funcall point-box end))
         (start-arrowhead (plist-get state :start-arrowhead))
         (end-arrowhead (plist-get state :end-arrowhead))
         (start-element-bounds
          (if hovered-start
              (excali--aabb-for-element
               hovered-start
               (excali--offset-from-heading
                start-heading (* (excali--binding-gap hovered-start) (if start-arrowhead 6 2)) 1))
            start-point-bounds))
         (end-element-bounds
          (if hovered-end
              (excali--aabb-for-element
               hovered-end
               (excali--offset-from-heading
                end-heading (* (excali--binding-gap hovered-end) (if end-arrowhead 6 2)) 1))
            end-point-bounds))
         (pad excali--elbow-base-padding)
         (overlap
          (or (excali--point-inside-bounds-p
               start (if hovered-end
                         (excali--aabb-for-element
                          hovered-end (excali--offset-from-heading end-heading pad pad))
                       end-point-bounds))
              (excali--point-inside-bounds-p
               end (if hovered-start
                       (excali--aabb-for-element
                        hovered-start (excali--offset-from-heading start-heading pad pad))
                     start-point-bounds))))
         (none-bound (and (not hovered-start) (not hovered-end)))
         (common (excali--common-aabb (if overlap
                                         (list start-point-bounds end-point-bounds)
                                       (list start-element-bounds end-element-bounds))))
         (difference
          (lambda (heading arrowhead)
            (if overlap
                (excali--offset-from-heading heading (if none-bound 0 pad) 0)
              (excali--offset-from-heading
               heading
               (if none-bound 0
                 (- pad (* excali--base-binding-gap (if arrowhead 6 2))))
               pad))))
         (dynamic (excali--generate-dynamic-aabbs
                   (if overlap start-point-bounds start-element-bounds)
                   (if overlap end-point-bounds end-element-bounds)
                   common
                   (funcall difference start-heading start-arrowhead)
                   (funcall difference end-heading end-arrowhead)
                   overlap
                   (and hovered-start (excali--aabb-for-element hovered-start))
                   (and hovered-end (excali--aabb-for-element hovered-end)))))
    (list :dynamic-aabbs dynamic
          :start-dongle (excali--dongle-position (nth 0 dynamic) start-heading start)
          :start start :start-heading start-heading
          :end-dongle (excali--dongle-position (nth 1 dynamic) end-heading end)
          :end end :end-heading end-heading
          :common common
          :hovered-start hovered-start :hovered-end hovered-end)))

(defun excali--generate-dynamic-aabbs (a b common start-diff end-diff
                                          &optional disable-side-hack
                                          start-el-bounds end-el-bounds)
  "Return the two obstacle boxes around A and B (`generateDynamicAABBs').
COMMON bounds both; START-DIFF and END-DIFF are (UP RIGHT DOWN LEFT)
paddings; DISABLE-SIDE-HACK skips splitting diagonal overlaps;
START-EL-BOUNDS and END-EL-BOUNDS are the bound elements' boxes."
  (let* ((se (or start-el-bounds a)) (ee (or end-el-bounds b))
         (a0 (aref a 0)) (a1 (aref a 1)) (a2 (aref a 2)) (a3 (aref a 3))
         (b0 (aref b 0)) (b1 (aref b 1)) (b2 (aref b 2)) (b3 (aref b 3)))
    (pcase-let* ((`(,start-up ,start-right ,start-down ,start-left) (or start-diff '(0 0 0 0)))
                 (`(,end-up ,end-right ,end-down ,end-left) (or end-diff '(0 0 0 0)))
                 (first
                  (vector
                   (if (> a0 b2)
                       (if (or (> a1 b3) (< a3 b1))
                           (min (/ (+ (aref se 0) (aref ee 2)) 2) (- a0 start-left))
                         (/ (+ (aref se 0) (aref ee 2)) 2))
                     (if (> a0 b0) (- a0 start-left) (- (aref common 0) start-left)))
                   (if (> a1 b3)
                       (if (or (> a0 b2) (< a2 b0))
                           (min (/ (+ (aref se 1) (aref ee 3)) 2) (- a1 start-up))
                         (/ (+ (aref se 1) (aref ee 3)) 2))
                     (if (> a1 b1) (- a1 start-up) (- (aref common 1) start-up)))
                   (if (< a2 b0)
                       (if (or (> a1 b3) (< a3 b1))
                           (max (/ (+ (aref se 2) (aref ee 0)) 2) (+ a2 start-right))
                         (/ (+ (aref se 2) (aref ee 0)) 2))
                     (if (< a2 b2) (+ a2 start-right) (+ (aref common 2) start-right)))
                   (if (< a3 b1)
                       (if (or (> a0 b2) (< a2 b0))
                           (max (/ (+ (aref se 3) (aref ee 1)) 2) (+ a3 start-down))
                         (/ (+ (aref se 3) (aref ee 1)) 2))
                     (if (< a3 b3) (+ a3 start-down) (+ (aref common 3) start-down)))))
                 (second
                  (vector
                   (if (> b0 a2)
                       (if (or (> b1 a3) (< b3 a1))
                           (min (/ (+ (aref ee 0) (aref se 2)) 2) (- b0 end-left))
                         (/ (+ (aref ee 0) (aref se 2)) 2))
                     (if (> b0 a0) (- b0 end-left) (- (aref common 0) end-left)))
                   (if (> b1 a3)
                       (if (or (> b0 a2) (< b2 a0))
                           (min (/ (+ (aref ee 1) (aref se 3)) 2) (- b1 end-up))
                         (/ (+ (aref ee 1) (aref se 3)) 2))
                     (if (> b1 a1) (- b1 end-up) (- (aref common 1) end-up)))
                   (if (< b2 a0)
                       (if (or (> b1 a3) (< b3 a1))
                           (max (/ (+ (aref ee 2) (aref se 0)) 2) (+ b2 end-right))
                         (/ (+ (aref ee 2) (aref se 0)) 2))
                     (if (< b2 a2) (+ b2 end-right) (+ (aref common 2) end-right)))
                   (if (< b3 a1)
                       (if (or (> b0 a2) (< b2 a0))
                           (max (/ (+ (aref ee 3) (aref se 1)) 2) (+ b3 end-down))
                         (/ (+ (aref ee 3) (aref se 1)) 2))
                     (if (< b3 a3) (+ b3 end-down) (+ (aref common 3) end-down)))))
                 (c (excali--common-aabb (list first second)))
                 (f0 (aref first 0)) (f1 (aref first 1)) (f2 (aref first 2)) (f3 (aref first 3))
                 (s0 (aref second 0)) (s1 (aref second 1))
                 (s2 (aref second 2)) (s3 (aref second 3)))
      (or
       (when (and (not disable-side-hack)
                  (> (+ (- f2 f0) (- s2 s0)) (+ (- (aref c 2) (aref c 0)) 0.00000000001))
                  (> (+ (- f3 f1) (- s3 s1)) (+ (- (aref c 3) (aref c 1)) 0.00000000001)))
         (let* ((ecx (/ (+ s0 s2) 2)) (ecy (/ (+ s1 s3) 2))
                (cross (lambda (x1 y1 x2 y2)
                         (excali--ev-cross (excali--ep (- x1 ecx) (- y1 ecy))
                                          (excali--ep (- x2 ecx) (- y2 ecy))))))
           (cond
            ((and (> b0 a2) (> a1 b3))  ; BOTTOM LEFT
             (let ((cx (+ f2 (/ (- s0 f2) 2))) (cy (+ s3 (/ (- f1 s3) 2))))
               (if (> (funcall cross a2 a1 a0 a3) 0)
                   (list (vector f0 f1 cx f3) (vector cx s1 s2 s3))
                 (list (vector f0 cy f2 f3) (vector s0 s1 s2 cy)))))
            ((and (< a2 b0) (< a3 b1))  ; TOP LEFT
             (let ((cx (+ f2 (/ (- s0 f2) 2))) (cy (+ f3 (/ (- s1 f3) 2))))
               (if (> (funcall cross a0 a1 a2 a3) 0)
                   (list (vector f0 f1 f2 cy) (vector s0 cy s2 s3))
                 (list (vector f0 f1 cx f3) (vector cx s1 s2 s3)))))
            ((and (> a0 b2) (< a3 b1))  ; TOP RIGHT
             (let ((cx (+ s2 (/ (- f0 s2) 2))) (cy (+ f3 (/ (- s1 f3) 2))))
               (if (> (funcall cross a2 a1 a0 a3) 0)
                   (list (vector cx f1 f2 f3) (vector s0 s1 cx s3))
                 (list (vector f0 f1 f2 cy) (vector s0 cy s2 s3)))))
            ((and (> a0 b2) (> a1 b3))  ; BOTTOM RIGHT
             (let ((cx (+ s2 (/ (- f0 s2) 2))) (cy (+ s3 (/ (- f1 s3) 2))))
               (if (> (funcall cross a0 a1 a2 a3) 0)
                   (list (vector cx f1 f2 f3) (vector s0 s1 cx s3))
                 (list (vector f0 cy f2 f3) (vector s0 s1 s2 cy))))))))
       (list first second)))))

(defun excali--dongle-position (bounds heading p)
  "Return P projected onto BOUNDS' edge in HEADING (`getDonglePosition')."
  (pcase heading
    ('up (excali--ep (aref p 0) (aref bounds 1)))
    ('right (excali--ep (aref bounds 2) (aref p 1)))
    ('down (excali--ep (aref p 0) (aref bounds 3)))
    (_ (excali--ep (aref bounds 0) (aref p 1)))))

;;;; Grid and A*

(cl-defstruct (excali--enode (:constructor excali--enode-create (pos col row)))
  "A grid node of the router."
  (f 0) (g 0) (h 0) closed visited parent pos col row)

(cl-defstruct (excali--egrid (:constructor excali--egrid-create (row col data)))
  "The router's grid: ROW by COL nodes in DATA, row-major."
  row col data)

(defun excali--calculate-grid (aabbs start start-heading end end-heading common)
  "Return the routing grid (`calculateGrid').
Lines run along the edges of AABBS and COMMON and through START and END
across their headings."
  (let (horizontal vertical)
    (if (excali--heading-horizontal-p start-heading)
        (push (aref start 1) vertical)
      (push (aref start 0) horizontal))
    (if (excali--heading-horizontal-p end-heading)
        (push (aref end 1) vertical)
      (push (aref end 0) horizontal))
    (dolist (b aabbs)
      (push (aref b 0) horizontal) (push (aref b 2) horizontal)
      (push (aref b 1) vertical) (push (aref b 3) vertical))
    (push (aref common 0) horizontal) (push (aref common 2) horizontal)
    (push (aref common 1) vertical) (push (aref common 3) vertical)
    (let* ((ys (sort (cl-remove-duplicates vertical :test #'=) #'<))
           (xs (sort (cl-remove-duplicates horizontal :test #'=) #'<))
           (data (make-vector (* (length ys) (length xs)) nil))
           (i 0))
      (cl-loop for y in ys for row from 0
               do (cl-loop for x in xs for col from 0
                           do (aset data i (excali--enode-create (excali--ep x y) col row))
                           (cl-incf i)))
      (excali--egrid-create (length ys) (length xs) data))))

(defun excali--grid-node (col row grid)
  "Return GRID's node at COL, ROW or nil (`gridNodeFromAddr')."
  (unless (or (< col 0) (>= col (excali--egrid-col grid))
              (< row 0) (>= row (excali--egrid-row grid)))
    (aref (excali--egrid-data grid) (+ (* row (excali--egrid-col grid)) col))))

(defun excali--point-to-grid-node (p grid)
  "Return GRID's node exactly at P, or nil (`pointToGridNode')."
  (cl-loop for col below (excali--egrid-col grid)
           thereis (cl-loop for row below (excali--egrid-row grid)
                            for node = (excali--grid-node col row grid)
                            when (and node (= (aref p 0) (aref (excali--enode-pos node) 0))
                                      (= (aref p 1) (aref (excali--enode-pos node) 1)))
                            return node)))

;; The binary heap of common/src/binary-heap.ts, ported as is.
(cl-defstruct (excali--eheap (:constructor excali--eheap-create ()))
  "A binary min-heap of nodes keyed on their f score."
  (content (make-vector 16 nil)) (size 0))

(defun excali--eheap-sink-down (heap idx)
  "Move the node at IDX of HEAP up while it beats its parent."
  (let* ((content (excali--eheap-content heap))
         (node (aref content idx))
         (score (excali--enode-f node)))
    (catch 'done
      (while (> idx 0)
        (let* ((parent-n (1- (ash (1+ idx) -1)))
               (parent (aref content parent-n)))
          (if (< score (excali--enode-f parent))
              (progn (aset content idx parent) (setq idx parent-n))
            (throw 'done nil)))))
    (aset content idx node)))

(defun excali--eheap-bubble-up (heap idx)
  "Move the node at IDX of HEAP down below its smaller children."
  (let* ((content (excali--eheap-content heap))
         (length (excali--eheap-size heap))
         (node (aref content idx))
         (score (excali--enode-f node)))
    (catch 'done
      (while t
        (let* ((child1 (1- (ash (1+ idx) 1)))
               (child2 (1+ child1))
               (smallest idx)
               (smallest-score score))
          (when (< child1 length)
            (let ((s (excali--enode-f (aref content child1))))
              (when (< s smallest-score)
                (setq smallest child1 smallest-score s))))
          (when (< child2 length)
            (let ((s (excali--enode-f (aref content child2))))
              (when (< s smallest-score)
                (setq smallest child2))))
          (when (= smallest idx) (throw 'done nil))
          (aset content idx (aref content smallest))
          (setq idx smallest))))
    (aset content idx node)))

(defun excali--eheap-push (heap node)
  "Add NODE to HEAP."
  (let ((size (excali--eheap-size heap)))
    (when (= size (length (excali--eheap-content heap)))
      (setf (excali--eheap-content heap)
            (vconcat (excali--eheap-content heap) (make-vector size nil))))
    (aset (excali--eheap-content heap) size node)
    (setf (excali--eheap-size heap) (1+ size))
    (excali--eheap-sink-down heap size)))

(defun excali--eheap-pop (heap)
  "Remove and return HEAP's best node, or nil."
  (let ((size (excali--eheap-size heap)))
    (unless (zerop size)
      (let* ((content (excali--eheap-content heap))
             (result (aref content 0))
             (end (aref content (1- size))))
        (aset content (1- size) nil)
        (setf (excali--eheap-size heap) (1- size))
        (when (> (1- size) 0)
          (aset content 0 end)
          (excali--eheap-bubble-up heap 0))
        result))))

(defun excali--eheap-rescore (heap node)
  "Restore HEAP's order after NODE's score dropped."
  (excali--eheap-sink-down
   heap (cl-position node (excali--eheap-content heap) :end (excali--eheap-size heap))))

(defun excali--estimate-segment-count (start end start-heading end-heading)
  "Estimate the segments between nodes START and END (`estimateSegmentCount').
START leaves in START-HEADING and END is entered in END-HEADING."
  (let ((sx (aref (excali--enode-pos start) 0)) (sy (aref (excali--enode-pos start) 1))
        (ex (aref (excali--enode-pos end) 0)) (ey (aref (excali--enode-pos end) 1)))
    (pcase end-heading
      ('right
       (pcase start-heading
         ('right (cond ((>= sx ex) 4) ((= sy ey) 0) (t 2)))
         ('up (if (and (> sy ey) (< sx ex)) 1 3))
         ('down (if (and (< sy ey) (< sx ex)) 1 3))
         ('left (if (= sy ey) 4 2))))
      ('left
       (pcase start-heading
         ('right (if (= sy ey) 4 2))
         ('up (if (and (> sy ey) (> sx ex)) 1 3))
         ('down (if (and (< sy ey) (> sx ex)) 1 3))
         ('left (cond ((<= sx ex) 4) ((= sy ey) 0) (t 2)))))
      ('up
       (pcase start-heading
         ('right (if (and (> sy ey) (< sx ex)) 1 3))
         ('up (cond ((>= sy ey) 4) ((= sx ex) 0) (t 2)))
         ('down (if (= sx ex) 4 2))
         ('left (if (and (> sy ey) (> sx ex)) 1 3))))
      ('down
       (pcase start-heading
         ('right (if (and (< sy ey) (< sx ex)) 1 3))
         ('up (if (= sx ex) 4 2))
         ('down (cond ((<= sy ey) 4) ((= sx ex) 0) (t 2)))
         ('left (if (and (< sy ey) (> sx ex)) 1 3))))
      (_ 0))))

(defun excali--elbow-astar (start end grid start-heading end-heading aabbs)
  "Return the node path from START to END through GRID, or nil (`astar').
START-HEADING and END-HEADING constrain the first and last moves; moves
whose midpoint falls in one of AABBS are forbidden.  Bends cost the cube
of the Manhattan distance between START and END."
  (let* ((bend (excali--m-dist (excali--enode-pos start) (excali--enode-pos end)))
         (bend2 (expt bend 2)) (bend3 (expt bend 3))
         (open (excali--eheap-create)))
    (excali--eheap-push open start)
    (catch 'found
      (while (> (excali--eheap-size open) 0)
        (let ((current (excali--eheap-pop open)))
          (when (and current (not (excali--enode-closed current)))
            (when (eq current end)
              (throw 'found (excali--elbow-path-to start current)))
            (setf (excali--enode-closed current) t)
            (let ((col (excali--enode-col current)) (row (excali--enode-row current))
                  (cpos (excali--enode-pos current)))
              (cl-loop
               for neighbor in (list (excali--grid-node col (1- row) grid)
                                     (excali--grid-node (1+ col) row grid)
                                     (excali--grid-node col (1+ row) grid)
                                     (excali--grid-node (1- col) row grid))
               for heading in '(up right down left)
               when (and neighbor (not (excali--enode-closed neighbor)))
               do
               (let* ((npos (excali--enode-pos neighbor))
                      (half (excali--ep-scale-from npos cpos 0.5)))
                 (unless (seq-some (lambda (b) (excali--point-inside-bounds-p half b)) aabbs)
                   (let* ((previous (if (excali--enode-parent current)
                                        (excali--heading-for-point
                                         cpos (excali--enode-pos (excali--enode-parent current)))
                                      start-heading))
                          (reverse-route
                           (or (eq (excali--flip-heading previous) heading)
                               (and (eq neighbor start) (eq heading start-heading))
                               (and (eq neighbor end) (eq heading end-heading)))))
                     (unless reverse-route
                       (let ((g (+ (excali--enode-g current) (excali--m-dist npos cpos)
                                   (if (eq previous heading) 0 bend3)))
                             (visited (excali--enode-visited neighbor)))
                         (when (or (not visited) (< g (excali--enode-g neighbor)))
                           (let ((est (excali--estimate-segment-count neighbor end heading
                                                                     end-heading)))
                             (setf (excali--enode-visited neighbor) t
                                   (excali--enode-parent neighbor) current
                                   (excali--enode-h neighbor) (+ (excali--m-dist (excali--enode-pos end) npos)
                                                                (* est bend2))
                                   (excali--enode-g neighbor) g
                                   (excali--enode-f neighbor) (+ g (excali--enode-h neighbor)))
                             (if (not visited)
                                 (excali--eheap-push open neighbor)
                               (excali--eheap-rescore open neighbor))))))))))))))
      nil)))

(defun excali--elbow-path-to (start node)
  "Return the nodes from START to NODE following parents (`pathTo')."
  (let ((path nil) (curr node))
    (while (excali--enode-parent curr)
      (push curr path)
      (setq curr (excali--enode-parent curr)))
    (cons start path)))

(defun excali--elbow-route (state data)
  "Return the global points routing STATE with DATA, or nil (`routeElbowArrow')."
  (let* ((aabbs (plist-get data :dynamic-aabbs))
         (start-dongle-pos (plist-get data :start-dongle))
         (end-dongle-pos (plist-get data :end-dongle))
         (start (plist-get data :start)) (end (plist-get data :end))
         (grid (excali--calculate-grid aabbs (or start-dongle-pos start)
                                      (plist-get data :start-heading)
                                      (or end-dongle-pos end)
                                      (plist-get data :end-heading)
                                      (plist-get data :common)))
         (start-dongle (and start-dongle-pos (excali--point-to-grid-node start-dongle-pos grid)))
         (end-dongle (and end-dongle-pos (excali--point-to-grid-node end-dongle-pos grid)))
         (end-node (excali--point-to-grid-node end grid))
         (start-node nil))
    ;; Do not allow stepping on the true end or start points.
    (when (and end-node (plist-get data :hovered-end))
      (setf (excali--enode-closed end-node) t))
    (setq start-node (excali--point-to-grid-node start grid))
    (when (and start-node (plist-get state :start-binding))
      (setf (excali--enode-closed start-node) t))
    (let* ((overlap (and start-dongle end-dongle
                         (or (excali--point-inside-bounds-p (excali--enode-pos start-dongle)
                                                           (nth 1 aabbs))
                             (excali--point-inside-bounds-p (excali--enode-pos end-dongle)
                                                           (nth 0 aabbs)))))
           (from (or start-dongle start-node))
           (to (or end-dongle end-node))
           (path (and from to
                      (excali--elbow-astar from to grid
                                          (or (plist-get data :start-heading) 'right)
                                          (or (plist-get data :end-heading) 'right)
                                          (if overlap nil aabbs)))))
      (when path
        (let ((points (mapcar (lambda (n) (copy-sequence (excali--enode-pos n))) path)))
          (when start-dongle (push start points))
          (when end-dongle (setq points (append points (list end))))
          points)))))

;;;; Post-processing

(defun excali--elbow-remove-short-segments (points)
  "Drop interior POINTS within 1 px of their predecessor.
This is `removeElbowArrowShortSegments'."
  (if (< (length points) 4)
      points
    (let ((n (length points)) (prev nil))
      (cl-loop for p in points for i from 0
               when (or (= i 0) (= i (1- n))
                        (> (excali--ep-distance prev p) excali--elbow-dedup-threshold))
               collect p
               do (setq prev p)))))

(defun excali--elbow-corner-points (points)
  "Drop interior POINTS that do not turn (`getElbowArrowCornerPoints')."
  (if (<= (length points) 1)
      points
    (let* ((vec (vconcat points))
           (n (length vec))
           (horizontal-p (lambda (a b) (< (abs (- (aref a 1) (aref b 1)))
                                          (abs (- (aref a 0) (aref b 0))))))
           (previous (funcall horizontal-p (aref vec 0) (aref vec 1))))
      (cl-loop for i below n
               for p = (aref vec i)
               when (or (= i 0) (= i (1- n))
                        (let ((next (funcall horizontal-p p (aref vec (1+ i)))))
                          (prog1 (not (eq (not previous) (not next)))
                            (setq previous next))))
               collect p))))

(defun excali--validate-elbow-points (points &optional tolerance)
  "Return non-nil if consecutive POINTS are aligned (`validateElbowPoints')."
  (let ((tolerance (or tolerance excali--elbow-dedup-threshold)))
    (cl-loop for (a b) on points
             while b
             always (or (< (abs (- (aref b 0) (aref a 0))) tolerance)
                        (< (abs (- (aref b 1) (aref a 1))) tolerance)))))

(defun excali--elbow-normalize (global fixed start-special end-special)
  "Return the element update for GLOBAL points (`normalizeArrowElementUpdate').
FIXED are the fixed segments; START-SPECIAL and END-SPECIAL are stored
as given."
  (when global
   (let* ((ox (aref (car global) 0)) (oy (aref (car global) 1))
         (clamp (lambda (v) (excali--clamp v (- excali--elbow-max-pos) excali--elbow-max-pos)))
         (points (mapcar (lambda (p) (excali--ep (funcall clamp (- (aref p 0) ox))
                                                (funcall clamp (- (aref p 1) oy))))
                         global))
         (xs (mapcar #'excali--ex points)) (ys (mapcar #'excali--ey points)))
    (list :points points
          :x (funcall clamp ox) :y (funcall clamp oy)
          :fixed (and fixed (> (length fixed) 0) fixed)
          :width (- (apply #'max xs) (apply #'min xs))
          :height (- (apply #'max ys) (apply #'min ys))
          :start-special start-special :end-special end-special))))

(defun excali--elbow-global-points (state &optional points)
  "Return STATE's POINTS (default its own) in scene coordinates."
  (let ((x (plist-get state :x)) (y (plist-get state :y)))
    (mapcar (lambda (p) (excali--ep (+ x (aref p 0)) (+ y (aref p 1))))
            (or points (plist-get state :points)))))

(defun excali--elbow-full-route (state data fixed start-special end-special)
  "Route STATE with DATA and normalize with FIXED, START-SPECIAL, END-SPECIAL."
  (excali--elbow-normalize
   (excali--elbow-corner-points
    (excali--elbow-remove-short-segments (excali--elbow-route state data)))
   fixed start-special end-special))

;;;; Updating the points (updateElbowArrowPoints)

(defun excali--elbow-update-points (state updates &optional dragging)
  "Return the update routing arrow STATE given UPDATES (`updateElbowArrowPoints').
UPDATES is a plist that may hold :points (local points: two for new
ends, or all), :fixed (fixed segments) and :start-binding /
:end-binding.  DRAGGING means the ends are being dragged: they snap to
the elements under them.  The result is a plist of :points :x :y :fixed
:width :height :start-special :end-special, some of which may be absent."
  (let* ((points (plist-get state :points))
         (n (length points)))
    (if (< n 2)
        (list :points (or (plist-get updates :points) points))
      (let* ((up-points (plist-get updates :points))
             ;; An explicit :fixed, even empty, replaces the arrow's.
             (fixed (if (plist-member updates :fixed) (plist-get updates :fixed)
                      (plist-get state :fixed)))
             (updated (cond ((null up-points) (mapcar #'copy-sequence points))
                            ((= (length up-points) 2)
                             (cl-loop for p in points for i from 0
                                      collect (cond ((= i 0) (car up-points))
                                                    ((= i (1- n)) (cadr up-points))
                                                    (t p))))
                            (t (copy-sequence up-points))))
             (start-binding (if (plist-member updates :start-binding)
                                (plist-get updates :start-binding)
                              (plist-get state :start-binding)))
             (end-binding (if (plist-member updates :end-binding)
                              (plist-get updates :end-binding)
                            (plist-get state :end-binding)))
             (start-element (excali--elbow-bound-element start-binding))
             (end-element (excali--elbow-bound-element end-binding))
             (valid (excali--validate-elbow-points updated))
             (rest-empty (not (or (plist-member updates :points)
                                  (plist-member updates :fixed)))))
        (cond
         ((or (and start-binding (not start-element) valid)
              (and end-binding (not end-element) valid)
              (and (null (excali--live-elements)) valid)
              (and rest-empty
                   (or (and start-binding (not start-element))
                       (and end-binding (not end-element)))))
          (excali--elbow-normalize (excali--elbow-global-points state updated)
                                  (plist-get state :fixed)
                                  (plist-get state :start-special)
                                  (plist-get state :end-special)))
         ;; 1. Renormalize the arrow.
         ((not (or up-points (plist-member updates :fixed)
                   (plist-get updates :start-binding) (plist-get updates :end-binding)))
          (excali--elbow-renormalize state))
         ;; Short circuit on no-op.
         ((and (plist-member updates :start-binding) (plist-member updates :end-binding)
               (eq (plist-get updates :start-binding) (plist-get state :start-binding))
               (eq (plist-get updates :end-binding) (plist-get state :end-binding))
               (cl-loop for p in up-points for i from 0
                        always (and (< i n) (excali--ep-equal p (nth i points))))
               valid)
          nil)
         (t
          (let* ((bound-state (plist-put (plist-put (copy-sequence state) :start-binding start-binding)
                                         :end-binding end-binding))
                 (data (excali--elbow-data bound-state updated dragging)))
            (cond
             ;; 2. Just normal elbow arrow things.
             ((null fixed)
              (excali--elbow-full-route state data nil :null :null))
             ;; 3. A fixed segment was released.
             ((> (length (plist-get state :fixed)) (length fixed))
              (excali--elbow-segment-release state fixed))
             ;; 4. A segment was moved.
             ((null up-points)
              (excali--elbow-segment-move state fixed data))
             ;; 5. Resize.
             ((plist-member updates :fixed)
              (list :points up-points :fixed fixed))
             ;; 6. Endpoints moved while segments are fixed.
             (t (excali--elbow-endpoint-drag state updated fixed data))))))))))

(defun excali--elbow-renormalize (state)
  "Merge collinear and drop tiny segments of STATE.
This is `handleSegmentRenormalization'."
  (let ((fixed (excali--copy-segments (plist-get state :fixed)))
        (x (plist-get state :x)) (y (plist-get state :y)))
    (if (null (plist-get state :fixed))
        (list :x x :y y :points (plist-get state :points) :fixed nil
              :start-special (plist-get state :start-special)
              :end-special (plist-get state :end-special))
      (let* ((points (vconcat (excali--elbow-global-points state)))
             (find (lambda (index) (cl-find index fixed :key #'excali--eseg-index)))
             (next1 nil))
        (dotimes (i (length points))
          (let ((p (aref points i)))
            (when (and (>= i 2)
                       (eq (excali--heading-for-point p (aref points (1- i)))
                           (excali--heading-for-point (aref points (1- i)) (aref points (- i 2)))))
              (let ((prev-seg (funcall find (1- i))) (seg (funcall find i)))
                (when seg
                  (setf (excali--eseg-start seg)
                        (excali--ep (- (aref (aref points (- i 2)) 0) x)
                                   (- (aref (aref points (- i 2)) 1) y))))
                (when prev-seg (setq fixed (delq prev-seg fixed)))
                (setq next1 (butlast next1))
                (dolist (s fixed)
                  (when (> (excali--eseg-index s) (1- i))
                    (cl-decf (excali--eseg-index s))))))
            (setq next1 (append next1 (list p)))))
        (let* ((pts (vconcat next1))
               (next2 nil))
          (dotimes (i (length pts))
            (let ((p (aref pts i)))
              (if (and (>= i 3)
                       (< (excali--ep-distance (aref pts (- i 2)) (aref pts (1- i)))
                          excali--elbow-dedup-threshold))
                  (let ((prev-prev (funcall find (- i 2))) (prev (funcall find (1- i))))
                    (when prev (setq fixed (delq prev fixed)))
                    (when prev-prev (setq fixed (delq prev-prev fixed)))
                    (setq next2 (butlast next2 2))
                    (dolist (s fixed)
                      (when (> (excali--eseg-index s) (- i 2))
                        (cl-decf (excali--eseg-index s) 2)))
                    (let ((horizontal (excali--heading-for-point-horizontal-p p (aref pts (1- i)))))
                      (setq next2 (append next2
                                          (list (excali--ep
                                                 (if horizontal (aref p 0) (aref (aref pts (- i 2)) 0))
                                                 (if horizontal (aref (aref pts (- i 2)) 1) (aref p 1))))))))
                (setq next2 (append next2 (list p))))))
          (let ((filtered (seq-remove (lambda (s) (or (= (excali--eseg-index s) 1)
                                                      (= (excali--eseg-index s) (1- (length next2)))))
                                      fixed)))
            (if (null filtered)
                (excali--elbow-full-route
                 state
                 (excali--elbow-data state (mapcar (lambda (p) (excali--ep (- (aref p 0) x)
                                                                         (- (aref p 1) y)))
                                                  next2))
                 nil :null :null)
              (excali--elbow-normalize next2 filtered (plist-get state :start-special)
                                      (plist-get state :end-special)))))))))

(defun excali--elbow-segment-release (state fixed)
  "Re-route the part of STATE freed by releasing a fixed segment.
FIXED are the remaining fixed segments.  This is `handleSegmentRelease'."
  (let* ((old (plist-get state :fixed))
         (new-indices (mapcar #'excali--eseg-index fixed))
         (deleted-pos (cl-position-if-not (lambda (s) (memq (excali--eseg-index s) new-indices)) old))
         (points (plist-get state :points))
         (n (length points))
         (ax (plist-get state :x)) (ay (plist-get state :y)))
    (if (null deleted-pos)
        (list :points points)
      (let* ((deleted-idx (excali--eseg-index (nth deleted-pos old)))
             (prev (and (> deleted-pos 0) (nth (1- deleted-pos) old)))
             (next (nth (1+ deleted-pos) old))
             (x (+ ax (if prev (aref (excali--eseg-end prev) 0) 0)))
             (y (+ ay (if prev (aref (excali--eseg-end prev) 1) 0)))
             (sub (list :x x :y y
                        :start-binding (if prev nil (plist-get state :start-binding))
                        :end-binding (if next nil (plist-get state :end-binding))
                        :start-arrowhead nil :end-arrowhead nil
                        :element (plist-get state :element)))
             (target (if next (excali--eseg-start next) (nth (1- n) points)))
             (data (excali--elbow-data
                    sub (list (excali--ep 0 0)
                              (excali--ep (- (+ ax (aref target 0)) x)
                                         (- (+ ay (aref target 1)) y)))))
             (restored (plist-get (excali--elbow-full-route state data fixed :null :null)
                                  :points))
             (next-points nil))
        (when (< (length restored) 2)
          (error "Elbow arrow: no route to restore a released segment"))
        (when prev
          (dotimes (i (excali--eseg-index prev))
            (push (excali--ep (+ ax (aref (nth i points) 0)) (+ ay (aref (nth i points) 1)))
                  next-points)))
        (dolist (p restored)
          (push (excali--ep (+ x (aref p 0)) (+ y (aref p 1))) next-points))
        (when next
          (cl-loop for i from (excali--eseg-index next) below n
                   do (push (excali--ep (+ ax (aref (nth i points) 0))
                                       (+ ay (aref (nth i points) 1)))
                            next-points)))
        (setq next-points (vconcat (nreverse next-points)))
        (let* ((diff (- (- (if next (excali--eseg-index next) n)
                           (if prev (excali--eseg-index prev) 0))
                        1))
               (next-fixed (mapcar (lambda (s)
                                     (if (> (excali--eseg-index s) deleted-idx)
                                         (excali--eseg-create
                                          (+ (- (excali--eseg-index s) diff) (1- (length restored)))
                                          (excali--eseg-start s) (excali--eseg-end s))
                                       s))
                                   fixed))
               (m (length next-points))
               (simplified nil))
          (dotimes (i m)
            (let ((p (aref next-points i))
                  (before (and (> i 0) (aref next-points (1- i))))
                  (after (and (< (1+ i) m) (aref next-points (1+ i)))))
              (if (and before after)
                  (let ((prev-heading (excali--heading-for-point p before))
                        (next-heading (excali--heading-for-point after p)))
                    (cond ((eq prev-heading next-heading)
                           (dolist (s next-fixed)
                             (when (> (excali--eseg-index s) i) (cl-decf (excali--eseg-index s)))))
                          ((eq prev-heading (excali--flip-heading next-heading))
                           (dolist (s next-fixed)
                             (when (> (excali--eseg-index s) i) (cl-incf (excali--eseg-index s))))
                           (push p simplified) (push p simplified))
                          (t (push p simplified))))
                (push p simplified))))
          (excali--elbow-normalize (nreverse simplified) next-fixed :false :false))))))

(defun excali--elbow-segment-move (state fixed data)
  "Apply a moved fixed segment of FIXED to STATE (`handleSegmentMove').
DATA gives the headings and bound elements of the ends."
  (let* ((old (plist-get state :fixed))
         (points (plist-get state :points))
         (n (length points))
         (ax (plist-get state :x)) (ay (plist-get state :y))
         (active (cl-loop for s in fixed for i from 0
                          for o = (nth i old)
                          when (or (null o) (/= (excali--eseg-index o) (excali--eseg-index s))
                                   (not (eq (and (/= (aref (excali--eseg-start s) 0)
                                                     (aref (excali--eseg-start o) 0))
                                                 (/= (aref (excali--eseg-end s) 0)
                                                     (aref (excali--eseg-end o) 0)))
                                            (and (/= (aref (excali--eseg-start s) 1)
                                                     (aref (excali--eseg-start o) 1))
                                                 (/= (aref (excali--eseg-end s) 1)
                                                     (aref (excali--eseg-end o) 1))))))
                          return i))
         (start-heading (plist-get data :start-heading))
         (end-heading (plist-get data :end-heading))
         (hovered-start (plist-get data :hovered-start))
         (hovered-end (plist-get data :hovered-end)))
    (if (null active)
        (list :points points)
      (let* ((first-fixed (cl-find 1 old :key #'excali--eseg-index))
             (last-fixed (cl-find (1- n) old :key #'excali--eseg-index))
             (seg (nth active fixed))
             (seg-length (excali--ep-distance (excali--eseg-start seg) (excali--eseg-end seg)))
             (too-short (< seg-length (+ excali--elbow-base-padding 5)))
             (padding-for
              (lambda (heading)
                (let ((positive (if (excali--heading-horizontal-p heading)
                                    (eq heading 'right) (eq heading 'down))))
                  (if positive
                      (if too-short (/ seg-length 2) excali--elbow-base-padding)
                    (if too-short (- (/ seg-length 2)) (- excali--elbow-base-padding)))))))
        ;; Special case for the first segment move.
        (when (and (null first-fixed) (= (excali--eseg-index seg) 1) hovered-start)
          (let ((horizontal (excali--heading-horizontal-p start-heading))
                (padding (funcall padding-for start-heading)))
            (setf (excali--eseg-start seg)
                  (excali--ep (+ (aref (excali--eseg-start seg) 0) (if horizontal padding 0))
                             (+ (aref (excali--eseg-start seg) 1) (if horizontal 0 padding))))))
        ;; Special case for the last segment move.
        (when (and (null last-fixed) (= (excali--eseg-index seg) (1- n)) hovered-end)
          (let ((horizontal (excali--heading-horizontal-p end-heading))
                (padding (funcall padding-for end-heading)))
            (setf (excali--eseg-end seg)
                  (excali--ep (+ (aref (excali--eseg-end seg) 0) (if horizontal padding 0))
                             (+ (aref (excali--eseg-end seg) 1) (if horizontal 0 padding))))))
        (let* ((next-fixed (mapcar (lambda (s)
                                     (excali--eseg-create
                                      (excali--eseg-index s)
                                      (excali--ep (+ ax (aref (excali--eseg-start s) 0))
                                                 (+ ay (aref (excali--eseg-start s) 1)))
                                      (excali--ep (+ ax (aref (excali--eseg-end s) 0))
                                                 (+ ay (aref (excali--eseg-end s) 1)))))
                                   fixed))
               (new-points (vconcat (excali--elbow-global-points state)))
               (moved (nth active next-fixed))
               (start-idx (1- (excali--eseg-index moved)))
               (end-idx (excali--eseg-index moved))
               (start (excali--eseg-start moved))
               (end (excali--eseg-end moved))
               (at (lambda (i) (and (>= i 0) (< i (length new-points)) (aref new-points i))))
               (prev-horizontal
                (and (funcall at (1- start-idx))
                     (not (excali--ep-equal (aref new-points start-idx)
                                           (aref new-points (1- start-idx))))
                     (list (excali--heading-for-point-horizontal-p
                            (aref new-points (1- start-idx)) (aref new-points start-idx)))))
               (next-horizontal
                (and (funcall at (1+ end-idx))
                     (not (excali--ep-equal (aref new-points end-idx)
                                           (aref new-points (1+ end-idx))))
                     (list (excali--heading-for-point-horizontal-p
                            (aref new-points (1+ end-idx)) (aref new-points end-idx))))))
          ;; Override the segment points with the actively moved segment.
          (when prev-horizontal
            (let ((dir (if (car prev-horizontal) 1 0)))
              (aset (aref new-points (1- start-idx)) dir (aref start dir))))
          (aset new-points start-idx start)
          (aset new-points end-idx end)
          (when next-horizontal
            (let ((dir (if (car next-horizontal) 1 0)))
              (aset (aref new-points (1+ end-idx)) dir (aref end dir))))
          ;; Override neighbouring fixed segments, if any.
          (when-let* ((prev (cl-find start-idx next-fixed :key #'excali--eseg-index)))
            (let ((dir (if (excali--heading-for-point-horizontal-p (excali--eseg-end prev)
                                                                  (excali--eseg-start prev))
                           1 0)))
              (aset (excali--eseg-start prev) dir (aref start dir))
              (setf (excali--eseg-end prev) start)))
          (when-let* ((next (cl-find (1+ end-idx) next-fixed :key #'excali--eseg-index)))
            (let ((dir (if (excali--heading-for-point-horizontal-p (excali--eseg-end next)
                                                                  (excali--eseg-start next))
                           1 0)))
              (aset (excali--eseg-end next) dir (aref end dir))
              (setf (excali--eseg-start next) end)))
          (let ((result (append new-points nil))
                (first-point (excali--ep (+ ax (aref (car points) 0)) (+ ay (aref (car points) 1))))
                (last-point (excali--ep (+ ax (aref (car (last points)) 0))
                                       (+ ay (aref (car (last points)) 1)))))
            ;; A first segment move needs an additional segment.
            (when (and (null first-fixed) (= start-idx 0))
              (let ((horizontal (if hovered-start
                                    (excali--heading-horizontal-p start-heading)
                                  (excali--heading-for-point-horizontal-p
                                   (aref new-points 1) (aref new-points 0)))))
                (push (excali--ep (if horizontal (aref start 0) (aref first-point 0))
                                 (if horizontal (aref first-point 1) (aref start 1)))
                      result)
                (when hovered-start (push (copy-sequence first-point) result))
                (dolist (s next-fixed)
                  (cl-incf (excali--eseg-index s) (if hovered-start 2 1)))))
            ;; A last segment move needs an additional segment.
            (when (and (null last-fixed) (= end-idx (1- n)))
              (let ((horizontal (excali--heading-horizontal-p end-heading)))
                (setq result
                      (append result
                              (list (excali--ep (if horizontal (aref end 0) (aref last-point 0))
                                               (if horizontal (aref last-point 1) (aref end 1))))
                              (and hovered-end (list (copy-sequence last-point)))))))
            (excali--elbow-normalize
             result
             (mapcar (lambda (s)
                       (excali--eseg-create (excali--eseg-index s)
                                           (excali--ep (- (aref (excali--eseg-start s) 0) ax)
                                                      (- (aref (excali--eseg-start s) 1) ay))
                                           (excali--ep (- (aref (excali--eseg-end s) 0) ax)
                                                      (- (aref (excali--eseg-end s) 1) ay))))
                     next-fixed)
             :false :false)))))))

(defun excali--elbow-endpoint-drag (state updated fixed data)
  "Move STATE's ends to UPDATED keeping FIXED segments (`handleEndpointDrag').
DATA gives the new end points, headings and bound elements."
  (let* ((start-special (eq (plist-get state :start-special) t))
         (end-special (eq (plist-get state :end-special) t))
         (ax (plist-get state :x)) (ay (plist-get state :y))
         (points (plist-get state :points))
         (m (length updated))
         (global (vconcat
                  (cl-loop for p in updated for i from 0
                           collect (if (or (= i 0) (= i (1- m)))
                                       (excali--ep (+ ax (aref p 0)) (+ ay (aref p 1)))
                                     (excali--ep (+ ax (aref (nth i points) 0))
                                                (+ ay (aref (nth i points) 1)))))))
         (at (lambda (i) (let ((i (if (< i 0) (+ (length global) i) i)))
                           (and (>= i 0) (< i (length global)) (aref global i)))))
         (next-fixed (mapcar (lambda (s) (excali--eseg-create (excali--eseg-index s) nil nil))
                             fixed))
         (start (plist-get data :start)) (end (plist-get data :end))
         (start-heading (plist-get data :start-heading))
         (end-heading (plist-get data :end-heading))
         (pad excali--elbow-base-padding)
         (new-points nil)
         (offset (+ 2 (if start-special 1 0)))
         (end-offset (+ 2 (if end-special 1 0))))
    ;; Add the inside points.
    (while (< (+ (length new-points) offset) (- (length global) end-offset))
      (setq new-points (append new-points (list (aref global (+ (length new-points) offset))))))
    ;; The moving second point, and the start point.
    (let ((second (funcall at (if start-special 2 1)))
          (third (funcall at (if start-special 3 2))))
      (unless (and second third)
        (error "Elbow arrow: second and third points must exist when dragging an end"))
      (let ((start-horizontal (excali--heading-horizontal-p start-heading))
            (second-horizontal (excali--heading-horizontal-p
                                (excali--heading-for-point second third))))
        (if (and (plist-get data :hovered-start)
                 (eq (not start-horizontal) (not second-horizontal)))
            (let* ((positive (if start-horizontal (eq start-heading 'right)
                               (eq start-heading 'down)))
                   (d (if positive pad (- pad))))
              (push (excali--ep (if second-horizontal (+ (aref start 0) d) (aref third 0))
                               (if second-horizontal (aref third 1) (+ (aref start 1) d)))
                    new-points)
              (push (excali--ep (if start-horizontal (+ (aref start 0) d) (aref start 0))
                               (if start-horizontal (aref start 1) (+ (aref start 1) d)))
                    new-points)
              (unless start-special
                (setq start-special t)
                (dolist (s next-fixed)
                  (when (> (excali--eseg-index s) 1) (cl-incf (excali--eseg-index s))))))
          (push (excali--ep (if second-horizontal (aref start 0) (aref second 0))
                           (if second-horizontal (aref second 1) (aref start 1)))
                new-points)
          (when start-special
            (setq start-special nil)
            (dolist (s next-fixed)
              (when (> (excali--eseg-index s) 1) (cl-decf (excali--eseg-index s))))))
        (push start new-points)))
    ;; The moving second to last point.
    (let* ((len (length global))
           (second-last (funcall at (- len (if end-special 3 2))))
           (third-last (funcall at (- len (if end-special 4 3)))))
      (unless (and second-last third-last)
        (error "Elbow arrow: points missing when dragging the end"))
      (let ((end-horizontal (excali--heading-horizontal-p end-heading))
            (second-horizontal (excali--heading-for-point-horizontal-p third-last second-last)))
        (if (and (plist-get data :hovered-end)
                 (eq (not end-horizontal) (not second-horizontal)))
            (let* ((positive (if end-horizontal (eq end-heading 'right) (eq end-heading 'down)))
                   (d (if positive pad (- pad))))
              (setq new-points
                    (append new-points
                            (list (excali--ep (if second-horizontal (+ (aref end 0) d)
                                               (aref third-last 0))
                                             (if second-horizontal (aref third-last 1)
                                               (+ (aref end 1) d)))
                                  (excali--ep (if end-horizontal (+ (aref end 0) d) (aref end 0))
                                             (if end-horizontal (aref end 1)
                                               (+ (aref end 1) d))))))
              (setq end-special t))
          (setq new-points
                (append new-points
                        (list (excali--ep (if second-horizontal (aref end 0) (aref second-last 0))
                                         (if second-horizontal (aref second-last 1)
                                           (aref end 1))))))
          (setq end-special nil))))
    (setq new-points (append new-points (list end)))
    (let ((pv (vconcat new-points)))
      (excali--elbow-normalize
       new-points
       (mapcar (lambda (s)
                 (let ((i (excali--eseg-index s)))
                   (excali--eseg-create
                    i
                    (excali--ep (- (aref (aref pv (1- i)) 0) (aref start 0))
                               (- (aref (aref pv (1- i)) 1) (aref start 1)))
                    (excali--ep (- (aref (aref pv i) 0) (aref start 0))
                               (- (aref (aref pv i) 1) (aref start 1))))))
               next-fixed)
       (if start-special t :false)
       (if end-special t :false)))))

;;;; Applying updates to elements

(defun excali--elbow-apply (arrow update)
  "Write the router's UPDATE plist into ARROW.
Only the keys present in UPDATE change; the arrow is never rotated."
  (when update
    (when (plist-member update :x) (excali--put arrow 'x (float (plist-get update :x))))
    (when (plist-member update :y) (excali--put arrow 'y (float (plist-get update :y))))
    (when-let* ((points (plist-get update :points)))
      (excali--put arrow 'points (vconcat (mapcar (lambda (p) (excali--ep (aref p 0) (aref p 1)))
                                                 points)))
      (excali--linear-extent arrow))
    (when (plist-member update :fixed)
      (excali--put arrow 'fixedSegments (excali--elbow-write-segments (plist-get update :fixed))))
    (when (plist-member update :start-special)
      (excali--put arrow 'startIsSpecial (plist-get update :start-special)))
    (when (plist-member update :end-special)
      (excali--put arrow 'endIsSpecial (plist-get update :end-special)))
    (excali--put arrow 'angle 0)
    (excali--touch arrow)))

(defun excali--elbow-update (arrow updates &optional dragging)
  "Update elbow ARROW for UPDATES (see `excali--elbow-update-points').
Like upstream's `mutateElement', this is how every change to an elbow
arrow's points, fixed segments or bindings goes; DRAGGING snaps the ends
to the elements under them.  Errors from inconsistent fixed segments
fall back to routing from scratch."
  (let ((excali--zoom 1.0))
    (excali--elbow-apply
     arrow
     (condition-case nil
         (excali--elbow-update-points (excali--elbow-snapshot arrow) updates dragging)
       (error
        (let* ((state (plist-put (excali--elbow-snapshot arrow) :fixed nil))
               (points (plist-get state :points))
               (ends (list (car points) (car (last points)))))
          (excali--elbow-full-route state (excali--elbow-data state ends dragging)
                                   nil :null :null)))))))

(defun excali--elbow-end-points (arrow)
  "Return ARROW's first and last local points."
  (let ((points (excali--get arrow 'points)))
    (list (excali--ep (elt (aref points 0) 0) (elt (aref points 0) 1))
          (let ((p (aref points (1- (length points))))) (excali--ep (elt p 0) (elt p 1))))))

(defun excali--elbow-reroute (arrow)
  "Re-route elbow ARROW after the shapes it is bound to changed.
This is what `updateBoundElements' amounts to for elbow arrows: the
ends move to their bindings' fixed points and the route is recomputed,
keeping fixed segments."
  (excali--elbow-update arrow (list :points (excali--elbow-end-points arrow))))

(defun excali--elbow-route-fresh (arrow &optional dragging)
  "Route elbow ARROW from scratch between its ends, dropping fixed segments."
  (excali--put arrow 'fixedSegments :null)
  (excali--put arrow 'startIsSpecial :null)
  (excali--put arrow 'endIsSpecial :null)
  (excali--elbow-update arrow (list :points (excali--elbow-end-points arrow)) dragging))

(defun excali--elbow-bind-end (arrow end element)
  "Bind ARROW's END (`start' or `end') to ELEMENT in orbit mode.
The fixed point is where the end snaps onto ELEMENT's outline."
  (let* ((point (excali--arrow-point arrow (excali--end-index arrow end)))
         (fixed (let ((excali--zoom 1.0))
                  (excali--elbow-fixed-point element (excali--ep (car point) (cdr point))))))
    (excali--unbind-end arrow end)
    (excali--put arrow (excali--binding-key end)
                (list (cons 'elementId (excali--get element 'id))
                      (cons 'fixedPoint fixed)
                      (cons 'mode "orbit")))
    (excali--add-bound-element element arrow)
    (excali--touch arrow)))

;;;; Creating

(defun excali--elbow-make (arrow)
  "Turn the new ARROW into an elbow arrow (`newArrowElement' with elbowed)."
  (excali--put arrow 'elbowed t)
  (excali--put arrow 'roundness :null)
  (excali--put arrow 'fixedSegments [])
  (excali--put arrow 'startIsSpecial :false)
  (excali--put arrow 'endIsSpecial :false)
  arrow)

(defun excali--elbow-drag-to (arrow scene-point)
  "Route the elbow ARROW being drawn to SCENE-POINT, snapping its ends."
  (let ((first (car (excali--elbow-end-points arrow))))
    (excali--elbow-update arrow
                         (list :points
                               (list first
                                     (excali--ep (- (car scene-point) (excali--get arrow 'x))
                                                (- (cdr scene-point) (excali--get arrow 'y)))))
                         t)))

(defun excali--elbow-finish-new (arrow start-target)
  "Bind the new elbow ARROW's ends and route it.
START-TARGET is the element under the start of the drawing, if any."
  (let* ((points (excali--elbow-end-points arrow))
         (x (excali--get arrow 'x)) (y (excali--get arrow 'y))
         (end-point (cons (+ x (aref (cadr points) 0)) (+ y (aref (cadr points) 1))))
         (end-target (excali--binding-candidate end-point (list arrow))))
    (when start-target (excali--elbow-bind-end arrow 'start start-target))
    (when end-target (excali--elbow-bind-end arrow 'end end-target))
    (excali--elbow-route-fresh arrow)))

;;;; Transforming (resize and flip)

(defun excali--elbow-transformed (arrow geometry map &optional rebind)
  "Finish elbow ARROW after its points were mapped from GEOMETRY by MAP.
GEOMETRY is the snapshot the transform started from (see
`excali--snapshot-geometry'); MAP takes a scene point (X . Y) to its new
place.  The fixed segments are mapped like the points, and both go to
the router, as `resizeMultipleElements' does: an arrow without fixed
segments is routed afresh, one with them keeps the mapped route.  With
REBIND each end is bound to what lies under it, or unbound, as
`bindOrUnbindLinearElements' after a flip."
  (let* ((ox (plist-get geometry :x)) (oy (plist-get geometry :y))
         (local (lambda (p)
                  (let ((q (funcall map (cons (+ ox (aref p 0)) (+ oy (aref p 1))))))
                    (excali--ep (- (car q) (excali--el-x arrow))
                               (- (cdr q) (excali--el-y arrow))))))
         (fixed (mapcar (lambda (s)
                          (excali--eseg-create (excali--eseg-index s)
                                              (funcall local (excali--eseg-start s))
                                              (funcall local (excali--eseg-end s))))
                        (excali--elbow-read-segments (plist-get geometry :fixed)))))
    (when rebind
      (dolist (end '(start end))
        (let* ((point (excali--arrow-point arrow (excali--end-index arrow end)))
               (target (excali--binding-candidate point (list arrow))))
          (if target
              (excali--elbow-bind-end arrow end target)
            (excali--unbind-end arrow end)))))
    (excali--elbow-update arrow
                         (list :points (mapcar (lambda (p) (excali--ep (elt p 0) (elt p 1)))
                                               (excali--get arrow 'points))
                               :fixed fixed))))

;;;; Converting (arrowType)

(defun excali--elbow-convert (arrow elbow)
  "Make ARROW an elbow arrow when ELBOW is non-nil, a plain arrow otherwise.
Like upstream's `changeArrowType', only the ends are kept; an elbow
arrow re-binds its ends with elbow fixed points and is routed."
  (let ((was (excali--elbow-p arrow)))
    (when (or elbow was)
      (let ((ends (excali--elbow-end-points arrow)))
        (excali--put arrow 'points (vconcat ends))
        (excali--linear-extent arrow)))
    (excali--put arrow 'elbowed (if elbow t :false))
    (cond
     (elbow
      (excali--put arrow 'roundness :null)
      (excali--put arrow 'angle 0)
      (dolist (end '(start end))
        (when-let* ((binding (excali--get arrow (excali--binding-key end)))
                    (element (excali--elbow-bound-element binding)))
          (excali--elbow-bind-end arrow end element)))
      (excali--elbow-route-fresh arrow))
     (t
      (when was
        (excali--put arrow 'fixedSegments :null)
        (excali--put arrow 'startIsSpecial :null)
        (excali--put arrow 'endIsSpecial :null)
        (excali--update-arrow arrow))))
    (excali--touch arrow)))

;;;; Editing: handles

(defconst excali--elbow-midpoint-min 5
  "POINT_HANDLE_SIZE / 2: shorter segments, in screen px, get no midpoint.")

(defun excali--elbow-scene-points (arrow)
  "Return ARROW's points as scene conses."
  (let ((x (excali--get arrow 'x)) (y (excali--get arrow 'y)))
    (mapcar (lambda (p) (cons (+ x (elt p 0)) (+ y (elt p 1))))
            (excali--get arrow 'points))))

(defun excali--elbow-midpoints (arrow)
  "Return (INDEX . SCENE-POINT) for each segment midpoint of ARROW.
INDEX is the segment's end point index; too short segments are skipped
\(`getEditorMidPoints')."
  (cl-loop for (a b) on (excali--elbow-scene-points arrow)
           for i from 1
           while b
           unless (< (* excali--zoom (sqrt (+ (expt (- (car b) (car a)) 2)
                                             (expt (- (cdr b) (cdr a)) 2))))
                     excali--elbow-midpoint-min)
           collect (cons i (cons (/ (+ (car a) (car b)) 2.0) (/ (+ (cdr a) (cdr b)) 2.0)))))

(defun excali--elbow-fixed-indices (arrow)
  "Return the indices of ARROW's fixed segments."
  (mapcar #'excali--eseg-index (excali--elbow-read-segments (excali--get arrow 'fixedSegments))))

(defun excali--elbow-midpoint-at (arrow scene-xy)
  "Return the segment index of ARROW's midpoint under SCENE-XY, or nil."
  (let ((limit (/ 11.0 excali--zoom)))
    (car (cl-find-if (lambda (m) (<= (sqrt (+ (expt (- (cadr m) (car scene-xy)) 2)
                                              (expt (- (cddr m) (cdr scene-xy)) 2)))
                                     limit))
                     (excali--elbow-midpoints arrow)))))

(defun excali--elbow-end-at (arrow scene-xy)
  "Return `start' or `end' if SCENE-XY is on that end of ARROW, else nil."
  (let* ((limit (/ 11.0 excali--zoom))
         (points (excali--elbow-scene-points arrow))
         (near (lambda (p) (< (sqrt (+ (expt (- (car p) (car scene-xy)) 2)
                                      (expt (- (cdr p) (cdr scene-xy)) 2)))
                             limit))))
    (cond ((funcall near (car points)) 'start)
          ((funcall near (car (last points))) 'end))))

(defconst excali--elbow-hover-color "#6965db66"
  "`highlightPoint' fill, rgba(105, 101, 219, 0.4).")

(defvar-local excali--elbow-hover nil
  "Scene point (X . Y) of the elbow handle under the pointer, or nil.")

(defun excali--elbow-hover-damage (point)
  "Return the damage of the hover highlight at scene POINT, or nil."
  (when point
    (let ((r (/ 10.0 excali--zoom)))
      (excali--scene-rect-damage (list (- (car point) r) (- (cdr point) r)
                                      (+ (car point) r) (+ (cdr point) r))))))

(defun excali--elbow-track-hover (xy)
  "Track the selected elbow arrow's handle under scene XY.
Its ends and segment midpoints are highlighted on hover, as upstream's
`highlightPoint'.  Return the damage of a change, or nil."
  (let* ((arrow (excali--single-selection))
         (point
          (when (and xy arrow (excali--elbow-p arrow) (not (excali--get arrow 'locked)))
            ;; The same precedence as `excali--elbow-mouse-down'.
            (let ((index (excali--elbow-midpoint-at arrow xy))
                  (end (excali--elbow-end-at arrow xy))
                  (points (excali--elbow-scene-points arrow)))
              (cond (index (cdr (assq index (excali--elbow-midpoints arrow))))
                    ((eq end 'start) (car points))
                    (end (car (last points))))))))
    (unless (equal point excali--elbow-hover)
      (prog1 (excali--damage-union (excali--elbow-hover-damage excali--elbow-hover)
                                  (excali--elbow-hover-damage point))
        (setq excali--elbow-hover point)))))

(defun excali--elbow-overlays (arrow)
  "Return the handle overlays of the selected elbow ARROW.
Only the end points get handles; each segment shows its midpoint, drawn
as a normal point when the segment is fixed and as a phantom otherwise.
The handle under the pointer gets the hover highlight."
  (let* ((diameter (/ (float excali--point-handle-size) excali--zoom))
         (mid (/ 10.0 excali--zoom))
         (fixed (excali--elbow-fixed-indices arrow))
         (points (excali--elbow-scene-points arrow)))
    (append
     (mapcar (lambda (m)
               (if (memq (car m) fixed)
                   (excali--ov "ov-circle" (- (cadr m) (/ mid 2)) (- (cddr m) (/ mid 2)) mid mid
                              :stroke "#5e5ad8" :fill "#ffffffe6")
                 (excali--ov "ov-circle" (- (cadr m) (/ mid 2)) (- (cddr m) (/ mid 2)) mid mid
                            :fill "#b197fcb3")))
             (excali--elbow-midpoints arrow))
     (mapcar (lambda (p)
               (excali--ov "ov-circle" (- (car p) (/ diameter 2)) (- (cdr p) (/ diameter 2))
                          diameter diameter :stroke "#5e5ad8" :fill "#ffffffe6"))
             (list (car points) (car (last points))))
     (when-let* ((hover excali--elbow-hover)
                 ((or (member hover (list (car points) (car (last points))))
                      (rassoc hover (excali--elbow-midpoints arrow)))))
       (let ((r (/ 10.0 excali--zoom)))
         (list (excali--ov "ov-circle" (- (car hover) r) (- (cdr hover) r) (* 2 r) (* 2 r)
                          :fill excali--elbow-hover-color)))))))

;;;; Editing: fixed segments

(defun excali--elbow-move-fixed-segment (arrow index x y)
  "Fix ARROW's segment INDEX through scene X, Y (`moveFixedSegment').
Return the index the segment has afterwards, as moving the first or last
segment adds points in front of it."
  (let* ((points (mapcar (lambda (p) (excali--ep (elt p 0) (elt p 1))) (excali--get arrow 'points)))
         (ax (excali--get arrow 'x)) (ay (excali--get arrow 'y)))
    (if (not (and index (> index 0) (< index (length points))))
        index
      (let* ((a (nth (1- index) points)) (b (nth index points))
             (horizontal (excali--heading-horizontal-p (excali--heading-for-point b a)))
             (segments (seq-remove (lambda (s) (= (excali--eseg-index s) index))
                                   (excali--elbow-read-segments (excali--get arrow 'fixedSegments))))
             (segment (excali--eseg-create
                       index
                       (excali--ep (if horizontal (aref a 0) (- x ax))
                                  (if horizontal (- y ay) (aref a 1)))
                       (excali--ep (if horizontal (aref b 0) (- x ax))
                                  (if horizontal (- y ay) (aref b 1)))))
             (next (sort (cons segment segments)
                         (lambda (s1 s2) (< (excali--eseg-index s1) (excali--eseg-index s2)))))
             (offset (cl-count-if (lambda (s) (< (excali--eseg-index s) index)) next)))
        (excali--elbow-update arrow (list :fixed next))
        (let ((after (excali--elbow-read-segments (excali--get arrow 'fixedSegments))))
          (if (nth offset after) (excali--eseg-index (nth offset after)) index))))))

(defun excali--elbow-delete-fixed-segment (arrow index)
  "Release ARROW's fixed segment INDEX so it is routed again.
Return non-nil if there was such a segment (`deleteFixedSegment')."
  (let ((segments (excali--elbow-read-segments (excali--get arrow 'fixedSegments))))
    (when (cl-find index segments :key #'excali--eseg-index)
      (excali--elbow-update arrow (list :fixed (seq-remove (lambda (s) (= (excali--eseg-index s) index))
                                                          segments)))
      t)))

;;;; Editing: mouse

(declare-function excali--drag-loop "excali-edit")
(declare-function excali--event-scene-xy "excali-view")
(declare-function excali--grid-point "excali-snap")

(defun excali--elbow-drag-segment (arrow index)
  "Drag ARROW's segment INDEX with the mouse, fixing it."
  (excali--drag-loop
   (lambda (ev)
     (let ((p (excali--grid-point (excali--event-scene-xy ev))))
       (excali--with-damage arrow
         (setq index (excali--elbow-move-fixed-segment arrow index (car p) (cdr p)))
         (excali--refresh-bound-text arrow))))))

(defun excali--elbow-drag-end (arrow end start)
  "Drag ARROW's END (`start' or `end') with the mouse from scene point START.
The route follows, snapping to shapes under the end; on release the end
binds to the shape under it or is unbound
\(`bindingStrategyForElbowArrowEndpointDragging')."
  (let* ((origin (excali--arrow-point arrow (excali--end-index arrow end)))
         (pointer origin))
    (excali--drag-loop
     (lambda (ev)
       (let* ((p (excali--event-scene-xy ev)))
         (setq pointer (excali--grid-point (cons (+ (car origin) (- (car p) (car start)))
                                                (+ (cdr origin) (- (cdr p) (cdr start))))))
         (excali--damage-union
          (excali--with-damage arrow
            (let* ((ends (excali--elbow-end-points arrow))
                   (local (excali--ep (- (car pointer) (excali--get arrow 'x))
                                     (- (cdr pointer) (excali--get arrow 'y)))))
              (excali--elbow-update arrow (list :points (if (eq end 'start)
                                                           (list local (cadr ends))
                                                         (list (car ends) local)))
                                   t))
            (excali--refresh-bound-text arrow))
          (let ((old excali--binding-highlight))
            (setq excali--binding-highlight (excali--binding-candidate pointer (list arrow)))
            (unless (eq old excali--binding-highlight)
              (excali--elements-damage (delq nil (list old excali--binding-highlight)))))))))
    (setq excali--binding-highlight nil)
    (unless (equal pointer origin)
      (let ((target (excali--binding-candidate pointer (list arrow))))
        (if target
            (excali--elbow-bind-end arrow end target)
          (excali--unbind-end arrow end)))
      (excali--elbow-reroute arrow)
      (excali--refresh-bound-text arrow))))

(defun excali--elbow-mouse-down (start)
  "Handle a press at scene point START on the lone selected elbow arrow.
A segment midpoint is dragged as a fixed segment, an end point re-routes
and rebinds; return non-nil if the press was handled."
  (when-let* ((arrow (excali--single-selection))
              ((excali--elbow-p arrow)))
    (let ((index (excali--elbow-midpoint-at arrow start))
          (end (excali--elbow-end-at arrow start)))
      ;; No hover highlight while dragging.
      (setq excali--elbow-hover nil)
      (cond (index (excali--elbow-drag-segment arrow index) t)
            (end (excali--elbow-drag-end arrow end start) t)))))

(defun excali--elbow-double-click (arrow scene-xy)
  "Release the fixed segment of ARROW whose midpoint is at SCENE-XY.
Return non-nil if a midpoint was hit."
  (when-let* ((index (excali--elbow-midpoint-at arrow scene-xy)))
    (excali--elbow-delete-fixed-segment arrow index)
    (excali--refresh-bound-text arrow)
    t))

;;;; Moving

(defun excali--elbow-movable (elements)
  "Return ELEMENTS without the elbow arrows that cannot move with them.
A lone bound elbow arrow does not move, and one bound at both ends
moves only with both of its shapes (`dragSelectedElements')."
  (if (and (null (cdr elements)) (excali--elbow-p (car elements))
           (or (excali--get (car elements) 'startBinding)
               (excali--get (car elements) 'endBinding)))
      nil
    (seq-remove (lambda (e)
                  (and (excali--elbow-p e)
                       (excali--get e 'startBinding) (excali--get e 'endBinding)
                       (not (and (memq (excali--elbow-bound-element (excali--get e 'startBinding))
                                       elements)
                                 (memq (excali--elbow-bound-element (excali--get e 'endBinding))
                                       elements)))))
                elements)))

(provide 'excali-elbow)
;;; excali-elbow.el ends here
