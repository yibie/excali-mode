;;; excal-restore.el --- Restore and migrate loaded scenes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Port of packages/excalidraw/data/restore.ts: fills in missing element
;; fields, migrates legacy formats (strokeSharpness, boundElementIds,
;; `font', `draw' elements, focus/gap bindings, old arrowhead names),
;; repairs references between elements and fixes fractional indices.
;;
;; Elements are JSON alists as parsed by `json-parse-buffer' with
;; :null-object :null and :false-object :false.  A missing key reads as
;; `:undefined' here, so that JavaScript's `??' (null or undefined) and
;; `||' (any falsy value) can be told apart from an empty JSON object,
;; which parses to nil and is truthy in JavaScript.
;;
;; Deliberate differences from upstream:
;; - Elements of unknown types are kept untouched instead of dropped.
;; - `normalizeLink' only trims, escapes double quotes and blanks
;;   javascript:/data:/vbscript: URLs; it does not canonicalise URLs the
;;   way @braintree/sanitize-url does.
;; - Legacy bindings are migrated with a simplified point-in-shape test
;;   (rounded corners are ignored) and elbow arrows are not re-routed.
;; - A legacy text without fontSize gets its lineHeight from the
;;   restored font size rather than NaN.
;; - `refreshDimensions' (text re-measurement) is not supported.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'excal-core)
(require 'excal-index)

;;;; Constants (packages/common/src/constants.ts, font-metadata.ts)

(defconst excal--restore-font-families
  '(("Virgil" . 1) ("Helvetica" . 2) ("Cascadia" . 3) ("Excalifont" . 5)
    ("Nunito" . 6) ("Lilita One" . 7) ("Comic Shanns" . 8)
    ("Liberation Sans" . 9) ("Assistant" . 10))
  "Upstream `FONT_FAMILY': font names and ids.")

(defconst excal--restore-default-font-family 5 "Excalifont.")
(defconst excal--restore-default-font-size 20)

(defconst excal--restore-font-line-heights
  '((1 . 1.25) (2 . 1.15) (3 . 1.2) (5 . 1.25) (6 . 1.25) (7 . 1.15)
    (8 . 1.25) (9 . 1.15) (10 . 1.25) (100 . 1.25) (1000 . 1.25))
  "Default unitless line height per font id (`FONT_METADATA').")

(defconst excal--restore-stroke-widths '(("thin" . 1) ("medium" . 2) ("bold" . 4))
  "`STROKE_WIDTH' for the keys in `STROKE_WIDTH_KEYS'.")

(defconst excal--restore-max-linear-px 75000 "`MAX_LINEAR_PX' in restore.ts.")
(defconst excal--restore-sticky-note-min-size 75)
(defconst excal--restore-default-sticky-note-size 250)
(defconst excal--restore-sticky-note-max-font-size 512)
(defconst excal--restore-sticky-note-fallback-font-size 28)
(defconst excal--restore-default-sticky-note-bg "#ffdf6b")
(defconst excal--restore-base-binding-gap 5)
(defconst excal--restore-fixed-point-bound 10)

(defconst excal--restore-export-app-state-defaults
  '((gridSize . 20) (gridStep . 5) (gridModeEnabled . :false)
    (viewBackgroundColor . "#ffffff") (lockedMultiSelections))
  "The app state keys upstream writes to files, with their defaults.")

(defun excal--restore-line-height-for (font-family)
  "Return the default line height of FONT-FAMILY (`getLineHeight')."
  (or (and (numberp font-family) (alist-get font-family excal--restore-font-line-heights))
      1.25))

(defun excal--stroke-width-key (width)
  "Return the `currentItemStrokeWidthKey' for numeric stroke WIDTH, or nil.
Port of `getStrokeWidthKey'; for legacy `currentItemStrokeWidth' values."
  (and (numberp width)
       (car (cl-find-if (lambda (entry) (= (cdr entry) width))
                        excal--restore-stroke-widths))))

;;;; JavaScript value semantics

(defsubst excal--restore-js (element key)
  "Return KEY of ELEMENT, or `:undefined' if it is missing."
  (alist-get key element :undefined))

(defsubst excal--restore-nullish-p (value)
  "Return non-nil if VALUE is null or undefined."
  (memq value '(:undefined :null)))

(defun excal--restore-falsy-p (value)
  "Return non-nil if VALUE is falsy in JavaScript."
  (or (memq value '(:undefined :null :false))
      (and (numberp value) (or (= value 0) (isnan (float value))))
      (equal value "")))

(defsubst excal--restore-or (value default)
  "JavaScript VALUE || DEFAULT."
  (if (excal--restore-falsy-p value) default value))

(defsubst excal--restore-nullish (value default)
  "JavaScript VALUE ?? DEFAULT."
  (if (excal--restore-nullish-p value) default value))

(defun excal--restore-finite-p (value)
  "Return non-nil if VALUE is a finite number."
  (and (numberp value)
       (or (integerp value)
           (not (or (isnan value) (= value 1.0e+INF) (= value -1.0e+INF))))))

(defun excal--restore-valid-point-p (point)
  "Return non-nil if POINT is a vector of two finite numbers."
  (and (vectorp point) (= (length point) 2)
       (excal--restore-finite-p (aref point 0)) (excal--restore-finite-p (aref point 1))))

(defun excal--restore-put-defined (element key value)
  "Set KEY of ELEMENT to VALUE, or remove KEY if VALUE is `:undefined'.
Return the possibly new ELEMENT."
  (if (eq value :undefined)
      (assq-delete-all key element)
    (excal--put element key value)
    element))

(defun excal--restore-clamp (value low high)
  "Clamp VALUE between LOW and HIGH."
  (max low (min high value)))

(defun excal--restore-parse-float (string)
  "Return JavaScript parseFloat of STRING, or nil for NaN."
  (and (stringp string)
       (string-match "\\`[ \t\n\r]*\\([-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\)"
                     string)
       (string-to-number (match-string 1 string))))

(defun excal--restore-transparent-p (color)
  "Return non-nil if COLOR has zero alpha (tinycolor `getAlpha' = 0)."
  (and (stringp color)
       (let ((c (downcase (string-trim color))))
         (or (equal c "transparent")
             (string-match-p "\\`#[0-9a-f]\\{3\\}0\\'" c)
             (string-match-p "\\`#[0-9a-f]\\{6\\}00\\'" c)
             (string-match-p "\\`\\(?:rgb\\|hsl\\)a?(.*[,/][ \t]*0*\\.?0*%?[ \t]*)\\'" c)))))

(defun excal--restore-normalize-link (link)
  "Port of `normalizeLink' (without URL canonicalisation)."
  (let ((link (string-trim link)))
    (cond
     ((string-empty-p link) link)
     ((string-match-p "\\`[^[:alnum:]]*\\(?:javascript\\|data\\|vbscript\\):"
                      (downcase (replace-regexp-in-string "[[:cntrl:][:space:]]" "" link)))
      "about:blank")
     (t (string-replace "\"" "&quot;" link)))))

(defun excal--restore-normalize-arrowhead (arrowhead)
  "Port of `normalizeArrowhead'."
  (pcase arrowhead
    ((or :undefined :null) :null)
    ("dot" "circle")
    ("crowfoot_one" "cardinality_one")
    ("crowfoot_many" "cardinality_many")
    ("crowfoot_one_or_many" "cardinality_one_or_many")
    (_ arrowhead)))

;;;; Geometry helpers for binding migration

(defun excal--restore-num (element key &optional default)
  "Return numeric KEY of ELEMENT, or DEFAULT (0)."
  (let ((v (alist-get key element)))
    (if (excal--restore-finite-p v) v (or default 0))))

(defun excal--restore-rotate (point center angle)
  "Rotate POINT (X . Y) about CENTER by ANGLE radians."
  (if (or (null angle) (= angle 0))
      point
    (let ((dx (- (car point) (car center))) (dy (- (cdr point) (cdr center)))
          (c (cos angle)) (s (sin angle)))
      (cons (+ (car center) (- (* dx c) (* dy s)))
            (+ (cdr center) (* dx s) (* dy c))))))

(defun excal--restore-shape-center (element)
  "Return the center (X . Y) of bindable ELEMENT."
  (cons (+ (excal--restore-num element 'x) (/ (excal--restore-num element 'width) 2.0))
        (+ (excal--restore-num element 'y) (/ (excal--restore-num element 'height) 2.0))))

(defun excal--restore-linear-point-global (arrow index)
  "Return point INDEX of ARROW in scene coordinates (rotation applied)."
  (let* ((points (alist-get 'points arrow))
         (x (excal--restore-num arrow 'x)) (y (excal--restore-num arrow 'y))
         (xs (mapcar (lambda (p) (aref p 0)) points))
         (ys (mapcar (lambda (p) (aref p 1)) points))
         (center (cons (+ x (/ (+ (apply #'min xs) (apply #'max xs)) 2.0))
                       (+ y (/ (+ (apply #'min ys) (apply #'max ys)) 2.0))))
         (p (and (< -1 index (length points)) (aref points index))))
    (excal--restore-rotate (if p (cons (+ x (aref p 0)) (+ y (aref p 1))) (cons x y))
                   center (excal--restore-num arrow 'angle))))

(defun excal--restore-point-in-shape-p (point element)
  "Return non-nil if POINT lies inside ELEMENT (`isPointInElement').
Rounded corners are ignored; lines and freedraw have no inside."
  (let ((type (alist-get 'type element)))
    (unless (member type '("line" "arrow" "freedraw"))
      (let* ((w (excal--restore-num element 'width)) (h (excal--restore-num element 'height))
             (x (excal--restore-num element 'x)) (y (excal--restore-num element 'y))
             (center (excal--restore-shape-center element))
             (p (excal--restore-rotate point center (- (excal--restore-num element 'angle))))
             (dx (- (car p) (car center))) (dy (- (cdr p) (cdr center)))
             (hw (/ w 2.0)) (hh (/ h 2.0)))
        (and (> w 0) (> h 0)
             (<= x (car p) (+ x w)) (<= y (cdr p) (+ y h))
             (pcase type
               ("diamond" (<= (+ (/ (abs dx) hw) (/ (abs dy) hh)) 1.0))
               ("ellipse" (<= (+ (/ (* dx dx) (* hw hw)) (/ (* dy dy) (* hh hh))) 1.0))
               (_ t)))))))

(defun excal--restore-segment-intersection (a1 a2 b1 b2)
  "Return the intersection of segments A1-A2 and B1-B2, or nil."
  (let* ((rx (- (car a2) (car a1))) (ry (- (cdr a2) (cdr a1)))
         (sx (- (car b2) (car b1))) (sy (- (cdr b2) (cdr b1)))
         (den (- (* rx sy) (* ry sx))))
    (unless (= den 0)
      (let* ((qx (- (car b1) (car a1))) (qy (- (cdr b1) (cdr a1)))
             (u (/ (- (* qx ry) (* qy rx)) (float den)))
             (tt (/ (- (* qx sy) (* qy sx)) (float den))))
        (when (and (<= 0 tt 1) (<= 0 u 1))
          (cons (+ (car a1) (* tt rx)) (+ (cdr a1) (* tt ry))))))))

(defun excal--restore-distance (a b)
  "Return the distance between points A and B."
  (sqrt (+ (expt (- (car a) (car b)) 2) (expt (- (cdr a) (cdr b)) 2))))

(defun excal--restore-normalize-fixed-point (fixed-point)
  "Port of `normalizeFixedPoint'."
  (if (not (and (vectorp fixed-point) (= (length fixed-point) 2)
                (excal--restore-finite-p (aref fixed-point 0))
                (excal--restore-finite-p (aref fixed-point 1))))
      (vector 0.5001 0.5001)
    (let* ((clamped (vconcat (mapcar (lambda (r) (excal--restore-clamp r (- excal--restore-fixed-point-bound)
                                                                excal--restore-fixed-point-bound))
                                     fixed-point)))
           (near (lambda (r) (< (abs (- r 0.5)) 0.0001))))
      (if (or (funcall near (aref clamped 0)) (funcall near (aref clamped 1)))
          (vconcat (mapcar (lambda (r) (if (funcall near r) 0.5001 r)) clamped))
        clamped))))

(defun excal--restore-global-fixed-point (fixed-point element)
  "Port of `getGlobalFixedPointForBindableElement'."
  (excal--restore-rotate (cons (+ (excal--restore-num element 'x)
                          (* (excal--restore-num element 'width) (aref fixed-point 0)))
                       (+ (excal--restore-num element 'y)
                          (* (excal--restore-num element 'height) (aref fixed-point 1))))
                 (excal--restore-shape-center element) (excal--restore-num element 'angle)))

(defun excal--restore-snap-outline-midpoint (point element)
  "Port of `getSnapOutlineMidPoint' for simple arrows at zoom 1."
  (let* ((x (excal--restore-num element 'x)) (y (excal--restore-num element 'y))
         (w (excal--restore-num element 'width)) (h (excal--restore-num element 'height))
         (center (excal--restore-shape-center element))
         (angle (excal--restore-num element 'angle))
         ;; maxBindingDistance_simple(1) = clamp(15 / 1.5, 15, 30) = 15
         (threshold (+ 15 (/ (excal--restore-num element 'strokeWidth) 2.0))))
    (cl-find-if (lambda (mid)
                  (and (<= (excal--restore-distance mid point) threshold)
                       (not (excal--restore-point-in-shape-p point element))))
                (mapcar (lambda (p) (excal--restore-rotate p center angle))
                        (list (cons (+ x w) (+ y (/ h 2.0)))
                              (cons (+ x (/ w 2.0)) (+ y h))
                              (cons x (+ y (/ h 2.0)))
                              (cons (+ x (/ w 2.0)) y))))))

(defun excal--restore-element-diagonals (element)
  "Port of `getDiagonalsForBindableElement'."
  (let* ((x (excal--restore-num element 'x)) (y (excal--restore-num element 'y))
         (w (excal--restore-num element 'width)) (h (excal--restore-num element 'height))
         (center (excal--restore-shape-center element))
         (angle (excal--restore-num element 'angle))
         (offset (if (equal (alist-get 'type element) "rectangle") 15 0))
         (rot (lambda (px py) (excal--restore-rotate (cons px py) center angle)))
         (shrink (lambda (a b)
                   (let* ((dx (- (car b) (car a))) (dy (- (cdr b) (cdr a)))
                          (len (sqrt (+ (* dx dx) (* dy dy))))
                          (ox (if (> len 0) (* offset (/ dx len)) 0))
                          (oy (if (> len 0) (* offset (/ dy len)) 0)))
                     ;; vectorFromPoint(seg[1], seg[0]) = seg[1] - seg[0]
                     (list (cons (+ (car a) ox) (+ (cdr a) oy))
                           (cons (- (car b) ox) (- (cdr b) oy)))))))
    (if (member (alist-get 'type element) '("diamond" "ellipse"))
        (list (funcall shrink (funcall rot (+ x (/ w 2.0)) y)
                       (funcall rot (+ x (/ w 2.0)) (+ y h)))
              (funcall shrink (funcall rot x (+ y (/ h 2.0)))
                       (funcall rot (+ x w) (+ y (/ h 2.0)))))
      (list (funcall shrink (funcall rot x y) (funcall rot (+ x w) (+ y h)))
            (funcall shrink (funcall rot (+ x w) y) (funcall rot x (+ y h)))))))

(defun excal--restore-project-fixed-point (arrow point element start elements-map)
  "Port of `projectFixedPointOntoDiagonal' at zoom 1, or nil.
ARROW is the arrow with restored points, POINT its endpoint, ELEMENT
the bound element, START non-nil for the start binding."
  (or (excal--restore-snap-outline-midpoint point element)
      (let ((aw (alist-get 'width arrow)) (ah (alist-get 'height arrow)))
        (unless (and (numberp aw) (< aw 3) (numberp ah) (< ah 3))
          (pcase-let* ((points (alist-get 'points arrow))
                       (`(,d1 ,d2) (excal--restore-element-diagonals element))
                       (a (excal--restore-linear-point-global
                           arrow (if start 1 (- (length points) 2)))))
            (when (= (length points) 2)
              (let* ((other (alist-get (if start 'endBinding 'startBinding) arrow))
                     (other-id (and (consp other) (alist-get 'elementId other)))
                     (bindable (and (stringp other-id) (gethash other-id elements-map))))
                (when bindable
                  (setq a (excal--restore-global-fixed-point
                           (excal--restore-normalize-fixed-point (alist-get 'fixedPoint other))
                           bindable)))))
            (let* ((scale (+ (* 2 (excal--restore-distance a point))
                             (max (excal--restore-distance (nth 0 d1) (nth 1 d1))
                                  (excal--restore-distance (nth 0 d2) (nth 1 d2)))))
                   (b (cons (+ (car a) (* scale (- (car point) (car a))))
                            (+ (cdr a) (* scale (- (cdr point) (cdr a))))))
                   (p1 (excal--restore-segment-intersection (nth 0 d1) (nth 1 d1) b a))
                   (p2 (excal--restore-segment-intersection (nth 0 d2) (nth 1 d2) b a))
                   (projection (cond ((and p1 p2)
                                      (if (< (excal--restore-distance a p1) (excal--restore-distance a p2))
                                          p1 p2))
                                     (t (or p1 p2)))))
              (and projection (excal--restore-point-in-shape-p projection element)
                   projection)))))))

(defun excal--restore-fixed-point-for (element focus-point)
  "Port of `calculateFixedPointForNonElbowArrowBinding' for FOCUS-POINT."
  (let* ((p (excal--restore-rotate focus-point (excal--restore-shape-center element)
                           (- (excal--restore-num element 'angle))))
         (w (excal--restore-num element 'width)) (h (excal--restore-num element 'height)))
    (if (or (< w 1) (< h 1))
        (excal--restore-normalize-fixed-point (vector 0.5 0.5))
      (let ((gap (+ excal--restore-base-binding-gap
                    (/ (excal--restore-num element 'strokeWidth) 2.0))))
        (excal--restore-normalize-fixed-point
         (vector (/ (- (car p) (excal--restore-num element 'x)) (float (max w gap)))
                 (/ (- (cdr p) (excal--restore-num element 'y)) (float (max h gap)))))))))

(defun excal--restore-repair-binding (arrow binding targets existing start)
  "Port of `repairBinding' for BINDING of ARROW (with restored points).
TARGETS and EXISTING map ids to the raw loaded and to existing elements.
START is non-nil for the start binding."
  (condition-case nil
      (cond
       ((or (excal--restore-nullish-p binding) (eq binding :false)) :null)
       ((eq (alist-get 'elbowed arrow) t)
        (let ((b (copy-alist binding)))
          (excal--put b 'fixedPoint (excal--restore-normalize-fixed-point
                                     (alist-get 'fixedPoint b)))
          (excal--put b 'mode (excal--restore-or (excal--restore-js binding 'mode) "orbit"))
          b))
       ((not (excal--restore-falsy-p (excal--restore-js binding 'mode)))
        (if (excal--restore-falsy-p (excal--restore-js binding 'elementId))
            :null
          (list (cons 'elementId (alist-get 'elementId binding))
                (cons 'mode (alist-get 'mode binding))
                (cons 'fixedPoint (excal--restore-normalize-fixed-point
                                   (alist-get 'fixedPoint binding))))))
       (t
        (let* ((id (alist-get 'elementId binding))
               (target (and (stringp id) (gethash id targets)))
               (bound (or target (and existing (stringp id) (gethash id existing))))
               (map (if target targets existing)))
          (if (not (and bound map))
              :null
            (let* ((points (alist-get 'points arrow))
                   (p (excal--restore-linear-point-global
                       arrow (if start 0 (1- (length points)))))
                   (mode (if (excal--restore-point-in-shape-p p bound) "inside" "orbit"))
                   (safe (copy-alist arrow)))
              (dolist (key '(startBinding endBinding))
                (let ((b (alist-get key arrow)))
                  (excal--put safe key
                              (if (and (consp b) (not (excal--restore-falsy-p (excal--restore-js b 'elementId))))
                                  (let ((b (copy-alist b)))
                                    (excal--put b 'mode mode)
                                    (excal--put b 'fixedPoint (excal--restore-normalize-fixed-point
                                                               (alist-get 'fixedPoint b)))
                                    b)
                                :null))))
              (let ((focus (if (equal mode "inside")
                               p
                             (or (excal--restore-project-fixed-point safe p bound start map) p))))
                (list (cons 'mode mode)
                      (cons 'elementId id)
                      (cons 'fixedPoint (excal--restore-fixed-point-for bound focus)))))))))
    (error :null)))

;;;; Elements

(defun excal--restore-base (element extra)
  "Port of `restoreElementWithProperties'.
ELEMENT is a fresh copy of the loaded alist, EXTRA an alist of
per-type fields (values may be `:undefined' to drop a key).  Return the
restored alist, reusing ELEMENT's cells and key order."
  (let* ((get (lambda (key) (excal--restore-js element key)))
         (type (excal--restore-or (alist-get 'type extra :undefined) (funcall get 'type)))
         (width (excal--restore-or (funcall get 'width) 0))
         (height (excal--restore-or (funcall get 'height) 0))
         (x (excal--restore-nullish (alist-get 'x extra :undefined)
                            (excal--restore-nullish (funcall get 'x) 0)))
         (y (excal--restore-nullish (alist-get 'y extra :undefined)
                            (excal--restore-nullish (funcall get 'y) 0)))
         (roundness (funcall get 'roundness))
         (bound-ids (funcall get 'boundElementIds))
         (link (funcall get 'link))
         (base
          `((type . ,type)
            (version . ,(excal--restore-or (funcall get 'version) 1))
            (versionNonce . ,(excal--restore-nullish (funcall get 'versionNonce) 0))
            (index . ,(excal--restore-nullish (funcall get 'index) :null))
            (isDeleted . ,(excal--restore-nullish (funcall get 'isDeleted) :false))
            (id . ,(excal--restore-or (funcall get 'id) (excal--new-id)))
            (fillStyle . ,(excal--restore-or (funcall get 'fillStyle) "solid"))
            (strokeWidth . ,(excal--restore-or (funcall get 'strokeWidth) 2))
            (strokeStyle . ,(excal--restore-nullish (funcall get 'strokeStyle) "solid"))
            (roughness . ,(excal--restore-nullish (funcall get 'roughness) 1))
            (opacity . ,(excal--restore-nullish (funcall get 'opacity) 100))
            (angle . ,(excal--restore-or (funcall get 'angle) 0))
            (x . ,x) (y . ,y)
            (strokeColor . ,(excal--restore-or (funcall get 'strokeColor) "#1e1e1e"))
            (backgroundColor . ,(excal--restore-or (funcall get 'backgroundColor) "transparent"))
            (width . ,width) (height . ,height)
            (seed . ,(excal--restore-nullish (funcall get 'seed) 1))
            (groupIds . ,(excal--restore-nullish (funcall get 'groupIds) []))
            (frameId . ,(excal--restore-nullish (funcall get 'frameId) :null))
            (roundness
             . ,(cond ((not (excal--restore-falsy-p roundness)) roundness)
                      ((equal (funcall get 'strokeSharpness) "round")
                       (list (cons 'type (if (member type '("rectangle" "embeddable"
                                                             "iframe" "image"))
                                             1 2))))
                      (t :null)))
            (boundElements
             . ,(if (not (excal--restore-falsy-p bound-ids))
                    (vconcat (mapcar (lambda (id) (list (cons 'type "arrow") (cons 'id id)))
                                     bound-ids))
                  (excal--restore-nullish (funcall get 'boundElements) [])))
            (updated . ,(excal--restore-nullish (funcall get 'updated)
                                        (truncate (* 1000 (float-time)))))
            (created . ,(excal--restore-nullish (funcall get 'created) :null))
            (link . ,(if (and (stringp link) (not (string-empty-p link)))
                         (excal--restore-normalize-link link)
                       (if (excal--restore-falsy-p link) :null link)))
            (locked . ,(excal--restore-nullish (funcall get 'locked) :false)))))
    ;; getNormalizedDimensions
    (when (and (numberp width) (< width 0))
      (setf (alist-get 'width base) (abs width)
            (alist-get 'x base) (- x (abs width))))
    (when (and (numberp height) (< height 0))
      (setf (alist-get 'height base) (abs height)
            (alist-get 'y base) (- y (abs height))))
    (pcase-dolist (`(,key . ,value) base)
      (excal--put element key value))
    (pcase-dolist (`(,key . ,value) extra)
      (setq element (excal--restore-put-defined element key value)))
    (dolist (key '(strokeSharpness boundElementIds))
      (setq element (assq-delete-all key element)))
    element))

(defun excal--restore-points (points width height)
  "Port of `restoreLinearElementPoints'."
  (let ((valid (and (vectorp points)
                    (cl-loop for p across points
                             when (excal--restore-valid-point-p p)
                             collect (vector (aref p 0) (aref p 1))))))
    (if (< (length valid) 2)
        (vector (vector 0 0)
                (vector (if (excal--restore-finite-p width) width 0)
                        (if (excal--restore-finite-p height) height 0)))
      (vconcat valid))))

(defun excal--restore-normalize-points (points x y)
  "Shift POINTS so the first is the origin; return (POINTS X Y)."
  (let ((ox (aref (aref points 0) 0)) (oy (aref (aref points 0) 1)))
    (if (and (= ox 0) (= oy 0))
        (list points x y)
      (list (vconcat (mapcar (lambda (p) (vector (- (aref p 0) ox) (- (aref p 1) oy)))
                             points))
            (+ x ox) (+ y oy)))))

(defun excal--restore-points-size (points)
  "Return (WIDTH . HEIGHT) of POINTS (`getSizeFromPoints')."
  (let ((xs (mapcar (lambda (p) (aref p 0)) points))
        (ys (mapcar (lambda (p) (aref p 1)) points)))
    (cons (- (apply #'max xs) (apply #'min xs))
          (- (apply #'max ys) (apply #'min ys)))))

(defun excal--restore-points-equal-p (a b tolerance)
  "Return non-nil if points A and B are within TOLERANCE on both axes."
  (and (<= (abs (- (aref a 0) (aref b 0))) tolerance)
       (<= (abs (- (aref a 1) (aref b 1))) tolerance)))

(defun excal--restore-handle-oversized (element)
  "Port of `handleOversizedLinearElements'."
  (if (and (<= (alist-get 'width element) excal--restore-max-linear-px)
           (<= (alist-get 'height element) excal--restore-max-linear-px))
      element
    (message "excal: removing oversized %s %s" (alist-get 'type element)
             (alist-get 'id element))
    (dolist (field `((x . 0) (y . 0) (width . 100) (height . 100)
                     (points . ,(vector (vector 0 0) (vector 100 100)))
                     (isDeleted . t)))
      (excal--put element (car field) (cdr field)))
    element))

(defun excal--restore-bump-version (element &optional version)
  "Port of `bumpVersion': increment ELEMENT's version (from VERSION)."
  (excal--put element 'version
              (1+ (or version (let ((v (alist-get 'version element)))
                                (if (numberp v) v 0)))))
  (excal--put element 'versionNonce (random (ash 1 31)))
  (excal--put element 'updated (truncate (* 1000 (float-time))))
  element)

(defun excal--restore-text (element delete-invisible)
  "Restore text ELEMENT; with DELETE-INVISIBLE, delete it if empty."
  (setq element (assq-delete-all 'rawText element))
  (let* ((font-size (excal--restore-js element 'fontSize))
         (font-family (excal--restore-js element 'fontFamily))
         (font (excal--restore-js element 'font))
         (raw-text (excal--restore-js element 'text)))
    (unless (eq font :undefined)
      (let ((parts (split-string (if (stringp font) font "") " ")))
        (setq font-size (or (excal--restore-parse-float (car parts)) :undefined)
              font-family (or (cdr (assoc (cadr parts) excal--restore-font-families))
                              excal--restore-default-font-family))))
    (unless (excal--restore-finite-p font-size)
      (setq font-size excal--restore-default-font-size))
    (let* ((text (if (and (stringp raw-text) (not (string-empty-p raw-text)))
                     raw-text ""))
           (height (excal--restore-js element 'height))
           (line-height
            (let ((own (excal--restore-js element 'lineHeight)))
              (cond
               ((not (excal--restore-falsy-p own)) own)
               ((or (excal--restore-falsy-p height) (not (excal--restore-finite-p height)))
                (excal--restore-line-height-for (excal--restore-js element 'fontFamily)))
               (t
                ;; detectLineHeight
                (let ((raw-size (excal--restore-js element 'fontSize)))
                  (/ (float height)
                     (length (split-string
                              (replace-regexp-in-string
                               "\r\n?" "\n" (if (stringp raw-text) raw-text ""))
                              "\n"))
                     (if (excal--restore-finite-p raw-size) raw-size font-size)))))))
           (label (excal--restore-js element 'labelPosition))
           (base-size (excal--restore-js element 'baseFontSize)))
      (setq element
            (excal--restore-base
             element
             `((fontSize . ,font-size)
               (fontFamily . ,font-family)
               (text . ,text)
               (textAlign . ,(excal--restore-or (excal--restore-js element 'textAlign) "left"))
               (verticalAlign . ,(excal--restore-or (excal--restore-js element 'verticalAlign) "top"))
               (containerId . ,(excal--restore-nullish (excal--restore-js element 'containerId) :null))
               (originalText . ,(excal--restore-or (excal--restore-js element 'originalText) text))
               (autoResize . ,(excal--restore-nullish (excal--restore-js element 'autoResize) t))
               (lineHeight . ,line-height)
               (labelPosition . ,(if (excal--restore-finite-p label) (excal--restore-clamp label 0 1) :null))
               (baseFontSize . ,(if (excal--restore-finite-p base-size)
                                    (excal--restore-clamp base-size 1 excal--restore-sticky-note-max-font-size)
                                  :null)))))
      (when (and delete-invisible (string-empty-p text)
                 (not (eq (alist-get 'isDeleted element) t)))
        (excal--put element 'originalText text)
        (excal--put element 'isDeleted t)
        (excal--restore-bump-version element))
      element)))

(defun excal--restore-linear (element targets existing)
  "Restore line, legacy draw or arrow ELEMENT.
TARGETS and EXISTING are id maps used to migrate arrow bindings."
  (let* ((type (alist-get 'type element))
         (arrow (equal type "arrow"))
         (x (excal--restore-js element 'x)) (y (excal--restore-js element 'y))
         (points (excal--restore-points (excal--restore-js element 'points)
                                        (excal--restore-js element 'width)
                                        (excal--restore-js element 'height))))
    (if (not arrow)
        (pcase-let* ((`(,points ,x ,y)
                      (excal--restore-normalize-points points (excal--restore-nullish x 0)
                                               (excal--restore-nullish y 0)))
                     (size (excal--restore-points-size points)))
          (excal--restore-handle-oversized
           (excal--restore-base
            element
            `((type . "line") (startBinding . :null) (endBinding . :null)
              (startArrowhead . ,(excal--restore-normalize-arrowhead
                                  (excal--restore-js element 'startArrowhead)))
              (endArrowhead . ,(excal--restore-normalize-arrowhead
                                (excal--restore-js element 'endArrowhead)))
              (points . ,points) (x . ,x) (y . ,y)
              ;; Upstream adds `polygon' only to "line", not legacy "draw"
              ;; elements; adding it to both keeps restore idempotent.
              ,@(progn
                  (let ((polygon (excal--restore-js element 'polygon)))
                    `((polygon . ,(if (and (> (length points) 3)
                                           (excal--restore-points-equal-p
                                            (aref points 0)
                                            (aref points (1- (length points)))
                                            1e-4))
                                      (excal--restore-nullish polygon :false)
                                    :false)))))
              (width . ,(car size)) (height . ,(cdr size))))))
      (let* ((x (excal--restore-nullish x 0)) (y (excal--restore-nullish y 0))
             (with-points (copy-alist element))
             (elbow (eq (alist-get 'elbowed element) t))
             (end-head (excal--restore-js element 'endArrowhead))
             (size (excal--restore-points-size points)))
        (excal--put with-points 'points points)
        (excal--put with-points 'x x)
        (excal--put with-points 'y y)
        (setq element
              (excal--restore-base
               element
               `((type . ,type)
                 (startBinding . ,(excal--restore-repair-binding
                                   with-points (excal--restore-js element 'startBinding)
                                   targets existing t))
                 (endBinding . ,(excal--restore-repair-binding
                                 with-points (excal--restore-js element 'endBinding)
                                 targets existing nil))
                 (startArrowhead . ,(excal--restore-normalize-arrowhead
                                     (excal--restore-js element 'startArrowhead)))
                 (endArrowhead . ,(if (eq end-head :undefined) "arrow"
                                    (excal--restore-normalize-arrowhead end-head)))
                 (points . ,points) (x . ,x) (y . ,y)
                 (elbowed . ,(if elbow t (excal--restore-js element 'elbowed)))
                 (width . ,(car size)) (height . ,(cdr size))
                 ,@(when elbow
                     (let ((segments (excal--restore-js element 'fixedSegments)))
                       `((fixedSegments . ,(if (and (vectorp segments)
                                                    (> (length segments) 0)
                                                    (>= (length points) 4))
                                               segments :null))
                         (startIsSpecial . ,(excal--restore-js element 'startIsSpecial))
                         (endIsSpecial . ,(excal--restore-js element 'endIsSpecial))))))))
        (pcase-let ((`(,points ,x ,y)
                     (excal--restore-normalize-points (alist-get 'points element)
                                              (alist-get 'x element)
                                              (alist-get 'y element))))
          (excal--put element 'points points)
          (excal--put element 'x x)
          (excal--put element 'y y))
        (excal--restore-handle-oversized element)))))

(defun excal--restore-freedraw-points (points pressures)
  "Port of `restoreFreedrawPoints': return (POINTS . PRESSURES)."
  (if (not (vectorp points))
      (cons [] [])
    (let ((pressures (if (vectorp pressures) pressures []))
          kept kept-pressures)
      (cl-loop for p across points for i from 0
               when (excal--restore-valid-point-p p)
               do (push (vector (aref p 0) (aref p 1)) kept)
               (when (< i (length pressures))
                 (let ((v (aref pressures i)))
                   (push (if (excal--restore-finite-p v) v 0.5) kept-pressures))))
      (cons (vconcat (nreverse kept)) (vconcat (nreverse kept-pressures))))))

(defun excal--restore-element (element targets existing delete-invisible)
  "Port of `restoreElement': return the restored copy of ELEMENT.
Return nil for `selection' elements and ELEMENT itself (untouched) for
unknown types.  TARGETS and EXISTING map ids to elements."
  (let ((type (alist-get 'type element)))
    (if (not (member type (cons "draw" excal--indexed-types)))
        element
      (let ((element (copy-alist element)))
        (pcase type
          ("selection" nil)
          ("text" (excal--restore-text element delete-invisible))
          ("freedraw"
           (let ((restored (excal--restore-freedraw-points
                            (excal--restore-js element 'points) (excal--restore-js element 'pressures)))
                 (options (excal--restore-js element 'strokeOptions)))
             (excal--restore-base
              element
              `((points . ,(car restored))
                (simulatePressure . ,(excal--restore-js element 'simulatePressure))
                (strokeOptions
                 . ((variability . ,(let ((v (and (consp options)
                                                  (alist-get 'variability options))))
                                      (if (member v '("variable" "constant")) v "variable")))
                    (streamline . ,(let ((v (and (consp options)
                                                 (alist-get 'streamline options))))
                                     (if (excal--restore-finite-p v) v 0.5)))))
                (pressures . ,(cdr restored))))))
          ("image"
           (excal--restore-base
            element
            `((status . ,(excal--restore-or (excal--restore-js element 'status) "pending"))
              (fileId . ,(excal--restore-js element 'fileId))
              (scale . ,(excal--restore-or (excal--restore-js element 'scale) [1 1]))
              (crop . ,(excal--restore-nullish (excal--restore-js element 'crop) :null)))))
          ((or "line" "draw" "arrow") (excal--restore-linear element targets existing))
          ((or "rectangle" "ellipse" "diamond" "iframe" "embeddable")
           (excal--restore-base element nil))
          ("stickynote"
           (excal--restore-normalize-sticky-note
            (excal--restore-base
             element
             `((baseHeight . ,(excal--restore-nullish
                               (excal--restore-js element 'baseHeight)
                               (excal--restore-nullish (excal--restore-js element 'maxHeight)
                                               (excal--restore-js element 'height))))))))
          ((or "frame" "magicframe")
           (excal--restore-base
            element `((name . ,(excal--restore-nullish (excal--restore-js element 'name) :null))))))))))

(defun excal--restore-normalize-sticky-note (element)
  "Port of `normalizeStickyNote' (style, then geometry)."
  (let ((bg (alist-get 'backgroundColor element))
        (stroke (alist-get 'strokeColor element)))
    (excal--put element 'backgroundColor
                (if (or (not (stringp bg)) (string-empty-p bg) (excal--restore-transparent-p bg))
                    excal--restore-default-sticky-note-bg bg))
    (excal--put element 'strokeColor (excal--restore-sticky-stroke stroke))
    (excal--put element 'fillStyle "solid")
    (let* ((width (max (excal--restore-num element 'width) excal--restore-sticky-note-min-size))
           (height (excal--restore-num element 'height))
           (base (max (excal--restore-or (alist-get 'baseHeight element)
                                 (excal--restore-or height excal--restore-default-sticky-note-size))
                      excal--restore-sticky-note-min-size)))
      (excal--put element 'width width)
      (excal--put element 'height (max height base))
      (excal--put element 'baseHeight base)
      element)))

(defun excal--restore-sticky-stroke (color)
  "Port of `normalizeStickyNoteStrokeColor'."
  (if (or (not (stringp color)) (string-empty-p color) (excal--restore-transparent-p color))
      "#1e1e1e"
    color))

(defun excal--restore-invisibly-small-p (element)
  "Port of `isInvisiblySmallElement' for the loaded ELEMENT."
  (let ((type (alist-get 'type element)))
    (if (member type '("line" "arrow" "freedraw"))
        (let* ((points (alist-get 'points element))
               (n (if (vectorp points) (length points) 0)))
          (or (< n 2)
              (and (= n 2) (equal type "arrow")
                   (excal--restore-valid-point-p (aref points 0))
                   (excal--restore-valid-point-p (aref points 1))
                   (excal--restore-points-equal-p (aref points 0) (aref points 1) 0.1))))
      (let ((w (alist-get 'width element)) (h (alist-get 'height element)))
        (and (numberp w) (= w 0) (numberp h) (= h 0))))))

(defun excal--restore-id-map (elements)
  "Return a hash table mapping ids to ELEMENTS (later ones win)."
  (let ((map (make-hash-table :test #'equal)))
    (dolist (e elements map)
      (let ((id (alist-get 'id e)))
        (when (stringp id) (puthash id e map))))))

(defun excal--restore-known-p (element)
  "Return non-nil if restore understands ELEMENT's type."
  (excal--indexed-p element))

(defun excal--restore-repair-references (elements map)
  "Repair frame, container and binding references of ELEMENTS in MAP."
  (dolist (e elements)
    (when (excal--restore-known-p e)
      (let ((frame (alist-get 'frameId e)))
        (when (and (stringp frame) (not (string-empty-p frame))
                   (not (gethash frame map)))
          (excal--put e 'frameId :null)))
      (let ((container-id (alist-get 'containerId e)))
        (cond
         ((and (equal (alist-get 'type e) "text")
               (stringp container-id) (not (string-empty-p container-id)))
          ;; repairBoundElement
          (let ((container (gethash container-id map)))
            (excal--put e 'angle
                        (if (or (null container)
                                (equal (alist-get 'type container) "arrow"))
                            0
                          (excal--restore-nullish (excal--restore-js container 'angle) 0)))
            (cond
             ((null container) (excal--put e 'containerId :null))
             ((eq (alist-get 'isDeleted e) t))
             ((and (excal--restore-known-p container)
                   (vectorp (alist-get 'boundElements container))
                   (not (cl-find (alist-get 'id e) (alist-get 'boundElements container)
                                 :key (lambda (b) (and (consp b) (alist-get 'id b)))
                                 :test #'equal)))
              (excal--put container 'boundElements
                          (vconcat (alist-get 'boundElements container)
                                   (list (list (cons 'type "text")
                                               (cons 'id (alist-get 'id e))))))))))
         ((not (excal--restore-falsy-p (excal--restore-js e 'boundElements)))
          ;; repairContainerElement
          (let ((seen (make-hash-table :test #'equal)) kept)
            (seq-doseq (b (alist-get 'boundElements e))
              (let* ((id (and (consp b) (alist-get 'id b)))
                     (bound (and (stringp id) (gethash id map))))
                (when (and bound (not (gethash id seen)))
                  (puthash id t seen)
                  (unless (eq (alist-get 'isDeleted bound) t)
                    (push b kept)
                    (when (and (equal (alist-get 'type bound) "text")
                               (excal--restore-known-p bound)
                               (excal--restore-falsy-p (excal--restore-js bound 'containerId)))
                      (excal--put bound 'containerId (alist-get 'id e)))))))
            (excal--put e 'boundElements (vconcat (nreverse kept)))))))
      (when (member (alist-get 'type e) '("line" "arrow"))
        (dolist (key '(startBinding endBinding))
          (let ((b (alist-get key e)))
            (when (and (consp b)
                       (or (not (gethash (alist-get 'elementId b) map))
                           (not (equal (alist-get 'type e) "arrow"))))
              (excal--put e key :null))))))))

(defun excal--restore-sticky-notes (elements map)
  "Port of `restoreStickyNotes' without re-layout."
  (dolist (e elements)
    (when (and (equal (alist-get 'type e) "text")
               (not (eq (alist-get 'isDeleted e) t)))
      (let* ((cid (alist-get 'containerId e))
             (container (and (stringp cid) (gethash cid map))))
        (if (and container (equal (alist-get 'type container) "stickynote"))
            (let* ((own (alist-get 'strokeColor e))
                   (stroke (excal--restore-sticky-stroke
                            (if (excal--restore-transparent-p own)
                                (alist-get 'strokeColor container)
                              own)))
                   (size (excal--restore-nullish (excal--restore-js e 'baseFontSize)
                                         (alist-get 'fontSize e))))
              (excal--put e 'baseFontSize
                          (if (excal--restore-finite-p size)
                              (excal--restore-clamp size 1 excal--restore-sticky-note-max-font-size)
                            excal--restore-sticky-note-fallback-font-size))
              (excal--put e 'strokeColor stroke)
              (unless (equal (alist-get 'strokeColor container) stroke)
                (excal--put container 'strokeColor stroke)))
          (unless (excal--restore-nullish-p (excal--restore-js e 'baseFontSize))
            (excal--put e 'baseFontSize :null)))))))

(defun excal--restore-normalize-bound-elements-order (elements map)
  "Port of `normalizeBoundElementsOrder': bound text right after its container."
  (let ((sorted (make-hash-table :test #'eq)) result)
    (dolist (e elements)
      (unless (gethash e sorted)
        (let ((bound (alist-get 'boundElements e)))
          (cond
           ((and (vectorp bound) (> (length bound) 0))
            (puthash e t sorted) (push e result)
            (seq-doseq (b bound)
              (let ((child (and (consp b) (gethash (alist-get 'id b) map))))
                (when (and child (equal (alist-get 'type b) "text")
                           (not (gethash child sorted)))
                  (puthash child t sorted) (push child result)))))
           ((and (equal (alist-get 'type e) "text")
                 (stringp (alist-get 'containerId e))
                 (let ((c (gethash (alist-get 'containerId e) map)))
                   (and c (seq-some (lambda (b) (and (consp b)
                                                     (equal (alist-get 'id b)
                                                            (alist-get 'id e))))
                                    (let ((v (alist-get 'boundElements c)))
                                      (if (vectorp v) v []))))))
            nil)
           (t (puthash e t sorted) (push e result))))))
    (if (= (length result) (length elements))
        (nreverse result)
      (message "excal: normalizeBoundElementsOrder lost some elements")
      elements)))

(defun excal--restore-fix-self-bound-elbow (e map)
  "Replace the points of a self-bound elbow arrow E with runaway coords."
  (let ((start (alist-get 'startBinding e)) (end (alist-get 'endBinding e))
        (points (alist-get 'points e)))
    (when (and (equal (alist-get 'type e) "arrow") (eq (alist-get 'elbowed e) t)
               (consp start) (consp end)
               (equal (alist-get 'elementId start) (alist-get 'elementId end))
               (> (length points) 1)
               (seq-some (lambda (p) (or (> (abs (aref p 0)) 1e6) (> (abs (aref p 1)) 1e6)))
                         points))
      (when-let* ((b (gethash (alist-get 'elementId start) map)))
        (let ((bw (excal--restore-num b 'width)) (bh (excal--restore-num b 'height)))
          (excal--put e 'x (+ (excal--restore-num b 'x) (/ bw 2.0)))
          (excal--put e 'y (- (excal--restore-num b 'y) 5))
          (excal--put e 'width bw)
          (excal--put e 'height bh)
          (excal--put e 'points (vector (vector 0 0) (vector 0 -10)
                                        (vector (+ (/ bw 2.0) 5) -10)
                                        (vector (+ (/ bw 2.0) 5) (+ (/ bh 2.0) 5)))))))))

(declare-function excal--elbow-update "excal-elbow")
(declare-function excal--validate-elbow-points "excal-elbow")

(defun excal--restore-reroute-elbow (e)
  "Re-route E if it is an unbound elbow arrow with non-orthogonal points.
The route keeps E's ends; version, nonce and time stay, as upstream
spreads `updateElbowArrowPoints' into the element without bumping it."
  (let ((points (alist-get 'points e)))
    (when (and (equal (alist-get 'type e) "arrow") (eq (alist-get 'elbowed e) t)
               (not (consp (alist-get 'startBinding e)))
               (not (consp (alist-get 'endBinding e)))
               (fboundp 'excal--elbow-update)
               (vectorp points) (> (length points) 1)
               (not (excal--validate-elbow-points
                     (mapcar (lambda (p) (vector (float (elt p 0)) (float (elt p 1))))
                             points))))
      (let ((kept (mapcar (lambda (key) (cons key (alist-get key e)))
                          '(version versionNonce updated)))
            (last (aref points (1- (length points)))))
        (excal--elbow-update e (list :points (list (vector 0.0 0.0)
                                                   (vector (float (elt last 0))
                                                           (float (elt last 1))))))
        (pcase-dolist (`(,key . ,value) kept)
          (excal--put e key value))))))

(cl-defun excal--restore-elements (elements &key existing repair-bindings
                                            delete-invisible)
  "Restore loaded ELEMENTS (a list or vector of alists); return a new list.
Port of upstream `restoreElements'.  EXISTING is a list of elements
already in the scene, used to migrate legacy arrow bindings.  With
REPAIR-BINDINGS (file loads), repair frame, container and binding
references and bound-text order.  With DELETE-INVISIBLE, mark empty and
zero-size elements deleted.  Elements of unknown types are passed
through untouched; `selection' elements are dropped.

File loads use :repair-bindings t :delete-invisible t; clipboard and
library payloads use :delete-invisible t, as upstream."
  (let* ((elements (append elements nil))
         (targets (excal--restore-id-map elements))
         (existing-map (and existing (excal--restore-id-map existing)))
         (ids (make-hash-table :test #'equal))
         result)
    (dolist (element elements)
      (let ((restored (condition-case err
                          (excal--restore-element element targets existing-map
                                                  delete-invisible)
                        (error (message "excal: error restoring element: %S" err)
                               nil))))
        (when restored
          (when (excal--restore-known-p restored)
            (when (and delete-invisible (excal--restore-invisibly-small-p element))
              (let ((local (and existing-map
                                (gethash (alist-get 'id element) existing-map))))
                (excal--restore-bump-version restored
                                     (and local (numberp (alist-get 'version local))
                                          (alist-get 'version local))))
              (excal--put restored 'isDeleted t))
            (when (gethash (alist-get 'id restored) ids)
              (excal--put restored 'id (excal--new-id))))
          (puthash (alist-get 'id restored) t ids)
          (push restored result))))
    (setq result (nreverse result))
    (when result (excal--sync-indices result))
    (when (and repair-bindings result)
      (let ((map (excal--restore-id-map result)))
        (excal--restore-repair-references result map)
        (excal--restore-sticky-notes result map)
        ;; repairBoundTextElementOrder
        (let* ((positions (make-hash-table :test #'eq))
               (_ (cl-loop for e in result for i from 0 do (puthash e i positions)))
               (normalized (excal--restore-normalize-bound-elements-order result map))
               (moved (cl-loop for e in normalized for i from 0
                               when (and (equal (alist-get 'type e) "text")
                                         (stringp (alist-get 'containerId e))
                                         (/= (gethash e positions) i))
                               collect e)))
          (when moved (excal--sync-moved-indices moved normalized))
          (setq result normalized))
        (dolist (e result)
          (excal--restore-reroute-elbow e)
          (excal--restore-fix-self-bound-elbow e map))))
    result))

;;;; App state and documents

(defun excal--restore-app-state (app-state)
  "Return the restored APP-STATE alist of a loaded file.
Unlike upstream, which keeps only the exported keys, every key of
APP-STATE is kept (so saving loses nothing); the exported keys get their
defaults and normalisation, and a legacy numeric `currentItemStrokeWidth'
also sets `currentItemStrokeWidthKey'."
  (let ((state (copy-alist (if (listp app-state) app-state nil))))
    (pcase-dolist (`(,key . ,default) excal--restore-export-app-state-defaults)
      (unless (assq key state)
        (setq state (append state (list (cons key default))))))
    (dolist (key '(gridSize gridStep))
      (let ((v (alist-get key state)))
        (excal--put state key
                    (excal--restore-clamp (round (if (excal--restore-finite-p v) v
                                           (alist-get key excal--restore-export-app-state-defaults)))
                                  1 100))))
    (let ((width (excal--restore-js state 'currentItemStrokeWidth)))
      (unless (eq width :undefined)
        (excal--put state 'currentItemStrokeWidthKey
                    (or (excal--stroke-width-key width) "medium"))))
    state))

(defun excal--restore-doc (doc)
  "Return a restored copy of the parsed .excalidraw DOC.
Signal `user-error' if DOC is not an Excalidraw scene."
  (unless (and (listp doc) (equal (alist-get 'type doc) "excalidraw")
               (let ((elements (excal--restore-js doc 'elements)))
                 (or (eq elements :undefined) (vectorp elements))))
    (user-error "Not an Excalidraw scene"))
  (let ((doc (copy-alist doc)))
    (excal--put doc 'elements
                (vconcat (excal--restore-elements
                          (let ((els (alist-get 'elements doc)))
                            (if (vectorp els) els nil))
                          :repair-bindings t :delete-invisible t)))
    (excal--put doc 'appState (excal--restore-app-state (alist-get 'appState doc)))
    (let ((files (excal--restore-js doc 'files)))
      (unless (and (listp files) (not (keywordp files)))
        (excal--put doc 'files nil)))
    doc))

(provide 'excal-restore)
;;; excal-restore.el ends here
