;;; excali-restore.el --- Restore and migrate loaded scenes  -*- lexical-binding: t; -*-

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
(require 'excali-core)
(require 'excali-index)

;;;; Constants (packages/common/src/constants.ts, font-metadata.ts)

(defconst excali--restore-font-families
  '(("Virgil" . 1) ("Helvetica" . 2) ("Cascadia" . 3) ("Excalifont" . 5)
    ("Nunito" . 6) ("Lilita One" . 7) ("Comic Shanns" . 8)
    ("Liberation Sans" . 9) ("Assistant" . 10))
  "Upstream `FONT_FAMILY': font names and ids.")

(defconst excali--restore-default-font-family 5 "Excalifont.")
(defconst excali--restore-default-font-size 20)

(defconst excali--restore-font-line-heights
  '((1 . 1.25) (2 . 1.15) (3 . 1.2) (5 . 1.25) (6 . 1.25) (7 . 1.15)
    (8 . 1.25) (9 . 1.15) (10 . 1.25) (100 . 1.25) (1000 . 1.25))
  "Default unitless line height per font id (`FONT_METADATA').")

(defconst excali--restore-stroke-widths '(("thin" . 1) ("medium" . 2) ("bold" . 4))
  "`STROKE_WIDTH' for the keys in `STROKE_WIDTH_KEYS'.")

(defconst excali--restore-max-linear-px 75000 "`MAX_LINEAR_PX' in restore.ts.")
(defconst excali--restore-sticky-note-min-size 75)
(defconst excali--restore-default-sticky-note-size 250)
(defconst excali--restore-sticky-note-max-font-size 512)
(defconst excali--restore-sticky-note-fallback-font-size 28)
(defconst excali--restore-default-sticky-note-bg "#ffdf6b")
(defconst excali--restore-base-binding-gap 5)
(defconst excali--restore-fixed-point-bound 10)

(defconst excali--restore-export-app-state-defaults
  '((gridSize . 20) (gridStep . 5) (gridModeEnabled . :false)
    (viewBackgroundColor . "#ffffff") (lockedMultiSelections))
  "The app state keys upstream writes to files, with their defaults.")

(defun excali--restore-line-height-for (font-family)
  "Return the default line height of FONT-FAMILY (`getLineHeight')."
  (or (and (numberp font-family) (alist-get font-family excali--restore-font-line-heights))
      1.25))

(defun excali--stroke-width-key (width)
  "Return the `currentItemStrokeWidthKey' for numeric stroke WIDTH, or nil.
Port of `getStrokeWidthKey'; for legacy `currentItemStrokeWidth' values."
  (and (numberp width)
       (car (cl-find-if (lambda (entry) (= (cdr entry) width))
                        excali--restore-stroke-widths))))

;;;; JavaScript value semantics

(defsubst excali--restore-js (element key)
  "Return KEY of ELEMENT, or `:undefined' if it is missing."
  (alist-get key element :undefined))

(defsubst excali--restore-nullish-p (value)
  "Return non-nil if VALUE is null or undefined."
  (memq value '(:undefined :null)))

(defun excali--restore-falsy-p (value)
  "Return non-nil if VALUE is falsy in JavaScript."
  (or (memq value '(:undefined :null :false))
      (and (numberp value) (or (= value 0) (isnan (float value))))
      (equal value "")))

(defsubst excali--restore-or (value default)
  "JavaScript VALUE || DEFAULT."
  (if (excali--restore-falsy-p value) default value))

(defsubst excali--restore-nullish (value default)
  "JavaScript VALUE ?? DEFAULT."
  (if (excali--restore-nullish-p value) default value))

(defun excali--restore-finite-p (value)
  "Return non-nil if VALUE is a finite number."
  (and (numberp value)
       (or (integerp value)
           (not (or (isnan value) (= value 1.0e+INF) (= value -1.0e+INF))))))

(defun excali--restore-valid-point-p (point)
  "Return non-nil if POINT is a vector of two finite numbers."
  (and (vectorp point) (= (length point) 2)
       (excali--restore-finite-p (aref point 0)) (excali--restore-finite-p (aref point 1))))

(defun excali--restore-put-defined (element key value)
  "Set KEY of ELEMENT to VALUE, or remove KEY if VALUE is `:undefined'.
Return the possibly new ELEMENT."
  (if (eq value :undefined)
      (assq-delete-all key element)
    (excali--put element key value)
    element))

(defun excali--restore-clamp (value low high)
  "Clamp VALUE between LOW and HIGH."
  (max low (min high value)))

(defun excali--restore-parse-float (string)
  "Return JavaScript parseFloat of STRING, or nil for NaN."
  (and (stringp string)
       (string-match "\\`[ \t\n\r]*\\([-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\)"
                     string)
       (string-to-number (match-string 1 string))))

(defun excali--restore-transparent-p (color)
  "Return non-nil if COLOR has zero alpha (tinycolor `getAlpha' = 0)."
  (and (stringp color)
       (let ((c (downcase (string-trim color))))
         (or (equal c "transparent")
             (string-match-p "\\`#[0-9a-f]\\{3\\}0\\'" c)
             (string-match-p "\\`#[0-9a-f]\\{6\\}00\\'" c)
             (string-match-p "\\`\\(?:rgb\\|hsl\\)a?(.*[,/][ \t]*0*\\.?0*%?[ \t]*)\\'" c)))))

(defun excali--restore-normalize-link (link)
  "Port of `normalizeLink' (without URL canonicalisation)."
  (let ((link (string-trim link)))
    (cond
     ((string-empty-p link) link)
     ((string-match-p "\\`[^[:alnum:]]*\\(?:javascript\\|data\\|vbscript\\):"
                      (downcase (replace-regexp-in-string "[[:cntrl:][:space:]]" "" link)))
      "about:blank")
     (t (string-replace "\"" "&quot;" link)))))

(defun excali--restore-normalize-arrowhead (arrowhead)
  "Port of `normalizeArrowhead'."
  (pcase arrowhead
    ((or :undefined :null) :null)
    ("dot" "circle")
    ("crowfoot_one" "cardinality_one")
    ("crowfoot_many" "cardinality_many")
    ("crowfoot_one_or_many" "cardinality_one_or_many")
    (_ arrowhead)))

;;;; Geometry helpers for binding migration

(defun excali--restore-num (element key &optional default)
  "Return numeric KEY of ELEMENT, or DEFAULT (0)."
  (let ((v (alist-get key element)))
    (if (excali--restore-finite-p v) v (or default 0))))

(defun excali--restore-rotate (point center angle)
  "Rotate POINT (X . Y) about CENTER by ANGLE radians."
  (if (or (null angle) (= angle 0))
      point
    (let ((dx (- (car point) (car center))) (dy (- (cdr point) (cdr center)))
          (c (cos angle)) (s (sin angle)))
      (cons (+ (car center) (- (* dx c) (* dy s)))
            (+ (cdr center) (* dx s) (* dy c))))))

(defun excali--restore-shape-center (element)
  "Return the center (X . Y) of bindable ELEMENT."
  (cons (+ (excali--restore-num element 'x) (/ (excali--restore-num element 'width) 2.0))
        (+ (excali--restore-num element 'y) (/ (excali--restore-num element 'height) 2.0))))

(defun excali--restore-linear-point-global (arrow index)
  "Return point INDEX of ARROW in scene coordinates (rotation applied)."
  (let* ((points (alist-get 'points arrow))
         (x (excali--restore-num arrow 'x)) (y (excali--restore-num arrow 'y))
         (xs (mapcar (lambda (p) (aref p 0)) points))
         (ys (mapcar (lambda (p) (aref p 1)) points))
         (center (cons (+ x (/ (+ (apply #'min xs) (apply #'max xs)) 2.0))
                       (+ y (/ (+ (apply #'min ys) (apply #'max ys)) 2.0))))
         (p (and (< -1 index (length points)) (aref points index))))
    (excali--restore-rotate (if p (cons (+ x (aref p 0)) (+ y (aref p 1))) (cons x y))
                   center (excali--restore-num arrow 'angle))))

(defun excali--restore-point-in-shape-p (point element)
  "Return non-nil if POINT lies inside ELEMENT (`isPointInElement').
Rounded corners are ignored; lines and freedraw have no inside."
  (let ((type (alist-get 'type element)))
    (unless (member type '("line" "arrow" "freedraw"))
      (let* ((w (excali--restore-num element 'width)) (h (excali--restore-num element 'height))
             (x (excali--restore-num element 'x)) (y (excali--restore-num element 'y))
             (center (excali--restore-shape-center element))
             (p (excali--restore-rotate point center (- (excali--restore-num element 'angle))))
             (dx (- (car p) (car center))) (dy (- (cdr p) (cdr center)))
             (hw (/ w 2.0)) (hh (/ h 2.0)))
        (and (> w 0) (> h 0)
             (<= x (car p) (+ x w)) (<= y (cdr p) (+ y h))
             (pcase type
               ("diamond" (<= (+ (/ (abs dx) hw) (/ (abs dy) hh)) 1.0))
               ("ellipse" (<= (+ (/ (* dx dx) (* hw hw)) (/ (* dy dy) (* hh hh))) 1.0))
               (_ t)))))))

(defun excali--restore-segment-intersection (a1 a2 b1 b2)
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

(defun excali--restore-distance (a b)
  "Return the distance between points A and B."
  (sqrt (+ (expt (- (car a) (car b)) 2) (expt (- (cdr a) (cdr b)) 2))))

(defun excali--restore-normalize-fixed-point (fixed-point)
  "Port of `normalizeFixedPoint'."
  (if (not (and (vectorp fixed-point) (= (length fixed-point) 2)
                (excali--restore-finite-p (aref fixed-point 0))
                (excali--restore-finite-p (aref fixed-point 1))))
      (vector 0.5001 0.5001)
    (let* ((clamped (vconcat (mapcar (lambda (r) (excali--restore-clamp r (- excali--restore-fixed-point-bound)
                                                                excali--restore-fixed-point-bound))
                                     fixed-point)))
           (near (lambda (r) (< (abs (- r 0.5)) 0.0001))))
      (if (or (funcall near (aref clamped 0)) (funcall near (aref clamped 1)))
          (vconcat (mapcar (lambda (r) (if (funcall near r) 0.5001 r)) clamped))
        clamped))))

(defun excali--restore-global-fixed-point (fixed-point element)
  "Port of `getGlobalFixedPointForBindableElement'."
  (excali--restore-rotate (cons (+ (excali--restore-num element 'x)
                          (* (excali--restore-num element 'width) (aref fixed-point 0)))
                       (+ (excali--restore-num element 'y)
                          (* (excali--restore-num element 'height) (aref fixed-point 1))))
                 (excali--restore-shape-center element) (excali--restore-num element 'angle)))

(defun excali--restore-snap-outline-midpoint (point element)
  "Port of `getSnapOutlineMidPoint' for simple arrows at zoom 1."
  (let* ((x (excali--restore-num element 'x)) (y (excali--restore-num element 'y))
         (w (excali--restore-num element 'width)) (h (excali--restore-num element 'height))
         (center (excali--restore-shape-center element))
         (angle (excali--restore-num element 'angle))
         ;; maxBindingDistance_simple(1) = clamp(15 / 1.5, 15, 30) = 15
         (threshold (+ 15 (/ (excali--restore-num element 'strokeWidth) 2.0))))
    (cl-find-if (lambda (mid)
                  (and (<= (excali--restore-distance mid point) threshold)
                       (not (excali--restore-point-in-shape-p point element))))
                (mapcar (lambda (p) (excali--restore-rotate p center angle))
                        (list (cons (+ x w) (+ y (/ h 2.0)))
                              (cons (+ x (/ w 2.0)) (+ y h))
                              (cons x (+ y (/ h 2.0)))
                              (cons (+ x (/ w 2.0)) y))))))

(defun excali--restore-element-diagonals (element)
  "Port of `getDiagonalsForBindableElement'."
  (let* ((x (excali--restore-num element 'x)) (y (excali--restore-num element 'y))
         (w (excali--restore-num element 'width)) (h (excali--restore-num element 'height))
         (center (excali--restore-shape-center element))
         (angle (excali--restore-num element 'angle))
         (offset (if (equal (alist-get 'type element) "rectangle") 15 0))
         (rot (lambda (px py) (excali--restore-rotate (cons px py) center angle)))
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

(defun excali--restore-project-fixed-point (arrow point element start elements-map)
  "Port of `projectFixedPointOntoDiagonal' at zoom 1, or nil.
ARROW is the arrow with restored points, POINT its endpoint, ELEMENT
the bound element, START non-nil for the start binding."
  (or (excali--restore-snap-outline-midpoint point element)
      (let ((aw (alist-get 'width arrow)) (ah (alist-get 'height arrow)))
        (unless (and (numberp aw) (< aw 3) (numberp ah) (< ah 3))
          (pcase-let* ((points (alist-get 'points arrow))
                       (`(,d1 ,d2) (excali--restore-element-diagonals element))
                       (a (excali--restore-linear-point-global
                           arrow (if start 1 (- (length points) 2)))))
            (when (= (length points) 2)
              (let* ((other (alist-get (if start 'endBinding 'startBinding) arrow))
                     (other-id (and (consp other) (alist-get 'elementId other)))
                     (bindable (and (stringp other-id) (gethash other-id elements-map))))
                (when bindable
                  (setq a (excali--restore-global-fixed-point
                           (excali--restore-normalize-fixed-point (alist-get 'fixedPoint other))
                           bindable)))))
            (let* ((scale (+ (* 2 (excali--restore-distance a point))
                             (max (excali--restore-distance (nth 0 d1) (nth 1 d1))
                                  (excali--restore-distance (nth 0 d2) (nth 1 d2)))))
                   (b (cons (+ (car a) (* scale (- (car point) (car a))))
                            (+ (cdr a) (* scale (- (cdr point) (cdr a))))))
                   (p1 (excali--restore-segment-intersection (nth 0 d1) (nth 1 d1) b a))
                   (p2 (excali--restore-segment-intersection (nth 0 d2) (nth 1 d2) b a))
                   (projection (cond ((and p1 p2)
                                      (if (< (excali--restore-distance a p1) (excali--restore-distance a p2))
                                          p1 p2))
                                     (t (or p1 p2)))))
              (and projection (excali--restore-point-in-shape-p projection element)
                   projection)))))))

(defun excali--restore-fixed-point-for (element focus-point)
  "Port of `calculateFixedPointForNonElbowArrowBinding' for FOCUS-POINT."
  (let* ((p (excali--restore-rotate focus-point (excali--restore-shape-center element)
                           (- (excali--restore-num element 'angle))))
         (w (excali--restore-num element 'width)) (h (excali--restore-num element 'height)))
    (if (or (< w 1) (< h 1))
        (excali--restore-normalize-fixed-point (vector 0.5 0.5))
      (let ((gap (+ excali--restore-base-binding-gap
                    (/ (excali--restore-num element 'strokeWidth) 2.0))))
        (excali--restore-normalize-fixed-point
         (vector (/ (- (car p) (excali--restore-num element 'x)) (float (max w gap)))
                 (/ (- (cdr p) (excali--restore-num element 'y)) (float (max h gap)))))))))

(defun excali--restore-repair-binding (arrow binding targets existing start)
  "Port of `repairBinding' for BINDING of ARROW (with restored points).
TARGETS and EXISTING map ids to the raw loaded and to existing elements.
START is non-nil for the start binding."
  (condition-case nil
      (cond
       ((or (excali--restore-nullish-p binding) (eq binding :false)) :null)
       ((eq (alist-get 'elbowed arrow) t)
        (let ((b (copy-alist binding)))
          (excali--put b 'fixedPoint (excali--restore-normalize-fixed-point
                                     (alist-get 'fixedPoint b)))
          (excali--put b 'mode (excali--restore-or (excali--restore-js binding 'mode) "orbit"))
          b))
       ((not (excali--restore-falsy-p (excali--restore-js binding 'mode)))
        (if (excali--restore-falsy-p (excali--restore-js binding 'elementId))
            :null
          (list (cons 'elementId (alist-get 'elementId binding))
                (cons 'mode (alist-get 'mode binding))
                (cons 'fixedPoint (excali--restore-normalize-fixed-point
                                   (alist-get 'fixedPoint binding))))))
       (t
        (let* ((id (alist-get 'elementId binding))
               (target (and (stringp id) (gethash id targets)))
               (bound (or target (and existing (stringp id) (gethash id existing))))
               (map (if target targets existing)))
          (if (not (and bound map))
              :null
            (let* ((points (alist-get 'points arrow))
                   (p (excali--restore-linear-point-global
                       arrow (if start 0 (1- (length points)))))
                   (mode (if (excali--restore-point-in-shape-p p bound) "inside" "orbit"))
                   (safe (copy-alist arrow)))
              (dolist (key '(startBinding endBinding))
                (let ((b (alist-get key arrow)))
                  (excali--put safe key
                              (if (and (consp b) (not (excali--restore-falsy-p (excali--restore-js b 'elementId))))
                                  (let ((b (copy-alist b)))
                                    (excali--put b 'mode mode)
                                    (excali--put b 'fixedPoint (excali--restore-normalize-fixed-point
                                                               (alist-get 'fixedPoint b)))
                                    b)
                                :null))))
              (let ((focus (if (equal mode "inside")
                               p
                             (or (excali--restore-project-fixed-point safe p bound start map) p))))
                (list (cons 'mode mode)
                      (cons 'elementId id)
                      (cons 'fixedPoint (excali--restore-fixed-point-for bound focus)))))))))
    (error :null)))

;;;; Elements

(defun excali--restore-base (element extra)
  "Port of `restoreElementWithProperties'.
ELEMENT is a fresh copy of the loaded alist, EXTRA an alist of
per-type fields (values may be `:undefined' to drop a key).  Return the
restored alist, reusing ELEMENT's cells and key order."
  (let* ((get (lambda (key) (excali--restore-js element key)))
         (type (excali--restore-or (alist-get 'type extra :undefined) (funcall get 'type)))
         (width (excali--restore-or (funcall get 'width) 0))
         (height (excali--restore-or (funcall get 'height) 0))
         (x (excali--restore-nullish (alist-get 'x extra :undefined)
                            (excali--restore-nullish (funcall get 'x) 0)))
         (y (excali--restore-nullish (alist-get 'y extra :undefined)
                            (excali--restore-nullish (funcall get 'y) 0)))
         (roundness (funcall get 'roundness))
         (bound-ids (funcall get 'boundElementIds))
         (link (funcall get 'link))
         (base
          `((type . ,type)
            (version . ,(excali--restore-or (funcall get 'version) 1))
            (versionNonce . ,(excali--restore-nullish (funcall get 'versionNonce) 0))
            (index . ,(excali--restore-nullish (funcall get 'index) :null))
            (isDeleted . ,(excali--restore-nullish (funcall get 'isDeleted) :false))
            (id . ,(excali--restore-or (funcall get 'id) (excali--new-id)))
            (fillStyle . ,(excali--restore-or (funcall get 'fillStyle) "solid"))
            (strokeWidth . ,(excali--restore-or (funcall get 'strokeWidth) 2))
            (strokeStyle . ,(excali--restore-nullish (funcall get 'strokeStyle) "solid"))
            (roughness . ,(excali--restore-nullish (funcall get 'roughness) 1))
            (opacity . ,(excali--restore-nullish (funcall get 'opacity) 100))
            (angle . ,(excali--restore-or (funcall get 'angle) 0))
            (x . ,x) (y . ,y)
            (strokeColor . ,(excali--restore-or (funcall get 'strokeColor) "#1e1e1e"))
            (backgroundColor . ,(excali--restore-or (funcall get 'backgroundColor) "transparent"))
            (width . ,width) (height . ,height)
            (seed . ,(excali--restore-nullish (funcall get 'seed) 1))
            (groupIds . ,(excali--restore-nullish (funcall get 'groupIds) []))
            (frameId . ,(excali--restore-nullish (funcall get 'frameId) :null))
            (roundness
             . ,(cond ((not (excali--restore-falsy-p roundness)) roundness)
                      ((equal (funcall get 'strokeSharpness) "round")
                       (list (cons 'type (if (member type '("rectangle" "embeddable"
                                                             "iframe" "image"))
                                             1 2))))
                      (t :null)))
            (boundElements
             . ,(if (not (excali--restore-falsy-p bound-ids))
                    (vconcat (mapcar (lambda (id) (list (cons 'type "arrow") (cons 'id id)))
                                     bound-ids))
                  (excali--restore-nullish (funcall get 'boundElements) [])))
            (updated . ,(excali--restore-nullish (funcall get 'updated)
                                        (truncate (* 1000 (float-time)))))
            (created . ,(excali--restore-nullish (funcall get 'created) :null))
            (link . ,(if (and (stringp link) (not (string-empty-p link)))
                         (excali--restore-normalize-link link)
                       (if (excali--restore-falsy-p link) :null link)))
            (locked . ,(excali--restore-nullish (funcall get 'locked) :false)))))
    ;; getNormalizedDimensions
    (when (and (numberp width) (< width 0))
      (setf (alist-get 'width base) (abs width)
            (alist-get 'x base) (- x (abs width))))
    (when (and (numberp height) (< height 0))
      (setf (alist-get 'height base) (abs height)
            (alist-get 'y base) (- y (abs height))))
    (pcase-dolist (`(,key . ,value) base)
      (excali--put element key value))
    (pcase-dolist (`(,key . ,value) extra)
      (setq element (excali--restore-put-defined element key value)))
    (dolist (key '(strokeSharpness boundElementIds))
      (setq element (assq-delete-all key element)))
    element))

(defun excali--restore-points (points width height)
  "Port of `restoreLinearElementPoints'."
  (let ((valid (and (vectorp points)
                    (cl-loop for p across points
                             when (excali--restore-valid-point-p p)
                             collect (vector (aref p 0) (aref p 1))))))
    (if (< (length valid) 2)
        (vector (vector 0 0)
                (vector (if (excali--restore-finite-p width) width 0)
                        (if (excali--restore-finite-p height) height 0)))
      (vconcat valid))))

(defun excali--restore-normalize-points (points x y)
  "Shift POINTS so the first is the origin; return (POINTS X Y)."
  (let ((ox (aref (aref points 0) 0)) (oy (aref (aref points 0) 1)))
    (if (and (= ox 0) (= oy 0))
        (list points x y)
      (list (vconcat (mapcar (lambda (p) (vector (- (aref p 0) ox) (- (aref p 1) oy)))
                             points))
            (+ x ox) (+ y oy)))))

(defun excali--restore-points-size (points)
  "Return (WIDTH . HEIGHT) of POINTS (`getSizeFromPoints')."
  (let ((xs (mapcar (lambda (p) (aref p 0)) points))
        (ys (mapcar (lambda (p) (aref p 1)) points)))
    (cons (- (apply #'max xs) (apply #'min xs))
          (- (apply #'max ys) (apply #'min ys)))))

(defun excali--restore-points-equal-p (a b tolerance)
  "Return non-nil if points A and B are within TOLERANCE on both axes."
  (and (<= (abs (- (aref a 0) (aref b 0))) tolerance)
       (<= (abs (- (aref a 1) (aref b 1))) tolerance)))

(defun excali--restore-handle-oversized (element)
  "Port of `handleOversizedLinearElements'."
  (if (and (<= (alist-get 'width element) excali--restore-max-linear-px)
           (<= (alist-get 'height element) excali--restore-max-linear-px))
      element
    (message "excali: removing oversized %s %s" (alist-get 'type element)
             (alist-get 'id element))
    (dolist (field `((x . 0) (y . 0) (width . 100) (height . 100)
                     (points . ,(vector (vector 0 0) (vector 100 100)))
                     (isDeleted . t)))
      (excali--put element (car field) (cdr field)))
    element))

(defun excali--restore-bump-version (element &optional version)
  "Port of `bumpVersion': increment ELEMENT's version (from VERSION)."
  (excali--put element 'version
              (1+ (or version (let ((v (alist-get 'version element)))
                                (if (numberp v) v 0)))))
  (excali--put element 'versionNonce (random (ash 1 31)))
  (excali--put element 'updated (truncate (* 1000 (float-time))))
  element)

(defun excali--restore-text (element delete-invisible)
  "Restore text ELEMENT; with DELETE-INVISIBLE, delete it if empty."
  (setq element (assq-delete-all 'rawText element))
  (let* ((font-size (excali--restore-js element 'fontSize))
         (font-family (excali--restore-js element 'fontFamily))
         (font (excali--restore-js element 'font))
         (raw-text (excali--restore-js element 'text)))
    (unless (eq font :undefined)
      (let ((parts (split-string (if (stringp font) font "") " ")))
        (setq font-size (or (excali--restore-parse-float (car parts)) :undefined)
              font-family (or (cdr (assoc (cadr parts) excali--restore-font-families))
                              excali--restore-default-font-family))))
    (unless (excali--restore-finite-p font-size)
      (setq font-size excali--restore-default-font-size))
    (let* ((text (if (and (stringp raw-text) (not (string-empty-p raw-text)))
                     raw-text ""))
           (height (excali--restore-js element 'height))
           (line-height
            (let ((own (excali--restore-js element 'lineHeight)))
              (cond
               ((not (excali--restore-falsy-p own)) own)
               ((or (excali--restore-falsy-p height) (not (excali--restore-finite-p height)))
                (excali--restore-line-height-for (excali--restore-js element 'fontFamily)))
               (t
                ;; detectLineHeight
                (let ((raw-size (excali--restore-js element 'fontSize)))
                  (/ (float height)
                     (length (split-string
                              (replace-regexp-in-string
                               "\r\n?" "\n" (if (stringp raw-text) raw-text ""))
                              "\n"))
                     (if (excali--restore-finite-p raw-size) raw-size font-size)))))))
           (label (excali--restore-js element 'labelPosition))
           (base-size (excali--restore-js element 'baseFontSize)))
      (setq element
            (excali--restore-base
             element
             `((fontSize . ,font-size)
               (fontFamily . ,font-family)
               (text . ,text)
               (textAlign . ,(excali--restore-or (excali--restore-js element 'textAlign) "left"))
               (verticalAlign . ,(excali--restore-or (excali--restore-js element 'verticalAlign) "top"))
               (containerId . ,(excali--restore-nullish (excali--restore-js element 'containerId) :null))
               (originalText . ,(excali--restore-or (excali--restore-js element 'originalText) text))
               (autoResize . ,(excali--restore-nullish (excali--restore-js element 'autoResize) t))
               (lineHeight . ,line-height)
               (labelPosition . ,(if (excali--restore-finite-p label) (excali--restore-clamp label 0 1) :null))
               (baseFontSize . ,(if (excali--restore-finite-p base-size)
                                    (excali--restore-clamp base-size 1 excali--restore-sticky-note-max-font-size)
                                  :null)))))
      (when (and delete-invisible (string-empty-p text)
                 (not (eq (alist-get 'isDeleted element) t)))
        (excali--put element 'originalText text)
        (excali--put element 'isDeleted t)
        (excali--restore-bump-version element))
      element)))

(defun excali--restore-linear (element targets existing)
  "Restore line, legacy draw or arrow ELEMENT.
TARGETS and EXISTING are id maps used to migrate arrow bindings."
  (let* ((type (alist-get 'type element))
         (arrow (equal type "arrow"))
         (x (excali--restore-js element 'x)) (y (excali--restore-js element 'y))
         (points (excali--restore-points (excali--restore-js element 'points)
                                        (excali--restore-js element 'width)
                                        (excali--restore-js element 'height))))
    (if (not arrow)
        (pcase-let* ((`(,points ,x ,y)
                      (excali--restore-normalize-points points (excali--restore-nullish x 0)
                                               (excali--restore-nullish y 0)))
                     (size (excali--restore-points-size points)))
          (excali--restore-handle-oversized
           (excali--restore-base
            element
            `((type . "line") (startBinding . :null) (endBinding . :null)
              (startArrowhead . ,(excali--restore-normalize-arrowhead
                                  (excali--restore-js element 'startArrowhead)))
              (endArrowhead . ,(excali--restore-normalize-arrowhead
                                (excali--restore-js element 'endArrowhead)))
              (points . ,points) (x . ,x) (y . ,y)
              ;; Upstream adds `polygon' only to "line", not legacy "draw"
              ;; elements; adding it to both keeps restore idempotent.
              ,@(progn
                  (let ((polygon (excali--restore-js element 'polygon)))
                    `((polygon . ,(if (and (> (length points) 3)
                                           (excali--restore-points-equal-p
                                            (aref points 0)
                                            (aref points (1- (length points)))
                                            1e-4))
                                      (excali--restore-nullish polygon :false)
                                    :false)))))
              (width . ,(car size)) (height . ,(cdr size))))))
      (let* ((x (excali--restore-nullish x 0)) (y (excali--restore-nullish y 0))
             (with-points (copy-alist element))
             (elbow (eq (alist-get 'elbowed element) t))
             (end-head (excali--restore-js element 'endArrowhead))
             (size (excali--restore-points-size points)))
        (excali--put with-points 'points points)
        (excali--put with-points 'x x)
        (excali--put with-points 'y y)
        (setq element
              (excali--restore-base
               element
               `((type . ,type)
                 (startBinding . ,(excali--restore-repair-binding
                                   with-points (excali--restore-js element 'startBinding)
                                   targets existing t))
                 (endBinding . ,(excali--restore-repair-binding
                                 with-points (excali--restore-js element 'endBinding)
                                 targets existing nil))
                 (startArrowhead . ,(excali--restore-normalize-arrowhead
                                     (excali--restore-js element 'startArrowhead)))
                 (endArrowhead . ,(if (eq end-head :undefined) "arrow"
                                    (excali--restore-normalize-arrowhead end-head)))
                 (points . ,points) (x . ,x) (y . ,y)
                 (elbowed . ,(if elbow t (excali--restore-js element 'elbowed)))
                 (width . ,(car size)) (height . ,(cdr size))
                 ,@(when elbow
                     (let ((segments (excali--restore-js element 'fixedSegments)))
                       `((fixedSegments . ,(if (and (vectorp segments)
                                                    (> (length segments) 0)
                                                    (>= (length points) 4))
                                               segments :null))
                         (startIsSpecial . ,(excali--restore-js element 'startIsSpecial))
                         (endIsSpecial . ,(excali--restore-js element 'endIsSpecial))))))))
        (pcase-let ((`(,points ,x ,y)
                     (excali--restore-normalize-points (alist-get 'points element)
                                              (alist-get 'x element)
                                              (alist-get 'y element))))
          (excali--put element 'points points)
          (excali--put element 'x x)
          (excali--put element 'y y))
        (excali--restore-handle-oversized element)))))

(defun excali--restore-freedraw-points (points pressures)
  "Port of `restoreFreedrawPoints': return (POINTS . PRESSURES)."
  (if (not (vectorp points))
      (cons [] [])
    (let ((pressures (if (vectorp pressures) pressures []))
          kept kept-pressures)
      (cl-loop for p across points for i from 0
               when (excali--restore-valid-point-p p)
               do (push (vector (aref p 0) (aref p 1)) kept)
               (when (< i (length pressures))
                 (let ((v (aref pressures i)))
                   (push (if (excali--restore-finite-p v) v 0.5) kept-pressures))))
      (cons (vconcat (nreverse kept)) (vconcat (nreverse kept-pressures))))))

(defun excali--restore-element (element targets existing delete-invisible)
  "Port of `restoreElement': return the restored copy of ELEMENT.
Return nil for `selection' elements and ELEMENT itself (untouched) for
unknown types.  TARGETS and EXISTING map ids to elements."
  (let ((type (alist-get 'type element)))
    (if (not (member type (cons "draw" excali--indexed-types)))
        element
      (let ((element (copy-alist element)))
        (pcase type
          ("selection" nil)
          ("text" (excali--restore-text element delete-invisible))
          ("freedraw"
           (let ((restored (excali--restore-freedraw-points
                            (excali--restore-js element 'points) (excali--restore-js element 'pressures)))
                 (options (excali--restore-js element 'strokeOptions)))
             (excali--restore-base
              element
              `((points . ,(car restored))
                (simulatePressure . ,(excali--restore-js element 'simulatePressure))
                (strokeOptions
                 . ((variability . ,(let ((v (and (consp options)
                                                  (alist-get 'variability options))))
                                      (if (member v '("variable" "constant")) v "variable")))
                    (streamline . ,(let ((v (and (consp options)
                                                 (alist-get 'streamline options))))
                                     (if (excali--restore-finite-p v) v 0.5)))))
                (pressures . ,(cdr restored))))))
          ("image"
           (excali--restore-base
            element
            `((status . ,(excali--restore-or (excali--restore-js element 'status) "pending"))
              (fileId . ,(excali--restore-js element 'fileId))
              (scale . ,(excali--restore-or (excali--restore-js element 'scale) [1 1]))
              (crop . ,(excali--restore-nullish (excali--restore-js element 'crop) :null)))))
          ((or "line" "draw" "arrow") (excali--restore-linear element targets existing))
          ((or "rectangle" "ellipse" "diamond" "iframe" "embeddable")
           (excali--restore-base element nil))
          ("stickynote"
           (excali--restore-normalize-sticky-note
            (excali--restore-base
             element
             `((baseHeight . ,(excali--restore-nullish
                               (excali--restore-js element 'baseHeight)
                               (excali--restore-nullish (excali--restore-js element 'maxHeight)
                                               (excali--restore-js element 'height))))))))
          ((or "frame" "magicframe")
           (excali--restore-base
            element `((name . ,(excali--restore-nullish (excali--restore-js element 'name) :null))))))))))

(defun excali--restore-normalize-sticky-note (element)
  "Port of `normalizeStickyNote' (style, then geometry)."
  (let ((bg (alist-get 'backgroundColor element))
        (stroke (alist-get 'strokeColor element)))
    (excali--put element 'backgroundColor
                (if (or (not (stringp bg)) (string-empty-p bg) (excali--restore-transparent-p bg))
                    excali--restore-default-sticky-note-bg bg))
    (excali--put element 'strokeColor (excali--restore-sticky-stroke stroke))
    (excali--put element 'fillStyle "solid")
    (let* ((width (max (excali--restore-num element 'width) excali--restore-sticky-note-min-size))
           (height (excali--restore-num element 'height))
           (base (max (excali--restore-or (alist-get 'baseHeight element)
                                 (excali--restore-or height excali--restore-default-sticky-note-size))
                      excali--restore-sticky-note-min-size)))
      (excali--put element 'width width)
      (excali--put element 'height (max height base))
      (excali--put element 'baseHeight base)
      element)))

(defun excali--restore-sticky-stroke (color)
  "Port of `normalizeStickyNoteStrokeColor'."
  (if (or (not (stringp color)) (string-empty-p color) (excali--restore-transparent-p color))
      "#1e1e1e"
    color))

(defun excali--restore-invisibly-small-p (element)
  "Port of `isInvisiblySmallElement' for the loaded ELEMENT."
  (let ((type (alist-get 'type element)))
    (if (member type '("line" "arrow" "freedraw"))
        (let* ((points (alist-get 'points element))
               (n (if (vectorp points) (length points) 0)))
          (or (< n 2)
              (and (= n 2) (equal type "arrow")
                   (excali--restore-valid-point-p (aref points 0))
                   (excali--restore-valid-point-p (aref points 1))
                   (excali--restore-points-equal-p (aref points 0) (aref points 1) 0.1))))
      (let ((w (alist-get 'width element)) (h (alist-get 'height element)))
        (and (numberp w) (= w 0) (numberp h) (= h 0))))))

(defun excali--restore-id-map (elements)
  "Return a hash table mapping ids to ELEMENTS (later ones win)."
  (let ((map (make-hash-table :test #'equal)))
    (dolist (e elements map)
      (let ((id (alist-get 'id e)))
        (when (stringp id) (puthash id e map))))))

(defun excali--restore-known-p (element)
  "Return non-nil if restore understands ELEMENT's type."
  (excali--indexed-p element))

(defun excali--restore-repair-references (elements map)
  "Repair frame, container and binding references of ELEMENTS in MAP."
  (dolist (e elements)
    (when (excali--restore-known-p e)
      (let ((frame (alist-get 'frameId e)))
        (when (and (stringp frame) (not (string-empty-p frame))
                   (not (gethash frame map)))
          (excali--put e 'frameId :null)))
      (let ((container-id (alist-get 'containerId e)))
        (cond
         ((and (equal (alist-get 'type e) "text")
               (stringp container-id) (not (string-empty-p container-id)))
          ;; repairBoundElement
          (let ((container (gethash container-id map)))
            (excali--put e 'angle
                        (if (or (null container)
                                (equal (alist-get 'type container) "arrow"))
                            0
                          (excali--restore-nullish (excali--restore-js container 'angle) 0)))
            (cond
             ((null container) (excali--put e 'containerId :null))
             ((eq (alist-get 'isDeleted e) t))
             ((and (excali--restore-known-p container)
                   (vectorp (alist-get 'boundElements container))
                   (not (cl-find (alist-get 'id e) (alist-get 'boundElements container)
                                 :key (lambda (b) (and (consp b) (alist-get 'id b)))
                                 :test #'equal)))
              (excali--put container 'boundElements
                          (vconcat (alist-get 'boundElements container)
                                   (list (list (cons 'type "text")
                                               (cons 'id (alist-get 'id e))))))))))
         ((not (excali--restore-falsy-p (excali--restore-js e 'boundElements)))
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
                               (excali--restore-known-p bound)
                               (excali--restore-falsy-p (excali--restore-js bound 'containerId)))
                      (excali--put bound 'containerId (alist-get 'id e)))))))
            (excali--put e 'boundElements (vconcat (nreverse kept)))))))
      (when (member (alist-get 'type e) '("line" "arrow"))
        (dolist (key '(startBinding endBinding))
          (let ((b (alist-get key e)))
            (when (and (consp b)
                       (or (not (gethash (alist-get 'elementId b) map))
                           (not (equal (alist-get 'type e) "arrow"))))
              (excali--put e key :null))))))))

(defun excali--restore-sticky-notes (elements map)
  "Port of `restoreStickyNotes' without re-layout."
  (dolist (e elements)
    (when (and (equal (alist-get 'type e) "text")
               (not (eq (alist-get 'isDeleted e) t)))
      (let* ((cid (alist-get 'containerId e))
             (container (and (stringp cid) (gethash cid map))))
        (if (and container (equal (alist-get 'type container) "stickynote"))
            (let* ((own (alist-get 'strokeColor e))
                   (stroke (excali--restore-sticky-stroke
                            (if (excali--restore-transparent-p own)
                                (alist-get 'strokeColor container)
                              own)))
                   (size (excali--restore-nullish (excali--restore-js e 'baseFontSize)
                                         (alist-get 'fontSize e))))
              (excali--put e 'baseFontSize
                          (if (excali--restore-finite-p size)
                              (excali--restore-clamp size 1 excali--restore-sticky-note-max-font-size)
                            excali--restore-sticky-note-fallback-font-size))
              (excali--put e 'strokeColor stroke)
              (unless (equal (alist-get 'strokeColor container) stroke)
                (excali--put container 'strokeColor stroke)))
          (unless (excali--restore-nullish-p (excali--restore-js e 'baseFontSize))
            (excali--put e 'baseFontSize :null)))))))

(defun excali--restore-normalize-bound-elements-order (elements map)
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
      (message "excali: normalizeBoundElementsOrder lost some elements")
      elements)))

(defun excali--restore-fix-self-bound-elbow (e map)
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
        (let ((bw (excali--restore-num b 'width)) (bh (excali--restore-num b 'height)))
          (excali--put e 'x (+ (excali--restore-num b 'x) (/ bw 2.0)))
          (excali--put e 'y (- (excali--restore-num b 'y) 5))
          (excali--put e 'width bw)
          (excali--put e 'height bh)
          (excali--put e 'points (vector (vector 0 0) (vector 0 -10)
                                        (vector (+ (/ bw 2.0) 5) -10)
                                        (vector (+ (/ bw 2.0) 5) (+ (/ bh 2.0) 5)))))))))

(declare-function excali--elbow-update "excali-elbow")
(declare-function excali--validate-elbow-points "excali-elbow")

(defun excali--restore-reroute-elbow (e)
  "Re-route E if it is an unbound elbow arrow with non-orthogonal points.
The route keeps E's ends; version, nonce and time stay, as upstream
spreads `updateElbowArrowPoints' into the element without bumping it."
  (let ((points (alist-get 'points e)))
    (when (and (equal (alist-get 'type e) "arrow") (eq (alist-get 'elbowed e) t)
               (not (consp (alist-get 'startBinding e)))
               (not (consp (alist-get 'endBinding e)))
               (fboundp 'excali--elbow-update)
               (vectorp points) (> (length points) 1)
               (not (excali--validate-elbow-points
                     (mapcar (lambda (p) (vector (float (elt p 0)) (float (elt p 1))))
                             points))))
      (let ((kept (mapcar (lambda (key) (cons key (alist-get key e)))
                          '(version versionNonce updated)))
            (last (aref points (1- (length points)))))
        (excali--elbow-update e (list :points (list (vector 0.0 0.0)
                                                   (vector (float (elt last 0))
                                                           (float (elt last 1))))))
        (pcase-dolist (`(,key . ,value) kept)
          (excali--put e key value))))))

(cl-defun excali--restore-elements (elements &key existing repair-bindings
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
         (targets (excali--restore-id-map elements))
         (existing-map (and existing (excali--restore-id-map existing)))
         (ids (make-hash-table :test #'equal))
         result)
    (dolist (element elements)
      (let ((restored (condition-case err
                          (excali--restore-element element targets existing-map
                                                  delete-invisible)
                        (error (message "excali: error restoring element: %S" err)
                               nil))))
        (when restored
          (when (excali--restore-known-p restored)
            (when (and delete-invisible (excali--restore-invisibly-small-p element))
              (let ((local (and existing-map
                                (gethash (alist-get 'id element) existing-map))))
                (excali--restore-bump-version restored
                                     (and local (numberp (alist-get 'version local))
                                          (alist-get 'version local))))
              (excali--put restored 'isDeleted t))
            (when (gethash (alist-get 'id restored) ids)
              (excali--put restored 'id (excali--new-id))))
          (puthash (alist-get 'id restored) t ids)
          (push restored result))))
    (setq result (nreverse result))
    (when result (excali--sync-indices result))
    (when (and repair-bindings result)
      (let ((map (excali--restore-id-map result)))
        (excali--restore-repair-references result map)
        (excali--restore-sticky-notes result map)
        ;; repairBoundTextElementOrder
        (let* ((positions (make-hash-table :test #'eq))
               (_ (cl-loop for e in result for i from 0 do (puthash e i positions)))
               (normalized (excali--restore-normalize-bound-elements-order result map))
               (moved (cl-loop for e in normalized for i from 0
                               when (and (equal (alist-get 'type e) "text")
                                         (stringp (alist-get 'containerId e))
                                         (/= (gethash e positions) i))
                               collect e)))
          (when moved (excali--sync-moved-indices moved normalized))
          (setq result normalized))
        (dolist (e result)
          (excali--restore-reroute-elbow e)
          (excali--restore-fix-self-bound-elbow e map))))
    result))

;;;; App state and documents

(defun excali--restore-app-state (app-state)
  "Return the restored APP-STATE alist of a loaded file.
Unlike upstream, which keeps only the exported keys, every key of
APP-STATE is kept (so saving loses nothing); the exported keys get their
defaults and normalisation, and a legacy numeric `currentItemStrokeWidth'
also sets `currentItemStrokeWidthKey'."
  (let ((state (copy-alist (if (listp app-state) app-state nil))))
    (pcase-dolist (`(,key . ,default) excali--restore-export-app-state-defaults)
      (unless (assq key state)
        (setq state (append state (list (cons key default))))))
    (dolist (key '(gridSize gridStep))
      (let ((v (alist-get key state)))
        (excali--put state key
                    (excali--restore-clamp (round (if (excali--restore-finite-p v) v
                                           (alist-get key excali--restore-export-app-state-defaults)))
                                  1 100))))
    (let ((width (excali--restore-js state 'currentItemStrokeWidth)))
      (unless (eq width :undefined)
        (excali--put state 'currentItemStrokeWidthKey
                    (or (excali--stroke-width-key width) "medium"))))
    state))

(defun excali--restore-doc (doc)
  "Return a restored copy of the parsed .excalidraw DOC.
Signal `user-error' if DOC is not an Excalidraw scene."
  (unless (and (listp doc) (equal (alist-get 'type doc) "excalidraw")
               (let ((elements (excali--restore-js doc 'elements)))
                 (or (eq elements :undefined) (vectorp elements))))
    (user-error "Not an Excalidraw scene"))
  (let ((doc (copy-alist doc)))
    (excali--put doc 'elements
                (vconcat (excali--restore-elements
                          (let ((els (alist-get 'elements doc)))
                            (if (vectorp els) els nil))
                          :repair-bindings t :delete-invisible t)))
    (excali--put doc 'appState (excali--restore-app-state (alist-get 'appState doc)))
    (let ((files (excali--restore-js doc 'files)))
      (unless (and (listp files) (not (keywordp files)))
        (excali--put doc 'files nil)))
    doc))

(provide 'excali-restore)
;;; excali-restore.el ends here
