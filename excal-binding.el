;;; excal-binding.el --- Binding arrows to shapes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Arrow binding following Excalidraw's simple binding path (the default;
;; docs/excalidraw-spec.md §3b.7, §2c.10).  A binding is a
;; FixedPointBinding: the bound element's id, a `fixedPoint' as ratios of
;; its unrotated box, and a mode.  "inside" puts the arrow end on the
;; fixed point; "orbit" puts it where the line from the fixed point
;; toward the arrow's other end crosses the shape's outline grown by the
;; binding gap.  Moving, resizing or rotating a shape recomputes the ends
;; of the arrows bound to it.

;;; Code:

(require 'excal-core)
(require 'excal-select)
(require 'excal-handles)
(require 'excal-hit)
(require 'excal-text)

(defconst excal--binding-highlight-color "#6abdfc" "BINDING_HIGHLIGHT_RGB.")
(defconst excal--base-binding-gap 5 "BASE_BINDING_GAP.")
(defconst excal--base-arrow-min-length 10 "BASE_ARROW_MIN_LENGTH.")
(defconst excal--bind-mode-timeout 0.7
  "BIND_MODE_TIMEOUT in seconds: hovering this long binds \"inside\".")

(defvar-local excal--binding-highlight nil
  "Element that the arrow end being drawn would bind to, or nil.")

;;;; Candidates

(defun excal--bindable-p (element)
  "Return non-nil if arrows can bind to ELEMENT."
  (and (member (excal--get element 'type)
               '("rectangle" "diamond" "ellipse" "text" "image" "frame" "stickynote"))
       (not (excal--get element 'locked))
       (not (excal--bound-text-p element))))

(defun excal--max-binding-distance ()
  "Return the binding reach in scene units (`maxBindingDistance_simple')."
  (let ((z (min excal--zoom 1.0)))
    (max 15.0 (min 30.0 (/ 15.0 (* z 1.5))))))

(defun excal--binding-gap (element)
  "Return the gap between a bound arrow end and ELEMENT's outline."
  (+ excal--base-binding-gap (/ (float (or (excal--get element 'strokeWidth) 1)) 2)))

(defun excal--outline-distance (element point)
  "Return POINT's distance to ELEMENT's outline and whether it is inside.
The result is (DISTANCE . INSIDE)."
  (let* ((box (excal--element-box element))
         (local (excal--rotate-point point (excal--box-center box)
                                     (- (excal--element-angle element))))
         (outline (excal--outline element))
         (points (cdr outline))
         (segments (cl-mapcar #'cons points (append (cdr points) (list (car points)))))
         (best most-positive-fixnum))
    (dolist (s segments)
      (let* ((a (car s)) (b (cdr s))
             (dx (- (car b) (car a))) (dy (- (cdr b) (cdr a)))
             (len2 (+ (* dx dx) (* dy dy)))
             (u (if (zerop len2) 0.0
                  (max 0.0 (min 1.0 (/ (+ (* (- (car local) (car a)) dx)
                                          (* (- (cdr local) (cdr a)) dy))
                                       len2)))))
             (d (sqrt (+ (expt (- (car local) (+ (car a) (* u dx))) 2)
                         (expt (- (cdr local) (+ (cdr a) (* u dy))) 2)))))
        (setq best (min best d))))
    (cons best (excal--point-in-polygon-p local points))))

(defun excal--opaque-p (element)
  "Return non-nil if ELEMENT hides elements below it from binding."
  (or (equal (excal--get element 'type) "image")
      (let ((bg (excal--get element 'backgroundColor)))
        (and bg (not (equal bg "transparent"))))))

(defun excal--binding-candidate (point &optional exclude)
  "Return the element an arrow end at POINT would bind to, or nil.
Elements in EXCLUDE are skipped.  Candidates lie within the binding
distance of their outline or contain POINT; the search stops at the
first opaque element containing it.  The closest outline wins, except
that a smaller element containing POINT beats a larger one."
  (let ((reach (excal--max-binding-distance))
        (candidates nil))
    (catch 'stop
      (dolist (e (reverse (excal--live-elements)))
        (when (and (excal--bindable-p e) (not (memq e exclude)))
          (pcase-let ((`(,d . ,inside) (excal--outline-distance e point)))
            (when (or inside (<= d reach))
              (push (list e d inside) candidates))
            (when (and inside (excal--opaque-p e))
              (throw 'stop nil))))))
    (let ((area (lambda (c) (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excal--element-box (car c))))
                              (* (- x2 x1) (- y2 y1))))))
      (car (car (sort candidates
                      (lambda (a b)
                        (if (and (nth 2 a) (nth 2 b))
                            (< (funcall area a) (funcall area b))
                          (< (nth 1 a) (nth 1 b))))))))))

;;;; Fixed points

(defun excal--fixed-point (element point)
  "Return POINT as ratios [RX RY] of ELEMENT's unrotated box."
  (pcase-let* ((box (excal--element-box element))
               (`(,x1 ,y1 ,x2 ,y2) box)
               (local (excal--rotate-point point (excal--box-center box)
                                           (- (excal--element-angle element)))))
    (vector (if (> x2 x1) (/ (- (car local) x1) (- x2 x1)) 0.5)
            (if (> y2 y1) (/ (- (cdr local) y1) (- y2 y1)) 0.5))))

(defun excal--global-fixed-point (element fixed)
  "Return the scene point for ratios FIXED of ELEMENT's box."
  (pcase-let* ((box (excal--element-box element))
               (`(,x1 ,y1 ,x2 ,y2) box))
    (excal--rotate-point (cons (+ x1 (* (aref fixed 0) (- x2 x1)))
                               (+ y1 (* (aref fixed 1) (- y2 y1))))
                         (excal--box-center box) (excal--element-angle element))))

(defun excal--grown-outline (element)
  "Return ELEMENT's outline grown by the binding gap, unrotated."
  (pcase-let* ((gap (excal--binding-gap element))
               (`(,x1 ,y1 ,x2 ,y2) (excal--element-box element))
               (cx (/ (+ x1 x2) 2.0)) (cy (/ (+ y1 y2) 2.0))
               (a (/ (- x2 x1) 2.0)) (b (/ (- y2 y1) 2.0)))
    (pcase (excal--get element 'type)
      ("ellipse" (excal--ellipse-outline (list (- x1 gap) (- y1 gap) (+ x2 gap) (+ y2 gap)) 64))
      ("diamond"
       ;; Moving each edge out by GAP scales the half diagonals.
       (let* ((edge (sqrt (+ (* a a) (* b b))))
              (a2 (+ a (if (> b 0) (/ (* gap edge) b) gap)))
              (b2 (+ b (if (> a 0) (/ (* gap edge) a) gap))))
         (list (cons cx (- cy b2)) (cons (+ cx a2) cy) (cons cx (+ cy b2)) (cons (- cx a2) cy))))
      (_ (list (cons (- x1 gap) (- y1 gap)) (cons (+ x2 gap) (- y1 gap))
               (cons (+ x2 gap) (+ y2 gap)) (cons (- x1 gap) (+ y2 gap)))))))

(defun excal--segment-intersection (p1 p2 q1 q2)
  "Return where segments P1-P2 and Q1-Q2 cross, or nil."
  (let* ((rx (- (car p2) (car p1))) (ry (- (cdr p2) (cdr p1)))
         (sx (- (car q2) (car q1))) (sy (- (cdr q2) (cdr q1)))
         (den (- (* rx sy) (* ry sx))))
    (unless (zerop den)
      (let ((u (/ (- (* (- (car q1) (car p1)) sy) (* (- (cdr q1) (cdr p1)) sx)) den))
            (v (/ (- (* (- (car q1) (car p1)) ry) (* (- (cdr q1) (cdr p1)) rx)) den)))
        (when (and (<= 0 u 1) (<= 0 v 1))
          (cons (+ (car p1) (* u rx)) (+ (cdr p1) (* u ry))))))))

(defun excal--orbit-point (element focus toward)
  "Return where the line from FOCUS to TOWARD leaves ELEMENT's grown outline.
FOCUS and TOWARD are scene points; the crossing nearest TOWARD is used,
and FOCUS itself when there is none or the arrow is too short."
  (let* ((center (excal--box-center (excal--element-box element)))
         (angle (excal--element-angle element))
         (f (excal--rotate-point focus center (- angle)))
         (o (excal--rotate-point toward center (- angle)))
         (outline (excal--grown-outline element))
         (edges (cl-mapcar #'cons outline (append (cdr outline) (list (car outline)))))
         (dist (lambda (a b) (sqrt (+ (expt (- (car a) (car b)) 2) (expt (- (cdr a) (cdr b)) 2)))))
         (hits (delq nil (mapcar (lambda (e) (excal--segment-intersection f o (car e) (cdr e)))
                                 edges))))
    (if (or (null hits) (<= (funcall dist f o) excal--base-arrow-min-length))
        focus
      (excal--rotate-point (car (sort hits (lambda (a b) (< (funcall dist a o) (funcall dist b o)))))
                           center angle))))

;;;; Binding ends

(defun excal--binding-key (end)
  "Return the binding field for END, `start' or `end'."
  (if (eq end 'start) 'startBinding 'endBinding))

(defun excal--add-bound-element (element arrow)
  "Record in ELEMENT that ARROW is bound to it."
  (let ((id (excal--get arrow 'id))
        (bound (append (excal--get element 'boundElements) nil)))
    (unless (seq-some (lambda (b) (equal (alist-get 'id b) id)) bound)
      (excal--put element 'boundElements
                  (vconcat bound (list (list (cons 'id id) (cons 'type "arrow")))))
      (excal--touch element))))

(defun excal--remove-bound-element (element arrow)
  "Forget in ELEMENT that ARROW is bound to it."
  (let* ((id (excal--get arrow 'id))
         (bound (seq-remove (lambda (b) (equal (alist-get 'id b) id))
                            (excal--get element 'boundElements))))
    (excal--put element 'boundElements (if bound (vconcat bound) :null))
    (excal--touch element)))

(defun excal--unbind-end (arrow end)
  "Remove ARROW's binding at END, if any."
  (when-let* ((binding (excal--get arrow (excal--binding-key end))))
    (let ((target (excal--live-element-by-id (alist-get 'elementId binding)))
          (other (excal--get arrow (excal--binding-key (if (eq end 'start) 'end 'start)))))
      (excal--put arrow (excal--binding-key end) :null)
      ;; Keep the back reference while the other end still uses it.
      (when (and target (not (equal (alist-get 'elementId other)
                                    (excal--get target 'id))))
        (excal--remove-bound-element target arrow))
      (excal--touch arrow))))

(defun excal--bind-end (arrow end element point &optional inside)
  "Bind ARROW's END to ELEMENT, aiming at scene POINT.
The binding orbits the outline, or sits on POINT when INSIDE is non-nil
and POINT lies within ELEMENT (upstream's \"inside\" mode, reached by
holding Alt or hovering for `excal--bind-mode-timeout')."
  (excal--unbind-end arrow end)
  (excal--put arrow (excal--binding-key end)
              (list (cons 'elementId (excal--get element 'id))
                    (cons 'fixedPoint (excal--fixed-point element point))
                    (cons 'mode (if (and inside (cdr (excal--outline-distance element point)))
                                    "inside" "orbit"))))
  (excal--add-bound-element element arrow)
  (excal--touch arrow))

(defun excal--end-index (arrow end)
  "Return the index of ARROW's point at END."
  (if (eq end 'start) 0 (1- (length (excal--get arrow 'points)))))

(defun excal--arrow-point (arrow index)
  "Return ARROW's point INDEX in scene coordinates."
  (let ((p (aref (excal--get arrow 'points) index)))
    (cons (+ (excal--get arrow 'x) (aref p 0)) (+ (excal--get arrow 'y) (aref p 1)))))

(defun excal--set-arrow-point (arrow index point)
  "Move ARROW's point INDEX to scene POINT, keeping point 0 at the origin."
  (let* ((points (copy-tree (excal--get arrow 'points) t))
         (x (excal--get arrow 'x)) (y (excal--get arrow 'y)))
    (aset points index (vector (- (car point) x) (- (cdr point) y)))
    (let ((first (aref points 0)))
      (unless (equal first [0.0 0.0])
        (dotimes (i (length points))
          (let ((p (aref points i)))
            (aset points i (vector (- (aref p 0) (aref first 0))
                                   (- (aref p 1) (aref first 1))))))
        (excal--put arrow 'x (float (+ x (aref first 0))))
        (excal--put arrow 'y (float (+ y (aref first 1))))))
    (excal--put arrow 'points points)
    (excal--linear-extent arrow)
    (excal--touch arrow)))

(defun excal--bound-point (arrow end)
  "Return where ARROW's END belongs given its binding, or nil if unbound."
  (when-let* ((binding (excal--get arrow (excal--binding-key end)))
              (element (excal--live-element-by-id (alist-get 'elementId binding))))
    (let* ((focus (excal--global-fixed-point element (alist-get 'fixedPoint binding)))
           (n (length (excal--get arrow 'points)))
           (other-end (if (eq end 'start) 'end 'start))
           (other-binding (excal--get arrow (excal--binding-key other-end)))
           (other-element (and other-binding
                               (excal--live-element-by-id (alist-get 'elementId other-binding))))
           ;; Aim at the other binding's focus for two-point arrows,
           ;; otherwise at the neighbouring point.
           (toward (cond ((and (= n 2) other-element)
                          (excal--global-fixed-point other-element
                                                     (alist-get 'fixedPoint other-binding)))
                         (t (excal--arrow-point arrow (if (eq end 'start) 1 (- n 2)))))))
      (if (equal (alist-get 'mode binding) "inside")
          focus
        (excal--orbit-point element focus toward)))))

(declare-function excal--elbow-p "excal-elbow")
(declare-function excal--elbow-reroute "excal-elbow")

(defun excal--update-arrow (arrow)
  "Move ARROW's bound ends to where their bindings put them.
Elbow arrows are routed again instead (excal-elbow.el)."
  (if (and (fboundp 'excal--elbow-p) (excal--elbow-p arrow))
      (excal--elbow-reroute arrow)
    (dolist (end '(start end))
      (when-let* ((point (excal--bound-point arrow end)))
        (excal--set-arrow-point arrow (excal--end-index arrow end) point)))))

(defun excal--bound-arrows (elements)
  "Return the live arrows bound to any of ELEMENTS."
  (let (arrows)
    (dolist (e elements)
      (seq-doseq (b (or (excal--get e 'boundElements) []))
        (when (equal (alist-get 'type b) "arrow")
          (when-let* ((arrow (excal--live-element-by-id (alist-get 'id b))))
            (cl-pushnew arrow arrows)))))
    (nreverse arrows)))

(defun excal--update-bound-arrows (changed &optional moving)
  "Re-route the arrows bound to CHANGED elements, except those in MOVING.
Return the arrows updated."
  (let ((arrows (seq-remove (lambda (a) (memq a moving)) (excal--bound-arrows changed))))
    (mapc #'excal--update-arrow arrows)
    arrows))

(defun excal--release-moved-arrows (moved)
  "Unbind ends of arrows in MOVED whose bound element did not move with them."
  (dolist (arrow moved)
    (when (equal (excal--get arrow 'type) "arrow")
      (dolist (end '(start end))
        (let ((binding (excal--get arrow (excal--binding-key end))))
          (unless (or (null binding)
                      (memq (excal--live-element-by-id (alist-get 'elementId binding)) moved))
            (excal--unbind-end arrow end)))))))

(defun excal--forget-bindings-to (elements)
  "Unbind every arrow end bound to ELEMENTS, which are being deleted."
  (dolist (arrow (excal--bound-arrows elements))
    (dolist (end '(start end))
      (when (memq (excal--live-element-by-id
                   (alist-get 'elementId (excal--get arrow (excal--binding-key end))))
                  elements)
        (excal--put arrow (excal--binding-key end) :null)
        (excal--touch arrow)))))

;;;; Keeping dependents in place

(defun excal--labels-of (elements)
  "Return the live bound text of ELEMENTS."
  (delq nil (mapcar #'excal--bound-text-of elements)))

(defun excal--dependents (elements)
  "Return ELEMENTS with the arrows bound to them and every label involved.
This is everything a change to ELEMENTS may redraw."
  (let ((with-arrows (seq-union elements (excal--bound-arrows elements))))
    (seq-union with-arrows (excal--labels-of with-arrows))))

(defun excal--follow (elements &optional moving handle keep-aspect from-center)
  "Update what depends on ELEMENTS after they moved or changed shape.
Arrows bound to ELEMENTS are re-routed, except those in MOVING; the
labels of ELEMENTS and of the re-routed arrows are laid out again, as a
resize by HANDLE (with KEEP-ASPECT and FROM-CENTER) when HANDLE is given."
  (let ((arrows (excal--update-bound-arrows elements moving)))
    (dolist (e elements)
      (when (excal--bound-text-of e)
        (if handle
            (excal--layout-bound-text e handle keep-aspect from-center)
          (excal--refresh-bound-text e))))
    (dolist (a arrows)
      (excal--refresh-bound-text a))))

;;;; Highlight

(defun excal--binding-highlight-overlay ()
  "Return the overlay tracing the element an arrow end would bind to."
  (when-let* ((e excal--binding-highlight))
    (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excal--element-box e)))
      (excal--ov (pcase (excal--get e 'type)
                   ("ellipse" "ov-ellipse") ("diamond" "ov-diamond") (_ "ov-rect"))
                 x1 y1 (- x2 x1) (- y2 y1)
                 :angle (excal--element-angle e)
                 :stroke (if (eq excal--theme 'dark) "#68b6f0"
                           excal--binding-highlight-color)
                 :width (min 4 (max 1.75 (or (excal--get e 'strokeWidth) 1)))))))

(provide 'excal-binding)
;;; excal-binding.el ends here
