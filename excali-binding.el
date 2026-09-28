;;; excali-binding.el --- Binding arrows to shapes  -*- lexical-binding: t; -*-

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

(require 'excali-core)
(require 'excali-select)
(require 'excali-handles)
(require 'excali-hit)
(require 'excali-text)

(defconst excali--binding-highlight-color "#6abdfc" "BINDING_HIGHLIGHT_RGB.")
(defconst excali--base-binding-gap 5 "BASE_BINDING_GAP.")
(defconst excali--base-arrow-min-length 10 "BASE_ARROW_MIN_LENGTH.")
(defconst excali--bind-mode-timeout 0.7
  "BIND_MODE_TIMEOUT in seconds: hovering this long binds \"inside\".")

(defvar-local excali--binding-highlight nil
  "Element that the arrow end being drawn would bind to, or nil.")

;;;; Candidates

(defun excali--bindable-p (element)
  "Return non-nil if arrows can bind to ELEMENT."
  (and (member (excali--get element 'type)
               '("rectangle" "diamond" "ellipse" "text" "image" "frame" "stickynote"))
       (not (excali--get element 'locked))
       (not (excali--bound-text-p element))))

(defun excali--max-binding-distance ()
  "Return the binding reach in scene units (`maxBindingDistance_simple')."
  (let ((z (min excali--zoom 1.0)))
    (max 15.0 (min 30.0 (/ 15.0 (* z 1.5))))))

(defun excali--binding-gap (element)
  "Return the gap between a bound arrow end and ELEMENT's outline."
  (+ excali--base-binding-gap (/ (float (or (excali--get element 'strokeWidth) 1)) 2)))

(defun excali--outline-distance (element point)
  "Return POINT's distance to ELEMENT's outline and whether it is inside.
The result is (DISTANCE . INSIDE)."
  (let* ((box (excali--element-box element))
         (local (excali--rotate-point point (excali--box-center box)
                                     (- (excali--element-angle element))))
         (outline (excali--outline element))
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
    (cons best (excali--point-in-polygon-p local points))))

(defun excali--opaque-p (element)
  "Return non-nil if ELEMENT hides elements below it from binding."
  (or (equal (excali--get element 'type) "image")
      (let ((bg (excali--get element 'backgroundColor)))
        (and bg (not (equal bg "transparent"))))))

(defun excali--binding-candidate (point &optional exclude)
  "Return the element an arrow end at POINT would bind to, or nil.
Elements in EXCLUDE are skipped.  Candidates lie within the binding
distance of their outline or contain POINT; the search stops at the
first opaque element containing it.  The closest outline wins, except
that a smaller element containing POINT beats a larger one."
  (let ((reach (excali--max-binding-distance))
        (candidates nil))
    (catch 'stop
      (dolist (e (reverse (excali--live-elements)))
        (when (and (excali--bindable-p e) (not (memq e exclude)))
          (pcase-let ((`(,d . ,inside) (excali--outline-distance e point)))
            (when (or inside (<= d reach))
              (push (list e d inside) candidates))
            (when (and inside (excali--opaque-p e))
              (throw 'stop nil))))))
    (let ((area (lambda (c) (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excali--element-box (car c))))
                              (* (- x2 x1) (- y2 y1))))))
      (car (car (sort candidates
                      (lambda (a b)
                        (if (and (nth 2 a) (nth 2 b))
                            (< (funcall area a) (funcall area b))
                          (< (nth 1 a) (nth 1 b))))))))))

;;;; Fixed points

(defun excali--fixed-point (element point)
  "Return POINT as ratios [RX RY] of ELEMENT's unrotated box."
  (pcase-let* ((box (excali--element-box element))
               (`(,x1 ,y1 ,x2 ,y2) box)
               (local (excali--rotate-point point (excali--box-center box)
                                           (- (excali--element-angle element)))))
    (vector (if (> x2 x1) (/ (- (car local) x1) (- x2 x1)) 0.5)
            (if (> y2 y1) (/ (- (cdr local) y1) (- y2 y1)) 0.5))))

(defun excali--global-fixed-point (element fixed)
  "Return the scene point for ratios FIXED of ELEMENT's box."
  (pcase-let* ((box (excali--element-box element))
               (`(,x1 ,y1 ,x2 ,y2) box))
    (excali--rotate-point (cons (+ x1 (* (aref fixed 0) (- x2 x1)))
                               (+ y1 (* (aref fixed 1) (- y2 y1))))
                         (excali--box-center box) (excali--element-angle element))))

(defun excali--grown-outline (element)
  "Return ELEMENT's outline grown by the binding gap, unrotated."
  (pcase-let* ((gap (excali--binding-gap element))
               (`(,x1 ,y1 ,x2 ,y2) (excali--element-box element))
               (cx (/ (+ x1 x2) 2.0)) (cy (/ (+ y1 y2) 2.0))
               (a (/ (- x2 x1) 2.0)) (b (/ (- y2 y1) 2.0)))
    (pcase (excali--get element 'type)
      ("ellipse" (excali--ellipse-outline (list (- x1 gap) (- y1 gap) (+ x2 gap) (+ y2 gap)) 64))
      ("diamond"
       ;; Moving each edge out by GAP scales the half diagonals.
       (let* ((edge (sqrt (+ (* a a) (* b b))))
              (a2 (+ a (if (> b 0) (/ (* gap edge) b) gap)))
              (b2 (+ b (if (> a 0) (/ (* gap edge) a) gap))))
         (list (cons cx (- cy b2)) (cons (+ cx a2) cy) (cons cx (+ cy b2)) (cons (- cx a2) cy))))
      (_ (list (cons (- x1 gap) (- y1 gap)) (cons (+ x2 gap) (- y1 gap))
               (cons (+ x2 gap) (+ y2 gap)) (cons (- x1 gap) (+ y2 gap)))))))

(defun excali--segment-intersection (p1 p2 q1 q2)
  "Return where segments P1-P2 and Q1-Q2 cross, or nil."
  (let* ((rx (- (car p2) (car p1))) (ry (- (cdr p2) (cdr p1)))
         (sx (- (car q2) (car q1))) (sy (- (cdr q2) (cdr q1)))
         (den (- (* rx sy) (* ry sx))))
    (unless (zerop den)
      (let ((u (/ (- (* (- (car q1) (car p1)) sy) (* (- (cdr q1) (cdr p1)) sx)) den))
            (v (/ (- (* (- (car q1) (car p1)) ry) (* (- (cdr q1) (cdr p1)) rx)) den)))
        (when (and (<= 0 u 1) (<= 0 v 1))
          (cons (+ (car p1) (* u rx)) (+ (cdr p1) (* u ry))))))))

(defun excali--orbit-point (element focus toward)
  "Return where the line from FOCUS to TOWARD leaves ELEMENT's grown outline.
FOCUS and TOWARD are scene points; the crossing nearest TOWARD is used,
and FOCUS itself when there is none or the arrow is too short."
  (let* ((center (excali--box-center (excali--element-box element)))
         (angle (excali--element-angle element))
         (f (excali--rotate-point focus center (- angle)))
         (o (excali--rotate-point toward center (- angle)))
         (outline (excali--grown-outline element))
         (edges (cl-mapcar #'cons outline (append (cdr outline) (list (car outline)))))
         (dist (lambda (a b) (sqrt (+ (expt (- (car a) (car b)) 2) (expt (- (cdr a) (cdr b)) 2)))))
         (hits (delq nil (mapcar (lambda (e) (excali--segment-intersection f o (car e) (cdr e)))
                                 edges))))
    (if (or (null hits) (<= (funcall dist f o) excali--base-arrow-min-length))
        focus
      (excali--rotate-point (car (sort hits (lambda (a b) (< (funcall dist a o) (funcall dist b o)))))
                           center angle))))

;;;; Binding ends

(defun excali--binding-key (end)
  "Return the binding field for END, `start' or `end'."
  (if (eq end 'start) 'startBinding 'endBinding))

(defun excali--add-bound-element (element arrow)
  "Record in ELEMENT that ARROW is bound to it."
  (let ((id (excali--get arrow 'id))
        (bound (append (excali--get element 'boundElements) nil)))
    (unless (seq-some (lambda (b) (equal (alist-get 'id b) id)) bound)
      (excali--put element 'boundElements
                  (vconcat bound (list (list (cons 'id id) (cons 'type "arrow")))))
      (excali--touch element))))

(defun excali--remove-bound-element (element arrow)
  "Forget in ELEMENT that ARROW is bound to it."
  (let* ((id (excali--get arrow 'id))
         (bound (seq-remove (lambda (b) (equal (alist-get 'id b) id))
                            (excali--get element 'boundElements))))
    (excali--put element 'boundElements (if bound (vconcat bound) :null))
    (excali--touch element)))

(defun excali--unbind-end (arrow end)
  "Remove ARROW's binding at END, if any."
  (when-let* ((binding (excali--get arrow (excali--binding-key end))))
    (let ((target (excali--live-element-by-id (alist-get 'elementId binding)))
          (other (excali--get arrow (excali--binding-key (if (eq end 'start) 'end 'start)))))
      (excali--put arrow (excali--binding-key end) :null)
      ;; Keep the back reference while the other end still uses it.
      (when (and target (not (equal (alist-get 'elementId other)
                                    (excali--get target 'id))))
        (excali--remove-bound-element target arrow))
      (excali--touch arrow))))

(defun excali--bind-end (arrow end element point &optional inside)
  "Bind ARROW's END to ELEMENT, aiming at scene POINT.
The binding orbits the outline, or sits on POINT when INSIDE is non-nil
and POINT lies within ELEMENT (upstream's \"inside\" mode, reached by
holding Alt or hovering for `excali--bind-mode-timeout')."
  (excali--unbind-end arrow end)
  (excali--put arrow (excali--binding-key end)
              (list (cons 'elementId (excali--get element 'id))
                    (cons 'fixedPoint (excali--fixed-point element point))
                    (cons 'mode (if (and inside (cdr (excali--outline-distance element point)))
                                    "inside" "orbit"))))
  (excali--add-bound-element element arrow)
  (excali--touch arrow))

(defun excali--end-index (arrow end)
  "Return the index of ARROW's point at END."
  (if (eq end 'start) 0 (1- (length (excali--get arrow 'points)))))

(defun excali--arrow-point (arrow index)
  "Return ARROW's point INDEX in scene coordinates."
  (let ((p (aref (excali--get arrow 'points) index)))
    (cons (+ (excali--get arrow 'x) (aref p 0)) (+ (excali--get arrow 'y) (aref p 1)))))

(defun excali--set-arrow-point (arrow index point)
  "Move ARROW's point INDEX to scene POINT, keeping point 0 at the origin."
  (let* ((points (copy-tree (excali--get arrow 'points) t))
         (x (excali--get arrow 'x)) (y (excali--get arrow 'y)))
    (aset points index (vector (- (car point) x) (- (cdr point) y)))
    (let ((first (aref points 0)))
      (unless (equal first [0.0 0.0])
        (dotimes (i (length points))
          (let ((p (aref points i)))
            (aset points i (vector (- (aref p 0) (aref first 0))
                                   (- (aref p 1) (aref first 1))))))
        (excali--put arrow 'x (float (+ x (aref first 0))))
        (excali--put arrow 'y (float (+ y (aref first 1))))))
    (excali--put arrow 'points points)
    (excali--linear-extent arrow)
    (excali--touch arrow)))

(defun excali--bound-point (arrow end)
  "Return where ARROW's END belongs given its binding, or nil if unbound."
  (when-let* ((binding (excali--get arrow (excali--binding-key end)))
              (element (excali--live-element-by-id (alist-get 'elementId binding))))
    (let* ((focus (excali--global-fixed-point element (alist-get 'fixedPoint binding)))
           (n (length (excali--get arrow 'points)))
           (other-end (if (eq end 'start) 'end 'start))
           (other-binding (excali--get arrow (excali--binding-key other-end)))
           (other-element (and other-binding
                               (excali--live-element-by-id (alist-get 'elementId other-binding))))
           ;; Aim at the other binding's focus for two-point arrows,
           ;; otherwise at the neighbouring point.
           (toward (cond ((and (= n 2) other-element)
                          (excali--global-fixed-point other-element
                                                     (alist-get 'fixedPoint other-binding)))
                         (t (excali--arrow-point arrow (if (eq end 'start) 1 (- n 2)))))))
      (if (equal (alist-get 'mode binding) "inside")
          focus
        (excali--orbit-point element focus toward)))))

(declare-function excali--elbow-p "excali-elbow")
(declare-function excali--elbow-reroute "excali-elbow")

(defun excali--update-arrow (arrow)
  "Move ARROW's bound ends to where their bindings put them.
Elbow arrows are routed again instead (excali-elbow.el)."
  (if (and (fboundp 'excali--elbow-p) (excali--elbow-p arrow))
      (excali--elbow-reroute arrow)
    (dolist (end '(start end))
      (when-let* ((point (excali--bound-point arrow end)))
        (excali--set-arrow-point arrow (excali--end-index arrow end) point)))))

(defun excali--bound-arrows (elements)
  "Return the live arrows bound to any of ELEMENTS."
  (let (arrows)
    (dolist (e elements)
      (seq-doseq (b (or (excali--get e 'boundElements) []))
        (when (equal (alist-get 'type b) "arrow")
          (when-let* ((arrow (excali--live-element-by-id (alist-get 'id b))))
            (cl-pushnew arrow arrows)))))
    (nreverse arrows)))

(defun excali--update-bound-arrows (changed &optional moving)
  "Re-route the arrows bound to CHANGED elements, except those in MOVING.
Return the arrows updated."
  (let ((arrows (seq-remove (lambda (a) (memq a moving)) (excali--bound-arrows changed))))
    (mapc #'excali--update-arrow arrows)
    arrows))

(defun excali--release-moved-arrows (moved)
  "Unbind ends of arrows in MOVED whose bound element did not move with them."
  (dolist (arrow moved)
    (when (equal (excali--get arrow 'type) "arrow")
      (dolist (end '(start end))
        (let ((binding (excali--get arrow (excali--binding-key end))))
          (unless (or (null binding)
                      (memq (excali--live-element-by-id (alist-get 'elementId binding)) moved))
            (excali--unbind-end arrow end)))))))

(defun excali--forget-bindings-to (elements)
  "Unbind every arrow end bound to ELEMENTS, which are being deleted."
  (dolist (arrow (excali--bound-arrows elements))
    (dolist (end '(start end))
      (when (memq (excali--live-element-by-id
                   (alist-get 'elementId (excali--get arrow (excali--binding-key end))))
                  elements)
        (excali--put arrow (excali--binding-key end) :null)
        (excali--touch arrow)))))

;;;; Keeping dependents in place

(defun excali--labels-of (elements)
  "Return the live bound text of ELEMENTS."
  (delq nil (mapcar #'excali--bound-text-of elements)))

(defun excali--dependents (elements)
  "Return ELEMENTS with the arrows bound to them and every label involved.
This is everything a change to ELEMENTS may redraw."
  (let ((with-arrows (seq-union elements (excali--bound-arrows elements))))
    (seq-union with-arrows (excali--labels-of with-arrows))))

(defun excali--follow (elements &optional moving handle keep-aspect from-center)
  "Update what depends on ELEMENTS after they moved or changed shape.
Arrows bound to ELEMENTS are re-routed, except those in MOVING; the
labels of ELEMENTS and of the re-routed arrows are laid out again, as a
resize by HANDLE (with KEEP-ASPECT and FROM-CENTER) when HANDLE is given."
  (let ((arrows (excali--update-bound-arrows elements moving)))
    (dolist (e elements)
      (when (excali--bound-text-of e)
        (if handle
            (excali--layout-bound-text e handle keep-aspect from-center)
          (excali--refresh-bound-text e))))
    (dolist (a arrows)
      (excali--refresh-bound-text a))))

;;;; Highlight

(defun excali--binding-highlight-overlay ()
  "Return the overlay tracing the element an arrow end would bind to."
  (when-let* ((e excali--binding-highlight))
    (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excali--element-box e)))
      (excali--ov (pcase (excali--get e 'type)
                   ("ellipse" "ov-ellipse") ("diamond" "ov-diamond") (_ "ov-rect"))
                 x1 y1 (- x2 x1) (- y2 y1)
                 :angle (excali--element-angle e)
                 :stroke (if (eq excali--theme 'dark) "#68b6f0"
                           excali--binding-highlight-color)
                 :width (min 4 (max 1.75 (or (excali--get e 'strokeWidth) 1)))))))

(provide 'excali-binding)
;;; excali-binding.el ends here
