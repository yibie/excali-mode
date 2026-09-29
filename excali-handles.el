;;; excali-handles.el --- Selection UI and transform handles  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Geometry of the selection UI, following Excalidraw's
;; `renderSelectionBorder', `getTransformHandlesFromCoords' and
;; `resizeTest' (docs/excalidraw-spec.md §2c.7–2c.9, §3b.5):
;;
;; - a lone element gets a solid border in the selection color, rotated
;;   with it, plus four corner handles and a round rotation handle;
;; - elements selected through a group get no border of their own; each
;;   group gets one dashed black box, as does the group being edited;
;; - a multi-selection adds a dotted box around everything with handles;
;; - a lone two-point line or arrow shows only its endpoint dots;
;; - sides have no handles: grabbing the border (a 4 px band) resizes.
;;
;; Sizes are in screen pixels and converted to scene units with the zoom.
;; The overlays are handed to the renderer as pseudo-elements, see
;; excali-overlay.c.

;;; Code:

(require 'excali-core)
(require 'excali-select)

(defconst excali-selection-color "#6965db" "Color of selection borders and handles.")
(defconst excali-selection-color-dark "#b4b0ff" "Selection color in the dark theme.")

(defun excali--selection-color ()
  "Return the selection color for the buffer's theme."
  (if (eq excali--theme 'dark) excali-selection-color-dark excali-selection-color))

(declare-function excali--binding-highlight-overlay "excali-binding")
(declare-function excali--linear-editor-overlays "excali-linear")
(declare-function excali--grid-native "excali-snap")
(declare-function excali--link-icon-overlays "excali-erase")
(declare-function excali--snap-line-natives "excali-snap")
(defconst excali--handle-size 8 "Transform handle edge, screen px.")
(defconst excali--handle-spacing 2 "DEFAULT_TRANSFORM_HANDLE_SPACING, screen px.")
(defconst excali--rotation-gap 16 "ROTATION_RESIZE_HANDLE_GAP, screen px.")
(defconst excali--side-threshold 4 "SIDE_RESIZING_THRESHOLD, screen px.")
(defconst excali--point-handle-size 10 "Linear point handle diameter, screen px.")

;;;; Geometry helpers

(defun excali--element-box (element)
  "Return ELEMENT's unrotated box (X1 Y1 X2 Y2)."
  (excali--bounds element))

(defun excali--element-angle (element)
  "Return ELEMENT's rotation in radians."
  (let ((angle (excali--get element 'angle)))
    (if (numberp angle) (float angle) 0.0)))

(defun excali--rotated-p (element)
  "Return non-nil if ELEMENT is rotated."
  (let ((angle (excali--get element 'angle)))
    (and (numberp angle) (/= angle 0))))

(defun excali--box-center (box)
  "Return the center (X . Y) of BOX."
  (cons (/ (+ (nth 0 box) (nth 2 box)) 2.0)
        (/ (+ (nth 1 box) (nth 3 box)) 2.0)))

(defun excali--linear-p (element)
  "Return non-nil if ELEMENT is a line or arrow."
  (member (excali--get element 'type) '("line" "arrow")))

(defun excali--two-point-linear-p (element)
  "Return non-nil if ELEMENT is a line or arrow with at most two points."
  (and (excali--linear-p element)
       (<= (length (excali--get element 'points)) 2)))

(defun excali--handle-margin (element)
  "Return the handle margin for a lone ELEMENT, in screen px."
  (pcase (excali--get element 'type)
    ((or "line" "arrow" "freedraw") (+ excali--handle-spacing 8))
    ("image" 0)
    (_ excali--handle-spacing)))

;;;; Transform handles

(defun excali--transform-handles (box angle margin &optional spacing no-rotation)
  "Return transform handles for BOX rotated by ANGLE, with MARGIN in px.
SPACING defaults to `excali--handle-spacing'.  With NO-ROTATION, omit the
rotation handle.  The result is an alist (NAME X Y W H) in scene units,
NAME one of nw, ne, sw, se and rotation; positions are rotated about the
box center while the squares stay axis-aligned, as upstream draws them."
  (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) box)
               (z excali--zoom)
               (size (/ (float excali--handle-size) z))
               (dlm (/ (float margin) z))
               (offset (/ (- excali--handle-size (* 2 (or spacing excali--handle-spacing)))
                          (* 2.0 z)))
               (left (+ (- x1 dlm size) offset))
               (right (- (+ x2 dlm) offset))
               (top (+ (- y1 dlm size) offset))
               (bottom (- (+ y2 dlm) offset))
               (center (excali--box-center box))
               (raw `((nw ,left . ,top) (ne ,right . ,top)
                      (sw ,left . ,bottom) (se ,right . ,bottom)
                      ,@(unless no-rotation
                          `((rotation ,(- (+ x1 (/ (- x2 x1) 2.0)) (/ size 2))
                                      . ,(- top (/ (float excali--rotation-gap) z))))))))
    (mapcar (lambda (handle)
              (let* ((corner (cdr handle))
                     (mid (excali--rotate-point
                           (cons (+ (car corner) (/ size 2)) (+ (cdr corner) (/ size 2)))
                           center angle)))
                (list (car handle) (- (car mid) (/ size 2)) (- (cdr mid) (/ size 2))
                      size size)))
            raw)))

(defun excali--transform-target ()
  "Return what the transform handles act on, or nil.
The result is a plist (:box BOX :angle ANGLE :margin PX :spacing PX
:sides BOOL :rotation BOOL): a lone element uses its own rotated box, a
selection of several elements their common box."
  (let ((single (excali--single-selection)))
    (cond
     ((null excali--selection) nil)
     (excali--editing-linear nil)
     ((and single (excali--two-point-linear-p single)) nil)
     ;; Elbow arrows have no transform handles.
     ((and single (equal (excali--get single 'type) "arrow")
           (excali--get single 'elbowed))
      nil)
     (single
      (list :box (excali--element-box single)
            :angle (excali--element-angle single)
            :margin (excali--handle-margin single)
            :spacing (if (equal (excali--get single 'type) "image") 0
                       excali--handle-spacing)
            :sides t :rotation (not (equal (excali--get single 'type) "frame"))))
     (t
      (list :box (excali--selection-bounds) :angle 0.0 :margin 4
            :spacing excali--handle-spacing :sides t
            :rotation (not (seq-some (lambda (e) (equal (excali--get e 'type) "frame"))
                                     excali--selection)))))))

(defun excali--point-on-segment-p (point a b threshold)
  "Return non-nil if POINT lies within THRESHOLD of segment A-B."
  (let* ((ax (car a)) (ay (cdr a)) (bx (car b)) (by (cdr b))
         (px (car point)) (py (cdr point))
         (dx (- bx ax)) (dy (- by ay))
         (len2 (+ (* dx dx) (* dy dy)))
         (u (if (zerop len2) 0.0
              (max 0.0 (min 1.0 (/ (+ (* (- px ax) dx) (* (- py ay) dy)) len2)))))
         (cx (+ ax (* u dx))) (cy (+ ay (* u dy))))
    (<= (sqrt (+ (expt (- px cx) 2) (expt (- py cy) 2))) threshold)))

(defun excali--handle-at (scene-xy)
  "Return the transform handle under SCENE-XY, or nil.
Like upstream `resizeTest': the rotation handle first, then corners,
then the four edges of the box as a band `excali--side-threshold' wide."
  (when-let* ((target (excali--transform-target)))
    (let* ((box (plist-get target :box))
           (angle (plist-get target :angle))
           (handles (excali--transform-handles box angle (plist-get target :margin)
                                              (plist-get target :spacing)
                                              (not (plist-get target :rotation))))
           (inside (lambda (h)
                     (pcase-let ((`(,_ ,x ,y ,w ,hh) h))
                       (and (<= x (car scene-xy) (+ x w))
                            (<= y (cdr scene-xy) (+ y hh))))))
           (ordered (append (seq-filter (lambda (h) (eq (car h) 'rotation)) handles)
                            (seq-remove (lambda (h) (eq (car h) 'rotation)) handles))))
      (or (car (cl-find-if inside ordered))
          (when (plist-get target :sides)
            (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) box)
                         (s (/ (float (if (eql (plist-get target :spacing) 0) 0
                                        excali--side-threshold))
                               excali--zoom))
                         (center (excali--box-center box))
                         (corner (lambda (x y) (excali--rotate-point (cons x y) center angle)))
                         (tl (funcall corner (- x1 s) (- y1 s)))
                         (tr (funcall corner (+ x2 s) (- y1 s)))
                         (br (funcall corner (+ x2 s) (+ y2 s)))
                         (bl (funcall corner (- x1 s) (+ y2 s)))
                         (threshold (/ (float excali--side-threshold) excali--zoom)))
              (cond ((excali--point-on-segment-p scene-xy tl tr threshold) 'n)
                    ((excali--point-on-segment-p scene-xy tr br threshold) 'e)
                    ((excali--point-on-segment-p scene-xy br bl threshold) 's)
                    ((excali--point-on-segment-p scene-xy bl tl threshold) 'w))))))))

;;;; Overlays

(defun excali--ov (type x y w h &rest props)
  "Return an overlay native vector of TYPE at X, Y sized W, H.
PROPS may give :angle, :stroke, :fill, :width (px) and :style."
  (vector type (float x) (float y) (float w) (float h)
          (float (or (plist-get props :angle) 0))
          (plist-get props :stroke) (plist-get props :fill) nil
          (or (plist-get props :width) 1) 0 1 nil nil nil 100 0
          (or (plist-get props :style) "solid") nil nil nil nil nil nil [] []))

(defun excali--ov-box (box pad &rest props)
  "Return an ov-rect around BOX padded by PAD screen px; see `excali--ov'."
  (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) box)
               (p (/ (float pad) excali--zoom)))
    (apply #'excali--ov "ov-rect" (- x1 p) (- y1 p)
           (+ (- x2 x1) (* 2 p)) (+ (- y2 y1) (* 2 p)) props)))

(defun excali--selected-groups ()
  "Return the groups selected as units, each as (GROUP . MEMBERS).
One pass over the scene finds the members of every group the selection
touches, so a large selection of grouped elements stays cheap."
  (let ((wanted (make-hash-table :test #'equal))
        (selected (make-hash-table :test #'eq))
        order)
    (dolist (e excali--selection)
      (puthash e t selected)
      (when-let* ((group (excali--unit-group e))
                  ((not (gethash group wanted))))
        (puthash group t wanted)
        (push group order)))
    (when order
      (let ((members (make-hash-table :test #'equal)))
        ;; `mapc' and `cl-every', not their generic `seq' kin: this
        ;; runs over the whole scene every frame.
        (dolist (e excali--elements)
          (unless (excali--get e 'isDeleted)
            (when-let* ((ids (excali--get e 'groupIds)))
              (mapc (lambda (group)
                      (when (and (gethash group wanted)
                                 (not (eq (car (gethash group members)) e)))
                        (push e (gethash group members))))
                    ids))))
        (cl-loop for group in (nreverse order)
                 for ms = (reverse (gethash group members))
                 when (and (cdr ms) (cl-every (lambda (m) (gethash m selected)) ms))
                 collect (cons group ms))))))

(defun excali--handle-overlays (target)
  "Return handle overlays for TARGET, see `excali--transform-target'."
  (mapcar (lambda (h)
            (pcase-let ((`(,name ,x ,y ,w ,hh) h))
              (excali--ov (if (eq name 'rotation) "ov-circle" "ov-handle") x y w hh
                         :stroke (excali--selection-color) :fill "#ffffff")))
          (excali--transform-handles (plist-get target :box) (plist-get target :angle)
                                    (plist-get target :margin)
                                    (plist-get target :spacing)
                                    (not (plist-get target :rotation)))))

(defvar excali--hide-editor-overlays nil
  "Non-nil hides the selection UI, as while the eye dropper reads pixels.")

(declare-function excali--tool-overlays "excali-tools")

(defun excali--overlay-natives ()
  "Return overlay pseudo-elements for the selection UI and the marquee.
Tool overlays (laser, lasso, eye dropper) go on top."
  (append (unless excali--hide-editor-overlays (excali--editor-overlay-natives))
          (and (fboundp 'excali--tool-overlays) (excali--tool-overlays))))

(defun excali--editor-overlay-natives ()
  "Return overlay pseudo-elements for the selection UI and the marquee."
  (let* ((groups (excali--selected-groups))
         (grouped (let ((set (make-hash-table :test #'eq)))
                    (dolist (g groups set) (dolist (m (cdr g)) (puthash m t set)))))
         (single (excali--single-selection))
         (overlays nil))
    (if (or excali--editing-linear (and single (excali--two-point-linear-p single)))
        ;; Only points: the editor hides the box and handles.
        nil
      ;; Borders of elements not selected through a group; a lone bound
      ;; elbow arrow has none.
      (dolist (e excali--selection)
        (unless (or (gethash e grouped)
                    (and single (excali--get e 'elbowed)
                         (or (excali--get e 'startBinding) (excali--get e 'endBinding))))
          (push (excali--ov-box (excali--element-box e) (* 2 excali--handle-spacing)
                               :angle (excali--element-angle e)
                               :stroke (excali--selection-color))
                overlays)))
      ;; One dashed box per selected group, and for the entered group.
      (dolist (members (append (mapcar #'cdr groups)
                               (and excali--editing-group
                                    (list (excali--group-members excali--editing-group)))))
        (when members
          (push (excali--ov-box (excali--elements-bounds members)
                               (* 2 excali--handle-spacing)
                               :stroke "#000000" :style "dashed")
                overlays)))
      (when (cdr excali--selection)
        (push (excali--ov-box (excali--selection-bounds) (* 2 excali--handle-spacing)
                             :stroke (excali--selection-color) :style "dotted")
              overlays))
      (when-let* ((target (excali--transform-target)))
        ;; OVERLAYS is built in reverse; keep the handles in order.
        (setq overlays (append (reverse (excali--handle-overlays target)) overlays))))
    ;; Point handles of a lone line or arrow go above its box.
    (when (fboundp 'excali--linear-editor-overlays)
      (dolist (ov (excali--linear-editor-overlays))
        (push ov overlays)))
    (when excali--marquee
      (push (excali--ov-box excali--marquee 0 :stroke (excali--selection-color)
                           :fill "#0000c80a")
            overlays))
    (when-let* ((highlight (and (fboundp 'excali--binding-highlight-overlay)
                                (excali--binding-highlight-overlay))))
      (push highlight overlays))
    (when (fboundp 'excali--link-icon-overlays)
      (setq overlays (append (excali--link-icon-overlays) overlays)))
    (when (fboundp 'excali--snap-line-natives)
      (setq overlays (append (excali--snap-line-natives) overlays)))
    ;; The grid is drawn below the elements whatever its position here.
    (when-let* ((grid (and (fboundp 'excali--grid-native) (excali--grid-native))))
      (push grid overlays))
    (nreverse overlays)))

(provide 'excali-handles)
;;; excali-handles.el ends here
