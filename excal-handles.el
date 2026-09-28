;;; excal-handles.el --- Selection UI and transform handles  -*- lexical-binding: t; -*-

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
;; excal-overlay.c.

;;; Code:

(require 'excal-core)
(require 'excal-select)

(defconst excal-selection-color "#6965db" "Color of selection borders and handles.")

(declare-function excal--binding-highlight-overlay "excal-binding")
(declare-function excal--linear-editor-overlays "excal-linear")
(defconst excal--handle-size 8 "Transform handle edge, screen px.")
(defconst excal--handle-spacing 2 "DEFAULT_TRANSFORM_HANDLE_SPACING, screen px.")
(defconst excal--rotation-gap 16 "ROTATION_RESIZE_HANDLE_GAP, screen px.")
(defconst excal--side-threshold 4 "SIDE_RESIZING_THRESHOLD, screen px.")
(defconst excal--point-handle-size 10 "Linear point handle diameter, screen px.")

;;;; Geometry helpers

(defun excal--element-box (element)
  "Return ELEMENT's unrotated box (X1 Y1 X2 Y2)."
  (excal--bounds element))

(defun excal--element-angle (element)
  "Return ELEMENT's rotation in radians."
  (let ((angle (excal--get element 'angle)))
    (if (numberp angle) (float angle) 0.0)))

(defun excal--rotated-p (element)
  "Return non-nil if ELEMENT is rotated."
  (let ((angle (excal--get element 'angle)))
    (and (numberp angle) (/= angle 0))))

(defun excal--box-center (box)
  "Return the center (X . Y) of BOX."
  (cons (/ (+ (nth 0 box) (nth 2 box)) 2.0)
        (/ (+ (nth 1 box) (nth 3 box)) 2.0)))

(defun excal--rotate-point (point center angle)
  "Rotate POINT (X . Y) about CENTER by ANGLE radians."
  (let* ((dx (- (car point) (car center)))
         (dy (- (cdr point) (cdr center)))
         (c (cos angle)) (s (sin angle)))
    (cons (+ (car center) (- (* dx c) (* dy s)))
          (+ (cdr center) (+ (* dx s) (* dy c))))))

(defun excal--linear-p (element)
  "Return non-nil if ELEMENT is a line or arrow."
  (member (excal--get element 'type) '("line" "arrow")))

(defun excal--two-point-linear-p (element)
  "Return non-nil if ELEMENT is a line or arrow with at most two points."
  (and (excal--linear-p element)
       (<= (length (excal--get element 'points)) 2)))

(defun excal--handle-margin (element)
  "Return the handle margin for a lone ELEMENT, in screen px."
  (pcase (excal--get element 'type)
    ((or "line" "arrow" "freedraw") (+ excal--handle-spacing 8))
    ("image" 0)
    (_ excal--handle-spacing)))

;;;; Transform handles

(defun excal--transform-handles (box angle margin &optional spacing no-rotation)
  "Return transform handles for BOX rotated by ANGLE, with MARGIN in px.
SPACING defaults to `excal--handle-spacing'.  With NO-ROTATION, omit the
rotation handle.  The result is an alist (NAME X Y W H) in scene units,
NAME one of nw, ne, sw, se and rotation; positions are rotated about the
box center while the squares stay axis-aligned, as upstream draws them."
  (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) box)
               (z excal--zoom)
               (size (/ (float excal--handle-size) z))
               (dlm (/ (float margin) z))
               (offset (/ (- excal--handle-size (* 2 (or spacing excal--handle-spacing)))
                          (* 2.0 z)))
               (left (+ (- x1 dlm size) offset))
               (right (- (+ x2 dlm) offset))
               (top (+ (- y1 dlm size) offset))
               (bottom (- (+ y2 dlm) offset))
               (center (excal--box-center box))
               (raw `((nw ,left . ,top) (ne ,right . ,top)
                      (sw ,left . ,bottom) (se ,right . ,bottom)
                      ,@(unless no-rotation
                          `((rotation ,(- (+ x1 (/ (- x2 x1) 2.0)) (/ size 2))
                                      . ,(- top (/ (float excal--rotation-gap) z))))))))
    (mapcar (lambda (handle)
              (let* ((corner (cdr handle))
                     (mid (excal--rotate-point
                           (cons (+ (car corner) (/ size 2)) (+ (cdr corner) (/ size 2)))
                           center angle)))
                (list (car handle) (- (car mid) (/ size 2)) (- (cdr mid) (/ size 2))
                      size size)))
            raw)))

(defun excal--transform-target ()
  "Return what the transform handles act on, or nil.
The result is a plist (:box BOX :angle ANGLE :margin PX :spacing PX
:sides BOOL :rotation BOOL): a lone element uses its own rotated box, a
selection of several elements their common box."
  (let ((single (excal--single-selection)))
    (cond
     ((null excal--selection) nil)
     (excal--editing-linear nil)
     ((and single (excal--two-point-linear-p single)) nil)
     (single
      (list :box (excal--element-box single)
            :angle (excal--element-angle single)
            :margin (excal--handle-margin single)
            :spacing (if (equal (excal--get single 'type) "image") 0
                       excal--handle-spacing)
            :sides t :rotation (not (equal (excal--get single 'type) "frame"))))
     (t
      (list :box (excal--selection-bounds) :angle 0.0 :margin 4
            :spacing excal--handle-spacing :sides t
            :rotation (not (seq-some (lambda (e) (equal (excal--get e 'type) "frame"))
                                     excal--selection)))))))

(defun excal--point-on-segment-p (point a b threshold)
  "Return non-nil if POINT lies within THRESHOLD of segment A-B."
  (let* ((ax (car a)) (ay (cdr a)) (bx (car b)) (by (cdr b))
         (px (car point)) (py (cdr point))
         (dx (- bx ax)) (dy (- by ay))
         (len2 (+ (* dx dx) (* dy dy)))
         (u (if (zerop len2) 0.0
              (max 0.0 (min 1.0 (/ (+ (* (- px ax) dx) (* (- py ay) dy)) len2)))))
         (cx (+ ax (* u dx))) (cy (+ ay (* u dy))))
    (<= (sqrt (+ (expt (- px cx) 2) (expt (- py cy) 2))) threshold)))

(defun excal--handle-at (scene-xy)
  "Return the transform handle under SCENE-XY, or nil.
Like upstream `resizeTest': the rotation handle first, then corners,
then the four edges of the box as a band `excal--side-threshold' wide."
  (when-let* ((target (excal--transform-target)))
    (let* ((box (plist-get target :box))
           (angle (plist-get target :angle))
           (handles (excal--transform-handles box angle (plist-get target :margin)
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
                                        excal--side-threshold))
                               excal--zoom))
                         (center (excal--box-center box))
                         (corner (lambda (x y) (excal--rotate-point (cons x y) center angle)))
                         (tl (funcall corner (- x1 s) (- y1 s)))
                         (tr (funcall corner (+ x2 s) (- y1 s)))
                         (br (funcall corner (+ x2 s) (+ y2 s)))
                         (bl (funcall corner (- x1 s) (+ y2 s)))
                         (threshold (/ (float excal--side-threshold) excal--zoom)))
              (cond ((excal--point-on-segment-p scene-xy tl tr threshold) 'n)
                    ((excal--point-on-segment-p scene-xy tr br threshold) 'e)
                    ((excal--point-on-segment-p scene-xy br bl threshold) 's)
                    ((excal--point-on-segment-p scene-xy bl tl threshold) 'w))))))))

;;;; Overlays

(defun excal--ov (type x y w h &rest props)
  "Return an overlay native vector of TYPE at X, Y sized W, H.
PROPS may give :angle, :stroke, :fill, :width (px) and :style."
  (vector type (float x) (float y) (float w) (float h)
          (float (or (plist-get props :angle) 0))
          (plist-get props :stroke) (plist-get props :fill) nil
          (or (plist-get props :width) 1) 0 1 nil nil nil 100 0
          (or (plist-get props :style) "solid") nil nil nil nil nil nil [] []))

(defun excal--ov-box (box pad &rest props)
  "Return an ov-rect around BOX padded by PAD screen px; see `excal--ov'."
  (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) box)
               (p (/ (float pad) excal--zoom)))
    (apply #'excal--ov "ov-rect" (- x1 p) (- y1 p)
           (+ (- x2 x1) (* 2 p)) (+ (- y2 y1) (* 2 p)) props)))

(defun excal--selected-groups ()
  "Return the groups selected as units, each as (GROUP . MEMBERS)."
  (let (groups)
    (dolist (e excal--selection)
      (when-let* ((group (excal--unit-group e))
                  ((not (assoc group groups))))
        (let ((members (excal--group-members group)))
          (when (and (cdr members) (seq-every-p #'excal--selected-p members))
            (push (cons group members) groups)))))
    (nreverse groups)))

(defun excal--handle-overlays (target)
  "Return handle overlays for TARGET, see `excal--transform-target'."
  (mapcar (lambda (h)
            (pcase-let ((`(,name ,x ,y ,w ,hh) h))
              (excal--ov (if (eq name 'rotation) "ov-circle" "ov-handle") x y w hh
                         :stroke excal-selection-color :fill "#ffffff")))
          (excal--transform-handles (plist-get target :box) (plist-get target :angle)
                                    (plist-get target :margin)
                                    (plist-get target :spacing)
                                    (not (plist-get target :rotation)))))

(defun excal--overlay-natives ()
  "Return overlay pseudo-elements for the selection UI and the marquee."
  (let* ((groups (excal--selected-groups))
         (grouped (apply #'append (mapcar #'cdr groups)))
         (single (excal--single-selection))
         (overlays nil))
    (if (or excal--editing-linear (and single (excal--two-point-linear-p single)))
        ;; Only points: the editor hides the box and handles.
        nil
      ;; Borders of elements not selected through a group.
      (dolist (e excal--selection)
        (unless (memq e grouped)
          (push (excal--ov-box (excal--element-box e) (* 2 excal--handle-spacing)
                               :angle (excal--element-angle e)
                               :stroke excal-selection-color)
                overlays)))
      ;; One dashed box per selected group, and for the entered group.
      (dolist (members (append (mapcar #'cdr groups)
                               (and excal--editing-group
                                    (list (excal--group-members excal--editing-group)))))
        (when members
          (push (excal--ov-box (excal--elements-bounds members)
                               (* 2 excal--handle-spacing)
                               :stroke "#000000" :style "dashed")
                overlays)))
      (when (cdr excal--selection)
        (push (excal--ov-box (excal--selection-bounds) (* 2 excal--handle-spacing)
                             :stroke excal-selection-color :style "dotted")
              overlays))
      (when-let* ((target (excal--transform-target)))
        ;; OVERLAYS is built in reverse; keep the handles in order.
        (setq overlays (append (reverse (excal--handle-overlays target)) overlays))))
    ;; Point handles of a lone line or arrow go above its box.
    (when (fboundp 'excal--linear-editor-overlays)
      (dolist (ov (excal--linear-editor-overlays))
        (push ov overlays)))
    (when excal--marquee
      (push (excal--ov-box excal--marquee 0 :stroke excal-selection-color
                           :fill "#0000c80a")
            overlays))
    (when-let* ((highlight (and (fboundp 'excal--binding-highlight-overlay)
                                (excal--binding-highlight-overlay))))
      (push highlight overlays))
    (nreverse overlays)))

(provide 'excal-handles)
;;; excal-handles.el ends here
