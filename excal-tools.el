;;; excal-tools.el --- Laser, eye dropper, autoshape and lasso  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The smaller tools of docs/excalidraw-spec.md §3.A.3-4:
;;
;; - Laser (`k'): dragging leaves a red trail that fades within a
;;   second (laserTrails.ts).  It is never an element and never saved.
;; - Eye dropper (`i', `S', `G'): the next click picks the color under
;;   the pointer (App.tsx `openEyeDropper').  `i' and `G' pick a
;;   background, `S' a stroke; meta at the click picks the other one.
;;   With a selection and the selection tool, the selected elements take
;;   the color, else it becomes the current style.
;; - Autoshape (`X'): a freehand stroke that is recognised as a
;;   rectangle, ellipse, diamond or line when released.
;; - Lasso: a free-form selection, from the selection tool with
;;   control+meta or super+meta, or as the preferred selection tool.
;;   Like the default box selection mode, "contain": an element is
;;   selected when all of its outline lies inside the lasso.

;;; Code:

(require 'excal-core)
(require 'excal-view)
(require 'excal-select)
(require 'excal-handles)
(require 'excal-hit)
(require 'excal-style)

(declare-function excal--drag-loop "excal-edit")
(declare-function excal--await-release "excal-edit")
(declare-function excal--create-freedraw "excal-create")
(declare-function excal--created "excal-create")
(declare-function excal--add-new "excal-create")
(defvar excal--tool-locked)
(defvar excal--previous-tool)
(defvar excal--canvas-size)
(defvar excal--pixel-scale)
(defvar excal--fb)
(defvar excal--hide-editor-overlays)

;;;; Laser

(defconst excal--laser-color "#ff0000" "DEFAULT_LASER_COLOR (\"red\").")
(defconst excal--laser-decay-time 1.0 "DECAY_TIME, in seconds.")
(defconst excal--laser-decay-length 50 "DECAY_LENGTH, in points.")
(defconst excal--laser-streamline 0.4 "The trail's `streamline' option.")
(defconst excal--laser-size 2.0
  "Trail radius in screen px at full size (laser-pointer's `size').")

(defvar-local excal--laser-trails nil
  "Laser trails on screen: lists of [X Y TIME] points, newest first.")
(defvar-local excal--laser-timer nil "Timer animating the laser decay.")
(defvar-local excal--laser-damage nil "Scene box the trails last covered.")

(defun excal--ease-out (k)
  "Return the cubic ease-out of K."
  (- 1 (expt (- 1 k) 3)))

(defun excal--laser-add (trail x y &optional time)
  "Add scene point X, Y to TRAIL (newest first) and return it.
The point is pulled toward the last one by the streamline factor."
  (let ((time (or time (float-time)))
        (last (car trail)))
    (if last
        (let ((f (- 1 excal--laser-streamline)))
          (cons (vector (+ (aref last 0) (* f (- x (aref last 0))))
                        (+ (aref last 1) (* f (- y (aref last 1))))
                        time)
                trail))
      (list (vector (float x) (float y) time)))))

(defun excal--laser-radii (points now)
  "Return the radius of each of POINTS (oldest first) at time NOW.
Upstream's sizeMapping: the least of the eased length and time decay."
  (let ((n (length points)) (i 0) radii)
    (dolist (p points (nreverse radii))
      (let* ((l (/ (- excal--laser-decay-length (min excal--laser-decay-length (- n i)))
                   (float excal--laser-decay-length)))
             (tt (max 0.0 (- 1 (/ (- now (aref p 2)) excal--laser-decay-time)))))
        (push (* (/ excal--laser-size excal--zoom)
                 (min (excal--ease-out l) (excal--ease-out tt)))
              radii)
        (setq i (1+ i))))))

(defun excal--laser-outline (trail now)
  "Return TRAIL's outline at NOW as a list of (X . Y), or nil when gone.
Points are offset by their radius on both sides, with round caps."
  (let* ((points (reverse trail))
         (radii (excal--laser-radii points now))
         (pairs (seq-filter (lambda (pr) (> (cdr pr) 1e-3))
                            (cl-mapcar #'cons points radii))))
    (when pairs
      (let* ((v (vconcat pairs)) (n (length v)) left right)
        (dotimes (i n)
          (let* ((p (car (aref v i))) (r (cdr (aref v i)))
                 (a (car (aref v (max 0 (1- i)))))
                 (b (car (aref v (min (1- n) (1+ i)))))
                 (dx (- (aref b 0) (aref a 0))) (dy (- (aref b 1) (aref a 1)))
                 (len (sqrt (+ (* dx dx) (* dy dy))))
                 (nx (if (> len 0) (/ (- dy) len) 0.0))
                 (ny (if (> len 0) (/ dx len) 1.0)))
            (push (cons (+ (aref p 0) (* r nx)) (+ (aref p 1) (* r ny))) left)
            (push (cons (- (aref p 0) (* r nx)) (- (aref p 1) (* r ny))) right)))
        (let* ((cap (lambda (i from)
                      ;; Half circle around point I starting at angle FROM.
                      (let ((p (car (aref v i))) (r (cdr (aref v i))))
                        (cl-loop for k from 1 below 8
                                 for a = (+ from (* float-pi (/ k 8.0)))
                                 collect (cons (+ (aref p 0) (* r (cos a)))
                                               (+ (aref p 1) (* r (sin a))))))))
               (dir (lambda (i j)
                      (atan (- (aref (car (aref v j)) 1) (aref (car (aref v i)) 1))
                            (- (aref (car (aref v j)) 0) (aref (car (aref v i)) 0))))))
          ;; LEFT and RIGHT were pushed, so they run newest first.
          (append (reverse left)
                  (funcall cap (1- n) (+ (funcall dir (max 0 (- n 2)) (1- n))
                                         (/ float-pi 2)))
                  right
                  (funcall cap 0 (- (funcall dir 0 (min 1 (1- n)))
                                    (/ float-pi 2)))))))))

(defun excal--laser-box ()
  "Return the scene box around all laser trails, or nil."
  (let ((pad (/ (* 2 excal--laser-size) excal--zoom)) box)
    (dolist (trail excal--laser-trails box)
      (dolist (p trail)
        (let ((x (aref p 0)) (y (aref p 1)))
          (setq box (if box
                        (list (min (nth 0 box) (- x pad)) (min (nth 1 box) (- y pad))
                              (max (nth 2 box) (+ x pad)) (max (nth 3 box) (+ y pad)))
                      (list (- x pad) (- y pad) (+ x pad) (+ y pad)))))))))

(defun excal--laser-refresh-damage ()
  "Return the damage for redrawing the trails, updating the record."
  (let ((old excal--laser-damage) (new (excal--laser-box)))
    (setq excal--laser-damage new)
    (excal--damage-union (and old (excal--scene-rect-damage old))
                         (and new (excal--scene-rect-damage new)))))

(defun excal--laser-overlays ()
  "Return the laser trails as filled overlay polygons."
  (let ((now (float-time)) overlays)
    (dolist (trail excal--laser-trails overlays)
      (when-let* ((outline (excal--laser-outline trail now)))
        (let ((ov (excal--ov "ov-poly" 0 0 0 0 :fill excal--laser-color)))
          (aset ov 6 nil)
          (aset ov 12 (vconcat (apply #'append (mapcar (lambda (p) (list (car p) (cdr p)))
                                                       outline))))
          (push ov overlays))))))

(defvar-local excal--laser-live nil "The trail being drawn, if any.")

(defun excal--laser-prune (now)
  "Drop trails that have fully decayed by NOW, except the one being drawn."
  (setq excal--laser-trails
        (seq-filter (lambda (trail)
                      (or (eq trail excal--laser-live)
                          (< (- now (aref (car trail) 2)) excal--laser-decay-time)))
                    excal--laser-trails)))

(defun excal--laser-tick (buffer)
  "Advance the laser decay in BUFFER; stop the timer once all trails faded."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (excal--laser-prune (float-time))
      (excal--render (or (excal--laser-refresh-damage) 'full))
      (unless excal--laser-trails
        (when excal--laser-timer (cancel-timer excal--laser-timer))
        (setq excal--laser-timer nil)))))

(defun excal--laser-drag (start)
  "Draw a laser trail from scene point START while the button is held."
  (setq excal--laser-live (excal--laser-add nil (car start) (cdr start)))
  (push excal--laser-live excal--laser-trails)
  (unless excal--laser-timer
    (setq excal--laser-timer
          (run-with-timer 0 (/ 1.0 30) #'excal--laser-tick (current-buffer))))
  (unwind-protect
      (excal--drag-loop
       (lambda (ev)
         (let* ((p (excal--event-scene-xy ev))
                (trail (excal--laser-add excal--laser-live (car p) (cdr p))))
           (setq excal--laser-trails
                 (cons trail (delq excal--laser-live excal--laser-trails))
                 excal--laser-live trail)
           (excal--laser-refresh-damage))))
    (setq excal--laser-live nil)))

;;;; Eye dropper

(defconst excal--dark-filter-matrix
  [[-0.574 1.430 0.144] [0.426 0.430 0.144] [0.426 1.430 -0.856]]
  "hue-rotate(180deg), as `excal_dark_filter' applies it.")

(defun excal--undo-dark-filter (rgb)
  "Return the color that the dark theme draws as RGB (list of 0-1 floats)."
  (let* ((m excal--dark-filter-matrix)
         (e (lambda (i j) (aref (aref m i) j)))
         (det (- (+ (* (funcall e 0 0) (- (* (funcall e 1 1) (funcall e 2 2))
                                          (* (funcall e 1 2) (funcall e 2 1))))
                    (* (funcall e 0 2) (- (* (funcall e 1 0) (funcall e 2 1))
                                          (* (funcall e 1 1) (funcall e 2 0)))))
                 (* (funcall e 0 1) (- (* (funcall e 1 0) (funcall e 2 2))
                                       (* (funcall e 1 2) (funcall e 2 0))))))
         (cof (lambda (i j)
                ;; Cofactor of the transposed matrix, for the inverse.
                (let ((r (remq j '(0 1 2))) (c (remq i '(0 1 2))))
                  (* (if (cl-evenp (+ i j)) 1 -1)
                     (- (* (funcall e (nth 0 r) (nth 0 c)) (funcall e (nth 1 r) (nth 1 c)))
                        (* (funcall e (nth 0 r) (nth 1 c)) (funcall e (nth 1 r) (nth 0 c))))))))
         (inverted (cl-loop for i below 3
                            collect (/ (cl-loop for j below 3
                                                sum (* (funcall cof i j) (nth j rgb)))
                                       det))))
    ;; Undo invert(0.93): c = 0.93 - 0.86 x.
    (mapcar (lambda (c) (min 1.0 (max 0.0 (/ (- 0.93 c) 0.86)))) inverted)))

(defun excal--pixel-color (scene-xy)
  "Return the \"#rrggbb\" drawn at SCENE-XY, or nil off the canvas."
  (when-let* ((excal--fb)
              (argb (excal-native-fb-pixel
                     excal--fb
                     (floor (* (+ (car scene-xy) excal--scroll-x) excal--zoom excal--pixel-scale))
                     (floor (* (+ (cdr scene-xy) excal--scroll-y) excal--zoom excal--pixel-scale)))))
    (let ((rgb (list (/ (logand (ash argb -16) 255) 255.0)
                     (/ (logand (ash argb -8) 255) 255.0)
                     (/ (logand argb 255) 255.0))))
      (when (eq excal--theme 'dark)
        (setq rgb (excal--undo-dark-filter rgb)))
      (apply #'format "#%02x%02x%02x" (mapcar (lambda (c) (round (* 255 c))) rgb)))))

(defvar-local excal--eyedropper nil
  "The eye dropper in use: a plist with :type, :color and :at (scene point).")

(defun excal--eyedropper-overlays ()
  "Return the eye dropper's preview swatch next to the pointer."
  (when-let* ((at (plist-get excal--eyedropper :at))
              (color (plist-get excal--eyedropper :color)))
    (let ((d (/ 16.0 excal--zoom)) (s (/ 20.0 excal--zoom)))
      (list (excal--ov "ov-rect" (+ (car at) d) (+ (cdr at) d) s s
                       :stroke "#1e1e1e" :fill color)))))

(defun excal--eyedropper-damage (at)
  "Return the damage of a swatch shown at scene point AT, or nil."
  (when at
    (let ((d (/ 40.0 excal--zoom)))
      (excal--scene-rect-damage (list (car at) (cdr at) (+ (car at) d) (+ (cdr at) d))))))

(defun excal--eyedropper-apply (type color alt)
  "Apply picked COLOR as TYPE (`stroke' or `background'); ALT swaps."
  (let ((property (if (eq (eq type 'stroke) (not alt)) 'strokeColor 'backgroundColor)))
    (if (and excal--selection (eq excal--tool 'select))
        (dolist (e excal--selection)
          (excal--put e property color)
          (excal--touch e))
      (setf (alist-get property excal--current-style) color))
    (message "Picked %s %s" (if (eq property 'strokeColor) "stroke" "background") color)))

(defun excal-eyedropper (&optional type)
  "Pick a color from the canvas with the next click.
TYPE is `background' (the default) or `stroke'; meta at the click picks
the other.  Escape or any other key cancels."
  (interactive)
  (let ((type (or type 'background)))
    (setq excal--eyedropper (list :type type))
    (message "Pick a %s color: click (meta: %s); ESC cancels"
             type (if (eq type 'stroke) "background" "stroke"))
    (unwind-protect
        (let ((excal--hide-editor-overlays t))
          (excal--render)
          (track-mouse
            (catch 'done
              (while t
                (let ((event (read--potential-mouse-event)))
                  (cond
                   ((mouse-movement-p event)
                    (let* ((old (plist-get excal--eyedropper :at))
                           (at (excal--event-scene-xy event)))
                      (setq excal--eyedropper
                            (list :type type :at at :color (excal--pixel-color at)))
                      (excal--render (excal--damage-union (excal--eyedropper-damage old)
                                                          (excal--eyedropper-damage at)))))
                   ((eq (event-basic-type event) 'mouse-1)
                    (when (memq 'down (event-modifiers event))
                      (let* ((at (excal--event-scene-xy event))
                             (color (progn
                                      ;; Pick from a frame without the swatch.
                                      (setq excal--eyedropper (list :type type))
                                      (excal--render)
                                      (excal--pixel-color at))))
                        (excal--await-release)
                        (when color
                          (excal--eyedropper-apply type color
                                                   (memq 'meta (event-modifiers event))))
                        (throw 'done nil))))
                   ((memq event '(escape ?\e ?\C-g))
                    (message "Eye dropper cancelled")
                    (throw 'done nil))
                   (t
                    (push event unread-command-events)
                    (throw 'done nil))))))))
      (setq excal--eyedropper nil)
      (excal--render))))

(defun excal-eyedropper-stroke ()
  "Pick a stroke color from the canvas with the next click."
  (interactive)
  (excal-eyedropper 'stroke))

;;;; Autoshape

(defconst excal--autoshape-tolerance 0.08
  "Largest mean distance, in fractions of the box, for a recognised shape.")

(defun excal--autoshape-scores (points box)
  "Return (TYPE . SCORE) for how well POINTS follow each shape in BOX.
The score is the mean distance to the shape's outline in box units."
  (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) box)
               (w (float (max 1e-6 (- x2 x1)))) (h (float (max 1e-6 (- y2 y1))))
               (n (float (length points)))
               (uv (mapcar (lambda (p) (cons (- (* 2 (/ (- (car p) x1) w)) 1)
                                             (- (* 2 (/ (- (cdr p) y1) h)) 1)))
                           points))
               (mean (lambda (f) (/ (apply #'+ (mapcar f uv)) n))))
    (list (cons "rectangle"
                (funcall mean (lambda (p) (/ (min (- 1 (abs (car p))) (- 1 (abs (cdr p))))
                                             2))))
          (cons "ellipse"
                (funcall mean (lambda (p) (/ (abs (- (sqrt (+ (expt (car p) 2) (expt (cdr p) 2))) 1))
                                             2))))
          (cons "diamond"
                (funcall mean (lambda (p) (/ (abs (- (+ (abs (car p)) (abs (cdr p))) 1))
                                             (* 2 (sqrt 2)))))))))

(defun excal--autoshape-recognize (points)
  "Return what the freehand stroke POINTS ((X . Y) ...) looks like.
The result is (TYPE X1 Y1 X2 Y2) for a closed shape, (\"line\" FROM TO)
for a straight stroke, or nil."
  (let* ((xs (mapcar #'car points)) (ys (mapcar #'cdr points))
         (box (list (apply #'min xs) (apply #'min ys) (apply #'max xs) (apply #'max ys)))
         (w (- (nth 2 box) (nth 0 box))) (h (- (nth 3 box) (nth 1 box)))
         (size (max w h))
         (first (car points)) (last (car (last points)))
         (chord (sqrt (+ (expt (- (car last) (car first)) 2)
                         (expt (- (cdr last) (cdr first)) 2))))
         (length (cl-loop for (a b) on points while b
                          sum (sqrt (+ (expt (- (car b) (car a)) 2)
                                       (expt (- (cdr b) (cdr a)) 2))))))
    (cond
     ((< size (/ 10.0 excal--zoom)) nil)
     ((< chord (* 0.25 size))
      (when (> (min w h) (* 0.15 size))
        (let ((best (car (sort (excal--autoshape-scores points box)
                               (lambda (a b) (< (cdr a) (cdr b)))))))
          (when (< (cdr best) excal--autoshape-tolerance)
            (cons (car best) box)))))
     ((and (> length 0) (> (/ chord length) 0.92))
      (list "line" first last)))))

(defun excal--autoshape-drag (start)
  "Draw a stroke from scene point START and turn it into a shape."
  (let ((excal--tool-locked t))
    (excal--create-freedraw start))
  (let* ((stroke (car (last excal--elements)))
         (points (excal--absolute-points stroke))
         (shape (excal--autoshape-recognize points))
         (result
          (pcase shape
            (`("line" ,from ,to)
             (excal--apply-current-style
              (excal--make-element "line" (car from) (cdr from)
                                   (cons 'points (vector [0.0 0.0]
                                                         (vector (float (- (car to) (car from)))
                                                                 (float (- (cdr to) (cdr from))))))
                                   (cons 'width (abs (float (- (car to) (car from)))))
                                   (cons 'height (abs (float (- (cdr to) (cdr from)))))
                                   (cons 'startBinding :null) (cons 'endBinding :null)
                                   (cons 'startArrowhead :null) (cons 'endArrowhead :null)
                                   (cons 'polygon :false))))
            (`(,type ,x1 ,y1 ,x2 ,y2)
             (excal--apply-current-style
              (excal--make-element type x1 y1 (cons 'width (float (- x2 x1)))
                                   (cons 'height (float (- y2 y1))))))
            (_ stroke))))
    (unless (eq result stroke)
      (setq excal--elements (append (delq stroke excal--elements) (list result))))
    (unless excal--tool-locked
      (excal--deselect)
      (excal--select (list result))
      (setq excal--tool excal--preferred-selection-tool))
    result))

;;;; Lasso

(defvar-local excal--lasso nil "The lasso path being drawn, newest point first.")

(defun excal-toggle-lasso ()
  "Switch the preferred selection tool between the box and the lasso."
  (interactive)
  (setq excal--preferred-selection-tool
        (if (eq excal--preferred-selection-tool 'lasso) 'select 'lasso))
  (when (memq excal--tool '(select lasso))
    (setq excal--tool excal--preferred-selection-tool))
  (message "Selection tool: %s" excal--preferred-selection-tool))

(defun excal--lasso-outline-points (element)
  "Return points along ELEMENT's rotated outline, in scene coordinates."
  (let* ((outline (excal--outline element))
         (center (excal--box-center (excal--element-box element)))
         (angle (excal--element-angle element)))
    (mapcar (lambda (p) (excal--rotate-point p center angle)) (cdr outline))))

(defun excal--lasso-selection (polygon)
  "Return the elements whose whole unit lies inside POLYGON, in z-order."
  (let (units)
    (dolist (element (excal--live-elements))
      (unless (or (excal--get element 'locked) (excal--bound-text-p element))
        (let ((unit (excal--unit element)))
          (unless (assoc unit units)
            (push (cons unit
                        (seq-every-p
                         (lambda (e)
                           (seq-every-p (lambda (p) (excal--point-in-polygon-p p polygon))
                                        (excal--lasso-outline-points e)))
                         unit))
                  units)))))
    (apply #'append (mapcar #'car (seq-filter #'cdr (nreverse units))))))

(defun excal--lasso-overlays ()
  "Return the lasso path as an overlay."
  (when (cdr excal--lasso)
    (let ((ov (excal--ov "ov-poly" 0 0 0 0 :stroke (excal--selection-color)
                         :fill "#0000c80a" :style "dashed")))
      (aset ov 12 (vconcat (apply #'append (mapcar (lambda (p) (list (car p) (cdr p)))
                                                   (reverse excal--lasso)))))
      (list ov))))

(defun excal--lasso-box ()
  "Return the scene box around the lasso path."
  (when excal--lasso
    (let ((xs (mapcar #'car excal--lasso)) (ys (mapcar #'cdr excal--lasso)))
      (list (apply #'min xs) (apply #'min ys) (apply #'max xs) (apply #'max ys)))))

(defun excal--lasso-drag (start add)
  "Select with a free-form path from scene point START.
With ADD, extend the existing selection instead of replacing it.  The
path skips points closer than 5 screen px (`simplifyDistance')."
  (let ((base (and add excal--selection)))
    (setq excal--lasso (list start))
    (unwind-protect
        (excal--drag-loop
         (lambda (ev)
           (let ((p (excal--event-scene-xy ev))
                 (old-box (excal--lasso-box))
                 (old-selection excal--selection))
             (when (>= (sqrt (+ (expt (- (car p) (caar excal--lasso)) 2)
                                (expt (- (cdr p) (cdar excal--lasso)) 2)))
                       (/ 5.0 excal--zoom))
               (push p excal--lasso)
               (setq excal--selection nil)
               (excal--select (append base (excal--lasso-selection excal--lasso)))
               (excal--damage-union
                (excal--damage-union (excal--scene-rect-damage old-box)
                                     (excal--scene-rect-damage (excal--lasso-box)))
                (unless (equal old-selection excal--selection)
                  (excal--elements-damage (append old-selection excal--selection))))))))
      (setq excal--lasso nil))))

;;;; Overlays

(defun excal--tool-overlays ()
  "Return the overlays of the laser, eye dropper and lasso."
  (append (excal--laser-overlays) (excal--lasso-overlays) (excal--eyedropper-overlays)))

(provide 'excal-tools)
;;; excal-tools.el ends here
