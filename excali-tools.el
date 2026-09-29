;;; excali-tools.el --- Laser, eye dropper, autoshape and lasso  -*- lexical-binding: t; -*-

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

(require 'excali-core)
(require 'excali-view)
(require 'excali-select)
(require 'excali-handles)
(require 'excali-hit)
(require 'excali-style)

(declare-function excali--drag-loop "excali-edit")
(declare-function excali--await-release "excali-edit")
(declare-function excali--create-freedraw "excali-create")
(declare-function excali--created "excali-create")
(declare-function excali--add-new "excali-create")
(defvar excali--tool-locked)
(defvar excali--previous-tool)
(defvar excali--canvas-size)
(defvar excali--pixel-scale)
(defvar excali--fb)
(defvar excali--hide-editor-overlays)

;;;; Laser

(defconst excali--laser-color "#ff0000" "DEFAULT_LASER_COLOR (\"red\").")
(defconst excali--laser-decay-time 1.0 "DECAY_TIME, in seconds.")
(defconst excali--laser-decay-length 50 "DECAY_LENGTH, in points.")
(defconst excali--laser-streamline 0.4 "The trail's `streamline' option.")
(defconst excali--laser-size 2.0
  "Trail radius in screen px at full size (laser-pointer's `size').")

(defvar-local excali--laser-trails nil
  "Laser trails on screen: lists of [X Y TIME] points, newest first.")
(defvar-local excali--laser-timer nil "Timer animating the laser decay.")
(defvar-local excali--laser-damage nil "Scene box the trails last covered.")

(defun excali--ease-out (k)
  "Return the cubic ease-out of K."
  (- 1 (expt (- 1 k) 3)))

(defun excali--laser-add (trail x y &optional time)
  "Add scene point X, Y to TRAIL (newest first) and return it.
The point is pulled toward the last one by the streamline factor."
  (let ((time (or time (float-time)))
        (last (car trail)))
    (if last
        (let ((f (- 1 excali--laser-streamline)))
          (cons (vector (+ (aref last 0) (* f (- x (aref last 0))))
                        (+ (aref last 1) (* f (- y (aref last 1))))
                        time)
                trail))
      (list (vector (float x) (float y) time)))))

(defun excali--laser-radii (points now)
  "Return the radius of each of POINTS (oldest first) at time NOW.
Upstream's sizeMapping: the least of the eased length and time decay."
  (let ((n (length points)) (i 0) radii)
    (dolist (p points (nreverse radii))
      (let* ((l (/ (- excali--laser-decay-length (min excali--laser-decay-length (- n i)))
                   (float excali--laser-decay-length)))
             (tt (max 0.0 (- 1 (/ (- now (aref p 2)) excali--laser-decay-time)))))
        (push (* (/ excali--laser-size excali--zoom)
                 (min (excali--ease-out l) (excali--ease-out tt)))
              radii)
        (setq i (1+ i))))))

(defun excali--laser-outline (trail now)
  "Return TRAIL's outline at NOW as a list of (X . Y), or nil when gone.
Points are offset by their radius on both sides, with round caps."
  (let* ((points (reverse trail))
         (radii (excali--laser-radii points now))
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

(defun excali--laser-box ()
  "Return the scene box around all laser trails, or nil."
  (let ((pad (/ (* 2 excali--laser-size) excali--zoom)) box)
    (dolist (trail excali--laser-trails box)
      (dolist (p trail)
        (let ((x (aref p 0)) (y (aref p 1)))
          (setq box (if box
                        (list (min (nth 0 box) (- x pad)) (min (nth 1 box) (- y pad))
                              (max (nth 2 box) (+ x pad)) (max (nth 3 box) (+ y pad)))
                      (list (- x pad) (- y pad) (+ x pad) (+ y pad)))))))))

(defun excali--laser-refresh-damage ()
  "Return the damage for redrawing the trails, updating the record."
  (let ((old excali--laser-damage) (new (excali--laser-box)))
    (setq excali--laser-damage new)
    (excali--damage-union (and old (excali--scene-rect-damage old))
                         (and new (excali--scene-rect-damage new)))))

(defun excali--laser-overlays ()
  "Return the laser trails as filled overlay polygons."
  (let ((now (float-time)) overlays)
    (dolist (trail excali--laser-trails overlays)
      (when-let* ((outline (excali--laser-outline trail now)))
        (let ((ov (excali--ov "ov-poly" 0 0 0 0 :fill excali--laser-color)))
          (aset ov 6 nil)
          (aset ov 12 (vconcat (apply #'append (mapcar (lambda (p) (list (car p) (cdr p)))
                                                       outline))))
          (push ov overlays))))))

(defvar-local excali--laser-live nil "The trail being drawn, if any.")

(defun excali--laser-prune (now)
  "Drop trails that have fully decayed by NOW, except the one being drawn."
  (setq excali--laser-trails
        (seq-filter (lambda (trail)
                      (or (eq trail excali--laser-live)
                          (< (- now (aref (car trail) 2)) excali--laser-decay-time)))
                    excali--laser-trails)))

(defun excali--laser-tick (buffer &optional window)
  "Advance the laser decay in BUFFER; stop the timer once all trails faded.
The trails show in WINDOW's view, where they were drawn, if given."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (excali--with-view (if (window-live-p window) window excali--view-window)
        (excali--laser-prune (float-time))
        (excali--render (or (excali--laser-refresh-damage) 'full)))
      (unless excali--laser-trails
        (when excali--laser-timer (cancel-timer excali--laser-timer))
        (setq excali--laser-timer nil)))))

(defun excali--laser-drag (start)
  "Draw a laser trail from scene point START while the button is held."
  (setq excali--laser-live (excali--laser-add nil (car start) (cdr start)))
  (push excali--laser-live excali--laser-trails)
  (unless excali--laser-timer
    (setq excali--laser-timer
          (run-with-timer 0 (/ 1.0 30) #'excali--laser-tick (current-buffer)
                          excali--view-window)))
  (unwind-protect
      (excali--drag-loop
       (lambda (ev)
         (let* ((p (excali--event-scene-xy ev))
                (trail (excali--laser-add excali--laser-live (car p) (cdr p))))
           (setq excali--laser-trails
                 (cons trail (delq excali--laser-live excali--laser-trails))
                 excali--laser-live trail)
           (excali--laser-refresh-damage))))
    (setq excali--laser-live nil)))

;;;; Eye dropper

(defconst excali--dark-filter-matrix
  [[-0.574 1.430 0.144] [0.426 0.430 0.144] [0.426 1.430 -0.856]]
  "hue-rotate(180deg), as `excali_dark_filter' applies it.")

(defun excali--undo-dark-filter (rgb)
  "Return the color that the dark theme draws as RGB (list of 0-1 floats)."
  (let* ((m excali--dark-filter-matrix)
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

(defun excali--pixel-color (scene-xy)
  "Return the \"#rrggbb\" drawn at SCENE-XY, or nil off the canvas."
  (when-let* ((excali--fb)
              (argb (excali-native-fb-pixel
                     excali--fb
                     (floor (* (+ (car scene-xy) excali--scroll-x) excali--zoom excali--pixel-scale))
                     (floor (* (+ (cdr scene-xy) excali--scroll-y) excali--zoom excali--pixel-scale)))))
    (let ((rgb (list (/ (logand (ash argb -16) 255) 255.0)
                     (/ (logand (ash argb -8) 255) 255.0)
                     (/ (logand argb 255) 255.0))))
      (when (eq excali--theme 'dark)
        (setq rgb (excali--undo-dark-filter rgb)))
      (apply #'format "#%02x%02x%02x" (mapcar (lambda (c) (round (* 255 c))) rgb)))))

(defvar-local excali--eyedropper nil
  "The eye dropper in use: a plist with :type, :color and :at (scene point).")

(defun excali--eyedropper-overlays ()
  "Return the eye dropper's preview swatch next to the pointer."
  (when-let* ((at (plist-get excali--eyedropper :at))
              (color (plist-get excali--eyedropper :color)))
    (let ((d (/ 16.0 excali--zoom)) (s (/ 20.0 excali--zoom)))
      (list (excali--ov "ov-rect" (+ (car at) d) (+ (cdr at) d) s s
                       :stroke "#1e1e1e" :fill color)))))

(defun excali--eyedropper-damage (at)
  "Return the damage of a swatch shown at scene point AT, or nil."
  (when at
    (let ((d (/ 40.0 excali--zoom)))
      (excali--scene-rect-damage (list (car at) (cdr at) (+ (car at) d) (+ (cdr at) d))))))

(defun excali--eyedropper-apply (type color alt)
  "Apply picked COLOR as TYPE (`stroke' or `background'); ALT swaps."
  (let ((property (if (eq (eq type 'stroke) (not alt)) 'strokeColor 'backgroundColor)))
    (if (and excali--selection (eq excali--tool 'select))
        (dolist (e excali--selection)
          (excali--put e property color)
          (excali--touch e))
      (setf (alist-get property excali--current-style) color))
    (message "Picked %s %s" (if (eq property 'strokeColor) "stroke" "background") color)))

(defun excali-eyedropper (&optional type)
  "Pick a color from the canvas with the next click.
TYPE is `background' (the default) or `stroke'; meta at the click picks
the other.  Escape or any other key cancels."
  (interactive)
  (let ((type (or type 'background)))
    (setq excali--eyedropper (list :type type))
    (message "Pick a %s color: click (meta: %s); ESC cancels"
             type (if (eq type 'stroke) "background" "stroke"))
    (unwind-protect
        (let ((excali--hide-editor-overlays t))
          (excali--render)
          (track-mouse
            (catch 'done
              (while t
                (let ((event (read--potential-mouse-event)))
                  (cond
                   ((mouse-movement-p event)
                    (let* ((old (plist-get excali--eyedropper :at))
                           (at (excali--event-scene-xy event)))
                      (setq excali--eyedropper
                            (list :type type :at at :color (excali--pixel-color at)))
                      (excali--render (excali--damage-union (excali--eyedropper-damage old)
                                                          (excali--eyedropper-damage at)))))
                   ((eq (event-basic-type event) 'mouse-1)
                    (when (memq 'down (event-modifiers event))
                      (let* ((at (excali--event-scene-xy event))
                             (color (progn
                                      ;; Pick from a frame without the swatch.
                                      (setq excali--eyedropper (list :type type))
                                      (excali--render)
                                      (excali--pixel-color at))))
                        (excali--await-release)
                        (when color
                          (excali--eyedropper-apply type color
                                                   (memq 'meta (event-modifiers event))))
                        (throw 'done nil))))
                   ((memq event '(escape ?\e ?\C-g))
                    (message "Eye dropper cancelled")
                    (throw 'done nil))
                   (t
                    (push event unread-command-events)
                    (throw 'done nil))))))))
      (setq excali--eyedropper nil)
      (excali--render))))

(defun excali-eyedropper-stroke ()
  "Pick a stroke color from the canvas with the next click."
  (interactive)
  (excali-eyedropper 'stroke))

;;;; Autoshape

(defconst excali--autoshape-tolerance 0.08
  "Largest mean distance, in fractions of the box, for a recognised shape.")

(defun excali--autoshape-scores (points box)
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

(defun excali--autoshape-recognize (points)
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
     ((< size (/ 10.0 excali--zoom)) nil)
     ((< chord (* 0.25 size))
      (when (> (min w h) (* 0.15 size))
        (let ((best (car (sort (excali--autoshape-scores points box)
                               (lambda (a b) (< (cdr a) (cdr b)))))))
          (when (< (cdr best) excali--autoshape-tolerance)
            (cons (car best) box)))))
     ((and (> length 0) (> (/ chord length) 0.92))
      (list "line" first last)))))

(defun excali--autoshape-drag (start)
  "Draw a stroke from scene point START and turn it into a shape."
  (let ((excali--tool-locked t))
    (excali--create-freedraw start))
  (let* ((stroke (car (last excali--elements)))
         (points (excali--absolute-points stroke))
         (shape (excali--autoshape-recognize points))
         (result
          (pcase shape
            (`("line" ,from ,to)
             (excali--apply-current-style
              (excali--make-element "line" (car from) (cdr from)
                                   (cons 'points (vector [0.0 0.0]
                                                         (vector (float (- (car to) (car from)))
                                                                 (float (- (cdr to) (cdr from))))))
                                   (cons 'width (abs (float (- (car to) (car from)))))
                                   (cons 'height (abs (float (- (cdr to) (cdr from)))))
                                   (cons 'startBinding :null) (cons 'endBinding :null)
                                   (cons 'startArrowhead :null) (cons 'endArrowhead :null)
                                   (cons 'polygon :false))))
            (`(,type ,x1 ,y1 ,x2 ,y2)
             (excali--apply-current-style
              (excali--make-element type x1 y1 (cons 'width (float (- x2 x1)))
                                   (cons 'height (float (- y2 y1))))))
            (_ stroke))))
    (unless (eq result stroke)
      (setq excali--elements (append (delq stroke excali--elements) (list result))))
    (unless excali--tool-locked
      (excali--deselect)
      (excali--select (list result))
      (setq excali--tool excali--preferred-selection-tool))
    result))

;;;; Lasso

(defvar-local excali--lasso nil "The lasso path being drawn, newest point first.")

(defun excali-toggle-lasso ()
  "Switch the preferred selection tool between the box and the lasso."
  (interactive)
  (setq excali--preferred-selection-tool
        (if (eq excali--preferred-selection-tool 'lasso) 'select 'lasso))
  (when (memq excali--tool '(select lasso))
    (setq excali--tool excali--preferred-selection-tool))
  (message "Selection tool: %s" excali--preferred-selection-tool))

(defun excali--lasso-outline-points (element)
  "Return points along ELEMENT's rotated outline, in scene coordinates."
  (let* ((outline (excali--outline element))
         (center (excali--box-center (excali--element-box element)))
         (angle (excali--element-angle element)))
    (mapcar (lambda (p) (excali--rotate-point p center angle)) (cdr outline))))

(defun excali--lasso-selection (polygon)
  "Return the elements whose whole unit lies inside POLYGON, in z-order."
  (let (units)
    (dolist (element (excali--live-elements))
      (unless (or (excali--get element 'locked) (excali--bound-text-p element))
        (let ((unit (excali--unit element)))
          (unless (assoc unit units)
            (push (cons unit
                        (seq-every-p
                         (lambda (e)
                           (seq-every-p (lambda (p) (excali--point-in-polygon-p p polygon))
                                        (excali--lasso-outline-points e)))
                         unit))
                  units)))))
    (apply #'append (mapcar #'car (seq-filter #'cdr (nreverse units))))))

(defun excali--lasso-overlays ()
  "Return the lasso path as an overlay."
  (when (cdr excali--lasso)
    (let ((ov (excali--ov "ov-poly" 0 0 0 0 :stroke (excali--selection-color)
                         :fill "#0000c80a" :style "dashed")))
      (aset ov 12 (vconcat (apply #'append (mapcar (lambda (p) (list (car p) (cdr p)))
                                                   (reverse excali--lasso)))))
      (list ov))))

(defun excali--lasso-box ()
  "Return the scene box around the lasso path."
  (when excali--lasso
    (let ((xs (mapcar #'car excali--lasso)) (ys (mapcar #'cdr excali--lasso)))
      (list (apply #'min xs) (apply #'min ys) (apply #'max xs) (apply #'max ys)))))

(defun excali--lasso-drag (start add)
  "Select with a free-form path from scene point START.
With ADD, extend the existing selection instead of replacing it.  The
path skips points closer than 5 screen px (`simplifyDistance')."
  (let ((base (and add excali--selection)))
    (setq excali--lasso (list start))
    (unwind-protect
        (excali--drag-loop
         (lambda (ev)
           (let ((p (excali--event-scene-xy ev))
                 (old-box (excali--lasso-box))
                 (old-selection excali--selection))
             (when (>= (sqrt (+ (expt (- (car p) (caar excali--lasso)) 2)
                                (expt (- (cdr p) (cdar excali--lasso)) 2)))
                       (/ 5.0 excali--zoom))
               (push p excali--lasso)
               (setq excali--selection nil)
               (excali--select (append base (excali--lasso-selection excali--lasso)))
               (excali--damage-union
                (excali--damage-union (excali--scene-rect-damage old-box)
                                     (excali--scene-rect-damage (excali--lasso-box)))
                (unless (equal old-selection excali--selection)
                  (excali--elements-damage (append old-selection excali--selection))))))))
      (setq excali--lasso nil))))

;;;; Overlays

(defun excali--tool-overlays ()
  "Return the overlays of the laser, eye dropper and lasso."
  (append (excali--laser-overlays) (excali--lasso-overlays) (excali--eyedropper-overlays)))

(provide 'excali-tools)
;;; excali-tools.el ends here
