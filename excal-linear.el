;;; excal-linear.el --- Point editor for lines and arrows  -*- lexical-binding: t; -*-

;;; Commentary:

;; Editing the points of lines and arrows, following Excalidraw's
;; LinearElementEditor (docs/excalidraw-spec.md §3b.2, §2c.9).
;;
;; A lone selected line or arrow shows its points and can have them
;; dragged; a two-point one also offers its segment midpoint.  In edit
;; mode (double-click a line, RET on a line, s-RET on any linear
;; element) points are larger, every long enough segment offers a
;; midpoint that inserts a point when dragged, shift-click toggles point
;; selection, meta-click appends a point and DEL deletes selected points.
;; Dragging an arrow end binds it like drawing does.

;;; Code:

(require 'excal-core)
(require 'excal-view)
(require 'excal-select)
(require 'excal-handles)
(require 'excal-transform)
(require 'excal-binding)
(require 'excal-create)

(defconst excal--point-hit-size 11
  "POINT_HANDLE_SIZE + 1: screen px within which a point is grabbed.")


(declare-function excal--drag-loop "excal-edit")

;;;; Geometry

(defun excal--linear-scene-points (element)
  "Return ELEMENT's points in scene coordinates, rotation applied."
  (let ((center (excal--box-center (excal--element-box element)))
        (angle (excal--element-angle element)))
    (mapcar (lambda (p) (excal--rotate-point p center angle))
            (excal--absolute-points element))))

(defun excal--set-linear-scene-points (element points)
  "Give ELEMENT the scene POINTS, keeping its rotation.
The points are taken into the element's unrotated frame about its old
center; the new box center then gets the usual compensation so the
points land exactly where given."
  (let* ((old-center (excal--box-center (excal--element-box element)))
         (angle (excal--element-angle element))
         (local (mapcar (lambda (p) (excal--rotate-point p old-center (- angle))) points))
         (xs (mapcar #'car local)) (ys (mapcar #'cdr local))
         (new-center (cons (/ (+ (apply #'min xs) (apply #'max xs)) 2.0)
                           (/ (+ (apply #'min ys) (apply #'max ys)) 2.0)))
         (shift (excal--rotation-compensation old-center new-center angle))
         (first (car local))
         (ox (+ (car first) (car shift))) (oy (+ (cdr first) (cdr shift))))
    (excal--put element 'x (float ox))
    (excal--put element 'y (float oy))
    (excal--put element 'points
                (vconcat (mapcar (lambda (p) (vector (- (+ (car p) (car shift)) ox)
                                                     (- (+ (cdr p) (cdr shift)) oy)))
                                 local)))
    (excal--linear-extent element)
    (excal--touch element)))

(defun excal--linear-target ()
  "Return the line or arrow whose points are shown, or nil."
  (or excal--editing-linear
      (let ((single (excal--single-selection)))
        (and single (excal--linear-p single) single))))

(defun excal--segment-midpoints (element &optional all)
  "Return (INDEX . SCENE-POINT) midpoints of ELEMENT's segments.
INDEX is the position a new point would take.  Segments shorter than 40
screen px are skipped; unless ALL, only two-point elements offer one."
  (let ((points (excal--linear-scene-points element)))
    (when (or all (= (length points) 2))
      (cl-loop for (a b) on points
               for i from 1
               while b
               when (>= (* excal--zoom (sqrt (+ (expt (- (car b) (car a)) 2)
                                                (expt (- (cdr b) (cdr a)) 2))))
                        (* 4 excal--point-handle-size))
               collect (cons i (cons (/ (+ (car a) (car b)) 2.0)
                                     (/ (+ (cdr a) (cdr b)) 2.0)))))))

(defun excal--point-at (element scene-xy)
  "Return the index of ELEMENT's point under SCENE-XY, or nil."
  (let ((limit (/ (float excal--point-hit-size) excal--zoom)))
    (cl-position-if (lambda (p) (< (sqrt (+ (expt (- (car p) (car scene-xy)) 2)
                                            (expt (- (cdr p) (cdr scene-xy)) 2)))
                                   limit))
                    (excal--linear-scene-points element))))

(defun excal--midpoint-at (element scene-xy)
  "Return the midpoint (INDEX . POINT) of ELEMENT under SCENE-XY, or nil."
  (let ((limit (/ (float excal--point-hit-size) excal--zoom)))
    (cl-find-if (lambda (m) (< (sqrt (+ (expt (- (cadr m) (car scene-xy)) 2)
                                        (expt (- (cddr m) (cdr scene-xy)) 2)))
                               limit))
                (excal--segment-midpoints element (eq element excal--editing-linear)))))

;;;; Overlays

(defun excal--linear-editor-overlays ()
  "Return point and midpoint overlays for the shown line or arrow."
  (when-let* ((element (excal--linear-target)))
    (let* ((editing (eq element excal--editing-linear))
           (diameter (/ (* 2.0 (if editing excal--point-handle-size
                                 (/ excal--point-handle-size 2.0)))
                        excal--zoom))
           (mid (/ (* 2.0 5) excal--zoom))
           (i -1))
      (append
       (mapcar (lambda (m)
                 (excal--ov "ov-circle" (- (cadr m) (/ mid 2)) (- (cddr m) (/ mid 2)) mid mid
                            :fill "#b197fcb3"))
               (excal--segment-midpoints element editing))
       (mapcar (lambda (p)
                 (cl-incf i)
                 (excal--ov "ov-circle" (- (car p) (/ diameter 2)) (- (cdr p) (/ diameter 2))
                            diameter diameter :stroke "#5e5ad8"
                            :fill (if (and editing (memq i excal--selected-points))
                                      "#8683e2e6" "#ffffffe6")))
               (excal--linear-scene-points element))))))

;;;; Editing

(defun excal-edit-linear (&optional any)
  "Edit the points of the selected line.
With ANY, or interactively with a prefix argument, arrows qualify too."
  (interactive "P")
  (let ((element (excal--single-selection)))
    (when (and element
               (if any (excal--linear-p element)
                 (equal (excal--get element 'type) "line")))
      (setq excal--editing-linear element
            excal--selected-points nil)
      (excal--render))))

(defun excal-edit-linear-any ()
  "Edit the points of the selected line or arrow."
  (interactive)
  (excal-edit-linear t))

(defun excal-stop-editing-linear ()
  "Leave point-edit mode."
  (interactive)
  (setq excal--editing-linear nil
        excal--selected-points nil)
  (excal--render))

(defun excal--arrow-end-for (element index)
  "Return `start' or `end' if INDEX is an end of arrow ELEMENT, else nil."
  (when (equal (excal--get element 'type) "arrow")
    (cond ((= index 0) 'start)
          ((= index (1- (length (excal--get element 'points)))) 'end))))

(defun excal--drag-points (element indices start &optional lock-angle)
  "Drag ELEMENT's points INDICES by the mouse from scene point START.
With LOCK-ANGLE and a single point, its segment keeps a 15-degree step
from the neighbouring point.  Dragged arrow ends bind like drawing does."
  (let* ((origin (excal--linear-scene-points element))
         (ends (delq nil (mapcar (lambda (i) (cons (excal--arrow-end-for element i) i))
                                 indices)))
         (ends (seq-filter #'car ends)))
    (excal--drag-loop
     (lambda (ev)
       (let* ((p (excal--event-scene-xy ev))
              (dx (- (car p) (car start))) (dy (- (cdr p) (cdr start)))
              (points (copy-sequence origin)))
         (dolist (i indices)
           (let ((o (nth i origin)))
             (setf (nth i points)
                   (if (and lock-angle (null (cdr indices)))
                       (let* ((pivot (nth (if (> i 0) (1- i) 1) origin))
                              (d (excal--lock-angle (- (+ (car o) dx) (car pivot))
                                                    (- (+ (cdr o) dy) (cdr pivot)))))
                         (cons (+ (car pivot) (car d)) (+ (cdr pivot) (cdr d))))
                     (cons (+ (car o) dx) (+ (cdr o) dy))))))
         (excal--damage-union
          (excal--with-damage element
            (excal--set-linear-scene-points element points))
          (when ends
            (let ((old excal--binding-highlight))
              (setq excal--binding-highlight
                    (excal--binding-candidate (nth (cdar ends) points) (list element)))
              (unless (eq old excal--binding-highlight)
                (excal--elements-damage (delq nil (list old excal--binding-highlight))))))))))
    ;; Bind or release dragged arrow ends, then snap bound ends.
    (pcase-dolist (`(,end . ,i) ends)
      (let* ((point (nth i (excal--linear-scene-points element)))
             (target (excal--binding-candidate point (list element))))
        (if target
            (excal--bind-end element end target point)
          (excal--unbind-end element end))))
    (setq excal--binding-highlight nil)
    (when ends (excal--update-arrow element))))

(defun excal--linear-mouse-down (event start)
  "Handle a press at START on the shown line or arrow's points.
Return non-nil if the press was handled."
  (when-let* ((element (excal--linear-target)))
    (let* ((mods (event-modifiers event))
           (editing (eq element excal--editing-linear))
           (index (excal--point-at element start))
           (mid (and (not index) (excal--midpoint-at element start))))
      (cond
       (index
        (cond ((and editing (memq 'shift mods))
               (setq excal--selected-points
                     (if (memq index excal--selected-points)
                         (delq index excal--selected-points)
                       (cons index excal--selected-points))))
              ((not (memq index excal--selected-points))
               (setq excal--selected-points (list index))))
        (excal--render)
        (excal--drag-points element (if editing excal--selected-points (list index))
                            start (and (not editing) (memq 'shift mods)))
        t)
       (mid
        ;; Insert a point at the midpoint and drag it.
        (let ((points (excal--linear-scene-points element)))
          (excal--set-linear-scene-points
           element (append (seq-take points (car mid)) (list (cdr mid))
                           (nthcdr (car mid) points)))
          (setq excal--selected-points (list (car mid)))
          (excal--drag-points element (list (car mid)) start))
        t)
       ((and editing (memq 'meta mods))
        (excal--set-linear-scene-points
         element (append (excal--linear-scene-points element) (list start)))
        (setq excal--selected-points
              (list (1- (length (excal--get element 'points)))))
        (excal--render)
        t)
       (editing
        ;; A press anywhere else leaves the editor and is handled normally.
        (setq excal--editing-linear nil excal--selected-points nil)
        nil)))))

(defun excal-delete-points ()
  "Delete the selected points of the edited line or arrow.
Deleting all but one point deletes the element."
  (interactive)
  (when-let* ((element excal--editing-linear)
              (selected excal--selected-points))
    (let* ((points (excal--linear-scene-points element))
           (kept (cl-loop for p in points for i from 0
                          unless (memq i selected) collect p)))
      (setq excal--selected-points nil)
      (if (< (length kept) 2)
          (progn (excal--put element 'isDeleted t)
                 (excal--touch element)
                 (setq excal--editing-linear nil)
                 (excal--deselect))
        (excal--set-linear-scene-points element kept)
        (when (equal (excal--get element 'type) "arrow")
          (excal--update-arrow element)))
      (excal--render))))

(provide 'excal-linear)
;;; excal-linear.el ends here
