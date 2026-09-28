;;; excal-frame.el --- Frame behavior  -*- lexical-binding: t; -*-

;;; Commentary:

;; Frames following Excalidraw's frame.ts (docs/excalidraw-spec.md §3b.9):
;;
;; - a new frame adopts the elements (whole groups only) completely
;;   inside it, and they move just below it in z-order; frames do not
;;   nest;
;; - an element drawn inside a frame joins the topmost frame there;
;; - moving a frame moves its children; after a drag, elements dropped on
;;   a frame join it and elements dragged off leave;
;; - deleting a frame releases its children and selects them;
;; - a frame and its children are never selected together.
;;
;; Rendering (border, name, clipping) lives elsewhere.

;;; Code:

(require 'excal-core)
(require 'excal-view)
(require 'excal-select)
(require 'excal-handles)
(require 'excal-hit)

(defconst excal--frame-style
  '((strokeColor . "#bbb") (strokeWidth . 2) (strokeStyle . "solid")
    (fillStyle . "solid") (roughness . 0) (roundness . :null)
    (backgroundColor . "transparent"))
  "FRAME_STYLE fields stored on frame elements.")

(defconst excal-default-frame-name "Frame" "Name shown for unnamed frames.")

;;;; Queries

(defun excal--frame-p (element)
  "Return non-nil if ELEMENT is a frame (or magic frame)."
  (member (excal--get element 'type) '("frame" "magicframe")))

(defun excal--frame-children (frame)
  "Return the live elements belonging to FRAME, in z-order."
  (let ((id (excal--get frame 'id)))
    (seq-filter (lambda (e) (equal (excal--get e 'frameId) id)) (excal--live-elements))))

(defun excal--frame-at (point &optional exclude)
  "Return the topmost unlocked frame containing scene POINT, or nil.
Frames in EXCLUDE are skipped."
  (cl-find-if (lambda (e)
                (and (excal--frame-p e) (not (excal--get e 'locked))
                     (not (memq e exclude))
                     (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excal--element-box e)))
                       (and (<= x1 (car point) x2) (<= y1 (cdr point) y2)))))
              (reverse (excal--live-elements))))

(defun excal--boxes-overlap-p (a b)
  "Return non-nil if boxes A and B intersect."
  (and (<= (nth 0 a) (nth 2 b)) (<= (nth 0 b) (nth 2 a))
       (<= (nth 1 a) (nth 3 b)) (<= (nth 1 b) (nth 3 a))))

(defun excal--overlaps-frame-p (element frame)
  "Return non-nil if ELEMENT is inside, crosses or contains FRAME's box.
Boxes are compared, a simplification of `elementOverlapsWithFrame'."
  (excal--boxes-overlap-p (excal--elements-bounds (list element))
                          (excal--element-box frame)))

(defun excal-frame-name (frame)
  "Return the name shown for FRAME."
  (let ((name (excal--get frame 'name)))
    (if (and (stringp name) (not (string-empty-p name))) name
      (if (equal (excal--get frame 'type) "magicframe") "AI Frame"
        excal-default-frame-name))))

;;;; Membership

(defun excal--set-frame (elements frame)
  "Put ELEMENTS (and their labels) in FRAME, or out of any frame if nil."
  (dolist (e elements)
    (dolist (x (cons e (delq nil (list (and (fboundp 'excal--bound-text-of)
                                            (excal--bound-text-of e))))))
      (unless (equal (excal--get x 'frameId) (and frame (excal--get frame 'id)))
        (excal--put x 'frameId (if frame (excal--get frame 'id) :null))
        (excal--touch x)))))

(defun excal--place-below (elements frame)
  "Move ELEMENTS just below FRAME in z-order, keeping their order."
  (let* ((rest (seq-remove (lambda (e) (memq e elements)) excal--elements))
         (tail (memq frame rest)))
    (when tail
      (let ((before (seq-take rest (- (length rest) (length tail)))))
        (setq excal--elements (append before (seq-filter (lambda (e) (memq e elements))
                                                         excal--elements)
                                      tail))))))

(defun excal--elements-in-new-frame (frame)
  "Return the elements a new FRAME adopts (`getElementsInNewFrame').
Elements completely inside it that are not frames and not already in
another frame; groups only when all their members qualify."
  (let* ((box (excal--element-box frame))
         (candidates
          (seq-filter (lambda (e)
                        (and (not (eq e frame)) (not (excal--frame-p e))
                             (not (excal--bound-text-p e))
                             (let ((f (excal--get e 'frameId)))
                               (or (null f) (equal f (excal--get frame 'id))))
                             (excal--inside-p (excal--elements-bounds (list e)) box)))
                      (excal--live-elements))))
    (seq-filter (lambda (e)
                  (let ((group (car (last (append (excal--get e 'groupIds) nil)))))
                    (or (null group)
                        (seq-every-p (lambda (m) (memq m candidates))
                                     (excal--group-members group)))))
                candidates)))

(defun excal--adopt-into-frame (frame)
  "Give new FRAME the elements inside it and order them below it."
  (let ((children (excal--elements-in-new-frame frame)))
    (when children
      (excal--set-frame children frame)
      (excal--place-below children frame))
    children))

(defun excal--update-frame-membership (moved point)
  "Re-file MOVED elements after a drag released at scene POINT.
Dropping on a frame adds those overlapping it; elsewhere, elements that
no longer overlap their frame leave it."
  (let* ((movable (seq-remove #'excal--frame-p moved))
         (target (excal--frame-at point moved)))
    (if target
        (let ((joining (seq-filter (lambda (e) (excal--overlaps-frame-p e target)) movable)))
          (excal--set-frame joining target))
      (dolist (e movable)
        (when-let* ((frame (excal--live-element-by-id (excal--get e 'frameId))))
          (unless (excal--overlaps-frame-p e frame)
            (excal--set-frame (list e) nil)))))))

(defun excal--update-resized-frames (frames)
  "Refresh the children of FRAMES after resizing them.
Children no longer overlapping leave; ungrouped elements now completely
inside join (`getElementsInResizingFrame', simplified)."
  (dolist (frame frames)
    (dolist (child (excal--frame-children frame))
      (unless (or (excal--bound-text-p child) (excal--overlaps-frame-p child frame))
        (excal--set-frame (list child) nil)))
    (excal--adopt-into-frame frame)))

(defun excal--with-frame-children (elements)
  "Return ELEMENTS plus the children of the frames among them."
  (seq-union elements (apply #'append (mapcar #'excal--frame-children
                                              (seq-filter #'excal--frame-p elements)))))

(defun excal--drop-frame-children (elements)
  "Return ELEMENTS without children of frames also in ELEMENTS.
A frame and its children are never selected together."
  (let ((ids (delq nil (mapcar (lambda (e) (and (excal--frame-p e) (excal--get e 'id)))
                               elements))))
    (seq-remove (lambda (e) (member (excal--get e 'frameId) ids)) elements)))

(defun excal--release-frame-children (frames)
  "Release the children of FRAMES, which are being deleted; return them."
  (let ((children (apply #'append (mapcar #'excal--frame-children frames))))
    (excal--set-frame children nil)
    children))

;;;; Creation and naming

(defun excal--new-frame (x y)
  "Return a new frame element at X, Y."
  (apply #'excal--make-element "frame" x y (cons 'name :null) excal--frame-style))

(defun excal-rename-frame ()
  "Rename the selected frame."
  (interactive)
  (let ((frame (excal--single-selection)))
    (when (and frame (excal--frame-p frame))
      (let ((name (read-string "Frame name: " (excal-frame-name frame))))
        (excal--put frame 'name (if (string-empty-p name) :null name))
        (excal--touch frame)
        (excal--render)))))

(provide 'excal-frame)
;;; excal-frame.el ends here
