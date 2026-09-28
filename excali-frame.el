;;; excali-frame.el --- Frame behavior  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

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

(require 'excali-core)
(require 'excali-view)
(require 'excali-select)
(require 'excali-handles)
(require 'excali-hit)

(defconst excali--frame-style
  '((strokeColor . "#bbb") (strokeWidth . 2) (strokeStyle . "solid")
    (fillStyle . "solid") (roughness . 0) (roundness . :null)
    (backgroundColor . "transparent"))
  "FRAME_STYLE fields stored on frame elements.")

(defconst excali-default-frame-name "Frame" "Name shown for unnamed frames.")

;;;; Queries

(defun excali--frame-p (element)
  "Return non-nil if ELEMENT is a frame (or magic frame)."
  (member (excali--get element 'type) '("frame" "magicframe")))

(defun excali--frame-children (frame)
  "Return the live elements belonging to FRAME, in z-order."
  (let ((id (excali--get frame 'id)))
    (seq-filter (lambda (e) (equal (excali--get e 'frameId) id)) (excali--live-elements))))

(defun excali--frame-at (point &optional exclude)
  "Return the topmost unlocked frame containing scene POINT, or nil.
Frames in EXCLUDE are skipped."
  (cl-find-if (lambda (e)
                (and (excali--frame-p e) (not (excali--get e 'locked))
                     (not (memq e exclude))
                     (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excali--element-box e)))
                       (and (<= x1 (car point) x2) (<= y1 (cdr point) y2)))))
              (reverse (excali--live-elements))))

(defun excali--boxes-overlap-p (a b)
  "Return non-nil if boxes A and B intersect."
  (and (<= (nth 0 a) (nth 2 b)) (<= (nth 0 b) (nth 2 a))
       (<= (nth 1 a) (nth 3 b)) (<= (nth 1 b) (nth 3 a))))

(defun excali--overlaps-frame-p (element frame)
  "Return non-nil if ELEMENT is inside, crosses or contains FRAME's box.
Boxes are compared, a simplification of `elementOverlapsWithFrame'."
  (excali--boxes-overlap-p (excali--elements-bounds (list element))
                          (excali--element-box frame)))

(defun excali-frame-name (frame)
  "Return the name shown for FRAME."
  (let ((name (excali--get frame 'name)))
    (if (and (stringp name) (not (string-empty-p name))) name
      (if (equal (excali--get frame 'type) "magicframe") "AI Frame"
        excali-default-frame-name))))

;;;; Membership

(defun excali--set-frame (elements frame)
  "Put ELEMENTS (and their labels) in FRAME, or out of any frame if nil."
  (dolist (e elements)
    (dolist (x (cons e (delq nil (list (and (fboundp 'excali--bound-text-of)
                                            (excali--bound-text-of e))))))
      (unless (equal (excali--get x 'frameId) (and frame (excali--get frame 'id)))
        (excali--put x 'frameId (if frame (excali--get frame 'id) :null))
        (excali--touch x)))))

(defun excali--place-below (elements frame)
  "Move ELEMENTS just below FRAME in z-order, keeping their order."
  (let* ((rest (seq-remove (lambda (e) (memq e elements)) excali--elements))
         (tail (memq frame rest)))
    (when tail
      (let ((before (seq-take rest (- (length rest) (length tail)))))
        (setq excali--elements (append before (seq-filter (lambda (e) (memq e elements))
                                                         excali--elements)
                                      tail))))))

(defun excali--elements-in-new-frame (frame)
  "Return the elements a new FRAME adopts (`getElementsInNewFrame').
Elements completely inside it that are not frames and not already in
another frame; groups only when all their members qualify."
  (let* ((box (excali--element-box frame))
         (candidates
          (seq-filter (lambda (e)
                        (and (not (eq e frame)) (not (excali--frame-p e))
                             (not (excali--bound-text-p e))
                             (let ((f (excali--get e 'frameId)))
                               (or (null f) (equal f (excali--get frame 'id))))
                             (excali--inside-p (excali--elements-bounds (list e)) box)))
                      (excali--live-elements))))
    (seq-filter (lambda (e)
                  (let ((group (car (last (append (excali--get e 'groupIds) nil)))))
                    (or (null group)
                        (seq-every-p (lambda (m) (memq m candidates))
                                     (excali--group-members group)))))
                candidates)))

(defun excali--adopt-into-frame (frame)
  "Give new FRAME the elements inside it and order them below it."
  (let ((children (excali--elements-in-new-frame frame)))
    (when children
      (excali--set-frame children frame)
      (excali--place-below children frame))
    children))

(defun excali--update-frame-membership (moved point)
  "Re-file MOVED elements after a drag released at scene POINT.
Dropping on a frame adds those overlapping it; elsewhere, elements that
no longer overlap their frame leave it."
  (let* ((movable (seq-remove #'excali--frame-p moved))
         (target (excali--frame-at point moved)))
    (if target
        (let ((joining (seq-filter (lambda (e) (excali--overlaps-frame-p e target)) movable)))
          (excali--set-frame joining target))
      (dolist (e movable)
        (when-let* ((frame (excali--live-element-by-id (excali--get e 'frameId))))
          (unless (excali--overlaps-frame-p e frame)
            (excali--set-frame (list e) nil)))))))

(defun excali--update-resized-frames (frames)
  "Refresh the children of FRAMES after resizing them.
Children no longer overlapping leave; ungrouped elements now completely
inside join (`getElementsInResizingFrame', simplified)."
  (dolist (frame frames)
    (dolist (child (excali--frame-children frame))
      (unless (or (excali--bound-text-p child) (excali--overlaps-frame-p child frame))
        (excali--set-frame (list child) nil)))
    (excali--adopt-into-frame frame)))

(defun excali--with-frame-children (elements)
  "Return ELEMENTS plus the children of the frames among them."
  (seq-union elements (apply #'append (mapcar #'excali--frame-children
                                              (seq-filter #'excali--frame-p elements)))))

(defun excali--drop-frame-children (elements)
  "Return ELEMENTS without children of frames also in ELEMENTS.
A frame and its children are never selected together."
  (let ((ids (delq nil (mapcar (lambda (e) (and (excali--frame-p e) (excali--get e 'id)))
                               elements))))
    (seq-remove (lambda (e) (member (excali--get e 'frameId) ids)) elements)))

(defun excali--release-frame-children (frames)
  "Release the children of FRAMES, which are being deleted; return them."
  (let ((children (apply #'append (mapcar #'excali--frame-children frames))))
    (excali--set-frame children nil)
    children))

;;;; Creation and naming

(defun excali--new-frame (x y)
  "Return a new frame element at X, Y."
  (apply #'excali--make-element "frame" x y (cons 'name :null) excali--frame-style))

(defun excali-rename-frame ()
  "Rename the selected frame."
  (interactive)
  (let ((frame (excali--single-selection)))
    (when (and frame (excali--frame-p frame))
      (let ((name (read-string "Frame name: " (excali-frame-name frame))))
        (excali--put frame 'name (if (string-empty-p name) :null name))
        (excali--touch frame)
        (excali--render)))))

(provide 'excali-frame)
;;; excali-frame.el ends here
