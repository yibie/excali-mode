;;; excal-erase.el --- Eraser and element links  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The eraser and element links (docs/excalidraw-spec.md §3b.10).
;;
;; Dragging the eraser marks what it touches: the whole outermost group,
;; and containers together with their labels, skipping locked elements.
;; Marked elements are drawn at 20% opacity and deleted on release; meta
;; at the press un-marks instead.
;;
;; A link is any URL stored in an element's `link'.  An element link is
;; a URL whose query holds `element=<id>' for an element or group in this
;; scene.  Linked elements show an icon at their top-right corner;
;; clicking the icon of an unselected element follows the link.

;;; Code:

(require 'url-parse)
(require 'excal-core)
(require 'excal-view)
(require 'excal-select)
(require 'excal-handles)
(require 'excal-hit)
(require 'excal-binding)

(declare-function excal--drag-loop "excal-edit")
(declare-function excal-delete-selected "excal-edit")
(declare-function excal-zoom-to-fit-selection-in-viewport "excal-actions")

(defconst excal--erase-opacity 20 "ELEMENT_READY_TO_ERASE_OPACITY.")

(defvar-local excal--erase-marked nil "Elements the eraser will delete.")

;;;; Eraser

(defun excal--erase-unit (element)
  "Return what erasing ELEMENT removes: its outermost group and labels."
  (let* ((element (or (and (excal--bound-text-p element) (excal--container-of element))
                      element))
         (group (car (last (append (excal--get element 'groupIds) nil))))
         (members (if group (excal--group-members group) (list element))))
    (seq-union members (excal--labels-of members))))

(defun excal--eraser-hits (point)
  "Return the unlocked elements the eraser touches at scene POINT."
  (seq-filter (lambda (e)
                (and (not (excal--get e 'locked))
                     (excal--hit-element-p
                      e point
                      (if (equal (excal--get e 'type) "freedraw")
                          (max 2.25 (/ 5.0 excal--zoom))
                        (max (/ (float (or (excal--get e 'strokeWidth) 1)) 2)
                             (/ 5.0 excal--zoom))))))
              (excal--live-elements)))

(defun excal--erase-drag (start restore)
  "Erase along a drag from scene point START; RESTORE un-marks instead."
  (let ((mark (lambda (point)
                (let ((changed nil))
                  (dolist (hit (excal--eraser-hits point))
                    (dolist (e (excal--erase-unit hit))
                      (if restore
                          (when (memq e excal--erase-marked)
                            (setq excal--erase-marked (delq e excal--erase-marked))
                            (push e changed))
                        (unless (memq e excal--erase-marked)
                          (push e excal--erase-marked)
                          (push e changed)))))
                  (mapc (lambda (e) (remhash e excal--native-cache)) changed)
                  (and changed (excal--elements-damage changed))))))
    (excal--render (funcall mark start))
    (excal--drag-loop
     (lambda (ev) (funcall mark (excal--event-scene-xy ev))))
    (when excal--erase-marked
      (let ((doomed excal--erase-marked))
        (setq excal--erase-marked nil)
        (excal--deselect)
        (excal--select doomed)
        (excal-delete-selected)))))

(defun excal--erase-opacity-for (element opacity)
  "Return the OPACITY to draw ELEMENT with while the eraser marks it."
  (if (memq element excal--erase-marked)
      (/ (* excal--erase-opacity (or opacity 100)) 100.0)
    opacity))

;;;; Links

(defun excal--element-link-target (url)
  "Return the element id in element link URL, or nil."
  (when (and (stringp url) (string-match "[?&]element=\\([^&#]+\\)" url))
    (url-unhex-string (match-string 1 url))))

(defun excal-set-link (url)
  "Give the selection the link URL; an empty URL removes the link.
With several elements selected they must share a group, as upstream."
  (interactive
   (list (read-string "Link (URL, or ?element=ID): "
                      (let ((e (car excal--selection)))
                        (and e (excal--get e 'link))))))
  (let ((targets (if (excal--single-selection)
                     excal--selection
                   (and (excal--unit-group (car excal--selection))
                        excal--selection))))
    (if (null targets)
        (user-error "Select one element or one group")
      (dolist (e targets)
        (excal--put e 'link (if (string-empty-p url) :null url))
        (excal--touch e))
      (excal--render))))

(defun excal-copy-element-link ()
  "Put a link to the selected element or group on the kill ring."
  (interactive)
  (let* ((single (excal--single-selection))
         (id (or (and single (excal--get single 'id))
                 (excal--unit-group (car excal--selection)))))
    (unless id (user-error "Select one element or one group"))
    (kill-new (format "?element=%s" (url-hexify-string id)))
    (message "Copied link to %s" id)))

(defun excal--link-icon-box (element)
  "Return the link icon box (X1 Y1 X2 Y2) at ELEMENT's top-right.
Upstream `getLinkHandleFromCoords', unrotated."
  (pcase-let* ((`(,_x1 ,y1 ,x2 ,_y2) (excal--element-box element))
               (z (max excal--zoom 1.0))
               (size (/ 12.0 z))
               (x (- (+ x2 (/ 4.0 excal--zoom)) (/ (- 12 8) (* 2.0 excal--zoom))))
               (y (+ (- y1 (/ 4.0 excal--zoom) (/ 12.0 excal--zoom))
                     (/ (- 12 8) (* 2.0 excal--zoom)))))
    (list x y (+ x size) (+ y size))))

(defun excal--link-at (point)
  "Return the element whose link icon lies under scene POINT, or nil."
  (let ((tolerance (/ 4.0 excal--zoom)))
    (cl-find-if (lambda (e)
                  (and (stringp (excal--get e 'link))
                       (not (excal--selected-p e))
                       (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excal--link-icon-box e)))
                         (and (<= (- x1 tolerance) (car point) (+ x2 tolerance))
                              (<= (- y1 tolerance) (cdr point) (+ y2 tolerance))))))
                (reverse (excal--live-elements)))))

(defun excal-follow-link (element)
  "Follow ELEMENT's link: select the linked element here, or browse it."
  (let* ((url (excal--get element 'link))
         (id (excal--element-link-target url)))
    (if id
        (let ((targets (or (and (excal--live-element-by-id id)
                                (list (excal--live-element-by-id id)))
                           (excal--group-members id))))
          (if (null targets)
              (message "No element %s in this scene" id)
            (excal--deselect)
            (excal--select targets)
            (excal-zoom-to-fit-selection-in-viewport)))
      (browse-url url))))

(defun excal--link-icon-overlays ()
  "Return link icons for linked elements: a box with an arrow."
  (let (overlays)
    (dolist (e (excal--live-elements) overlays)
      (when (stringp (excal--get e 'link))
        (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) (excal--link-icon-box e))
                     (w (- x2 x1)) (h (- y2 y1))
                     (arrow (excal--ov "ov-poly" (+ x1 (* 0.3 w)) (+ y1 (* 0.3 h)) 0 0
                                       :stroke (excal--selection-color) :width 1.5)))
          (aset arrow 12 (vector (* 0.4 w) 0.0 (* 0.4 w) (* 0.4 h)
                                 (* 0.4 w) 0.0 0.0 (* 0.4 h)))
          (push (excal--ov "ov-handle" x1 y1 w h :stroke (excal--selection-color)
                           :fill "#ffffff" :width 1)
                overlays)
          (push arrow overlays))))))

(provide 'excal-erase)
;;; excal-erase.el ends here
