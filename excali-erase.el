;;; excali-erase.el --- Eraser and element links  -*- lexical-binding: t; -*-

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
(require 'excali-core)
(require 'excali-view)
(require 'excali-select)
(require 'excali-handles)
(require 'excali-hit)
(require 'excali-binding)

(declare-function excali--drag-loop "excali-edit")
(declare-function excali-delete-selected "excali-edit")
(declare-function excali-zoom-to-fit-selection-in-viewport "excali-actions")

(defconst excali--erase-opacity 20 "ELEMENT_READY_TO_ERASE_OPACITY.")

(defvar-local excali--erase-marked nil "Elements the eraser will delete.")

;;;; Eraser

(defun excali--erase-unit (element)
  "Return what erasing ELEMENT removes: its outermost group and labels."
  (let* ((element (or (and (excali--bound-text-p element) (excali--container-of element))
                      element))
         (group (car (last (append (excali--get element 'groupIds) nil))))
         (members (if group (excali--group-members group) (list element))))
    (seq-union members (excali--labels-of members))))

(defun excali--eraser-hits (point)
  "Return the unlocked elements the eraser touches at scene POINT."
  (seq-filter (lambda (e)
                (and (not (excali--get e 'locked))
                     (excali--hit-element-p
                      e point
                      (if (equal (excali--get e 'type) "freedraw")
                          (max 2.25 (/ 5.0 excali--zoom))
                        (max (/ (float (or (excali--get e 'strokeWidth) 1)) 2)
                             (/ 5.0 excali--zoom))))))
              (excali--live-elements)))

(defun excali--erase-drag (start restore)
  "Erase along a drag from scene point START; RESTORE un-marks instead."
  (let ((mark (lambda (point)
                (let ((changed nil))
                  (dolist (hit (excali--eraser-hits point))
                    (dolist (e (excali--erase-unit hit))
                      (if restore
                          (when (memq e excali--erase-marked)
                            (setq excali--erase-marked (delq e excali--erase-marked))
                            (push e changed))
                        (unless (memq e excali--erase-marked)
                          (push e excali--erase-marked)
                          (push e changed)))))
                  (mapc (lambda (e) (remhash e excali--native-cache)) changed)
                  (and changed (excali--elements-damage changed))))))
    (excali--render (funcall mark start))
    (excali--drag-loop
     (lambda (ev) (funcall mark (excali--event-scene-xy ev))))
    (when excali--erase-marked
      (let ((doomed excali--erase-marked))
        (setq excali--erase-marked nil)
        (excali--deselect)
        (excali--select doomed)
        (excali-delete-selected)))))

(defun excali--erase-opacity-for (element opacity)
  "Return the OPACITY to draw ELEMENT with while the eraser marks it."
  (if (memq element excali--erase-marked)
      (/ (* excali--erase-opacity (or opacity 100)) 100.0)
    opacity))

;;;; Links

(defun excali--element-link-target (url)
  "Return the element id in element link URL, or nil."
  (when (and (stringp url) (string-match "[?&]element=\\([^&#]+\\)" url))
    (url-unhex-string (match-string 1 url))))

(defun excali-set-link (url)
  "Give the selection the link URL; an empty URL removes the link.
With several elements selected they must share a group, as upstream."
  (interactive
   (list (read-string "Link (URL, or ?element=ID): "
                      (let ((e (car excali--selection)))
                        (and e (excali--get e 'link))))))
  (let ((targets (if (excali--single-selection)
                     excali--selection
                   (and (excali--unit-group (car excali--selection))
                        excali--selection))))
    (if (null targets)
        (user-error "Select one element or one group")
      (dolist (e targets)
        (excali--put e 'link (if (string-empty-p url) :null url))
        (excali--touch e))
      (excali--render))))

(defun excali-copy-element-link ()
  "Put a link to the selected element or group on the kill ring."
  (interactive)
  (let* ((single (excali--single-selection))
         (id (or (and single (excali--get single 'id))
                 (excali--unit-group (car excali--selection)))))
    (unless id (user-error "Select one element or one group"))
    (kill-new (format "?element=%s" (url-hexify-string id)))
    (message "Copied link to %s" id)))

(defun excali--link-icon-box (element)
  "Return the link icon box (X1 Y1 X2 Y2) at ELEMENT's top-right.
Upstream `getLinkHandleFromCoords', unrotated."
  (pcase-let* ((`(,_x1 ,y1 ,x2 ,_y2) (excali--element-box element))
               (z (max excali--zoom 1.0))
               (size (/ 12.0 z))
               (x (- (+ x2 (/ 4.0 excali--zoom)) (/ (- 12 8) (* 2.0 excali--zoom))))
               (y (+ (- y1 (/ 4.0 excali--zoom) (/ 12.0 excali--zoom))
                     (/ (- 12 8) (* 2.0 excali--zoom)))))
    (list x y (+ x size) (+ y size))))

(defun excali--link-at (point)
  "Return the element whose link icon lies under scene POINT, or nil."
  (let ((tolerance (/ 4.0 excali--zoom)))
    (cl-find-if (lambda (e)
                  (and (stringp (excali--get e 'link))
                       (not (excali--selected-p e))
                       (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excali--link-icon-box e)))
                         (and (<= (- x1 tolerance) (car point) (+ x2 tolerance))
                              (<= (- y1 tolerance) (cdr point) (+ y2 tolerance))))))
                (reverse (excali--live-elements)))))

(defun excali-follow-link (element)
  "Follow ELEMENT's link: select the linked element here, or browse it."
  (let* ((url (excali--get element 'link))
         (id (excali--element-link-target url)))
    (if id
        (let ((targets (or (and (excali--live-element-by-id id)
                                (list (excali--live-element-by-id id)))
                           (excali--group-members id))))
          (if (null targets)
              (message "No element %s in this scene" id)
            (excali--deselect)
            (excali--select targets)
            (excali-zoom-to-fit-selection-in-viewport)))
      (browse-url url))))

(defun excali--link-icon-overlays ()
  "Return link icons for linked elements: a box with an arrow."
  (let (overlays)
    (dolist (e (excali--live-elements) overlays)
      (when (stringp (excali--get e 'link))
        (pcase-let* ((`(,x1 ,y1 ,x2 ,y2) (excali--link-icon-box e))
                     (w (- x2 x1)) (h (- y2 y1))
                     (arrow (excali--ov "ov-poly" (+ x1 (* 0.3 w)) (+ y1 (* 0.3 h)) 0 0
                                       :stroke (excali--selection-color) :width 1.5)))
          (aset arrow 12 (vector (* 0.4 w) 0.0 (* 0.4 w) (* 0.4 h)
                                 (* 0.4 w) 0.0 0.0 (* 0.4 h)))
          (push (excali--ov "ov-handle" x1 y1 w h :stroke (excali--selection-color)
                           :fill "#ffffff" :width 1)
                overlays)
          (push arrow overlays))))))

(provide 'excali-erase)
;;; excali-erase.el ends here
