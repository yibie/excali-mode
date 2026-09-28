;;; excali-clipboard.el --- Copy, cut, paste and duplicate  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The clipboard carries Excalidraw's own format, a JSON object of type
;; "excalidraw/clipboard", as plain text, so scenes can be copied between
;; excali and excalidraw.com in both directions.  Pasted and duplicated
;; elements get fresh ids and seeds; references between them (groups,
;; containers, bound elements, arrow bindings) are remapped, and
;; references to elements outside the pasted set are dropped.

;;; Code:

(require 'excali-core)
(require 'excali-view)
(require 'excali-select)
(require 'excali-style)
(require 'excali-restore)
(require 'excali-index)
(require 'excali-image)

(declare-function excali--elbow-p "excali-elbow")
(declare-function excali--update-arrow "excali-binding")

(defconst excali-clipboard-type "excalidraw/clipboard"
  "Value of the `type' field of Excalidraw clipboard data.")

(defcustom excali-duplicate-offset 10
  "Scene units by which `excali-duplicate' shifts the copies."
  :type 'number
  :group 'excali)

;;;; Serialization

(defun excali--clipboard-files (elements)
  "Return the document's files entries used by image ELEMENTS."
  (let ((ids (delq nil (mapcar (lambda (e) (and (equal (excali--get e 'type) "image")
                                                 (excali--get e 'fileId)))
                               elements))))
    (seq-filter (lambda (entry) (member (symbol-name (car entry)) ids))
                (let ((files (alist-get 'files excali--doc)))
                  (and (listp files) files)))))

(defun excali--clipboard-json (elements)
  "Return Excalidraw clipboard JSON for ELEMENTS, with their image files."
  (json-serialize (list (cons 'type excali-clipboard-type)
                        (cons 'elements (vconcat elements))
                        (cons 'files (excali--clipboard-files elements)))
                  :null-object :null :false-object :false))

(defun excali--parse-clipboard-files (text)
  "Return the files map in Excalidraw clipboard TEXT, an alist, or nil."
  (condition-case nil
      (let ((files (alist-get 'files (json-parse-string
                                      text :object-type 'alist :array-type 'array
                                      :null-object :null :false-object :false))))
        (and (listp files) files))
    (error nil)))

(defun excali--parse-clipboard (text)
  "Return the elements in Excalidraw clipboard TEXT, or nil if it is not one."
  (when (and (stringp text) (string-prefix-p "{" (string-trim-left text)))
    (condition-case nil
        (let ((data (json-parse-string text :object-type 'alist :array-type 'array
                                       :null-object :null :false-object :false)))
          (when (member (alist-get 'type data)
                        (list excali-clipboard-type "excalidraw"))
            (append (alist-get 'elements data) nil)))
      (json-parse-error nil))))

;;;; Cloning

(defun excali--remap-reference (value ids)
  "Return VALUE mapped through IDS, or :null if it is not in IDS."
  (or (and (stringp value) (gethash value ids)) :null))

(defun excali--clone-elements (elements)
  "Return fresh copies of ELEMENTS with new ids, seeds and remapped references."
  (let ((ids (make-hash-table :test #'equal))
        (groups (make-hash-table :test #'equal))
        (now (truncate (* 1000 (float-time)))))
    (dolist (e elements)
      (puthash (excali--get e 'id) (excali--new-id) ids))
    (mapcar
     (lambda (original)
       (let ((e (copy-tree original t)))
         (excali--put e 'id (gethash (excali--get original 'id) ids))
         (excali--put e 'seed (1+ (random (1- (ash 1 31)))))
         (excali--put e 'version 1)
         (excali--put e 'versionNonce (random (ash 1 31)))
         (excali--put e 'updated now)
         (excali--put e 'isDeleted :false)
         (when (assq 'index e) (excali--put e 'index :null))
         (excali--put e 'groupIds
                     (vconcat (mapcar (lambda (g)
                                        (or (gethash g groups)
                                            (puthash g (excali--new-id) groups)))
                                      (excali--get e 'groupIds))))
         (when (assq 'containerId e)
           (excali--put e 'containerId
                       (excali--remap-reference (excali--get e 'containerId) ids)))
         (when (assq 'boundElements e)
           (let ((bound (delq nil
                              (mapcar (lambda (b)
                                        (when-let* ((id (gethash (alist-get 'id b) ids)))
                                          (let ((b (copy-alist b)))
                                            (setf (alist-get 'id b) id)
                                            b)))
                                      (excali--get e 'boundElements)))))
             (excali--put e 'boundElements (if bound (vconcat bound) :null))))
         (dolist (key '(startBinding endBinding))
           (when-let* ((binding (excali--get e key)))
             (excali--put e key
                         (if-let* ((id (gethash (alist-get 'elementId binding) ids)))
                             (let ((binding (copy-alist binding)))
                               (setf (alist-get 'elementId binding) id)
                               binding)
                           :null))))
         (when-let* ((frame (excali--get e 'frameId)))
           (excali--put e 'frameId
                       (or (gethash frame ids)
                           (and (cl-find frame (excali--live-elements)
                                         :key (lambda (x) (excali--get x 'id))
                                         :test #'equal)
                                frame)
                           :null)))
         e))
     elements)))

(defun excali--translate (element dx dy)
  "Move ELEMENT by DX, DY scene units."
  (excali--put element 'x (float (+ (excali--get element 'x) dx)))
  (excali--put element 'y (float (+ (excali--get element 'y) dy)))
  (excali--touch element))

(defun excali--insert-elements (elements)
  "Add ELEMENTS on top of the scene and select them."
  (setq excali--elements (append excali--elements elements))
  (excali--sync-moved-indices elements)
  ;; Pasted elbow arrows route again against the pasted shapes.
  (dolist (e elements)
    (when (and (fboundp 'excali--elbow-p) (excali--elbow-p e))
      (excali--update-arrow e)))
  (excali--deselect)
  (excali--select elements))

;;;; Commands

(defun excali-copy ()
  "Copy the selected elements to the clipboard."
  (interactive)
  (if (null excali--selection)
      (message "Nothing selected")
    (kill-new (excali--clipboard-json excali--selection))
    (message "Copied %d element%s" (length excali--selection)
             (if (cdr excali--selection) "s" ""))))

(defun excali-cut ()
  "Copy the selected elements to the clipboard and delete them."
  (interactive)
  (when excali--selection
    (excali-copy)
    (dolist (e excali--selection)
      (excali--put e 'isDeleted t)
      (excali--touch e))
    (excali--deselect)
    (excali--render)))

(defun excali-paste ()
  "Paste clipboard elements, or clipboard text as a text element.
The pasted content is centered on the mouse, or on the view when the
mouse is not over the canvas."
  (interactive)
  (let* ((text (current-kill 0 t))
         ;; Clipboard data may come from any Excalidraw version.
         (parsed (excali--restore-elements (excali--parse-clipboard text)
                                          :existing excali--elements
                                          :delete-invisible t))
         (target (or (excali--mouse-scene-xy) (excali--view-center))))
    (cond
     (parsed
      ;; Images bring their data along.
      (when-let* ((files (excali--parse-clipboard-files text)))
        (excali--image-add-files files))
      (let* ((clones (excali--clone-elements parsed))
             (bounds (excali--elements-bounds clones))
             (dx (- (car target) (/ (+ (nth 0 bounds) (nth 2 bounds)) 2.0)))
             (dy (- (cdr target) (/ (+ (nth 1 bounds) (nth 3 bounds)) 2.0))))
        (dolist (e clones) (excali--translate e dx dy))
        (excali--insert-elements clones)))
     ((and (stringp text) (not (string-empty-p text)))
      (let ((element (excali--apply-current-style
                      (excali--make-text-element (car target) (cdr target) text))))
        (excali--insert-elements (list element))))
     (t (message "Clipboard is empty")))
    (excali--render)))

(defun excali-duplicate ()
  "Duplicate the selected elements, slightly offset."
  (interactive)
  (when excali--selection
    (let ((clones (excali--clone-elements excali--selection)))
      (dolist (e clones)
        (excali--translate e excali-duplicate-offset excali-duplicate-offset))
      (excali--insert-elements clones)
      (excali--render))))

(provide 'excali-clipboard)
;;; excali-clipboard.el ends here
