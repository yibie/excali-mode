;;; excal-clipboard.el --- Copy, cut, paste and duplicate  -*- lexical-binding: t; -*-

;;; Commentary:

;; The clipboard carries Excalidraw's own format, a JSON object of type
;; "excalidraw/clipboard", as plain text, so scenes can be copied between
;; excal and excalidraw.com in both directions.  Pasted and duplicated
;; elements get fresh ids and seeds; references between them (groups,
;; containers, bound elements, arrow bindings) are remapped, and
;; references to elements outside the pasted set are dropped.

;;; Code:

(require 'excal-core)
(require 'excal-view)
(require 'excal-select)
(require 'excal-style)
(require 'excal-restore)
(require 'excal-index)
(require 'excal-image)

(defconst excal-clipboard-type "excalidraw/clipboard"
  "Value of the `type' field of Excalidraw clipboard data.")

(defcustom excal-duplicate-offset 10
  "Scene units by which `excal-duplicate' shifts the copies."
  :type 'number
  :group 'excal)

;;;; Serialization

(defun excal--clipboard-files (elements)
  "Return the document's files entries used by image ELEMENTS."
  (let ((ids (delq nil (mapcar (lambda (e) (and (equal (excal--get e 'type) "image")
                                                 (excal--get e 'fileId)))
                               elements))))
    (seq-filter (lambda (entry) (member (symbol-name (car entry)) ids))
                (let ((files (alist-get 'files excal--doc)))
                  (and (listp files) files)))))

(defun excal--clipboard-json (elements)
  "Return Excalidraw clipboard JSON for ELEMENTS, with their image files."
  (json-serialize (list (cons 'type excal-clipboard-type)
                        (cons 'elements (vconcat elements))
                        (cons 'files (excal--clipboard-files elements)))
                  :null-object :null :false-object :false))

(defun excal--parse-clipboard-files (text)
  "Return the files map in Excalidraw clipboard TEXT, an alist, or nil."
  (condition-case nil
      (let ((files (alist-get 'files (json-parse-string
                                      text :object-type 'alist :array-type 'array
                                      :null-object :null :false-object :false))))
        (and (listp files) files))
    (error nil)))

(defun excal--parse-clipboard (text)
  "Return the elements in Excalidraw clipboard TEXT, or nil if it is not one."
  (when (and (stringp text) (string-prefix-p "{" (string-trim-left text)))
    (condition-case nil
        (let ((data (json-parse-string text :object-type 'alist :array-type 'array
                                       :null-object :null :false-object :false)))
          (when (member (alist-get 'type data)
                        (list excal-clipboard-type "excalidraw"))
            (append (alist-get 'elements data) nil)))
      (json-parse-error nil))))

;;;; Cloning

(defun excal--remap-reference (value ids)
  "Return VALUE mapped through IDS, or :null if it is not in IDS."
  (or (and (stringp value) (gethash value ids)) :null))

(defun excal--clone-elements (elements)
  "Return fresh copies of ELEMENTS with new ids, seeds and remapped references."
  (let ((ids (make-hash-table :test #'equal))
        (groups (make-hash-table :test #'equal))
        (now (truncate (* 1000 (float-time)))))
    (dolist (e elements)
      (puthash (excal--get e 'id) (excal--new-id) ids))
    (mapcar
     (lambda (original)
       (let ((e (copy-tree original t)))
         (excal--put e 'id (gethash (excal--get original 'id) ids))
         (excal--put e 'seed (1+ (random (1- (ash 1 31)))))
         (excal--put e 'version 1)
         (excal--put e 'versionNonce (random (ash 1 31)))
         (excal--put e 'updated now)
         (excal--put e 'isDeleted :false)
         (when (assq 'index e) (excal--put e 'index :null))
         (excal--put e 'groupIds
                     (vconcat (mapcar (lambda (g)
                                        (or (gethash g groups)
                                            (puthash g (excal--new-id) groups)))
                                      (excal--get e 'groupIds))))
         (when (assq 'containerId e)
           (excal--put e 'containerId
                       (excal--remap-reference (excal--get e 'containerId) ids)))
         (when (assq 'boundElements e)
           (let ((bound (delq nil
                              (mapcar (lambda (b)
                                        (when-let* ((id (gethash (alist-get 'id b) ids)))
                                          (let ((b (copy-alist b)))
                                            (setf (alist-get 'id b) id)
                                            b)))
                                      (excal--get e 'boundElements)))))
             (excal--put e 'boundElements (if bound (vconcat bound) :null))))
         (dolist (key '(startBinding endBinding))
           (when-let* ((binding (excal--get e key)))
             (excal--put e key
                         (if-let* ((id (gethash (alist-get 'elementId binding) ids)))
                             (let ((binding (copy-alist binding)))
                               (setf (alist-get 'elementId binding) id)
                               binding)
                           :null))))
         (when-let* ((frame (excal--get e 'frameId)))
           (excal--put e 'frameId
                       (or (gethash frame ids)
                           (and (cl-find frame (excal--live-elements)
                                         :key (lambda (x) (excal--get x 'id))
                                         :test #'equal)
                                frame)
                           :null)))
         e))
     elements)))

(defun excal--translate (element dx dy)
  "Move ELEMENT by DX, DY scene units."
  (excal--put element 'x (float (+ (excal--get element 'x) dx)))
  (excal--put element 'y (float (+ (excal--get element 'y) dy)))
  (excal--touch element))

(defun excal--insert-elements (elements)
  "Add ELEMENTS on top of the scene and select them."
  (setq excal--elements (append excal--elements elements))
  (excal--sync-moved-indices elements)
  (excal--deselect)
  (excal--select elements))

;;;; Commands

(defun excal-copy ()
  "Copy the selected elements to the clipboard."
  (interactive)
  (if (null excal--selection)
      (message "Nothing selected")
    (kill-new (excal--clipboard-json excal--selection))
    (message "Copied %d element%s" (length excal--selection)
             (if (cdr excal--selection) "s" ""))))

(defun excal-cut ()
  "Copy the selected elements to the clipboard and delete them."
  (interactive)
  (when excal--selection
    (excal-copy)
    (dolist (e excal--selection)
      (excal--put e 'isDeleted t)
      (excal--touch e))
    (excal--deselect)
    (excal--render)))

(defun excal-paste ()
  "Paste clipboard elements, or clipboard text as a text element.
The pasted content is centered on the mouse, or on the view when the
mouse is not over the canvas."
  (interactive)
  (let* ((text (current-kill 0 t))
         ;; Clipboard data may come from any Excalidraw version.
         (parsed (excal--restore-elements (excal--parse-clipboard text)
                                          :existing excal--elements
                                          :delete-invisible t))
         (target (or (excal--mouse-scene-xy) (excal--view-center))))
    (cond
     (parsed
      ;; Images bring their data along.
      (when-let* ((files (excal--parse-clipboard-files text)))
        (excal--image-add-files files))
      (let* ((clones (excal--clone-elements parsed))
             (bounds (excal--elements-bounds clones))
             (dx (- (car target) (/ (+ (nth 0 bounds) (nth 2 bounds)) 2.0)))
             (dy (- (cdr target) (/ (+ (nth 1 bounds) (nth 3 bounds)) 2.0))))
        (dolist (e clones) (excal--translate e dx dy))
        (excal--insert-elements clones)))
     ((and (stringp text) (not (string-empty-p text)))
      (let ((element (excal--apply-current-style
                      (excal--make-text-element (car target) (cdr target) text))))
        (excal--insert-elements (list element))))
     (t (message "Clipboard is empty")))
    (excal--render)))

(defun excal-duplicate ()
  "Duplicate the selected elements, slightly offset."
  (interactive)
  (when excal--selection
    (let ((clones (excal--clone-elements excal--selection)))
      (dolist (e clones)
        (excal--translate e excal-duplicate-offset excal-duplicate-offset))
      (excal--insert-elements clones)
      (excal--render))))

(provide 'excal-clipboard)
;;; excal-clipboard.el ends here
