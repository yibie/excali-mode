;;; excali-library.el --- The element library  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A personal element library in Excalidraw's `.excalidrawlib' format
;; (docs/excalidraw-spec.md §1b.5), so libraries move freely between
;; excali and excalidraw.com:
;;
;; - `excali-library-add' stores the selection as a new item;
;; - `excali-library-insert' places items, laid out on a square grid like
;;   `distributeLibraryItemsOnSquareGrid', centered on the view, with new
;;   ids and seeds;
;; - `excali-library-import' merges another library (v1 or v2), skipping
;;   items already present; `excali-library-export' writes one;
;; - `excali-library-browse' shows thumbnails; RET or a click inserts.

;;; Code:

(require 'excali-core)
(require 'excali-view)
(require 'excali-select)
(require 'excali-restore)
(require 'excali-clipboard)

(declare-function excali-native-fb-create "excali-module")
(declare-function excali-native-fb-render "excali-module")
(declare-function excali-native-fb-write-png "excali-module")

(defcustom excali-library-file
  (expand-file-name "excali/library.excalidrawlib" user-emacs-directory)
  "File holding the personal element library."
  :type 'file
  :group 'excali)

(defconst excali--library-padding 50 "Grid gap between inserted items.")
(defconst excali--thumbnail-size 96 "Largest thumbnail edge in pixels.")

(defvar excali--library nil "Loaded library items, newest first, or :unloaded.")
(defvar excali--library-loaded nil "Non-nil once `excali-library-file' was read.")

;;;; Items

(defun excali--library-item-label (item index)
  "Return a completion label for library ITEM at INDEX."
  (let ((name (alist-get 'name item))
        (count (length (alist-get 'elements item))))
    (format "%d. %s (%d element%s)" (1+ index)
            (if (and (stringp name) (not (string-empty-p name))) name "untitled")
            count (if (= count 1) "" "s"))))

(defun excali--restore-library-items (items)
  "Restore ITEMS parsed from a library file (`restoreLibraryItems').
Bare element arrays (version 1) become items; items left without live
elements are dropped."
  (delq nil
        (mapcar
         (lambda (item)
           (let* ((bare (vectorp item))
                  (elements (if bare item (alist-get 'elements item)))
                  (restored (seq-remove
                             (lambda (e) (excali--get e 'isDeleted))
                             (excali--restore-elements (append elements nil)))))
             (when restored
               (let ((result (if bare nil (copy-alist item))))
                 (setf (alist-get 'id result)
                       (let ((id (alist-get 'id result)))
                         (if (stringp id) id (excali--new-id))))
                 (setf (alist-get 'status result)
                       (let ((s (alist-get 'status result)))
                         (if (member s '("published" "unpublished")) s "unpublished")))
                 (setf (alist-get 'created result)
                       (let ((c (alist-get 'created result)))
                         (if (numberp c) c (truncate (* 1000 (float-time))))))
                 (setf (alist-get 'elements result) (vconcat restored))
                 result))))
         items)))

(defun excali--parse-library (text)
  "Return the library items in TEXT, or signal an error if it is not one."
  (let ((data (json-parse-string text :object-type 'alist :array-type 'array
                                 :null-object :null :false-object :false)))
    (unless (and (equal (alist-get 'type data) "excalidrawlib")
                 (memq (alist-get 'version data) '(1 2)))
      (user-error "Not an Excalidraw library"))
    (excali--restore-library-items
     (append (or (alist-get 'libraryItems data) (alist-get 'library data)) nil))))

(defun excali--library-same-item-p (a b)
  "Return non-nil if items A and B hold the same element versions."
  (let ((ea (alist-get 'elements a)) (eb (alist-get 'elements b)))
    (and (= (length ea) (length eb))
         (cl-every (lambda (x y)
                     (and (equal (alist-get 'id x) (alist-get 'id y))
                          (equal (alist-get 'versionNonce x) (alist-get 'versionNonce y))))
                   ea eb))))

(defun excali--merge-library (local incoming)
  "Return INCOMING items not already in LOCAL, followed by LOCAL."
  (append (seq-remove (lambda (item)
                        (seq-some (lambda (l) (excali--library-same-item-p l item)) local))
                      incoming)
          local))

(defun excali--serialize-library (items)
  "Return `.excalidrawlib' version 2 text for ITEMS."
  (excali--json-encode (list (cons 'type "excalidrawlib")
                            (cons 'version 2)
                            (cons 'source "https://excalidraw.com")
                            (cons 'libraryItems (vconcat items)))))

;;;; Storage

(defun excali--library ()
  "Return the library items, reading `excali-library-file' once."
  (unless excali--library-loaded
    (setq excali--library
          (and (file-readable-p excali-library-file)
               (with-temp-buffer
                 (insert-file-contents excali-library-file)
                 (excali--parse-library (buffer-string))))
          excali--library-loaded t))
  excali--library)

(defun excali--save-library ()
  "Write the library to `excali-library-file'."
  (make-directory (file-name-directory excali-library-file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (excali--serialize-library (excali--library)) nil
                  excali-library-file nil 'silent)))

;;;; Commands

(defun excali-library-add (name)
  "Add the selection to the library as an item called NAME."
  (interactive (list (read-string "Library item name (optional): ")))
  (unless excali--selection (user-error "Nothing selected"))
  (let* ((elements (seq-union excali--selection
                              (and (fboundp 'excali--labels-of)
                                   (excali--labels-of excali--selection))))
         (item (list (cons 'id (excali--new-id))
                     (cons 'status "unpublished")
                     (cons 'elements (vconcat (mapcar (lambda (e) (copy-tree e t))
                                                      (excali--live-elements-in elements))))
                     (cons 'created (truncate (* 1000 (float-time)))))))
    (unless (string-empty-p name)
      (setf (alist-get 'name item) name))
    (setq excali--library (cons item (excali--library)))
    (excali--save-library)
    (message "Added to the library (%d items)" (length excali--library))))

(defun excali--live-elements-in (elements)
  "Return ELEMENTS in scene order."
  (seq-filter (lambda (e) (memq e elements)) (excali--live-elements)))

(defun excali--grid-layout (items)
  "Return ITEMS' element lists shifted onto a square grid around 0,0.
Each item is centered in its cell; cells are as wide as the widest item
in their column and as tall as the tallest in their row."
  (let* ((n (length items))
         (per-row (max 1 (ceiling (sqrt n))))
         (groups (mapcar (lambda (item)
                           (excali--restore-elements
                            (append (alist-get 'elements item) nil)))
                         items))
         (bounds (mapcar #'excali--elements-bounds groups))
         (col-w (make-vector per-row 0.0))
         (row-h (make-vector (ceiling n (float per-row)) 0.0)))
    (cl-loop for b in bounds for i from 0
             do (let ((col (% i per-row)) (row (/ i per-row)))
                  (aset col-w col (max (aref col-w col) (- (nth 2 b) (nth 0 b))))
                  (aset row-h row (max (aref row-h row) (- (nth 3 b) (nth 1 b))))))
    (cl-loop for group in groups for b in bounds for i from 0
             append (let* ((col (% i per-row)) (row (/ i per-row))
                           (cx (+ (cl-loop for c below col sum (+ (aref col-w c) excali--library-padding))
                                  (/ (aref col-w col) 2.0)))
                           (cy (+ (cl-loop for r below row sum (+ (aref row-h r) excali--library-padding))
                                  (/ (aref row-h row) 2.0)))
                           (dx (- cx (/ (+ (nth 0 b) (nth 2 b)) 2.0)))
                           (dy (- cy (/ (+ (nth 1 b) (nth 3 b)) 2.0))))
                      (dolist (e group) (excali--translate e dx dy))
                      group))))

(defun excali--insert-library-items (items &optional target)
  "Insert library ITEMS centered on TARGET, the mouse or the view center."
  (let* ((elements (excali--clone-elements (excali--grid-layout items)))
         (target (or target (excali--mouse-scene-xy) (excali--view-center)))
         (b (excali--elements-bounds elements))
         (dx (- (car target) (/ (+ (nth 0 b) (nth 2 b)) 2.0)))
         (dy (- (cdr target) (/ (+ (nth 1 b) (nth 3 b)) 2.0))))
    (dolist (e elements) (excali--translate e dx dy))
    (excali--insert-elements elements)
    (excali--render)))

(defun excali-library-insert (labels)
  "Insert the library items chosen by LABELS into the scene."
  (interactive
   (let ((items (excali--library)))
     (unless items (user-error "The library is empty"))
     (list (completing-read-multiple
            "Insert library items: "
            (cl-loop for item in items for i from 0
                     collect (excali--library-item-label item i))
            nil t))))
  (let* ((items (excali--library))
         (chosen (cl-loop for item in items for i from 0
                          when (member (excali--library-item-label item i) labels)
                          collect item)))
    (when chosen
      (excali--insert-library-items chosen))))

(defun excali-library-import (file)
  "Merge the library in FILE into the personal library."
  (interactive "fImport library: ")
  (let* ((incoming (with-temp-buffer
                     (insert-file-contents file)
                     (excali--parse-library (buffer-string))))
         (before (length (excali--library))))
    (setq excali--library (excali--merge-library (excali--library) incoming))
    (excali--save-library)
    (message "Imported %d new item%s" (- (length excali--library) before)
             (if (= (- (length excali--library) before) 1) "" "s"))))

(defun excali-library-export (file)
  "Write the personal library to FILE."
  (interactive "FExport library to: ")
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (excali--serialize-library (excali--library)) nil file)))

(defun excali-library-remove (label)
  "Remove the library item shown as LABEL."
  (interactive
   (let ((items (excali--library)))
     (unless items (user-error "The library is empty"))
     (list (completing-read "Remove library item: "
                            (cl-loop for item in items for i from 0
                                     collect (excali--library-item-label item i))
                            nil t))))
  (setq excali--library
        (cl-loop for item in (excali--library) for i from 0
                 unless (equal (excali--library-item-label item i) label)
                 collect item))
  (excali--save-library))

;;;; Browsing

(defvar-local excali--library-origin nil "The excali buffer the browser inserts into.")

(defun excali--library-thumbnail (item)
  "Return an image of library ITEM, at most `excali--thumbnail-size' wide."
  (let* ((elements (excali--restore-elements (append (alist-get 'elements item) nil)))
         (b (excali--elements-bounds elements))
         (pad 8.0)
         (w (+ (- (nth 2 b) (nth 0 b)) (* 2 pad)))
         (h (+ (- (nth 3 b) (nth 1 b)) (* 2 pad)))
         (scale (min 1.0 (/ excali--thumbnail-size (max w h))))
         (fb (excali-native-fb-create (max 1 (round (* w scale))) (max 1 (round (* h scale)))))
         (file (make-temp-file "excali-lib" nil ".png")))
    (with-temp-buffer
      (setq excali--native-cache (make-hash-table :test #'eq))
      (setq excali--elements elements)
      (excali-native-fb-render fb scale 1.0 (- pad (nth 0 b)) (- pad (nth 1 b))
                              (vconcat (mapcar #'excali--native-element elements)) nil))
    (excali-native-fb-write-png fb file)
    (prog1 (create-image (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert-file-contents-literally file)
                           (buffer-string))
                         'png t)
      (delete-file file))))

(defun excali-library-browse ()
  "Show the library as thumbnails; RET or a click inserts an item."
  (interactive)
  (let ((origin (current-buffer))
        (items (excali--library)))
    (with-current-buffer (get-buffer-create "*excali library*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (excali-library-mode)
        (setq excali--library-origin origin)
        (if (null items)
            (insert "The library is empty.  Select elements and run `excali-library-add'.\n")
          (cl-loop for item in items for i from 0
                   do (let ((start (point)))
                        (insert-image (excali--library-thumbnail item))
                        (insert " " (excali--library-item-label item i) "\n")
                        (put-text-property start (point) 'excali-library-item item)
                        (put-text-property start (point) 'mouse-face 'highlight))))
        (goto-char (point-min)))
      (pop-to-buffer (current-buffer)))))

(defun excali-library-insert-at-point (&optional event)
  "Insert the library item at point, or at the click EVENT."
  (interactive (list last-nonmenu-event))
  (let* ((pos (if (mouse-event-p event) (posn-point (event-start event)) (point)))
         (item (get-text-property pos 'excali-library-item))
         (origin excali--library-origin))
    (when (and item (buffer-live-p origin))
      (with-current-buffer origin
        (excali--insert-library-items (list item) (excali--view-center))))))

(defvar-keymap excali-library-mode-map
  "RET" #'excali-library-insert-at-point
  "<mouse-1>" #'excali-library-insert-at-point)

(define-derived-mode excali-library-mode special-mode "Excali-Library"
  "Browse the excali element library.")

(provide 'excali-library)
;;; excali-library.el ends here
