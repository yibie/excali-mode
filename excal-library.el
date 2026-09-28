;;; excal-library.el --- The element library  -*- lexical-binding: t; -*-

;;; Commentary:

;; A personal element library in Excalidraw's `.excalidrawlib' format
;; (docs/excalidraw-spec.md §1b.5), so libraries move freely between
;; excal and excalidraw.com:
;;
;; - `excal-library-add' stores the selection as a new item;
;; - `excal-library-insert' places items, laid out on a square grid like
;;   `distributeLibraryItemsOnSquareGrid', centered on the view, with new
;;   ids and seeds;
;; - `excal-library-import' merges another library (v1 or v2), skipping
;;   items already present; `excal-library-export' writes one;
;; - `excal-library-browse' shows thumbnails; RET or a click inserts.

;;; Code:

(require 'excal-core)
(require 'excal-view)
(require 'excal-select)
(require 'excal-restore)
(require 'excal-clipboard)

(declare-function excal-native-fb-create "excal-module")
(declare-function excal-native-fb-render "excal-module")
(declare-function excal-native-fb-write-png "excal-module")

(defcustom excal-library-file
  (expand-file-name "excal/library.excalidrawlib" user-emacs-directory)
  "File holding the personal element library."
  :type 'file
  :group 'excal)

(defconst excal--library-padding 50 "Grid gap between inserted items.")
(defconst excal--thumbnail-size 96 "Largest thumbnail edge in pixels.")

(defvar excal--library nil "Loaded library items, newest first, or :unloaded.")
(defvar excal--library-loaded nil "Non-nil once `excal-library-file' was read.")

;;;; Items

(defun excal--library-item-label (item index)
  "Return a completion label for library ITEM at INDEX."
  (let ((name (alist-get 'name item))
        (count (length (alist-get 'elements item))))
    (format "%d. %s (%d element%s)" (1+ index)
            (if (and (stringp name) (not (string-empty-p name))) name "untitled")
            count (if (= count 1) "" "s"))))

(defun excal--restore-library-items (items)
  "Restore ITEMS parsed from a library file (`restoreLibraryItems').
Bare element arrays (version 1) become items; items left without live
elements are dropped."
  (delq nil
        (mapcar
         (lambda (item)
           (let* ((bare (vectorp item))
                  (elements (if bare item (alist-get 'elements item)))
                  (restored (seq-remove
                             (lambda (e) (excal--get e 'isDeleted))
                             (excal--restore-elements (append elements nil)))))
             (when restored
               (let ((result (if bare nil (copy-alist item))))
                 (setf (alist-get 'id result)
                       (let ((id (alist-get 'id result)))
                         (if (stringp id) id (excal--new-id))))
                 (setf (alist-get 'status result)
                       (let ((s (alist-get 'status result)))
                         (if (member s '("published" "unpublished")) s "unpublished")))
                 (setf (alist-get 'created result)
                       (let ((c (alist-get 'created result)))
                         (if (numberp c) c (truncate (* 1000 (float-time))))))
                 (setf (alist-get 'elements result) (vconcat restored))
                 result))))
         items)))

(defun excal--parse-library (text)
  "Return the library items in TEXT, or signal an error if it is not one."
  (let ((data (json-parse-string text :object-type 'alist :array-type 'array
                                 :null-object :null :false-object :false)))
    (unless (and (equal (alist-get 'type data) "excalidrawlib")
                 (memq (alist-get 'version data) '(1 2)))
      (user-error "Not an Excalidraw library"))
    (excal--restore-library-items
     (append (or (alist-get 'libraryItems data) (alist-get 'library data)) nil))))

(defun excal--library-same-item-p (a b)
  "Return non-nil if items A and B hold the same element versions."
  (let ((ea (alist-get 'elements a)) (eb (alist-get 'elements b)))
    (and (= (length ea) (length eb))
         (cl-every (lambda (x y)
                     (and (equal (alist-get 'id x) (alist-get 'id y))
                          (equal (alist-get 'versionNonce x) (alist-get 'versionNonce y))))
                   ea eb))))

(defun excal--merge-library (local incoming)
  "Return INCOMING items not already in LOCAL, followed by LOCAL."
  (append (seq-remove (lambda (item)
                        (seq-some (lambda (l) (excal--library-same-item-p l item)) local))
                      incoming)
          local))

(defun excal--serialize-library (items)
  "Return `.excalidrawlib' version 2 text for ITEMS."
  (excal--json-encode (list (cons 'type "excalidrawlib")
                            (cons 'version 2)
                            (cons 'source "https://excalidraw.com")
                            (cons 'libraryItems (vconcat items)))))

;;;; Storage

(defun excal--library ()
  "Return the library items, reading `excal-library-file' once."
  (unless excal--library-loaded
    (setq excal--library
          (and (file-readable-p excal-library-file)
               (with-temp-buffer
                 (insert-file-contents excal-library-file)
                 (excal--parse-library (buffer-string))))
          excal--library-loaded t))
  excal--library)

(defun excal--save-library ()
  "Write the library to `excal-library-file'."
  (make-directory (file-name-directory excal-library-file) t)
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (excal--serialize-library (excal--library)) nil
                  excal-library-file nil 'silent)))

;;;; Commands

(defun excal-library-add (name)
  "Add the selection to the library as an item called NAME."
  (interactive (list (read-string "Library item name (optional): ")))
  (unless excal--selection (user-error "Nothing selected"))
  (let* ((elements (seq-union excal--selection
                              (and (fboundp 'excal--labels-of)
                                   (excal--labels-of excal--selection))))
         (item (list (cons 'id (excal--new-id))
                     (cons 'status "unpublished")
                     (cons 'elements (vconcat (mapcar (lambda (e) (copy-tree e t))
                                                      (excal--live-elements-in elements))))
                     (cons 'created (truncate (* 1000 (float-time)))))))
    (unless (string-empty-p name)
      (setf (alist-get 'name item) name))
    (setq excal--library (cons item (excal--library)))
    (excal--save-library)
    (message "Added to the library (%d items)" (length excal--library))))

(defun excal--live-elements-in (elements)
  "Return ELEMENTS in scene order."
  (seq-filter (lambda (e) (memq e elements)) (excal--live-elements)))

(defun excal--grid-layout (items)
  "Return ITEMS' element lists shifted onto a square grid around 0,0.
Each item is centered in its cell; cells are as wide as the widest item
in their column and as tall as the tallest in their row."
  (let* ((n (length items))
         (per-row (max 1 (ceiling (sqrt n))))
         (groups (mapcar (lambda (item)
                           (excal--restore-elements
                            (append (alist-get 'elements item) nil)))
                         items))
         (bounds (mapcar #'excal--elements-bounds groups))
         (col-w (make-vector per-row 0.0))
         (row-h (make-vector (ceiling n (float per-row)) 0.0)))
    (cl-loop for b in bounds for i from 0
             do (let ((col (% i per-row)) (row (/ i per-row)))
                  (aset col-w col (max (aref col-w col) (- (nth 2 b) (nth 0 b))))
                  (aset row-h row (max (aref row-h row) (- (nth 3 b) (nth 1 b))))))
    (cl-loop for group in groups for b in bounds for i from 0
             append (let* ((col (% i per-row)) (row (/ i per-row))
                           (cx (+ (cl-loop for c below col sum (+ (aref col-w c) excal--library-padding))
                                  (/ (aref col-w col) 2.0)))
                           (cy (+ (cl-loop for r below row sum (+ (aref row-h r) excal--library-padding))
                                  (/ (aref row-h row) 2.0)))
                           (dx (- cx (/ (+ (nth 0 b) (nth 2 b)) 2.0)))
                           (dy (- cy (/ (+ (nth 1 b) (nth 3 b)) 2.0))))
                      (dolist (e group) (excal--translate e dx dy))
                      group))))

(defun excal--insert-library-items (items &optional target)
  "Insert library ITEMS centered on TARGET, the mouse or the view center."
  (let* ((elements (excal--clone-elements (excal--grid-layout items)))
         (target (or target (excal--mouse-scene-xy) (excal--view-center)))
         (b (excal--elements-bounds elements))
         (dx (- (car target) (/ (+ (nth 0 b) (nth 2 b)) 2.0)))
         (dy (- (cdr target) (/ (+ (nth 1 b) (nth 3 b)) 2.0))))
    (dolist (e elements) (excal--translate e dx dy))
    (excal--insert-elements elements)
    (excal--render)))

(defun excal-library-insert (labels)
  "Insert the library items chosen by LABELS into the scene."
  (interactive
   (let ((items (excal--library)))
     (unless items (user-error "The library is empty"))
     (list (completing-read-multiple
            "Insert library items: "
            (cl-loop for item in items for i from 0
                     collect (excal--library-item-label item i))
            nil t))))
  (let* ((items (excal--library))
         (chosen (cl-loop for item in items for i from 0
                          when (member (excal--library-item-label item i) labels)
                          collect item)))
    (when chosen
      (excal--insert-library-items chosen))))

(defun excal-library-import (file)
  "Merge the library in FILE into the personal library."
  (interactive "fImport library: ")
  (let* ((incoming (with-temp-buffer
                     (insert-file-contents file)
                     (excal--parse-library (buffer-string))))
         (before (length (excal--library))))
    (setq excal--library (excal--merge-library (excal--library) incoming))
    (excal--save-library)
    (message "Imported %d new item%s" (- (length excal--library) before)
             (if (= (- (length excal--library) before) 1) "" "s"))))

(defun excal-library-export (file)
  "Write the personal library to FILE."
  (interactive "FExport library to: ")
  (let ((coding-system-for-write 'utf-8-unix))
    (write-region (excal--serialize-library (excal--library)) nil file)))

(defun excal-library-remove (label)
  "Remove the library item shown as LABEL."
  (interactive
   (let ((items (excal--library)))
     (unless items (user-error "The library is empty"))
     (list (completing-read "Remove library item: "
                            (cl-loop for item in items for i from 0
                                     collect (excal--library-item-label item i))
                            nil t))))
  (setq excal--library
        (cl-loop for item in (excal--library) for i from 0
                 unless (equal (excal--library-item-label item i) label)
                 collect item))
  (excal--save-library))

;;;; Browsing

(defvar-local excal--library-origin nil "The excal buffer the browser inserts into.")

(defun excal--library-thumbnail (item)
  "Return an image of library ITEM, at most `excal--thumbnail-size' wide."
  (let* ((elements (excal--restore-elements (append (alist-get 'elements item) nil)))
         (b (excal--elements-bounds elements))
         (pad 8.0)
         (w (+ (- (nth 2 b) (nth 0 b)) (* 2 pad)))
         (h (+ (- (nth 3 b) (nth 1 b)) (* 2 pad)))
         (scale (min 1.0 (/ excal--thumbnail-size (max w h))))
         (fb (excal-native-fb-create (max 1 (round (* w scale))) (max 1 (round (* h scale)))))
         (file (make-temp-file "excal-lib" nil ".png")))
    (with-temp-buffer
      (setq excal--native-cache (make-hash-table :test #'eq))
      (setq excal--elements elements)
      (excal-native-fb-render fb scale 1.0 (- pad (nth 0 b)) (- pad (nth 1 b))
                              (vconcat (mapcar #'excal--native-element elements)) nil))
    (excal-native-fb-write-png fb file)
    (prog1 (create-image (with-temp-buffer
                           (set-buffer-multibyte nil)
                           (insert-file-contents-literally file)
                           (buffer-string))
                         'png t)
      (delete-file file))))

(defun excal-library-browse ()
  "Show the library as thumbnails; RET or a click inserts an item."
  (interactive)
  (let ((origin (current-buffer))
        (items (excal--library)))
    (with-current-buffer (get-buffer-create "*excal library*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (excal-library-mode)
        (setq excal--library-origin origin)
        (if (null items)
            (insert "The library is empty.  Select elements and run `excal-library-add'.\n")
          (cl-loop for item in items for i from 0
                   do (let ((start (point)))
                        (insert-image (excal--library-thumbnail item))
                        (insert " " (excal--library-item-label item i) "\n")
                        (put-text-property start (point) 'excal-library-item item)
                        (put-text-property start (point) 'mouse-face 'highlight))))
        (goto-char (point-min)))
      (pop-to-buffer (current-buffer)))))

(defun excal-library-insert-at-point (&optional event)
  "Insert the library item at point, or at the click EVENT."
  (interactive (list last-nonmenu-event))
  (let* ((pos (if (mouse-event-p event) (posn-point (event-start event)) (point)))
         (item (get-text-property pos 'excal-library-item))
         (origin excal--library-origin))
    (when (and item (buffer-live-p origin))
      (with-current-buffer origin
        (excal--insert-library-items (list item) (excal--view-center))))))

(defvar-keymap excal-library-mode-map
  "RET" #'excal-library-insert-at-point
  "<mouse-1>" #'excal-library-insert-at-point)

(define-derived-mode excal-library-mode special-mode "Excal-Library"
  "Browse the excal element library.")

(provide 'excal-library)
;;; excal-library.el ends here
