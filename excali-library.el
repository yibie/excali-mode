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
;; - `excali-library-browse' shows thumbnails; RET or a click inserts,
;;   `d' deletes; `excali-library-remove' deletes items by name;
;; - `excali-library-browse-official' lists the official collection of
;;   libraries.excalidraw.com: the same index (`libraries.json') and
;;   files (`libraries/AUTHOR/NAME.excalidrawlib') that the site's "Add to
;;   Excalidraw" button hands to excalidraw.com.  RET previews a library,
;;   `a' adds it all; in a preview, `+' adds the item at point.  Items
;;   from the collection are recognized by their element ids, so adding
;;   a library again adds nothing twice.  The index is cached in
;;   `excali-library-official-cache'; `g' fetches it again.

;;; Code:

(require 'excali-core)
(require 'excali-view)
(require 'excali-select)
(require 'excali-restore)
(require 'excali-clipboard)
(require 'tabulated-list)

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

(defun excali-library-remove (labels)
  "Remove the library items shown as LABELS, a list or a single label."
  (interactive
   (let ((items (excali--library)))
     (unless items (user-error "The library is empty"))
     (list (completing-read-multiple "Remove library items: "
                                     (cl-loop for item in items for i from 0
                                              collect (excali--library-item-label item i))
                                     nil t))))
  (let* ((labels (if (stringp labels) (list labels) labels))
         (doomed (cl-loop for item in (excali--library) for i from 0
                          when (member (excali--library-item-label item i) labels)
                          collect item)))
    (excali--library-delete-items doomed)
    (message "Removed %d item%s" (length doomed) (if (= (length doomed) 1) "" "s"))))

;;;; Browsing

(defvar-local excali--library-origin nil "The excali buffer the browser inserts into.")

(defcustom excali-library-thumbnail-directory
  (expand-file-name "excali/thumbnails/" user-emacs-directory)
  "Directory caching rendered library thumbnails as PNG files, or nil.
With nil, thumbnails are rendered again every time."
  :type '(choice directory (const :tag "No cache" nil))
  :group 'excali)

(defvar excali--thumbnail-memo (make-hash-table :test #'equal)
  "Thumbnail images by cache key, for this session.")

(defun excali--thumbnail-key (item)
  "Return the cache key of ITEM's thumbnail.
It hashes what the picture depends on: the thumbnail size and each
element's id, version, nonce, type and box."
  (sha1 (prin1-to-string
         (cons excali--thumbnail-size
               (mapcar (lambda (e)
                         (mapcar (lambda (key) (alist-get key e))
                                 '(id version versionNonce type x y width height isDeleted)))
                       (alist-get 'elements item))))))

(defun excali--render-thumbnail (item file)
  "Render library ITEM into the PNG FILE, at most `excali--thumbnail-size' wide."
  (let* ((elements (excali--restore-elements (append (alist-get 'elements item) nil)))
         (b (excali--elements-bounds elements))
         (pad 8.0)
         (w (+ (- (nth 2 b) (nth 0 b)) (* 2 pad)))
         (h (+ (- (nth 3 b) (nth 1 b)) (* 2 pad)))
         (scale (min 1.0 (/ excali--thumbnail-size (max w h))))
         (fb (excali-native-fb-create (max 1 (round (* w scale))) (max 1 (round (* h scale))))))
    (with-temp-buffer
      (setq excali--native-cache (make-hash-table :test #'eq))
      (setq excali--elements elements)
      (excali-native-fb-render fb scale 1.0 (- pad (nth 0 b)) (- pad (nth 1 b))
                              (vconcat (mapcar #'excali--native-element elements)) nil))
    (make-directory (file-name-directory file) t)
    (excali-native-fb-write-png fb file)))

(defun excali--cached-thumbnail (item)
  "Return ITEM's thumbnail if it needs no rendering, else nil."
  (let ((key (excali--thumbnail-key item)))
    (or (gethash key excali--thumbnail-memo)
        (when-let* ((dir excali-library-thumbnail-directory)
                    (file (expand-file-name (concat key ".png") dir))
                    ((file-exists-p file)))
          (puthash key (create-image file 'png nil) excali--thumbnail-memo)))))

(defun excali--library-thumbnail (item)
  "Return an image of library ITEM, rendering it unless cached."
  (or (excali--cached-thumbnail item)
      (let ((key (excali--thumbnail-key item)))
        (puthash key
                 (if excali-library-thumbnail-directory
                     (let ((file (expand-file-name (concat key ".png")
                                                   excali-library-thumbnail-directory)))
                       (excali--render-thumbnail item file)
                       (create-image file 'png nil))
                   ;; No cache: keep the pixels, not the file.
                   (let ((file (make-temp-file "excali-lib" nil ".png")))
                     (unwind-protect
                         (progn (excali--render-thumbnail item file)
                                (create-image (with-temp-buffer
                                                (set-buffer-multibyte nil)
                                                (insert-file-contents-literally file)
                                                (buffer-string))
                                              'png t))
                       (delete-file file))))
                 excali--thumbnail-memo))))

;;;;; Rendering thumbnails in the background

(defvar-local excali--thumbnail-queue nil
  "Rows still showing a placeholder, as (MARKER . ITEM) in buffer order.")
(defvar-local excali--thumbnail-timer nil "Timer rendering queued thumbnails.")

(defconst excali--thumbnail-budget 0.03
  "Seconds of thumbnail rendering between chances to handle input.")

(defun excali--thumbnail-placeholder ()
  "Return a thumbnail-sized grey square standing in for a picture."
  (propertize " " 'display `(space :width (,excali--thumbnail-size)
                                   :height (,excali--thumbnail-size))
              'face '(:background "#e9ecef")))

(defun excali--thumbnail-reset ()
  "Forget the thumbnails queued in this buffer."
  (when excali--thumbnail-timer (cancel-timer excali--thumbnail-timer))
  (dolist (job excali--thumbnail-queue) (set-marker (car job) nil))
  (setq excali--thumbnail-timer nil excali--thumbnail-queue nil))

(defun excali--next-thumbnail-job ()
  "Pop the next queued thumbnail, preferring one a window shows."
  (let* ((windows (get-buffer-window-list nil nil t))
         (job (or (seq-find (lambda (job)
                              (seq-some (lambda (w)
                                          (<= (window-start w) (car job) (window-end w)))
                                        windows))
                            excali--thumbnail-queue)
                  (car excali--thumbnail-queue))))
    (setq excali--thumbnail-queue (delq job excali--thumbnail-queue))
    job))

(defun excali--fill-thumbnail (job)
  "Replace the placeholder of JOB, (MARKER . ITEM), with its thumbnail."
  (let ((inhibit-read-only t) (pos (car job)))
    (when (marker-buffer pos)
      (with-silent-modifications
        (put-text-property pos (1+ pos) 'display (excali--library-thumbnail (cdr job)))
        (remove-text-properties pos (1+ pos) '(face nil)))
      (set-marker pos nil))))

(defun excali--thumbnail-work (buffer)
  "Render queued thumbnails of BUFFER for a moment, then yield."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq excali--thumbnail-timer nil)
      (let ((deadline (+ (float-time) excali--thumbnail-budget)))
        (while (and excali--thumbnail-queue (< (float-time) deadline))
          (excali--fill-thumbnail (excali--next-thumbnail-job))))
      (when excali--thumbnail-queue
        (setq excali--thumbnail-timer
              (run-with-timer 0.01 nil #'excali--thumbnail-work buffer))))))

(defun excali--library-finish-thumbnails ()
  "Render every thumbnail still queued in this buffer now."
  (while excali--thumbnail-queue
    (excali--fill-thumbnail (excali--next-thumbnail-job)))
  (excali--thumbnail-reset))

(defun excali--insert-item-rows (items &optional label status)
  "Insert a thumbnail row for each of ITEMS, with LABEL's text.
LABEL is called with an item and its index; it defaults to
`excali--library-item-label'.  STATUS, if given, is called with an item
and returns a string to show after the label, or nil.  Each row carries
its item as the `excali-library-item' property.  Thumbnails not cached
yet start as placeholders and are rendered in the background, those a
window shows first."
  (cl-loop for item in items for i from 0
           do (let ((start (point)))
                (if-let* ((image (excali--cached-thumbnail item)))
                    (insert-image image)
                  (push (cons (copy-marker (point)) item) excali--thumbnail-queue)
                  (insert (excali--thumbnail-placeholder)))
                (insert " " (funcall (or label #'excali--library-item-label) item i))
                (insert (propertize (or (and status (funcall status item)) "")
                                    'excali-library-status t))
                (insert "\n")
                (put-text-property start (point) 'excali-library-item item)
                (put-text-property start (point) 'mouse-face 'highlight)))
  (setq excali--thumbnail-queue (nreverse excali--thumbnail-queue))
  (when (and excali--thumbnail-queue (not excali--thumbnail-timer))
    (setq excali--thumbnail-timer
          (run-with-timer 0 nil #'excali--thumbnail-work (current-buffer)))))

(defun excali--library-render-browser ()
  "Fill the current buffer with the personal library's thumbnails."
  (let ((inhibit-read-only t)
        (items (excali--library)))
    (excali--thumbnail-reset)
    (erase-buffer)
    (if (null items)
        (insert "The library is empty.  Select elements and run `excali-library-add',\n"
                "or add libraries from the official collection with "
                "`excali-library-browse-official'.\n")
      (excali--insert-item-rows items))
    (goto-char (point-min))))

(defun excali-library-browse ()
  "Show the library as thumbnails; RET or a click inserts an item.
\\<excali-library-mode-map>\\[excali-library-delete-at-point] deletes the item at point."
  (interactive)
  (let ((origin (current-buffer)))
    (with-current-buffer (get-buffer-create "*excali library*")
      (excali-library-mode)
      (setq excali--library-origin origin)
      (excali--library-render-browser)
      (pop-to-buffer (current-buffer)))))

(defun excali--origin-buffer (origin)
  "Return ORIGIN if it is a live excali buffer, else the latest one, or nil."
  (if (and (buffer-live-p origin)
           (eq (buffer-local-value 'major-mode origin) 'excali-mode))
      origin
    (seq-find (lambda (b) (eq (buffer-local-value 'major-mode b) 'excali-mode))
              (buffer-list))))

(defun excali--library-item-at (&optional event)
  "Return the library item at point, or at the click EVENT."
  (get-text-property (if (mouse-event-p event) (posn-point (event-start event)) (point))
                     'excali-library-item))

(defun excali-library-insert-at-point (&optional event)
  "Insert the library item at point, or at the click EVENT."
  (interactive (list last-nonmenu-event))
  (let ((item (excali--library-item-at event))
        (origin (excali--origin-buffer excali--library-origin)))
    (cond ((null item) (user-error "No library item here"))
          ((null origin) (user-error "No excali buffer to insert into"))
          (t (with-current-buffer origin
               (excali--insert-library-items (list item) (excali--view-center)))))))

(defun excali--library-same-entry-p (a b)
  "Return non-nil if A and B are the same library item.
Items are compared by id, so a browser row still finds its item after
the library was read again."
  (or (eq a b) (equal (alist-get 'id a) (alist-get 'id b))))

(defun excali--library-delete-items (items)
  "Remove ITEMS from the personal library and save it."
  (setq excali--library
        (seq-remove (lambda (item)
                      (seq-some (lambda (doomed) (excali--library-same-entry-p item doomed))
                                items))
                    (excali--library)))
  (excali--save-library))

(defun excali-library-delete-at-point ()
  "Delete the library item at point, after confirming."
  (interactive)
  (let* ((item (or (excali--library-item-at) (user-error "No library item here")))
         (index (or (cl-position item (excali--library) :test #'excali--library-same-entry-p)
                    (user-error "That item is no longer in the library")))
         (line (line-number-at-pos)))
    (when (y-or-n-p (format "Delete %s from the library? "
                            (excali--library-item-label item index)))
      (excali--library-delete-items (list item))
      (excali--library-render-browser)
      (forward-line (1- (min line (max 1 (length (excali--library))))))
      (message "Deleted; %d item%s left" (length (excali--library))
               (if (= (length (excali--library)) 1) "" "s")))))

(defvar-keymap excali-library-mode-map
  "RET" #'excali-library-insert-at-point
  "<mouse-1>" #'excali-library-insert-at-point
  "d" #'excali-library-delete-at-point
  "DEL" #'excali-library-delete-at-point
  "g" #'excali-library-browse-refresh
  "o" #'excali-library-browse-official)

(defun excali-library-browse-refresh ()
  "Show the personal library again."
  (interactive)
  (excali--library-render-browser))

(define-derived-mode excali-library-mode special-mode "Excali-Library"
  "Browse the excali element library.
\\{excali-library-mode-map}")

;;;; The official collection (libraries.excalidraw.com)

(defcustom excali-library-official-url "https://libraries.excalidraw.com/"
  "Where the official library collection is served.
It holds the index `libraries.json', download counts in `stats.json'
and each library under `libraries/'.  These are the URLs the site's
\"Add to Excalidraw\" button hands to excalidraw.com as `?addLibrary='."
  :type 'string
  :group 'excali)

(defcustom excali-library-official-cache
  (expand-file-name "excali/official-libraries.json" user-emacs-directory)
  "File caching the official collection's index between sessions."
  :type 'file
  :group 'excali)

(defvar excali--official-index nil
  "The official collection: a list of index entries, or nil until loaded.
Entries are alists as in `libraries.json', plus `downloads'.")

(defun excali--official-url (path)
  "Return the URL of PATH in the official collection."
  (concat (file-name-as-directory excali-library-official-url) path))

(defun excali--official-library-url (entry)
  "Return the URL of the `.excalidrawlib' file of index ENTRY."
  (excali--official-url (concat "libraries/" (alist-get 'source entry))))

(defun excali--official-id (entry)
  "Return the site's id of index ENTRY: its source, flattened.
\"lipis/polygons.excalidrawlib\" becomes \"lipis-polygons\", the key of
`stats.json' and the anchor of the library on the site."
  (replace-regexp-in-string
   "/" "-" (string-remove-suffix ".excalidrawlib" (downcase (alist-get 'source entry)))))

;;;;; Fetching

(defun excali--http-body (binary)
  "Return the body of the HTTP response in the current buffer.
Text is decoded as UTF-8 unless BINARY."
  (goto-char (point-min))
  (let ((body (if (re-search-forward "\r?\n\r?\n" nil t)
                  (buffer-substring-no-properties (point) (point-max))
                "")))
    (if binary body (decode-coding-string body 'utf-8))))

(defun excali--library-fetch (url callback &optional binary)
  "Fetch URL in the background and call CALLBACK with its body.
CALLBACK takes the body (a string, raw bytes if BINARY) and nil, or nil
and an error message."
  (url-retrieve
   url
   (lambda (status)
     (let ((response (current-buffer))
           (failure (plist-get status :error)))
       (unwind-protect
           (if failure
               (funcall callback nil
                        (format "%s: %s" url
                                (pcase failure
                                  (`(error http ,code) (format "HTTP %s" code))
                                  (_ (error-message-string failure)))))
             (funcall callback (excali--http-body binary) nil))
         (kill-buffer response))))
   nil t t))

;;;;; The index

(defun excali--parse-json (text)
  "Parse JSON TEXT the way the library code expects."
  (json-parse-string text :object-type 'alist :array-type 'array
                     :null-object :null :false-object :false))

(defun excali--official-parse-index (libraries stats)
  "Return index entries from LIBRARIES and STATS, both parsed JSON.
Each entry gets `downloads', its total from STATS or 0."
  (let ((stats (and (consp stats) stats)))
    (mapcar (lambda (entry)
              (let ((counts (alist-get (intern (excali--official-id entry)) stats)))
                (cons (cons 'downloads (or (alist-get 'total counts) 0)) entry)))
            (append libraries nil))))

(defun excali--official-load-cache ()
  "Load the index cached in `excali-library-official-cache', if any."
  (when (and (null excali--official-index) (file-readable-p excali-library-official-cache))
    (let ((data (with-temp-buffer
                  (insert-file-contents excali-library-official-cache)
                  (excali--parse-json (buffer-string)))))
      (setq excali--official-index
            (excali--official-parse-index (alist-get 'libraries data) (alist-get 'stats data)))))
  excali--official-index)

(defun excali-library-official-refresh (&optional callback)
  "Download the official collection's index again, then call CALLBACK."
  (interactive)
  (message "Fetching the Excalidraw library index...")
  (excali--library-fetch
   (excali--official-url "libraries.json")
   (lambda (libraries error)
     (if error
         (message "Could not fetch the library index: %s" error)
       (excali--library-fetch
        (excali--official-url "stats.json")
        (lambda (stats _error)
          ;; Download counts are a nicety: carry on without them.
          (let ((libraries (excali--parse-json libraries))
                (stats (and stats (ignore-errors (excali--parse-json stats)))))
            (make-directory (file-name-directory excali-library-official-cache) t)
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (json-serialize (list (cons 'libraries libraries)
                                                  (cons 'stats (if (consp stats) stats :null)))
                                            :null-object :null :false-object :false)
                            nil excali-library-official-cache nil 'silent))
            (setq excali--official-index (excali--official-parse-index libraries stats))
            (message "Fetched %d libraries" (length excali--official-index))
            (when callback (funcall callback)))))))))

;;;;; Adding

(defun excali--library-same-ids-p (a b)
  "Return non-nil if items A and B hold elements with the same ids, in order.
Restoring gives elements without an index a new `versionNonce', so
libraries from the collection are recognized by their element ids."
  (let ((ea (alist-get 'elements a)) (eb (alist-get 'elements b)))
    (and (= (length ea) (length eb))
         (cl-every (lambda (x y) (equal (alist-get 'id x) (alist-get 'id y))) ea eb))))

(defvar excali--official-last-added nil
  "The items of the last addition that the personal library now holds.")

(defvar excali--official-complete (make-hash-table :test #'equal)
  "Sources of the collection's libraries known to be wholly in the library.")

(defun excali--official-in-library-p (item)
  "Return non-nil if the personal library holds ITEM."
  (seq-some (lambda (l) (excali--library-same-ids-p l item)) (excali--library)))

(defun excali--official-note-library (entry items)
  "Remember whether all ITEMS of the library ENTRY are in the personal library."
  (if (and items (seq-every-p #'excali--official-in-library-p items))
      (puthash (alist-get 'source entry) t excali--official-complete)
    (remhash (alist-get 'source entry) excali--official-complete)))

(defface excali-library-added
  '((t :inherit success :weight bold))
  "Face of the mark on library items the personal library holds."
  :group 'excali)

(defface excali-library-flash
  '((((background dark)) :background "#2b5c34")
    (t :background "#d3f9d8"))
  "Face briefly lighting up rows that were just added."
  :group 'excali)

(defun excali--flash-regions (regions)
  "Light up REGIONS, a list of (START . END), for a moment."
  (let ((overlays (mapcar (lambda (r)
                            (let ((ov (make-overlay (car r) (cdr r))))
                              (overlay-put ov 'face 'excali-library-flash)
                              (overlay-put ov 'priority 100)
                              ov))
                          regions)))
    (run-with-timer 0.9 nil (lambda () (mapc #'delete-overlay overlays)))))

(defun excali--official-add-items (entry items)
  "Add library ITEMS from index ENTRY to the personal library.
Items already present are skipped; unnamed ones take the library's
name.  Return the number added."
  (let* ((local (excali--library))
         (new (mapcar (lambda (item)
                        (let ((name (alist-get 'name item)))
                          (if (and (stringp name) (not (string-empty-p name)))
                              item
                            (cons (cons 'name (alist-get 'name entry)) item))))
                      (seq-remove (lambda (item)
                                    (seq-some (lambda (l) (excali--library-same-ids-p l item))
                                              local))
                                  items))))
    (when new
      (setq excali--library (append new local))
      (excali--save-library))
    (setq excali--official-last-added
          (seq-filter (lambda (item) (excali--official-in-library-p item)) items))
    (excali--official-note-library entry items)
    (message "Added %d item%s from %s%s" (length new) (if (= (length new) 1) "" "s")
             (alist-get 'name entry)
             (if (< (length new) (length items))
                 (format " (%d already in the library)" (- (length items) (length new)))
               ""))
    (length new)))

(defun excali--official-fetch-library (entry callback &optional on-error)
  "Fetch the library of index ENTRY and call CALLBACK with its items.
On failure, ON-ERROR, if given, is called with the error message."
  (message "Fetching %s..." (alist-get 'name entry))
  (excali--library-fetch
   (excali--official-library-url entry)
   (lambda (text error)
     (if error
         (progn (message "Could not fetch %s: %s" (alist-get 'name entry) error)
                (when on-error (funcall on-error error)))
       (funcall callback (excali--parse-library text))))))

;;;;; The list

(defvar-local excali--official-filter nil "Regexp the collection list is narrowed to.")

(defun excali--official-entry-text (entry)
  "Return the text of ENTRY that a filter searches."
  (mapconcat #'identity
             (append (list (alist-get 'name entry) (or (alist-get 'description entry) ""))
                     (mapcar (lambda (a) (or (alist-get 'name a) "")) (alist-get 'authors entry))
                     (append (alist-get 'itemNames entry) nil))
             " "))

(defun excali--official-row (entry)
  "Return the tabulated list row of index ENTRY."
  (let ((items (alist-get 'itemNames entry)))
    (list entry
          (vector (if (gethash (alist-get 'source entry) excali--official-complete)
                      (propertize "✓" 'face 'excali-library-added)
                    "")
                  (alist-get 'name entry)
                  (if items (number-to-string (length items)) "")
                  (number-to-string (alist-get 'downloads entry))
                  (or (alist-get 'updated entry) "")
                  (mapconcat (lambda (a) (or (alist-get 'name a) "")) (alist-get 'authors entry) ", ")
                  (replace-regexp-in-string "[\n\t ]+" " " (or (alist-get 'description entry) ""))))))

(defun excali--official-numeric-sort (column)
  "Return a predicate sorting rows by the number in COLUMN."
  (lambda (a b) (< (string-to-number (aref (cadr a) column))
                   (string-to-number (aref (cadr b) column)))))

(defun excali--official-refresh-list ()
  "Show the index in the current collection buffer."
  (setq tabulated-list-entries
        (mapcar #'excali--official-row
                (seq-filter (lambda (e) (or (null excali--official-filter)
                                            (string-match-p excali--official-filter
                                                            (excali--official-entry-text e))))
                            excali--official-index)))
  (tabulated-list-print t))

(defun excali-library-browse-official ()
  "List the official Excalidraw library collection.
\\<excali-library-official-mode-map>\\[excali-library-official-preview] previews a library, \
\\[excali-library-official-add] adds it to the personal library."
  (interactive)
  (let ((origin (if (derived-mode-p 'excali-library-mode) excali--library-origin
                  (current-buffer)))
        (buffer (get-buffer-create "*excali official libraries*")))
    (with-current-buffer buffer
      (excali-library-official-mode)
      (setq excali--library-origin origin)
      (if (excali--official-load-cache)
          (excali--official-refresh-list)
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Fetching the library index...\n"))
        (excali-library-official-refresh
         (lambda () (when (buffer-live-p buffer)
                      (with-current-buffer buffer (excali--official-refresh-list)))))))
    (pop-to-buffer buffer)))

(defun excali--official-entry-at-point ()
  "Return the index entry at point, or signal an error."
  (or (tabulated-list-get-id) (user-error "No library here")))

(defun excali-library-official-add (entry)
  "Add every item of the official library ENTRY to the personal library.
In the collection's list, its row is then marked and lights up."
  (interactive (list (excali--official-entry-at-point)))
  (let ((buffer (current-buffer)))
    (excali--official-fetch-library
     entry (lambda (items)
             (excali--official-add-items entry items)
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (when (derived-mode-p 'excali-library-official-mode)
                   (excali--official-refresh-list)
                   (when (excali--official-goto entry)
                     (excali--flash-regions
                      (list (cons (line-beginning-position) (line-end-position))))))))))))

(defun excali--official-goto (entry)
  "Move to the row of ENTRY in the collection's list; return non-nil if found."
  (let ((start (point)) found)
    (goto-char (point-min))
    (while (and (not found) (not (eobp)))
      (if (equal (alist-get 'source (tabulated-list-get-id)) (alist-get 'source entry))
          (setq found t)
        (forward-line 1)))
    (unless found (goto-char start))
    found))

(defun excali-library-official-filter (regexp)
  "Show only libraries whose name, description, authors or items match REGEXP.
An empty REGEXP shows them all."
  (interactive (list (read-regexp "Filter libraries (regexp, empty for all)")))
  (setq excali--official-filter (and regexp (not (string-empty-p regexp)) regexp))
  (excali--official-refresh-list))

(defun excali-library-official-update ()
  "Fetch the index again and show it."
  (interactive)
  (let ((buffer (current-buffer)))
    (excali-library-official-refresh
     (lambda () (when (buffer-live-p buffer)
                  (with-current-buffer buffer (excali--official-refresh-list)))))))

(defun excali-library-official-visit (entry)
  "Show the official library ENTRY on the collection's site."
  (interactive (list (excali--official-entry-at-point)))
  (browse-url (concat (file-name-as-directory excali-library-official-url)
                      "#" (excali--official-id entry))))

(defvar-keymap excali-library-official-mode-map
  :parent tabulated-list-mode-map
  "RET" #'excali-library-official-preview
  "a" #'excali-library-official-add
  "/" #'excali-library-official-filter
  "g" #'excali-library-official-update
  "o" #'excali-library-official-visit)

(define-derived-mode excali-library-official-mode tabulated-list-mode "Excali-Libraries"
  "List the official Excalidraw library collection.
\\{excali-library-official-mode-map}"
  (setq tabulated-list-format
        (vector '("" 1 nil)
                '("Name" 28 t)
                (list "Items" 5 (excali--official-numeric-sort 2) :right-align t)
                (list "Downloads" 9 (excali--official-numeric-sort 3) :right-align t)
                '("Updated" 10 t)
                '("Authors" 20 t)
                '("Description" 0 nil))
        tabulated-list-sort-key '("Updated" . t)
        tabulated-list-padding 1)
  (tabulated-list-init-header))

;;;;; Previewing a library

(defvar-local excali--official-entry nil "The index entry a preview shows.")
(defvar-local excali--official-items nil "The items of the library a preview shows.")
(defvar-local excali--official-return nil
  "The list a preview goes back to: (FILTER . ENTRY).")
(defvar-local excali--official-error nil "Why the previewed library could not be fetched.")

(defun excali-library-official-preview (entry)
  "Show the items of the official library ENTRY in place of the list.
In the preview, RET inserts an item into the scene, `+' adds it to the
personal library, `a' adds them all and `q' goes back to the list."
  (interactive (list (excali--official-entry-at-point)))
  (let* ((listing (derived-mode-p 'excali-library-official-mode))
         (origin (cond (listing excali--library-origin)
                       ((derived-mode-p 'excali-library-mode) excali--library-origin)
                       (t (current-buffer))))
         (return (cons (and listing excali--official-filter) entry))
         (buffer (if listing (current-buffer)
                   (get-buffer-create "*excali official libraries*"))))
    (with-current-buffer buffer
      (excali-library-official-preview-mode)
      (setq excali--library-origin origin
            excali--official-return return
            excali--official-entry entry)
      (excali--official-render-preview))
    (unless listing (pop-to-buffer buffer))
    (excali--official-fetch-library
     entry
     (lambda (items)
       (excali--official-preview-arrived buffer entry items nil))
     (lambda (error)
       (excali--official-preview-arrived buffer entry nil error)))))

(defun excali--official-preview-arrived (buffer entry items error)
  "Show ITEMS (or the fetch ERROR) of ENTRY if BUFFER still previews it."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'excali-library-official-preview-mode)
                 (eq excali--official-entry entry))
        (setq excali--official-items items excali--official-error error)
        (when items (excali--official-note-library entry items))
        (excali--official-render-preview)))))

(defun excali--official-item-status (item)
  "Return the mark of ITEM in a preview: whether the library holds it."
  (and (excali--official-in-library-p item)
       (propertize "  ✓ in library" 'face 'excali-library-added)))

(defun excali--official-preview-header ()
  "Return the header line of a preview."
  (concat (substitute-command-keys
           (concat "\\<excali-library-official-preview-mode-map>"
                   "\\[excali-library-official-back] back to the collection  "
                   "\\[excali-library-insert-at-point] insert  "
                   "\\[excali-library-official-add-at-point] add item  "
                   "\\[excali-library-official-add-all] add all"))
          (when excali--official-items
            (format "    %d of %d in your library"
                    (seq-count #'excali--official-in-library-p excali--official-items)
                    (length excali--official-items)))))

(defun excali--official-render-preview ()
  "Fill the preview buffer with the previewed library."
  (let ((inhibit-read-only t)
        (entry excali--official-entry)
        (items excali--official-items))
    (excali--thumbnail-reset)
    (erase-buffer)
    (insert (propertize (alist-get 'name entry) 'face 'bold)
            "  by " (mapconcat (lambda (a) (or (alist-get 'name a) ""))
                               (alist-get 'authors entry) ", ")
            "\n" (or (alist-get 'description entry) "") "\n\n")
    (cond
     (excali--official-error
      (insert (format "Could not fetch the library: %s\n" excali--official-error)))
     ((null items) (insert "Fetching the library...\n"))
     (t
      (excali--official-insert-preview-image entry (current-buffer) (point))
      (excali--insert-item-rows
       items (lambda (item i)
               (let ((name (alist-get 'name item)))
                 (format "%d. %s" (1+ i)
                         (if (and (stringp name) (not (string-empty-p name))) name
                           (format "%s %d" (alist-get 'name entry) (1+ i))))))
       #'excali--official-item-status)))
    (goto-char (point-min))
    (force-mode-line-update)))

(defun excali--official-mark-rows (items)
  "Mark the preview rows of ITEMS as just added and light them up."
  (let ((inhibit-read-only t) regions)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((item (get-text-property (point) 'excali-library-item))
              (bol (line-beginning-position)) (eol (line-end-position)))
          (when (and item (seq-some (lambda (i) (excali--library-same-ids-p i item)) items))
            ;; A row without a mark has an empty one at its end.
            (let* ((start (or (text-property-any bol eol 'excali-library-status t) eol))
                   (end (or (next-single-property-change start 'excali-library-status nil eol)
                            eol)))
              (delete-region start end)
              (goto-char start)
              (insert (propertize "  ✓ added" 'face 'excali-library-added
                                  'excali-library-status t
                                  'excali-library-item item 'mouse-face 'highlight)))
            (push (cons bol (line-end-position)) regions)))
        (forward-line 1)))
    (excali--flash-regions regions)
    (force-mode-line-update)))

(defun excali-library-official-back ()
  "Go back from a preview to the collection's list, where it was."
  (interactive)
  (let ((origin excali--library-origin)
        (return excali--official-return))
    (excali--thumbnail-reset)
    (excali-library-official-mode)
    (setq excali--library-origin origin
          excali--official-filter (car return))
    (excali--official-refresh-list)
    (goto-char (point-min))
    (when (cdr return) (excali--official-goto (cdr return)))))

(defun excali--official-insert-preview-image (entry buffer position)
  "Fetch the site's preview picture of ENTRY and show it in BUFFER at POSITION."
  (when-let* ((preview (alist-get 'preview entry))
              (marker (with-current-buffer buffer (copy-marker position))))
    (excali--library-fetch
     (excali--official-url (format "libraries/%s?v=%s" preview (or (alist-get 'updated entry) 0)))
     (lambda (data _error)
       (when (and data (buffer-live-p buffer))
         (with-current-buffer buffer
           (let ((inhibit-read-only t)
                 (image (ignore-errors
                          (create-image data (if (string-suffix-p ".png" preview) 'png 'jpeg)
                                        t :max-width 640 :max-height 360))))
             (when image
               (save-excursion
                 (goto-char marker)
                 (insert-image image)
                 (insert "\n\n"))))))
       (set-marker marker nil))
     t)))

(defun excali-library-official-add-at-point ()
  "Add the previewed item at point to the personal library, and mark it."
  (interactive)
  (excali--official-add-items excali--official-entry
                              (list (or (excali--library-item-at) (user-error "No item here"))))
  (excali--official-mark-rows excali--official-last-added))

(defun excali-library-official-add-all ()
  "Add every previewed item to the personal library, and mark them."
  (interactive)
  (unless excali--official-items (user-error "The library has not arrived yet"))
  (excali--official-add-items excali--official-entry excali--official-items)
  (excali--official-mark-rows excali--official-last-added))

(defvar-keymap excali-library-official-preview-mode-map
  "RET" #'excali-library-insert-at-point
  "<mouse-1>" #'excali-library-insert-at-point
  "+" #'excali-library-official-add-at-point
  "a" #'excali-library-official-add-all
  "^" #'excali-library-official-back
  "q" #'excali-library-official-back)

(define-derived-mode excali-library-official-preview-mode special-mode "Excali-Library"
  "Preview a library of the official collection, in place of its list.
\\{excali-library-official-preview-mode-map}"
  (setq header-line-format '(:eval (excali--official-preview-header))))

(provide 'excali-library)
;;; excali-library.el ends here
