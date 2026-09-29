;;; excali-library-test.el --- Element library  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)
(require 'excali-test)

(defmacro excali-library-test--with-file (&rest body)
  "Run BODY with a fresh temporary library file."
  `(let* ((dir (make-temp-file "excali-lib" t))
          (excali-library-file (expand-file-name "lib.excalidrawlib" dir))
          (excali-library-thumbnail-directory (expand-file-name "thumbnails/" dir))
          (excali--thumbnail-memo (make-hash-table :test #'equal))
          (excali--official-complete (make-hash-table :test #'equal))
          (excali--library nil) (excali--library-loaded nil))
     (unwind-protect (progn ,@body)
       ;; Background thumbnail work must not outlive the bindings above.
       (dolist (buffer (buffer-list))
         (with-current-buffer buffer
           (when (or excali--thumbnail-queue excali--thumbnail-timer)
             (excali--thumbnail-reset))))
       (delete-directory dir t))))

(ert-deftest excali-library-test-add-save-reload ()
  "Adding the selection writes a v2 library that reads back."
  (excali-library-test--with-file
   (excali-test--with-scene
    (setq excali--backend nil)
    (let ((a (excali-test--rect 0 0)) (b (excali-test--rect 20 0)))
      (setq excali--elements (list a b))
      (excali--select (list a b))
      (excali-library-add "Pair")
      (setq excali--library nil excali--library-loaded nil)
      (let ((items (excali--library)))
        (should (= (length items) 1))
        (should (equal (alist-get 'name (car items)) "Pair"))
        (should (= (length (alist-get 'elements (car items))) 2)))
      (with-temp-buffer
        (insert-file-contents excali-library-file)
        (let ((data (json-parse-buffer :object-type 'alist)))
          (should (equal (alist-get 'type data) "excalidrawlib"))
          (should (= (alist-get 'version data) 2))))))))

(ert-deftest excali-library-test-v1-and-merge ()
  "Version 1 libraries load; merging skips items already present."
  ;; Elements exported by Excalidraw carry valid indices, so restoring
  ;; them does not bump versions and duplicates can be recognized.
  (let* ((el (lambda (id) (list (cons 'id id) (cons 'type "rectangle") (cons 'x 0)
                                (cons 'y 0) (cons 'width 10) (cons 'height 10)
                                (cons 'index "a0") (cons 'version 3)
                                (cons 'versionNonce 7))))
         (v1 (json-serialize (list (cons 'type "excalidrawlib") (cons 'version 1)
                                   (cons 'library (vector (vector (funcall el "a"))
                                                          (vector (funcall el "b"))
                                                          []))))))
    (let ((items (excali--parse-library v1)))
      ;; The empty item is dropped; the others get ids and a status.
      (should (= (length items) 2))
      (should (equal (alist-get 'status (car items)) "unpublished"))
      (should (stringp (alist-get 'id (car items))))
      (let ((merged (excali--merge-library items (excali--parse-library v1))))
        (should (= (length merged) 2))))
    (should-error (excali--parse-library "{\"type\":\"excalidraw\"}"))))

(ert-deftest excali-library-test-insert-grid ()
  "Inserted items get new ids and sit on a square grid around the target."
  (excali-library-test--with-file
   (excali-test--with-scene
    (setq excali--backend nil excali--canvas-size '(800 . 600))
    (let ((items (list (list (cons 'id "i1") (cons 'status "unpublished")
                             (cons 'elements (vector (excali-test--rect 0 0))))
                       (list (cons 'id "i2") (cons 'status "unpublished")
                             (cons 'elements (vector (excali-test--rect 500 500)))))))
      (excali--insert-library-items items '(100.0 . 100.0))
      (should (= (length excali--elements) 2))
      (should (equal excali--selection excali--elements))
      (let ((b (excali--elements-bounds excali--elements)))
        ;; Two cells side by side, 50 apart, centered on the target.
        (should (= (- (nth 2 b) (nth 0 b)) 70.0))
        (should (= (/ (+ (nth 0 b) (nth 2 b)) 2) 100.0)))
      (should-not (member (excali--get (car excali--elements) 'id) '("i1" "i2")))))))

(defun excali-library-test--item (x)
  "Return a library item of one 400x100 rectangle at X."
  (list (cons 'id (format "item-%s" x))
        (cons 'elements (vector (excali--make-element "rectangle" x 0 (cons 'width 400.0)
                                                      (cons 'height 100.0))))))

(ert-deftest excali-library-test-thumbnail ()
  "Thumbnails render to images no larger than the thumbnail size."
  (excali-library-test--with-file
   (let ((image (excali--library-thumbnail (excali-library-test--item 0))))
     (should (eq (car image) 'image))
     (should (eq (plist-get (cdr image) :type) 'png)))))

;;;; Deleting

(defun excali-library-test--item-positions ()
  "Return the start of each item row in the current browser buffer."
  (let ((pos (point-min)) starts)
    (while (setq pos (text-property-not-all pos (point-max) 'excali-library-item nil))
      (push pos starts)
      (setq pos (or (next-single-property-change pos 'excali-library-item) (point-max))))
    (nreverse starts)))

(defun excali-library-test--two-items ()
  "Store two named one-rectangle items in the library, newest first."
  (setq excali--library
        (list (list (cons 'id "i2") (cons 'status "unpublished") (cons 'name "Second")
                    (cons 'elements (vector (excali-test--rect 30 0))))
              (list (cons 'id "i1") (cons 'status "unpublished") (cons 'name "First")
                    (cons 'elements (vector (excali-test--rect 0 0)))))
        excali--library-loaded t)
  (excali--save-library))

(ert-deftest excali-library-test-delete-at-point ()
  "`d' in the browser deletes the item at point, after asking, and saves."
  (excali-library-test--with-file
   (excali-library-test--two-items)
   (save-window-excursion
     (excali-library-browse)
     (with-current-buffer "*excali library*"
       (should (eq (key-binding "d") 'excali-library-delete-at-point))
       (goto-char (car (excali-library-test--item-positions)))
       ;; Declining keeps it.
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
         (excali-library-delete-at-point))
       (should (= (length (excali--library)) 2))
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
         (excali-library-delete-at-point))
       (should (equal (mapcar (lambda (i) (alist-get 'name i)) (excali--library)) '("First")))
       ;; The buffer shows what is left, and the file holds it.
       (should (= (length (excali-library-test--item-positions)) 1))
       (setq excali--library nil excali--library-loaded nil)
       (should (equal (mapcar (lambda (i) (alist-get 'name i)) (excali--library)) '("First")))
       (goto-char (car (excali-library-test--item-positions)))
       (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
         (excali-library-delete-at-point))
       (should (null (excali--library)))
       (should (string-match-p "empty" (buffer-string)))))))

(ert-deftest excali-library-test-remove-by-name ()
  "`excali-library-remove' deletes the items chosen by label."
  (excali-library-test--with-file
   (excali-library-test--two-items)
   (excali-library-remove (list (excali--library-item-label (car (excali--library)) 0)))
   (should (equal (mapcar (lambda (i) (alist-get 'name i)) (excali--library)) '("First")))
   (excali-library-remove (excali--library-item-label (car (excali--library)) 0))
   (should (null (excali--library)))))

;;;; The official collection

(defconst excali-library-test--fixtures
  (expand-file-name "fixtures/library"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Trimmed real files from excalidraw/excalidraw-libraries.")

(defvar excali-library-test--fetched nil "URLs the stubbed fetch was asked for.")

(defmacro excali-library-test--offline (&rest body)
  "Run BODY with a fresh library and collection, fetches served from fixtures.
A URL is answered, synchronously, with the fixture named like its last
path component; others fail as a 404 would."
  `(excali-library-test--with-file
    (let ((excali-library-official-cache (expand-file-name "official.json" dir))
          (excali-library-official-url "https://libraries.excalidraw.com/")
          (excali--official-index nil)
          (excali-library-test--fetched nil))
      (cl-letf (((symbol-function 'excali--library-fetch)
                 (lambda (url callback &optional _binary)
                   (push url excali-library-test--fetched)
                   (let ((file (expand-file-name
                                (file-name-nondirectory (car (split-string url "?")))
                                excali-library-test--fixtures)))
                     (if (file-readable-p file)
                         (funcall callback (with-temp-buffer
                                             (insert-file-contents file)
                                             (buffer-string))
                                  nil)
                       (funcall callback nil (format "%s: 404" url)))))))
        (save-window-excursion ,@body)))))

(defun excali-library-test--entry (name)
  "Return the collection's index entry called NAME."
  (seq-find (lambda (e) (equal (alist-get 'name e) name)) excali--official-index))

(ert-deftest excali-library-test-official-urls ()
  "Library files and ids follow the site's scheme."
  (let ((excali-library-official-url "https://libraries.excalidraw.com")
        (entry '((source . "Lipis/Polygons.excalidrawlib"))))
    (should (equal (excali--official-id entry) "lipis-polygons"))
    (should (equal (excali--official-library-url entry)
                   "https://libraries.excalidraw.com/libraries/Lipis/Polygons.excalidrawlib"))))

(ert-deftest excali-library-test-official-index ()
  "Fetching the index merges download counts and caches both."
  (excali-library-test--offline
   (let ((done nil))
     (excali-library-official-refresh (lambda () (setq done t)))
     (should done))
   (should (member "https://libraries.excalidraw.com/libraries.json" excali-library-test--fetched))
   (should (= (length excali--official-index) 3))
   (let ((polygons (excali-library-test--entry "Polygons")))
     (should (equal (alist-get 'source polygons) "lipis/polygons.excalidrawlib"))
     (should (> (alist-get 'downloads polygons) 0)))
   ;; A later session reads the cache instead of the network.
   (let ((before (mapcar (lambda (e) (alist-get 'downloads e)) excali--official-index)))
     (setq excali--official-index nil excali-library-test--fetched nil)
     (excali--official-load-cache)
     (should (equal (mapcar (lambda (e) (alist-get 'downloads e)) excali--official-index) before))
     (should-not excali-library-test--fetched))))

(ert-deftest excali-library-test-official-index-without-stats ()
  "Without download counts the index still loads, counting 0."
  (excali-library-test--offline
   (let* ((index (expand-file-name "libraries.json" excali-library-test--fixtures))
          (excali-library-test--fixtures (make-temp-file "excali-fix" t)))
     (copy-file index (expand-file-name "libraries.json" excali-library-test--fixtures))
     (excali-library-official-refresh)
     (should (= (length excali--official-index) 3))
     (should (cl-every (lambda (e) (= (alist-get 'downloads e) 0)) excali--official-index))
     (setq excali--official-index nil)
     (should (= (length (excali--official-load-cache)) 3))
     (delete-directory excali-library-test--fixtures t))))

(ert-deftest excali-library-test-official-list ()
  "The collection lists every library; `/' filters over names and items."
  (excali-library-test--offline
   (excali-library-browse-official)
   (with-current-buffer "*excali official libraries*"
     (should (derived-mode-p 'excali-library-official-mode))
     (should (= (length tabulated-list-entries) 3))
     (should (string-match-p "Polygons" (buffer-string)))
     (should (string-match-p "Information Architecture" (buffer-string)))
     (should (eq (key-binding "a") 'excali-library-official-add))
     ;; "cluster" is only an item name.
     (excali-library-official-filter "cluster")
     (should (equal (mapcar (lambda (row) (alist-get 'name (car row))) tabulated-list-entries)
                    '("Information Architecture")))
     (excali-library-official-filter "")
     (should (= (length tabulated-list-entries) 3)))))

(ert-deftest excali-library-test-official-add ()
  "`a' adds a whole library once; unnamed items take the library's name."
  (excali-library-test--offline
   (excali-library-official-refresh)
   (let ((polygons (excali-library-test--entry "Polygons"))
         (ia (excali-library-test--entry "Information Architecture")))
     (excali-library-official-add polygons)
     (should (member "https://libraries.excalidraw.com/libraries/lipis/polygons.excalidrawlib"
                     excali-library-test--fetched))
     (should (= (length (excali--library)) 6))
     (should (cl-every (lambda (i) (equal (alist-get 'name i) "Polygons")) (excali--library)))
     ;; Again: nothing new, though restoring gave new version nonces.
     (excali-library-official-add polygons)
     (should (= (length (excali--library)) 6))
     ;; Version 2 items keep their own names, and it all reaches the file.
     (excali-library-official-add ia)
     (should (= (length (excali--library)) 8))
     (should (equal (sort (mapcar (lambda (i) (alist-get 'name i)) (seq-take (excali--library) 2))
                          #'string<)
                    '("area" "cluster")))
     (setq excali--library nil excali--library-loaded nil)
     (should (= (length (excali--library)) 8)))))

(ert-deftest excali-library-test-official-preview ()
  "A preview replaces the list; `+' adds the item at point, `a' the rest."
  (excali-library-test--offline
   (excali-library-browse-official)
   (with-current-buffer "*excali official libraries*"
     (excali-library-official-filter "architecture")
     (let ((ia (excali-library-test--entry "Information Architecture")))
       (should (excali--official-goto ia))
       (excali-library-official-preview ia)
       ;; The same buffer now shows the library.
       (should (derived-mode-p 'excali-library-official-preview-mode))
       (should (string-match-p "Information Architecture" (buffer-string)))
       (should (string-match-p "0 of 2 in your library" (excali--official-preview-header)))
       (excali--library-finish-thumbnails)
       (let ((rows (excali-library-test--item-positions)))
         (should (= (length rows) 2))
         (goto-char (car rows))
         (excali-library-official-add-at-point)
         (should (= (length (excali--library)) 1))
         (setq rows (excali-library-test--item-positions))
         ;; The added row says so; the other does not.
         (should (string-match-p "✓ added" (buffer-substring (car rows) (cadr rows))))
         (should-not (string-match-p "✓" (buffer-substring (cadr rows) (point-max))))
         (excali-library-official-add-all)
         (should (= (length (excali--library)) 2))
         (excali-library-official-add-all)
         (should (= (length (excali--library)) 2))
         (should (string-match-p "2 of 2 in your library" (excali--official-preview-header))))
       ;; q goes back to the list, filtered as it was, at the same library,
       ;; now marked as wholly added.
       (excali-library-official-back)
       (should (derived-mode-p 'excali-library-official-mode))
       (should (equal excali--official-filter "architecture"))
       (should (equal (alist-get 'name (tabulated-list-get-id)) "Information Architecture"))
       (should (string-match-p "✓" (buffer-substring (line-beginning-position)
                                                      (line-end-position))))
       ;; Previewing again marks what the library holds.
       (excali-library-official-preview ia)
       (should (string-match-p "✓ in library" (buffer-string)))))))

(ert-deftest excali-library-test-thumbnails-cached-and-queued ()
  "Thumbnails start as placeholders, fill in, and come from the cache next time."
  (excali-library-test--with-file
   (let ((items (list (excali-library-test--item 0) (excali-library-test--item 40)))
         (rendered 0))
     (cl-letf* ((render (symbol-function 'excali--render-thumbnail))
                ((symbol-function 'excali--render-thumbnail)
                 (lambda (item file) (cl-incf rendered) (funcall render item file))))
       (with-temp-buffer
         (excali--insert-item-rows items)
         (should (= (length excali--thumbnail-queue) 2))
         (excali--library-finish-thumbnails)
         (should (null excali--thumbnail-queue))
         (should (= rendered 2))
         (should (eq (car-safe (get-text-property (point-min) 'display)) 'image)))
       ;; Next session: the files are there, nothing is rendered or queued.
       (clrhash excali--thumbnail-memo)
       (with-temp-buffer
         (excali--insert-item-rows items)
         (should (null excali--thumbnail-queue))
         (should (= rendered 2)))))))

(ert-deftest excali-library-test-http-body ()
  "Response bodies are split from headers and decoded as UTF-8."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n"
            (encode-coding-string "{\"name\":\"图\"}" 'utf-8))
    (should (equal (excali--http-body nil) "{\"name\":\"图\"}"))
    (should (equal (excali--http-body t) (encode-coding-string "{\"name\":\"图\"}" 'utf-8)))))

(provide 'excali-library-test)
;;; excali-library-test.el ends here
