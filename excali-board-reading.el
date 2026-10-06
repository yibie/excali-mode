;;; excali-board-reading.el --- Links and local inline images -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Code:
(require 'excali-board-vault)
(require 'excali-board-content)
(require 'excali-image)
(require 'browse-url)
(declare-function excali-board--reference "excali-board" (card))
(declare-function excali-board--resolve "excali-board" (reference))
(defvar excali-board--editor)
(declare-function excali-board-edit "excali-board-ui" ())
(declare-function excali-board-note-text "excali-board-note" (card))
(declare-function excali-board--scroll-values "excali-board-ui" (card))
(declare-function excali-board--local-point "excali-board-ui" (card point))
(declare-function excali-board-reference--id-index "excali-board-reference" ())
(declare-function excali-board--source-buffer "excali-board" (file))
(declare-function excali-native-board-hit "excali-module" (blocks width x y))

(defvar-local excali-board-reading--cache nil)
(defvar-local excali-board-reading--images nil)

(defun excali-board-reading--source-marker (card info)
  "Return a marker at CARD link INFO's original source, or nil for a note.
Verify the link at the recorded position before following it."
  (when-let* ((ref (excali-board--reference card)))
    (let ((marker (excali-board--resolve ref)))
      (set-marker marker (+ (marker-position marker) (1- (aref info 0)))
                  (marker-buffer marker))
      marker)))

(defun excali-board-reading--image-path (card info)
  "Resolve CARD's local image INFO using its actual source context."
  (let ((marker (excali-board-reading--source-marker card info)))
    (unwind-protect
        (excali-board-vault-resolve-file
         (org-link-unescape (aref info 2)) marker (equal (aref info 1) "attachment"))
      (when marker (set-marker marker nil)))))

(defun excali-board-reading--image (file)
  "Register bounded local FILE in the native image cache; return its ID.
Missing, unsupported or oversized files degrade to the block placeholder."
  (unless excali-board-reading--images
    (setq excali-board-reading--images (make-hash-table :test #'equal)))
  (let* ((attrs (and (not (file-remote-p file)) (file-attributes file)))
         (stamp (and attrs (list (file-attribute-modification-time attrs)
                                (file-attribute-size attrs))))
         (cached (gethash file excali-board-reading--images)))
    (if (equal stamp (car cached)) (cdr cached)
      (let ((id
             (when (and attrs (file-regular-p file)
                        (<= (file-attribute-size attrs) excali-image-max-file-size))
               (condition-case nil
                   (let* ((bytes (excali--read-image-file file))
                          (mime (excali--image-file-mime file bytes))
                          (key (concat "board-inline-" (secure-hash 'sha1 bytes))))
                     (when (excali--image-register key (excali--image-data-url mime bytes)) key))
                 (error nil)))))
        (puthash file (cons stamp id) excali-board-reading--images)
        id))))

(defun excali-board-reading-blocks (card)
  "Return CARD blocks with transient native image IDs.
Reparse old cached blocks from their source text for backward compatibility.
No image bytes or absolute resolved paths are added to the board document."
  (unless excali-board-reading--cache
    (setq excali-board-reading--cache (make-hash-table :test #'equal)))
  (when (> (hash-table-count excali-board-reading--cache) 128)
    (clrhash excali-board-reading--cache))
  (let* ((data (alist-get 'excaliBoardContent (excali--get card 'customData)))
         (text (alist-get 'sourceText data))
         (raw (alist-get 'blocks data))
         (key (list text raw))
         (blocks (or (gethash key excali-board-reading--cache)
                     (puthash key
                              (if (and (stringp text) (not (string-empty-p (string-trim text)))
                                       (seq-some (lambda (b) (< (length b) 3)) raw))
                                  (excali-board-content-blocks text) raw)
                              excali-board-reading--cache))))
    (vconcat
     (mapcar
      (lambda (block)
        (if (and (> (length block) 3) (vectorp (aref block 3)))
            (let ((copy (copy-sequence block)))
              (aset copy 3
                    (condition-case nil
                        (excali-board-reading--image
                         (excali-board-reading--image-path card (aref block 3)))
                      (error nil)))
              copy)
          block))
      blocks))))

(defun excali-board-reading-link-at (card point)
  "Return a text link at scene POINT on CARD, respecting clipping and scroll."
  (let* ((local (excali-board--local-point card point))
         (x (car local)) (y (cdr local))
         (blocks (excali-board-reading-blocks card)))
    (when (and (fboundp 'excali-native-board-hit)
               (<= 20 x (- (excali--get card 'width) 13))
               (<= 44 y (- (excali--get card 'height) 13)))
      (let* ((scroll (excali-board--scroll-values card))
             (hit (excali-native-board-hit blocks (excali--get card 'width)
                                          (+ (- x 20) (car scroll))
                                          (+ (- y 44) (cdr scroll)))))
        (when (and hit (>= (aref hit 1) 0))
          (let ((block (aref blocks (aref hit 0))))
            (when (> (length block) 2)
              (when-let* ((span (seq-find
                                (lambda (range)
                                  (<= (aref range 0) (aref hit 1) (1- (aref range 1))))
                                (aref block 2))))
                (aref span 2)))))))))

(defun excali-board-reading-open (card info)
  "Follow link INFO from CARD through a restricted, non-evaluating dispatcher.
Only http(s), local file/attachment, Org ID and fuzzy Org links are supported.
Never invoke shell/elisp/custom link handlers or auto-create target files."
  (let* ((board (current-buffer))
         (type (aref info 1))
         (path (org-link-unescape (aref info 2)))
         (search (aref info 3))
         (marker (excali-board-reading--source-marker card info))
         (temporary nil))
    (unwind-protect
        (progn
          ;; Positions are merely cache data: check against the current Org AST.
          (unless marker
            (setq temporary (generate-new-buffer " *board link validation*"))
            (with-current-buffer temporary
              (insert (excali-board-note-text card))
              (let ((org-mode-hook nil) (org-inhibit-startup t)) (org-mode))
              (setq marker (copy-marker (aref info 0)))))
          (with-current-buffer (marker-buffer marker)
            (save-restriction
              (widen)
              (save-excursion
                (goto-char marker)
                (let ((link (org-element-context)))
                  (unless (and (eq (org-element-type link) 'link)
                               (equal type (org-element-property :type link))
                               (equal (aref info 2) (org-element-property :path link))
                               (equal search (org-element-property :search-option link)))
                    (user-error "Link changed; refresh the card before opening"))))))
          (pcase type
            ((or "http" "https") (browse-url (concat type ":" (aref info 2))))
            ((or "file" "attachment")
             (let ((file (with-current-buffer board
                           (excali-board-vault-resolve-file
                            path (unless temporary marker) (equal type "attachment")))))
               (unless (file-exists-p file) (user-error "Link target is missing: %s" file))
               (let ((enable-local-eval nil) (enable-local-variables :safe))
                 (find-file-other-window file))
               (when search (org-link-search search))))
            ("id"
             (let ((matches (with-current-buffer board
                              (gethash path (excali-board-reference--id-index)))))
               (unless (= (length matches) 1)
                 (user-error "Org ID is missing or ambiguous in this vault"))
               (pop-to-buffer (excali-board--source-buffer (car matches)))
               (widen)
               (goto-char (or (org-find-entry-with-id path)
                              (user-error "Org ID disappeared")))))
            ("fuzzy"
             (if temporary
                 (let ((editor (with-current-buffer board
                                 (excali--select (list card))
                                 (excali-board-edit)
                                 excali-board--editor)))
                   (with-current-buffer editor (org-link-search path)))
               (pop-to-buffer (marker-buffer marker))
               (widen)
               (goto-char marker)
               (org-link-search path)))
            (_ (user-error "Link type %s is not enabled on the board" type))))
      (when marker (set-marker marker nil))
      (when (buffer-live-p temporary) (kill-buffer temporary)))))

(provide 'excali-board-reading)
;;; excali-board-reading.el ends here
