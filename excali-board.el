;;; excali-board.el --- Org boards derived from excali-mode -*- lexical-binding: t; -*-
;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; An optional derived mode, using ordinary .excalidraw scenes with a
;; versioned excaliBoard document marker and customData.excaliOrg references.
;; Org owns content.  Rich subtree blocks are rendered inside fixed cards.
;; No advice or global drawing key changes are used.
;;; Code:
(require 'excali)
(require 'org-id)
(require 'excali-board-content)
(require 'excali-board-ui)
(require 'excali-board-note)
(require 'excali-board-vault)
(require 'excali-board-reference)
(require 'excali-board-organize)
(declare-function excali-board--native "excali-board-ui" (element native))
(declare-function excali-board--layout "excali-board-ui" (card text))
(declare-function excali-board--close-editor "excali-board-ui" ())
(declare-function excali-board-edit "excali-board-ui" ())
(defvar excali-board--native-cache)

(defgroup excali-board nil
  "Org reference boards on the excali canvas."
  :group 'excali)

(defcustom excali-board-sync-delay 0.35
  "Idle seconds before edited Org sources update their boards."
  :type 'number :group 'excali-board)

(defvar excali-board--last-board nil "Most recently used board buffer.")
(defvar-local excali-board--errors nil "Alist of card IDs and refresh errors.")
(defvar-local excali-board--origin-file nil
  "Original drawing protected from overwrite after explicit copy/migration.")
(defvar-local excali-board--timer nil "Source buffer's pending refresh timer.")

(defun excali-board--validate-document (doc)
  "Reject DOC with an unsupported board schema before opening it."
  (unless (assq 'excaliBoard doc)
    (user-error "Not an Org board; open as a drawing and use excali-board-from-scene"))
  (let ((meta (alist-get 'excaliBoard doc)))
    (unless (and (listp meta) (eql (alist-get 'version meta) 1))
      (user-error "Unsupported excali board version; file left unchanged"))))

(defun excali-board--validate-reference (ref)
  "Check the version and scope of an Org reference REF."
  (unless (and (listp ref) (eql (alist-get 'version ref) 1)
               (equal (alist-get 'scope ref) "heading"))
    (user-error "Unsupported Org card reference version or scope")))

(defun excali-board--reference (card)
  "Return CARD's Org reference, if any."
  (let ((data (excali--get card 'customData)))
    (and (listp data) (alist-get 'excaliOrg data))))

(defun excali-board--require-board ()
  "Require the current buffer to be an Org board."
  (unless (derived-mode-p 'excali-board-mode)
    (user-error "Use this command in an excali board")))

(defun excali-board--source-buffer (file)
  "Visit local FILE without evaluating its local variables."
  (when (file-remote-p file) (user-error "Remote Org sources are not supported"))
  (or (find-buffer-visiting file)
      (let ((enable-local-eval nil)
            (enable-local-variables :safe))
        (find-file-noselect file))))

(defun excali-board--padding (card)
  "Return the preview inset for Org CARD, leaving normal shapes unchanged."
  (and (excali-board--reference card) 20))

(defun excali-board--heading ()
  "Read the current Org heading and a short, unrendered body excerpt."
  (unless (derived-mode-p 'org-mode) (user-error "Not an Org buffer"))
  (save-excursion
    (org-back-to-heading t)
    (let ((title (org-get-heading t nil t t))
          (end (save-excursion (outline-next-heading) (point))))
      (org-end-of-meta-data t)
      (concat (truncate-string-to-width title 70 nil nil "…")
              (let ((body (string-trim
                           (replace-regexp-in-string
                            "[ \t\n\r]+" " "
                            (buffer-substring-no-properties (min (point) end) end)))))
                (unless (string-empty-p body)
                  (concat "\n\n" (truncate-string-to-width body 140 nil nil "…"))))))))

(defun excali-board--reference-buffer (reference)
  "Resolve REFERENCE's local Org file, preferring unsaved visiting buffers."
  (excali-board--validate-reference reference)
  (let ((file (alist-get 'file reference)) (id (alist-get 'id reference)))
    (unless (and (stringp file) (file-name-absolute-p file)
                 (not (file-remote-p file)) (stringp id) (not (string-empty-p id)))
      (user-error "Invalid or non-local Org card reference"))
    (let ((buffer (or (find-buffer-visiting file)
                      (and (file-exists-p file) (excali-board--source-buffer file)))))
      (unless buffer (user-error "Org source file is missing: %s" file))
      (unless (with-current-buffer buffer (derived-mode-p 'org-mode))
        (user-error "Source is not an Org buffer"))
      (excali-board-reference--check-disk buffer)
      buffer)))

(defun excali-board--resolve (reference)
  "Resolve REFERENCE by ID, including unsaved edits and narrowed sources.
Never silently fall back to a different heading."
  (with-current-buffer (excali-board--reference-buffer reference)
    (save-restriction
      (widen)
      (let ((pos (org-find-entry-with-id (alist-get 'id reference))))
        (unless pos
          (user-error "Org heading ID not found: %s" (alist-get 'id reference)))
        (copy-marker pos)))))

(defun excali-board--selected-card ()
  "Return the single selected referenced card, accepting its bound label."
  (unless (derived-mode-p 'excali-board-mode) (user-error "Not an excali canvas"))
  (let ((cards (delete-dups
                (delq nil
                      (mapcar (lambda (e)
                                (let ((card (if (equal (excali--get e 'type) "text")
                                                (excali--container-of e) e)))
                                  (and card (or (excali-board--reference card) (excali-board-note-p card)) card)))
                              excali--selection)))))
    (unless (= (length cards) 1) (user-error "Select one Org card"))
    (car cards)))

(defun excali-board--insert (reference content &optional note)
  "Insert a card with REFERENCE and CONTENT; NOTE means independent text."
  (let* ((x (+ 60.0 (* 420 (length (seq-filter (lambda (e) (or (excali-board--reference e) (excali-board-note-p e)))
                                               excali--elements)))))
         (card (excali--make-element
                "rectangle" x 60.0 '(width . 360.0) '(height . 220.0)
                '(backgroundColor . "#ffffff") '(fillStyle . "solid")
                '(strokeColor . "#b8bec9") '(strokeWidth . 1) '(roughness . 0)
                '(roundness . ((type . 3) (value . 8)))
                (cons 'customData (if note '((excaliBoardNote . ((version . 1) (text . ""))))
                                    (list (cons 'excaliOrg reference)))))))
    (setq excali--elements (append excali--elements (list card)))
    (let ((label (excali--add-bound-text card '(fontSize . 20)
                                         '(fontFamily . 2) '(textAlign . "left")
                                         '(verticalAlign . "top")
                                         '(strokeColor . "#303446"))))
      (excali--set-text label content))
    (excali--select (list card))
    (excali--commit)
    card))

(defun excali-board-visit ()
  "Visit the selected card's Org heading in another window."
  (interactive)
  (when (excali-board-note-p (excali-board--selected-card))
    (user-error "Independent note: use excali-board-note-to-heading to create a source"))
  (let ((marker (excali-board--resolve
                 (excali-board--reference
                  (excali-board--selected-card)))))
    (setq excali-board--last-board (current-buffer))
    (excali-board--watch-sources)
    (pop-to-buffer
     (marker-buffer marker)
     '((display-buffer-reuse-window display-buffer-pop-up-window
				    display-buffer-use-some-window)
       (inhibit-same-window . t)))
    (widen)
    (goto-char marker)
    (org-fold-show-context)
    (org-fold-show-entry)
    (set-marker marker nil)))

(defun excali-board--refresh-card (card)
  "Update CARD's complete subtree cache without changing its dimensions."
  (let* ((label (excali--bound-text-of card))
         (marker (excali-board--resolve (excali-board--reference card)))
         (content
          (unwind-protect
              (with-current-buffer (marker-buffer marker)
                (save-restriction
                  (widen)
                  (save-excursion
                    (goto-char marker)
                    (list (excali-board--heading)
                          (buffer-substring-no-properties
                           (line-beginning-position)
                           (save-excursion (org-end-of-subtree t t) (point)))))))
            (set-marker marker nil)))
         (data (copy-tree (excali--get card 'customData) t))
         (old (alist-get 'excaliBoardContent data)))
    (unless label (user-error "Card has no bound text; no changes made"))
    (unless (equal (cadr content) (alist-get 'sourceText old))
      (setf (alist-get 'excaliBoardContent data)
            `((version . 1) (sourceText . ,(cadr content))
              (blocks . ,(excali-board-content-blocks (cadr content)))))
      (excali--put card 'customData data)
      (excali--touch card)
      (excali--set-text label (car content))
      t)))

(defun excali-board--boards ()
  "Return live board buffers."
  (seq-filter (lambda (buffer)
                (with-current-buffer buffer (derived-mode-p 'excali-board-mode)))
              (buffer-list)))

(defun excali-board--read-target ()
  "Choose an existing board buffer, a saved board, or a new board."
  (let* ((choices (append
                   '(("[New board]" . new) ("[Open board file...]" . open))
                   (mapcar (lambda (b) (cons (buffer-name b) b))
                           (excali-board--boards))))
         (default (and (memq excali-board--last-board (excali-board--boards))
                       (buffer-name excali-board--last-board)))
         (choice (cdr (assoc (completing-read "Target board: " choices nil t
                                              nil nil (or default "[New board]"))
                             choices))))
    (pcase choice
      ('new (excali-board-new))
      ('open (excali-board-open (read-file-name "Board file: " nil nil t)))
      (_ choice))))

(defun excali-board--capture-heading (marker)
  "Return (REFERENCE CONTENT) for the heading at MARKER.
Assign an ID only now, after target selection succeeded; never save Org."
  (with-current-buffer (marker-buffer marker)
    (unless (and (derived-mode-p 'org-mode) buffer-file-name
                 (not (file-remote-p buffer-file-name)))
      (user-error "Use a local, file-backed Org heading"))
    (save-excursion
      (goto-char marker)
      (org-back-to-heading t)
      (unless (org-entry-get nil "ID") (barf-if-buffer-read-only))
      (list `((version . 1) (scope . "heading")
              (file . ,(expand-file-name buffer-file-name))
              (id . ,(org-id-get-create)))
            (excali-board--heading)))))

(defun excali-board--add-marker (board marker)
  "Add MARKER's heading to BOARD without saving either file."
  (unless (and (buffer-live-p board)
               (with-current-buffer board (derived-mode-p 'excali-board-mode)))
    (user-error "Target is not an excali board"))
  (pcase-let ((`(,reference ,content) (excali-board--capture-heading marker)))
    (pop-to-buffer board)
    (excali-board--refresh-card (excali-board--insert reference content))
    (excali--commit)
    (excali-board--watch-sources)
    (excali--render)
    (setq excali-board--last-board board)
    (message "Org card added; save Org (including its ID) and board separately")
    board))

;;;###autoload
(defun excali-board-add-heading (&optional board)
  "Add the current Org heading to BOARD.
Interactively choose a live board, a saved board, or a new board.
Creating an ID modifies the Org buffer but does not save it."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) buffer-file-name
               (not (file-remote-p buffer-file-name)))
    (user-error "Run this at a heading in a local Org file"))
  (let ((marker (save-excursion (org-back-to-heading t) (point-marker))))
    (unwind-protect
        (excali-board--add-marker (or board (excali-board--read-target)) marker)
      (set-marker marker nil))))

(defun excali-board--read-heading (file)
  "Choose a heading in local Org FILE, returning its marker.
Outline paths and line numbers disambiguate duplicate heading names."
  (when (file-remote-p file) (user-error "Remote Org sources are not supported"))
  (unless (file-exists-p file) (user-error "Org file does not exist: %s" file))
  (with-current-buffer (excali-board--source-buffer file)
    (unless (derived-mode-p 'org-mode) (user-error "Not an Org file"))
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (let (choices)
          (while (re-search-forward org-heading-regexp nil t)
            (beginning-of-line)
            (push (cons (format "%s  [line %d]"
                                (string-join (org-get-outline-path t) " / ")
                                (line-number-at-pos))
                        (point))
                  choices)
            (forward-line 1))
          (unless choices (user-error "No headings in this Org file"))
          (copy-marker
           (cdr (assoc (completing-read "Org heading: " (reverse choices) nil t)
                       choices))))))))

;;;###autoload
(defun excali-board-insert-heading (file)
  "Choose and insert an Org heading from FILE in the current board."
  (interactive (progn (excali-board--require-board)
                      (list (excali-board-vault-read-file "Org file: " 'org))))
  (excali-board--require-board)
  (let ((board (current-buffer))
        (marker (excali-board--read-heading (expand-file-name file))))
    (unwind-protect (excali-board--add-marker board marker)
      (set-marker marker nil))))

(defun excali-board-refresh ()
  "Refresh the selected Org card, or all cards when none is selected."
  (interactive)
  (excali-board--require-board)
  (if (or (null excali--selection)
          (excali-board-media-data (excali--single-selection)))
      (progn (excali-board-refresh-all) (excali-board-media-refresh))
    (let ((card (excali-board--selected-card)))
      (when (if (excali-board-note-p card)
                (progn (excali-board-note--set card (excali-board-note-text card)) t)
              (excali-board--refresh-card card))
        (excali--commit)
        (excali--render)
        (excali--sync-views))
      ;; Image files may change even when Org text is unchanged.
      (excali--render)
      (excali--sync-views)
      (setq excali-board--errors
            (assoc-delete-all (excali--get card 'id) excali-board--errors))
      (force-mode-line-update)
      (message "Org card refreshed"))))

(defun excali-board-refresh-all ()
  "Refresh all live Org cards; keep cached content for unavailable sources."
  (interactive)
  (excali-board--require-board)
  (let (changed errors)
    (dolist (card excali--elements)
      (when (and (not (excali--get card 'isDeleted))
                 (excali-board--reference card))
        (condition-case err
            (when (excali-board--refresh-card card) (setq changed t))
          (error (push (cons (excali--get card 'id) (error-message-string err))
                       errors)))))
    (setq excali-board--errors (nreverse errors))
    (when changed
      (excali--commit)
      (excali--render)
      (excali--sync-views))
    (force-mode-line-update)
    (when (called-interactively-p 'interactive)
      (unless changed (excali--render) (excali--sync-views))
      (if errors
          (message "Org card errors: %s" (string-join (mapcar #'cdr errors) "; "))
        (message "All Org cards are up to date")))))

(defun excali-board--references-file-p (file)
  "Whether the current board has a live reference to FILE."
  (seq-some (lambda (card)
              (and (not (excali--get card 'isDeleted))
                   (let ((ref (excali-board--reference card)))
                     (and (listp ref) (equal file (alist-get 'file ref))))))
            excali--elements))

(defun excali-board--cancel-timer ()
  "Cancel the current source buffer's pending refresh."
  (when (timerp excali-board--timer) (cancel-timer excali-board--timer))
  (setq excali-board--timer nil))

(defun excali-board--sync-source (source)
  "Update live boards referencing SOURCE, without changing selected windows."
  (when (buffer-live-p source)
    (with-current-buffer source (setq excali-board--timer nil))
    (dolist (board (excali-board--boards))
      (with-current-buffer board
        (when (excali-board--references-file-p
               (buffer-local-value 'buffer-file-name source))
          (excali-board-refresh-all))))))

(defun excali-board--schedule (&rest _)
  "Debounce changes in a watched Org source."
  (excali-board--cancel-timer)
  (setq excali-board--timer
        (run-with-idle-timer excali-board-sync-delay nil
                             #'excali-board--sync-source (current-buffer))))

(defun excali-board--watch-source ()
  "Watch edits and reverts in the current source buffer."
  (add-hook 'after-change-functions #'excali-board--schedule nil t)
  (add-hook 'after-revert-hook #'excali-board--schedule nil t)
  (add-hook 'kill-buffer-hook #'excali-board--cancel-timer nil t))

(defun excali-board--watch-sources ()
  "Watch available source buffers without changing the board selection."
  (dolist (card excali--elements)
    (when (and (not (excali--get card 'isDeleted))
               (excali-board--reference card))
      (condition-case nil
          ;; Watch the file even if its heading ID is temporarily missing:
          ;; restoring that ID should recover the card without reopening it.
          (with-current-buffer
              (excali-board--reference-buffer (excali-board--reference card))
            (excali-board--watch-source))
        (error nil)))))

(defun excali-board--source-opened ()
  "Reattach watches when a referenced Org source is reopened."
  (when (and (derived-mode-p 'org-mode) buffer-file-name)
    (let ((file buffer-file-name))
      (when (seq-some (lambda (board)
			(with-current-buffer board
                          (excali-board--references-file-p file)))
                      (excali-board--boards))
        (excali-board--watch-source)
        (excali-board--schedule)))))
(add-hook 'find-file-hook #'excali-board--source-opened)

(defun excali-board--prune-watches ()
  "Remove unused source hooks after a board is killed or changes mode."
  (let ((boards (delq (current-buffer) (excali-board--boards))))
    (dolist (source (buffer-list))
      (with-current-buffer source
        (when (memq #'excali-board--schedule after-change-functions)
          (let ((file buffer-file-name))
            (unless (seq-some
                     (lambda (board)
                       (with-current-buffer board (excali-board--references-file-p file)))
                     boards)
              (excali-board--cancel-timer)
              (remove-hook 'after-change-functions #'excali-board--schedule t)
              (remove-hook 'after-revert-hook #'excali-board--schedule t)
              (remove-hook 'kill-buffer-hook #'excali-board--cancel-timer t))))))))

(defun excali-board--after-open ()
  "Initialize source watches after document state and view are available."
  (setq excali-board--last-board (current-buffer))
  (excali-board--watch-sources)
  (excali-board-refresh-all))

(defun excali-board--before-save ()
  "Validate the schema and protect a copied drawing's original file."
  (excali-board--validate-document excali--doc)
  (when (and excali-board--origin-file excali--file
             (or (equal (expand-file-name excali--file)
                        (expand-file-name excali-board--origin-file))
                 (and (file-exists-p excali--file)
                      (file-equal-p excali--file excali-board--origin-file))))
    (setq excali--file nil)
    (user-error "Save this board under a different name; original protected")))

(defun excali-board-save ()
  "Save this board, including its versioned marker, without saving Org."
  (interactive)
  (excali-board--require-board)
  (excali-board--validate-document excali--doc)
  (excali-save)
  (set-buffer-modified-p nil))

(defun excali-board-return ()
  "Return to the most recently used board, refreshing source content."
  (interactive)
  (unless (memq excali-board--last-board (excali-board--boards))
    (user-error "No live Org board"))
  (pop-to-buffer excali-board--last-board)
  (excali-board-refresh-all))

(defun excali-board-double-click (event)
  "At EVENT, open a file card, edit an Org card or extend a free arrow."
  (interactive "e")
  (excali--select-event-view event)
  (let* ((point (excali--event-scene-xy event))
         (hit (excali--hit point))
         (card (if (equal (excali--get hit 'type) "text")
                   (excali--container-of hit) hit))
         (arrow (if (equal (excali--get hit 'type) "arrow") hit
                  (excali--single-selection)))
         (end (excali-board-organize-free-end-at arrow point)))
    (cond
     (end
      (excali--await-release)
      (excali--select (list arrow))
      (excali-board-note-at-arrow-end end))
     ((or (excali-board-media-data card)
          (alist-get 'excaliBoardAttachment (excali--get card 'customData)))
      (excali--await-release)
      (excali--select (list card))
      (excali--render)
      (excali-board-open-attachment))
     ((and card (or (excali-board--reference card) (excali-board-note-p card)))
      (excali--await-release)
      (excali--select (list card))
      (excali-board-edit))
     (t (excali-double-click event)))))

(defvar excali-board-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map excali-mode-map)
    (define-key map [double-down-mouse-1] #'excali-board-double-click)
    ;; Image hot spots prefix mouse keys; retain child-mode dispatch there.
    (let ((canvas (make-sparse-keymap)))
      (set-keymap-parent canvas map)
      (define-key canvas [t] #'ignore)
      (define-key map [excali-canvas] canvas))
    (define-key map (kbd "C-c C-i") #'excali-board-insert-heading)
    (define-key map (kbd "C-c C-o") #'excali-board-visit)
    (define-key map (kbd "C-c C-r") #'excali-board-refresh)
    (define-key map (kbd "C-x C-s") #'excali-board-save)
    (define-key map (kbd "s-s") #'excali-board-save)
    map))

(define-derived-mode excali-board-mode excali-mode "Excali-Board"
  "Edit an Org board using the excali drawing editor.
Use \\[excali-board-new] or \\[excali-board-open] to enter this mode.
Do not activate it directly on a live drawing: mode initialization resets
buffer-local scene state.  To copy a drawing, use \\[excali-board-from-scene].
Org subtrees are formatted and clipped inside independently sized cards."
  :interactive nil
  :group 'excali-board
  (unless (and (fboundp 'excali-native-board-measure)
               (fboundp 'excali-native-board-hit))
    (user-error "Rebuild the native module with make and restart Emacs"))
  (excali-board--setup-keys)
  (excali-board-reference--start)
  (add-hook 'kill-buffer-hook #'excali-board-reference--stop nil t)
  (add-hook 'change-major-mode-hook #'excali-board-reference--stop nil t)
  (setq-local excali-native-element-function #'excali-board--native)
  (setq-local excali-text-layout-function #'excali-board--layout)
  (setq-local excali-board--native-cache (make-hash-table :test #'eq :weakness 'key))
  (setq-local excali-text-padding-function #'excali-board--padding)
  (setq mode-line-process
        (list mode-line-process
              '(:eval (when excali-board--errors
                        (format " [Org: %d error(s)]" (length excali-board--errors))))))
  (add-hook 'excali-after-open-hook #'excali-board--after-open nil t)
  (add-hook 'excali-before-save-hook #'excali-board--before-save nil t)
  (add-hook 'kill-buffer-hook #'excali-board--close-editor nil t)
  (add-hook 'kill-buffer-hook #'excali-board--prune-watches nil t)
  (add-hook 'change-major-mode-hook #'excali-board--close-editor nil t)
  (add-hook 'change-major-mode-hook #'excali-board--prune-watches nil t))

(defun excali-board--new-window ()
  "Return a right-hand window for a new board, preserving the source pane.
Reuse a board already to the right, including the selected board window
when it has a left neighbor.  Otherwise split the selected window equally."
  (let* ((origin (selected-window))
         (right-edge (nth 2 (window-edges origin)))
         (board-window-p
          (lambda (window)
            (and (not (window-dedicated-p window))
                 (not (window-parameter window 'window-side))
                 (with-current-buffer (window-buffer window)
                   (derived-mode-p 'excali-board-mode))))))
    (or (and (funcall board-window-p origin)
             (window-in-direction 'left origin)
             origin)
        (seq-find (lambda (window)
                    (and (>= (car (window-edges window)) right-edge)
                         (funcall board-window-p window)))
                  (window-list nil 'nomini))
        (split-window origin nil 'right))))

;;;###autoload
(defun excali-board-new (&optional file)
  "Create and save a new board FILE inside the configured vault.
Require the current source directory to be inside the vault.  Prompt only
for a filename, never a vault.  Display the board on the right, preserving
Org on the left.  Existing files are never overwritten."
  (interactive)
  (pcase-let* ((`(,root . ,directory) (excali-board-vault--creation-context))
               (file (excali-board-vault--new-file
                      (or file (read-file-name "New board: " directory nil nil
                                               "untitled.excalidraw")) root directory))
               (doc (excali--empty-doc))
               (configuration (current-window-configuration))
               (board nil))
    (setf (alist-get 'excaliBoard doc) `((version . 1) (vaultRoot . ,root)))
    (condition-case err
        (progn
          (select-window (excali-board--new-window))
          (setq board (excali--open doc file
                                   (format "*board %s*" (file-name-nondirectory file))))
          (with-current-buffer board
            (setq default-directory (file-name-directory file))
            (run-hooks 'excali-before-save-hook)
            ;; Exclusive creation also guards against a file appearing after
            ;; validation.  Never overwrite an existing drawing.
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region (excali--serialize-doc excali--doc excali--elements)
                            nil file nil 'silent nil 'excl))
            (set-buffer-modified-p nil))
          board)
      ((error quit)
       (when (buffer-live-p board)
         (with-current-buffer board (set-buffer-modified-p nil))
         (kill-buffer board))
       (set-window-configuration configuration)
       (signal (car err) (cdr err))))))

;;;###autoload
(defun excali-board-open (file)
  "Open an existing board FILE without converting ordinary drawings."
  (interactive "fOrg board (.excalidraw): ")
  (let* ((file (expand-file-name file))
         (doc (excali--read-file file)))
    (excali-board--validate-document doc)
    (let ((configuration (current-window-configuration)))
      (condition-case err
          (progn
            (select-window (excali-board--new-window))
            (excali--open doc file (format "*board %s*" (file-name-nondirectory file))))
        ((error quit)
         (set-window-configuration configuration)
         (signal (car err) (cdr err)))))))

(defun excali-board--migrate-document (doc)
  "Return a board copy of DOC, migrating known prototype references.
No source document, Org file, or on-disk file is changed."
  (let ((copy (copy-tree doc t)))
    (when (assq 'excaliBoard copy) (excali-board--validate-document copy))
    (setf (alist-get 'excaliBoard copy) '((version . 1)))
    (seq-doseq (card (alist-get 'elements copy))
      (let* ((data (excali--get card 'customData))
             (old (and (listp data) (alist-get 'excaliOrgPrototype data))))
        (when old
          (unless (and (listp old) (eql (alist-get 'version old) 1))
            (user-error "Unsupported prototype reference; original left unchanged"))
          (when (alist-get 'excaliOrg data)
            (user-error "Conflicting Org references; original left unchanged"))
          (setf (alist-get 'scope old) "heading")
          (setq data (assq-delete-all 'excaliOrgPrototype data))
          (setf (alist-get 'excaliOrg data) old)
          (excali--put card 'customData data))))
    copy))

;;;###autoload
(defun excali-board-from-scene ()
  "Create a new unsaved board copy of the current drawing or prototype.
The original scene buffer/file is never converted in place.
Save the copy under a different file name with \\[excali-board-save]."
  (interactive)
  (unless (derived-mode-p 'excali-mode) (user-error "Open an excali scene first"))
  (let ((doc (copy-tree excali--doc t))
        (origin excali--file))
    (setf (alist-get 'elements doc) (vconcat excali--elements))
    (let ((board (excali--open (excali-board--migrate-document doc)
                               nil "*excali board copy*")))
      (with-current-buffer board
        (setq excali-board--origin-file origin)
        (set-buffer-modified-p t))
      board)))

(provide 'excali-board)
;;; excali-board.el ends here
