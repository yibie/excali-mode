;;; excali-board-note.el --- Independent Org cards -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; A note card owns its Org text until explicitly converted to a heading.
;; Conversion adds a source reference without replacing the shape or edges.
;;; Code:
(require 'excali)
(require 'org-id)
(require 'excali-board-content)
(declare-function excali-board--require-board "excali-board" ())
(declare-function excali-board--insert "excali-board" (reference content &optional note))
(declare-function excali-board--selected-card "excali-board" ())
(declare-function excali-board--source-buffer "excali-board" (file))
(declare-function excali-board--refresh-card "excali-board" (card))
(declare-function excali-board--watch-sources "excali-board" ())
(declare-function excali-board-save "excali-board" ())
(defvar excali-board--editing-board)
(defvar-local excali-board-note--id nil)
(defvar-local excali-board-note--expected nil)

(defun excali-board-note-p (card)
  "Whether CARD owns independent Org text."
  (let ((note (alist-get 'excaliBoardNote (excali--get card 'customData))))
    (and (listp note) (eql (alist-get 'version note) 1)
         (stringp (alist-get 'text note)))))

(defun excali-board-note-text (card)
  "Return CARD's independent Org text."
  (alist-get 'text (alist-get 'excaliBoardNote (excali--get card 'customData))))

(defun excali-board-note--set (card text)
  "Set CARD's owned TEXT and formatted cache, without committing history."
  ;; Parse before mutation so an error cannot discard the previous content.
  (let* ((blocks (excali-board-content-blocks text))
         (data (copy-tree (excali--get card 'customData) t))
         (label (excali--bound-text-of card)))
    (unless label (user-error "Card has no bound text"))
    (setf (alist-get 'excaliBoardNote data) `((version . 1) (text . ,text))
          (alist-get 'excaliBoardContent data)
          `((version . 1) (sourceText . ,text)
            (blocks . ,(if (= (length blocks) 0) [["<i>Empty note</i>" 0]] blocks))))
    (excali--put card 'customData data)
    (excali--touch card)
    (excali--set-text label text)))

;;;###autoload
(defun excali-board-new-note (&optional text)
  "Create an independent Org card containing TEXT in the current board.
The card is stored in the board file, not in an external Org file."
  (interactive)
  (require 'excali-board)
  (excali-board--require-board)
  (let* ((text (or text "* New note\nWrite your ideas here.\n"))
         (excali--history-hold t)
         (card (excali-board--insert nil "" t)))
    (condition-case err
        (excali-board-note--set card text)
      (error
       (setq excali--elements
             (delq (excali--bound-text-of card) (delq card excali--elements)))
       (excali--select nil)
       (signal (car err) (cdr err))))
    (let ((excali--history-hold nil)) (excali--commit))
    (excali--render)
    card))

(defun excali-board-note--sync ()
  "Synchronize this note editor to its owning board.
Reject stale editors rather than overwrite a converted/deleted/undone card."
  (when excali-board-note--id
    (let ((text (save-restriction
                  (widen)
                  (buffer-substring-no-properties (point-min) (point-max))))
          (id excali-board-note--id)
          (expected excali-board-note--expected)
          (board excali-board--editing-board))
      (unless (buffer-live-p board) (user-error "Owning board is no longer open"))
      (with-current-buffer board
        (let ((card (excali--live-element-by-id id)))
          (unless (and card (excali-board-note-p card))
            (user-error "Card was deleted or converted; keep this editor to recover its text"))
          (unless (equal expected (excali-board-note-text card))
            (user-error "Card changed elsewhere; editor text retained to avoid overwriting it"))
          (unless (equal text expected)
            (excali-board-note--set card text)
            (excali--commit)
            (excali--render)
            (excali--sync-views))))
      (setq excali-board-note--expected text)
      ;; The board now owns the unsaved changes, not a separate file buffer.
      (set-buffer-modified-p nil))))

(defun excali-board-note--changed (&rest _)
  "Update a note card after editing; retain the editor on conflicts."
  (condition-case err (excali-board-note--sync)
    (error (message "Org card: %s" (error-message-string err)))))

(defun excali-board-note--can-kill ()
  "Prevent losing unsynchronized editor text."
  (condition-case err (progn (excali-board-note--sync) t)
    (error (message "%s" (error-message-string err)) nil)))

(defun excali-board-note--editor (card)
  "Return an Org editing buffer for independent CARD."
  (let ((board (current-buffer))
        (id (excali--get card 'id))
        (text (excali-board-note-text card))
        (editor (generate-new-buffer " *Org note card*")))
    (with-current-buffer editor
      (insert text)
      (let ((org-mode-hook nil) (org-inhibit-startup t)) (org-mode))
      (buffer-enable-undo)
      (goto-char (point-min))
      (setq-local excali-board--editing-board board)
      (setq excali-board-note--id id excali-board-note--expected text)
      (setq-local header-line-format "Board note · C-c C-c: close · C-x C-s: save board")
      (add-hook 'after-change-functions #'excali-board-note--changed nil t)
      (add-hook 'kill-buffer-query-functions #'excali-board-note--can-kill nil t)
      (set-buffer-modified-p nil))
    editor))

(defun excali-board-note--nested-text (text)
  "Nest headings in TEXT one level deeper without altering code blocks."
  (with-temp-buffer
    (insert text)
    (let ((org-mode-hook nil) (org-inhibit-startup t)) (org-mode))
    (let (positions)
      (org-element-map (org-element-parse-buffer) 'headline
                       (lambda (h) (push (org-element-property :begin h) positions)))
      (dolist (pos (sort positions #'>)) (goto-char pos) (insert "*")))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun excali-board-note--heading-text (text &optional title)
  "Return TEXT as one Org subtree, reusing its leading heading.
With explicit TITLE, wrap TEXT as before.  Otherwise a plain note uses its
first nonblank line as title (retaining the body); an empty note uses New note."
  (with-temp-buffer
    (insert text)
    (let ((org-mode-hook nil) (org-inhibit-startup t)) (org-mode))
    (goto-char (point-min))
    (skip-chars-forward " \t\n\r")
    (if (and (null title) (org-at-heading-p))
        (let* ((headlines (org-element-map (org-element-parse-buffer) 'headline
                           #'identity))
               (first (car headlines))
               (level (org-element-property :level first))
               (end (org-element-property :end first)))
          ;; Keep the first subtree's hierarchy.  Later root siblings become
          ;; children, so the converted reference includes the whole note.
          (dolist (h (reverse headlines))
            (goto-char (org-element-property :begin h))
            (let* ((old (org-element-property :level h))
                   (new (max 2 (+ old (if (< (point) end) 1 2) (- level)))))
              (when (eq h first) (setq new 1))
              (delete-region (point) (+ (point) old))
              (insert (make-string new ?*))))
          (goto-char (point-min))
          (skip-chars-forward " \t\n\r")
          (delete-region (point-min) (point)))
      (unless title
        (setq title (if (eobp) "New note"
                      (string-trim (buffer-substring-no-properties
                                    (line-beginning-position) (line-end-position))))))
      (erase-buffer)
      (insert "* " title "\n" (excali-board-note--nested-text text)))
    (buffer-substring-no-properties (point-min) (point-max))))

;;;###autoload
(defun excali-board-note-to-heading (file &optional title)
  "Convert the selected independent card into a heading in local Org FILE.
Reuse the note's heading without prompting for a title.  Optional TITLE
explicitly wraps the note instead.  Keep geometry and connections.
Neither Org nor board is saved automatically.  Undo in the board restores the
independent card but deliberately does not delete the newly created Org text."
  (interactive
   (progn
     (require 'excali-board)
     (excali-board--require-board)
     (unless (excali-board-note-p (excali-board--selected-card))
       (user-error "Select an independent Org note"))
     (list (read-file-name "Destination Org file: " nil nil nil nil
                           (lambda (f) (or (file-directory-p f)
                                           (equal (file-name-extension f) "org")))))))
  (require 'excali-board)
  (excali-board--require-board)
  (unless (or (null title)
              (and (stringp title) (not (string-empty-p (string-trim title)))
                   (not (string-match-p "[\n\r]" title))))
    (user-error "Use a non-empty, single-line heading title"))
  (setq file (expand-file-name file))
  (when (or (file-remote-p file) (not (equal (file-name-extension file) "org")))
    (user-error "Choose a local .org file"))
  (let* ((board (current-buffer))
         (card (excali-board--selected-card))
         (label (excali--bound-text-of card)))
    (unless (excali-board-note-p card) (user-error "Select an independent Org note"))
    ;; Conversion while its editor is open would invalidate that editor.
    (when (and (boundp 'excali-board--editor)
               (buffer-live-p (symbol-value 'excali-board--editor)))
      (user-error "Close the card editor before converting"))
    (let* ((text (excali-board-note--heading-text (excali-board-note-text card) title))
           (source (excali-board--source-buffer file))
           (old-card (copy-tree card t)) (old-label (copy-tree label t)))
      (condition-case err
          (with-current-buffer source
            (unless (derived-mode-p 'org-mode) (user-error "Destination is not Org"))
            (barf-if-buffer-read-only)
            (save-restriction
              (widen)
              (save-excursion
                (atomic-change-group
                  (goto-char (point-max))
                  (unless (bolp) (insert "\n"))
                  (insert "\n")
                  (let ((start (point)) (id (org-id-new)))
                    (insert text)
                    (unless (bolp) (insert "\n"))
                    (goto-char start)
                    (org-entry-put nil "ID" id)
                    (with-current-buffer board
                      (let ((data (copy-tree (excali--get card 'customData) t)))
                        (setq data (assq-delete-all 'excaliBoardNote data))
                        (setf (alist-get 'excaliOrg data)
                              `((version . 1) (scope . "heading") (file . ,file) (id . ,id)))
                        (excali--put card 'customData data)
                        (excali--touch card)
                        (excali-board--refresh-card card))))))))
        (error
         (setcdr card (cdr old-card))
         (when label (setcdr label (cdr old-label)))
         (excali--invalidate-native card)
         (when label (excali--invalidate-native label))
         (signal (car err) (cdr err))))
      (excali-board--watch-sources)
      (excali--commit)
      (excali--render)
      (message "Converted; save the Org file and board separately")
      card)))

(provide 'excali-board-note)
;;; excali-board-note.el ends here
