;;; excali-org-prototype.el --- Small Org/card connectivity experiment -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Opt-in experiment, not a replacement for ob-excali.  Org owns content;
;; .excalidraw customData owns the reference.  Live source edits refresh cards; no file relocation, automatic saves,
;; or source deletion is implemented.
;;; Code:
(require 'excali)
(require 'org-id)

(defvar excali-org-prototype--canvas nil
  "Most recently used experimental canvas buffer.")

(defun excali-org-prototype--heading ()
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

(defun excali-org-prototype--reference (card)
  "Read CARD's experimental Org reference."
  (alist-get 'excaliOrgPrototype (excali--get card 'customData)))

(defun excali-org-prototype--resolve (reference)
  "Resolve REFERENCE by ID in its local source file, returning a marker.
Prefer the live visiting buffer, including unsaved edits and narrowed buffers.
Never follow a remote path or silently fall back to another heading."
  (let ((file (alist-get 'file reference)) (id (alist-get 'id reference)))
    (unless (and (stringp file) (file-name-absolute-p file)
                 (not (file-remote-p file)) (stringp id) (not (string-empty-p id)))
      (user-error "Invalid or non-local Org card reference"))
    (let ((buffer (or (find-buffer-visiting file)
                      (and (file-exists-p file) (find-file-noselect file)))))
      (unless buffer (user-error "Org source file is missing: %s" file))
      (with-current-buffer buffer
        (unless (derived-mode-p 'org-mode) (user-error "Source is not an Org buffer"))
        (save-restriction
          (widen)
          (let ((pos (org-find-entry-with-id id)))
            (unless pos (user-error "Org heading ID not found: %s" id))
            (copy-marker pos)))))))

(defun excali-org-prototype--selected-card ()
  "Return the single selected referenced card, accepting its bound label."
  (unless (derived-mode-p 'excali-mode) (user-error "Not an excali canvas"))
  (let ((cards (delete-dups
                (delq nil
                      (mapcar (lambda (e)
                                (let ((card (if (equal (excali--get e 'type) "text")
                                                (excali--container-of e) e)))
                                  (and card (excali-org-prototype--reference card) card)))
                              excali--selection)))))
    (unless (= (length cards) 1) (user-error "Select one Org card"))
    (car cards)))

(defun excali-org-prototype--insert (reference content)
  "Insert a referenced card with CONTENT in the current canvas."
  (let* ((x (+ 60.0 (* 420 (length (seq-filter #'excali-org-prototype--reference
                                             excali--elements)))))
         (card (excali--make-element
                "rectangle" x 60.0 '(width . 360.0) '(height . 220.0)
                '(backgroundColor . "#ffffff") '(fillStyle . "solid")
                '(strokeColor . "#b8bec9") '(strokeWidth . 1) '(roughness . 0)
                '(roundness . ((type . 3) (value . 8)))
                (cons 'customData (list (cons 'excaliOrgPrototype reference))))))
    (setq excali--elements (append excali--elements (list card)))
    (let ((label (excali--add-bound-text card '(fontSize . 20)
                                        '(fontFamily . 2) '(textAlign . "left")
                                        '(verticalAlign . "top")
                                        '(strokeColor . "#303446"))))
      (excali--set-text label content))
    (excali--select (list card))
    (excali--commit)
    card))

;;;###autoload
(defun excali-org-prototype-add-heading (&optional canvas)
  "Add the current Org heading to CANVAS, or the last experimental canvas.
Create a new canvas when none is live.  Add an ID to the Org heading if
needed, but do not save the source automatically.  Only local files work."
  (interactive)
  (unless (derived-mode-p 'org-mode) (user-error "Run this at an Org heading"))
  (unless (and buffer-file-name (not (file-remote-p buffer-file-name)))
    (user-error "Use a local, file-backed Org buffer"))
  (org-back-to-heading t)
  (unless (org-entry-get nil "ID") (barf-if-buffer-read-only))
  (let ((content (excali-org-prototype--heading))
        (reference `((version . 1) (file . ,(expand-file-name buffer-file-name))
                     (id . ,(org-id-get-create))))
        (target (or canvas excali-org-prototype--canvas)))
    (unless (and (buffer-live-p target)
                 (with-current-buffer target (derived-mode-p 'excali-mode)))
      (setq target (excali-new)))
    (pop-to-buffer target)
    (excali-org-prototype-mode 1)
    (excali-org-prototype--insert reference content)
    (excali-org-prototype--watch-sources)
    (excali--render)
    (message "Org card added; save Org and canvas separately")
    target))

(defun excali-org-prototype-visit ()
  "Visit the selected card's Org heading in another window."
  (interactive)
  (let ((marker (excali-org-prototype--resolve
                 (excali-org-prototype--reference
                  (excali-org-prototype--selected-card)))))
    (setq excali-org-prototype--canvas (current-buffer))
    (excali-org-prototype--watch-sources)
    (pop-to-buffer
     (marker-buffer marker)
     '((display-buffer-reuse-window display-buffer-pop-up-window
        display-buffer-use-some-window)
       (inhibit-same-window . t)))
    (widen)
    (goto-char marker)
    (org-show-context)
    (org-show-entry)
    (set-marker marker nil)))

(defun excali-org-prototype-refresh ()
  "Refresh the selected card from Org without changing its position or style.
Missing sources signal an error and leave the existing card unchanged."
  (interactive)
  (when (excali-org-prototype--refresh-card
         (excali-org-prototype--selected-card))
    (excali--commit)
    (excali--render))
  (message "Org card refreshed (source → canvas only)"))

(defun excali-org-prototype--refresh-card (card)
  "Update CARD from its source; return non-nil only if text changed."
  (let* ((label (excali--bound-text-of card))
         (marker (excali-org-prototype--resolve
                  (excali-org-prototype--reference card)))
         (content (unwind-protect
                      (with-current-buffer (marker-buffer marker)
                        (save-restriction
                          (widen)
                          (save-excursion (goto-char marker)
                                          (excali-org-prototype--heading))))
                    (set-marker marker nil))))
    (unless label (user-error "Card has no bound text; no changes made"))
    (unless (equal content (excali--get label 'originalText))
      (excali--set-text label content)
      t)))

(defvar-local excali-org-prototype--sync-error nil
  "Last source refresh error, or nil.")
(defvar-local excali-org-prototype--timer nil
  "Pending source edit refresh timer.")

(defun excali-org-prototype-refresh-all ()
  "Refresh all Org cards, preserving cards whose sources are unavailable."
  (interactive)
  (let (changed errors)
    (dolist (card excali--elements)
      (when (and (not (excali--get card 'isDeleted))
                 (excali-org-prototype--reference card))
        (condition-case err
            (when (excali-org-prototype--refresh-card card) (setq changed t))
          (error (push (error-message-string err) errors)))))
    (setq excali-org-prototype--sync-error errors)
    (when changed
      (excali--commit)
      (excali--render)
      (excali--sync-views))
    (when (and errors (called-interactively-p 'interactive))
      (message "Org cards: %s" (string-join errors "; ")))))

(defun excali-org-prototype--sync-source (source)
  "Refresh enabled canvases referencing the live SOURCE buffer."
  (when (buffer-live-p source)
    (with-current-buffer source (setq excali-org-prototype--timer nil))
    (dolist (canvas (buffer-list))
      (with-current-buffer canvas
        (when (and (bound-and-true-p excali-org-prototype-mode)
                   (seq-some
                    (lambda (card)
                      (equal (alist-get 'file (excali-org-prototype--reference card))
                             (buffer-local-value 'buffer-file-name source)))
                    excali--elements))
          (excali-org-prototype-refresh-all))))))

(defun excali-org-prototype--schedule (&rest _)
  "Debounce live source changes without saving either file."
  (when (timerp excali-org-prototype--timer)
    (cancel-timer excali-org-prototype--timer))
  (setq excali-org-prototype--timer
        (run-with-idle-timer 0.35 nil #'excali-org-prototype--sync-source
                             (current-buffer))))

(defun excali-org-prototype--cancel-timer ()
  "Cancel a source buffer's pending refresh."
  (when (timerp excali-org-prototype--timer)
    (cancel-timer excali-org-prototype--timer))
  (setq excali-org-prototype--timer nil))

(defun excali-org-prototype--watch-sources ()
  "Install local edit hooks in this canvas's available Org sources."
  (dolist (card excali--elements)
    (when-let* ((ref (excali-org-prototype--reference card)))
      (condition-case nil
          (let ((marker (excali-org-prototype--resolve ref)))
            (with-current-buffer (marker-buffer marker)
              (add-hook 'after-change-functions #'excali-org-prototype--schedule nil t)
              (add-hook 'after-revert-hook #'excali-org-prototype--schedule nil t)
              (add-hook 'kill-buffer-hook #'excali-org-prototype--cancel-timer nil t))
            (set-marker marker nil))
        (error nil)))))

(defun excali-org-prototype--source-opened ()
  "Watch a reopened local Org source if an enabled canvas references it."
  (when (and (derived-mode-p 'org-mode) buffer-file-name)
    (let ((file buffer-file-name))
      (when (seq-some
             (lambda (buffer)
               (with-current-buffer buffer
                 (and (bound-and-true-p excali-org-prototype-mode)
                      (seq-some
                       (lambda (card)
                         (equal file (alist-get
                                      'file (excali-org-prototype--reference card))))
                       excali--elements))))
             (buffer-list))
        (add-hook 'after-change-functions #'excali-org-prototype--schedule nil t)
        (add-hook 'after-revert-hook #'excali-org-prototype--schedule nil t)
        (add-hook 'kill-buffer-hook #'excali-org-prototype--cancel-timer nil t)
        (excali-org-prototype--schedule)))))
(add-hook 'find-file-hook #'excali-org-prototype--source-opened)

;; These layout adapters are deliberately confined to referenced cards.
;; The saved scene still consists of ordinary editable rectangles and text.
(defun excali-org-prototype--padded-container (fn container &rest args)
  "Call FN on CONTAINER and ARGS with card-specific text insets."
  (let ((excali-bound-text-padding
         (if (excali-org-prototype--reference container) 20
           excali-bound-text-padding)))
    (apply fn container args)))

(defun excali-org-prototype--padded-text (fn text &optional container)
  "Call text layout FN with TEXT and CONTAINER using card insets."
  (let* ((container (or container (excali--container-of text)))
         (excali-bound-text-padding
          (if (excali-org-prototype--reference container) 20
            excali-bound-text-padding)))
    (funcall fn text container)))

(dolist (fn '(excali--bound-text-max-width excali--bound-text-max-height
              excali--container-coords excali--layout-bound-text))
  (advice-add fn :around #'excali-org-prototype--padded-container))
(advice-add 'excali--redraw-text :around #'excali-org-prototype--padded-text)

(defun excali-org-prototype--opened (buffer)
  "Restore card interaction in a newly opened scene BUFFER."
  (with-current-buffer buffer
    (when (seq-some #'excali-org-prototype--reference excali--elements)
      (excali-org-prototype-mode 1)
      (excali-org-prototype-refresh-all)))
  buffer)
(advice-add 'excali--open :filter-return #'excali-org-prototype--opened)

(defun excali-org-prototype-return ()
  "Return to the last experimental canvas after editing Org."
  (interactive)
  (unless (buffer-live-p excali-org-prototype--canvas)
    (user-error "No live experimental canvas"))
  (pop-to-buffer excali-org-prototype--canvas)
  (excali-org-prototype-refresh-all))

(defvar excali-org-prototype-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-o") #'excali-org-prototype-visit)
    (define-key map (kbd "C-c C-r") #'excali-org-prototype-refresh)
    map))

(define-minor-mode excali-org-prototype-mode
  "Opt-in experimental Org card commands on an excali canvas.
Saved referenced scenes enable this mode when reopened.  Deleting a card never deletes
Org content.  Refresh overwrites manual card text, not its source."
  :lighter " OrgCard" :keymap excali-org-prototype-mode-map
  (when excali-org-prototype-mode
    (unless (derived-mode-p 'excali-mode)
      (setq excali-org-prototype-mode nil)
      (user-error "Enable OrgCard only in an excali canvas"))
    (setq excali-org-prototype--canvas (current-buffer))
    (excali-org-prototype--watch-sources)))

(defun excali-org-prototype-demo ()
  "Open a disposable Org file and add its first heading to a new canvas."
  (interactive)
  (let* ((dir (make-temp-file "excali-org-experiment-" t))
         (file (expand-file-name "notes.org" dir)))
    (setq excali-org-prototype--canvas nil)
    (find-file file)
    (insert "* TODO Test Org canvas connectivity\nThis text belongs to Org. Edit the heading or body: the card updates automatically.\n\n* Another heading\nThis must not appear in the first card.\n")
    (goto-char (point-min))
    (org-id-get-create)
    (save-buffer)
    (excali-org-prototype-add-heading)
    (setq excali--file (expand-file-name "board.excalidraw" dir))
    (excali-save)
    (message "Experiment files: %s; C-c C-o visits Org, C-c C-r refreshes" dir)))

(provide 'excali-org-prototype)
;;; excali-org-prototype.el ends here
