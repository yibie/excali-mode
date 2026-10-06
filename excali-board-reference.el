;;; excali-board-reference.el --- Vault reference maintenance -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Explicit bounded repair, reverse references, and conservative disk refresh.
;;; Code:
(require 'excali-board-vault)
(require 'org-id)
(require 'button)
(declare-function excali-board--boards "excali-board" ())
(declare-function excali-board--reference "excali-board" (card))
(declare-function excali-board--require-board "excali-board" ())
(declare-function excali-board--source-buffer "excali-board" (file))
(declare-function excali-board--refresh-card "excali-board" (card))
(declare-function excali-board--watch-sources "excali-board" ())
(declare-function excali-board-refresh-all "excali-board" ())
(declare-function excali-board-open "excali-board" (file))
(defvar excali-board--errors)
(defvar excali-board--editing-board)

(defvar excali-board-reference--timer nil)
(defvar excali-board-reference--polling nil)

(defun excali-board-reference--check-disk (buffer)
  "Safely reconcile BUFFER with disk, or report a conflict.
Never overwrite unsaved text.  Revert only unchanged buffers with existing
files.  Missing paths are handled by explicit vault repair, not recreated."
  (with-current-buffer buffer
    (when (and buffer-file-name (not (verify-visited-file-modtime buffer)))
      (cond
       ((buffer-modified-p)
        (user-error "External modification conflicts with unsaved edits: %s"
                    buffer-file-name))
       ((not (file-exists-p buffer-file-name))
        (user-error "Source moved or deleted; run excali-board-repair-references"))
       (t (let ((enable-local-eval nil) (enable-local-variables :safe))
            (revert-buffer t t)))))))

(defun excali-board-reference--poll ()
  "Check sources of live boards; report failures through their card errors."
  (unless excali-board-reference--polling
    (let ((excali-board-reference--polling t))
      (dolist (board (excali-board--boards))
        (with-current-buffer board
          (when (or excali-board--errors
                    (seq-some
                     (lambda (card)
                       (when-let* ((ref (excali-board--reference card))
                                   (file (alist-get 'file ref))
                                   (source (find-buffer-visiting file)))
                         (not (verify-visited-file-modtime source))))
                     excali--elements))
            (condition-case err
                (excali-board-refresh-all)
              (error (message "Board refresh: %s" (error-message-string err))))))))))

(defun excali-board-reference--start ()
  "Start one shared periodic disk-check timer while boards are open."
  (unless (timerp excali-board-reference--timer)
    (setq excali-board-reference--timer
          (run-with-timer 2 2 #'excali-board-reference--poll))))

(defun excali-board-reference--stop ()
  "Stop disk checking when the last board goes away."
  (unless (delq (current-buffer) (excali-board--boards))
    (when (timerp excali-board-reference--timer)
      (cancel-timer excali-board-reference--timer))
    (setq excali-board-reference--timer nil)))

(defun excali-board-reference--id-index ()
  "Return ID to file-list index within the current vault.
Visiting buffers (including unsaved new files) take precedence over disk.
Do not visit unvisited files, execute local variables, or use global ID search."
  (let* ((root (excali-board-vault-root))
         (files (excali-board-vault-files 'org))
         (index (make-hash-table :test #'equal)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and (not (buffer-base-buffer)) buffer-file-name
                   (derived-mode-p 'org-mode)
                   (or (file-exists-p buffer-file-name) (buffer-modified-p))
                   (excali-board-vault--inside-p buffer-file-name root))
          (push (file-truename buffer-file-name) files))))
    (dolist (file (delete-dups (mapcar #'file-truename files)))
      (let* ((visiting (find-buffer-visiting file))
             (text (if visiting
                       (with-current-buffer visiting
                         (when (file-exists-p file)
                           (excali-board-reference--check-disk visiting))
                         (save-restriction
                           (widen) (buffer-substring-no-properties (point-min) (point-max))))
                     (with-temp-buffer (insert-file-contents file) (buffer-string)))))
        (with-temp-buffer
          (insert text)
          (let ((org-mode-hook nil) (org-inhibit-startup t)) (org-mode))
          (org-element-map (org-element-parse-buffer) 'headline
            (lambda (h)
              (goto-char (org-element-property :begin h))
              (when-let* ((id (org-entry-get nil "ID")))
                ;; Do not deduplicate: duplicate IDs in one file are ambiguous.
                (puthash id (cons file (gethash id index)) index)))))))
    index))

;;;###autoload
(defun excali-board-repair-references ()
  "Find each referenced ID in this vault and repair changed source paths.
Ambiguous or missing IDs are reported, never guessed.  Do not save either
the board or Org files.  Geometry, card identity and arrows stay unchanged."
  (interactive)
  (require 'excali-board)
  (excali-board--require-board)
  (let ((index (excali-board-reference--id-index)) (count 0) errors)
    (dolist (card excali--elements)
      (when-let* ((ref (and (not (excali--get card 'isDeleted))
                           (excali-board--reference card))))
        (let ((matches (gethash (alist-get 'id ref) index)))
          (cond
           ((not (= (length matches) 1))
            (push (cons (excali--get card 'id)
                        (if matches "Duplicate Org ID in vault; repair skipped"
                          "Org ID not found in vault")) errors))
           (t
            (let ((file (car matches)))
              (unless (equal file (file-truename (alist-get 'file ref)))
                ;; Do not detach a card from a dirty source whose file vanished.
                (let ((old (find-buffer-visiting (alist-get 'file ref))))
                  (if (and old (buffer-modified-p old))
                      (push (cons (excali--get card 'id)
                                  "Old source has unsaved edits; repair skipped") errors)
                    (let ((data (copy-tree (excali--get card 'customData) t)))
                      (setf (alist-get 'file (alist-get 'excaliOrg data)) file)
                      (excali--put card 'customData data)
                      (excali--touch card)
                      (cl-incf count)))))))))))
    (when (> count 0) (excali--commit))
    (excali-board--watch-sources)
    (excali-board-refresh-all)
    (setq excali-board--errors (append errors excali-board--errors))
    (force-mode-line-update)
    (message "Repaired %d reference(s); %d issue(s). Save the board separately.%s"
             count (length excali-board--errors)
             (if excali-board--errors
                 (concat " " (string-join (delete-dups (mapcar #'cdr excali-board--errors)) "; "))
               ""))
    count))

(defun excali-board-reference--backlinks (id root)
  "Return (FILE BUFFER CARD-ID) entries referencing ID within ROOT.
Prefer live board state over saved files, including unsaved boards associated
with ROOT.  Corrupt candidate files are reported rather than silently hidden."
  (let (result seen)
    (dolist (board (excali-board--boards))
      (with-current-buffer board
        (when (or (and excali--file (excali-board-vault--inside-p excali--file root))
                  (equal (alist-get 'vaultRoot (alist-get 'excaliBoard excali--doc)) root))
          (when excali--file (push (file-truename excali--file) seen))
          (dolist (card excali--elements)
            (when (and (not (excali--get card 'isDeleted))
                       (equal id (alist-get 'id (excali-board--reference card))))
              (push (list excali--file board (excali--get card 'id)) result))))))
    (with-temp-buffer
      (setq-local excali--doc `((excaliBoard . ((version . 1) (vaultRoot . ,root)))))
      (dolist (file (excali-board-vault-files 'board))
        (unless (member (file-truename file) seen)
          (let ((doc (excali--read-file file)))
            (when (assq 'excaliBoard doc)
              (dolist (card (append (alist-get 'elements doc) nil))
                (when (and (not (eq (excali--get card 'isDeleted) t))
                           (equal id (alist-get 'id (excali-board--reference card))))
                  (push (list file nil (excali--get card 'id)) result))))))))
    (nreverse result)))

;;;###autoload
(defun excali-board-find-backlinks (directory)
  "List boards in DIRECTORY's vault that reference the current Org heading.
Use its existing ID; never create one merely to search."
  (interactive
   (list (read-directory-name "Search vault: ")))
  (require 'excali-board)
  (unless (derived-mode-p 'org-mode) (user-error "Run at an Org heading"))
  (when (or (file-remote-p directory) (not (file-directory-p directory)))
    (user-error "Choose a local vault directory"))
  (let* ((id (save-excursion (org-back-to-heading t) (org-entry-get nil "ID")))
         (root (file-name-as-directory (file-truename directory))))
    (unless id (user-error "This heading has no ID"))
    (let ((hits (excali-board-reference--backlinks id root))
          (buffer (get-buffer-create "*Excali board references*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Heading ID: %s\nVault: %s\n\n" id root))
          (dolist (hit hits)
            (pcase-let ((`(,file ,board ,card-id) hit))
              (insert-text-button
               (format "%s  [card %s]" (if file (file-relative-name file root)
                                        (buffer-name board)) card-id)
               'follow-link t
               'action
               (lambda (_)
                 (if (buffer-live-p board) (pop-to-buffer board)
                   (excali-board-open file))
                 (when-let* ((card (excali--live-element-by-id card-id)))
                   (excali--select (list card))
                   (excali--render))))
              (insert "\n")))
          (unless hits (insert "No references found.\n"))
          (special-mode)))
      (pop-to-buffer buffer))))

(provide 'excali-board-reference)
;;; excali-board-reference.el ends here
