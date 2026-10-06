;;; excali-board-vault.el --- Bounded board file discovery -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; A vault is an ordinary local directory, not a database or import format.
;; Discovery is recursive and on demand; it never moves files or scans outside
;; the root through symbolic links.  Existing boards may remain vault-less.
;;; Code:
(require 'excali)
(require 'org-attach)
(declare-function excali-board--require-board "excali-board" ())
(declare-function excali-board--resolve "excali-board" (reference))

(defcustom excali-board-vaults nil
  "Named local vault directories, as (NAME . DIRECTORY) pairs.
Use this list for both single-vault and multiple-vault configurations.
Choose the deepest matching root automatically from the source location.
Existing boards keep their persisted root.  Names are configuration labels,
not directory names to create.  All configured directories must exist."
  :type '(alist :key-type string :value-type directory)
  :group 'excali)

(defun excali-board-vault--configured-roots ()
  "Return canonical configured roots, rejecting invalid configuration."
  (let ((entries excali-board-vaults)
        roots)
    (unless entries
      (user-error "Configure excali-board-vaults, or use M-x excali-new"))
    (dolist (entry entries)
      (unless (and (consp entry) (stringp (car entry))
                   (stringp (cdr entry)) (not (file-remote-p (cdr entry)))
                   (file-name-absolute-p (cdr entry)) (file-directory-p (cdr entry)))
        (user-error "Invalid vault entry %S: use an existing absolute local directory" entry))
      (cl-pushnew (file-name-as-directory (file-truename (cdr entry))) roots :test #'equal))
    roots))

(defun excali-board-vault--matching-root (directory roots)
  "Return the deepest root containing canonical DIRECTORY from ROOTS."
  (car (sort (seq-filter (lambda (root)
                          (or (equal directory root) (file-in-directory-p directory root)))
                        roots)
             (lambda (a b) (> (length a) (length b))))))

(defun excali-board-vault--creation-context ()
  "Return (ROOT . DIRECTORY) selected automatically from the source location."
  (let* ((roots (excali-board-vault--configured-roots))
         (file (or buffer-file-name
                   (and (derived-mode-p 'excali-board-mode) excali--file)))
         (location (or file default-directory)))
    (when (file-remote-p location)
      (user-error "Outside local vaults; update vault configuration or use M-x excali-new"))
    (let* ((directory (file-name-as-directory
                       (if file (file-name-directory (file-truename file))
                         (file-truename default-directory))))
           (root (excali-board-vault--matching-root directory roots)))
      (unless root
        (user-error "Outside configured vaults; update excali-board-vaults, or use M-x excali-new"))
      (cons root directory))))

(defun excali-board-vault--new-file (file root directory)
  "Validate a new board FILE within ROOT, relative to DIRECTORY."
  (setq file (expand-file-name file directory))
  (unless (file-name-extension file) (setq file (concat file ".excalidraw")))
  (unless (and (not (file-remote-p file))
               (equal (file-name-extension file) "excalidraw")
               (file-directory-p (file-name-directory file))
               (file-in-directory-p (file-truename file) root))
    (user-error "Choose a .excalidraw filename inside the configured vault"))
  (when (and excali-board-vaults
             (not (equal root
                         (excali-board-vault--matching-root
                          (file-name-directory (file-truename file))
                          (excali-board-vault--configured-roots)))))
    (user-error "Destination belongs to a different vault; create from that vault instead"))
  (when (or (file-exists-p file) (file-symlink-p file) (find-buffer-visiting file))
    (user-error "File already exists or is being edited; choose a new filename"))
  file)

(defun excali-board-vault-root (&optional noerror)
  "Return the current board's canonical vault root.
With NOERROR return nil when no vault is configured.  An unavailable configured
root is always an error: never silently broaden the discovery scope."
  (let ((root (alist-get 'vaultRoot (alist-get 'excaliBoard excali--doc))))
    (cond
     ((null root) (unless noerror (user-error "Set a vault with excali-board-set-vault")))
     ((or (not (stringp root)) (file-remote-p root)
          (not (file-name-absolute-p root)) (not (file-directory-p root)))
      (user-error "Vault unavailable; use excali-board-set-vault to relocate it"))
     (t (file-name-as-directory (file-truename root))))))

;;;###autoload
(defun excali-board-set-vault (directory)
  "Associate this board with local DIRECTORY and all its subdirectories.
Store the root in the board document without moving files or saving the board.
Re-select the root after moving a vault to another machine."
  (interactive "DVault directory: ")
  (require 'excali-board)
  (excali-board--require-board)
  (when (or (file-remote-p directory) (not (file-directory-p directory)))
    (user-error "Choose an existing local directory"))
  (let* ((root (file-name-as-directory (file-truename directory)))
         (meta (copy-tree (alist-get 'excaliBoard excali--doc))))
    (setf (alist-get 'vaultRoot meta) root
          (alist-get 'excaliBoard excali--doc) meta)
    (set-buffer-modified-p t)
    (message "Vault: %s (save the board to persist)" root)
    root))

(defun excali-board-vault--inside-p (file root)
  "Whether local FILE resolves inside canonical ROOT."
  (and (not (file-remote-p file))
       (file-in-directory-p (file-truename file) root)))

(defun excali-board-vault-files (&optional kind)
  "Return sorted absolute files recursively inside this board's vault.
KIND may be org, board, media or nil (all files).  Skip VCS/cache directories,
directory symlinks and symlinks escaping the vault.  Do not visit file buffers."
  (let ((root (excali-board-vault-root)) files)
    (cl-labels
        ((walk (dir)
           (dolist (file (directory-files dir t directory-files-no-dot-files-regexp t))
             (cond
              ((file-directory-p file)
               (unless (or (file-symlink-p file)
                           (member (file-name-nondirectory file)
                                   '(".git" ".hg" ".svn" ".excali-cache")))
                 (walk file)))
              ((and (file-regular-p file)
                    (excali-board-vault--inside-p file root)
                    (let ((ext (downcase (or (file-name-extension file) ""))))
                      (pcase kind
                        ('org (equal ext "org"))
                        ('board (equal ext "excalidraw"))
                        ('media (member ext '("pdf" "png" "jpg" "jpeg" "gif" "webp"
                                               "svg" "bmp" "avif" "mp4" "mov" "mkv" "webm" "avi" "m4v"
                                               "mp3" "wav" "flac" "m4a" "ogg" "opus" "aac")))
                        ('nil t)
                        (_ (error "Unknown vault file kind: %s" kind)))))
               (push file files))))))
      (walk root))
    (sort files #'string<)))

(defun excali-board-vault-read-file (prompt &optional kind)
  "Choose an existing file using PROMPT, optionally restricted to KIND.
Vault choices show relative paths to disambiguate duplicate basenames.
For older boards with no vault, retain the ordinary local file picker."
  (if-let* ((root (excali-board-vault-root t)))
      (let ((choices (mapcar (lambda (file) (cons (file-relative-name file root) file))
                            (excali-board-vault-files kind))))
        (unless choices (user-error "No matching files in vault"))
        (cdr (assoc (completing-read prompt choices nil t) choices)))
    (read-file-name prompt nil nil t)))

;;;###autoload
(defun excali-board-vault-find-file ()
  "Find any file recursively in the current board's vault."
  (interactive)
  (require 'excali-board)
  (excali-board--require-board)
  (excali-board-vault-root)
  (let ((file (excali-board-vault-read-file "Vault file: "))
        (enable-local-eval nil) (enable-local-variables :safe))
    (find-file-other-window file)))

(defun excali-board-vault-resolve-file (path &optional source-marker attachment)
  "Resolve local PATH in the context of SOURCE-MARKER or this board.
For ATTACHMENT use Org's own ID/DIR attachment resolution at SOURCE-MARKER.
Ordinary source links are relative to their Org file, not the board or vault.
Independent notes resolve relative paths against the vault root, or a saved
board's directory when no vault exists.  This function does not open files,
execute link handlers, create attachment directories or search by basename."
  (when (or (not (stringp path)) (file-remote-p path))
    (user-error "Only local file paths are supported"))
  (let* ((base
          (cond
           (attachment
            (unless (and (markerp source-marker) (marker-buffer source-marker))
              (user-error "Attachment links require a source heading"))
            (with-current-buffer (marker-buffer source-marker)
              (save-restriction
                (widen)
                (save-excursion
                  (goto-char source-marker)
                  (expand-file-name
                   (or (org-attach-dir nil t)
                       (user-error "Heading has no attachment directory"))
                   (if buffer-file-name (file-name-directory buffer-file-name)
                     default-directory))))))
           ((and (markerp source-marker) (marker-buffer source-marker))
            (with-current-buffer (marker-buffer source-marker)
              (unless buffer-file-name (user-error "Source has no file"))
              (file-name-directory buffer-file-name)))
           (t (or (excali-board-vault-root t)
                  (and excali--file (file-name-directory excali--file))
                  (unless (file-name-absolute-p path)
                    (user-error "Set a vault for relative file links"))))))
         (file (expand-file-name path base)))
    (when (file-remote-p file) (user-error "Remote attachments are not supported"))
    file))

(provide 'excali-board-vault)
;;; excali-board-vault.el ends here
