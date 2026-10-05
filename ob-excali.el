;;; ob-excali.el --- Org Babel support for Excali DSL -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Enable with `org-babel-do-load-languages', adding (excali . t).
;; Use #+begin_src excali :file diagram.svg :results file graphics.
;; Execution uses the local native renderer without opening a canvas.
;; Babel owns execution confirmation, :dir, noweb expansion and result links.
;; This adapter writes the result atomically and returns nil, so Babel does
;; not overwrite the generated file with a textual return value.

;;; Code:
(require 'ob)
(require 'org-src)
(require 'subr-x)

(autoload 'excali-dsl-mode "excali-dsl" nil t)
(add-to-list 'org-src-lang-modes '("excali" . excali-dsl))

(defvar org-babel-default-header-args:excali
  '((:results . "file") (:exports . "results"))
  "Default header arguments for Excali DSL blocks.")

(declare-function excali-dsl-scene "excali-dsl")
(declare-function excali-dsl-error-message "excali-dsl")
(declare-function excali--serialize-doc "excali-core")
(declare-function excali-export-svg "excali-export")
(declare-function excali-export-png "excali-export")
(defvar excali--doc)
(defvar excali--elements)
(defvar excali--native-cache)
(defvar excali--selection)

(defun org-babel-excali--validate (params)
  "Validate PARAMS and return the absolute local output path."
  (let ((file (cdr (assq :file params)))
        (session (cdr (assq :session params)))
        (results (cdr (assq :result-params params))))
    (unless (and (stringp file) (not (string-empty-p file)))
      (user-error "Excali requires :file with a .svg, .png or .excalidraw filename"))
    (unless (member (downcase (or (file-name-extension file) "")) '("svg" "png" "excalidraw"))
      (user-error "Excali output must be .svg, .png or .excalidraw"))
    (when (file-remote-p (expand-file-name file))
      (user-error "Excali supports local output files only"))
    (when (and results (not (member "file" results)))
      (user-error "Excali requires :results file (optionally graphics)"))
    (when (and session (not (member session '("none" "nil"))))
      (user-error "Excali does not support sessions"))
    (dolist (key '(:var :cmd :cmdline :prologue :epilogue))
      (when (assq key params)
        (user-error "Excali does not support %s" key)))
    (when-let* ((mode (cdr (assq :file-mode params))))
      (unless (and (integerp mode) (<= 0 mode #o7777))
        (user-error "Excali :file-mode must be an integer permission mode")))
    (expand-file-name file)))

(defun org-babel-execute:excali (body params)
  "Render Excali DSL BODY to :file according to Babel PARAMS.
Supported extensions are svg, png and excalidraw.  Relative paths use
Babel's effective `default-directory', including :dir.  Return nil to
signal that the result file has already been written.  This function
never disables Babel's execution confirmation."
  (let* ((file (org-babel-excali--validate params))
         (extension (downcase (file-name-extension file)))
         (directory (file-name-directory file))
         (source (or buffer-file-name (buffer-name)))
         (block-line (and org-babel-current-src-block-location
                          (line-number-at-pos org-babel-current-src-block-location)))
         doc temp)
    ;; Loading the Babel adapter alone must not load the native module.
    (require 'excali)
    (condition-case err
        (setq doc (excali-dsl-scene body))
      (excali-dsl-error
       (user-error "%s%s: %s" source
                   (if block-line (format ", block starting at line %d" block-line) "")
                   (excali-dsl-error-message err))))
    (unless (file-directory-p directory)
      (if (member (cdr (assq :mkdirp params)) '("yes" "t"))
          (make-directory directory t)
        (user-error "Output directory does not exist: %s (use :mkdirp yes)" directory)))
    (unwind-protect
        (progn
          ;; The same directory ensures rename is atomic on the local filesystem.
          (setq temp (make-temp-file (expand-file-name ".ob-excali-" directory) nil
                                     (concat "." extension)))
          (with-temp-buffer
            (setq-local excali--doc doc)
            (setq-local excali--elements (append (alist-get 'elements doc) nil))
            (setq-local excali--selection nil)
            (setq-local excali--native-cache (make-hash-table :test #'eq))
            (pcase extension
              ("svg" (excali-export-svg temp))
              ("png" (excali-export-png temp))
              ("excalidraw"
               (let ((text (excali--serialize-doc doc excali--elements)))
                 (with-temp-file temp
                   (setq buffer-file-coding-system 'utf-8-unix)
                   (insert text))))))
          (when-let* ((mode (or (cdr (assq :file-mode params))
                               (and (file-exists-p file) (file-modes file)))))
            (set-file-modes temp mode))
          (rename-file temp file t))
      (when (and temp (file-exists-p temp)) (delete-file temp)))
    nil))

(defun org-babel-prep-session:excali (_session _params)
  "Explain why Excali cannot prepare a Babel session."
  (user-error "Excali does not support sessions"))

(provide 'ob-excali)
;;; ob-excali.el ends here
