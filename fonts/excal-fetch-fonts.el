;;; excal-fetch-fonts.el --- Download Excalidraw's fonts into fonts/  -*- lexical-binding: t; -*-

;;; Commentary:

;; Run by `make fonts':
;;
;;   emacs --batch -Q -l fonts/excal-fetch-fonts.el
;;
;; Downloads the font files Excalidraw ships (packages/excalidraw/fonts
;; on the excalidraw master branch, listed through the GitHub contents
;; API) with curl, plus the complete Xiaolai TTF from lxgw/kose-font.
;; Upstream ships WOFF2 files split into unicode-range subsets.  When
;; `woff2_decompress' (Homebrew/Debian package "woff2") is installed they
;; are converted to TTF, and when `pyftmerge' (fonttools) is installed
;; the subsets of one family are merged into a single file.  CoreText
;; (macOS) reads WOFF2 directly; FreeType/fontconfig builds without
;; Brotli need the TTF conversion.  See fonts/README.

;;; Code:

(require 'json)

(defconst excal-fonts--dir
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst excal-fonts--upstream
  "https://api.github.com/repos/excalidraw/excalidraw/contents/packages/excalidraw/fonts/"
  "GitHub contents API URL of upstream's font directories.")

(defconst excal-fonts--families
  '("Excalifont" "Nunito" "Lilita" "ComicShanns" "Liberation" "Cascadia" "Virgil")
  "Upstream font directories to download.")

(defconst excal-fonts--xiaolai
  "https://github.com/lxgw/kose-font/releases/latest/download/Xiaolai-Regular.ttf"
  "Complete Xiaolai TTF (upstream only ships 208 CJK subsets).")

(defun excal-fonts--curl (url &optional file)
  "Fetch URL with curl, into FILE or returned as a string; nil on failure."
  (with-temp-buffer
    (let ((status (apply #'call-process "curl" nil t nil
                         "-fsSL" "--retry" "2"
                         (append (and file (list "-o" file)) (list url)))))
      (if (eq status 0)
          (or file (buffer-string))
        (message "  failed: %s" url)
        (when (and file (file-exists-p file)) (delete-file file))
        nil))))

(defun excal-fonts--run (program &rest args)
  "Run PROGRAM with ARGS; return non-nil on success."
  (eq 0 (apply #'call-process program nil nil nil args)))

(defun excal-fonts--fetch-family (name)
  "Download upstream directory NAME into fonts/NAME; return the files."
  (message "%s" name)
  (let* ((listing (excal-fonts--curl (concat excal-fonts--upstream name)))
         (entries (and listing (json-parse-string listing :object-type 'alist)))
         (dir (expand-file-name name excal-fonts--dir))
         files)
    (when entries (make-directory dir t))
    (seq-doseq (entry (or entries []))
      (let ((file-name (alist-get 'name entry))
            (url (alist-get 'download_url entry)))
        (when (and (stringp url) (string-match-p "\\.woff2?\\'" file-name))
          (let ((target (expand-file-name file-name dir)))
            (message "  %s" file-name)
            (when (excal-fonts--curl url target) (push target files))))))
    (nreverse files)))

(defun excal-fonts--convert (files)
  "Convert WOFF2 FILES to TTF when possible; return the resulting files."
  (if (not (executable-find "woff2_decompress"))
      files
    (mapcar (lambda (file)
              (let ((ttf (concat (file-name-sans-extension file) ".ttf")))
                (if (and (string-suffix-p ".woff2" file)
                         (excal-fonts--run "woff2_decompress" file)
                         (file-exists-p ttf))
                    (progn (delete-file file) ttf)
                  file)))
            files)))

(defun excal-fonts--merge (name files)
  "Merge the TTF subsets FILES of family NAME into one file if possible."
  (let ((ttfs (seq-filter (lambda (f) (string-suffix-p ".ttf" f)) files))
        (merged (expand-file-name (format "%s/%s-Regular.ttf" name name)
                                  excal-fonts--dir)))
    (when (and (> (length ttfs) 1) (executable-find "pyftmerge"))
      (if (apply #'excal-fonts--run "pyftmerge" (concat "--output-file=" merged) ttfs)
          (progn (mapc #'delete-file ttfs)
                 (message "  merged %d subsets into %s" (length ttfs)
                          (file-name-nondirectory merged)))
        (message "  pyftmerge failed; keeping the subsets")))))

(defun excal-fonts-fetch ()
  "Download all fonts."
  (unless (executable-find "curl") (error "curl is required"))
  (dolist (name excal-fonts--families)
    (excal-fonts--merge name (excal-fonts--convert (excal-fonts--fetch-family name))))
  (message "Xiaolai")
  (let ((dir (expand-file-name "Xiaolai" excal-fonts--dir)))
    (make-directory dir t)
    (unless (excal-fonts--curl excal-fonts--xiaolai
                               (expand-file-name "Xiaolai-Regular.ttf" dir))
      (delete-directory dir)))
  (unless (executable-find "woff2_decompress")
    (message "\nNote: woff2_decompress not found; fonts stay WOFF2.  macOS reads them,
but only one unicode-range subset per family is used there, and
FreeType without Brotli cannot read them at all.  Install `woff2'
(and fonttools for pyftmerge) and run `make fonts' again."))
  (message "Done.  Restart Emacs or run M-x excal-register-fonts."))

(when noninteractive (excal-fonts-fetch))

;;; excal-fetch-fonts.el ends here
