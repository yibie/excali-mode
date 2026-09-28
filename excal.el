;;; excal.el --- Excalidraw scenes on Emacs Canvas (spike)  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "32.0"))

;;; Commentary:

;; Excalidraw for Emacs: read and write .excalidraw files, render them
;; through a Cairo/Pango module into a Canvas image, and support basic
;; drawing, selection, panning and zooming.
;;
;; Entry points: `excal-open', `excal-new', `excal-bench'.

;;; Code:

(require 'excal-core)
(require 'excal-view)
(require 'excal-edit)
(require 'excal-bench)

(defmacro excal--tool-command (tool)
  "Return a command selecting TOOL."
  `(lambda ()
     (interactive)
     (setq excal--tool ',tool)
     (excal--update-pointer)
     (message "Tool: %s" ',tool)))

(defvar-keymap excal-mode-map
  "<down-mouse-1>" #'excal-mouse-down
  "<double-down-mouse-1>" #'excal-double-click
  "<mouse-movement>" #'excal-mouse-move
  "<wheel-up>" #'excal-wheel "<wheel-down>" #'excal-wheel
  "<wheel-left>" #'excal-wheel "<wheel-right>" #'excal-wheel
  "C-<wheel-up>" #'excal-wheel "C-<wheel-down>" #'excal-wheel
  "<pinch>" #'excal-pinch
  "v" (excal--tool-command select)
  "r" (excal--tool-command rectangle)
  "o" (excal--tool-command ellipse)
  "d" (excal--tool-command diamond)
  "a" (excal--tool-command arrow)
  "l" (excal--tool-command line)
  "p" (excal--tool-command freedraw)
  "t" (excal--tool-command text)
  "e" #'excal-edit-text "RET" #'excal-edit-text
  "<delete>" #'excal-delete-selected "DEL" #'excal-delete-selected
  "=" #'excal-zoom-in "-" #'excal-zoom-out "0" #'excal-zoom-reset
  "H" #'excal-toggle-pixel-scale
  "b" #'excal-cycle-backend
  "g" #'excal--sync-canvas
  "C-x C-s" #'excal-save
  "B" #'excal-bench)

(define-derived-mode excal-mode special-mode "Excal"
  "Major mode for editing Excalidraw scenes on a Canvas image."
  (setq-local cursor-type nil
              ;; Report plain mouse motion so the pointer can follow the
              ;; scene; see `excal-mouse-move'.
              track-mouse t
              truncate-lines t
              line-spacing nil
              mode-line-process '(:eval (format " %s %s %d%%" excal--backend
                                                excal--tool
                                                (round (* 100 excal--zoom)))))
  (setq excal--native-cache (make-hash-table :test #'eq :weakness 'key)
        excal--pixel-scale (excal--guess-pixel-scale)
        excal--backend (if (and (eq excal-backend 'layer)
                                (not (fboundp 'excal-native-layer-create)))
                           'tiles
                         excal-backend))
  (add-hook 'kill-buffer-hook #'excal--hide-layer nil t)
  (add-hook 'window-size-change-functions #'excal--window-size-change)
  (add-hook 'window-buffer-change-functions #'excal--window-size-change))

(defun excal--open (doc file name)
  "Show DOC saved to FILE in a buffer called NAME."
  (unless (and (display-graphic-p) (image-type-available-p 'canvas))
    (error "excal needs a graphical Emacs with Canvas images"))
  (let ((buffer (generate-new-buffer name)))
    (pop-to-buffer-same-window buffer)
    (excal-mode)
    (setq excal--file file
          excal--doc doc
          excal--elements (append (alist-get 'elements doc) nil))
    (excal--sync-canvas (selected-window))
    buffer))

;;;###autoload
(defun excal-open (file)
  "Open .excalidraw FILE."
  (interactive "fExcalidraw file: ")
  (let ((file (expand-file-name file)))
    (excal--open (excal--read-file file) file
                 (format "*excal %s*" (file-name-nondirectory file)))))

;;;###autoload
(defun excal-new ()
  "Start an empty scene."
  (interactive)
  (excal--open (excal--empty-doc) nil "*excal new*"))

(provide 'excal)
;;; excal.el ends here
