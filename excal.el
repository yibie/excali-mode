;;; excal.el --- Excalidraw scenes on Emacs Canvas (spike)  -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "32.0"))

;;; Commentary:

;; Excalidraw for Emacs: read and write .excalidraw files, render them
;; through a Cairo/Pango module into a Canvas image, and support basic
;; drawing, selection, panning and zooming.
;;
;; Entry points: `excal-open', `excal-new'.

;;; Code:

(require 'excal-core)
(require 'excal-view)
(require 'excal-select)
(require 'excal-handles)
(require 'excal-transform)
(require 'excal-style)
(require 'excal-edit)
(require 'excal-history)
(require 'excal-clipboard)
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
  "S-<down-mouse-1>" #'excal-mouse-down
  "M-<down-mouse-1>" #'excal-mouse-down
  "M-S-<down-mouse-1>" #'excal-mouse-down
  "<double-down-mouse-1>" #'excal-double-click
  "<down-mouse-2>" #'excal-mouse-pan
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
  "h" (excal--tool-command hand)
  "s" #'excal-style
  "<escape>" #'excal-escape
  ;; Emacs bindings first, then macOS Command-key equivalents.
  "C-/" #'excal-undo "C-_" #'excal-undo "C-x u" #'excal-undo "s-z" #'excal-undo
  "C-?" #'excal-redo "C-M-_" #'excal-redo "s-Z" #'excal-redo "s-y" #'excal-redo
  "M-w" #'excal-copy "s-c" #'excal-copy
  "C-w" #'excal-cut "s-x" #'excal-cut
  "C-y" #'excal-paste "s-v" #'excal-paste
  "C-c C-d" #'excal-duplicate "s-d" #'excal-duplicate
  "C-x h" #'excal-select-all "s-a" #'excal-select-all
  "C-c C-g" #'excal-group "s-g" #'excal-group
  "C-c C-u" #'excal-ungroup "s-G" #'excal-ungroup
  "C-c ]" #'excal-bring-forward "s-]" #'excal-bring-forward
  "C-c [" #'excal-send-backward "s-[" #'excal-send-backward
  "C-c }" #'excal-bring-to-front "s-}" #'excal-bring-to-front
  "C-c {" #'excal-send-to-back "s-{" #'excal-send-to-back
  "<left>" #'excal-nudge-left "<right>" #'excal-nudge-right
  "<up>" #'excal-nudge-up "<down>" #'excal-nudge-down
  "S-<left>" #'excal-nudge-left-large "S-<right>" #'excal-nudge-right-large
  "S-<up>" #'excal-nudge-up-large "S-<down>" #'excal-nudge-down-large
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
        excal--backend (excal--resolve-backend))
  (add-hook 'kill-buffer-hook #'excal--hide-layer nil t)
  (add-hook 'window-size-change-functions #'excal--window-size-change)
  (add-hook 'window-buffer-change-functions #'excal--window-size-change)
  ;; One undo step per command that changed the scene.
  (add-hook 'post-command-hook #'excal--commit nil t))

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
    (excal--load-current-style (alist-get 'appState doc))
    (excal--history-reset)
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
