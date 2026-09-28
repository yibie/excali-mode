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
(require 'excal-index)
(require 'excal-restore)
(require 'excal-text)
(require 'excal-view)
(require 'excal-select)
(require 'excal-handles)
(require 'excal-transform)
(require 'excal-hit)
(require 'excal-style)
(require 'excal-edit)
(require 'excal-cursor)
(require 'excal-history)
(require 'excal-clipboard)
(require 'excal-create)
(require 'excal-actions)
(require 'excal-linear)
(require 'excal-elbow)
(require 'excal-flowchart)
(require 'excal-snap)
(require 'excal-frame)
(require 'excal-erase)
(require 'excal-tools)
(require 'excal-bucket)
(require 'excal-library)
(require 'excal-image)
(require 'excal-export)
(require 'excal-bench)

;;;; Keys
;;
;; Plain keys follow Excalidraw.  Its Mod (Cmd on macOS) shortcuts are
;; bound with the super modifier, which is Cmd in the NS port; the
;; familiar Emacs keys (C-/, M-w, C-y, C-x C-s, ...) work as well.

(defmacro excal--tool-command (tool)
  "Return a command selecting TOOL."
  `(lambda ()
     ,(format "Select the %s tool." tool)
     (interactive)
     (excal-select-tool ',tool)
     (excal--update-pointer)
     (message "Tool: %s" excal--tool)))


(defun excal-return ()
  "Finish drawing points, edit the selected line's points, or edit text."
  (interactive)
  (let ((single (excal--single-selection)))
    (cond (excal--multi-element (excal-finish-multi-point))
          ((and single (excal--frame-p single)) (excal-rename-frame))
          ((and single (equal (excal--get single 'type) "line")
                (not excal--editing-linear))
           (excal-edit-linear))
          (t (excal-edit-text)))))

(defun excal-escape-dwim ()
  "Finish drawing points, leave the point editor or the entered group,
or deselect."
  (interactive)
  (cond (excal--multi-element (excal-finish-multi-point))
        (excal--editing-linear (excal-stop-editing-linear))
        (t (excal-escape))))

(defvar-keymap excal-mode-map
  ;; Mouse.
  "<down-mouse-1>" #'excal-mouse-down
  "S-<down-mouse-1>" #'excal-mouse-down
  "M-<down-mouse-1>" #'excal-mouse-down
  "M-S-<down-mouse-1>" #'excal-mouse-down
  ;; Mod+Alt drags a lasso from the selection tool.
  "C-M-<down-mouse-1>" #'excal-mouse-down
  "M-s-<down-mouse-1>" #'excal-mouse-down
  "<double-down-mouse-1>" #'excal-double-click
  "<down-mouse-2>" #'excal-mouse-pan
  "<mouse-movement>" #'excal-mouse-move
  ;; The canvas is one character: Emacs's region and secondary selection
  ;; commands would only highlight it whole, whichever click reaches them.
  "<remap> <mouse-drag-region>" #'ignore
  "<remap> <mouse-drag-region-rectangle>" #'ignore
  "<remap> <mouse-set-region>" #'ignore
  "<remap> <mouse-save-then-kill>" #'ignore
  "<remap> <mouse-drag-secondary>" #'ignore
  "<remap> <mouse-start-secondary>" #'ignore
  "<remap> <mouse-set-secondary>" #'ignore
  "<remap> <mouse-secondary-save-then-kill>" #'ignore
  "<wheel-up>" #'excal-wheel "<wheel-down>" #'excal-wheel
  "<wheel-left>" #'excal-wheel "<wheel-right>" #'excal-wheel
  "C-<wheel-up>" #'excal-wheel "C-<wheel-down>" #'excal-wheel
  "<pinch>" #'excal-pinch
  ;; Tools.
  "h" (excal--tool-command hand)
  "v" (excal--tool-command select) "1" (excal--tool-command select)
  "r" (excal--tool-command rectangle) "2" (excal--tool-command rectangle)
  "d" (excal--tool-command diamond) "3" (excal--tool-command diamond)
  "o" (excal--tool-command ellipse) "4" (excal--tool-command ellipse)
  "a" (excal--tool-command arrow) "5" (excal--tool-command arrow)
  "l" (excal--tool-command line) "6" (excal--tool-command line)
  "p" (excal--tool-command freedraw) "x" (excal--tool-command freedraw)
  "7" (excal--tool-command freedraw)
  "t" (excal--tool-command text) "8" (excal--tool-command text)
  "e" (excal--tool-command eraser) "0" (excal--tool-command eraser)
  "f" (excal--tool-command frame)
  "n" (excal--tool-command stickynote)
  "9" #'excal-insert-image
  "k" (excal--tool-command laser)
  "b" (excal--tool-command bucketfill)
  "X" (excal--tool-command autoshape)
  "i" #'excal-eyedropper "G" #'excal-eyedropper "S" #'excal-eyedropper-stroke
  "q" #'excal-toggle-tool-lock
  ;; Style.
  "s" #'excal-style
  "g" #'excal-style-backgroundColor
  "F" #'excal-style-fontFamily
  "s-<" #'excal-decrease-font-size "s->" #'excal-increase-font-size
  "M-s-c" #'excal-copy-styles "M-s-v" #'excal-paste-styles
  ;; Editing.
  "RET" #'excal-return "s-<return>" #'excal-edit-linear-any
  "s-<double-down-mouse-1>" #'excal-double-click
  "<escape>" #'excal-escape-dwim
  "<delete>" #'excal-delete-selected "DEL" #'excal-delete-selected
  "<left>" #'excal-nudge-left "<right>" #'excal-nudge-right
  "<up>" #'excal-nudge-up "<down>" #'excal-nudge-down
  "S-<left>" #'excal-nudge-left-large "S-<right>" #'excal-nudge-right-large
  "S-<up>" #'excal-nudge-up-large "S-<down>" #'excal-nudge-down-large
  "H" #'excal-flip-horizontal "V" #'excal-flip-vertical
  "TAB" #'excal-convert-type "<backtab>" #'excal-convert-type-backward
  "M-h" #'excal-distribute-horizontally "M-v" #'excal-distribute-vertically
  "S-s-<up>" #'excal-align-top "S-s-<down>" #'excal-align-bottom
  "S-s-<left>" #'excal-align-left "S-s-<right>" #'excal-align-right
  ;; Flowcharts: Mod+Arrow adds linked nodes, Alt+Arrow walks them.
  "s-<up>" #'excal-flowchart-up "s-<down>" #'excal-flowchart-down
  "s-<left>" #'excal-flowchart-left "s-<right>" #'excal-flowchart-right
  "C-<up>" #'excal-flowchart-up "C-<down>" #'excal-flowchart-down
  "C-<left>" #'excal-flowchart-left "C-<right>" #'excal-flowchart-right
  "M-<up>" #'excal-flowchart-navigate-up "M-<down>" #'excal-flowchart-navigate-down
  "M-<left>" #'excal-flowchart-navigate-left "M-<right>" #'excal-flowchart-navigate-right
  "s-L" #'excal-toggle-lock
  "s-k" #'excal-set-link
  "C-c l a" #'excal-library-add "C-c l i" #'excal-library-insert
  "C-c l b" #'excal-library-browse
  "s-z" #'excal-undo "s-Z" #'excal-redo "s-y" #'excal-redo
  "C-/" #'excal-undo "C-_" #'excal-undo "C-x u" #'excal-undo
  "C-?" #'excal-redo "C-M-_" #'excal-redo
  "s-c" #'excal-copy "M-w" #'excal-copy
  "s-x" #'excal-cut "C-w" #'excal-cut
  "s-v" #'excal-paste "C-y" #'excal-paste
  "s-d" #'excal-duplicate "C-c C-d" #'excal-duplicate
  "s-a" #'excal-select-all "C-x h" #'excal-select-all
  "s-g" #'excal-group "C-c C-g" #'excal-group
  "s-G" #'excal-ungroup "C-c C-u" #'excal-ungroup
  "s-]" #'excal-bring-forward "C-c ]" #'excal-bring-forward
  "s-[" #'excal-send-backward "C-c [" #'excal-send-backward
  "M-s-]" #'excal-bring-to-front "C-c }" #'excal-bring-to-front
  "M-s-[" #'excal-send-to-back "C-c {" #'excal-send-to-back
  ;; View.
  "s-=" #'excal-zoom-in "+" #'excal-zoom-in "C-x C-=" #'excal-zoom-in
  "s--" #'excal-zoom-out "_" #'excal-zoom-out "C-x C--" #'excal-zoom-out
  "s-0" #'excal-zoom-reset "C-x C-0" #'excal-zoom-reset
  "!" #'excal-zoom-to-fit "@" #'excal-zoom-to-fit-selection-in-viewport
  "#" #'excal-zoom-to-fit-selection
  "<prior>" #'excal-page-up "<next>" #'excal-page-down
  "S-<prior>" #'excal-page-left "S-<next>" #'excal-page-right
  ;; Files and debugging.
  "s-'" #'excal-toggle-grid "M-s" #'excal-toggle-objects-snap
  "M-D" #'excal-toggle-theme
  "s-s" #'excal-save "C-x C-s" #'excal-save
  "s-E" #'excal-export-image "C-c C-e" #'excal-export-image
  "C-c C-b" #'excal-cycle-backend
  "C-c C-p" #'excal-toggle-pixel-scale
  "C-c C-r" #'excal--sync-canvas
  "?" #'describe-mode)

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
  (add-hook 'kill-buffer-hook #'excal--hide-cursor-view nil t)
  (add-hook 'window-size-change-functions #'excal--window-size-change)
  (add-hook 'window-buffer-change-functions #'excal--window-size-change)
  ;; One undo step per command that changed the scene.
  (add-hook 'pre-command-hook #'excal--flowchart-pre-command nil t)
  (add-hook 'post-command-hook #'excal--commit nil t))

(defun excal--open (doc file name)
  "Show DOC saved to FILE in a buffer called NAME.
DOC is a parsed .excalidraw file; it is restored (migrated and repaired,
see `excal--restore-doc') before anything else sees it."
  (unless (and (display-graphic-p) (image-type-available-p 'canvas))
    (error "excal needs a graphical Emacs with Canvas images"))
  (let ((doc (excal--restore-doc doc))
        (buffer (generate-new-buffer name)))
    (pop-to-buffer-same-window buffer)
    (excal-mode)
    ;; Keep fractional indices valid after every command, before
    ;; `excal--commit' snapshots the scene; see excal-index.el.
    (add-hook 'post-command-hook #'excal--sync-indices-maybe -50 t)
    (setq excal--file file
          excal--doc doc
          excal--elements (append (alist-get 'elements doc) nil))
    (excal--load-current-style (alist-get 'appState doc))
    (excal--load-grid-state (alist-get 'appState doc))
    (excal--history-reset)
    (excal--sync-canvas (selected-window))
    buffer))

;;;###autoload
(defun excal-open (file)
  "Open .excalidraw FILE, or the scene embedded in a PNG or SVG FILE.
A scene read from an image is saved to a new .excalidraw file."
  (interactive "fExcalidraw file: ")
  (let ((file (expand-file-name file)))
    (excal--open (excal--read-scene-file file)
                 (and (excal--scene-file-p file) file)
                 (format "*excal %s*" (file-name-nondirectory file)))))

;;;###autoload
(defun excal-new ()
  "Start an empty scene."
  (interactive)
  (excal--open (excal--empty-doc) nil "*excal new*"))

(provide 'excal)
;;; excal.el ends here
