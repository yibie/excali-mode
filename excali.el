;;; excali.el --- Excalidraw scenes on Emacs Canvas  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie

;; Author: yibie <yibie@outlook.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "32.0"))
;; Keywords: multimedia, tools
;; URL: https://github.com/yibie/excali-mode

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Excalidraw for Emacs: read and write .excalidraw files, render them
;; through a Cairo/Pango module into a Canvas image, and support basic
;; drawing, selection, panning and zooming.
;;
;; Entry points: `excali-open', `excali-new'.

;;; Code:

(require 'excali-core)
(require 'excali-index)
(require 'excali-restore)
(require 'excali-text)
(require 'excali-view)
(require 'excali-select)
(require 'excali-handles)
(require 'excali-transform)
(require 'excali-hit)
(require 'excali-style)
(require 'excali-edit)
(require 'excali-cursor)
(require 'excali-history)
(require 'excali-clipboard)
(require 'excali-create)
(require 'excali-actions)
(require 'excali-linear)
(require 'excali-elbow)
(require 'excali-flowchart)
(require 'excali-snap)
(require 'excali-frame)
(require 'excali-erase)
(require 'excali-tools)
(require 'excali-bucket)
(require 'excali-library)
(require 'excali-image)
(require 'excali-export)
(require 'excali-bench)

;;;; Keys
;;
;; Plain keys follow Excalidraw.  Its Mod (Cmd on macOS) shortcuts are
;; bound with the super modifier, which is Cmd in the NS port; the
;; familiar Emacs keys (C-/, M-w, C-y, C-x C-s, ...) work as well.

(defmacro excali--tool-command (tool)
  "Return a command selecting TOOL."
  `(lambda ()
     ,(format "Select the %s tool." tool)
     (interactive)
     (excali-select-tool ',tool)
     (message "Tool: %s" excali--tool)))


(defun excali-return ()
  "Finish drawing points, edit the selected line's points, or edit text."
  (interactive)
  (let ((single (excali--single-selection)))
    (cond (excali--multi-element (excali-finish-multi-point))
          ((and single (excali--frame-p single)) (excali-rename-frame))
          ((and single (equal (excali--get single 'type) "line")
                (not excali--editing-linear))
           (excali-edit-linear))
          (t (excali-edit-text)))))

(defun excali-escape-dwim ()
  "Finish drawing points, leave the point editor or the entered group,
or deselect."
  (interactive)
  (cond (excali--multi-element (excali-finish-multi-point))
        (excali--editing-linear (excali-stop-editing-linear))
        (t (excali-escape))))

(defvar-keymap excali-mode-map
  ;; Mouse.
  "<down-mouse-1>" #'excali-mouse-down
  "S-<down-mouse-1>" #'excali-mouse-down
  "M-<down-mouse-1>" #'excali-mouse-down
  "M-S-<down-mouse-1>" #'excali-mouse-down
  ;; Mod+Alt drags a lasso from the selection tool.
  "C-M-<down-mouse-1>" #'excali-mouse-down
  "M-s-<down-mouse-1>" #'excali-mouse-down
  "<double-down-mouse-1>" #'excali-double-click
  "<down-mouse-2>" #'excali-mouse-pan
  "<down-mouse-3>" #'excali-mouse-pan
  "<mouse-movement>" #'excali-mouse-move
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
  "<wheel-up>" #'excali-wheel "<wheel-down>" #'excali-wheel
  "<wheel-left>" #'excali-wheel "<wheel-right>" #'excali-wheel
  "C-<wheel-up>" #'excali-wheel "C-<wheel-down>" #'excali-wheel
  "<pinch>" #'excali-pinch
  ;; Tools.
  "h" (excali--tool-command hand)
  "v" (excali--tool-command select) "1" (excali--tool-command select)
  "r" (excali--tool-command rectangle) "2" (excali--tool-command rectangle)
  "d" (excali--tool-command diamond) "3" (excali--tool-command diamond)
  "o" (excali--tool-command ellipse) "4" (excali--tool-command ellipse)
  "a" (excali--tool-command arrow) "5" (excali--tool-command arrow)
  "l" (excali--tool-command line) "6" (excali--tool-command line)
  "p" (excali--tool-command freedraw) "x" (excali--tool-command freedraw)
  "7" (excali--tool-command freedraw)
  "t" (excali--tool-command text) "8" (excali--tool-command text)
  "e" (excali--tool-command eraser) "0" (excali--tool-command eraser)
  "f" (excali--tool-command frame)
  "n" (excali--tool-command stickynote)
  "9" #'excali-insert-image
  "k" (excali--tool-command laser)
  "b" (excali--tool-command bucketfill)
  "X" (excali--tool-command autoshape)
  "i" #'excali-eyedropper "G" #'excali-eyedropper "S" #'excali-eyedropper-stroke
  "q" #'excali-toggle-tool-lock
  ;; Style.
  "s" #'excali-style
  "g" #'excali-style-backgroundColor
  "F" #'excali-style-fontFamily
  "s-<" #'excali-decrease-font-size "s->" #'excali-increase-font-size
  "M-s-c" #'excali-copy-styles "M-s-v" #'excali-paste-styles
  ;; Editing.
  "RET" #'excali-return "s-<return>" #'excali-edit-linear-any
  "s-<double-down-mouse-1>" #'excali-double-click
  "<escape>" #'excali-escape-dwim
  "<delete>" #'excali-delete-selected "DEL" #'excali-delete-selected
  "<left>" #'excali-nudge-left "<right>" #'excali-nudge-right
  "<up>" #'excali-nudge-up "<down>" #'excali-nudge-down
  "S-<left>" #'excali-nudge-left-large "S-<right>" #'excali-nudge-right-large
  "S-<up>" #'excali-nudge-up-large "S-<down>" #'excali-nudge-down-large
  "H" #'excali-flip-horizontal "V" #'excali-flip-vertical
  "TAB" #'excali-convert-type "<backtab>" #'excali-convert-type-backward
  "M-h" #'excali-distribute-horizontally "M-v" #'excali-distribute-vertically
  "S-s-<up>" #'excali-align-top "S-s-<down>" #'excali-align-bottom
  "S-s-<left>" #'excali-align-left "S-s-<right>" #'excali-align-right
  ;; Flowcharts: Mod+Arrow adds linked nodes, Alt+Arrow walks them.
  "s-<up>" #'excali-flowchart-up "s-<down>" #'excali-flowchart-down
  "s-<left>" #'excali-flowchart-left "s-<right>" #'excali-flowchart-right
  "C-<up>" #'excali-flowchart-up "C-<down>" #'excali-flowchart-down
  "C-<left>" #'excali-flowchart-left "C-<right>" #'excali-flowchart-right
  "M-<up>" #'excali-flowchart-navigate-up "M-<down>" #'excali-flowchart-navigate-down
  "M-<left>" #'excali-flowchart-navigate-left "M-<right>" #'excali-flowchart-navigate-right
  "s-L" #'excali-toggle-lock
  "s-k" #'excali-set-link
  "C-c l a" #'excali-library-add "C-c l i" #'excali-library-insert
  "C-c l b" #'excali-library-browse "C-c l d" #'excali-library-remove
  "C-c l o" #'excali-library-browse-official
  "s-z" #'excali-undo "s-Z" #'excali-redo "s-y" #'excali-redo
  "C-/" #'excali-undo "C-_" #'excali-undo "C-x u" #'excali-undo
  "C-?" #'excali-redo "C-M-_" #'excali-redo
  "s-c" #'excali-copy "M-w" #'excali-copy
  "s-x" #'excali-cut "C-w" #'excali-cut
  "s-v" #'excali-paste "C-y" #'excali-paste
  "s-d" #'excali-duplicate "C-c C-d" #'excali-duplicate
  "s-a" #'excali-select-all "C-x h" #'excali-select-all
  "s-g" #'excali-group "C-c C-g" #'excali-group
  "s-G" #'excali-ungroup "C-c C-u" #'excali-ungroup
  "s-]" #'excali-bring-forward "C-c ]" #'excali-bring-forward
  "s-[" #'excali-send-backward "C-c [" #'excali-send-backward
  "M-s-]" #'excali-bring-to-front "C-c }" #'excali-bring-to-front
  "M-s-[" #'excali-send-to-back "C-c {" #'excali-send-to-back
  ;; View.
  "s-=" #'excali-zoom-in "+" #'excali-zoom-in "C-x C-=" #'excali-zoom-in
  "s--" #'excali-zoom-out "_" #'excali-zoom-out "C-x C--" #'excali-zoom-out
  "s-0" #'excali-zoom-reset "C-x C-0" #'excali-zoom-reset
  "!" #'excali-zoom-to-fit "@" #'excali-zoom-to-fit-selection-in-viewport
  "#" #'excali-zoom-to-fit-selection
  "<prior>" #'excali-page-up "<next>" #'excali-page-down
  "S-<prior>" #'excali-page-left "S-<next>" #'excali-page-right
  ;; Files and debugging.
  "s-'" #'excali-toggle-grid "M-s" #'excali-toggle-objects-snap
  "M-D" #'excali-toggle-theme
  "s-s" #'excali-save "C-x C-s" #'excali-save
  "s-E" #'excali-export-image "C-c C-e" #'excali-export-image
  "C-c C-b" #'excali-cycle-backend
  "C-c C-p" #'excali-toggle-pixel-scale
  "C-c C-r" #'excali--sync-canvas
  "?" #'describe-mode)

;; Mouse events over the canvas's hot spots (excali-cursor.el) arrive
;; with the prefix key `excali-canvas'.  Under it the mode's own bindings
;; apply; anything else under it is ignored rather than undefined.
(let ((canvas (make-sparse-keymap)))
  (set-keymap-parent canvas excali-mode-map)
  (define-key canvas [t] #'ignore)
  (define-key excali-mode-map [excali-canvas] canvas))

(define-derived-mode excali-mode special-mode "Excali"
  "Major mode for editing Excalidraw scenes on a Canvas image."
  (setq-local cursor-type nil
              ;; Report plain mouse motion so the pointer can follow the
              ;; scene; see `excali-mouse-move'.
              track-mouse t
              truncate-lines t
              line-spacing nil
              mode-line-process '(:eval (format " %s %s %d%%" excali--backend
                                                excali--tool
                                                (round (* 100 excali--zoom)))))
  (setq excali--native-cache (make-hash-table :test #'eq :weakness 'key)
        excali--pixel-scale (excali--guess-pixel-scale)
        excali--backend (excali--resolve-backend))
  (add-hook 'kill-buffer-hook #'excali--hide-layer nil t)
  (add-hook 'kill-buffer-hook #'excali--hide-cursor-view nil t)
  (add-hook 'window-size-change-functions #'excali--window-size-change)
  (add-hook 'window-buffer-change-functions #'excali--window-size-change)
  ;; One undo step per command that changed the scene.
  (add-hook 'pre-command-hook #'excali--flowchart-pre-command nil t)
  (add-hook 'post-command-hook #'excali--commit nil t)
  (add-hook 'post-command-hook #'excali--schedule-pointer-update nil t))

(defun excali--open (doc file name)
  "Show DOC saved to FILE in a buffer called NAME.
DOC is a parsed .excalidraw file; it is restored (migrated and repaired,
see `excali--restore-doc') before anything else sees it."
  (unless (and (display-graphic-p) (image-type-available-p 'canvas))
    (error "excali needs a graphical Emacs with Canvas images"))
  (let ((doc (excali--restore-doc doc))
        (buffer (generate-new-buffer name)))
    (pop-to-buffer-same-window buffer)
    (excali-mode)
    ;; Keep fractional indices valid after every command, before
    ;; `excali--commit' snapshots the scene; see excali-index.el.
    (add-hook 'post-command-hook #'excali--sync-indices-maybe -50 t)
    (setq excali--file file
          excali--doc doc
          excali--elements (append (alist-get 'elements doc) nil))
    (excali--load-current-style (alist-get 'appState doc))
    (excali--load-grid-state (alist-get 'appState doc))
    (excali--history-reset)
    (excali--sync-canvas (selected-window))
    buffer))

;;;###autoload
(defun excali-open (file)
  "Open .excalidraw FILE, or the scene embedded in a PNG or SVG FILE.
A scene read from an image is saved to a new .excalidraw file."
  (interactive "fExcalidraw file: ")
  (let ((file (expand-file-name file)))
    (excali--open (excali--read-scene-file file)
                 (and (excali--scene-file-p file) file)
                 (format "*excali %s*" (file-name-nondirectory file)))))

;;;###autoload
(defun excali-new ()
  "Start an empty scene."
  (interactive)
  (excali--open (excali--empty-doc) nil "*excali new*"))

(provide 'excali)
;;; excali.el ends here
