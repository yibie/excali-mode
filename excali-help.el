;;; excali-help.el --- The keyboard shortcuts panel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `excali-help' (`?') groups the canvas's keys the way upstream's
;; keyboard shortcuts dialog does.  Every suffix uses the key that
;; `excali-mode-map' binds to the same command, so the panel teaches the
;; keys and also runs them; test/excali-help-test.el keeps the two in
;; step.  Keys transient keeps for itself (arrows, TAB, ESC) and mouse
;; gestures are listed as text.  `?' again shows `describe-mode', the
;; complete list.

;;; Code:

(require 'transient)

;;;###autoload (autoload 'excali-help "excali-help" nil t)
(transient-define-prefix excali-help ()
  "Show the canvas's keys, grouped; each key also runs its command.
Press ? again for every binding."
  [["Tools"
    ("v" "Select" excali-tool-select)
    ("h" "Hand (pan)" excali-tool-hand)
    ("r" "Rectangle" excali-tool-rectangle)
    ("d" "Diamond" excali-tool-diamond)
    ("o" "Ellipse" excali-tool-ellipse)
    ("a" "Arrow (again: type)" excali-tool-arrow)
    ("l" "Line" excali-tool-line)
    ("p" "Draw" excali-tool-freedraw)
    ("t" "Text" excali-tool-text)
    ("n" "Sticky note" excali-tool-stickynote)]
   ["More tools"
    ("e" "Eraser" excali-tool-eraser)
    ("f" "Frame" excali-tool-frame)
    ("b" "Bucket fill (again: color)" excali-tool-bucketfill)
    ("X" "Autoshape" excali-tool-autoshape)
    ("k" "Laser pointer" excali-tool-laser)
    ("9" "Insert image" excali-insert-image)
    ("i" "Eye dropper" excali-eyedropper)
    ("q" "Keep tool after drawing" excali-toggle-tool-lock)]
   ["Style"
    ("s" "Style panel" excali-style)
    ("g" "Background color" excali-style-backgroundColor)
    ("F" "Font family" excali-style-fontFamily)
    ("s-<" "Smaller text" excali-decrease-font-size)
    ("s->" "Larger text" excali-increase-font-size)
    ("M-s-c" "Copy styles" excali-copy-styles)
    ("M-s-v" "Paste styles" excali-paste-styles)]]
  [["Edit"
    ("RET" "Edit text / line points" excali-return)
    ("DEL" "Delete" excali-delete-selected)
    ("s-d" "Duplicate" excali-duplicate)
    ("s-a" "Select all" excali-select-all)
    ("s-g" "Group" excali-group)
    ("s-G" "Ungroup" excali-ungroup)
    ("s-L" "Lock / unlock" excali-toggle-lock)
    ("s-k" "Link" excali-set-link)
    ("H" "Flip horizontal" excali-flip-horizontal)
    ("V" "Flip vertical" excali-flip-vertical)]
   ["History & clipboard"
    ("s-z" "Undo" excali-undo)
    ("s-Z" "Redo" excali-redo)
    ("s-c" "Copy" excali-copy)
    ("s-x" "Cut" excali-cut)
    ("s-v" "Paste" excali-paste)]
   ["Layers"
    ("s-]" "Bring forward" excali-bring-forward)
    ("s-[" "Send backward" excali-send-backward)
    ("M-s-]" "Bring to front" excali-bring-to-front)
    ("M-s-[" "Send to back" excali-send-to-back)]]
  [["View"
    ("+" "Zoom in" excali-zoom-in)
    ("_" "Zoom out" excali-zoom-out)
    ("s-0" "Reset zoom" excali-zoom-reset)
    ("!" "Fit all" excali-zoom-to-fit)
    ("#" "Fit selection" excali-zoom-to-fit-selection)
    ("s-'" "Grid" excali-toggle-grid)
    ("M-s" "Snap to objects" excali-toggle-objects-snap)
    ("M-D" "Dark / light" excali-toggle-theme)]
   ["File"
    ("s-s" "Save" excali-save)
    ("s-E" "Export image" excali-export-image)
    ("?" "All keys" describe-mode)]]
  [["On the canvas only"
    (:info "ESC         cancel, leave, deselect")
    (:info "(S-)arrows  nudge")
    (:info "S-s-arrows  align")
    (:info "s-arrows    add flowchart node")
    (:info "M-arrows    walk flowchart")
    (:info "TAB         rectangle/diamond/ellipse")]
   ["Mouse"
    (:info "double-click       edit text or label")
    (:info "middle/right drag  pan")
    (:info "wheel              scroll (S-: sideways)")
    (:info "C-wheel            zoom")
    (:info "M-click            bucket: pick color")]])

(provide 'excali-help)
;;; excali-help.el ends here
