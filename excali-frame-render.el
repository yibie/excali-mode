;;; excali-frame-render.el --- Frame titles, name labels and render data  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Rendering-side helpers for frames (`frame' and `magicframe'):
;; upstream `getFrameLikeTitle', the geometry of the name label shown
;; above a frame on the live canvas (for hit testing, see
;; `excali--frame-name-bounds'), and the render data the module needs
;; to draw frames and clip their children (excali-frame.c).  Frame
;; behaviour (membership, moving children, the frame tool) lives
;; elsewhere.

;;; Code:

(require 'excali-core)

(declare-function excali-native-frame-label "excali-module")

;; FRAME_STYLE (packages/common/src/constants.ts).
(defconst excali-frame-name-offset-y 3
  "Gap in screen pixels between a frame's top edge and its name label.")
(defconst excali-frame-name-font-size 14
  "Font size in screen pixels of frame names (FRAME_STYLE.nameFontSize).")
(defconst excali-frame-name-line-height 1.25
  "Unitless line height of frame names (FRAME_STYLE.nameLineHeight).")
(defconst excali-frame-name-color "#999999"
  "Color of frame names in the light theme.")
(defconst excali-frame-default-name "Frame"
  "Title of a frame whose name is null (DEFAULT_FRAME_NAME).")
(defconst excali-magic-frame-default-name "AI Frame"
  "Title of a magic frame whose name is null (DEFAULT_AI_FRAME_NAME).")

(defun excali--frame-like-p (element)
  "Return non-nil if ELEMENT is a frame or a magic frame."
  (member (excali--get element 'type) '("frame" "magicframe")))

(defun excali--frame-title (frame)
  "Return FRAME's title, upstream `getFrameLikeTitle'."
  (let ((name (excali--get frame 'name)))
    (if (stringp name)
        name
      (if (equal (excali--get frame 'type) "magicframe")
          excali-magic-frame-default-name
        excali-frame-default-name))))

(defun excali--frame-label (frame &optional zoom)
  "Return (TEXT . WIDTH) of FRAME's name label at ZOOM.
TEXT is the title cut with an ellipsis to the frame's width on screen,
as the label's CSS `text-overflow: ellipsis' does; WIDTH is in screen
pixels.  ZOOM defaults to the view's."
  (let ((zoom (or zoom excali--zoom)))
    (excali-native-frame-label (excali--frame-title frame)
                              (* (abs (or (excali--get frame 'width) 0)) zoom))))

(defun excali--frame-name-bounds (frame &optional zoom)
  "Return the scene rectangle (X1 Y1 X2 Y2) of FRAME's name label.
The label is drawn at a constant screen size, so the rectangle depends
on ZOOM (default: the view's).  This mirrors upstream
`frameNameBoundsCache': x = frame.x, bottom 3px above the frame,
height one line of 14px text, width the (truncated) title's."
  (let* ((zoom (or zoom excali--zoom))
         (x (min (excali--get frame 'x)
                 (+ (excali--get frame 'x) (excali--get frame 'width))))
         (top (min (excali--get frame 'y)
                   (+ (excali--get frame 'y) (excali--get frame 'height))))
         (height (* excali-frame-name-font-size excali-frame-name-line-height))
         (width (cdr (excali--frame-label frame zoom)))
         (y2 (- top (/ (float excali-frame-name-offset-y) zoom))))
    (list (float x) (- y2 (/ height zoom)) (+ x (/ width zoom)) y2)))

(defun excali--frame-native-extras (element)
  "Return the frame render data of ELEMENT as a list (KEY VALUE ...).
Frames give their \"id\" and \"name\" (title), magic frames also
\"magic\"; elements in a frame give its \"frame-id\", and grouped ones
\"grouped\" (see `shouldApplyFrameClip' in excali-frame.c)."
  (let (out)
    (when (excali--frame-like-p element)
      (setq out (list "id" (excali--get element 'id)
                      "name" (excali--frame-title element)))
      (when (equal (excali--get element 'type) "magicframe")
        (setq out (append out (list "magic" t)))))
    (let ((frame (excali--get element 'frameId)))
      (when (and (stringp frame) (not (string-empty-p frame)))
        (setq out (append out (list "frame-id" frame)))))
    (when (> (length (excali--get element 'groupIds)) 0)
      (setq out (append out (list "grouped" t))))
    out))

(provide 'excali-frame-render)
;;; excali-frame-render.el ends here
