;;; excal-frame-render.el --- Frame titles, name labels and render data  -*- lexical-binding: t; -*-

;;; Commentary:

;; Rendering-side helpers for frames (`frame' and `magicframe'):
;; upstream `getFrameLikeTitle', the geometry of the name label shown
;; above a frame on the live canvas (for hit testing, see
;; `excal--frame-name-bounds'), and the render data the module needs
;; to draw frames and clip their children (excal-frame.c).  Frame
;; behaviour (membership, moving children, the frame tool) lives
;; elsewhere.

;;; Code:

(require 'excal-core)

(declare-function excal-native-frame-label "excal-module")

;; FRAME_STYLE (packages/common/src/constants.ts).
(defconst excal-frame-name-offset-y 3
  "Gap in screen pixels between a frame's top edge and its name label.")
(defconst excal-frame-name-font-size 14
  "Font size in screen pixels of frame names (FRAME_STYLE.nameFontSize).")
(defconst excal-frame-name-line-height 1.25
  "Unitless line height of frame names (FRAME_STYLE.nameLineHeight).")
(defconst excal-frame-name-color "#999999"
  "Color of frame names in the light theme.")
(defconst excal-frame-default-name "Frame"
  "Title of a frame whose name is null (DEFAULT_FRAME_NAME).")
(defconst excal-magic-frame-default-name "AI Frame"
  "Title of a magic frame whose name is null (DEFAULT_AI_FRAME_NAME).")

(defun excal--frame-like-p (element)
  "Return non-nil if ELEMENT is a frame or a magic frame."
  (member (excal--get element 'type) '("frame" "magicframe")))

(defun excal--frame-title (frame)
  "Return FRAME's title, upstream `getFrameLikeTitle'."
  (let ((name (excal--get frame 'name)))
    (if (stringp name)
        name
      (if (equal (excal--get frame 'type) "magicframe")
          excal-magic-frame-default-name
        excal-frame-default-name))))

(defun excal--frame-label (frame &optional zoom)
  "Return (TEXT . WIDTH) of FRAME's name label at ZOOM.
TEXT is the title cut with an ellipsis to the frame's width on screen,
as the label's CSS `text-overflow: ellipsis' does; WIDTH is in screen
pixels.  ZOOM defaults to the view's."
  (let ((zoom (or zoom excal--zoom)))
    (excal-native-frame-label (excal--frame-title frame)
                              (* (abs (or (excal--get frame 'width) 0)) zoom))))

(defun excal--frame-name-bounds (frame &optional zoom)
  "Return the scene rectangle (X1 Y1 X2 Y2) of FRAME's name label.
The label is drawn at a constant screen size, so the rectangle depends
on ZOOM (default: the view's).  This mirrors upstream
`frameNameBoundsCache': x = frame.x, bottom 3px above the frame,
height one line of 14px text, width the (truncated) title's."
  (let* ((zoom (or zoom excal--zoom))
         (x (min (excal--get frame 'x)
                 (+ (excal--get frame 'x) (excal--get frame 'width))))
         (top (min (excal--get frame 'y)
                   (+ (excal--get frame 'y) (excal--get frame 'height))))
         (height (* excal-frame-name-font-size excal-frame-name-line-height))
         (width (cdr (excal--frame-label frame zoom)))
         (y2 (- top (/ (float excal-frame-name-offset-y) zoom))))
    (list (float x) (- y2 (/ height zoom)) (+ x (/ width zoom)) y2)))

(defun excal--frame-native-extras (element)
  "Return the frame render data of ELEMENT as a list (KEY VALUE ...).
Frames give their \"id\" and \"name\" (title), magic frames also
\"magic\"; elements in a frame give its \"frame-id\", and grouped ones
\"grouped\" (see `shouldApplyFrameClip' in excal-frame.c)."
  (let (out)
    (when (excal--frame-like-p element)
      (setq out (list "id" (excal--get element 'id)
                      "name" (excal--frame-title element)))
      (when (equal (excal--get element 'type) "magicframe")
        (setq out (append out (list "magic" t)))))
    (let ((frame (excal--get element 'frameId)))
      (when (and (stringp frame) (not (string-empty-p frame)))
        (setq out (append out (list "frame-id" frame)))))
    (when (> (length (excal--get element 'groupIds)) 0)
      (setq out (append out (list "grouped" t))))
    out))

(provide 'excal-frame-render)
;;; excal-frame-render.el ends here
