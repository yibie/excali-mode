;;; excal-cursor.el --- Pointer shapes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Which pointer shape the canvas shows, following upstream's cursor.ts
;; (`setCursorForShape', `getCursorForResizingElement') and the hover
;; logic of App.tsx `handleCanvasPointerMove'.  Shapes are named like the
;; CSS cursors upstream sets: `default', `pointer', `move', `grab',
;; `grabbing', `crosshair', `text', `ns-resize', `ew-resize',
;; `nwse-resize', `nesw-resize', `not-allowed', and the custom `eraser'
;; (`eraser-dark' in the dark theme).
;;
;; On macOS the module shows them all (src/excal-cursor.m): a transparent
;; view over the canvas owns the pointer there, while Emacs' own `pointer'
;; property stays `arrow' so Emacs never fights over it.  Elsewhere, or
;; without the module, each shape falls back to the nearest one Emacs'
;; `pointer' property offers.

;;; Code:

(require 'excal-core)
(require 'excal-select)
(require 'excal-handles)
(require 'excal-hit)

(declare-function excal--in-selection-box-p "excal-edit")
(declare-function excal--link-at "excal-erase")
(declare-function excal--linear-target "excal-linear")
(declare-function excal--point-at "excal-linear")
(declare-function excal--midpoint-at "excal-linear")
(declare-function excal--elbow-p "excal-elbow")
(declare-function excal--elbow-end-at "excal-elbow")
(declare-function excal--elbow-midpoint-at "excal-elbow")
(declare-function excal-native-cursor-view-create "excal-module")
(declare-function excal-native-cursor-view-set-geometry "excal-module")
(declare-function excal-native-cursor-set "excal-module")
(defvar excal--multi-element)
(defvar excal--editing-linear)

(defcustom excal-native-cursors t
  "Non-nil means show Excalidraw's pointer shapes where the module can.
That is on macOS; elsewhere the nearest `pointer' property shape is used."
  :type 'boolean
  :group 'excal)

(defvar-local excal--cursor nil "Pointer shape last chosen for the canvas.")
(defvar-local excal--cursor-view nil "Module cursor view over the canvas, or nil.")
(defvar-local excal--cursor-view-shown nil "Non-nil while the cursor view is shown.")

;;;; Choosing

(defconst excal--resize-cursors ["ns" "nesw" "ew" "nwse"]
  "RESIZE_CURSORS, in the order rotation steps through them.")

(defun excal--resize-cursor (handle element)
  "Return the cursor for transform HANDLE of ELEMENT.
ELEMENT is nil for the common box of several elements.  Port of
`getCursorForResizingElement': mirrored elements swap the diagonals, and
rotation turns the cursor in 45 degree steps."
  (if (eq handle 'rotation)
      'grab
    (let* ((swap (and element
                      (< (* (cl-signum (or (excal--get element 'width) 0))
                            (cl-signum (or (excal--get element 'height) 0)))
                         0)))
           (base (pcase handle
                   ((or 'n 's) "ns")
                   ((or 'e 'w) "ew")
                   ((or 'nw 'se) (if swap "nesw" "nwse"))
                   (_ (if swap "nwse" "nesw"))))
           (steps (if element (round (/ (excal--element-angle element) (/ float-pi 4))) 0))
           (index (mod (+ (cl-position base excal--resize-cursors :test #'equal) steps)
                       (length excal--resize-cursors))))
      (intern (concat (aref excal--resize-cursors index) "-resize")))))

(defun excal--select-cursor-at (scene-xy)
  "Return the selection tool's cursor at SCENE-XY.
The checks follow `excal-mouse-down', so the cursor tells what a press
there would do."
  (let* ((single (excal--single-selection))
         (linear (and (fboundp 'excal--linear-target) (excal--linear-target)))
         (handle (excal--handle-at scene-xy)))
    (cond
     ((and (fboundp 'excal--link-at) (excal--link-at scene-xy)) 'pointer)
     ((and single (fboundp 'excal--elbow-p) (excal--elbow-p single)
           (or (excal--elbow-midpoint-at single scene-xy)
               (excal--elbow-end-at single scene-xy)))
      'pointer)
     ((and linear (not (and (fboundp 'excal--elbow-p) (excal--elbow-p linear)))
           (or (excal--point-at linear scene-xy)
               (excal--midpoint-at linear scene-xy)))
      'pointer)
     (handle
      (excal--resize-cursor handle (and single (not (cdr excal--selection)) single)))
     ((or (excal--hit scene-xy)
          (and (fboundp 'excal--in-selection-box-p)
               (excal--in-selection-box-p scene-xy)))
      'move)
     (t 'default))))

(defun excal--cursor-at (scene-xy)
  "Return the pointer shape for SCENE-XY given the current tool.
`setCursorForShape' plus the hover rules of `handleCanvasPointerMove'."
  (cond
   ((bound-and-true-p excal--multi-element) 'crosshair)
   (t
    (pcase excal--tool
      ('select (excal--select-cursor-at scene-xy))
      ('hand 'grab)
      ('eraser (if (eq excal--theme 'dark) 'eraser-dark 'eraser))
      ('text (if (equal (excal--get (excal--hit scene-xy) 'type) "text") 'text 'crosshair))
      (_ 'crosshair)))))

;;;; Showing

(defconst excal--cursor-fallbacks
  '((default . arrow) (auto . arrow) (pointer . hand) (move . hand)
    (grab . hand) (grabbing . hand) (text . text) (crosshair . arrow)
    (ns-resize . nhdrag) (ew-resize . hdrag) (nwse-resize . hdrag)
    (nesw-resize . hdrag) (not-allowed . arrow) (eraser . arrow)
    (eraser-dark . arrow))
  "The `pointer' property shape standing in for each cursor.
Emacs offers no diagonal resize, move, crosshair or rotate pointer.")

(defun excal--emacs-pointer (cursor)
  "Return the `pointer' property shape nearest CURSOR."
  (alist-get cursor excal--cursor-fallbacks 'arrow))

(defun excal--set-emacs-pointer (pointer)
  "Give the canvas the `pointer' property POINTER.
The shape is a text property, so changing it touches neither the image
cache nor the canvas pixels."
  (unless (eq pointer excal--pointer)
    (setq excal--pointer pointer)
    (with-silent-modifications
      (put-text-property (point-min) (point-max) 'pointer pointer))))

(defun excal--native-cursor-p (&optional frame)
  "Return non-nil if the module can show cursors on FRAME."
  (and excal-native-cursors
       (fboundp 'excal-native-cursor-view-create)
       (eq (framep (or frame (selected-frame))) 'ns)))

(defun excal--sync-cursor-view (window width height)
  "Place the cursor view over WINDOW's WIDTH by HEIGHT body, if supported."
  (when (excal--native-cursor-p (window-frame window))
    (unless excal--cursor-view
      (pcase-let ((`(,left ,top ,right ,bottom)
                   (frame-edges (window-frame window) 'native-edges)))
        (setq excal--cursor-view (excal-native-cursor-view-create
                                  left top (- right left) (- bottom top)))))
    (when excal--cursor-view
      (pcase-let ((`(,x ,y . ,_) (window-inside-pixel-edges window)))
        (excal-native-cursor-view-set-geometry excal--cursor-view x y width height t))
      (setq excal--cursor-view-shown t))))

(defun excal--hide-cursor-view ()
  "Hide this buffer's cursor view, if any, handing the pointer to Emacs."
  (when excal--cursor-view
    (excal-native-cursor-view-set-geometry excal--cursor-view 0 0 1 1 nil))
  (setq excal--cursor-view-shown nil))

(defun excal--set-pointer (cursor)
  "Show the pointer shape CURSOR over the canvas.
The module is told on every call, even for the same CURSOR, so the shape
comes back if something else changed the pointer meanwhile."
  (setq excal--cursor cursor)
  (if (and excal--cursor-view-shown
           (excal-native-cursor-set excal--cursor-view (symbol-name cursor)))
      (excal--set-emacs-pointer 'arrow)
    (excal--set-emacs-pointer (excal--emacs-pointer cursor))))

(provide 'excal-cursor)
;;; excal-cursor.el ends here
