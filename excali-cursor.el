;;; excali-cursor.el --- Pointer shapes  -*- lexical-binding: t; -*-

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
;; On macOS the module shows them all (src/excali-cursor.m): a transparent
;; view over the canvas owns the pointer there, while Emacs' own `pointer'
;; property stays `arrow' so Emacs never fights over it.  Elsewhere, or
;; without the module, each shape falls back to the nearest one Emacs'
;; `pointer' property offers.

;;; Code:

(require 'excali-core)
(require 'excali-select)
(require 'excali-handles)
(require 'excali-hit)

(declare-function excali--in-selection-box-p "excali-edit")
(declare-function excali--link-at "excali-erase")
(declare-function excali--linear-target "excali-linear")
(declare-function excali--point-at "excali-linear")
(declare-function excali--midpoint-at "excali-linear")
(declare-function excali--elbow-p "excali-elbow")
(declare-function excali--elbow-end-at "excali-elbow")
(declare-function excali--elbow-midpoint-at "excali-elbow")
(declare-function excali-native-cursor-view-create "excali-module")
(declare-function excali-native-cursor-view-set-geometry "excali-module")
(declare-function excali-native-cursor-set "excali-module")
(defvar excali--multi-element)
(defvar excali--editing-linear)

(defcustom excali-native-cursors t
  "Non-nil means show Excalidraw's pointer shapes where the module can.
That is on macOS; elsewhere the nearest `pointer' property shape is used."
  :type 'boolean
  :group 'excali)

(defvar-local excali--cursor nil "Pointer shape last chosen for the canvas.")
(defvar-local excali--cursor-view nil "Module cursor view over the canvas, or nil.")
(defvar-local excali--cursor-view-shown nil "Non-nil while the cursor view is shown.")

;;;; Choosing

(defconst excali--resize-cursors ["ns" "nesw" "ew" "nwse"]
  "RESIZE_CURSORS, in the order rotation steps through them.")

(defun excali--resize-cursor (handle element)
  "Return the cursor for transform HANDLE of ELEMENT.
ELEMENT is nil for the common box of several elements.  Port of
`getCursorForResizingElement': mirrored elements swap the diagonals, and
rotation turns the cursor in 45 degree steps."
  (if (eq handle 'rotation)
      'grab
    (let* ((swap (and element
                      (< (* (cl-signum (or (excali--get element 'width) 0))
                            (cl-signum (or (excali--get element 'height) 0)))
                         0)))
           (base (pcase handle
                   ((or 'n 's) "ns")
                   ((or 'e 'w) "ew")
                   ((or 'nw 'se) (if swap "nesw" "nwse"))
                   (_ (if swap "nwse" "nesw"))))
           (steps (if element (round (/ (excali--element-angle element) (/ float-pi 4))) 0))
           (index (mod (+ (cl-position base excali--resize-cursors :test #'equal) steps)
                       (length excali--resize-cursors))))
      (intern (concat (aref excali--resize-cursors index) "-resize")))))

(defun excali--select-cursor-at (scene-xy)
  "Return the selection tool's cursor at SCENE-XY.
The checks follow `excali-mouse-down', so the cursor tells what a press
there would do."
  (let* ((single (excali--single-selection))
         (linear (and (fboundp 'excali--linear-target) (excali--linear-target)))
         (handle (excali--handle-at scene-xy)))
    (cond
     ((and (fboundp 'excali--link-at) (excali--link-at scene-xy)) 'pointer)
     ((and single (fboundp 'excali--elbow-p) (excali--elbow-p single)
           (or (excali--elbow-midpoint-at single scene-xy)
               (excali--elbow-end-at single scene-xy)))
      'pointer)
     ((and linear (not (and (fboundp 'excali--elbow-p) (excali--elbow-p linear)))
           (or (excali--point-at linear scene-xy)
               (excali--midpoint-at linear scene-xy)))
      'pointer)
     (handle
      (excali--resize-cursor handle (and single (not (cdr excali--selection)) single)))
     ((or (excali--hit scene-xy)
          (and (fboundp 'excali--in-selection-box-p)
               (excali--in-selection-box-p scene-xy)))
      'move)
     (t 'default))))

(defun excali--cursor-at (scene-xy)
  "Return the pointer shape for SCENE-XY given the current tool.
`setCursorForShape' plus the hover rules of `handleCanvasPointerMove'."
  (cond
   ((bound-and-true-p excali--multi-element) 'crosshair)
   (t
    (pcase excali--tool
      ('select (excali--select-cursor-at scene-xy))
      ('hand 'grab)
      ('eraser (if (eq excali--theme 'dark) 'eraser-dark 'eraser))
      ('text (if (equal (excali--get (excali--hit scene-xy) 'type) "text") 'text 'crosshair))
      (_ 'crosshair)))))

;;;; Showing

(defconst excali--cursor-fallbacks
  '((default . arrow) (auto . arrow) (pointer . hand) (move . hand)
    (grab . hand) (grabbing . hand) (text . text) (crosshair . arrow)
    (ns-resize . nhdrag) (ew-resize . hdrag) (nwse-resize . hdrag)
    (nesw-resize . hdrag) (not-allowed . arrow) (eraser . arrow)
    (eraser-dark . arrow))
  "The `pointer' property shape standing in for each cursor.
Emacs offers no diagonal resize, move, crosshair or rotate pointer.")

(defun excali--emacs-pointer (cursor)
  "Return the `pointer' property shape nearest CURSOR."
  (alist-get cursor excali--cursor-fallbacks 'arrow))

(defun excali--set-emacs-pointer (pointer)
  "Give the canvas the `pointer' property POINTER.
The shape is a text property, so changing it touches neither the image
cache nor the canvas pixels."
  (unless (eq pointer excali--pointer)
    (setq excali--pointer pointer)
    (with-silent-modifications
      (put-text-property (point-min) (point-max) 'pointer pointer))))

(defun excali--native-cursor-p (&optional frame)
  "Return non-nil if the module can show cursors on FRAME."
  (and excali-native-cursors
       (fboundp 'excali-native-cursor-view-create)
       (eq (framep (or frame (selected-frame))) 'ns)))

(defun excali--sync-cursor-view (window width height)
  "Place the cursor view over WINDOW's WIDTH by HEIGHT body, if supported."
  (when (excali--native-cursor-p (window-frame window))
    (unless excali--cursor-view
      (pcase-let ((`(,left ,top ,right ,bottom)
                   (frame-edges (window-frame window) 'native-edges)))
        (setq excali--cursor-view (excali-native-cursor-view-create
                                  left top (- right left) (- bottom top)))))
    (when excali--cursor-view
      (pcase-let ((`(,x ,y . ,_) (window-inside-pixel-edges window)))
        (excali-native-cursor-view-set-geometry excali--cursor-view x y width height t))
      (setq excali--cursor-view-shown t))))

(defun excali--hide-cursor-view ()
  "Hide this buffer's cursor view, if any, handing the pointer to Emacs."
  (when excali--cursor-view
    (excali-native-cursor-view-set-geometry excali--cursor-view 0 0 1 1 nil))
  (setq excali--cursor-view-shown nil))

(defun excali--set-pointer (cursor)
  "Show the pointer shape CURSOR over the canvas.
The module is told on every call, even for the same CURSOR, so the shape
comes back if something else changed the pointer meanwhile."
  (setq excali--cursor cursor)
  (if (and excali--cursor-view-shown
           (excali-native-cursor-set excali--cursor-view (symbol-name cursor)))
      (excali--set-emacs-pointer 'arrow)
    (excali--set-emacs-pointer (excali--emacs-pointer cursor))))

(provide 'excali-cursor)
;;; excali-cursor.el ends here
