;;; excal-core.el --- Native module, shared state and document model  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Loads the rendering module and defines the per-buffer state and the
;; .excalidraw document model shared by the other excal files.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst excal--directory
  (file-name-directory (or load-file-name buffer-file-name default-directory)))

(unless (featurep 'excal-module)
  (module-load (expand-file-name (concat "excal-module" module-file-suffix)
                                 excal--directory)))

(declare-function excal-native-render "excal-module")
(declare-function excal-native-measure-text "excal-module")
(declare-function excal-native-write-png "excal-module")
(declare-function excal-native-fb-create "excal-module")
(declare-function excal-native-fb-render "excal-module")
(declare-function excal-native-fb-present-canvas "excal-module")
(declare-function excal-native-fb-present-tiles "excal-module")
(declare-function excal-native-fb-write-png "excal-module")
(declare-function excal-native-fb-diff "excal-module")
(declare-function excal-native-fb-scroll "excal-module")
(declare-function excal-native-layer-create "excal-module")
(declare-function excal-native-layer-set-geometry "excal-module")
(declare-function excal-native-layer-present "excal-module")
(declare-function excal-native-layer-flush "excal-module")
(declare-function excal--save-current-style "excal-style")

(defconst excal--dragging-threshold 10
  "DRAGGING_THRESHOLD, scene units: presses moving less count as clicks.")

(defgroup excal nil
  "Excalidraw scenes on Emacs Canvas."
  :group 'multimedia)

(defcustom excal-pixel-scale nil
  "Device pixels per logical pixel, or nil to guess from the frame."
  :type '(choice (const :tag "Auto" nil) number))

(defvar-local excal--file nil "File the scene is saved to.")
(defvar-local excal--doc nil "Top-level .excalidraw alist.")
(defvar-local excal--elements nil "Element alists in z-order.")
(defvar-local excal--canvas nil "Canvas image spec.")
(defvar-local excal--canvas-size nil "(WIDTH . HEIGHT) in device pixels.")
(defvar-local excal--pixel-scale 1.0)
(defvar-local excal--zoom 1.0)
(defvar-local excal--scroll-x 0.0)
(defvar-local excal--scroll-y 0.0)
(defvar-local excal--tool 'select)
(defvar-local excal--preferred-selection-tool 'select
  "The tool `v' and `1' choose: `select' or `lasso' (preferredSelectionTool).")
(defvar-local excal--selection nil "Selected element alists, in z-order.")
(defvar-local excal--editing-group nil
  "Group id entered by double-clicking, or nil; see `excal--unit'.")
(defvar-local excal--theme 'light "Color theme of the canvas: `light' or `dark'.")
(defvar-local excal--editing-linear nil "Line or arrow in point-edit mode, or nil.")
(defvar-local excal--selected-points nil "Indices of the points selected in the editor.")
(defvar-local excal--marquee nil
  "Box-selection rectangle (X1 Y1 X2 Y2) in scene units while dragging.")
(defvar-local excal--pointer nil "Pointer shape currently shown over the canvas.")
(defvar-local excal--rendered-origin nil
  "View origin of the framebuffer's contents; see `excal--view-origin'.")
(defvar-local excal--pan-remainder '(0.0 . 0.0)
  "Sub-pixel pan distance not yet applied; see `excal--pan'.")
(defvar-local excal--native-cache nil
  "Hash table mapping element alists to native vectors.")
(defvar-local excal--last-render-time nil
  "Seconds spent in the last native render.")

(defun excal--get (element key)
  "Return KEY of ELEMENT, mapping JSON null and false to nil."
  (let ((value (alist-get key element)))
    (if (memq value '(:null :false)) nil value)))

(defun excal--put (element key value)
  "Destructively set KEY of ELEMENT to VALUE."
  (if-let* ((cell (assq key element)))
      (setcdr cell value)
    (nconc element (list (cons key value))))
  value)

(defun excal--touch (element)
  "Invalidate ELEMENT's native cache and bump its version."
  (remhash element excal--native-cache)
  (excal--put element 'version (1+ (or (excal--get element 'version) 0)))
  (excal--put element 'versionNonce (random (ash 1 31)))
  (excal--put element 'updated (truncate (* 1000 (float-time)))))

;;;; Files

(defun excal--read-file (file)
  "Parse .excalidraw FILE into an alist.
The result is the raw JSON; `excal--open' restores it (see
`excal--restore-doc' in excal-restore.el)."
  (with-temp-buffer
    (set-buffer-multibyte t)
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (json-parse-buffer :object-type 'alist :array-type 'array
                       :null-object :null :false-object :false)))

(defun excal--empty-doc ()
  "Return an empty .excalidraw document, as upstream writes one."
  (list (cons 'type "excalidraw")
        (cons 'version 2)
        (cons 'source "https://excalidraw.com")
        (cons 'elements [])
        (cons 'appState (list (cons 'gridSize 20)
                              (cons 'gridStep 5)
                              (cons 'gridModeEnabled :false)
                              (cons 'viewBackgroundColor "#ffffff")
                              (cons 'lockedMultiSelections nil)))
        (cons 'files nil)))

;;;;; JSON output matching JSON.stringify (value, null, 2)

(defconst excal--json-indents
  (vconcat (cl-loop for i below 64 collect (concat "\n" (make-string (* 2 i) ?\s))))
  "Newline plus indentation for each nesting depth.")

(defun excal--json-indent (depth)
  "Insert a newline and the indentation of DEPTH."
  (insert (if (< depth (length excal--json-indents))
              (aref excal--json-indents depth)
            (concat "\n" (make-string (* 2 depth) ?\s)))))

(defun excal--json-number (x)
  "Return number X formatted like JavaScript's Number#toString."
  (cond
   ((integerp x) (number-to-string x))
   ((or (isnan x) (= x 1.0e+INF) (= x -1.0e+INF)) "null")
   ((= x 0) "0")
   ((and (= x (ffloor x)) (< (abs x) 9007199254740992.0))
    (number-to-string (truncate x)))
   (t
    ;; Emacs prints the shortest round-tripping digits, as JavaScript
    ;; does; only the layout (exponent thresholds) differs.
    (let ((s (number-to-string x)))
      (unless (string-match "\\`\\(-?\\)\\([0-9]+\\)\\(?:\\.\\([0-9]+\\)\\)?\\(?:e\\([-+]?[0-9]+\\)\\)?\\'" s)
        (error "Unexpected float syntax %s" s))
      (let* ((sign (match-string 1 s))
             (int (match-string 2 s))
             (frac (or (match-string 3 s) ""))
             (exp (if (match-string 4 s) (string-to-number (match-string 4 s)) 0))
             (digits (concat int frac))
             ;; value = 0.DIGITS * 10^n
             (n (+ (length int) exp)))
        (while (and (> (length digits) 1) (eq (aref digits 0) ?0))
          (setq digits (substring digits 1) n (1- n)))
        (setq digits (replace-regexp-in-string "0+\\'" "" digits))
        (let ((k (length digits)))
          (concat
           sign
           (cond
            ((and (<= k n) (<= n 21)) (concat digits (make-string (- n k) ?0)))
            ((and (< 0 n) (<= n 21)) (concat (substring digits 0 n) "." (substring digits n)))
            ((and (< -6 n) (<= n 0)) (concat "0." (make-string (- n) ?0) digits))
            (t (concat (substring digits 0 1)
                       (if (> k 1) (concat "." (substring digits 1)) "")
                       "e" (if (>= (1- n) 0) "+" "-")
                       (number-to-string (abs (1- n)))))))))))))

(defun excal--json-string (string)
  "Insert STRING as a JSON string literal, escaped like JSON.stringify."
  (insert ?\")
  (if (not (string-match-p "[\"\\\0-\37]" string))
      (insert string)
    (dotimes (i (length string))
      (let ((c (aref string i)))
        (cond
         ((eq c ?\") (insert "\\\""))
         ((eq c ?\\) (insert "\\\\"))
         ((eq c ?\n) (insert "\\n"))
         ((eq c ?\r) (insert "\\r"))
         ((eq c ?\t) (insert "\\t"))
         ((eq c ?\b) (insert "\\b"))
         ((eq c ?\f) (insert "\\f"))
         ((< c 32) (insert (format "\\u%04x" c)))
         (t (insert c))))))
  (insert ?\"))

(defun excal--json-object-p (value)
  "Return non-nil if VALUE is an alist standing for a JSON object."
  (or (null value) (and (consp value) (consp (car value)) (symbolp (caar value)))))

(defun excal--json-insert (value depth)
  "Insert VALUE as pretty-printed JSON at nesting DEPTH."
  (cond
   ((stringp value) (excal--json-string value))
   ((numberp value) (insert (excal--json-number value)))
   ((eq value t) (insert "true"))
   ((eq value :false) (insert "false"))
   ((eq value :null) (insert "null"))
   ((vectorp value)
    (if (= (length value) 0)
        (insert "[]")
      (insert ?\[)
      (let ((inner (1+ depth)))
        (dotimes (i (length value))
          (unless (= i 0) (insert ?,))
          (excal--json-indent inner)
          (excal--json-insert (aref value i) inner)))
      (excal--json-indent depth)
      (insert ?\])))
   ((excal--json-object-p value)
    (if (null value)
        (insert "{}")
      (insert ?\{)
      (let ((first t) (inner (1+ depth)))
        (dolist (cell value)
          (if first (setq first nil) (insert ?,))
          (excal--json-indent inner)
          (excal--json-string (symbol-name (car cell)))
          (insert ": ")
          (excal--json-insert (cdr cell) inner)))
      (excal--json-indent depth)
      (insert ?\})))
   ((listp value) (excal--json-insert (vconcat value) depth))
   ((keywordp value) (insert "null"))
   (t (error "Cannot encode %S as JSON" value))))

(defun excal--json-encode (value)
  "Return VALUE as JSON text formatted like JSON.stringify(VALUE, null, 2)."
  (with-temp-buffer
    (excal--json-insert value 0)
    (buffer-string)))

(defun excal--used-files (elements files)
  "Return the FILES entries used by live image ELEMENTS.
Port of upstream `filterOutDeletedFiles'."
  (let ((used (make-hash-table :test #'equal)))
    (dolist (e elements)
      (when (and (equal (alist-get 'type e) "image")
                 (not (eq (alist-get 'isDeleted e) t))
                 (stringp (alist-get 'fileId e)))
        (puthash (alist-get 'fileId e) t used)))
    (seq-filter (lambda (entry) (gethash (symbol-name (car entry)) used))
                (and (listp files) files))))

(defun excal--serialize-doc (doc elements)
  "Return the .excalidraw text of DOC with ELEMENTS.
Follows upstream `serializeAsJSON': type, version, source, elements,
appState, files, in that order; files are limited to those used by live
images.  Unlike upstream, the app state keeps every key it has (the
file's own keys survive a save), and top-level keys excal does not know
follow the standard ones."
  (let ((out (list (cons 'type "excalidraw")
                   (cons 'version 2)
                   (cons 'source (let ((source (alist-get 'source doc)))
                                   (if (stringp source) source
                                     "https://excalidraw.com")))
                   (cons 'elements (vconcat elements))
                   (cons 'appState (let ((state (alist-get 'appState doc)))
                                     (if (listp state) state nil)))
                   (cons 'files (excal--used-files elements (alist-get 'files doc))))))
    (dolist (cell doc)
      (unless (assq (car cell) out)
        (setq out (append out (list cell)))))
    (excal--json-encode out)))

(defun excal-save ()
  "Write the scene back to its .excalidraw file."
  (interactive)
  (unless excal--file
    (setq excal--file (read-file-name "Save scene to: " nil nil nil
                                      "untitled.excalidraw")))
  (let ((doc (copy-alist excal--doc)))
    (when (fboundp 'excal--save-current-style)
      (setf (alist-get 'appState doc)
            (excal--save-current-style (alist-get 'appState doc))))
    (let ((text (excal--serialize-doc doc excal--elements)))
      (with-temp-file excal--file
        (setq buffer-file-coding-system 'utf-8-unix)
        (insert text))))
  (message "Saved %s" excal--file))

(defun excal--new-id ()
  "Return a random element id."
  (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (apply #'string (cl-loop repeat 20 collect (aref chars (random (length chars)))))))

(defun excal--make-element (type x y &rest props)
  "Return a new element alist of TYPE at X, Y with extra PROPS alist."
  (let* ((now (truncate (* 1000 (float-time))))
         (element
         (list (cons 'id (excal--new-id)) (cons 'type type)
               (cons 'x (float x)) (cons 'y (float y))
               (cons 'width 0.0) (cons 'height 0.0) (cons 'angle 0)
               (cons 'strokeColor "#1e1e1e") (cons 'backgroundColor "transparent")
               (cons 'fillStyle "solid") (cons 'strokeWidth 2)
               (cons 'strokeStyle "solid") (cons 'roughness 1)
               (cons 'opacity 100) (cons 'groupIds []) (cons 'frameId :null)
               ;; `index' is filled in by `excal--sync-indices-maybe' or
               ;; `excal--sync-moved-indices' (excal-index.el).
               (cons 'index :null)
               (cons 'roundness :null) (cons 'seed (1+ (random (1- (ash 1 31)))))
               (cons 'version 1) (cons 'versionNonce (random (ash 1 31)))
               (cons 'isDeleted :false) (cons 'boundElements :null)
               (cons 'updated now) (cons 'created now)
               (cons 'link :null) (cons 'locked :false))))
    (dolist (prop props element)
      (excal--put element (car prop) (cdr prop)))))

(defun excal--rotate-point (point center angle)
  "Rotate POINT (X . Y) about CENTER by ANGLE radians."
  (let* ((dx (- (car point) (car center)))
         (dy (- (cdr point) (cdr center)))
         (c (cos angle)) (s (sin angle)))
    (cons (+ (car center) (- (* dx c) (* dy s)))
          (+ (cdr center) (+ (* dx s) (* dy c))))))

(defun excal--element-by-id (id)
  "Return the element with ID in the current scene, or nil."
  (and id (cl-find id excal--elements
                   :key (lambda (e) (excal--get e 'id)) :test #'equal)))

(defun excal--bounds (element)
  "Return (X1 Y1 X2 Y2) of ELEMENT, ignoring rotation."
  (let ((x (excal--get element 'x)) (y (excal--get element 'y))
        (w (excal--get element 'width)) (h (excal--get element 'height)))
    (if-let* ((points (excal--get element 'points))
              ((> (length points) 0)))
        (let ((xs (mapcar (lambda (p) (+ x (aref p 0))) points))
              (ys (mapcar (lambda (p) (+ y (aref p 1))) points)))
          (list (apply #'min xs) (apply #'min ys)
                (apply #'max xs) (apply #'max ys)))
      (list (min x (+ x w)) (min y (+ y h)) (max x (+ x w)) (max y (+ y h))))))

(defun excal--normalize-box (element)
  "Make ELEMENT's width and height non-negative."
  (let ((w (excal--get element 'width)) (h (excal--get element 'height)))
    (when (< w 0)
      (excal--put element 'x (+ (excal--get element 'x) w))
      (excal--put element 'width (- w)))
    (when (< h 0)
      (excal--put element 'y (+ (excal--get element 'y) h))
      (excal--put element 'height (- h)))))

(defun excal--linear-extent (element)
  "Recompute ELEMENT's width and height from its points."
  (let ((points (excal--get element 'points)))
    (excal--put element 'width
                (float (- (seq-max (seq-map (lambda (p) (aref p 0)) points))
                          (seq-min (seq-map (lambda (p) (aref p 0)) points)))))
    (excal--put element 'height
                (float (- (seq-max (seq-map (lambda (p) (aref p 1)) points))
                          (seq-min (seq-map (lambda (p) (aref p 1)) points)))))))

(declare-function excal--redraw-text "excal-text")
(declare-function excal--set-text "excal-text")
(declare-function excal--make-text-element "excal-text")

;; Text layout lives in excal-text.el, which requires this file:
;; `excal--measure-text' re-lays out a text element (wrapping, container
;; growth, placement), `excal--set-text' sets its source text, and
;; `excal--make-text-element' creates one.

(defun excal--measure-text (element)
  "Recompute text ELEMENT's lines and size from its text and font.
Bound text also follows and grows its container; see `excal--redraw-text'."
  (excal--redraw-text element))

(provide 'excal-core)
;;; excal-core.el ends here
