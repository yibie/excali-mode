;;; excali-core.el --- Native module, shared state and document model  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Loads the rendering module and defines the per-buffer state and the
;; .excalidraw document model shared by the other excali files.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst excali--directory
  (file-name-directory (or load-file-name buffer-file-name default-directory)))

(unless (featurep 'excali-module)
  (module-load (expand-file-name (concat "excali-module" module-file-suffix)
                                 excali--directory)))

(declare-function excali-native-render "excali-module")
(declare-function excali-native-measure-text "excali-module")
(declare-function excali-native-write-png "excali-module")
(declare-function excali-native-fb-create "excali-module")
(declare-function excali-native-fb-render "excali-module")
(declare-function excali-native-fb-present-canvas "excali-module")
(declare-function excali-native-fb-present-tiles "excali-module")
(declare-function excali-native-fb-write-png "excali-module")
(declare-function excali-native-fb-diff "excali-module")
(declare-function excali-native-fb-scroll "excali-module")
(declare-function excali-native-layer-create "excali-module")
(declare-function excali-native-layer-set-geometry "excali-module")
(declare-function excali-native-layer-present "excali-module")
(declare-function excali-native-layer-flush "excali-module")
(declare-function excali--save-current-style "excali-style")

(defconst excali--dragging-threshold 10
  "DRAGGING_THRESHOLD, scene units: presses moving less count as clicks.")

(defgroup excali nil
  "Excalidraw scenes on Emacs Canvas."
  :group 'multimedia)

(defcustom excali-pixel-scale nil
  "Device pixels per logical pixel, or nil to guess from the frame."
  :type '(choice (const :tag "Auto" nil) number))

(defvar-local excali--file nil "File the scene is saved to.")
(defvar-local excali--doc nil "Top-level .excalidraw alist.")
(defvar-local excali--elements nil "Element alists in z-order.")
(defvar-local excali--canvas nil "Canvas image spec.")
(defvar-local excali--canvas-size nil "(WIDTH . HEIGHT) in device pixels.")
(defvar-local excali--pixel-scale 1.0)
(defvar-local excali--zoom 1.0)
(defvar-local excali--scroll-x 0.0)
(defvar-local excali--scroll-y 0.0)
(defvar-local excali--tool 'select)
(defvar-local excali--preferred-selection-tool 'select
  "The tool `v' and `1' choose: `select' or `lasso' (preferredSelectionTool).")
(defvar-local excali--selection nil "Selected element alists, in z-order.")
(defvar-local excali--editing-group nil
  "Group id entered by double-clicking, or nil; see `excali--unit'.")
(defvar-local excali--theme 'light "Color theme of the canvas: `light' or `dark'.")
(defvar-local excali--editing-linear nil "Line or arrow in point-edit mode, or nil.")
(defvar-local excali--selected-points nil "Indices of the points selected in the editor.")
(defvar-local excali--marquee nil
  "Box-selection rectangle (X1 Y1 X2 Y2) in scene units while dragging.")
(defvar-local excali--rendered-origin nil
  "View origin of the framebuffer's contents; see `excali--view-origin'.")
(defvar-local excali--pan-remainder '(0.0 . 0.0)
  "Sub-pixel pan distance not yet applied; see `excali--pan'.")
(defvar-local excali--native-cache nil
  "Hash table mapping element alists to native vectors.")
(defvar-local excali--last-render-time nil
  "Seconds spent in the last native render.")

(defun excali--get (element key)
  "Return KEY of ELEMENT, mapping JSON null and false to nil."
  (let ((value (alist-get key element)))
    (if (memq value '(:null :false)) nil value)))

(defun excali--put (element key value)
  "Destructively set KEY of ELEMENT to VALUE."
  (if-let* ((cell (assq key element)))
      (setcdr cell value)
    (nconc element (list (cons key value))))
  value)

(defun excali--touch (element)
  "Invalidate ELEMENT's native cache and bump its version."
  (remhash element excali--native-cache)
  (excali--put element 'version (1+ (or (excali--get element 'version) 0)))
  (excali--put element 'versionNonce (random (ash 1 31)))
  (excali--put element 'updated (truncate (* 1000 (float-time)))))

;;;; Files

(defun excali--read-file (file)
  "Parse .excalidraw FILE into an alist.
The result is the raw JSON; `excali--open' restores it (see
`excali--restore-doc' in excali-restore.el)."
  (with-temp-buffer
    (set-buffer-multibyte t)
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents file))
    (json-parse-buffer :object-type 'alist :array-type 'array
                       :null-object :null :false-object :false)))

(defun excali--empty-doc ()
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

(defconst excali--json-indents
  (vconcat (cl-loop for i below 64 collect (concat "\n" (make-string (* 2 i) ?\s))))
  "Newline plus indentation for each nesting depth.")

(defun excali--json-indent (depth)
  "Insert a newline and the indentation of DEPTH."
  (insert (if (< depth (length excali--json-indents))
              (aref excali--json-indents depth)
            (concat "\n" (make-string (* 2 depth) ?\s)))))

(defun excali--json-number (x)
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

(defun excali--json-string (string)
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

(defun excali--json-object-p (value)
  "Return non-nil if VALUE is an alist standing for a JSON object."
  (or (null value) (and (consp value) (consp (car value)) (symbolp (caar value)))))

(defun excali--json-insert (value depth)
  "Insert VALUE as pretty-printed JSON at nesting DEPTH."
  (cond
   ((stringp value) (excali--json-string value))
   ((numberp value) (insert (excali--json-number value)))
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
          (excali--json-indent inner)
          (excali--json-insert (aref value i) inner)))
      (excali--json-indent depth)
      (insert ?\])))
   ((excali--json-object-p value)
    (if (null value)
        (insert "{}")
      (insert ?\{)
      (let ((first t) (inner (1+ depth)))
        (dolist (cell value)
          (if first (setq first nil) (insert ?,))
          (excali--json-indent inner)
          (excali--json-string (symbol-name (car cell)))
          (insert ": ")
          (excali--json-insert (cdr cell) inner)))
      (excali--json-indent depth)
      (insert ?\})))
   ((listp value) (excali--json-insert (vconcat value) depth))
   ((keywordp value) (insert "null"))
   (t (error "Cannot encode %S as JSON" value))))

(defun excali--json-encode (value)
  "Return VALUE as JSON text formatted like JSON.stringify(VALUE, null, 2)."
  (with-temp-buffer
    (excali--json-insert value 0)
    (buffer-string)))

(defun excali--used-files (elements files)
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

(defun excali--serialize-doc (doc elements)
  "Return the .excalidraw text of DOC with ELEMENTS.
Follows upstream `serializeAsJSON': type, version, source, elements,
appState, files, in that order; files are limited to those used by live
images.  Unlike upstream, the app state keeps every key it has (the
file's own keys survive a save), and top-level keys excali does not know
follow the standard ones."
  (let ((out (list (cons 'type "excalidraw")
                   (cons 'version 2)
                   (cons 'source (let ((source (alist-get 'source doc)))
                                   (if (stringp source) source
                                     "https://excalidraw.com")))
                   (cons 'elements (vconcat elements))
                   (cons 'appState (let ((state (alist-get 'appState doc)))
                                     (if (listp state) state nil)))
                   (cons 'files (excali--used-files elements (alist-get 'files doc))))))
    (dolist (cell doc)
      (unless (assq (car cell) out)
        (setq out (append out (list cell)))))
    (excali--json-encode out)))

(defun excali-save ()
  "Write the scene back to its .excalidraw file."
  (interactive)
  (unless excali--file
    (setq excali--file (read-file-name "Save scene to: " nil nil nil
                                      "untitled.excalidraw")))
  (let ((doc (copy-alist excali--doc)))
    (when (fboundp 'excali--save-current-style)
      (setf (alist-get 'appState doc)
            (excali--save-current-style (alist-get 'appState doc))))
    (let ((text (excali--serialize-doc doc excali--elements)))
      (with-temp-file excali--file
        (setq buffer-file-coding-system 'utf-8-unix)
        (insert text))))
  (message "Saved %s" excali--file))

(defun excali--new-id ()
  "Return a random element id."
  (let ((chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (apply #'string (cl-loop repeat 20 collect (aref chars (random (length chars)))))))

(defun excali--make-element (type x y &rest props)
  "Return a new element alist of TYPE at X, Y with extra PROPS alist."
  (let* ((now (truncate (* 1000 (float-time))))
         (element
         (list (cons 'id (excali--new-id)) (cons 'type type)
               (cons 'x (float x)) (cons 'y (float y))
               (cons 'width 0.0) (cons 'height 0.0) (cons 'angle 0)
               (cons 'strokeColor "#1e1e1e") (cons 'backgroundColor "transparent")
               (cons 'fillStyle "solid") (cons 'strokeWidth 2)
               (cons 'strokeStyle "solid") (cons 'roughness 1)
               (cons 'opacity 100) (cons 'groupIds []) (cons 'frameId :null)
               ;; `index' is filled in by `excali--sync-indices-maybe' or
               ;; `excali--sync-moved-indices' (excali-index.el).
               (cons 'index :null)
               (cons 'roundness :null) (cons 'seed (1+ (random (1- (ash 1 31)))))
               (cons 'version 1) (cons 'versionNonce (random (ash 1 31)))
               (cons 'isDeleted :false) (cons 'boundElements :null)
               (cons 'updated now) (cons 'created now)
               (cons 'link :null) (cons 'locked :false))))
    (dolist (prop props element)
      (excali--put element (car prop) (cdr prop)))))

(defun excali--rotate-point (point center angle)
  "Rotate POINT (X . Y) about CENTER by ANGLE radians."
  (let* ((dx (- (car point) (car center)))
         (dy (- (cdr point) (cdr center)))
         (c (cos angle)) (s (sin angle)))
    (cons (+ (car center) (- (* dx c) (* dy s)))
          (+ (cdr center) (+ (* dx s) (* dy c))))))

(defun excali--element-by-id (id)
  "Return the element with ID in the current scene, or nil."
  (and id (cl-find id excali--elements
                   :key (lambda (e) (excali--get e 'id)) :test #'equal)))

(defvar excali--points-bounds (make-hash-table :test #'eq :weakness 'key)
  "Linear elements' bounds, as (POINTS VERSION X Y . BOUNDS).
They stay valid while the points vector, the version and the origin do;
points are always replaced, never changed in place, when they move.")

(defun excali--bounds (element)
  "Return (X1 Y1 X2 Y2) of ELEMENT, ignoring rotation."
  (let ((x (excali--get element 'x)) (y (excali--get element 'y))
        (w (excali--get element 'width)) (h (excali--get element 'height)))
    (if-let* ((points (excali--get element 'points))
              ((> (length points) 0)))
        (let ((version (excali--get element 'version))
              (cached (gethash element excali--points-bounds)))
          (if (and cached (eq (nth 0 cached) points) (eql (nth 1 cached) version)
                   (eql (nth 2 cached) x) (eql (nth 3 cached) y))
              (copy-sequence (nthcdr 4 cached))
            ;; One pass without consing: scenes hold thousands of lines.
            (let* ((p0 (aref points 0))
                   (x1 (aref p0 0)) (y1 (aref p0 1)) (x2 x1) (y2 y1))
              (dotimes (i (length points))
                (let* ((p (aref points i)) (px (aref p 0)) (py (aref p 1)))
                  (cond ((< px x1) (setq x1 px)) ((> px x2) (setq x2 px)))
                  (cond ((< py y1) (setq y1 py)) ((> py y2) (setq y2 py)))))
              (let ((bounds (list (+ x x1) (+ y y1) (+ x x2) (+ y y2))))
                (puthash element (append (list points version x y) bounds)
                         excali--points-bounds)
                bounds))))
      (list (min x (+ x w)) (min y (+ y h)) (max x (+ x w)) (max y (+ y h))))))

(defun excali--normalize-box (element)
  "Make ELEMENT's width and height non-negative."
  (let ((w (excali--get element 'width)) (h (excali--get element 'height)))
    (when (< w 0)
      (excali--put element 'x (+ (excali--get element 'x) w))
      (excali--put element 'width (- w)))
    (when (< h 0)
      (excali--put element 'y (+ (excali--get element 'y) h))
      (excali--put element 'height (- h)))))

(defun excali--linear-extent (element)
  "Recompute ELEMENT's width and height from its points."
  (let ((points (excali--get element 'points)))
    (excali--put element 'width
                (float (- (seq-max (seq-map (lambda (p) (aref p 0)) points))
                          (seq-min (seq-map (lambda (p) (aref p 0)) points)))))
    (excali--put element 'height
                (float (- (seq-max (seq-map (lambda (p) (aref p 1)) points))
                          (seq-min (seq-map (lambda (p) (aref p 1)) points)))))))

(declare-function excali--redraw-text "excali-text")
(declare-function excali--set-text "excali-text")
(declare-function excali--make-text-element "excali-text")

;; Text layout lives in excali-text.el, which requires this file:
;; `excali--measure-text' re-lays out a text element (wrapping, container
;; growth, placement), `excali--set-text' sets its source text, and
;; `excali--make-text-element' creates one.

(defun excali--measure-text (element)
  "Recompute text ELEMENT's lines and size from its text and font.
Bound text also follows and grows its container; see `excali--redraw-text'."
  (excali--redraw-text element))

(provide 'excali-core)
;;; excali-core.el ends here
