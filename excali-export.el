;;; excali-export.el --- PNG and SVG export with embedded scenes, and import  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `excali-export-png' and `excali-export-svg' follow upstream
;; scene/export.ts (`exportToCanvas', `exportToSvg') and data/image.ts:
;;
;; - The exported elements are the selection (with bound text and, for
;;   several selected elements, the children of selected frames) or the
;;   whole scene; a single selected frame exports just that frame's area
;;   (`prepareElementsForExport', `getFrameRenderingConfig').
;; - Frame names become Helvetica 14px text elements above their frames
;;   (`addFrameLabelsAsTextElements'), so they count for the bounds.
;; - The output covers the common bounds of the root elements plus
;;   `excali-export-padding' on each side, at `excali-export-scale'
;;   pixels per scene unit, over `viewBackgroundColor' when
;;   `excali-export-background' is on.
;; - With `excali-export-embed-scene' the scene (as `excali-save' writes
;;   it) is embedded: in a PNG as a tEXt chunk with keyword
;;   "application/vnd.excalidraw+json" holding upstream's `encode'
;;   payload (zlib-compressed byte string), in an SVG as upstream's
;;   <metadata> comment payload (version 2, base64).
;;
;; Deviations: the SVG is produced by Cairo (glyphs as paths, raster
;; images as embedded PNG, no <symbol>/<use> or font-face CSS); the
;; root keeps upstream's width/height/viewBox and metadata layout.
;; There is no dark-mode export yet.  Raster images are drawn with
;; Cairo's filtering, SVG images through librsvg.
;;
;; `excali--read-scene-file' opens a .excalidraw file, or the scene
;; embedded in a PNG or SVG file (compressed, uncompressed and legacy
;; payloads, as upstream `decodePngMetadata' and
;; `decodeSvgBase64Payload').

;;; Code:

(require 'excali-core)
(require 'excali-frame-render)
(require 'excali-image)
(require 'excali-text)
(require 'excali-select)

(declare-function excali-native-export-png "excali-module")
(declare-function excali-native-export-svg "excali-module")
(declare-function excali-native-zlib-compress "excali-module")
(declare-function excali-native-zlib-decompress "excali-module")
(declare-function excali-native-text-width "excali-module")
(declare-function excali--native-element "excali-view")
(declare-function excali--save-current-style "excali-style")

(defconst excali-export-mime-type "application/vnd.excalidraw+json"
  "MIME_TYPES.excalidraw: the PNG tEXt keyword and SVG payload type.")

(defcustom excali-export-background t
  "Non-nil to paint the scene's `viewBackgroundColor' behind exports."
  :type 'boolean
  :group 'excali)

(defcustom excali-export-padding 10
  "Scene units of margin around exported content (DEFAULT_EXPORT_PADDING)."
  :type 'number
  :group 'excali)

(defcustom excali-export-scale 1
  "Output pixels per scene unit of PNG exports (EXPORT_SCALES: 1, 2, 3).
SVG exports get width and height attributes scaled by it."
  :type '(choice (const 1) (const 2) (const 3) number)
  :group 'excali)

(defcustom excali-export-embed-scene t
  "Non-nil to embed the scene in exported PNG and SVG files.
Such files open again in excali (`excali-open') and in Excalidraw."
  :type 'boolean
  :group 'excali)

;;;; Payload encoding (data/encode.ts)

(defun excali--bytes-to-byte-string (bytes)
  "Return unibyte BYTES as a string of characters 0..255."
  (decode-coding-string bytes 'iso-latin-1-unix))

(defun excali--byte-string-to-bytes (string)
  "Return STRING of characters 0..255 as a unibyte string."
  (encode-coding-string string 'iso-latin-1-unix))

(defun excali--encode-payload (text &optional uncompressed)
  "Return the JSON of upstream `encode({text})' for TEXT.
That is {\"version\":\"1\",\"encoding\":\"bstring\",\"compressed\":...,
\"encoded\":...} with the zlib-deflated UTF-8 bytes of TEXT as a byte
string, or the plain UTF-8 bytes when UNCOMPRESSED or compression fails.
The result only contains characters 0..255."
  (let* ((utf8 (encode-coding-string text 'utf-8-unix))
         (deflated (and (not uncompressed) (excali-native-zlib-compress utf8))))
    (with-temp-buffer
      (insert "{\"version\":\"1\",\"encoding\":\"bstring\",\"compressed\":"
              (if deflated "true" "false") ",\"encoded\":")
      (excali--json-string (excali--bytes-to-byte-string (or deflated utf8)))
      (insert "}")
      (buffer-string))))

(defun excali--inflate (bytes)
  "Return zlib data BYTES inflated, or nil."
  (if (fboundp 'excali-native-zlib-decompress)
      (excali-native-zlib-decompress bytes)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert bytes)
      (and (zlib-decompress-region (point-min) (point-max))
           (buffer-string)))))

(defun excali--decode-payload (json)
  "Return the scene text in the payload JSON (upstream `decode').
JSON is text of characters 0..255.  A payload that is itself a scene
\(legacy files) is returned as is."
  (let ((data (condition-case nil
                  (json-parse-string json :object-type 'alist :array-type 'array
                                     :null-object :null :false-object :false)
                (json-parse-error (user-error "Invalid embedded scene")))))
    (cond
     ((and (not (assq 'encoded data)) (equal (alist-get 'type data) "excalidraw"))
      json)
     ((not (equal (alist-get 'encoding data) "bstring"))
      (user-error "Unsupported embedded scene encoding: %s" (alist-get 'encoding data)))
     (t
      (let* ((bytes (excali--byte-string-to-bytes (alist-get 'encoded data)))
             (bytes (if (eq (alist-get 'compressed data) t)
                        (or (excali--inflate bytes)
                            (user-error "Corrupt embedded scene"))
                      bytes)))
        (decode-coding-string bytes 'utf-8-unix))))))

;;;; PNG and SVG containers

(defun excali--png-u32 (bytes pos)
  "Return the big-endian 32-bit integer at POS of unibyte BYTES."
  (logior (ash (aref bytes pos) 24) (ash (aref bytes (+ pos 1)) 16)
          (ash (aref bytes (+ pos 2)) 8) (aref bytes (+ pos 3))))

(defun excali--png-scene-payload (bytes)
  "Return the payload JSON of the first tEXt chunk of PNG BYTES, or nil.
Signal an error when the chunk's keyword is not
`excali-export-mime-type', as upstream `decodePngMetadata' does."
  (unless (string-prefix-p "\x89PNG\r\n\x1a\n" bytes)
    (user-error "Not a PNG file"))
  (let ((pos 8) (found nil))
    (while (and (not found) (<= (+ pos 12) (length bytes)))
      (let ((length (excali--png-u32 bytes pos))
            (type (substring bytes (+ pos 4) (+ pos 8))))
        (when (> (+ pos 12 length) (length bytes))
          (user-error "Truncated PNG file"))
        (if (not (equal type "tEXt"))
            (setq pos (+ pos 12 length))
          (let* ((data (substring bytes (+ pos 8) (+ pos 8 length)))
                 (nul (string-search "\0" data)))
            (unless (and nul (equal (substring data 0 nul) excali-export-mime-type))
              (user-error "No Excalidraw scene in this PNG"))
            (setq found (excali--bytes-to-byte-string (substring data (1+ nul))))))))
    found))

(defun excali--svg-scene-payload (svg)
  "Return the payload JSON embedded in SVG text, or nil.
Port of upstream `decodeSvgBase64Payload'."
  (when (string-search (concat "payload-type:" excali-export-mime-type) svg)
    (when (string-match "<!-- payload-start -->[ \t\n\r]*\\([^<]+?\\)[ \t\n\r]*<!-- payload-end -->"
                        svg)
      (let* ((base64 (match-string 1 svg))
             (version (if (string-match "<!-- payload-version:\\([0-9]+\\) -->" svg)
                          (match-string 1 svg)
                        "1"))
             (bytes (base64-decode-string
                     (replace-regexp-in-string "[ \t\n\r]" "" base64))))
        (if (equal version "1")
            (decode-coding-string bytes 'utf-8-unix)
          (excali--bytes-to-byte-string bytes))))))

(defun excali--parse-scene (text)
  "Parse scene JSON TEXT into a document alist, as `excali--read-file' does."
  (json-parse-string text :object-type 'alist :array-type 'array
                     :null-object :null :false-object :false))

(defun excali--read-scene-file (file)
  "Return the parsed scene of FILE: .excalidraw JSON, or a PNG or SVG
with an embedded scene."
  (let ((ext (downcase (or (file-name-extension file) ""))))
    (pcase ext
      ("png"
       (let ((payload (excali--png-scene-payload (excali--read-image-file file))))
         (unless payload (user-error "No Excalidraw scene in %s" file))
         (excali--parse-scene (excali--decode-payload payload))))
      ("svg"
       (let* ((svg (with-temp-buffer
                     (let ((coding-system-for-read 'utf-8-unix))
                       (insert-file-contents file))
                     (buffer-string)))
              (payload (excali--svg-scene-payload svg)))
         (unless payload (user-error "No Excalidraw scene in %s" file))
         (excali--parse-scene (excali--decode-payload payload))))
      (_ (excali--read-file file)))))

(defun excali--scene-file-p (file)
  "Return non-nil if FILE is saved by `excali-save' (not a PNG or SVG)."
  (not (member (downcase (or (file-name-extension file) "")) '("png" "svg"))))

;;;; What gets exported

(defun excali--export-rotated-bounds (element)
  "Return ELEMENT's axis-aligned bounds (X1 Y1 X2 Y2) including rotation."
  (let ((angle (or (excali--get element 'angle) 0))
        (box (excali--bounds element)))
    (if (or (not (numberp angle)) (zerop angle))
        box
      (let* ((points (excali--get element 'points))
             (x (excali--get element 'x)) (y (excali--get element 'y))
             (corners (if (and (vectorp points) (> (length points) 0))
                          (mapcar (lambda (p) (cons (+ x (aref p 0)) (+ y (aref p 1))))
                                  points)
                        (pcase-let ((`(,x1 ,y1 ,x2 ,y2) box))
                          (list (cons x1 y1) (cons x2 y1) (cons x2 y2) (cons x1 y2)))))
             (center (cons (/ (+ (nth 0 box) (nth 2 box)) 2.0)
                           (/ (+ (nth 1 box) (nth 3 box)) 2.0)))
             (rotated (mapcar (lambda (p) (excali--rotate-point p center angle)) corners)))
        (list (apply #'min (mapcar #'car rotated)) (apply #'min (mapcar #'cdr rotated))
              (apply #'max (mapcar #'car rotated)) (apply #'max (mapcar #'cdr rotated)))))))

(defun excali--export-root-elements (elements)
  "Return ELEMENTS minus the children of frames among them (`getRootElements')."
  (let ((frames (delq nil (mapcar (lambda (e) (and (excali--frame-like-p e)
                                                   (excali--get e 'id)))
                                  elements))))
    (seq-remove (lambda (e) (member (excali--get e 'frameId) frames)) elements)))

(defun excali--export-geometry (elements padding)
  "Return (X Y WIDTH HEIGHT) of an export of ELEMENTS with PADDING.
X, Y is the scene point at the output's top-left corner (`getCanvasSize')."
  (let* ((bounds (mapcar #'excali--export-rotated-bounds elements))
         (x1 (apply #'min (mapcar #'car bounds)))
         (y1 (apply #'min (mapcar #'cadr bounds)))
         (x2 (apply #'max (mapcar #'caddr bounds)))
         (y2 (apply #'max (mapcar #'cadddr bounds))))
    (list (float (- x1 padding)) (float (- y1 padding))
          (float (+ (- x2 x1) (* 2 padding))) (float (+ (- y2 y1) (* 2 padding))))))

(defun excali--elements-overlapping-frame (elements frame)
  "Return ELEMENTS overlapping FRAME's box that are in no other frame."
  (pcase-let ((`(,fx1 ,fy1 ,fx2 ,fy2) (excali--bounds frame))
              (id (excali--get frame 'id)))
    (seq-filter (lambda (e)
                  (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excali--export-rotated-bounds e))
                              (frame-id (excali--get e 'frameId)))
                    (and (or (null frame-id) (equal frame-id id))
                         (<= x1 fx2) (<= fx1 x2) (<= y1 fy2) (<= fy1 y2))))
                elements)))

(defun excali--export-selection (selection-only)
  "Return (ELEMENTS . EXPORTING-FRAME) to export.
Port of upstream `prepareElementsForExport': the whole scene, or with
SELECTION-ONLY and a selection, the selected elements with their bound
text; a single selected frame exports the elements overlapping it."
  (let* ((live (excali--live-elements))
         (selected (and selection-only excali--selection)))
    (cond
     ((null selected) (cons live nil))
     ((and (null (cdr selected)) (excali--frame-like-p (car selected)))
      (cons (excali--elements-overlapping-frame live (car selected)) (car selected)))
     (t
      (let* ((ids (mapcar (lambda (e) (excali--get e 'id)) selected))
             (frames (and (cdr selected)
                          (delq nil (mapcar (lambda (e) (and (excali--frame-like-p e)
                                                             (excali--get e 'id)))
                                            selected)))))
        (cons (seq-filter (lambda (e)
                            (or (memq e selected)
                                (member (excali--get e 'containerId) ids)
                                (member (excali--get e 'frameId) frames)))
                          live)
              nil))))))

(defun excali--truncate-label (element max-width)
  "Truncate text ELEMENT to MAX-WIDTH like upstream `truncateText'."
  (when (> (excali--get element 'width) max-width)
    (let* ((text (excali--get element 'text))
           (size (excali--get element 'fontSize))
           (family (excali--get element 'fontFamily)))
      (when (> (excali-native-text-width text size family) max-width)
        (catch 'done
          (cl-loop for i from (length text) downto 1
                   for candidate = (concat (substring text 0 i) "...")
                   when (<= (excali-native-text-width candidate size family) max-width)
                   do (excali--put element 'text candidate)
                   (excali--put element 'originalText candidate)
                   (throw 'done nil))))
      (excali--put element 'width (float max-width))))
  element)

(defun excali--frame-label-element (frame)
  "Return the text element upstream exports as FRAME's name."
  (let ((label (excali--make-text-element
                (excali--get frame 'x)
                (- (excali--get frame 'y) excali-frame-name-offset-y)
                (excali--frame-title frame)
                (cons 'fontFamily 2) (cons 'fontSize excali-frame-name-font-size)
                (cons 'lineHeight excali-frame-name-line-height)
                (cons 'strokeColor excali-frame-name-color))))
    (excali--put label 'y (- (excali--get label 'y) (excali--get label 'height)))
    (excali--truncate-label label (excali--get frame 'width))))

(defun excali--add-frame-labels (elements)
  "Return ELEMENTS with a name label before each frame."
  (mapcan (lambda (e)
            (if (excali--frame-like-p e)
                (list (excali--frame-label-element e) e)
              (list e)))
          elements))

(defun excali--export-background ()
  "Return the export background as a hex color, or nil for none."
  (when excali-export-background
    (let ((color (alist-get 'viewBackgroundColor (alist-get 'appState excali--doc))))
      (cond ((not (stringp color)) "#ffffff")
            ((string-prefix-p "#" color) color)
            ((equal color "transparent") nil)
            (t (or (ignore-errors
                     (apply #'color-rgb-to-hex
                            (append (color-name-to-rgb color) '(2))))
                   "#ffffff"))))))

(defun excali--export-plan (selection-only svg)
  "Return a plist describing an export of the scene or SELECTION-ONLY.
SVG non-nil keeps frame clipping for a single exported frame, as
upstream's SVG path does.  Keys: :elements (to embed), :render (with
frame labels), :x :y :width :height, :clip, :outline."
  (pcase-let* ((`(,elements . ,frame) (excali--export-selection selection-only)))
    (unless elements
      (user-error "Cannot export an empty canvas"))
    (let* ((render (if frame elements (excali--add-frame-labels elements)))
           (geometry (if frame
                         (excali--export-geometry (list frame) 0)
                       (excali--export-geometry (excali--export-root-elements render)
                                               excali-export-padding))))
      (list :elements elements :render render
            :x (nth 0 geometry) :y (nth 1 geometry)
            :width (nth 2 geometry) :height (nth 3 geometry)
            :clip (or (not frame) (and svg t))
            :outline (not frame)))))

(defun excali--export-scene-json (elements)
  "Return the .excalidraw text embedded for ELEMENTS."
  (let ((doc (copy-alist excali--doc)))
    (when (fboundp 'excali--save-current-style)
      (setf (alist-get 'appState doc)
            (excali--save-current-style (alist-get 'appState doc))))
    (excali--serialize-doc doc elements)))

(defun excali--export-natives (elements)
  "Return the native vectors of ELEMENTS."
  (vconcat (mapcar #'excali--native-element elements)))

;;;; Commands

(defun excali--export-default-name (extension)
  "Return a default export file name with EXTENSION."
  (concat (if excali--file
              (file-name-sans-extension (file-name-nondirectory excali--file))
            "Untitled")
          (if excali-export-embed-scene ".excalidraw." ".")
          extension))

(defun excali--export-read-args (extension)
  "Read the file and selection arguments of an export to EXTENSION."
  (list (read-file-name (format "Export %s: " (upcase extension)) nil nil nil
                        (excali--export-default-name extension))
        (and excali--selection (not current-prefix-arg))))

(defun excali-export-png (file &optional selection-only)
  "Export the scene to the PNG FILE.
With SELECTION-ONLY (interactively: when something is selected and no
prefix argument is given), export only the selection.  See
`excali-export-background', `excali-export-padding', `excali-export-scale'
and `excali-export-embed-scene'."
  (interactive (excali--export-read-args "png"))
  (let* ((plan (excali--export-plan selection-only nil))
         (text (and excali-export-embed-scene
                    (excali--byte-string-to-bytes
                     (excali--encode-payload
                      (excali--export-scene-json (plist-get plan :elements)))))))
    (unless (excali-native-export-png
             (expand-file-name file) (excali--export-natives (plist-get plan :render))
             (plist-get plan :x) (plist-get plan :y)
             (plist-get plan :width) (plist-get plan :height)
             (excali--export-background) (plist-get plan :clip)
             (plist-get plan :outline) (float excali-export-scale) text)
      (error "Could not export %s" file))
    (message "Exported %s" file)
    file))

(defun excali--svg-embed (svg width height scale payload)
  "Return Cairo's SVG text with upstream's root size and metadata.
WIDTH and HEIGHT are the scene size, SCALE the export scale; PAYLOAD
is the base64 scene payload or nil."
  (with-temp-buffer
    (insert svg)
    (goto-char (point-min))
    (unless (re-search-forward "<svg\\b[^>]*>" nil t)
      (error "Unexpected SVG output"))
    (let ((end (point-marker))
          (start (match-beginning 0)))
      (save-restriction
        (narrow-to-region start end)
        (dolist (attr `(("width" . ,(* width scale)) ("height" . ,(* height scale))))
          (let ((value (format "%s=\"%s\"" (car attr) (excali--json-number (cdr attr)))))
            (goto-char (point-min))
            (when (re-search-forward (format "\\_<%s=\"[^\"]*\"" (car attr)) nil t)
              (replace-match value t t)))))
      (goto-char end)
      (insert "\n<!-- svg-source:excalidraw -->")
      (when payload
        (insert "\n<metadata><!-- payload-type:" excali-export-mime-type
                " --><!-- payload-version:2 --><!-- payload-start -->"
                payload "<!-- payload-end --></metadata>")))
    (buffer-string)))

(defun excali-export-svg (file &optional selection-only)
  "Export the scene to the SVG FILE.
SELECTION-ONLY is as for `excali-export-png'."
  (interactive (excali--export-read-args "svg"))
  (let* ((plan (excali--export-plan selection-only t))
         (svg (excali-native-export-svg
               (excali--export-natives (plist-get plan :render))
               (plist-get plan :x) (plist-get plan :y)
               (plist-get plan :width) (plist-get plan :height)
               (excali--export-background) (plist-get plan :clip)
               (plist-get plan :outline)))
         (payload (and excali-export-embed-scene
                       (base64-encode-string
                        (excali--byte-string-to-bytes
                         (excali--encode-payload
                          (excali--export-scene-json (plist-get plan :elements))))
                        t))))
    (unless svg
      (error "This module was built without Cairo's SVG surface"))
    (let ((text (excali--svg-embed svg (plist-get plan :width) (plist-get plan :height)
                                  excali-export-scale payload)))
      (with-temp-file (expand-file-name file)
        (setq buffer-file-coding-system 'utf-8-unix)
        (insert text)))
    (message "Exported %s" file)
    file))

(defun excali-export-image (file &optional selection-only)
  "Export the scene to FILE as PNG or SVG, by FILE's extension.
SELECTION-ONLY is as for `excali-export-png'."
  (interactive
   (list (read-file-name "Export image (.png or .svg): " nil nil nil
                         (excali--export-default-name "png"))
         (and excali--selection (not current-prefix-arg))))
  (if (equal (downcase (or (file-name-extension file) "")) "svg")
      (excali-export-svg file selection-only)
    (excali-export-png file selection-only)))

(provide 'excali-export)
;;; excali-export.el ends here
