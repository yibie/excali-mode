;;; excali-export-test.el --- Tests for PNG/SVG export and import  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)

(defmacro excali-export-test--with-scene (elements &rest body)
  "Run BODY in a temporary scene buffer showing ELEMENTS."
  (declare (indent 1))
  `(with-temp-buffer
     (setq excali--native-cache (make-hash-table :test #'eq)
           excali--doc (excali--empty-doc)
           excali--zoom 1.0 excali--pixel-scale 1.0
           excali--scroll-x 0.0 excali--scroll-y 0.0
           excali--selection nil
           excali--elements ,elements)
     (let ((excali-export-scale 1)
           (excali-export-padding 10)
           (excali-export-background t)
           (excali-export-embed-scene t))
       ,@body)))

(defmacro excali-export-test--with-file (var extension &rest body)
  "Bind VAR to a temporary file name with EXTENSION around BODY."
  (declare (indent 2))
  `(let ((,var (make-temp-file "excali-export" nil ,extension)))
     (unwind-protect (progn ,@body)
       (delete-file ,var))))

(defun excali-export-test--rect (x y w h &rest props)
  "Return a rectangle element."
  (apply #'excali--make-element "rectangle" x y (cons 'width (float w)) (cons 'height (float h))
         props))

(defun excali-export-test--png-chunks (bytes)
  "Return the (TYPE . DATA) chunks of PNG BYTES."
  (let ((pos 8) chunks)
    (while (< pos (length bytes))
      (let ((len (excali--png-u32 bytes pos)))
        (push (cons (substring bytes (+ pos 4) (+ pos 8))
                    (substring bytes (+ pos 8) (+ pos 8 len)))
              chunks)
        (setq pos (+ pos 12 len))))
    (nreverse chunks)))

(defun excali-export-test--png-size (bytes)
  "Return (WIDTH HEIGHT COLOR-TYPE) from the IHDR of PNG BYTES."
  (list (excali--png-u32 bytes 16) (excali--png-u32 bytes 20) (aref bytes 25)))

(defun excali-export-test--json (elements)
  "Return ELEMENTS as JSON text, for comparing scenes."
  (excali--json-encode (vconcat elements)))

(defun excali-export-test--restored (doc)
  "Return the elements of the imported DOC as JSON.
DOC must also restore cleanly, as `excali-open' does with it."
  (should (= (length (alist-get 'elements (excali--restore-doc doc)))
             (length (alist-get 'elements doc))))
  (excali-export-test--json (alist-get 'elements doc)))

;;;; Geometry

(ert-deftest excali-export-test-geometry ()
  "Bounds are the union of rotated element bounds plus the padding."
  (let ((a (excali-export-test--rect 10 20 100 50))
        (b (excali-export-test--rect 200 -30 40 40)))
    (should (equal (excali--export-geometry (list a b) 10)
                   '(0.0 -40.0 250.0 120.0)))
    (should (equal (excali--export-geometry (list a) 0) '(10.0 20.0 100.0 50.0)))
    ;; A 100x50 box turned a quarter turn spans 50x100 about its centre.
    (excali--put a 'angle (/ float-pi 2))
    (pcase-let ((`(,x ,y ,w ,h) (excali--export-geometry (list a) 0)))
      (should (< (abs (- x 35)) 1e-9))
      (should (< (abs (- y -5)) 1e-9))
      (should (< (abs (- w 50)) 1e-9))
      (should (< (abs (- h 100)) 1e-9)))))

(ert-deftest excali-export-test-root-elements ()
  "Children of exported frames do not count for the bounds."
  (let* ((frame (excali--make-element "frame" 0 0 (cons 'width 100.0) (cons 'height 100.0)
                                     (cons 'name :null)))
         (child (excali-export-test--rect 50 50 500 500 (cons 'frameId (excali--get frame 'id))))
         (other (excali-export-test--rect 50 50 10 10 (cons 'frameId "elsewhere"))))
    (should (equal (excali--export-root-elements (list frame child other))
                   (list frame other)))))

(ert-deftest excali-export-test-frame-labels ()
  "Exports add each frame's name as Helvetica 14px text above it."
  (excali-export-test--with-scene nil
    (let* ((frame (excali--make-element "frame" 10 100 (cons 'width 300.0) (cons 'height 50.0)
                                       (cons 'name "Title")))
           (narrow (excali--make-element "frame" 10 300 (cons 'width 80.0) (cons 'height 50.0)
                                        (cons 'name "A long frame title")))
           (out (excali--add-frame-labels (list frame narrow)))
           (label (nth 0 out))
           (cut (nth 2 out)))
      (should (eq (nth 1 out) frame))
      (should (eq (nth 3 out) narrow))
      (should (equal (excali--get label 'text) "Title"))
      (should (= (excali--get label 'fontFamily) 2))
      (should (= (excali--get label 'fontSize) 14))
      (should (equal (excali--get label 'strokeColor) "#999999"))
      (should (= (excali--get label 'x) 10))
      ;; y = frame.y - 3 - height, one 14 * 1.25 line.
      (should (= (excali--get label 'height) 17.5))
      (should (= (excali--get label 'y) (- 100 3 17.5)))
      (should (string-suffix-p "..." (excali--get cut 'text)))
      (should (<= (excali-native-text-width (excali--get cut 'text) 14 2) 80))
      (should (= (excali--get cut 'width) 80)))))

(ert-deftest excali-export-test-selection ()
  "Selections export with bound text; a lone frame exports its area."
  (let* ((box (excali-export-test--rect 0 0 100 50))
         (text (excali--make-element "text" 10 10 (cons 'containerId (excali--get box 'id))
                                    (cons 'text "hi")))
         (frame (excali--make-element "frame" 200 0 (cons 'width 100.0) (cons 'height 100.0)
                                     (cons 'name :null)))
         (inside (excali-export-test--rect 220 20 20 20))
         (far (excali-export-test--rect 900 900 20 20)))
    (excali-export-test--with-scene (list box text frame inside far)
      (should (equal (excali--export-selection nil)
                     (cons (list box text frame inside far) nil)))
      (setq excali--selection (list box))
      (should (equal (excali--export-selection t) (cons (list box text) nil)))
      (setq excali--selection (list frame))
      (should (equal (excali--export-selection t) (cons (list frame inside) frame)))
      (let ((plan (excali--export-plan t nil)))
        (should (equal (list (plist-get plan :x) (plist-get plan :y)
                             (plist-get plan :width) (plist-get plan :height))
                       '(200.0 0.0 100.0 100.0)))
        (should-not (plist-get plan :outline))
        (should-not (plist-get plan :clip)))
      (should (plist-get (excali--export-plan t t) :clip)))))

;;;; Payloads

(ert-deftest excali-export-test-payload-encoding ()
  "Payloads follow upstream `encode': zlib-compressed byte strings."
  (let* ((text "{\"type\":\"excalidraw\",\"text\":\"héllo \r\n wörld\"}")
         (payload (excali--encode-payload text))
         (data (json-parse-string payload :object-type 'alist)))
    (should (string-prefix-p "{\"version\":\"1\",\"encoding\":\"bstring\",\"compressed\":true,\"encoded\":\""
                             payload))
    (should (cl-every (lambda (c) (< c 256)) payload))
    ;; zlib header.
    (should (eq (aref (alist-get 'encoded data) 0) #x78))
    (should (equal (excali--decode-payload payload) text))
    (let ((plain (excali--encode-payload text t)))
      (should (string-search "\"compressed\":false" plain))
      (should (equal (excali--decode-payload plain) text)))
    ;; A scene given directly (legacy) is returned as is.
    (should (equal (excali--decode-payload "{\"type\":\"excalidraw\",\"elements\":[]}")
                   "{\"type\":\"excalidraw\",\"elements\":[]}"))
    (should-error (excali--decode-payload "{\"encoding\":\"base64\",\"encoded\":\"\"}")
                  :type 'user-error)))

(ert-deftest excali-export-test-zlib ()
  "The module's zlib round-trips arbitrary bytes and rejects garbage."
  (let ((bytes (apply #'unibyte-string (number-sequence 0 255))))
    (should (equal (excali-native-zlib-decompress (excali-native-zlib-compress bytes)) bytes))
    (should-not (excali-native-zlib-decompress "garbage"))))

;;;; PNG

(ert-deftest excali-export-test-png-size-and-embed ()
  "PNG size follows bounds, padding and scale; the scene sits before IEND."
  (let ((rect (excali-export-test--rect 0 0 100 50 (cons 'roughness 0))))
    (excali-export-test--with-scene (list rect)
      (excali-export-test--with-file file ".png"
        (excali-export-png file)
        (let* ((bytes (excali--read-image-file file))
               (chunks (excali-export-test--png-chunks bytes))
               (types (mapcar #'car chunks)))
          (should (equal (excali-export-test--png-size bytes) '(120 70 2)))
          (should (equal (last types 2) '("tEXt" "IEND")))
          (should (string-prefix-p (concat excali-export-mime-type "\0")
                                   (cdr (assoc "tEXt" chunks)))))
        (let ((excali-export-scale 2) (excali-export-embed-scene nil)
              (excali-export-background nil))
          (excali-export-png file)
          (let ((bytes (excali--read-image-file file)))
            ;; Transparent background: RGBA.
            (should (equal (excali-export-test--png-size bytes) '(240 140 6)))
            (should-not (assoc "tEXt" (excali-export-test--png-chunks bytes)))))))))

(ert-deftest excali-export-test-png-roundtrip ()
  "An exported PNG opens again with the same elements and files."
  (let* ((png (let ((fb (excali-native-fb-create 2 2))
                    (tmp (make-temp-file "excali" nil ".png")))
                (unwind-protect
                    (progn (excali-native-fb-write-png fb tmp) (excali--read-image-file tmp))
                  (delete-file tmp))))
         (image (excali--make-element "image" 150 0 (cons 'width 20.0) (cons 'height 20.0)
                                     (cons 'fileId "f1") (cons 'status "saved")
                                     (cons 'scale [1 1]) (cons 'crop :null)))
         (elements (list (excali-export-test--rect 0 0 100 50)
                         (excali--make-text-element 10 70 "héllo\nwörld ✓")
                         image)))
    (excali-export-test--with-scene elements
      (setf (alist-get 'files excali--doc)
            (list (cons 'f1 (list (cons 'mimeType "image/png") (cons 'id "f1")
                                  (cons 'dataURL (excali--image-data-url "image/png" png))
                                  (cons 'created 1)))))
      (excali-export-test--with-file file ".png"
        (excali-export-png file)
        (let ((doc (excali--read-scene-file file)))
          (should (equal (alist-get 'type doc) "excalidraw"))
          (should (equal (excali-export-test--restored doc)
                         (excali-export-test--json elements)))
          (should (equal (alist-get 'dataURL (alist-get 'f1 (alist-get 'files doc)))
                         (excali--image-data-url "image/png" png))))))))

(ert-deftest excali-export-test-png-import-variants ()
  "Uncompressed and legacy PNG payloads import too; others are refused."
  (let* ((elements (list (excali-export-test--rect 0 0 10 10)))
         (scene (with-temp-buffer
                  (setq excali--doc (excali--empty-doc))
                  (excali--serialize-doc excali--doc elements))))
    (excali-export-test--with-scene elements
      (excali-export-test--with-file file ".png"
        (dolist (payload (list (excali--encode-payload scene t) scene))
          (should (excali-native-export-png
                   file (vconcat (mapcar #'excali--native-element elements))
                   0.0 0.0 10.0 10.0 nil t t 1.0
                   (excali--byte-string-to-bytes payload)))
          (should (equal (excali-export-test--restored (excali--read-scene-file file))
                         (excali-export-test--json elements))))
        ;; No tEXt chunk at all.
        (should (excali-native-export-png file [] 0.0 0.0 10.0 10.0 nil t t 1.0 nil))
        (should-error (excali--read-scene-file file) :type 'user-error)))))

;;;; SVG

(ert-deftest excali-export-test-svg-structure-and-roundtrip ()
  "SVG exports carry upstream's root size and metadata and open again."
  (let ((elements (list (excali-export-test--rect 0 0 100 50 (cons 'backgroundColor "#ffc9c9"))
                        (excali--make-text-element 0 60 "Ünïcödé"))))
    (excali-export-test--with-scene elements
      (excali-export-test--with-file file ".svg"
        (let ((excali-export-scale 2))
          (excali-export-svg file))
        (let ((svg (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string)))
              (plan (excali--export-plan nil t)))
          (should (string-match "<svg\\b[^>]*>" svg))
          (let ((root (match-string 0 svg))
                (w (plist-get plan :width)) (h (plist-get plan :height)))
            (should (string-search (format "width=\"%s\"" (excali--json-number (* 2 w))) root))
            (should (string-search (format "height=\"%s\"" (excali--json-number (* 2 h))) root))
            (should (string-search (format "viewBox=\"0 0 %s %s\""
                                           (excali--json-number w) (excali--json-number h))
                                   root)))
          (should (string-match-p
                   (concat "<!-- svg-source:excalidraw -->\n<metadata>"
                           "<!-- payload-type:application/vnd\\.excalidraw\\+json -->"
                           "<!-- payload-version:2 --><!-- payload-start -->"
                           "[A-Za-z0-9+/=]+<!-- payload-end --></metadata>")
                   svg)))
        (should (equal (excali-export-test--restored (excali--read-scene-file file))
                       (excali-export-test--json elements)))))))

(ert-deftest excali-export-test-svg-payload-versions ()
  "Version 1 payloads are base64 UTF-8 text; version 2 byte strings."
  (let* ((scene "{\"type\":\"excalidraw\",\"elements\":[],\"x\":\"é\"}")
         (v1 (concat "<svg><!-- payload-type:application/vnd.excalidraw+json -->"
                     "<!-- payload-start -->"
                     (base64-encode-string (encode-coding-string scene 'utf-8) t)
                     "<!-- payload-end --></svg>"))
         (v2 (concat "<svg><!-- payload-type:application/vnd.excalidraw+json -->"
                     "<!-- payload-version:2 --><!-- payload-start -->\n  "
                     (base64-encode-string
                      (excali--byte-string-to-bytes (excali--encode-payload scene)) t)
                     "\n<!-- payload-end --></svg>")))
    (should (equal (excali--decode-payload (excali--svg-scene-payload v1)) scene))
    (should (equal (excali--decode-payload (excali--svg-scene-payload v2)) scene))
    (should-not (excali--svg-scene-payload "<svg/>"))))

(ert-deftest excali-export-test-render-content ()
  "Exported pixels show the scene over the background, padded."
  (let ((rect (excali-export-test--rect 0 0 40 40 (cons 'roughness 0)
                                       (cons 'strokeColor "transparent")
                                       (cons 'backgroundColor "#ff0000")
                                       (cons 'fillStyle "solid"))))
    (excali-export-test--with-scene (list rect)
      (setf (alist-get 'viewBackgroundColor (alist-get 'appState excali--doc)) "#0000ff")
      (excali-export-test--with-file file ".png"
        (excali-export-png file)
        ;; Decode the export as an image and look at it.
        (let ((info (excali-native-image-register
                     "t-export" (excali--image-data-url "image/png"
                                                       (excali--read-image-file file)))))
          (unwind-protect
              (progn
                (should (equal info [60.0 60.0 "image/png"]))
                (setq excali--doc (list (cons 'files nil)))
                (setq excali--elements
                      (list (excali--make-element "image" 0 0 (cons 'width 60.0)
                                                 (cons 'height 60.0) (cons 'fileId "t-export")
                                                 (cons 'scale [1 1]) (cons 'crop :null))))
                (let ((fb (excali-native-fb-create 60 60)))
                  (clrhash excali--native-cache)
                  (excali-native-fb-render fb 1.0 1.0 0.0 0.0 (excali--visible-elements) nil)
                  (should (= (excali-native-fb-pixel fb 5 5) #xff0000ff))
                  (should (= (excali-native-fb-pixel fb 30 30) #xffff0000))))
            (excali-native-image-forget "t-export")))))))

(ert-deftest excali-export-test-empty-scene ()
  "Exporting nothing is an error."
  (excali-export-test--with-scene nil
    (should-error (excali-export-png "/nonexistent/x.png") :type 'user-error)))

(ert-deftest excali-export-test-open-files ()
  "PNG and SVG scenes are not saved over; .excalidraw files are."
  (should (excali--scene-file-p "a.excalidraw"))
  (should-not (excali--scene-file-p "a.excalidraw.png"))
  (should-not (excali--scene-file-p "a.SVG")))

;;; excali-export-test.el ends here
