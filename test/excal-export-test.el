;;; excal-export-test.el --- Tests for PNG/SVG export and import  -*- lexical-binding: t; -*-

(require 'ert)
(require 'excal)

(defmacro excal-export-test--with-scene (elements &rest body)
  "Run BODY in a temporary scene buffer showing ELEMENTS."
  (declare (indent 1))
  `(with-temp-buffer
     (setq excal--native-cache (make-hash-table :test #'eq)
           excal--doc (excal--empty-doc)
           excal--zoom 1.0 excal--pixel-scale 1.0
           excal--scroll-x 0.0 excal--scroll-y 0.0
           excal--selection nil
           excal--elements ,elements)
     (let ((excal-export-scale 1)
           (excal-export-padding 10)
           (excal-export-background t)
           (excal-export-embed-scene t))
       ,@body)))

(defmacro excal-export-test--with-file (var extension &rest body)
  "Bind VAR to a temporary file name with EXTENSION around BODY."
  (declare (indent 2))
  `(let ((,var (make-temp-file "excal-export" nil ,extension)))
     (unwind-protect (progn ,@body)
       (delete-file ,var))))

(defun excal-export-test--rect (x y w h &rest props)
  "Return a rectangle element."
  (apply #'excal--make-element "rectangle" x y (cons 'width (float w)) (cons 'height (float h))
         props))

(defun excal-export-test--png-chunks (bytes)
  "Return the (TYPE . DATA) chunks of PNG BYTES."
  (let ((pos 8) chunks)
    (while (< pos (length bytes))
      (let ((len (excal--png-u32 bytes pos)))
        (push (cons (substring bytes (+ pos 4) (+ pos 8))
                    (substring bytes (+ pos 8) (+ pos 8 len)))
              chunks)
        (setq pos (+ pos 12 len))))
    (nreverse chunks)))

(defun excal-export-test--png-size (bytes)
  "Return (WIDTH HEIGHT COLOR-TYPE) from the IHDR of PNG BYTES."
  (list (excal--png-u32 bytes 16) (excal--png-u32 bytes 20) (aref bytes 25)))

(defun excal-export-test--json (elements)
  "Return ELEMENTS as JSON text, for comparing scenes."
  (excal--json-encode (vconcat elements)))

(defun excal-export-test--restored (doc)
  "Return the elements of the imported DOC as JSON.
DOC must also restore cleanly, as `excal-open' does with it."
  (should (= (length (alist-get 'elements (excal--restore-doc doc)))
             (length (alist-get 'elements doc))))
  (excal-export-test--json (alist-get 'elements doc)))

;;;; Geometry

(ert-deftest excal-export-test-geometry ()
  "Bounds are the union of rotated element bounds plus the padding."
  (let ((a (excal-export-test--rect 10 20 100 50))
        (b (excal-export-test--rect 200 -30 40 40)))
    (should (equal (excal--export-geometry (list a b) 10)
                   '(0.0 -40.0 250.0 120.0)))
    (should (equal (excal--export-geometry (list a) 0) '(10.0 20.0 100.0 50.0)))
    ;; A 100x50 box turned a quarter turn spans 50x100 about its centre.
    (excal--put a 'angle (/ float-pi 2))
    (pcase-let ((`(,x ,y ,w ,h) (excal--export-geometry (list a) 0)))
      (should (< (abs (- x 35)) 1e-9))
      (should (< (abs (- y -5)) 1e-9))
      (should (< (abs (- w 50)) 1e-9))
      (should (< (abs (- h 100)) 1e-9)))))

(ert-deftest excal-export-test-root-elements ()
  "Children of exported frames do not count for the bounds."
  (let* ((frame (excal--make-element "frame" 0 0 (cons 'width 100.0) (cons 'height 100.0)
                                     (cons 'name :null)))
         (child (excal-export-test--rect 50 50 500 500 (cons 'frameId (excal--get frame 'id))))
         (other (excal-export-test--rect 50 50 10 10 (cons 'frameId "elsewhere"))))
    (should (equal (excal--export-root-elements (list frame child other))
                   (list frame other)))))

(ert-deftest excal-export-test-frame-labels ()
  "Exports add each frame's name as Helvetica 14px text above it."
  (excal-export-test--with-scene nil
    (let* ((frame (excal--make-element "frame" 10 100 (cons 'width 300.0) (cons 'height 50.0)
                                       (cons 'name "Title")))
           (narrow (excal--make-element "frame" 10 300 (cons 'width 80.0) (cons 'height 50.0)
                                        (cons 'name "A long frame title")))
           (out (excal--add-frame-labels (list frame narrow)))
           (label (nth 0 out))
           (cut (nth 2 out)))
      (should (eq (nth 1 out) frame))
      (should (eq (nth 3 out) narrow))
      (should (equal (excal--get label 'text) "Title"))
      (should (= (excal--get label 'fontFamily) 2))
      (should (= (excal--get label 'fontSize) 14))
      (should (equal (excal--get label 'strokeColor) "#999999"))
      (should (= (excal--get label 'x) 10))
      ;; y = frame.y - 3 - height, one 14 * 1.25 line.
      (should (= (excal--get label 'height) 17.5))
      (should (= (excal--get label 'y) (- 100 3 17.5)))
      (should (string-suffix-p "..." (excal--get cut 'text)))
      (should (<= (excal-native-text-width (excal--get cut 'text) 14 2) 80))
      (should (= (excal--get cut 'width) 80)))))

(ert-deftest excal-export-test-selection ()
  "Selections export with bound text; a lone frame exports its area."
  (let* ((box (excal-export-test--rect 0 0 100 50))
         (text (excal--make-element "text" 10 10 (cons 'containerId (excal--get box 'id))
                                    (cons 'text "hi")))
         (frame (excal--make-element "frame" 200 0 (cons 'width 100.0) (cons 'height 100.0)
                                     (cons 'name :null)))
         (inside (excal-export-test--rect 220 20 20 20))
         (far (excal-export-test--rect 900 900 20 20)))
    (excal-export-test--with-scene (list box text frame inside far)
      (should (equal (excal--export-selection nil)
                     (cons (list box text frame inside far) nil)))
      (setq excal--selection (list box))
      (should (equal (excal--export-selection t) (cons (list box text) nil)))
      (setq excal--selection (list frame))
      (should (equal (excal--export-selection t) (cons (list frame inside) frame)))
      (let ((plan (excal--export-plan t nil)))
        (should (equal (list (plist-get plan :x) (plist-get plan :y)
                             (plist-get plan :width) (plist-get plan :height))
                       '(200.0 0.0 100.0 100.0)))
        (should-not (plist-get plan :outline))
        (should-not (plist-get plan :clip)))
      (should (plist-get (excal--export-plan t t) :clip)))))

;;;; Payloads

(ert-deftest excal-export-test-payload-encoding ()
  "Payloads follow upstream `encode': zlib-compressed byte strings."
  (let* ((text "{\"type\":\"excalidraw\",\"text\":\"héllo \r\n wörld\"}")
         (payload (excal--encode-payload text))
         (data (json-parse-string payload :object-type 'alist)))
    (should (string-prefix-p "{\"version\":\"1\",\"encoding\":\"bstring\",\"compressed\":true,\"encoded\":\""
                             payload))
    (should (cl-every (lambda (c) (< c 256)) payload))
    ;; zlib header.
    (should (eq (aref (alist-get 'encoded data) 0) #x78))
    (should (equal (excal--decode-payload payload) text))
    (let ((plain (excal--encode-payload text t)))
      (should (string-search "\"compressed\":false" plain))
      (should (equal (excal--decode-payload plain) text)))
    ;; A scene given directly (legacy) is returned as is.
    (should (equal (excal--decode-payload "{\"type\":\"excalidraw\",\"elements\":[]}")
                   "{\"type\":\"excalidraw\",\"elements\":[]}"))
    (should-error (excal--decode-payload "{\"encoding\":\"base64\",\"encoded\":\"\"}")
                  :type 'user-error)))

(ert-deftest excal-export-test-zlib ()
  "The module's zlib round-trips arbitrary bytes and rejects garbage."
  (let ((bytes (apply #'unibyte-string (number-sequence 0 255))))
    (should (equal (excal-native-zlib-decompress (excal-native-zlib-compress bytes)) bytes))
    (should-not (excal-native-zlib-decompress "garbage"))))

;;;; PNG

(ert-deftest excal-export-test-png-size-and-embed ()
  "PNG size follows bounds, padding and scale; the scene sits before IEND."
  (let ((rect (excal-export-test--rect 0 0 100 50 (cons 'roughness 0))))
    (excal-export-test--with-scene (list rect)
      (excal-export-test--with-file file ".png"
        (excal-export-png file)
        (let* ((bytes (excal--read-image-file file))
               (chunks (excal-export-test--png-chunks bytes))
               (types (mapcar #'car chunks)))
          (should (equal (excal-export-test--png-size bytes) '(120 70 2)))
          (should (equal (last types 2) '("tEXt" "IEND")))
          (should (string-prefix-p (concat excal-export-mime-type "\0")
                                   (cdr (assoc "tEXt" chunks)))))
        (let ((excal-export-scale 2) (excal-export-embed-scene nil)
              (excal-export-background nil))
          (excal-export-png file)
          (let ((bytes (excal--read-image-file file)))
            ;; Transparent background: RGBA.
            (should (equal (excal-export-test--png-size bytes) '(240 140 6)))
            (should-not (assoc "tEXt" (excal-export-test--png-chunks bytes)))))))))

(ert-deftest excal-export-test-png-roundtrip ()
  "An exported PNG opens again with the same elements and files."
  (let* ((png (let ((fb (excal-native-fb-create 2 2))
                    (tmp (make-temp-file "excal" nil ".png")))
                (unwind-protect
                    (progn (excal-native-fb-write-png fb tmp) (excal--read-image-file tmp))
                  (delete-file tmp))))
         (image (excal--make-element "image" 150 0 (cons 'width 20.0) (cons 'height 20.0)
                                     (cons 'fileId "f1") (cons 'status "saved")
                                     (cons 'scale [1 1]) (cons 'crop :null)))
         (elements (list (excal-export-test--rect 0 0 100 50)
                         (excal--make-text-element 10 70 "héllo\nwörld ✓")
                         image)))
    (excal-export-test--with-scene elements
      (setf (alist-get 'files excal--doc)
            (list (cons 'f1 (list (cons 'mimeType "image/png") (cons 'id "f1")
                                  (cons 'dataURL (excal--image-data-url "image/png" png))
                                  (cons 'created 1)))))
      (excal-export-test--with-file file ".png"
        (excal-export-png file)
        (let ((doc (excal--read-scene-file file)))
          (should (equal (alist-get 'type doc) "excalidraw"))
          (should (equal (excal-export-test--restored doc)
                         (excal-export-test--json elements)))
          (should (equal (alist-get 'dataURL (alist-get 'f1 (alist-get 'files doc)))
                         (excal--image-data-url "image/png" png))))))))

(ert-deftest excal-export-test-png-import-variants ()
  "Uncompressed and legacy PNG payloads import too; others are refused."
  (let* ((elements (list (excal-export-test--rect 0 0 10 10)))
         (scene (with-temp-buffer
                  (setq excal--doc (excal--empty-doc))
                  (excal--serialize-doc excal--doc elements))))
    (excal-export-test--with-scene elements
      (excal-export-test--with-file file ".png"
        (dolist (payload (list (excal--encode-payload scene t) scene))
          (should (excal-native-export-png
                   file (vconcat (mapcar #'excal--native-element elements))
                   0.0 0.0 10.0 10.0 nil t t 1.0
                   (excal--byte-string-to-bytes payload)))
          (should (equal (excal-export-test--restored (excal--read-scene-file file))
                         (excal-export-test--json elements))))
        ;; No tEXt chunk at all.
        (should (excal-native-export-png file [] 0.0 0.0 10.0 10.0 nil t t 1.0 nil))
        (should-error (excal--read-scene-file file) :type 'user-error)))))

;;;; SVG

(ert-deftest excal-export-test-svg-structure-and-roundtrip ()
  "SVG exports carry upstream's root size and metadata and open again."
  (let ((elements (list (excal-export-test--rect 0 0 100 50 (cons 'backgroundColor "#ffc9c9"))
                        (excal--make-text-element 0 60 "Ünïcödé"))))
    (excal-export-test--with-scene elements
      (excal-export-test--with-file file ".svg"
        (let ((excal-export-scale 2))
          (excal-export-svg file))
        (let ((svg (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string)))
              (plan (excal--export-plan nil t)))
          (should (string-match "<svg\\b[^>]*>" svg))
          (let ((root (match-string 0 svg))
                (w (plist-get plan :width)) (h (plist-get plan :height)))
            (should (string-search (format "width=\"%s\"" (excal--json-number (* 2 w))) root))
            (should (string-search (format "height=\"%s\"" (excal--json-number (* 2 h))) root))
            (should (string-search (format "viewBox=\"0 0 %s %s\""
                                           (excal--json-number w) (excal--json-number h))
                                   root)))
          (should (string-match-p
                   (concat "<!-- svg-source:excalidraw -->\n<metadata>"
                           "<!-- payload-type:application/vnd\\.excalidraw\\+json -->"
                           "<!-- payload-version:2 --><!-- payload-start -->"
                           "[A-Za-z0-9+/=]+<!-- payload-end --></metadata>")
                   svg)))
        (should (equal (excal-export-test--restored (excal--read-scene-file file))
                       (excal-export-test--json elements)))))))

(ert-deftest excal-export-test-svg-payload-versions ()
  "Version 1 payloads are base64 UTF-8 text; version 2 byte strings."
  (let* ((scene "{\"type\":\"excalidraw\",\"elements\":[],\"x\":\"é\"}")
         (v1 (concat "<svg><!-- payload-type:application/vnd.excalidraw+json -->"
                     "<!-- payload-start -->"
                     (base64-encode-string (encode-coding-string scene 'utf-8) t)
                     "<!-- payload-end --></svg>"))
         (v2 (concat "<svg><!-- payload-type:application/vnd.excalidraw+json -->"
                     "<!-- payload-version:2 --><!-- payload-start -->\n  "
                     (base64-encode-string
                      (excal--byte-string-to-bytes (excal--encode-payload scene)) t)
                     "\n<!-- payload-end --></svg>")))
    (should (equal (excal--decode-payload (excal--svg-scene-payload v1)) scene))
    (should (equal (excal--decode-payload (excal--svg-scene-payload v2)) scene))
    (should-not (excal--svg-scene-payload "<svg/>"))))

(ert-deftest excal-export-test-render-content ()
  "Exported pixels show the scene over the background, padded."
  (let ((rect (excal-export-test--rect 0 0 40 40 (cons 'roughness 0)
                                       (cons 'strokeColor "transparent")
                                       (cons 'backgroundColor "#ff0000")
                                       (cons 'fillStyle "solid"))))
    (excal-export-test--with-scene (list rect)
      (setf (alist-get 'viewBackgroundColor (alist-get 'appState excal--doc)) "#0000ff")
      (excal-export-test--with-file file ".png"
        (excal-export-png file)
        ;; Decode the export as an image and look at it.
        (let ((info (excal-native-image-register
                     "t-export" (excal--image-data-url "image/png"
                                                       (excal--read-image-file file)))))
          (unwind-protect
              (progn
                (should (equal info [60.0 60.0 "image/png"]))
                (setq excal--doc (list (cons 'files nil)))
                (setq excal--elements
                      (list (excal--make-element "image" 0 0 (cons 'width 60.0)
                                                 (cons 'height 60.0) (cons 'fileId "t-export")
                                                 (cons 'scale [1 1]) (cons 'crop :null))))
                (let ((fb (excal-native-fb-create 60 60)))
                  (clrhash excal--native-cache)
                  (excal-native-fb-render fb 1.0 1.0 0.0 0.0 (excal--visible-elements) nil)
                  (should (= (excal-native-fb-pixel fb 5 5) #xff0000ff))
                  (should (= (excal-native-fb-pixel fb 30 30) #xffff0000))))
            (excal-native-image-forget "t-export")))))))

(ert-deftest excal-export-test-empty-scene ()
  "Exporting nothing is an error."
  (excal-export-test--with-scene nil
    (should-error (excal-export-png "/nonexistent/x.png") :type 'user-error)))

(ert-deftest excal-export-test-open-files ()
  "PNG and SVG scenes are not saved over; .excalidraw files are."
  (should (excal--scene-file-p "a.excalidraw"))
  (should-not (excal--scene-file-p "a.excalidraw.png"))
  (should-not (excal--scene-file-p "a.SVG")))

;;; excal-export-test.el ends here
