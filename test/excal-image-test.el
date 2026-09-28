;;; excal-image-test.el --- Tests for images and frame rendering  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excal)

;;;; Helpers

(defun excal-image-test--rect (x y w h color)
  "Return a solid, stroke-less, smooth rectangle element."
  (excal--make-element "rectangle" x y (cons 'width (float w)) (cons 'height (float h))
                       (cons 'strokeColor "transparent") (cons 'backgroundColor color)
                       (cons 'fillStyle "solid") (cons 'roughness 0)))

(defun excal-image-test--png (colors &optional height)
  "Return PNG bytes of a strip of 1-pixel-wide COLORS, HEIGHT rows high.
The image is drawn and encoded by the module."
  (let ((height (or height 1))
        (file (make-temp-file "excal-image" nil ".png")))
    (unwind-protect
        (with-temp-buffer
          (setq excal--native-cache (make-hash-table :test #'eq))
          (let ((fb (excal-native-fb-create (length colors) height))
                (x -1))
            (setq excal--elements
                  (mapcar (lambda (c) (excal-image-test--rect (cl-incf x) 0 1 height c))
                          colors))
            (excal-native-fb-render fb 1.0 1.0 0.0 0.0 (excal--visible-elements) nil)
            (excal-native-fb-write-png fb file)
            (excal--read-image-file file)))
      (delete-file file))))

(defun excal-image-test--channels (pixel)
  "Return (R G B) of ARGB integer PIXEL."
  (list (logand (ash pixel -16) 255) (logand (ash pixel -8) 255) (logand pixel 255)))

(defun excal-image-test--dominant (fb x y)
  "Return the strongest channel at X, Y of FB: `red', `green', `blue',
`white', `black' or `grey'."
  (pcase-let ((`(,r ,g ,b) (excal-image-test--channels (excal-native-fb-pixel fb x y))))
    (cond ((and (> r 200) (> g 200) (> b 200)) 'white)
          ((and (< r 60) (< g 60) (< b 60)) 'black)
          ((and (> r (+ g 60)) (> r (+ b 60))) 'red)
          ((and (> g (+ r 60)) (> g (+ b 60))) 'green)
          ((and (> b (+ r 60)) (> b (+ g 60))) 'blue)
          (t 'grey))))

(defmacro excal-image-test--with-scene (&rest body)
  "Run BODY in a temporary scene buffer with an empty document."
  (declare (indent 0))
  `(with-temp-buffer
     (setq excal--native-cache (make-hash-table :test #'eq)
           excal--doc (excal--empty-doc)
           excal--zoom 1.0 excal--pixel-scale 1.0
           excal--scroll-x 0.0 excal--scroll-y 0.0
           excal--elements nil)
     ,@body))

(defun excal-image-test--add-file (id bytes mime)
  "Add BYTES as file ID with MIME to the current document."
  (setf (alist-get 'files excal--doc)
        (append (excal--doc-files)
                (list (cons (intern id)
                            (list (cons 'mimeType mime) (cons 'id id)
                                  (cons 'dataURL (excal--image-data-url mime bytes))
                                  (cons 'created 0)))))))

(defun excal-image-test--image (id x y w h &rest props)
  "Return an image element showing file ID."
  (apply #'excal--make-element "image" x y (cons 'width (float w)) (cons 'height (float h))
         (cons 'fileId id) (cons 'status "saved") (cons 'scale [1 1]) (cons 'crop :null)
         props))

(defun excal-image-test--render (width height)
  "Render the scene into a new WIDTH by HEIGHT framebuffer and return it."
  (let ((fb (excal-native-fb-create width height)))
    (clrhash excal--native-cache)
    (excal-native-fb-render fb 1.0 excal--zoom excal--scroll-x excal--scroll-y
                            (excal--visible-elements) nil)
    fb))

(defconst excal-image-test--strip '("#ff0000" "#00ff00" "#0000ff" "#000000")
  "Colors of the synthetic test image, left to right.")

(defun excal-image-test--row (fb y)
  "Return the dominant colors at the centres of four 10px columns of FB."
  (mapcar (lambda (x) (excal-image-test--dominant fb x y)) '(5 15 25 35)))

;;;; Data URLs

(ert-deftest excal-image-test-data-url-roundtrip ()
  "Data URLs are base64 with the MIME type, and decode back."
  (let* ((bytes (apply #'unibyte-string (number-sequence 0 255)))
         (url (excal--image-data-url "image/png" bytes)))
    (should (string-prefix-p "data:image/png;base64," url))
    (should-not (string-search "\n" url))
    (should (equal (excal--image-data-url-bytes url) (cons "image/png" bytes)))
    (should (equal (excal--image-data-url-bytes "data:image/svg+xml,%3Csvg%2F%3E")
                   (cons "image/svg+xml" "<svg/>")))
    (should-not (excal--image-data-url-bytes "http://example.com/a.png"))))

(ert-deftest excal-image-test-module-decodes-data-urls ()
  "The module decodes base64 (with whitespace) and percent-encoded URLs."
  (let* ((png (excal-image-test--png excal-image-test--strip))
         (url (excal--image-data-url "image/png" png))
         ;; Line breaks inside the base64 payload are ignored.
         (wrapped (concat (substring url 0 40) "\n  " (substring url 40))))
    (unwind-protect
        (progn
          (should (equal (excal-native-image-register "t-url-1" url) [4.0 1.0 "image/png"]))
          (should (equal (excal-native-image-register "t-url-2" wrapped) [4.0 1.0 "image/png"]))
          ;; The MIME type is lower-cased; PNG is recognised by signature.
          (should (equal (aref (excal-native-image-register
                                "t-url-3" (concat "data:IMAGE/PNG;base64,"
                                                  (base64-encode-string png t)))
                               2)
                         "image/png"))
          (should-not (excal-native-image-register "t-url-4" "data:image/png;base64,AAAA"))
          (should-not (excal-native-image-register "t-url-5" "not a data url"))
          (should-not (excal-native-image-info "t-url-4")))
      (dolist (id '("t-url-1" "t-url-2" "t-url-3"))
        (excal-native-image-forget id)))))

(ert-deftest excal-image-test-mime-sniffing ()
  "File types come from the extension, else from the signature."
  (should (equal (excal--image-file-mime "a.JPG" "") "image/jpeg"))
  (should (equal (excal--image-file-mime "a" "\x89PNG\r\n\x1a\nxxxx") "image/png"))
  (should (equal (excal--image-file-mime "a" "GIF89a") "image/gif"))
  (should (equal (excal--image-file-mime "a" "RIFF\0\0\0\0WEBPVP8 ") "image/webp"))
  (should (equal (excal--image-file-mime "a" "  <svg") "image/svg+xml"))
  (should-not (excal--image-file-mime "a.txt" "hello")))

(ert-deftest excal-image-test-svg-decoding ()
  "SVG files decode through librsvg when the module has it."
  (let ((svg "<svg xmlns='http://www.w3.org/2000/svg' width='40' height='10'><rect width='20' height='10' fill='#f00'/><rect x='20' width='20' height='10' fill='#00f'/></svg>"))
    (skip-unless (excal-native-image-register "t-svg-probe" (excal--image-data-url "image/svg+xml" svg)))
    (excal-native-image-forget "t-svg-probe")
    (excal-image-test--with-scene
      (excal-image-test--add-file "t-svg" (encode-coding-string svg 'utf-8) "image/svg+xml")
      (setq excal--elements (list (excal-image-test--image "t-svg" 0 0 40 10)))
      (let ((fb (excal-image-test--render 40 10)))
        (should (equal (excal-image-test--row fb 5) '(red red blue blue))))
      (should (equal (excal-native-image-info "t-svg") [40.0 10.0 "image/svg+xml"])))))

;;;; Cache lifecycle

(ert-deftest excal-image-test-cache-lifecycle ()
  "A file is decoded once, shared by buffers and freed with the last one."
  (let* ((png (excal-image-test--png '("#ff0000")))
         (id "t-life")
         (a (generate-new-buffer " excal-a"))
         (b (generate-new-buffer " excal-b"))
         (setup (lambda ()
                  (setq excal--doc (excal--empty-doc)
                        excal--native-cache (make-hash-table :test #'eq))
                  (excal-image-test--add-file id png "image/png"))))
    (unwind-protect
        (let ((before (excal-native-image-count)))
          (with-current-buffer a
            (funcall setup)
            (should (excal--image-ensure id))
            (should (= (excal-native-image-count) (1+ before)))
            ;; Asking again does not decode again.
            (should (excal--image-ensure id))
            (should (= (excal-native-image-count) (1+ before))))
          (with-current-buffer b
            (funcall setup)
            (should (excal--image-ensure id))
            (should (= (excal-native-image-count) (1+ before))))
          (should (equal (gethash id excal--image-users) (list b a)))
          (kill-buffer a)
          (should (excal-native-image-info id))
          (kill-buffer b)
          (should-not (excal-native-image-info id))
          (should-not (gethash id excal--image-users))
          (should (= (excal-native-image-count) before)))
      (when (buffer-live-p a) (kill-buffer a))
      (when (buffer-live-p b) (kill-buffer b)))))

(ert-deftest excal-image-test-cache-gc-dead-buffers ()
  "Images of buffers killed without their hooks are freed later."
  (let ((png (excal-image-test--png '("#00ff00"))))
    (let ((kill-buffer-hook nil))
      (excal-image-test--with-scene
        (excal-image-test--add-file "t-gc" png "image/png")
        (should (excal--image-ensure "t-gc"))))
    (excal--image-gc)
    (should-not (excal-native-image-info "t-gc"))))

(ert-deftest excal-image-test-failed-file-not-retried ()
  "A file that cannot be decoded is marked and drawn as an error."
  (excal-image-test--with-scene
    (excal-image-test--add-file "t-bad" "not an image at all" "image/png")
    (should-not (excal--image-ensure "t-bad"))
    (should (gethash "t-bad" excal--image-failed))
    (let ((extras (excal--native-media-extras (excal-image-test--image "t-bad" 0 0 10 10))))
      (should (member "error" (append extras nil))))))

;;;; Drawing

(ert-deftest excal-image-test-draw-flip-crop ()
  "Images are drawn stretched to the element box, flipped and cropped."
  (excal-image-test--with-scene
    (excal-image-test--add-file "t-strip" (excal-image-test--png excal-image-test--strip)
                                "image/png")
    (let ((img (excal-image-test--image "t-strip" 0 0 40 10)))
      (setq excal--elements (list img))
      (should (equal (excal-image-test--row (excal-image-test--render 40 10) 5)
                     '(red green blue black)))
      ;; scale [-1 1] mirrors horizontally about the centre.
      (excal--put img 'scale [-1 1])
      (should (equal (excal-image-test--row (excal-image-test--render 40 10) 5)
                     '(black blue green red)))
      ;; A crop picks the source rectangle in natural pixels.
      (excal--put img 'scale [1 1])
      (excal--put img 'crop '((x . 2) (y . 0) (width . 2) (height . 1)
                              (naturalWidth . 4) (naturalHeight . 1)))
      (let ((fb (excal-image-test--render 40 10)))
        (should (eq (excal-image-test--dominant fb 3 5) 'blue))
        (should (eq (excal-image-test--dominant fb 36 5) 'black)))
      ;; Rotating by pi turns it around its centre.
      (excal--put img 'crop :null)
      (excal--put img 'angle float-pi)
      (should (equal (excal-image-test--row (excal-image-test--render 40 10) 5)
                     '(black blue green red))))))

(ert-deftest excal-image-test-draw-rounded-and-opacity ()
  "Rounded images are clipped at the corners; opacity blends them."
  (excal-image-test--with-scene
    (excal-image-test--add-file "t-red" (excal-image-test--png '("#ff0000") 1) "image/png")
    (let ((img (excal-image-test--image "t-red" 0 0 40 40)))
      (setq excal--elements (list img))
      (should (eq (excal-image-test--dominant (excal-image-test--render 40 40) 0 0) 'red))
      ;; Adaptive radius: min(40,40)*0.25 = 10 at each corner.
      (excal--put img 'roundness '((type . 3)))
      (let ((fb (excal-image-test--render 40 40)))
        (should (eq (excal-image-test--dominant fb 0 0) 'white))
        (should (eq (excal-image-test--dominant fb 20 20) 'red)))
      (excal--put img 'roundness :null)
      (excal--put img 'opacity 50)
      (pcase-let ((`(,r ,g ,_) (excal-image-test--channels
                                (excal-native-fb-pixel (excal-image-test--render 40 40) 20 20))))
        (should (> r 240))
        (should (< 110 g 145))))))

(ert-deftest excal-image-test-corner-radius ()
  "getCornerRadius: proportional below the cutoff, fixed above."
  (let ((e (excal--make-element "image" 0 0 (cons 'roundness '((type . 3))))))
    (should (= (excal--corner-radius 40 e) 10))
    (should (= (excal--corner-radius 400 e) 32))
    (excal--put e 'roundness '((type . 3) (value . 8)))
    (should (= (excal--corner-radius 400 e) 8))
    (excal--put e 'roundness '((type . 2)))
    (should (= (excal--corner-radius 400 e) 100))
    (excal--put e 'roundness :null)
    (should (= (excal--corner-radius 400 e) 0))))

(ert-deftest excal-image-test-placeholder ()
  "Missing files show the grey placeholder; errors add the ban icon."
  (excal-image-test--with-scene
    (let ((img (excal-image-test--image "t-missing" 0 0 200 200 (cons 'status "pending"))))
      (setq excal--elements (list img))
      (let ((pending (excal-image-test--render 200 200)))
        ;; #E7E7E7 background, #888 icon of size min(200*0.4, 100) = 80.
        (should (= (excal-native-fb-pixel pending 2 2) #xffe7e7e7))
        (should (= (excal-native-fb-pixel pending 100 73) #xff888888))
        (excal--put img 'status "error")
        (let ((error (excal-image-test--render 200 200)))
          (should (= (excal-native-fb-pixel error 2 2) #xffe7e7e7))
          (should (> (excal-native-fb-mean-diff pending error) 0.5)))))))

(ert-deftest excal-image-test-insert-image ()
  "Inserting an image adds a files entry and a selected, sized element."
  (let ((png (excal-image-test--png (make-list 400 "#0000ff") 100))
        (file (make-temp-file "excal-insert" nil ".png")))
    (unwind-protect
        (excal-image-test--with-scene
          (let ((coding-system-for-write 'binary))
            (write-region png nil file nil 'silent))
          (let* ((element (excal-insert-image file))
                 (id (secure-hash 'sha1 png))
                 (entry (excal--image-file-entry id)))
            (should (member element excal--elements))
            (should (equal excal--selection (list element)))
            (should (equal (excal--get element 'fileId) id))
            (should (equal (excal--get element 'type) "image"))
            (should (equal (excal--get element 'scale) [1 1]))
            (should (equal (alist-get 'mimeType entry) "image/png"))
            (should (equal (alist-get 'id entry) id))
            (should (equal (cdr (excal--image-data-url-bytes (alist-get 'dataURL entry))) png))
            ;; Natural size fits: 400x100 centred on the view centre.
            (should (= (excal--get element 'width) 400))
            (should (= (excal--get element 'height) 100))
            (let ((center (excal--view-center)))
              (should (= (+ (excal--get element 'x) 200) (car center)))
              (should (= (+ (excal--get element 'y) 50) (cdr center))))
            (should (stringp (excal--get element 'index)))
            ;; The saved scene keeps the file.
            (should (excal--used-files excal--elements (excal--doc-files)))))
      (delete-file file))))

(ert-deftest excal-image-test-insert-downscales ()
  "Big raster images are scaled to `excal-image-max-size', keeping the id."
  (let ((png (excal-image-test--png (make-list 400 "#0000ff") 100))
        (file (make-temp-file "excal-insert" nil ".png"))
        (excal-image-max-size 100))
    (unwind-protect
        (excal-image-test--with-scene
          (let ((coding-system-for-write 'binary))
            (write-region png nil file nil 'silent))
          (let* ((element (excal-insert-image file))
                 (id (secure-hash 'sha1 png)))
            (should (equal (excal--get element 'fileId) id))
            (should (equal (excal-native-image-info id) [100.0 25.0 "image/png"]))
            (should (= (excal--get element 'width) 100))
            (should (= (excal--get element 'height) 25))))
      (delete-file file))))

(ert-deftest excal-image-test-initial-size ()
  "initializeImageDimensions: at most half the canvas height, in scene units."
  (excal-image-test--with-scene
    (setq excal--canvas-size '(800 . 600))
    (should (equal (excal--image-initial-size 2000 1000) '(600.0 . 300.0)))
    (setq excal--zoom 2.0)
    (should (equal (excal--image-initial-size 2000 1000) '(300.0 . 150.0)))
    (should (equal (excal--image-initial-size 100 50) '(100.0 . 50.0)))))

;;;; Frames

(defun excal-image-test--frame (x y w h &rest props)
  "Return a frame element."
  (apply #'excal--make-element "frame" x y (cons 'width (float w)) (cons 'height (float h))
         (cons 'name :null) (cons 'roughness 0) (cons 'strokeColor "#bbb") props))

(ert-deftest excal-image-test-frame-title ()
  "Frames without a name are called Frame, magic frames AI Frame."
  (should (equal (excal--frame-title (excal-image-test--frame 0 0 10 10)) "Frame"))
  (should (equal (excal--frame-title (excal-image-test--frame 0 0 10 10 (cons 'name "Hi")))
                 "Hi"))
  (should (equal (excal--frame-title (excal--make-element "magicframe" 0 0 (cons 'name :null)))
                 "AI Frame")))

(ert-deftest excal-image-test-frame-clip ()
  "Children are clipped to their frame; pixels outside stay white."
  (excal-image-test--with-scene
    (let* ((frame (excal-image-test--frame 20 20 100 60))
           (id (excal--get frame 'id))
           (child (excal-image-test--rect 80 40 100 20 "#ff0000"))
           (far (excal-image-test--rect 150 90 30 20 "#00ff00"))
           (grouped (excal-image-test--rect 150 120 30 20 "#0000ff")))
      (excal--put child 'frameId id)
      ;; Wholly outside and ungrouped: not clipped.
      (excal--put far 'frameId id)
      ;; Wholly outside but grouped: clipped with its group.
      (excal--put grouped 'frameId id)
      (excal--put grouped 'groupIds ["g1"])
      (setq excal--elements (list child far grouped frame))
      (let ((fb (excal-image-test--render 200 160)))
        (should (eq (excal-image-test--dominant fb 100 50) 'red))
        (should (eq (excal-image-test--dominant fb 150 50) 'white))
        (should (eq (excal-image-test--dominant fb 170 50) 'white))
        (should (eq (excal-image-test--dominant fb 165 100) 'green))
        (should (eq (excal-image-test--dominant fb 165 130) 'white)))
      ;; Without the frame the child is whole.
      (excal--put frame 'isDeleted t)
      (let ((fb (excal-image-test--render 200 160)))
        (should (eq (excal-image-test--dominant fb 150 50) 'red))
        (should (eq (excal-image-test--dominant fb 165 130) 'blue))))))

(ert-deftest excal-image-test-frame-opacity ()
  "A child's opacity is multiplied by its frame's."
  (excal-image-test--with-scene
    (let* ((frame (excal-image-test--frame 0 0 100 100 (cons 'opacity 50)))
           (child (excal-image-test--rect 20 20 40 40 "#000000")))
      (excal--put child 'frameId (excal--get frame 'id))
      (setq excal--elements (list child frame))
      (pcase-let ((`(,r ,_ ,_) (excal-image-test--channels
                                (excal-native-fb-pixel (excal-image-test--render 100 100) 40 40))))
        (should (< 110 r 145))))))

(ert-deftest excal-image-test-frame-outline-and-label ()
  "Frames draw a grey outline and their name above, at screen size."
  (excal-image-test--with-scene
    (let ((frame (excal-image-test--frame 20 40 150 60)))
      (setq excal--elements (list frame))
      (let ((fb (excal-image-test--render 200 120))
            (bounds (excal--frame-name-bounds frame)))
        ;; Outline #bbb, 2px on the left edge; inside stays white.
        (should (eq (excal-image-test--dominant fb 20 70) 'grey))
        (should (eq (excal-image-test--dominant fb 60 70) 'white))
        ;; The label: bottom 3px above the frame, 17.5px tall.
        (should (equal (nth 0 bounds) 20.0))
        (should (= (nth 3 bounds) 37.0))
        (should (= (nth 1 bounds) 19.5))
        (should (< 20 (nth 2 bounds) 170))
        ;; Some label ink lies inside the bounds, none below the frame top.
        (let ((ink 0))
          (dotimes (x (round (- (nth 2 bounds) 20)))
            (dotimes (y 17)
              (unless (eq (excal-image-test--dominant fb (+ 20 x) (+ 20 y)) 'white)
                (cl-incf ink))))
          (should (> ink 10)))
        (should (eq (excal-image-test--dominant fb 150 25) 'white))))))

(ert-deftest excal-image-test-frame-label-zoom-and-truncation ()
  "Label bounds scale with 1/zoom; long names are cut to the frame width."
  (excal-image-test--with-scene
    (let ((frame (excal-image-test--frame 0 100 400 100)))
      (let ((b1 (excal--frame-name-bounds frame 1.0))
            (b2 (excal--frame-name-bounds frame 2.0)))
        (should (= (- (nth 3 b1) (nth 1 b1)) 17.5))
        (should (= (- (nth 3 b2) (nth 1 b2)) 8.75))
        (should (= (nth 3 b2) 98.5))
        (should (< (abs (- (* 2 (- (nth 2 b2) (nth 0 b2))) (- (nth 2 b1) (nth 0 b1)))) 1e-6)))
      (excal--put frame 'name "A rather long frame name that will not fit")
      (excal--put frame 'width 60.0)
      (pcase-let ((`(,text . ,width) (excal--frame-label frame 1.0)))
        (should (string-suffix-p "…" text))
        (should (<= width 60))
        (should (<= (- (nth 2 (excal--frame-name-bounds frame 1.0)) 0) 60))))))

(ert-deftest excal-image-test-frame-native-extras ()
  "Frames pass their id and title; children their frame id."
  (let* ((frame (excal-image-test--frame 0 0 10 10 (cons 'name "F")))
         (child (excal--make-element "rectangle" 0 0 (cons 'frameId (excal--get frame 'id))
                                     (cons 'groupIds ["g"]))))
    (should (equal (excal--native-media-extras frame)
                   (vector "id" (excal--get frame 'id) "name" "F")))
    (should (equal (excal--native-media-extras child)
                   (vector "frame-id" (excal--get frame 'id) "grouped" t)))
    (should (equal (excal--native-media-extras (excal--make-element "rectangle" 0 0)) []))))

;;; excal-image-test.el ends here
