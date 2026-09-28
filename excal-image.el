;;; excal-image.el --- Image elements, the files map and image insertion  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Image elements show `files[fileId].dataURL' of the document.  The
;; module decodes each file once into a session-wide cache keyed by file
;; id (`excal-native-image-register'); this file tracks which excal
;; buffers use which cached image and frees an image
;; (`excal-native-image-forget') once no live buffer uses it.
;;
;; `excal-insert-image' inserts an image file like upstream's image
;; tool: the file id is the SHA-1 of the file, large raster images are
;; scaled down to `excal-image-max-size', and the new element is sized
;; by upstream `initializeImageDimensions' at the view centre.

;;; Code:

(require 'excal-core)
(require 'excal-frame-render)
(require 'url-util)

(declare-function excal-native-image-register "excal-module")
(declare-function excal--stroke-width-for "excal-style")
(declare-function excal-native-image-forget "excal-module")
(declare-function excal-native-image-info "excal-module")
(declare-function excal-native-image-png "excal-module")
(declare-function excal--insert-elements "excal-clipboard")
(declare-function excal--view-center "excal-select")
(declare-function excal--style-value "excal-style")
(declare-function excal--render "excal-view")

(defcustom excal-image-max-size 1440
  "Largest width or height in pixels of an inserted raster image.
Larger images are scaled down (upstream DEFAULT_IMAGE_OPTIONS
maxWidthOrHeight).  nil keeps images as they are."
  :type '(choice (const :tag "Never scale" nil) integer)
  :group 'excal)

(defcustom excal-image-max-file-size (* 4 1024 1024)
  "Largest image file, in bytes, `excal-insert-image' accepts."
  :type 'integer
  :group 'excal)

;; IMAGE_MIME_TYPES (packages/common/src/constants.ts).
(defconst excal-image-mime-types
  '(("svg" . "image/svg+xml") ("png" . "image/png") ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg") ("jfif" . "image/jfif") ("gif" . "image/gif")
    ("webp" . "image/webp") ("bmp" . "image/bmp") ("ico" . "image/x-icon")
    ("avif" . "image/avif"))
  "Image file extensions and their MIME types.")

;;;; Data URLs

(defun excal--image-data-url (mime bytes)
  "Return the base64 data URL of unibyte BYTES with MIME type."
  (concat "data:" mime ";base64," (base64-encode-string bytes t)))

(defun excal--image-data-url-bytes (url)
  "Return (MIME . BYTES) decoded from data URL URL, or nil.
Base64 and percent-encoded payloads are accepted."
  (when (and (stringp url)
             (string-match "\\`data:\\([^,;]*\\)\\(\\(?:;[^,]*\\)?\\)," url))
    (let ((mime (downcase (match-string 1 url)))
          (base64 (string-match-p ";base64\\(;\\|\\'\\)" (match-string 2 url)))
          (data (substring url (match-end 0))))
      (cons mime
            (if base64
                (ignore-errors
                  (base64-decode-string (replace-regexp-in-string "[ \t\n\r]" "" data)))
              (encode-coding-string (url-unhex-string data) 'utf-8-unix))))))

(defun excal--image-sniff-mime (bytes)
  "Guess the image MIME type of unibyte BYTES from their signature, or nil."
  (let ((head (substring bytes 0 (min 16 (length bytes)))))
    (cond ((string-prefix-p "\x89PNG\r\n\x1a\n" head) "image/png")
          ((string-prefix-p "\xff\xd8\xff" head) "image/jpeg")
          ((string-prefix-p "GIF8" head) "image/gif")
          ((and (string-prefix-p "RIFF" head) (>= (length head) 12)
                (equal (substring head 8 12) "WEBP"))
           "image/webp")
          ((string-prefix-p "BM" head) "image/bmp")
          ((string-prefix-p "\0\0\1\0" head) "image/x-icon")
          ((string-match-p "\\`\\(?:\xef\xbb\xbf\\)?[ \t\r\n]*<" head)
           "image/svg+xml"))))

(defun excal--image-file-mime (file bytes)
  "Return the MIME type of image FILE with contents BYTES, or nil."
  (or (cdr (assoc (downcase (or (file-name-extension file) ""))
                  excal-image-mime-types))
      (excal--image-sniff-mime bytes)))

;;;; The module's image cache

(defvar excal--image-users (make-hash-table :test #'equal)
  "Map file ids cached in the module to the buffers using them.")

(defvar-local excal--image-failed nil
  "Hash table of file ids whose data could not be decoded, or nil.")

(defun excal--doc-files ()
  "Return the files map of the current document as an alist."
  (let ((files (alist-get 'files excal--doc)))
    (and (listp files) files)))

(defun excal--image-file-entry (file-id)
  "Return the files entry of FILE-ID in the current document, or nil."
  (and (stringp file-id)
       (alist-get (intern file-id) (excal--doc-files))))

(defun excal--image-release-buffer ()
  "Stop the current buffer using cached images; free unused ones."
  (let ((buffer (current-buffer)) unused)
    (maphash (lambda (id users)
               (when (memq buffer users)
                 (let ((rest (delq buffer (copy-sequence users))))
                   (if rest
                       (puthash id rest excal--image-users)
                     (push id unused)))))
             excal--image-users)
    (dolist (id unused)
      (remhash id excal--image-users)
      (excal-native-image-forget id))))

(defun excal--image-gc ()
  "Free cached images whose buffers are all dead.
Buffers killed without running `kill-buffer-hook' (such as
`with-temp-buffer' ones) leave their images behind until this runs."
  (let (unused)
    (maphash (lambda (id users)
               (let ((live (seq-filter #'buffer-live-p users)))
                 (cond ((null live) (push id unused))
                       ((not (equal live users)) (puthash id live excal--image-users)))))
             excal--image-users)
    (dolist (id unused)
      (remhash id excal--image-users)
      (excal-native-image-forget id))))

(defun excal--image-use (file-id)
  "Record that the current buffer uses the cached image FILE-ID."
  (let ((users (gethash file-id excal--image-users)))
    (unless (memq (current-buffer) users)
      (puthash file-id (cons (current-buffer) users) excal--image-users)
      (add-hook 'kill-buffer-hook #'excal--image-release-buffer nil t))))

(defun excal--image-register (file-id data-url)
  "Decode DATA-URL into the module's cache as FILE-ID for this buffer.
Return the image's [WIDTH HEIGHT MIME], or nil if it cannot be decoded."
  (excal--image-gc)
  (when-let* ((info (excal-native-image-register file-id data-url)))
    (when excal--image-failed (remhash file-id excal--image-failed))
    (excal--image-use file-id)
    info))

(defun excal--image-ensure (file-id)
  "Make sure the image of FILE-ID is decoded; return its info or nil.
The data comes from the document's files map.  A file is decoded once
per session: buffers showing the same file share it, and a file that
failed to decode is not tried again."
  (when (stringp file-id)
    (or (and (gethash file-id excal--image-users)
             (progn (excal--image-use file-id)
                    (excal-native-image-info file-id)))
        (and (not (and excal--image-failed (gethash file-id excal--image-failed)))
             (when-let* ((url (alist-get 'dataURL (excal--image-file-entry file-id))))
               (or (excal--image-register file-id url)
                   (progn
                     (unless excal--image-failed
                       (setq excal--image-failed (make-hash-table :test #'equal)))
                     (puthash file-id t excal--image-failed)
                     nil)))))))

(defun excal--image-add-files (files)
  "Add FILES, a files-map alist, to the document and decode them.
Entries already in the document are kept.  For pasting images."
  (let ((existing (excal--doc-files)) added)
    (dolist (entry files)
      (unless (assq (car entry) existing)
        (push entry added)))
    (when added
      (setf (alist-get 'files excal--doc) (append existing (nreverse added))))
    (dolist (entry files)
      (excal--image-ensure (symbol-name (car entry))))))

;;;; Render data

(defun excal--corner-radius (x element)
  "Return upstream `getCornerRadius' of X for ELEMENT's roundness."
  (let* ((roundness (excal--get element 'roundness))
         (type (and (consp roundness) (alist-get 'type roundness)))
         (value (and (consp roundness) (alist-get 'value roundness))))
    (pcase type
      ((or 1 2) (* x 0.25))
      (3 (let ((fixed (if (numberp value) value 32)))
           (if (<= x (/ fixed 0.25)) (* x 0.25) fixed)))
      (_ 0))))

(defun excal--image-native-extras (element)
  "Return the image render data of ELEMENT as a list (KEY VALUE ...)."
  (when (equal (excal--get element 'type) "image")
    (let* ((file-id (excal--get element 'fileId))
           (info (excal--image-ensure file-id))
           (scale (excal--get element 'scale))
           (crop (excal--get element 'crop))
           (w (abs (or (excal--get element 'width) 0)))
           (h (abs (or (excal--get element 'height) 0)))
           out)
      (when (stringp file-id)
        (setq out (list "file-id" file-id)))
      (when (or (equal (excal--get element 'status) "error")
                (and (stringp file-id) (not info)
                     excal--image-failed (gethash file-id excal--image-failed)))
        (setq out (append out (list "error" t))))
      (when (and (vectorp scale) (= (length scale) 2))
        (setq out (append out (list "scale" (vector (float (aref scale 0))
                                                    (float (aref scale 1)))))))
      (when (consp crop)
        (setq out (append out
                          (list "crop"
                                (vconcat (mapcar (lambda (k)
                                                   (float (or (alist-get k crop) 0)))
                                                 '(x y width height
                                                     naturalWidth naturalHeight)))))))
      (when (excal--get element 'roundness)
        (setq out (append out (list "radius"
                                    (float (excal--corner-radius (min w h) element))))))
      out)))

(defun excal--native-media-extras (element)
  "Return image and frame render data of ELEMENT for the module.
A vector [KEY VALUE ...] read by `read_media_extras' in excal-module.c;
see `excal--image-native-extras' and `excal--frame-native-extras'."
  (vconcat (excal--frame-native-extras element)
           (excal--image-native-extras element)))

;;;; Inserting images

(defun excal--image-initial-size (natural-width natural-height)
  "Return (WIDTH . HEIGHT) of a new image of the given natural size.
Port of upstream `initializeImageDimensions': at most half the canvas
height (in scene units), and at most the canvas height minus 120px."
  (let* ((canvas-height (if excal--canvas-size
                            (/ (cdr excal--canvas-size) excal--pixel-scale)
                          600.0))
         (min-height (max (- canvas-height 120) 160))
         (max-height (min min-height (/ (ffloor (* canvas-height 0.5)) excal--zoom)))
         (height (min natural-height max-height)))
    (cons (* height (/ (float natural-width) natural-height)) (float height))))

(defun excal--read-image-file (file)
  "Return FILE's contents as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun excal--make-image-element (file-id natural-width natural-height center)
  "Return a new image element for FILE-ID centred on CENTER (X . Y).
It is sized by `excal--image-initial-size' and styled like upstream
`newImagePlaceholder' (current stroke, background, fill, stroke width,
stroke style, roughness and opacity; sharp corners)."
  (pcase-let* ((`(,w . ,h) (excal--image-initial-size natural-width natural-height))
               (element (excal--make-element
                         "image" (- (car center) (/ w 2)) (- (cdr center) (/ h 2))
                         (cons 'width w) (cons 'height h))))
    (when (fboundp 'excal--style-value)
      (dolist (property '(strokeColor backgroundColor fillStyle strokeWidth
                                      strokeStyle roughness opacity))
        (let ((value (excal--style-value property)))
          (when value
            (excal--put element property
                        (if (and (eq property 'strokeWidth) (not (numberp value)))
                            (excal--stroke-width-for "image" value)
                          value))))))
    (dolist (field `((roundness . :null) (status . "saved") (fileId . ,file-id)
                     (scale . [1 1]) (crop . :null)))
      (excal--put element (car field) (cdr field)))
    element))

(defun excal-insert-image (file)
  "Insert image FILE at the centre of the view and select it.
The file is stored in the scene's files map as a data URL, as upstream
does; its id is the file's SHA-1."
  (interactive "fInsert image: ")
  (require 'excal-clipboard)
  (let* ((file (expand-file-name file))
         (bytes (excal--read-image-file file))
         (mime (excal--image-file-mime file bytes)))
    (unless (member mime (mapcar #'cdr excal-image-mime-types))
      (user-error "Unsupported image type: %s" file))
    (let* ((file-id (secure-hash 'sha1 bytes))
           (url (excal--image-data-url mime bytes))
           (info (excal-native-image-register file-id url)))
      (unless info
        (user-error "Cannot decode image %s" file))
      ;; Downscale big raster images (upstream `resizeImageFile'); this
      ;; re-encodes them as PNG.
      (when (and excal-image-max-size
                 (not (equal mime "image/svg+xml"))
                 (> (max (aref info 0) (aref info 1)) excal-image-max-size))
        (when-let* ((png (excal-native-image-png file-id excal-image-max-size)))
          (setq mime "image/png"
                url (excal--image-data-url mime png)
                info (or (excal-native-image-register file-id url) info)
                bytes png)))
      (when (> (length bytes) excal-image-max-file-size)
        (unless (gethash file-id excal--image-users)
          (excal-native-image-forget file-id))
        (user-error "File is too big.  Maximum allowed size is %dMB"
                    (/ excal-image-max-file-size 1024 1024)))
      (excal--image-use file-id)
      (let ((now (truncate (* 1000 (float-time)))))
        (unless (excal--image-file-entry file-id)
          (setf (alist-get 'files excal--doc)
                (append (excal--doc-files)
                        (list (cons (intern file-id)
                                    (list (cons 'mimeType mime)
                                          (cons 'id file-id)
                                          (cons 'dataURL url)
                                          (cons 'created now)
                                          (cons 'lastRetrieved now))))))))
      (let ((element (excal--make-image-element
                      file-id (aref info 0) (aref info 1) (excal--view-center))))
        (excal--insert-elements (list element))
        (when (fboundp 'excal--render) (excal--render))
        element))))

(provide 'excal-image)
;;; excal-image.el ends here
