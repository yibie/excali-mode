;;; excali-image.el --- Image elements, the files map and image insertion  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Image elements show `files[fileId].dataURL' of the document.  The
;; module decodes each file once into a session-wide cache keyed by file
;; id (`excali-native-image-register'); this file tracks which excali
;; buffers use which cached image and frees an image
;; (`excali-native-image-forget') once no live buffer uses it.
;;
;; `excali-insert-image' inserts an image file like upstream's image
;; tool: the file id is the SHA-1 of the file, large raster images are
;; scaled down to `excali-image-max-size', and the new element is sized
;; by upstream `initializeImageDimensions' at the view centre.

;;; Code:

(require 'excali-core)
(require 'excali-frame-render)
(require 'url-util)

(declare-function excali-native-image-register "excali-module")
(declare-function excali--stroke-width-for "excali-style")
(declare-function excali-native-image-forget "excali-module")
(declare-function excali-native-image-info "excali-module")
(declare-function excali-native-image-png "excali-module")
(declare-function excali--insert-elements "excali-clipboard")
(declare-function excali--view-center "excali-select")
(declare-function excali--style-value "excali-style")
(declare-function excali--render "excali-view")

(defcustom excali-image-max-size 1440
  "Largest width or height in pixels of an inserted raster image.
Larger images are scaled down (upstream DEFAULT_IMAGE_OPTIONS
maxWidthOrHeight).  nil keeps images as they are."
  :type '(choice (const :tag "Never scale" nil) integer)
  :group 'excali)

(defcustom excali-image-max-file-size (* 4 1024 1024)
  "Largest image file, in bytes, `excali-insert-image' accepts."
  :type 'integer
  :group 'excali)

;; IMAGE_MIME_TYPES (packages/common/src/constants.ts).
(defconst excali-image-mime-types
  '(("svg" . "image/svg+xml") ("png" . "image/png") ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg") ("jfif" . "image/jfif") ("gif" . "image/gif")
    ("webp" . "image/webp") ("bmp" . "image/bmp") ("ico" . "image/x-icon")
    ("avif" . "image/avif"))
  "Image file extensions and their MIME types.")

;;;; Data URLs

(defun excali--image-data-url (mime bytes)
  "Return the base64 data URL of unibyte BYTES with MIME type."
  (concat "data:" mime ";base64," (base64-encode-string bytes t)))

(defun excali--image-data-url-bytes (url)
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

(defun excali--image-sniff-mime (bytes)
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

(defun excali--image-file-mime (file bytes)
  "Return the MIME type of image FILE with contents BYTES, or nil."
  (or (cdr (assoc (downcase (or (file-name-extension file) ""))
                  excali-image-mime-types))
      (excali--image-sniff-mime bytes)))

;;;; The module's image cache

(defvar excali--image-users (make-hash-table :test #'equal)
  "Map file ids cached in the module to the buffers using them.")

(defvar-local excali--image-failed nil
  "Hash table of file ids whose data could not be decoded, or nil.")

(defun excali--doc-files ()
  "Return the files map of the current document as an alist."
  (let ((files (alist-get 'files excali--doc)))
    (and (listp files) files)))

(defun excali--image-file-entry (file-id)
  "Return the files entry of FILE-ID in the current document, or nil."
  (and (stringp file-id)
       (alist-get (intern file-id) (excali--doc-files))))

(defun excali--image-release-buffer ()
  "Stop the current buffer using cached images; free unused ones."
  (let ((buffer (current-buffer)) unused)
    (maphash (lambda (id users)
               (when (memq buffer users)
                 (let ((rest (delq buffer (copy-sequence users))))
                   (if rest
                       (puthash id rest excali--image-users)
                     (push id unused)))))
             excali--image-users)
    (dolist (id unused)
      (remhash id excali--image-users)
      (excali-native-image-forget id))))

(defun excali--image-gc ()
  "Free cached images whose buffers are all dead.
Buffers killed without running `kill-buffer-hook' (such as
`with-temp-buffer' ones) leave their images behind until this runs."
  (let (unused)
    (maphash (lambda (id users)
               (let ((live (seq-filter #'buffer-live-p users)))
                 (cond ((null live) (push id unused))
                       ((not (equal live users)) (puthash id live excali--image-users)))))
             excali--image-users)
    (dolist (id unused)
      (remhash id excali--image-users)
      (excali-native-image-forget id))))

(defun excali--image-use (file-id)
  "Record that the current buffer uses the cached image FILE-ID."
  (let ((users (gethash file-id excali--image-users)))
    (unless (memq (current-buffer) users)
      (puthash file-id (cons (current-buffer) users) excali--image-users)
      (add-hook 'kill-buffer-hook #'excali--image-release-buffer nil t))))

(defun excali--image-register (file-id data-url)
  "Decode DATA-URL into the module's cache as FILE-ID for this buffer.
Return the image's [WIDTH HEIGHT MIME], or nil if it cannot be decoded."
  (excali--image-gc)
  (when-let* ((info (excali-native-image-register file-id data-url)))
    (when excali--image-failed (remhash file-id excali--image-failed))
    (excali--image-use file-id)
    info))

(defun excali--image-ensure (file-id)
  "Make sure the image of FILE-ID is decoded; return its info or nil.
The data comes from the document's files map.  A file is decoded once
per session: buffers showing the same file share it, and a file that
failed to decode is not tried again."
  (when (stringp file-id)
    (or (and (gethash file-id excali--image-users)
             (progn (excali--image-use file-id)
                    (excali-native-image-info file-id)))
        (and (not (and excali--image-failed (gethash file-id excali--image-failed)))
             (when-let* ((url (alist-get 'dataURL (excali--image-file-entry file-id))))
               (or (excali--image-register file-id url)
                   (progn
                     (unless excali--image-failed
                       (setq excali--image-failed (make-hash-table :test #'equal)))
                     (puthash file-id t excali--image-failed)
                     nil)))))))

(defun excali--image-add-files (files)
  "Add FILES, a files-map alist, to the document and decode them.
Entries already in the document are kept.  For pasting images."
  (let ((existing (excali--doc-files)) added)
    (dolist (entry files)
      (unless (assq (car entry) existing)
        (push entry added)))
    (when added
      (setf (alist-get 'files excali--doc) (append existing (nreverse added))))
    (dolist (entry files)
      (excali--image-ensure (symbol-name (car entry))))))

;;;; Render data

(defun excali--corner-radius (x element)
  "Return upstream `getCornerRadius' of X for ELEMENT's roundness."
  (let* ((roundness (excali--get element 'roundness))
         (type (and (consp roundness) (alist-get 'type roundness)))
         (value (and (consp roundness) (alist-get 'value roundness))))
    (pcase type
      ((or 1 2) (* x 0.25))
      (3 (let ((fixed (if (numberp value) value 32)))
           (if (<= x (/ fixed 0.25)) (* x 0.25) fixed)))
      (_ 0))))

(defun excali--image-native-extras (element)
  "Return the image render data of ELEMENT as a list (KEY VALUE ...)."
  (when (equal (excali--get element 'type) "image")
    (let* ((file-id (excali--get element 'fileId))
           (info (excali--image-ensure file-id))
           (scale (excali--get element 'scale))
           (crop (excali--get element 'crop))
           (w (abs (or (excali--get element 'width) 0)))
           (h (abs (or (excali--get element 'height) 0)))
           out)
      (when (stringp file-id)
        (setq out (list "file-id" file-id)))
      (when (or (equal (excali--get element 'status) "error")
                (and (stringp file-id) (not info)
                     excali--image-failed (gethash file-id excali--image-failed)))
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
      (when (excali--get element 'roundness)
        (setq out (append out (list "radius"
                                    (float (excali--corner-radius (min w h) element))))))
      out)))

(defun excali--native-media-extras (element)
  "Return image and frame render data of ELEMENT for the module.
A vector [KEY VALUE ...] read by `read_media_extras' in excali-module.c;
see `excali--image-native-extras' and `excali--frame-native-extras'."
  (vconcat (excali--frame-native-extras element)
           (excali--image-native-extras element)))

;;;; Inserting images

(defun excali--image-initial-size (natural-width natural-height)
  "Return (WIDTH . HEIGHT) of a new image of the given natural size.
Port of upstream `initializeImageDimensions': at most half the canvas
height (in scene units), and at most the canvas height minus 120px."
  (let* ((canvas-height (if excali--canvas-size
                            (/ (cdr excali--canvas-size) excali--pixel-scale)
                          600.0))
         (min-height (max (- canvas-height 120) 160))
         (max-height (min min-height (/ (ffloor (* canvas-height 0.5)) excali--zoom)))
         (height (min natural-height max-height)))
    (cons (* height (/ (float natural-width) natural-height)) (float height))))

(defun excali--read-image-file (file)
  "Return FILE's contents as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun excali--make-image-element (file-id natural-width natural-height center)
  "Return a new image element for FILE-ID centred on CENTER (X . Y).
It is sized by `excali--image-initial-size' and styled like upstream
`newImagePlaceholder' (current stroke, background, fill, stroke width,
stroke style, roughness and opacity; sharp corners)."
  (pcase-let* ((`(,w . ,h) (excali--image-initial-size natural-width natural-height))
               (element (excali--make-element
                         "image" (- (car center) (/ w 2)) (- (cdr center) (/ h 2))
                         (cons 'width w) (cons 'height h))))
    (when (fboundp 'excali--style-value)
      (dolist (property '(strokeColor backgroundColor fillStyle strokeWidth
                                      strokeStyle roughness opacity))
        (let ((value (excali--style-value property)))
          (when value
            (excali--put element property
                        (if (and (eq property 'strokeWidth) (not (numberp value)))
                            (excali--stroke-width-for "image" value)
                          value))))))
    (dolist (field `((roundness . :null) (status . "saved") (fileId . ,file-id)
                     (scale . [1 1]) (crop . :null)))
      (excali--put element (car field) (cdr field)))
    element))

(defun excali-insert-image (file)
  "Insert image FILE at the centre of the view and select it.
The file is stored in the scene's files map as a data URL, as upstream
does; its id is the file's SHA-1."
  (interactive "fInsert image: ")
  (require 'excali-clipboard)
  (let* ((file (expand-file-name file))
         (bytes (excali--read-image-file file))
         (mime (excali--image-file-mime file bytes)))
    (unless (member mime (mapcar #'cdr excali-image-mime-types))
      (user-error "Unsupported image type: %s" file))
    (let* ((file-id (secure-hash 'sha1 bytes))
           (url (excali--image-data-url mime bytes))
           (info (excali-native-image-register file-id url)))
      (unless info
        (user-error "Cannot decode image %s" file))
      ;; Downscale big raster images (upstream `resizeImageFile'); this
      ;; re-encodes them as PNG.
      (when (and excali-image-max-size
                 (not (equal mime "image/svg+xml"))
                 (> (max (aref info 0) (aref info 1)) excali-image-max-size))
        (when-let* ((png (excali-native-image-png file-id excali-image-max-size)))
          (setq mime "image/png"
                url (excali--image-data-url mime png)
                info (or (excali-native-image-register file-id url) info)
                bytes png)))
      (when (> (length bytes) excali-image-max-file-size)
        (unless (gethash file-id excali--image-users)
          (excali-native-image-forget file-id))
        (user-error "File is too big.  Maximum allowed size is %dMB"
                    (/ excali-image-max-file-size 1024 1024)))
      (excali--image-use file-id)
      (let ((now (truncate (* 1000 (float-time)))))
        (unless (excali--image-file-entry file-id)
          (setf (alist-get 'files excali--doc)
                (append (excali--doc-files)
                        (list (cons (intern file-id)
                                    (list (cons 'mimeType mime)
                                          (cons 'id file-id)
                                          (cons 'dataURL url)
                                          (cons 'created now)
                                          (cons 'lastRetrieved now))))))))
      (let ((element (excali--make-image-element
                      file-id (aref info 0) (aref info 1) (excali--view-center))))
        (excali--insert-elements (list element))
        (when (fboundp 'excali--render) (excali--render))
        element))))

(provide 'excali-image)
;;; excali-image.el ends here
