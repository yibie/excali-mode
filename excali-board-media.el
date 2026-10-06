;;; excali-board-media.el --- Static linked media cards -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;; Commentary:
;; Local previews only.  Generated files and process state never enter the scene.
;;; Code:
(require 'excali-board-reading)
(declare-function excali-board--require-board "excali-board" ())

(defvar-local excali-board-media--cache nil)
(defvar-local excali-board-media--jobs nil)
(defvar-local excali-board-media--directory nil)

(defun excali-board-media-type (file)
  "Return the supported media type of FILE, or nil."
  (let ((extension (downcase (or (file-name-extension file) ""))))
    (cond ((equal extension "pdf") "pdf")
          ((member extension '("png" "jpg" "jpeg" "svg" "webp" "gif" "bmp" "avif")) "image")
          ((member extension '("mp4" "mov" "mkv" "webm" "avi" "m4v")) "video")
          ((member extension '("mp3" "wav" "flac" "m4a" "ogg" "opus" "aac")) "audio"))))

(defun excali-board-media-data (card)
  "Return validated media metadata for CARD, or nil."
  (let* ((data (alist-get 'excaliBoardMedia (excali--get card 'customData)))
         (file (alist-get 'file data)))
    (and (stringp file) (file-name-absolute-p file) (not (file-remote-p file))
         (member (alist-get 'type data) '("pdf" "image" "video" "audio")) data)))

(defun excali-board-media--cleanup ()
  "Stop this board's preview jobs and remove temporary previews."
  (dolist (process excali-board-media--jobs)
    (set-process-sentinel process #'ignore)
    (when-let* ((timer (process-get process 'timeout))) (cancel-timer timer))
    (when (process-live-p process) (delete-process process)))
  (setq excali-board-media--jobs nil excali-board-media--cache nil)
  (when (and excali-board-media--directory (file-directory-p excali-board-media--directory))
    (delete-directory excali-board-media--directory t))
  (setq excali-board-media--directory nil))

(defun excali-board-media--command (type file output)
  "Return a local preview command for TYPE, FILE and OUTPUT, or nil."
  (if (equal type "pdf")
      (when-let* ((program (executable-find "pdftoppm")))
        (list program "-f" "1" "-singlefile" "-scale-to" "640" "-png"
              file (file-name-sans-extension output)))
    (when-let* ((program (executable-find "ffmpeg")))
      (append (list program "-nostdin" "-v" "error" "-y" "-protocol_whitelist" "file,pipe" "-i" file
                    "-map" "0:v:0"
                    "-frames:v" "1" "-vf" "scale=640:320:force_original_aspect_ratio=decrease"
                    "-threads" "1")
              (list output)))))

(defun excali-board-media--start (key type file)
  "Schedule a bounded preview job for KEY, TYPE and FILE."
  (when (< (length excali-board-media--jobs) 2)
    (unless excali-board-media--directory
      (setq excali-board-media--directory (make-temp-file "excali-media-" t))
      (add-hook 'kill-buffer-hook #'excali-board-media--cleanup nil t)
      (add-hook 'change-major-mode-hook #'excali-board-media--cleanup nil t))
    (let* ((output (expand-file-name (concat (secure-hash 'sha1 (prin1-to-string key)) ".png")
                                    excali-board-media--directory))
           (command (excali-board-media--command type file output))
           (board (current-buffer)))
      (puthash key (if command "Preparing preview…" "Preview tool unavailable") excali-board-media--cache)
      (when command
        (condition-case nil
            (let ((process
                   (make-process
                    :name "excali-media-preview" :buffer nil :command command
                    :connection-type 'pipe :noquery t
                    :sentinel
                    (lambda (process _event)
                      (when (memq (process-status process) '(exit signal))
                        (when-let* ((timer (process-get process 'timeout))) (cancel-timer timer))
                        (when (buffer-live-p board)
                          (with-current-buffer board
                            (setq excali-board-media--jobs (delq process excali-board-media--jobs))
                            (puthash key
                                     (or (and (= 0 (process-exit-status process))
                                              (file-regular-p output)
                                              (when-let* ((id (excali-board-reading--image output)))
                                                (cons 'image id)))
                                         (if (equal type "audio") "No embedded cover" "Preview unavailable"))
                                     excali-board-media--cache)
                            (excali--render)
                            (excali--sync-views))))))))
              (push process excali-board-media--jobs)
              (process-put process 'timeout
                           (run-at-time 20 nil (lambda ()
                                                (when (process-live-p process) (delete-process process))))))
          (error (puthash key "Preview unavailable" excali-board-media--cache)))))))

(defun excali-board-media--placeholder (type)
  "Return a native TYPE placeholder image ID."
  (let* ((id (concat "board-media-placeholder-" type))
         (svg (format "<svg xmlns='http://www.w3.org/2000/svg' width='280' height='120'><rect width='280' height='120' rx='8' fill='#f1f3f7'/><rect x='114' y='18' width='52' height='60' rx='5' fill='none' stroke='#8793a5' stroke-width='3'/><path d='M124 38h32M124 50h32M124 62h20' stroke='#8793a5' stroke-width='3'/><text x='140' y='104' text-anchor='middle' font-family='sans-serif' font-size='14' fill='#526079'>%s</text></svg>" (upcase type))))
    (if (excali-native-image-info id)
        (progn (excali--image-use id) id)
      (when (excali--image-register id (excali--image-data-url "image/svg+xml" svg)) id))))

(defun excali-board-media-blocks (card)
  "Return transient static preview blocks for media CARD."
  (when-let* ((data (excali-board-media-data card)))
    (unless excali-board-media--cache
      (setq excali-board-media--cache (make-hash-table :test #'equal)))
    (let* ((file (alist-get 'file data)) (type (alist-get 'type data))
           (attrs (and (file-regular-p file) (file-attributes file)))
           (key (list file type (and attrs (file-attribute-modification-time attrs))
                      (and attrs (file-attribute-size attrs))))
           (preview
            (cond ((not attrs) "File missing — relink explicitly")
                  ((equal type "image")
                   (or (when-let* ((id (excali-board-reading--image file))) (cons 'image id))
                       "Image unavailable or too large"))
                  (t (or (gethash key excali-board-media--cache)
                         (progn (excali-board-media--start key type file)
                                (gethash key excali-board-media--cache))
                         "Waiting for preview…"))))
           (id (if (consp preview) (cdr preview) (excali-board-media--placeholder type))))
      (vector (vector (format "<b>%s</b>" (excali-board-content--escape (file-name-nondirectory file))) nil [])
              (vector "Preview unavailable" nil [] id)
              (vector (if (consp preview) "Static preview" preview) nil [])))))

(defun excali-board-insert-media (file)
  "Insert local FILE as a typed static media card, not an embedded copy."
  (interactive (list (excali-board-vault-read-file "Media: " 'media)))
  (excali-board--require-board)
  (setq file (expand-file-name file))
  (let ((type (excali-board-media-type file)))
    (unless (and type (not (file-remote-p file)) (file-regular-p file))
      (user-error "Choose an existing local PDF, image, video or audio file"))
    (let ((card (excali--make-element
                 "rectangle" (+ 60.0 (* 40 (length excali--elements))) 60.0
                 '(width . 360.0) '(height . 300.0)
                 '(backgroundColor . "#ffffff") '(fillStyle . "solid")
                 '(strokeColor . "#b8bec9") '(roughness . 0) '(strokeWidth . 1)
                 '(roundness . ((type . 3) (value . 8)))
                 (cons 'customData `((excaliBoardMedia . ((version . 1) (type . ,type) (file . ,file)))
                                     (excaliBoardAttachment . ((file . ,file))))))))
      (setq excali--elements (append excali--elements (list card)))
      (excali--set-text (excali--add-bound-text card)
                        (concat (upcase type) "\n" (file-name-nondirectory file)))
      (excali--select (list card))
      (excali--commit)
      (excali--render)
      (excali--sync-views)
      card)))

(defun excali-board-media-refresh ()
  "Discard transient preview results and retry, without modifying the scene."
  (interactive)
  (excali-board--require-board)
  (excali-board-media--cleanup)
  (setq excali-board-reading--images nil)
  (excali--render)
  (excali--sync-views))

(defun excali-board-media-relink (file)
  "Explicitly replace the selected media card's source with local FILE."
  (interactive (list (excali-board-vault-read-file "Replacement media: " 'media)))
  (excali-board--require-board)
  (let* ((card (excali--single-selection))
         (data (copy-tree (excali--get card 'customData) t))
         (type (excali-board-media-type file)))
    (setq file (expand-file-name file))
    (unless (and (excali-board-media-data card) type
                 (not (file-remote-p file)) (file-regular-p file))
      (user-error "Select one media card and an existing local media file"))
    (setf (alist-get 'excaliBoardMedia data)
          `((version . 1) (type . ,type) (file . ,file))
          (alist-get 'excaliBoardAttachment data) `((file . ,file)))
    (excali--put card 'customData data)
    (excali--touch card)
    (excali--set-text (or (excali--bound-text-of card) (excali--add-bound-text card))
                      (concat (upcase type) "\n" (file-name-nondirectory file)))
    (excali--commit)
    (excali--render)
    (excali--sync-views)))

(provide 'excali-board-media)
;;; excali-board-media.el ends here
