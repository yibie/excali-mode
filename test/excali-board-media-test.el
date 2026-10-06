;;; excali-board-media-test.el --- Static media regressions -*- lexical-binding: t; -*-
(require 'excali-board-test)

(defun excali-board-media-test--svg (file)
  (with-temp-file file
    (insert "<svg xmlns='http://www.w3.org/2000/svg' width='200' height='100'><rect width='200' height='100' fill='#52a878'/></svg>")))

(defun excali-board-media-test--wait ()
  (let ((deadline (+ (float-time) 25)))
    (while (and excali-board-media--jobs (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (should-not excali-board-media--jobs)))

(ert-deftest excali-board-media-types-and-validation ()
  (dolist (pair '(("A.PDF" . "pdf") ("a.svg" . "image") ("a.m4v" . "video") ("a.opus" . "audio")))
    (should (equal (cdr pair) (excali-board-media-type (car pair)))))
  (excali-board-test--with
    (should-error (excali-board-insert-media file) :type 'user-error)
    (should-error (excali-board-insert-media "/ssh:host:/a.png") :type 'user-error)))

(ert-deftest excali-board-media-image-roundtrip-and-relink ()
  (excali-board-test--with
    (let* ((image (expand-file-name "A & B.svg" dir))
           (other (expand-file-name "other.svg" dir)))
      (excali-board-media-test--svg image)
      (excali-board-media-test--svg other)
      (let* ((card (excali-board-insert-attachment image))
             (blocks (excali-board--blocks card))
             (data (copy-tree (excali--get card 'customData) t)))
        (should (equal "image" (alist-get 'type (excali-board-media-data card))))
        (should (string-match-p "A &amp; B" (aref (aref blocks 0) 0)))
        (should (string-prefix-p "board-inline-" (aref (aref blocks 1) 3)))
        (should-not (alist-get 'files excali--doc))
        (setq excali--file (expand-file-name "board.excalidraw" dir))
        (excali-board-save)
        (let* ((doc (excali--restore-doc (excali--read-file excali--file)))
               (restored (aref (alist-get 'elements doc) 0)))
          (should (equal data (excali--get restored 'customData))))
        (excali-board-media-relink other)
        (should (= 2 (length excali--elements)))
        (should (equal other (alist-get 'file (excali-board-media-data card))))
        (delete-file other)
        (should (string-match-p "File missing" (aref (aref (excali-board--blocks card) 2) 0)))
        ;; Never guess the still-existing similarly typed file.
        (should (equal other (alist-get 'file (excali-board-media-data card))))))))

(ert-deftest excali-board-media-missing-tool-and-refresh ()
  (excali-board-test--with
    (let ((pdf (expand-file-name "empty.pdf" dir)))
      (write-region "not a PDF" nil pdf nil 'silent)
      (let ((card (excali-board-insert-media pdf)))
        (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
          (should (equal "Preview tool unavailable" (aref (aref (excali-board--blocks card) 2) 0)))
          (should-not excali-board-media--jobs)
          (set-buffer-modified-p nil)
          (excali-board-refresh)
          (should-not (buffer-modified-p (current-buffer))))))))

(ert-deftest excali-board-media-process-cap-and-cleanup ()
  (excali-board-test--with
    (cl-letf (((symbol-function 'excali-board-media--command)
               (lambda (&rest _) '("/bin/sleep" "10"))))
      (dotimes (i 3)
        (let ((file (expand-file-name (format "%s.pdf" i) dir)))
          (write-region "x" nil file nil 'silent)
          (excali-board--blocks (excali-board-insert-media file))))
      (should (= 2 (length excali-board-media--jobs)))
      (let ((directory excali-board-media--directory)
            (jobs (copy-sequence excali-board-media--jobs)))
        (excali-board-media--cleanup)
        (should-not (file-exists-p directory))
        (should-not (seq-some #'process-live-p jobs))))))

(ert-deftest excali-board-media-real-video-and-audio ()
  (skip-unless (executable-find "ffmpeg"))
  (excali-board-test--with
    (dolist (type '("video" "audio"))
      (let* ((video (equal type "video"))
             (path (expand-file-name (if video "clip.mp4" "sound.wav") dir)))
        (should (= 0 (call-process "ffmpeg" nil nil nil "-v" "error" "-y"
                                   "-f" "lavfi" "-i" (if video "color=c=blue:s=160x100:d=0.2" "sine=duration=0.2") path)))
        (let ((card (excali-board-insert-media path)))
          (excali-board--blocks card)
          (excali-board-media-test--wait)
          (let ((blocks (excali-board--blocks card)))
            (should (equal (if video "Static preview" "No embedded cover")
                           (aref (aref blocks 2) 0)))
            (should (stringp (aref (aref blocks 1) 3)))))))))

(ert-deftest excali-board-media-real-pdf-first-page ()
  (skip-unless (executable-find "pdftoppm"))
  (excali-board-test--with
    (let ((pdf (expand-file-name "sample.pdf" dir)))
      ;; A small valid two-page PDF; previews must extract only the first.
      (with-temp-file pdf
        (set-buffer-multibyte nil)
        (insert "%PDF-1.4\n")
        (let ((objects '("<< /Type /Catalog /Pages 2 0 R >>"
                         "<< /Type /Pages /Kids [3 0 R 4 0 R] /Count 2 >>"
                         "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 120 80] >>"
                         "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 80 120] >>"))
              offsets)
          (cl-loop for obj in objects for i from 1 do
                   (push (1- (point)) offsets)
                   (insert (format "%d 0 obj\n%s\nendobj\n" i obj)))
          (let ((xref (1- (point))))
            (insert "xref\n0 5\n0000000000 65535 f \n")
            (dolist (offset (nreverse offsets)) (insert (format "%010d 00000 n \n" offset)))
            (insert (format "trailer\n<< /Size 5 /Root 1 0 R >>\nstartxref\n%d\n%%%%EOF\n" xref)))))
      (let ((card (excali-board-insert-media pdf)))
        (excali-board--blocks card)
        (excali-board-media-test--wait)
        (let* ((blocks (excali-board--blocks card))
               (size (excali-native-image-info (aref (aref blocks 1) 3))))
          (should (equal "Static preview" (aref (aref blocks 2) 0)))
          (should (> (aref size 0) (aref size 1)))
          (should (= 1 (length (directory-files excali-board-media--directory nil "\\.png\\'")))))))))

(ert-deftest excali-board-media-real-audio-cover ()
  (skip-unless (executable-find "ffmpeg"))
  (excali-board-test--with
    (let ((cover (expand-file-name "cover.png" dir))
          (audio (expand-file-name "cover.mp3" dir)))
      (should (= 0 (call-process "ffmpeg" nil nil nil "-v" "error" "-y"
                                 "-f" "lavfi" "-i" "color=c=red:s=160x100"
                                 "-frames:v" "1" cover)))
      (should (= 0 (call-process "ffmpeg" nil nil nil "-v" "error" "-y"
                                 "-f" "lavfi" "-i" "sine=duration=0.2" "-i" cover
                                 "-map" "0:a" "-map" "1:v" "-c:a" "libmp3lame"
                                 "-c:v" "copy" "-id3v2_version" "3"
                                 "-disposition:v" "attached_pic" audio)))
      (let ((card (excali-board-insert-media audio)))
        (excali-board--blocks card)
        (excali-board-media-test--wait)
        (should (equal "Static preview" (aref (aref (excali-board--blocks card) 2) 0)))))))

(ert-deftest excali-board-media-timeout-is-cached-and-retryable ()
  (excali-board-test--with
    (let ((pdf (expand-file-name "slow.pdf" dir)) timeout)
      (write-region "x" nil pdf nil 'silent)
      (let ((card (excali-board-insert-media pdf)))
        (cl-letf (((symbol-function 'excali-board-media--command)
                   (lambda (&rest _) '("/bin/sleep" "10")))
                  ((symbol-function 'run-at-time)
                   (lambda (_seconds _repeat callback &rest _) (setq timeout callback) nil)))
          (excali-board--blocks card))
        (should (= 1 (length excali-board-media--jobs)))
        (funcall timeout)
        (excali-board-media-test--wait)
        (should (equal "Preview unavailable" (aref (aref (excali-board--blocks card) 2) 0)))
        (should-not excali-board-media--jobs)
        (excali-board-media-refresh)
        (should-not excali-board-media--cache)))))

(ert-deftest excali-board-media-malformed-metadata-not-rendered ()
  (excali-board-test--with
    (let ((card (excali--make-element "rectangle" 0 0)))
      (dolist (file '("relative.mp4" "/ssh:host:/video.mp4" 10))
        (excali--put card 'customData `((excaliBoardMedia . ((type . "video") (file . ,file)))))
        (should-not (excali-board-media-data card))
        (should-not (excali-board-media-blocks card))))))

(provide 'excali-board-media-test)

(ert-deftest excali-board-file-double-click-opens-media-and-generic ()
  (excali-board-test--with
    (let ((image (expand-file-name "image.svg" dir)))
      (excali-board-media-test--svg image)
      (dolist (path (list image file))
        (excali-board-insert-attachment path)
        (let* ((card (excali--single-selection))
               (label (excali--bound-text-of card)))
          ;; Both the body and bound filename dispatch to the file, not text editing.
          (dolist (hit (list card label))
            (let (opened released)
              (cl-letf (((symbol-function 'excali--select-event-view) #'ignore)
                        ((symbol-function 'excali--event-scene-xy) (lambda (_) '(0 . 0)))
                        ((symbol-function 'excali--hit) (lambda (_) hit))
                        ((symbol-function 'excali--await-release) (lambda () (setq released t)))
                        ((symbol-function 'find-file-other-window)
                         (lambda (f) (should released) (setq opened f)))
                        ((symbol-function 'excali-double-click) (lambda (_) (ert-fail "Unexpected editor"))))
                (excali-board-double-click 'test)
                (should (equal path opened))))))))))

(ert-deftest excali-board-file-open-missing-does-not-create-file ()
  (excali-board-test--with
    (let ((image (expand-file-name "gone.svg" dir)))
      (excali-board-media-test--svg image)
      (excali-board-insert-media image)
      (delete-file image)
      (cl-letf (((symbol-function 'find-file-other-window)
                 (lambda (_) (ert-fail "Must not visit a missing attachment"))))
        (should-error (excali-board-open-attachment) :type 'user-error)))))
