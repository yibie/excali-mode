;;; excali-org-prototype-test.el --- Opt-in card experiment tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'excali-org-prototype
         (expand-file-name "../experiments/excali-org-prototype"
                           (file-name-directory (or load-file-name buffer-file-name))))

(defmacro excali-org-prototype-test--with (&rest body)
  "Run BODY with temporary ORG, scene, FILE and REFERENCE."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "excali-org-test-" t))
          (file (expand-file-name "notes.org" dir))
          (org (find-file-noselect file))
          (scene (generate-new-buffer " *org-card-test*"))
          (reference `((version . 1) (file . ,file) (id . "card-test-id")))
          (org-id-track-globally nil))
     (unwind-protect
         (progn
           (with-current-buffer org
             (insert "* TODO Original\n:PROPERTIES:\n:ID: card-test-id\n:END:\nOriginal body.\n** Child\nPrivate child text.\n")
             (save-buffer) (goto-char (point-min)))
           (with-current-buffer scene
             (setq major-mode 'excali-mode
                   excali--native-cache (make-hash-table :test #'eq)
                   excali--doc (excali--empty-doc))
             (excali--history-reset)
             (cl-letf (((symbol-function 'excali--render) #'ignore)) ,@body)))
       (dolist (buf (list org scene))
         (when (buffer-live-p buf)
           (with-current-buffer buf (set-buffer-modified-p nil)) (kill-buffer buf)))
       (delete-directory dir t))))

(ert-deftest excali-org-prototype-heading-excludes-metadata-and-children ()
  (excali-org-prototype-test--with
    (let ((text (with-current-buffer org (excali-org-prototype--heading))))
      (should (equal text "TODO Original\n\nOriginal body.")))))

(ert-deftest excali-org-prototype-refresh-live-narrowed-source-preserves-layout ()
  (excali-org-prototype-test--with
    (let ((card (excali-org-prototype--insert reference "Old")))
      (excali--put card 'x 410.0) (excali--put card 'y 230.0)
      (excali--put card 'backgroundColor "#ff0000") (excali--touch card)
      (with-current-buffer org
        (goto-char (point-min)) (search-forward "Original") (replace-match "Renamed")
        (goto-char (point-min)) (search-forward "Original body.") (replace-match "Unsaved edit.")
        (search-forward "** Child") (beginning-of-line) (org-narrow-to-subtree))
      (excali-org-prototype-refresh)
      (should (equal (excali--get (excali--bound-text-of card) 'originalText)
                     "TODO Renamed\n\nUnsaved edit."))
      (should (= 410 (excali--get card 'x)))
      (should (= 230 (excali--get card 'y)))
      (should (equal "#ff0000" (excali--get card 'backgroundColor)))
      (should (with-current-buffer org (buffer-narrowed-p)))
      (should (with-current-buffer org (buffer-modified-p))))))

(ert-deftest excali-org-prototype-roundtrip-reference-and-bound-label ()
  (excali-org-prototype-test--with
    (excali-org-prototype--insert reference "Card")
    (setq excali--file (expand-file-name "board.excalidraw" dir))
    (excali-save)
    (let* ((doc (excali--restore-doc (excali--read-file excali--file)))
           (excali--elements (append (alist-get 'elements doc) nil))
           (card (car excali--elements)))
      (should (equal reference (excali-org-prototype--reference card)))
      (setq excali--selection (list (excali--bound-text-of card)))
      (should (eq card (excali-org-prototype--selected-card)))
      (excali-org-prototype-refresh)
      (should (string-match-p "Original body" (excali--get (excali--bound-text-of card) 'text))))))

(ert-deftest excali-org-prototype-missing-id-does-not-change-card ()
  (excali-org-prototype-test--with
    (let* ((card (excali-org-prototype--insert reference "Keep this"))
           (before (copy-tree excali--elements t)))
      (with-current-buffer org (erase-buffer) (insert "* Replacement\n"))
      (should-error (excali-org-prototype-refresh) :type 'user-error)
      (should (equal before excali--elements))
      (should (equal "Keep this" (excali--get (excali--bound-text-of card) 'text))))))

(ert-deftest excali-org-prototype-refresh-is-undoable ()
  (excali-org-prototype-test--with
    (excali-org-prototype--insert reference "Before")
    (excali-org-prototype-refresh)
    (excali-undo)
    (should (equal "Before" (excali--get (excali--bound-text-of (car excali--elements))
                                       'originalText)))))

(ert-deftest excali-org-prototype-delete-card-does-not-delete-heading ()
  (excali-org-prototype-test--with
    (excali-org-prototype--insert reference "Card")
    (excali-delete-selected)
    (should-not (excali--live-elements))
    (should (with-current-buffer org (org-find-entry-with-id "card-test-id")))))

(ert-deftest excali-org-prototype-rejects-missing-and-remote-files ()
  (should-error (excali-org-prototype--resolve '((file . "/ssh:host:/a.org") (id . "x")))
                :type 'user-error)
  (should-error (excali-org-prototype--resolve '((file . "/nonexistent-excali-test/a.org") (id . "x")))
                :type 'user-error))

(ert-deftest excali-org-prototype-enable-only-in-canvas ()
  (with-temp-buffer
    (should-error (excali-org-prototype-mode 1) :type 'user-error)
    (should-not excali-org-prototype-mode)))

(ert-deftest excali-org-prototype-add-heading-adds-id-without-saving ()
  (excali-org-prototype-test--with
    (with-current-buffer org
      (erase-buffer) (insert "* New heading\nBody\n") (save-buffer)
      (goto-char (point-min)))
    (save-window-excursion
      (with-current-buffer org (excali-org-prototype-add-heading scene)))
    (should (= 2 (length excali--elements)))
    (should (with-current-buffer org (buffer-modified-p)))
    (should (with-current-buffer org (org-entry-get (point-min) "ID")))
    (should-not (with-temp-buffer (insert-file-contents file)
                                 (search-forward ":ID:" nil t)))))

(ert-deftest excali-org-prototype-read-only-without-id-does-not-create-card ()
  (excali-org-prototype-test--with
    (with-current-buffer org
      (erase-buffer) (insert "* Read-only heading\n") (goto-char (point-min))
      (let ((buffer-read-only t))
        (should-error (excali-org-prototype-add-heading scene))))
    (should-not excali--elements)))


(ert-deftest excali-org-prototype-live-source-hook-refreshes-without-selection ()
  (excali-org-prototype-test--with
    (let ((card (excali-org-prototype--insert reference "Old")))
      (excali-org-prototype-mode 1)
      (setq excali--selection nil)
      (with-current-buffer org
        (goto-char (point-min)) (search-forward "Original")
        (replace-match "Live")
        (should (timerp excali-org-prototype--timer))
        (excali-org-prototype--cancel-timer))
      (excali-org-prototype--sync-source org)
      (should (string-match-p "Live" (excali--get (excali--bound-text-of card) 'text)))
      (should (with-current-buffer org (buffer-modified-p)))
      (should-not excali--selection)
      ;; An unchanged refresh must not dirty the scene or create undo steps.
      (set-buffer-modified-p nil)
      (excali-org-prototype-refresh-all)
      (should-not (buffer-modified-p)))))

(ert-deftest excali-org-prototype-reopened-scene-enables-commands ()
  (excali-org-prototype-test--with
    (excali-org-prototype--insert reference "Old")
    (excali-org-prototype--opened scene)
    (should excali-org-prototype-mode)
    (should (eq (key-binding (kbd "C-c C-o")) #'excali-org-prototype-visit))
    (should (with-current-buffer org
              (memq #'excali-org-prototype--schedule after-change-functions)))))

(ert-deftest excali-org-prototype-style-and-padding-survive-resize ()
  (excali-org-prototype-test--with
    (let* ((card (excali-org-prototype--insert reference "Title

Body"))
           (label (excali--bound-text-of card))
           (plain (excali--make-element "rectangle" 0 0 '(width . 360))))
      (should (= (excali--get card 'roughness) 0))
      (should (equal (excali--get card 'backgroundColor) "#ffffff"))
      (should (= (- (excali--get label 'x) (excali--get card 'x)) 20))
      (should (= (- (excali--get label 'y) (excali--get card 'y)) 20))
      (excali--put card 'width 280.0)
      (excali--layout-bound-text card 'se)
      (should (= (- (excali--get label 'x) (excali--get card 'x)) 20))
      (should (= (excali--bound-text-max-width plain) 350)))))

(ert-deftest excali-org-prototype-sync-preserves-broken-card-and-updates-good-one ()
  (excali-org-prototype-test--with
    (let ((good (excali-org-prototype--insert reference "Old"))
          (bad (excali-org-prototype--insert
                (cons '(id . "missing") reference) "Keep")))
      (excali-org-prototype-refresh-all)
      (should (string-match-p "Original" (excali--get (excali--bound-text-of good) 'text)))
      (should (equal "Keep" (excali--get (excali--bound-text-of bad) 'text)))
      (should excali-org-prototype--sync-error))))


(ert-deftest excali-org-prototype-visit-keeps-canvas-visible ()
  (excali-org-prototype-test--with
    (excali-org-prototype--insert reference "Old")
    (save-window-excursion
      (delete-other-windows)
      (switch-to-buffer scene)
      (let ((canvas-window (selected-window)))
        (excali-org-prototype-visit)
        (should (eq (current-buffer) org))
        (should (eq (window-buffer canvas-window) scene))
        (should-not (eq (selected-window) canvas-window))
        (should (org-at-heading-p))
        (should (equal (org-entry-get nil "ID") "card-test-id"))))))

(ert-deftest excali-org-prototype-revisited-source-restores-watch ()
  (excali-org-prototype-test--with
    (excali-org-prototype--insert reference "Old")
    (excali-org-prototype-mode 1)
    (kill-buffer org)
    (setq org (find-file-noselect file))
    (with-current-buffer org
      (should (memq #'excali-org-prototype--schedule after-change-functions))
      (should (timerp excali-org-prototype--timer))
      (excali-org-prototype--cancel-timer))))

(provide 'excali-org-prototype-test)
