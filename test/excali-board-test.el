;;; excali-board-test.el --- Org board regression tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'excali-board)

(defmacro excali-board-test--with (&rest body)
  "Run BODY with a real derived-mode BOARD and local Org SOURCE."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "excali-board-test-" t))
          (file (expand-file-name "notes.org" dir))
          (source (find-file-noselect file))
          (board (generate-new-buffer " *board-test*"))
          (org-id-track-globally nil)
          (native-comp-enable-subr-trampolines nil)
          (excali-board--last-board nil)
          (ref `((version . 1) (scope . "heading")
                 (file . ,file) (id . "board-heading"))))
     (unwind-protect
         (save-window-excursion
           (with-current-buffer source
             (insert "* TODO Original\n:PROPERTIES:\n:ID: board-heading\n:END:\nBody text.\n** Child\nChild body.\n* Original\nSecond heading.\n")
             (save-buffer) (goto-char (point-min)))
           (with-current-buffer board
             (excali-board-mode)
             (setq excali--doc (excali--empty-doc))
             (setf (alist-get 'excaliBoard excali--doc) '((version . 1)))
             (excali--history-reset)
             (cl-letf (((symbol-function 'excali--render) #'ignore))
               ,@body)))
       (dolist (buffer (list board source))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory dir t))))

(ert-deftest excali-board-derived-mode-and-keys ()
  (excali-board-test--with
    (should (derived-mode-p 'excali-mode))
    (should (eq (key-binding (kbd "C-c C-i")) #'excali-board-insert-heading))
    (should (eq (key-binding (kbd "C-c C-o")) #'excali-board-visit))
    (should (eq (key-binding (kbd "C-x C-s")) #'excali-board-save))
    (should (eq (lookup-key excali-board-mode-map (kbd "C-c C-g")) #'excali-group))
    (should-not (lookup-key excali-mode-map (kbd "C-c C-o")))))

(ert-deftest excali-board-document-dispatch-is-explicit ()
  (should (eq (excali--document-mode (excali--empty-doc)) #'excali-mode))
  (excali-board-test--with
    (should (eq (excali--document-mode excali--doc) #'excali-board-mode))
    (setf (alist-get 'excaliBoard excali--doc) '((version . 999)))
    (should-error (excali--document-mode excali--doc) :type 'user-error)))

(ert-deftest excali-board-roundtrip-schema-and-reference ()
  (excali-board-test--with
    (excali-board--insert ref "Old")
    (setq excali--file (expand-file-name "board.excalidraw" dir))
    (excali-board-save)
    (should-not (buffer-modified-p))
    (let* ((doc (excali--restore-doc (excali--read-file excali--file)))
           (card (aref (alist-get 'elements doc) 0)))
      (should (equal (alist-get 'excaliBoard doc) '((version . 1))))
      (should (equal (excali-board--reference card) ref))
      (should (equal (excali--get card 'type) "rectangle")))))

(ert-deftest excali-board-open-runs-derived-initialization ()
  (excali-board-test--with
    (excali-board--insert ref "Old")
    (let ((doc (copy-tree excali--doc t)) opened)
      (setf (alist-get 'elements doc) (vconcat excali--elements))
      (unwind-protect
          (cl-letf (((symbol-function 'display-graphic-p) #'always)
                    ((symbol-function 'image-type-available-p) #'always)
                    ((symbol-function 'excali--sync-canvas) #'ignore))
            (setq opened (excali--open doc nil " *opened-board*"))
            (with-current-buffer opened
              (should (eq major-mode 'excali-board-mode))
              (should (string-match-p
                       "Original" (excali--get (excali--bound-text-of (car excali--elements)) 'text)))
              (should-not excali-board--errors)))
        (when (buffer-live-p opened)
          (with-current-buffer opened (set-buffer-modified-p nil))
          (kill-buffer opened))))))

(ert-deftest excali-board-add-from-org-chooses-target-before-modifying-source ()
  (excali-board-test--with
    (with-current-buffer source
      (goto-char (point-max)) (insert "* New heading\n")
      (forward-line -1)
      (cl-letf (((symbol-function 'excali-board--read-target) (lambda () board)))
        (call-interactively #'excali-board-add-heading)))
    (with-current-buffer board
      (should (= (length excali--elements) 2))
      (should (equal "New heading" (excali--get (excali--bound-text-of (car excali--elements))
                                               'originalText))))
    (with-current-buffer source
      (goto-char (point-max)) (forward-line -1)
      (should (org-entry-get nil "ID"))
      (should (buffer-modified-p)))
    (should-not (string-match-p "New heading"
                                (with-temp-buffer (insert-file-contents file) (buffer-string))))))

(ert-deftest excali-board-cancel-does-not-create-source-id ()
  (excali-board-test--with
    (with-current-buffer source
      (goto-char (point-max)) (insert "* No ID\n") (forward-line -1)
      (cl-letf (((symbol-function 'excali-board--read-target)
                 (lambda () (user-error "Cancelled"))))
        (should-error (excali-board-add-heading) :type 'user-error))
      (should-not (org-entry-get nil "ID")))
    (should-not excali--elements)))

(ert-deftest excali-board-read-heading-disambiguates-duplicates ()
  (excali-board-test--with
    (let (labels marker)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_ collection &rest _)
                   (setq labels (mapcar #'car collection))
                   (car (last labels)))))
        (setq marker (excali-board--read-heading file)))
      (unwind-protect
          (progn
            (should (= (length labels) 3))
            (should (= (length (delete-dups (copy-sequence labels))) 3))
            (with-current-buffer source
              (goto-char marker)
              (should (equal (org-get-heading t t t t) "Original"))
              (should (> (line-number-at-pos) 7))))
        (set-marker marker nil)))))

(ert-deftest excali-board-insert-from-board ()
  (excali-board-test--with
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_ collection &rest _) (caar collection))))
      (excali-board-insert-heading file))
    (should (eq (current-buffer) board))
    (should (= (length excali--elements) 2))
    (should (equal (excali-board--reference (car excali--elements)) ref))))

(ert-deftest excali-board-refresh-live-narrowed-source-preserves-layout ()
  (excali-board-test--with
    (let ((card (excali-board--insert ref "Old")))
      (excali--put card 'x 410.0) (excali--put card 'y 220.0)
      (excali--put card 'backgroundColor "#ff0000") (excali--touch card)
      (excali-board--watch-sources)
      (with-current-buffer source
        (goto-char (point-min)) (search-forward "Original") (replace-match "Renamed")
        (search-forward "** Child") (beginning-of-line) (org-narrow-to-subtree)
        (should (timerp excali-board--timer))
        (excali-board--cancel-timer))
      (setq excali--selection nil)
      (excali-board--sync-source source)
      (should (equal (excali--get (excali--bound-text-of card) 'originalText)
                     "TODO Renamed\n\nBody text."))
      (should (= (excali--get card 'x) 410))
      (should (= (excali--get card 'y) 220))
      (should (equal (excali--get card 'backgroundColor) "#ff0000"))
      (should (with-current-buffer source (buffer-narrowed-p)))
      (should (with-current-buffer source (buffer-modified-p)))
      (set-buffer-modified-p nil)
      (let ((history excali--undo-stack))
        (excali-board-refresh-all)
        (should (eq history excali--undo-stack))
        (should-not (buffer-modified-p))))))

(ert-deftest excali-board-refresh-undo-and-broken-reference ()
  (excali-board-test--with
    (excali-board--insert ref "Old")
    (excali-board-refresh-all)
    (excali-undo)
    (should (equal "Old" (excali--get (excali--bound-text-of (car excali--elements))
                                     'originalText)))
    (with-current-buffer source (erase-buffer) (insert "* Missing\n"))
    (excali-board-refresh-all)
    (should (= (length excali-board--errors) 1))
    (should (equal "Old" (excali--get (excali--bound-text-of (car excali--elements)) 'text)))))

(ert-deftest excali-board-rejects-unsupported-and-remote-references ()
  (excali-board-test--with
    (setf (alist-get 'version ref) 2)
    (should-error (excali-board--resolve ref) :type 'user-error)
    (setf (alist-get 'version ref) 1 (alist-get 'file ref) "/ssh:invalid:/notes.org")
    (should-error (excali-board--resolve ref) :type 'user-error)))

(ert-deftest excali-board-padding-is-local ()
  (excali-board-test--with
    (let* ((card (excali-board--insert ref "Title\n\nBody"))
           (label (excali--bound-text-of card)))
      (should (= (- (excali--get label 'x) (excali--get card 'x)) 20))
      (let ((plain (excali--make-element "rectangle" 0 0 '(width . 360))))
        (should (= (excali--bound-text-max-width plain) 350)))
      (with-temp-buffer
        (excali-mode)
        (should-not excali-text-padding-function)
        (should (= (excali--bound-text-max-width card) 350))))))

(ert-deftest excali-board-visit-preserves-canvas-and-source-id ()
  (excali-board-test--with
    (excali-board--insert ref "Old")
    (delete-other-windows) (switch-to-buffer board)
    (let ((window (selected-window)))
      (excali-board-visit)
      (should (eq (current-buffer) source))
      (should (equal (org-entry-get nil "ID") "board-heading"))
      (should (eq (window-buffer window) board))
      (excali-board-return)
      (should (eq (current-buffer) board)))))

(ert-deftest excali-board-migration-is-deep-copy ()
  (excali-board-test--with
    (let* ((old-ref '((version . 1) (file . "/tmp/notes.org") (id . "old")))
           (card (excali--make-element
                  "rectangle" 10 20 '(width . 100) '(height . 80)
                  (cons 'customData (list (cons 'other "keep")
                                         (cons 'excaliOrgPrototype old-ref)))))
           (doc (excali--empty-doc)))
      (setf (alist-get 'elements doc) (vector card))
      (let* ((before (copy-tree doc t))
             (copy (excali-board--migrate-document doc))
             (migrated (aref (alist-get 'elements copy) 0)))
        (should (equal doc before))
        (should-not (assq 'excaliBoard doc))
        (should (equal (alist-get 'scope (excali-board--reference migrated)) "heading"))
        (should (equal (alist-get 'other (excali--get migrated 'customData)) "keep"))
        (should-not (alist-get 'excaliOrgPrototype (excali--get migrated 'customData)))))))

(ert-deftest excali-board-copy-protects-original-on-save ()
  (excali-board-test--with
    (let ((original (expand-file-name "drawing.excalidraw" dir)))
      (with-temp-file original (insert "original"))
      (setq excali-board--origin-file original excali--file original)
      (should-error (excali-save) :type 'user-error)
      (should-not excali--file)
      (should (equal (with-temp-buffer (insert-file-contents original) (buffer-string))
                     "original")))))

(ert-deftest excali-board-watches-removed-after-last-board-killed ()
  (excali-board-test--with
    (excali-board--insert ref "Old")
    (excali-board--watch-sources)
    (should (with-current-buffer source
              (memq #'excali-board--schedule after-change-functions)))
    (kill-buffer board)
    (should-not (with-current-buffer source
                  (memq #'excali-board--schedule after-change-functions)))))


(ert-deftest excali-board-hotspot-uses-child-mouse-command ()
  (excali-board-test--with
    (should (eq (key-binding [excali-canvas double-down-mouse-1])
                #'excali-board-double-click))
    (should (eq (key-binding [excali-canvas down-mouse-1])
                #'excali-board-mouse-down))
    (should (eq (lookup-key excali-mode-map [excali-canvas double-down-mouse-1])
                #'excali-double-click))))

(ert-deftest excali-board-read-only-heading-without-id-does-not-insert ()
  (excali-board-test--with
    (with-current-buffer source
      (goto-char (point-max)) (insert "* Read only
") (forward-line -1)
      (setq buffer-read-only t)
      (should-error (excali-board-add-heading board) :type 'buffer-read-only))
    (should-not excali--elements)))

(ert-deftest excali-board-reopen-source-restores-watches ()
  (excali-board-test--with
    (excali-board--insert ref "Old")
    (excali-board--watch-sources)
    (kill-buffer source)
    (setq source (find-file-noselect file))
    (should (with-current-buffer source
              (memq #'excali-board--schedule after-change-functions)))))

(ert-deftest excali-board-unsupported-migration-does-not-touch-source ()
  (excali-board-test--with
    (let ((doc (excali--empty-doc)))
      (setf (alist-get 'elements doc)
            [((customData . ((excaliOrgPrototype . ((version . 2))))))])
      (let ((before (copy-tree doc t)))
        (should-error (excali-board--migrate-document doc) :type 'user-error)
        (should (equal before doc))))))

(ert-deftest excali-board-select-target-excludes-ordinary-drawings ()
  (excali-board-test--with
    (let ((ordinary (generate-new-buffer " *drawing-only*")))
      (unwind-protect
          (progn
            (with-current-buffer ordinary (excali-mode))
            (let ((completing-read-function
                   (lambda (_ collection &rest _)
                     (should (assoc (buffer-name board) collection))
                     (should-not (assoc (buffer-name ordinary) collection))
                     (buffer-name board))))
              (should (eq (excali-board--read-target) board))))
        (kill-buffer ordinary)))))


(ert-deftest excali-board-missing-heading-recovers-after-source-edit ()
  (excali-board-test--with
    (excali-board--insert ref "Cached")
    (with-current-buffer source
      (goto-char (point-min))
      (org-entry-delete nil "ID"))
    (excali-board--watch-sources)
    (excali-board-refresh-all)
    (should excali-board--errors)
    (with-current-buffer source
      (org-entry-put nil "ID" "board-heading")
      (should (timerp excali-board--timer))
      (excali-board--cancel-timer))
    (excali-board--sync-source source)
    (should-not excali-board--errors)
    (should (string-match-p "Original"
                            (excali--get (excali--bound-text-of (car excali--elements)) 'text)))))


(ert-deftest excali-board-new-opens-right-and-reuses-board-pane ()
  (excali-board-test--with
    (delete-other-windows)
    (switch-to-buffer source)
    (let ((left (selected-window)) (excali-board-vaults (list (cons "Default" dir))) first second third)
      (unwind-protect
          (cl-letf (((symbol-function 'display-graphic-p) #'always)
                    ((symbol-function 'image-type-available-p) #'always)
                    ((symbol-function 'excali--sync-canvas) #'ignore))
            (setq first (excali-board-new (expand-file-name "one.excalidraw" dir)))
            (should (file-exists-p (expand-file-name "one.excalidraw" dir)))
            (should (equal (alist-get 'vaultRoot
                                     (alist-get 'excaliBoard
                                                (excali--read-file (expand-file-name "one.excalidraw" dir))))
                           (file-name-as-directory (file-truename dir))))
            (should-not (buffer-modified-p first))
            (let ((right (selected-window)))
              (should (= (length (window-list)) 2))
              (should (eq (window-buffer left) source))
              (should (> (car (window-edges right)) (car (window-edges left))))
              (should (<= (abs (- (window-total-width left)
                                 (window-total-width right))) 1))
              (should (eq (window-buffer right) first))
              ;; Repeating the command from the right pane does not split.
              (setq second (excali-board-new (expand-file-name "two.excalidraw" dir)))
              (should (eq (selected-window) right))
              (should (= (length (window-list)) 2))
              (should (buffer-live-p first))
              ;; Calling from Org reuses that same right-hand board pane.
              (select-window left)
              (setq third (excali-board-new (expand-file-name "three.excalidraw" dir)))
              (should (eq (selected-window) right))
              (should (eq (window-buffer left) source))
              (should (eq (window-buffer right) third))
              (should (= (length (window-list)) 2))))
        (dolist (b (list first second third))
          (when (buffer-live-p b) (kill-buffer b)))))))

(ert-deftest excali-board-new-failure-restores-source-window ()
  (excali-board-test--with
    (delete-other-windows)
    (switch-to-buffer source)
    (let ((left (selected-window)) (excali-board-vaults (list (cons "Default" dir))))
      (cl-letf (((symbol-function 'excali--open)
                 (lambda (&rest _) (user-error "No graphical canvas"))))
        (should-error (excali-board-new (expand-file-name "failure.excalidraw" dir)) :type 'user-error))
      (should (eq (selected-window) left))
      (should (eq (window-buffer left) source))
      (should (= (length (window-list)) 1)))))

(ert-deftest excali-board-new-does-not-overwrite-unrelated-right-pane ()
  (excali-board-test--with
    (delete-other-windows)
    (switch-to-buffer source)
    (let* ((left (selected-window))
           (right (split-window-right))
           (other (get-buffer-create " *board-unrelated-test*")))
      (unwind-protect
          (progn
            (set-window-buffer right other)
            (let ((window-min-width 5))
              (should-not (eq (excali-board--new-window) right)))
            (should (eq (window-buffer right) other))
            (should (eq (window-buffer left) source)))
        (kill-buffer other)))))

(provide 'excali-board-test)
;;; excali-board-test.el ends here

(ert-deftest excali-board-open-preserves-left-source-and-stored-vault ()
  (excali-board-test--with
    (let* ((path (expand-file-name "existing.excalidraw" dir))
           (doc (excali--empty-doc))
           (root (file-name-as-directory (file-truename dir)))
           (excali-board-vaults nil)
           opened)
      (setf (alist-get 'excaliBoard doc) `((version . 1) (vaultRoot . ,root)))
      (write-region (excali--serialize-doc doc nil) nil path nil 'silent)
      (delete-other-windows)
      (switch-to-buffer source)
      (let ((left (selected-window)))
        (unwind-protect
            (cl-letf (((symbol-function 'display-graphic-p) #'always)
                      ((symbol-function 'image-type-available-p) #'always)
                      ((symbol-function 'excali--sync-canvas) #'ignore))
              (setq opened (excali-board-open path))
              (should (= 2 (length (window-list))))
              (should (eq (window-buffer left) source))
              (should (> (car (window-edges (selected-window))) (car (window-edges left))))
              (should (equal root (with-current-buffer opened (excali-board-vault-root)))))
          (when (buffer-live-p opened) (kill-buffer opened)))))))
