;;; excali-board-vault-test.el --- Vault tests -*- lexical-binding: t; -*-
(require 'excali-board-test)

(ert-deftest excali-board-vault-recursive-bounded-discovery ()
  (excali-board-test--with
    (let* ((nested (expand-file-name "nested/deeper" dir))
           (outside (make-temp-file "excali-outside-" t)))
      (unwind-protect
          (progn
            (make-directory nested t)
            (write-region "image" nil (expand-file-name "same.png" nested) nil 'silent)
            (write-region "image" nil (expand-file-name "same.png" dir) nil 'silent)
            (write-region "* Outside" nil (expand-file-name "outside.org" outside) nil 'silent)
            (make-symbolic-link outside (expand-file-name "escape" dir))
            (make-symbolic-link dir (expand-file-name "loop" nested))
            (make-symbolic-link (expand-file-name "outside.org" outside)
                                (expand-file-name "external.org" dir))
            (excali-board-set-vault dir)
            (should (= 2 (length (excali-board-vault-files 'media))))
            (should (equal (list (file-truename file)) (excali-board-vault-files 'org)))
            (should (buffer-modified-p))
            (setq excali--file (expand-file-name "board.excalidraw" dir))
            (excali-board-save)
            (should (equal (alist-get 'vaultRoot (alist-get 'excaliBoard
                                                           (excali--read-file excali--file)))
                           (file-name-as-directory (file-truename dir)))))
        (delete-directory outside t)))))

(ert-deftest excali-board-vault-relative-picker-disambiguates ()
  (excali-board-test--with
    (make-directory (expand-file-name "sub" dir))
    (write-region "" nil (expand-file-name "sub/notes.org" dir) nil 'silent)
    (excali-board-set-vault dir)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_ choices &rest _)
                 (should (assoc "notes.org" choices))
                 (should (assoc "sub/notes.org" choices))
                 "sub/notes.org")))
      (should (equal (excali-board-vault-read-file "Org: " 'org)
                     (file-truename (expand-file-name "sub/notes.org" dir)))))))

(ert-deftest excali-board-vault-unavailable-never-falls-back ()
  (excali-board-test--with
    (should-not (excali-board-vault-root t))
    (setf (alist-get 'vaultRoot (alist-get 'excaliBoard excali--doc))
          (expand-file-name "missing" dir))
    (should-error (excali-board-vault-root t) :type 'user-error)
    (should-error (excali-board-vault-read-file "File: ") :type 'user-error)))

(ert-deftest excali-board-vault-resolves-source-and-dir-attachments ()
  (excali-board-test--with
    (excali-board-set-vault dir)
    (let ((marker (with-current-buffer source
                    (goto-char (point-min))
                    (org-entry-put nil "DIR" "./assets/")
                    (point-marker)))
          (org-attach-dir-relative t))
      (unwind-protect
          (progn
            (should (equal (excali-board-vault-resolve-file "pic.png" marker)
                           (expand-file-name "pic.png" dir)))
            (should (equal (excali-board-vault-resolve-file "pic.png" marker t)
                           (expand-file-name "assets/pic.png" dir)))
            (should (equal (excali-board-vault-resolve-file "nested/pic.png")
                           (expand-file-name "nested/pic.png" (file-truename dir))))
            (should-not (file-exists-p (expand-file-name "assets" dir)))
            (should-error (excali-board-vault-resolve-file "/ssh:host:/pic.png")
                          :type 'user-error))
        (set-marker marker nil)))))

(ert-deftest excali-board-vault-supports-default-id-attachments ()
  (excali-board-test--with
    (let ((marker (with-current-buffer source (goto-char (point-min)) (point-marker))))
      (unwind-protect
          (let ((expected (with-current-buffer source
                            (expand-file-name "image.png" (org-attach-dir nil t)))))
            (should (equal expected
                           (excali-board-vault-resolve-file "image.png" marker t))))
        (set-marker marker nil)))))

(ert-deftest excali-board-connect-save-reopen-and-undo ()
  (excali-board-test--with
    (let* ((one (excali-board-new-note "* One"))
           (two (excali-board-new-note "* Two")))
      (excali--select (list one two))
      (let* ((arrow (excali-board-connect "relates"))
             (id (excali--get arrow 'id))
             (start (excali--get one 'id))
             (end (excali--get two 'id)))
        (excali-undo)
        (should-not (excali--live-element-by-id id))
        (excali-redo)
        (setq arrow (excali--live-element-by-id id))
        (should (equal start (excali--binding-element-id arrow 'start)))
        (should (equal end (excali--binding-element-id arrow 'end)))
        (setq excali--file (expand-file-name "connections.excalidraw" dir))
        (excali-board-save)
        (let* ((doc (excali--restore-doc (excali--read-file excali--file)))
               (saved (seq-find (lambda (e) (equal (excali--get e 'id) id))
                                (alist-get 'elements doc))))
          (should saved)
          (should (equal start (excali--binding-element-id saved 'start)))
          (should (equal end (excali--binding-element-id saved 'end))))))))

(provide 'excali-board-vault-test)

(ert-deftest excali-board-vault-new-requires-configured-inside-context ()
  (excali-board-test--with
    (with-current-buffer source
      (let ((excali-board-vaults nil))
        (should-error (excali-board-new "never.excalidraw") :type 'user-error))
      (let ((excali-board-vaults (list (cons "Default" dir))))
        (should (equal (car (excali-board-vault--creation-context))
                       (file-name-as-directory (file-truename dir)))))
      (let ((excali-board-vaults (list (cons "Other" (make-temp-file "other-vault-" t)))))
        (unwind-protect
            (should-error (excali-board-new "never.excalidraw") :type 'user-error)
          (delete-directory (cdar excali-board-vaults)))))
    (should-not (file-exists-p (expand-file-name "never.excalidraw" dir)))))

(ert-deftest excali-board-vault-new-validates-destination-and-symlinks ()
  (excali-board-test--with
    (let ((outside (make-temp-file "outside-vault-" t))
          (root (file-name-as-directory (file-truename dir))))
      (unwind-protect
          (progn
            (make-symbolic-link outside (expand-file-name "escape" dir))
            (should-error (excali-board-vault--new-file "escape/board.excalidraw" root dir)
                          :type 'user-error)
            (should-error (excali-board-vault--new-file "../board.excalidraw" root dir)
                          :type 'user-error)
            (write-region "keep" nil (expand-file-name "exists.excalidraw" dir) nil 'silent)
            (should-error (excali-board-vault--new-file "exists.excalidraw" root dir)
                          :type 'user-error)
            (make-symbolic-link (expand-file-name "missing" outside)
                                (expand-file-name "broken.excalidraw" dir))
            (should-error (excali-board-vault--new-file "broken.excalidraw" root dir)
                          :type 'user-error)
            (make-directory (expand-file-name "nested" dir))
            (should (equal (expand-file-name "nested/new.excalidraw" dir)
                           (excali-board-vault--new-file "nested/new" root dir))))
        (delete-directory outside t)))))

(ert-deftest excali-board-vault-context-does-not-follow-outside-symlink ()
  (excali-board-test--with
    (let* ((outside (make-temp-file "outside-vault-" t))
           (excali-board-vaults (list (cons "Default" dir)))
           (link (expand-file-name "escape" dir)))
      (unwind-protect
          (progn
            (make-symbolic-link outside link)
            (with-temp-buffer
              (setq default-directory (file-name-as-directory link))
              (should-error (excali-board-vault--creation-context) :type 'user-error))
            (with-temp-buffer
              (setq default-directory (file-name-as-directory dir))
              (should (excali-board-vault--creation-context))))
        (delete-directory outside t)))))

(ert-deftest excali-board-vault-multiple-automatic-and-no-implicit-root ()
  (excali-board-test--with
    (let* ((a (expand-file-name "personal" dir))
           (b (expand-file-name "work" dir))
           (excali-board-vaults (list (cons "Personal" a) (cons "Work" b))))
      (make-directory a) (make-directory b)
      (dolist (directory (list a b))
        (with-temp-buffer
          (setq default-directory (file-name-as-directory directory))
          (should (equal (file-name-as-directory (file-truename directory))
                         (car (excali-board-vault--creation-context))))))
      (with-current-buffer source
        (should-error (excali-board-vault--creation-context) :type 'user-error)
        (let ((excali-board-vaults nil))
          (should-error (excali-board-vault--creation-context) :type 'user-error))))))

(ert-deftest excali-board-vault-nested-deepest-independent-of-order ()
  (excali-board-test--with
    (let* ((nested (expand-file-name "work" dir))
           (deeper (expand-file-name "projects" nested))
           (parent (file-name-as-directory (file-truename dir)))
           (excali-board-vaults (list (cons "All" dir) (cons "Work" nested))))
      (make-directory deeper t)
      (with-temp-buffer
        (setq default-directory (file-name-as-directory deeper))
        (dotimes (_ 2)
          (should (equal (file-name-as-directory (file-truename nested))
                         (car (excali-board-vault--creation-context))))
          (setq excali-board-vaults (reverse excali-board-vaults))))
      ;; A parent-vault board cannot be saved into the nested vault.
      (should-error (excali-board-vault--new-file "work/new" parent dir) :type 'user-error))))

(ert-deftest excali-board-vault-multiple-invalid-config-is-not-silently-ignored ()
  (excali-board-test--with
    (dolist (bad (list (cons "Missing" (expand-file-name "missing" dir))
                      '("Remote" . "/ssh:host:/notes/") '("Relative" . "notes") '("Bad" . 7)))
      (let ((excali-board-vaults (list (cons "Valid" dir) bad)))
        (should-error (excali-board-vault--configured-roots) :type 'user-error)))))

(ert-deftest excali-board-vault-list-does-not-change-existing-board-root ()
  (excali-board-test--with
    (excali-board-set-vault dir)
    (let ((excali-board-vaults '(("Elsewhere" . "/unavailable-vault/"))))
      (should (equal (file-name-as-directory (file-truename dir))
                     (excali-board-vault-root)))
      (should (member (file-truename file) (excali-board-vault-files 'org))))))
