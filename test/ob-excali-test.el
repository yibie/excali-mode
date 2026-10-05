;;; ob-excali-test.el --- Babel integration regressions -*- lexical-binding: t; -*-
;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'ert)
(require 'cl-lib)
(require 'ob-excali)
(require 'excali)
(defvar org-babel-confirm-evaluate-answer-no)

(defconst ob-excali-test--body
  "node app \"Application\"\nnode app.api \"API\"\nnode db \"Database\" right of app level with app.api\nedge app.api -> db \"query\" from: right to: left")

(defmacro ob-excali-test--with-org (headers &rest body)
  "Run BODY in an Org buffer containing an excali block with HEADERS."
  (declare (indent 1))
  `(let ((directory (make-temp-file "ob-excali-test-" t)))
     (unwind-protect
         (with-temp-buffer
           (setq default-directory (file-name-as-directory directory)
                 buffer-file-name (expand-file-name "diagrams.org" directory))
           (org-mode)
           (insert "#+name: application\n#+begin_src excali " ,headers "\n"
                   ob-excali-test--body "\n#+end_src\n")
           (goto-char (point-min))
           (forward-line 1)
           ;; Local test binding only; the adapter never changes this setting.
           (let ((org-confirm-babel-evaluate nil)
                 (excali-export-embed-scene t))
             ,@body))
       (delete-directory directory t))))

(ert-deftest ob-excali-formats-and-file-results ()
  (dolist (ext '("svg" "png" "excalidraw"))
    (ob-excali-test--with-org (concat ":file application." ext)
      (let* ((file (expand-file-name (concat "application." ext)))
             (result (org-babel-execute-src-block)))
        (should (file-exists-p file))
        (should (> (file-attribute-size (file-attributes file)) 100))
        (should (stringp result))
        (should (string-match-p "#\\+RESULTS: application" (buffer-string)))
        (should (string-match-p (regexp-quote (concat "[[file:application." ext "]]")) (buffer-string)))
        (let ((doc (excali--read-scene-file file)))
          (should (equal (alist-get 'type doc) "excalidraw"))
          (should (= (length (alist-get 'elements doc)) 8)))
        (when (equal ext "png")
          (with-temp-buffer
            (set-buffer-multibyte nil) (insert-file-contents-literally file)
            (should (string-prefix-p "\x89PNG\r\n\x1a\n" (buffer-string)))))
        (when (equal ext "svg")
          (with-temp-buffer (insert-file-contents file)
                            (should (search-forward "<svg" nil t))))))))

(ert-deftest ob-excali-dir-mkdirp-and-result-link ()
  (ob-excali-test--with-org ":dir work :file images/app.svg :mkdirp yes :results file graphics"
    (org-babel-execute-src-block)
    (should (file-exists-p (expand-file-name "work/images/app.svg" directory)))
    (goto-char (point-min))
    (re-search-forward "\\[\\[file:\\([^]]+\\)\\]\\]")
    (should (file-equal-p (expand-file-name (match-string 1) directory)
                         (expand-file-name "work/images/app.svg" directory)))))

(ert-deftest ob-excali-existing-result-preserved-on-error ()
  (ob-excali-test--with-org ":file application.svg"
    (org-babel-execute-src-block)
    (let ((before (with-temp-buffer (insert-file-contents "application.svg") (buffer-string)))
          (old-result (save-excursion (goto-char (point-max))
                                     (search-backward "#+RESULTS:")
                                     (buffer-substring-no-properties (point) (point-max)))))
      (goto-char (point-min))
      (search-forward "node db") (beginning-of-line)
      (delete-region (point) (line-end-position))
      (insert "node db right of missing")
      (goto-char (point-min)) (forward-line 1)
      (let ((err (should-error (org-babel-execute-src-block) :type 'user-error)))
        (should (string-match-p "diagrams.org, block starting at line 2" (error-message-string err)))
        (should (string-match-p "line 3, column" (error-message-string err))))
      (should (equal before (with-temp-buffer (insert-file-contents "application.svg") (buffer-string))))
      (should (string-suffix-p old-result (buffer-string)))
      (should-not (directory-files directory nil "^\\.ob-excali-")))))

(ert-deftest ob-excali-export-failure-is-atomic ()
  (ob-excali-test--with-org ":file application.svg"
    (with-temp-file "application.svg" (insert "keep previous file"))
    (cl-letf (((symbol-function 'excali-export-svg)
               (lambda (file &rest _)
                 (with-temp-file file (insert "partial render"))
                 (error "Render failed"))))
      (should-error (org-babel-execute-src-block)))
    (should (equal "keep previous file"
                   (with-temp-buffer (insert-file-contents "application.svg") (buffer-string))))
    (should-not (directory-files directory nil "^\\.ob-excali-"))))

(ert-deftest ob-excali-missing-output-directory ()
  (ob-excali-test--with-org ":file missing/app.svg"
    (should-error (org-babel-execute-src-block) :type 'user-error)
    (should-not (file-exists-p "missing"))))

(ert-deftest ob-excali-invalid-headers ()
  (dolist (params '(nil ((:file . "")) ((:file . "app.pdf"))
                    ((:file . "/ssh:host:/tmp/a.svg"))
                    ((:file . "app.svg") (:session . "diagram"))
                    ((:file . "app.svg") (:var . (x . 1)))
                    ((:file . "app.svg") (:cmdline . "--run"))
                    ((:file . "app.svg") (:prologue . "anything"))
                    ((:file . "app.svg") (:file-mode . "invalid"))
                    ((:file . "app.svg") (:result-params . ("scalar")))))
    (should-error (org-babel-execute:excali ob-excali-test--body params) :type 'user-error)))

(ert-deftest ob-excali-confirmation-is-respected ()
  (ob-excali-test--with-org ":file application.svg"
    (let* (asked
           (org-confirm-babel-evaluate
            (lambda (language _body) (setq asked language) t))
           ;; Org's noninteractive "no" response, also used by async export.
           (org-babel-confirm-evaluate-answer-no t))
      (should-not (org-babel-execute-src-block))
      (should (equal asked "excali"))
      (should-not (file-exists-p "application.svg")))))

(ert-deftest ob-excali-eval-never ()
  (ob-excali-test--with-org ":file application.svg :eval never"
    (org-babel-execute-src-block)
    (should-not (file-exists-p "application.svg"))))

(ert-deftest ob-excali-org-edit-special ()
  (ob-excali-test--with-org ":file application.svg"
    (let ((source (current-buffer)) edit)
      (save-window-excursion
        (unwind-protect
            (progn
              (switch-to-buffer source)
              (org-edit-special)
              (setq edit (current-buffer))
              (should (derived-mode-p 'excali-dsl-mode))
              (should org-src-mode)
              (should (string-match-p "node app" (buffer-string)))
              (org-edit-src-exit)
              (setq edit nil))
          (when (buffer-live-p edit) (kill-buffer edit)))))))

(ert-deftest ob-excali-rerun-replaces-result ()
  (ob-excali-test--with-org ":file application.excalidraw"
    (org-babel-execute-src-block)
    (goto-char (point-min)) (forward-line 1)
    (org-babel-execute-src-block)
    (goto-char (point-min))
    (let ((count 0))
      (while (re-search-forward "^#\\+RESULTS:" nil t) (cl-incf count))
      (should (= count 1)))))

(defconst ob-excali-test--root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))

(ert-deftest ob-excali-cold-load-and-execution ()
  (let ((form
         '(progn
            (require 'cl-lib)
            (require 'org)
            (org-babel-do-load-languages 'org-babel-load-languages '((excali . t)))
            (cl-assert (not (featurep 'excali-core)))
            (cl-assert (eq (cdr (assoc "excali" org-src-lang-modes)) 'excali-dsl))
            (let ((directory (make-temp-file "ob-excali-cold-" t)))
              (unwind-protect
                  (with-temp-buffer
                    (setq default-directory (file-name-as-directory directory))
                    (org-mode)
                    (insert "#+begin_src excali :file cold.svg\nnode a\n#+end_src\n")
                    (goto-char (point-min))
                    (let ((org-confirm-babel-evaluate nil)) (org-babel-execute-src-block))
                    (cl-assert (file-exists-p "cold.svg")))
                (delete-directory directory t))))))
    (with-temp-buffer
      (let ((status (call-process (expand-file-name invocation-name invocation-directory)
                                 nil t nil "-Q" "--batch" "-L" ob-excali-test--root
                                 "--eval" (prin1-to-string form))))
        (ert-info ((buffer-string)) (should (equal status 0)))))))

(ert-deftest ob-excali-example-document ()
  (let ((directory (make-temp-file "ob-excali-example-" t)))
    (unwind-protect
        (with-temp-buffer
          (setq default-directory (file-name-as-directory directory))
          (insert-file-contents (expand-file-name "examples/org-babel/application.org" ob-excali-test--root))
          (org-mode)
          (let ((org-confirm-babel-evaluate nil)) (org-babel-execute-buffer))
          (dolist (file '("output/application.svg" "output/flow.png" "output/editable.excalidraw"))
            (should (file-exists-p file))))
      (delete-directory directory t))))

(ert-deftest ob-excali-file-mode ()
  (ob-excali-test--with-org ":file application.svg :file-mode (identity #o640)"
    (org-babel-execute-src-block)
    (should (= (logand (file-modes "application.svg") #o777) #o640))))

(provide 'ob-excali-test)
;;; ob-excali-test.el ends here
