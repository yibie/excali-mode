;;; excali-board-reference-test.el --- Reference safety tests -*- lexical-binding: t; -*-
(require 'excali-board-test)

(ert-deftest excali-board-reference-repairs-renamed-file ()
  (excali-board-test--with
    (excali-board-set-vault dir)
    (let* ((card (excali-board--insert ref "Old"))
           (id (excali--get card 'id))
           (target (expand-file-name "nested/moved.org" dir)))
      (make-directory (file-name-directory target))
      (rename-file file target)
      (should (= 1 (excali-board-repair-references)))
      (should (equal (file-truename target) (alist-get 'file (excali-board--reference card))))
      (should (equal id (excali--get card 'id)))
      (should-not excali-board--errors)
      (should (buffer-modified-p))
      (let ((opened (find-buffer-visiting target)))
        (when opened (kill-buffer opened))))))

(ert-deftest excali-board-reference-repairs-heading-moved-to-other-file ()
  (excali-board-test--with
    (excali-board-set-vault dir)
    (let ((card (excali-board--insert ref "Old"))
          (target (expand-file-name "moved.org" dir)))
      (with-current-buffer source
        (write-region (point-min) (point-max) target nil 'silent)
        (erase-buffer) (insert "* Other\n") (save-buffer))
      (should (= 1 (excali-board-repair-references)))
      (should (equal (file-truename target) (alist-get 'file (excali-board--reference card))))
      (kill-buffer (find-buffer-visiting target)))))

(ert-deftest excali-board-reference-refuses-duplicate-id ()
  (excali-board-test--with
    (excali-board-set-vault dir)
    (let ((card (excali-board--insert ref "Old")))
      (copy-file file (expand-file-name "duplicate.org" dir))
      (should (= 0 (excali-board-repair-references)))
      (should (equal file (alist-get 'file (excali-board--reference card))))
      (should (string-match-p "Duplicate" (cdar excali-board--errors))))))

(ert-deftest excali-board-reference-external-clean-source-refreshes ()
  (excali-board-test--with
    (let ((card (excali-board--insert ref "Old")))
      (excali-board--refresh-card card)
      (with-temp-file file
        (insert "* External title\n:PROPERTIES:\n:ID: board-heading\n:END:\nNew disk text longer.\n"))
      (with-current-buffer source (set-visited-file-modtime '(0 0 0 0)))
      (excali-board-reference--poll)
      (should (string-match-p "New disk text"
                              (alist-get 'sourceText
                                         (alist-get 'excaliBoardContent
                                                    (excali--get card 'customData)))))
      (should-not (buffer-modified-p source)))))

(ert-deftest excali-board-reference-conflict-preserves-buffer-and-cache ()
  (excali-board-test--with
    (let ((card (excali-board--insert ref "Old")))
      (excali-board--refresh-card card)
      (let ((cache (copy-tree (excali--get card 'customData) t)))
        (with-current-buffer source (goto-char (point-max)) (insert "Unsaved edit"))
        (with-temp-file file (insert "* Disk replacement\n"))
        (with-current-buffer source (set-visited-file-modtime '(0 0 0 0)))
        (excali-board-reference--poll)
        (should (buffer-modified-p source))
        (should (with-current-buffer source (string-match-p "Unsaved edit" (buffer-string))))
        (should (equal cache (excali--get card 'customData)))
        (should (string-match-p "conflicts" (cdar excali-board--errors)))))))

(ert-deftest excali-board-reference-index-is-vault-bounded ()
  (excali-board-test--with
    (let ((inner (expand-file-name "inner" dir)))
      (make-directory inner)
      (excali-board-set-vault inner)
      (should-not (gethash "board-heading" (excali-board-reference--id-index))))))

(ert-deftest excali-board-reference-backlinks-prefer-live-state ()
  (excali-board-test--with
    (excali-board-set-vault dir)
    (let ((card (excali-board--insert ref "Old")))
      (setq excali--file (expand-file-name "board.excalidraw" dir))
      (excali-board-save)
      (let ((hits (excali-board-reference--backlinks "board-heading" (excali-board-vault-root))))
        (should (= 1 (length hits)))
        (should (eq board (cadar hits))))
      (excali--put card 'isDeleted t)
      (should-not (excali-board-reference--backlinks "board-heading" (excali-board-vault-root))))))

(ert-deftest excali-board-reference-backlinks-read-closed-board ()
  (excali-board-test--with
    (excali-board-set-vault dir)
    (excali-board--insert ref "Old")
    (setq excali--file (expand-file-name "board.excalidraw" dir))
    (excali-board-save)
    (cl-letf (((symbol-function 'excali-board--boards) (lambda () nil)))
      (let ((hits (excali-board-reference--backlinks "board-heading" (excali-board-vault-root))))
        (should (= 1 (length hits)))
        (should-not (cadar hits))))))

(ert-deftest excali-board-reference-duplicate-id-in-same-file ()
  (excali-board-test--with
    (excali-board-set-vault dir)
    (excali-board--insert ref "Old")
    (with-current-buffer source
      (goto-char (point-max))
      (insert "* Duplicate\n:PROPERTIES:\n:ID: board-heading\n:END:\n"))
    (should (= 0 (excali-board-repair-references)))
    (should (string-match-p "Duplicate" (cdar excali-board--errors)))))

(ert-deftest excali-board-reference-missing-dirty-source-is-not-detached ()
  (excali-board-test--with
    (excali-board-set-vault dir)
    (let ((card (excali-board--insert ref "Old"))
          (target (expand-file-name "moved.org" dir)))
      (with-current-buffer source (goto-char (point-max)) (insert "Unsaved recovery"))
      (rename-file file target)
      (should (= 0 (excali-board-repair-references)))
      (should (equal file (alist-get 'file (excali-board--reference card))))
      (should (with-current-buffer source
                (string-match-p "Unsaved recovery" (buffer-string))))
      (should excali-board--errors))))

(ert-deftest excali-board-reference-last-board-stops-timer ()
  (excali-board-test--with
    (should (timerp excali-board-reference--timer))
    (cl-letf (((symbol-function 'excali-board--boards) (lambda () (list board))))
      (excali-board-reference--stop)
      (should-not excali-board-reference--timer))))

(provide 'excali-board-reference-test)
