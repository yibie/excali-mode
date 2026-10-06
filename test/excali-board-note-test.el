;;; excali-board-note-test.el --- Independent card tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'excali-board-test)

(ert-deftest excali-board-note-roundtrip ()
  (excali-board-test--with
   (let* ((text "* Ideas\n- [ ] Try it\n** Detail\nFull content.\n")
          (card (excali-board-new-note text)))
     (should (excali-board-note-p card))
     (should-not (excali-board--reference card))
     (should (= 220 (excali--get card 'height)))
     (should (equal text (excali-board-note-text card)))
     (should (eq card (excali-board--selected-card)))
     (setq excali--file (expand-file-name "note.excalidraw" dir))
     (excali-board-save)
     (let ((saved (aref (alist-get 'elements (excali--read-file excali--file)) 0)))
       (should (equal text (excali-board-note-text saved)))
       (should (equal (excali-board--blocks saved) (excali-board--blocks card))))
     (with-current-buffer source (should-not (buffer-modified-p))))))

(ert-deftest excali-board-note-editor-live-and-undo ()
  (excali-board-test--with
   (let* ((card (excali-board-new-note "Initial"))
          (id (excali--get card 'id))
          (editor (excali-board--make-editor card)))
     (unwind-protect
         (with-current-buffer editor
           (should-not (buffer-base-buffer))
           (goto-char (point-max)) (insert " edited")
           (should (equal "Initial edited"
                          (with-current-buffer board
                            (excali-board-note-text (excali--live-element-by-id id)))))
           (should-not (buffer-modified-p)))
       (with-current-buffer editor (excali-board-edit-finish)))
     (excali-undo)
     (should (equal "Initial" (excali-board-note-text (excali--live-element-by-id id))))
     (excali-redo)
     (should (equal "Initial edited" (excali-board-note-text (excali--live-element-by-id id)))))))

(ert-deftest excali-board-note-empty-renders-placeholder ()
  (excali-board-test--with
   (let ((card (excali-board-new-note "")))
     (should (excali-board-note-p card))
     (should (> (length (excali-board--blocks card)) 0))
     (should (= 0 (aref (excali--native-element (excali--bound-text-of card)) 15))))))

(ert-deftest excali-board-note-conversion-preserves-layout-and-content ()
  (excali-board-test--with
   (let* ((text "A paragraph.\n* A child\nNested.\n#+begin_src text\n,* not a heading\n#+end_src\n")
          (card (excali-board-new-note text))
          (id (excali--get card 'id)))
     (excali--put card 'x 321.0)
     (excali--put card 'width 444.0)
     (excali--put card 'strokeColor "#aabbcc")
     (excali--commit)
     (excali-board-note-to-heading file "Captured")
     (should-not (excali-board-note-p card))
     (should (equal id (excali--get card 'id)))
     (should (= 321 (excali--get card 'x)))
     (should (= 444 (excali--get card 'width)))
     (should (equal "#aabbcc" (excali--get card 'strokeColor)))
     (let ((marker (excali-board--resolve (excali-board--reference card))))
       (with-current-buffer source
         (should (buffer-modified-p))
         (goto-char marker)
         (should (equal "Captured" (org-get-heading t t t t)))
         (should (string-match-p "\\*\\* A child" (buffer-string)))
         (should (string-match-p "#\\+begin_src text\n,\\* not a heading" (buffer-string))))
       (set-marker marker nil))
     (with-temp-buffer
       (insert-file-contents file)
       (should-not (string-match-p "Captured" (buffer-string))))
     (excali-undo)
     (should (equal text (excali-board-note-text (excali--live-element-by-id id))))
     ;; Undo must never delete the new heading from a different buffer.
     (with-current-buffer source (should (string-match-p "Captured" (buffer-string)))))))

(ert-deftest excali-board-note-conversion-rolls-back-on-error ()
  (excali-board-test--with
   (let* ((card (excali-board-new-note "Keep me"))
          (before (copy-tree card t))
          (org-before (with-current-buffer source (buffer-string))))
     (cl-letf (((symbol-function 'excali-board--refresh-card)
                (lambda (_) (error "Injected renderer failure"))))
       (should-error (excali-board-note-to-heading file "Failed")))
     (should (equal before card))
     (with-current-buffer source
       (should (equal org-before (buffer-string)))
       (should-not (buffer-modified-p)))
     (should (equal "Keep me" (excali-board-note-text card))))))

(ert-deftest excali-board-note-readonly-conversion-preserves-note ()
  (excali-board-test--with
   (let ((card (excali-board-new-note "Keep me")))
     (with-current-buffer source (setq buffer-read-only t))
     (should-error (excali-board-note-to-heading file "Failed") :type 'buffer-read-only)
     (should (excali-board-note-p card))
     (should-not (excali-board--reference card)))))

(ert-deftest excali-board-note-stale-editor-keeps-recovery-text ()
  (excali-board-test--with
   (let* ((card (excali-board-new-note "Original"))
          (editor (excali-board--make-editor card)))
     (unwind-protect
         (progn
           (excali-board-note--set card "Changed elsewhere")
           (with-current-buffer editor
             (goto-char (point-max)) (insert " my edit")
             (should-error (excali-board-note--sync) :type 'user-error)
             (should-not (excali-board-note--can-kill))
             (should (string-match-p "my edit" (buffer-string)))))
       ;; This test explicitly discards its recovery buffer.
       (with-current-buffer editor
         (setq excali-board-note--id nil)
         (kill-buffer editor))))))

(ert-deftest excali-board-note-editor-save-targets-board ()
  (excali-board-test--with
   (let* ((card (excali-board-new-note "Owned"))
          (editor (excali-board--make-editor card))
          (saved nil))
     (unwind-protect
         (with-current-buffer editor
           (cl-letf (((symbol-function 'excali-board-save)
                      (lambda () (setq saved (current-buffer)))))
             (excali-board-edit-save))
           (should (eq saved board)))
       (with-current-buffer editor (excali-board-edit-finish))))))

(ert-deftest excali-board-note-narrowed-editor-preserves-other-content ()
  (excali-board-test--with
   (let* ((card (excali-board-new-note "* One\nFirst.\n* Two\nSecond.\n"))
          (editor (excali-board--make-editor card)))
     (unwind-protect
         (with-current-buffer editor
           (goto-char (point-min))
           (org-narrow-to-subtree)
           (goto-char (point-max)) (insert "Edited.\n")
           (should (string-match-p "Second." (excali-board-note-text card)))
           (should (string-match-p "Edited." (excali-board-note-text card))))
       (with-current-buffer editor (excali-board-edit-finish))))))

(ert-deftest excali-board-note-conversion-keeps-bound-relationship ()
  (excali-board-test--with
   (let* ((one (excali-board-new-note "Convert me"))
          (two (excali-board-new-note "Keep independent")))
     (excali--select (list one two))
     (let* ((arrow (excali-board-connect "relates to"))
            (binding (copy-tree (excali--get arrow 'startBinding) t)))
       (excali--select (list one))
       (excali-board-note-to-heading file "Linked idea")
       (should (equal binding (excali--get arrow 'startBinding)))
       (should (equal "relates to"
                      (excali--get (excali--bound-text-of arrow) 'originalText)))
       (should (excali-board-note-p two))))))


(ert-deftest excali-board-note-conversion-reuses-title-without-prompt ()
  (excali-board-test--with
    (let ((card (excali-board-new-note "* Existing title\nBody\n** Detail\nChild\n")))
      (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) file))
                ((symbol-function 'read-string)
                 (lambda (&rest _) (ert-fail "Unexpected title prompt"))))
        (call-interactively #'excali-board-note-to-heading))
      (let ((marker (excali-board--resolve (excali-board--reference card))))
        (unwind-protect
            (with-current-buffer source
              (goto-char marker)
              (should (equal (org-get-heading t t t t) "Existing title"))
              (org-narrow-to-subtree)
              (should (= 1 (how-many "^\\*+ Existing title" (point-min) (point-max))))
              (should (string-match-p "^\\*\\* Detail" (buffer-string)))
              (should (string-match-p "Body" (buffer-string)))
              (widen))
          (set-marker marker nil))))))

(ert-deftest excali-board-note-heading-text-keeps-structure ()
  (should (equal (excali-board-note--heading-text
                  "** First\n*** Child\n** Second\n*** Detail\n")
                 "* First\n** Child\n** Second\n*** Detail\n"))
  (should (equal (excali-board-note--heading-text "")
                 "* New note\n"))
  (should (equal (excali-board-note--heading-text "Plain title\nBody")
                 "* Plain title\nPlain title\nBody"))
  (should (equal (excali-board-note--heading-text
                  "* TODO Plan :work:\n:PROPERTIES:\n:CUSTOM_ID: keep\n:END:\nText\n")
                 "* TODO Plan :work:\n:PROPERTIES:\n:CUSTOM_ID: keep\n:END:\nText\n")))

(provide 'excali-board-note-test)
;;; excali-board-note-test.el ends here
