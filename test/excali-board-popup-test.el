;;; excali-board-popup-test.el --- Fixed editor popup tests -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'excali-board-test)

(ert-deftest excali-board-popup-geometry-ignores-canvas ()
  (let ((native-comp-enable-subr-trampolines nil))
   (cl-letf (((symbol-function 'frame-native-width) (lambda (_) 1000))
            ((symbol-function 'frame-native-height) (lambda (_) 800)))
    (let ((excali--zoom 0.1) (excali--scroll-x -9000) (excali--scroll-y 7000))
      (should (equal (excali-board--popup-rect nil) '(140 160 720 480)))
      (setq excali--zoom 8 excali--scroll-x 123 excali--scroll-y -456)
      (should (equal (excali-board--popup-rect nil) '(140 160 720 480)))))))

(ert-deftest excali-board-popup-small-parent ()
  (let ((native-comp-enable-subr-trampolines nil))
   (cl-letf (((symbol-function 'frame-native-width) (lambda (_) 400))
            ((symbol-function 'frame-native-height) (lambda (_) 300)))
    (should (equal (excali-board--popup-rect nil) '(24 40 352 220))))))

(ert-deftest excali-board-popup-expand-preserves-note-buffer ()
  (excali-board-test--with
    (switch-to-buffer board)
    (let* ((origin (selected-window))
           (card (excali-board-new-note "* Note\nBody\n"))
           (editor (excali-board--make-editor card)))
      (unwind-protect
          (with-current-buffer editor
            (setq excali-board--editor-window origin)
            (goto-char (point-max))
            (insert "More")
            (let ((pos (point)) (history buffer-undo-list))
              (excali-board-edit-expand)
              (should (eq editor (window-buffer (selected-window))))
              (should (= pos (point)))
              (should (eq history buffer-undo-list))
              (should-not excali-board--editing-frame)
              (should (equal "* Note\nBody\nMore" (excali-board-note-text card))))
            (excali-board-edit-finish)
            (should-not (buffer-live-p editor))
            (should (eq (selected-window) origin))
            (should (eq (window-buffer origin) board)))
        (when (buffer-live-p editor)
          (with-current-buffer editor (excali-board-edit-finish)))))))

(ert-deftest excali-board-popup-expand-preserves-indirect-subtree ()
  (excali-board-test--with
    (switch-to-buffer board)
    (let* ((card (excali-board--insert ref "Old"))
           (editor (excali-board--make-editor card)))
      (unwind-protect
          (with-current-buffer editor
            (let ((start (point-min)) (end (point-max)))
              (excali-board-edit-expand)
              (should (eq (buffer-base-buffer) source))
              (should (= start (point-min)))
              (should (= end (point-max)))
              (should (buffer-narrowed-p))
              (should (eq (key-binding (kbd "C-c C-e")) #'excali-board-edit-expand))
              (should (eq (key-binding (kbd "C-c C-c")) #'excali-board-edit-finish))))
        (when (buffer-live-p editor)
          (with-current-buffer editor (excali-board-edit-finish)))))))

(provide 'excali-board-popup-test)
;;; excali-board-popup-test.el ends here
