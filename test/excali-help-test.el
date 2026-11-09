;;; excali-help-test.el --- The keyboard shortcuts panel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)

(defun excali-help-test--suffixes (layout)
  "Return (KEY . COMMAND) for every suffix in the transient LAYOUT."
  (cond ((and (consp layout) (symbolp (car layout))
              (plist-get (cdr layout) :key) (plist-get (cdr layout) :command))
         (list (cons (plist-get (cdr layout) :key) (plist-get (cdr layout) :command))))
        ((or (consp layout) (vectorp layout))
         (mapcan #'excali-help-test--suffixes (append layout nil)))))

(ert-deftest excali-help-test-keys-match-the-mode-map ()
  "Each key in the panel runs, on the canvas, the command it shows."
  (let ((suffixes (excali-help-test--suffixes (get 'excali-help 'transient--layout))))
    (should (> (length suffixes) 40))
    (pcase-dolist (`(,key . ,command) suffixes)
      (unless (eq command 'describe-mode) ; `?' again: the full list.
        (should (equal (list key (keymap-lookup excali-mode-map key))
                       (list key command)))))))

(ert-deftest excali-help-test-question-mark-opens-it ()
  "? opens the panel."
  (should (eq (keymap-lookup excali-mode-map "?") 'excali-help)))

(provide 'excali-help-test)
;;; excali-help-test.el ends here
