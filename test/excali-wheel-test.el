;;; excali-wheel-test.el --- Wheel panning  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Events are shaped as the window systems make them: NS puts signed
;; pixel deltas on the event's axis, Windows reports horizontal amounts
;; on the y part, and legacy wheel buttons carry no delta at all.

(require 'ert)
(require 'excali)
(require 'excali-test)

(defun excali-wheel-test--event (name &optional delta)
  "Return a wheel event NAME with pixel DELTA (DX . DY)."
  (list name (excali-test--posn 50 50) 1 1 delta))

(ert-deftest excali-wheel-test-follows-emacs-scrolling ()
  "The canvas moves the way `mwheel-scroll' moves text, whatever the delta."
  (let ((mouse-wheel-flip-direction nil))
    (dolist (case '(;; NS: signed deltas on the event's axis.
                    (wheel-up (0.0 . 12.0) (0.0 . 12.0))
                    (wheel-down (0.0 . -12.0) (0.0 . -12.0))
                    (wheel-left (20.0 . 0.0) (-20.0 . 0.0))
                    (wheel-right (-20.0 . 0.0) (20.0 . 0.0))
                    ;; X / PGTK: the same names with the other signs.
                    (wheel-up (0.0 . -12.0) (0.0 . 12.0))
                    (wheel-left (-30.0 . 0.0) (-30.0 . 0.0))
                    ;; Windows: horizontal amounts come on y.
                    (wheel-left (0.0 . 36.0) (-36.0 . 0.0))
                    (wheel-right (0.0 . -36.0) (36.0 . 0.0))
                    ;; Legacy buttons: no delta, one step.
                    (wheel-up nil (0.0 . 40.0))
                    (wheel-right nil (40.0 . 0.0))))
      (should (equal (excali--wheel-delta
                      (excali-wheel-test--event (nth 0 case) (nth 1 case)))
                     (nth 2 case))))))

(ert-deftest excali-wheel-test-flip-and-shift ()
  "`mouse-wheel-flip-direction' swaps sideways; shift pans sideways."
  (let ((mouse-wheel-flip-direction t))
    (should (equal (excali--wheel-delta (excali-wheel-test--event 'wheel-left '(20.0 . 0.0)))
                   '(20.0 . 0.0)))
    (should (equal (excali--wheel-delta (excali-wheel-test--event 'wheel-up '(0.0 . 5.0)))
                   '(0.0 . 5.0))))
  (let ((mouse-wheel-flip-direction nil))
    (should (equal (excali--wheel-delta (excali-wheel-test--event 'S-wheel-down '(0.0 . -8.0)))
                   '(-8.0 . 0.0)))
    (should (equal (excali--wheel-delta (excali-wheel-test--event 'S-wheel-up nil))
                   '(40.0 . 0.0)))))

(ert-deftest excali-wheel-test-pans-and-zooms ()
  "Wheel events pan the view; with control they zoom."
  (excali-test--in-window
   (excali-mode)
   (setq excali--scroll-x 0.0 excali--scroll-y 0.0 excali--pan-remainder '(0.0 . 0.0))
   (should (eq (key-binding [S-wheel-down]) 'excali-wheel))
   (excali-wheel (excali-wheel-test--event 'wheel-up '(0.0 . 30.0)))
   (should (= excali--scroll-y 30.0))
   (excali-wheel (excali-wheel-test--event 'wheel-right '(0.0 . -20.0)))
   (should (= excali--scroll-x 20.0))
   (let ((zoom excali--zoom))
     (excali-wheel (excali-wheel-test--event 'C-wheel-up '(0.0 . 3.0)))
     (should (> excali--zoom zoom)))))

(provide 'excali-wheel-test)
;;; excali-wheel-test.el ends here
