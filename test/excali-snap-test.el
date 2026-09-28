;;; excali-snap-test.el --- Grid and object snapping  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)
(require 'excali-test)

(defmacro excali-snap-test--scene (&rest body)
  "Run BODY in a window-backed scene with snapping reset."
  `(excali-test--in-window
    (excali--load-current-style nil)
    (setq excali--grid-enabled nil excali--grid-size 20 excali--grid-step 5
          excali--objects-snap-enabled nil excali--snap-lines nil
          excali--tool-locked nil excali--multi-element nil
          excali--canvas-size '(400 . 300))
    ,@body))

(ert-deftest excali-snap-test-grid-points ()
  "Grid points round to the nearest multiple; super suppresses it."
  (excali-snap-test--scene
   (should (equal (excali--grid-point '(29.0 . 31.0)) '(29.0 . 31.0)))
   (setq excali--grid-enabled t)
   (should (equal (excali--grid-point '(29.0 . 31.0)) '(20.0 . 40.0)))
   (should (equal (excali--grid-point '(29.0 . 31.0) t) '(29.0 . 31.0)))))

(ert-deftest excali-snap-test-grid-create-move-nudge ()
  "New shapes, drags and arrow keys follow the grid."
  (excali-snap-test--scene
   (setq excali--grid-enabled t excali--tool 'rectangle)
   (excali-test--drag 23 18 67 55)
   (let ((r (car (last excali--elements))))
     (should (equal (list (excali--get r 'x) (excali--get r 'y)
                          (excali--get r 'width) (excali--get r 'height))
                    '(20.0 20.0 40.0 40.0)))
     ;; Dragging moves the top-left corner onto grid lines.
     (excali-test--drag 40 40 53 47)
     (should (equal (list (excali--get r 'x) (excali--get r 'y)) '(40.0 20.0)))
     (excali-nudge-right)
     (should (= (excali--get r 'x) 60.0))
     (excali-nudge-right-large)
     (should (= (excali--get r 'x) 61.0)))))

(ert-deftest excali-snap-test-objects ()
  "Moving near another element's edge aligns to it and draws a snap line."
  (excali-snap-test--scene
   (setq excali--objects-snap-enabled t)
   (let ((fixed (excali-test--rect 100 100))
         (moving (excali-test--rect 0 0)))
     (setq excali--elements (list fixed moving))
     ;; Left edges and centers both end up 1 apart: both snap at dx 100.
     (let ((d (excali--snap-move (list moving) 99 50 nil)))
       (should (equal d '(100.0 . 50))))
     (should excali--snap-lines)
     ;; Left edges, centers and right edges all align: one line each.
     (should (= (length excali--snap-lines) 3))
     (should (seq-some (lambda (line) (seq-every-p (lambda (p) (= (car p) 100.0)) line))
                       excali--snap-lines))
     ;; Far away nothing snaps.
     (should (equal (excali--snap-move (list moving) 50 50 nil) '(50 . 50)))
     (should-not excali--snap-lines)
     ;; Super inverts the setting.
     (should (equal (excali--snap-move (list moving) 99 50 t) '(99 . 50))))))

(ert-deftest excali-snap-test-overlays ()
  "The grid and snap lines become overlays; the grid draws gray lines."
  (excali-snap-test--scene
   (setq excali--grid-enabled t excali--snap-lines (list '((0.0 . 0.0) (0.0 . 50.0))))
   (let ((kinds (mapcar (lambda (v) (aref v 0)) (excali--overlay-natives))))
     (should (member "ov-grid" kinds))
     (should (>= (seq-count (lambda (k) (equal k "ov-poly")) kinds) 5)))
   (setq excali--snap-lines nil)
   (let ((fb (excali-native-fb-create 100 100))
         (white (excali-native-fb-create 100 100)))
     (excali-native-fb-render white 1.0 1.0 0.0 0.0 [] nil)
     (excali-native-fb-render fb 1.0 1.0 0.0 0.0 (excali--visible-elements) nil)
     (should (> (excali-native-fb-diff fb white) 20)))))

(provide 'excali-snap-test)
;;; excali-snap-test.el ends here
