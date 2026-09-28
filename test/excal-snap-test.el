;;; excal-snap-test.el --- Grid and object snapping  -*- lexical-binding: t; -*-

(require 'ert)
(require 'excal)
(require 'excal-test)

(defmacro excal-snap-test--scene (&rest body)
  "Run BODY in a window-backed scene with snapping reset."
  `(excal-test--in-window
    (excal--load-current-style nil)
    (setq excal--grid-enabled nil excal--grid-size 20 excal--grid-step 5
          excal--objects-snap-enabled nil excal--snap-lines nil
          excal--tool-locked nil excal--multi-element nil
          excal--canvas-size '(400 . 300))
    ,@body))

(ert-deftest excal-snap-test-grid-points ()
  "Grid points round to the nearest multiple; super suppresses it."
  (excal-snap-test--scene
   (should (equal (excal--grid-point '(29.0 . 31.0)) '(29.0 . 31.0)))
   (setq excal--grid-enabled t)
   (should (equal (excal--grid-point '(29.0 . 31.0)) '(20.0 . 40.0)))
   (should (equal (excal--grid-point '(29.0 . 31.0) t) '(29.0 . 31.0)))))

(ert-deftest excal-snap-test-grid-create-move-nudge ()
  "New shapes, drags and arrow keys follow the grid."
  (excal-snap-test--scene
   (setq excal--grid-enabled t excal--tool 'rectangle)
   (excal-test--drag 23 18 67 55)
   (let ((r (car (last excal--elements))))
     (should (equal (list (excal--get r 'x) (excal--get r 'y)
                          (excal--get r 'width) (excal--get r 'height))
                    '(20.0 20.0 40.0 40.0)))
     ;; Dragging moves the top-left corner onto grid lines.
     (excal-test--drag 40 40 53 47)
     (should (equal (list (excal--get r 'x) (excal--get r 'y)) '(40.0 20.0)))
     (excal-nudge-right)
     (should (= (excal--get r 'x) 60.0))
     (excal-nudge-right-large)
     (should (= (excal--get r 'x) 61.0)))))

(ert-deftest excal-snap-test-objects ()
  "Moving near another element's edge aligns to it and draws a snap line."
  (excal-snap-test--scene
   (setq excal--objects-snap-enabled t)
   (let ((fixed (excal-test--rect 100 100))
         (moving (excal-test--rect 0 0)))
     (setq excal--elements (list fixed moving))
     ;; Left edges and centers both end up 1 apart: both snap at dx 100.
     (let ((d (excal--snap-move (list moving) 99 50 nil)))
       (should (equal d '(100.0 . 50))))
     (should excal--snap-lines)
     ;; Left edges, centers and right edges all align: one line each.
     (should (= (length excal--snap-lines) 3))
     (should (seq-some (lambda (line) (seq-every-p (lambda (p) (= (car p) 100.0)) line))
                       excal--snap-lines))
     ;; Far away nothing snaps.
     (should (equal (excal--snap-move (list moving) 50 50 nil) '(50 . 50)))
     (should-not excal--snap-lines)
     ;; Super inverts the setting.
     (should (equal (excal--snap-move (list moving) 99 50 t) '(99 . 50))))))

(ert-deftest excal-snap-test-overlays ()
  "The grid and snap lines become overlays; the grid draws gray lines."
  (excal-snap-test--scene
   (setq excal--grid-enabled t excal--snap-lines (list '((0.0 . 0.0) (0.0 . 50.0))))
   (let ((kinds (mapcar (lambda (v) (aref v 0)) (excal--overlay-natives))))
     (should (member "ov-grid" kinds))
     (should (>= (seq-count (lambda (k) (equal k "ov-poly")) kinds) 5)))
   (setq excal--snap-lines nil)
   (let ((fb (excal-native-fb-create 100 100))
         (white (excal-native-fb-create 100 100)))
     (excal-native-fb-render white 1.0 1.0 0.0 0.0 [] nil)
     (excal-native-fb-render fb 1.0 1.0 0.0 0.0 (excal--visible-elements) nil)
     (should (> (excal-native-fb-diff fb white) 20)))))

(provide 'excal-snap-test)
;;; excal-snap-test.el ends here
