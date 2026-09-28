;;; excali-tools-test.el --- Laser, eye dropper, autoshape, lasso, bucket  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)
(require 'excali-test)

(defmacro excali-tools-test--with-canvas (&rest body)
  "Run BODY in a window-backed scene with a 300x200 framebuffer."
  `(excali-test--in-window
    (setq excali--elements nil excali--canvas-size (cons 300 200)
          excali--fb (excali-native-fb-create 300 200)
          excali--theme 'light excali--tool-locked nil
          excali--preferred-selection-tool 'select
          excali--bucket-color nil excali--laser-trails nil excali--laser-live nil)
    (excali--load-current-style nil)
    (unwind-protect (progn ,@body)
      (when excali--laser-timer (cancel-timer excali--laser-timer))
      (setq excali--laser-timer nil))))

(defun excali-tools-test--box (x y w h &rest props)
  "Return a rectangle at X, Y sized W by H with PROPS, not rough."
  (apply #'excali--make-element "rectangle" x y (cons 'width (float w))
         (cons 'height (float h)) (cons 'roughness 0) props))

;;;; Laser

(ert-deftest excali-tools-test-laser-streamline ()
  "Each point moves 60% of the way toward the pointer."
  (let ((trail (excali--laser-add nil 0 0 1.0)))
    (setq trail (excali--laser-add trail 10 0 1.0))
    (should (equal (append (car trail) nil) '(6.0 0.0 1.0)))))

(ert-deftest excali-tools-test-laser-decays ()
  "Radii taper toward the tail and fade out over a second."
  (let ((excali--zoom 1.0) trail)
    (dotimes (i 60) (setq trail (excali--laser-add trail (* i 5) 0 100.0)))
    (let ((radii (excali--laser-radii (reverse trail) 100.0)))
      ;; Older than DECAY_LENGTH points: gone; the head is almost full size.
      (should (= (car radii) 0))
      (should (> (car (last radii)) 1.9))
      (should (apply #'<= (last radii 50))))
    (should (excali--laser-outline trail 100.5))
    (should-not (excali--laser-outline trail 101.0))))

(ert-deftest excali-tools-test-laser-drag-leaves-no-element ()
  "Dragging the laser draws a trail overlay and adds nothing."
  (excali-tools-test--with-canvas
   (excali-select-tool 'laser)
   (excali-test--drag 20 20 120 60)
   (should (null excali--elements))
   (should excali--laser-trails)
   (should excali--laser-timer)
   (let ((ov (car (excali--laser-overlays))))
     (should (equal (aref ov 0) "ov-poly"))
     (should (equal (aref ov 7) "#ff0000")))
   ;; Once decayed, the trails and the timer go.
   (dolist (trail excali--laser-trails)
     (dolist (p trail) (aset p 2 0.0)))
   (excali--laser-tick (current-buffer))
   (should (null excali--laser-trails))
   (should (null excali--laser-timer))))

;;;; Eye dropper

(ert-deftest excali-tools-test-pixel-color ()
  "The color under a point is read back from the framebuffer."
  (excali-tools-test--with-canvas
   (setq excali--elements (list (excali-tools-test--box 50 50 100 100
                                                      (cons 'backgroundColor "#a5d8ff")
                                                      (cons 'fillStyle "solid"))))
   (excali--render)
   (should (equal (excali--pixel-color '(100 . 100)) "#a5d8ff"))
   (should (equal (excali--pixel-color '(10 . 10)) "#ffffff"))))

(ert-deftest excali-tools-test-undo-dark-filter ()
  "Picking in the dark theme recovers the light color."
  (let ((rgb '(0.647 0.847 1.0)))
    ;; Apply the filter as excali_dark_filter does.
    (let* ((c (mapcar (lambda (v) (+ (* v 0.07) (* (- 1 v) 0.93))) rgb))
           (m excali--dark-filter-matrix)
           (dark (cl-loop for i below 3
                          collect (min 1.0 (max 0.0 (cl-loop for j below 3
                                                             sum (* (aref (aref m i) j)
                                                                    (nth j c))))))))
      (should (cl-every (lambda (a b) (< (abs (- a b)) 1e-6))
                        (excali--undo-dark-filter dark) rgb)))))

(ert-deftest excali-tools-test-eyedropper-applies ()
  "Picks go to the selection with the selection tool, else the style."
  (excali-tools-test--with-canvas
   (let ((a (excali-tools-test--box 0 0 10 10)))
     (setq excali--elements (list a))
     (excali--eyedropper-apply 'background "#123456" nil)
     (should (equal (excali--style-value 'backgroundColor) "#123456"))
     (excali--select (list a))
     (excali--eyedropper-apply 'stroke "#654321" nil)
     (should (equal (excali--get a 'strokeColor) "#654321"))
     ;; Meta swaps the target.
     (excali--eyedropper-apply 'stroke "#abcdef" t)
     (should (equal (excali--get a 'backgroundColor) "#abcdef")))))

(ert-deftest excali-tools-test-eyedropper-click ()
  "A click after `i' picks the pixel under it as the background."
  (excali-tools-test--with-canvas
   (setq excali--elements (list (excali-tools-test--box 50 50 100 100
                                                      (cons 'backgroundColor "#ffc9c9")
                                                      (cons 'fillStyle "solid"))))
   (excali--render)
   (let ((at (excali-test--posn 100 100)))
     (setq unread-command-events
           (list (list 'mouse-movement at) (list 'down-mouse-1 at) (list 'mouse-1 at)))
     (excali-eyedropper))
   (should (equal (excali--style-value 'backgroundColor) "#ffc9c9"))
   (should (null excali--eyedropper))))

(ert-deftest excali-tools-test-eyedropper-escape ()
  "Escape cancels the eye dropper."
  (excali-tools-test--with-canvas
   (setq unread-command-events (list 'escape))
   (excali-eyedropper 'stroke)
   (should (equal (excali--style-value 'strokeColor) "#1e1e1e"))))

;;;; Autoshape

(defun excali-tools-test--around (f n)
  "Return N points (X . Y) of F applied to angles around a circle."
  (cl-loop for i to n collect (funcall f (/ (* 2 float-pi i) n))))

(ert-deftest excali-tools-test-autoshape-recognizes ()
  "Closed strokes become the closest shape; straight ones lines."
  (let ((excali--zoom 1.0))
    (should (equal (car (excali--autoshape-recognize
                         (excali-tools-test--around
                          (lambda (a) (cons (+ 100 (* 80 (cos a))) (+ 100 (* 50 (sin a))))) 40)))
                   "ellipse"))
    (should (equal (car (excali--autoshape-recognize
                         '((0 . 0) (50 . 0) (100 . 0) (100 . 40) (100 . 80) (50 . 80)
                           (0 . 80) (0 . 40) (0 . 2))))
                   "rectangle"))
    (should (equal (car (excali--autoshape-recognize
                         '((50 . 0) (75 . 25) (100 . 50) (75 . 75) (50 . 100) (25 . 75)
                           (0 . 50) (25 . 25) (49 . 1))))
                   "diamond"))
    (should (equal (car (excali--autoshape-recognize '((0 . 0) (40 . 21) (80 . 39) (120 . 61))))
                   "line"))
    ;; A zig-zag is left as drawn.
    (should-not (excali--autoshape-recognize '((0 . 0) (30 . 60) (60 . 0) (90 . 60) (120 . 0))))))

(ert-deftest excali-tools-test-autoshape-tool ()
  "Drawing with the autoshape tool leaves the recognised shape selected."
  (excali-tools-test--with-canvas
   (excali-select-tool 'autoshape)
   (setq unread-command-events
         (append (mapcar (lambda (p) (list 'mouse-movement (excali-test--posn (car p) (cdr p))))
                         '((50 . 20) (80 . 20) (80 . 50) (80 . 80) (50 . 80) (20 . 80)
                           (20 . 50) (20 . 22)))
                 (list (list 'drag-mouse-1 (excali-test--posn 20 20) (excali-test--posn 20 22)))))
   (excali-mouse-down (list 'down-mouse-1 (excali-test--posn 20 20)))
   (should (= (length excali--elements) 1))
   (should (equal (excali--get (car excali--elements) 'type) "rectangle"))
   (should (equal excali--selection excali--elements))
   (should (eq excali--tool 'select))))

;;;; Lasso

(ert-deftest excali-tools-test-lasso-selection ()
  "Elements count only when their whole outline is inside the lasso."
  (excali-test--with-scene
   (let ((a (excali-tools-test--box 10 10 20 20))
         (b (excali-tools-test--box 50 10 20 20))
         (c (excali-tools-test--box 90 10 20 20)))
     (setq excali--elements (list a b c))
     (let ((lasso '((0 . 0) (80 . 0) (80 . 40) (0 . 40))))
       (should (equal (excali--lasso-selection lasso) (list a b))))
     ;; B grouped with C: the group is only partly inside.
     (excali--put b 'groupIds ["g"]) (excali--put c 'groupIds ["g"])
     (should (equal (excali--lasso-selection '((0 . 0) (80 . 0) (80 . 40) (0 . 40)))
                    (list a))))))

(ert-deftest excali-tools-test-lasso-drag ()
  "Control+meta drag from the selection tool draws a lasso."
  (excali-tools-test--with-canvas
   (let ((a (excali-tools-test--box 40 40 20 20))
         (b (excali-tools-test--box 150 40 20 20)))
     (setq excali--elements (list a b))
     (setq unread-command-events
           (append (mapcar (lambda (p) (list 'mouse-movement (excali-test--posn (car p) (cdr p))))
                           '((100 . 20) (100 . 100) (20 . 100) (20 . 25)))
                   (list (list 'drag-mouse-1 (excali-test--posn 20 20) (excali-test--posn 20 25)))))
     (excali-mouse-down (list 'C-M-down-mouse-1 (excali-test--posn 20 20)))
     (should (equal excali--selection (list a)))
     (should (null excali--lasso))
     (should (eq excali--tool 'select)))))

(ert-deftest excali-tools-test-preferred-lasso ()
  "With the lasso preferred, `v' chooses it."
  (excali-tools-test--with-canvas
   (excali-toggle-lasso)
   (excali-select-tool 'rectangle)
   (excali-select-tool 'select)
   (should (eq excali--tool 'lasso))
   (excali-toggle-lasso)
   (should (eq excali--tool 'select))))

;;;; Bucket fill

(ert-deftest excali-tools-test-bucket-fills-ring ()
  "The region between two nested squares is filled with a hole."
  (excali-tools-test--with-canvas
   (let ((outer (excali--make-element "line" 0 0 (cons 'roughness 0)
                                     (cons 'points [[0.0 0.0] [100.0 0.0] [100.0 100.0]
                                                    [0.0 100.0] [0.0 0.0]])))
         (inner (excali-tools-test--box 40 40 20 20)))
     (excali--linear-extent outer)
     (setq excali--elements (list outer inner))
     (let* ((fill (excali-bucket-fill-at '(20 . 20)))
            (points (excali--absolute-points fill)))
       (should (excali--bucket-fill-p fill))
       (should (equal (excali--get fill 'backgroundColor) "#ffc9c9"))
       (should (equal (excali--get fill 'strokeColor) "transparent"))
       (should (equal (car points) (car (last points))))
       ;; About 100x100 minus the 20x20 island.
       (should (< (abs (- (excali--polygon-area points) 9600)) 400))
       ;; Above the enclosing outline, below the island.
       (should (equal excali--elements (list outer fill inner)))
       ;; Filling the same region again recolours it.
       (excali-bucket-cycle-color)
       (should (eq (excali-bucket-fill-at '(20 . 20)) fill))
       (should (= (length excali--elements) 3))
       (should (equal (excali--get fill 'backgroundColor) "#b2f2bb"))))))

(ert-deftest excali-tools-test-bucket-fills-shape-inside ()
  "Filling inside a closed shape gives the shape the background."
  (excali-tools-test--with-canvas
   (let ((box (excali-tools-test--box 20 20 100 60)))
     (setq excali--elements (list box))
     (should (eq (excali-bucket-fill-at '(70 . 50)) box))
     (should (= (length excali--elements) 1))
     (should (equal (excali--get box 'backgroundColor) "#ffc9c9")))))

(ert-deftest excali-tools-test-bucket-bridges-gaps ()
  "Small gaps close the region; open space is refused."
  (excali-tools-test--with-canvas
   ;; Two open lines leaving a 4px gap at (0, 51)-(0, 55).
   (let ((a (excali--make-element "line" 0 0 (cons 'roughness 0)
                                 (cons 'points [[0.0 0.0] [100.0 0.0] [100.0 100.0]
                                                [0.0 100.0] [0.0 55.0]])))
         (b (excali--make-element "line" 0 0 (cons 'roughness 0)
                                 (cons 'points [[0.0 51.0] [0.0 0.0]]))))
     (excali--linear-extent a)
     (excali--linear-extent b)
     (setq excali--elements (list a b))
     (should (excali--bucket-fill-p (excali-bucket-fill-at '(50 . 50))))
     (should (null (excali-bucket-fill-at '(200 . 50)))))))

(ert-deftest excali-tools-test-bucket-tool ()
  "`b' again cycles the color; meta+click picks it; the tool stays."
  (excali-tools-test--with-canvas
   (setq excali--elements (list (excali-tools-test--box 20 20 100 60)))
   (excali-select-tool 'bucketfill)
   (excali-select-tool 'bucketfill)
   (should (equal excali--bucket-color "#b2f2bb"))
   (should (eq excali--tool 'bucketfill))
   (setq unread-command-events (list (list 'mouse-1 (excali-test--posn 70 50))))
   (excali-mouse-down (list 'down-mouse-1 (excali-test--posn 70 50)))
   (should (equal (excali--get (car excali--elements) 'backgroundColor) "#b2f2bb"))
   (should (eq excali--tool 'bucketfill))
   (excali--render)
   (setq unread-command-events (list (list 'M-mouse-1 (excali-test--posn 5 5))))
   (excali-mouse-down (list 'M-down-mouse-1 (excali-test--posn 5 5)))
   (should (equal excali--bucket-color "#ffffff"))))

(provide 'excali-tools-test)
;;; excali-tools-test.el ends here
