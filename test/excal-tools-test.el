;;; excal-tools-test.el --- Laser, eye dropper, autoshape, lasso, bucket  -*- lexical-binding: t; -*-

(require 'ert)
(require 'excal)
(require 'excal-test)

(defmacro excal-tools-test--with-canvas (&rest body)
  "Run BODY in a window-backed scene with a 300x200 framebuffer."
  `(excal-test--in-window
    (setq excal--elements nil excal--canvas-size (cons 300 200)
          excal--fb (excal-native-fb-create 300 200)
          excal--theme 'light excal--tool-locked nil
          excal--preferred-selection-tool 'select
          excal--bucket-color nil excal--laser-trails nil excal--laser-live nil)
    (excal--load-current-style nil)
    (unwind-protect (progn ,@body)
      (when excal--laser-timer (cancel-timer excal--laser-timer))
      (setq excal--laser-timer nil))))

(defun excal-tools-test--box (x y w h &rest props)
  "Return a rectangle at X, Y sized W by H with PROPS, not rough."
  (apply #'excal--make-element "rectangle" x y (cons 'width (float w))
         (cons 'height (float h)) (cons 'roughness 0) props))

;;;; Laser

(ert-deftest excal-tools-test-laser-streamline ()
  "Each point moves 60% of the way toward the pointer."
  (let ((trail (excal--laser-add nil 0 0 1.0)))
    (setq trail (excal--laser-add trail 10 0 1.0))
    (should (equal (append (car trail) nil) '(6.0 0.0 1.0)))))

(ert-deftest excal-tools-test-laser-decays ()
  "Radii taper toward the tail and fade out over a second."
  (let ((excal--zoom 1.0) trail)
    (dotimes (i 60) (setq trail (excal--laser-add trail (* i 5) 0 100.0)))
    (let ((radii (excal--laser-radii (reverse trail) 100.0)))
      ;; Older than DECAY_LENGTH points: gone; the head is almost full size.
      (should (= (car radii) 0))
      (should (> (car (last radii)) 1.9))
      (should (apply #'<= (last radii 50))))
    (should (excal--laser-outline trail 100.5))
    (should-not (excal--laser-outline trail 101.0))))

(ert-deftest excal-tools-test-laser-drag-leaves-no-element ()
  "Dragging the laser draws a trail overlay and adds nothing."
  (excal-tools-test--with-canvas
   (excal-select-tool 'laser)
   (excal-test--drag 20 20 120 60)
   (should (null excal--elements))
   (should excal--laser-trails)
   (should excal--laser-timer)
   (let ((ov (car (excal--laser-overlays))))
     (should (equal (aref ov 0) "ov-poly"))
     (should (equal (aref ov 7) "#ff0000")))
   ;; Once decayed, the trails and the timer go.
   (dolist (trail excal--laser-trails)
     (dolist (p trail) (aset p 2 0.0)))
   (excal--laser-tick (current-buffer))
   (should (null excal--laser-trails))
   (should (null excal--laser-timer))))

;;;; Eye dropper

(ert-deftest excal-tools-test-pixel-color ()
  "The color under a point is read back from the framebuffer."
  (excal-tools-test--with-canvas
   (setq excal--elements (list (excal-tools-test--box 50 50 100 100
                                                      (cons 'backgroundColor "#a5d8ff")
                                                      (cons 'fillStyle "solid"))))
   (excal--render)
   (should (equal (excal--pixel-color '(100 . 100)) "#a5d8ff"))
   (should (equal (excal--pixel-color '(10 . 10)) "#ffffff"))))

(ert-deftest excal-tools-test-undo-dark-filter ()
  "Picking in the dark theme recovers the light color."
  (let ((rgb '(0.647 0.847 1.0)))
    ;; Apply the filter as excal_dark_filter does.
    (let* ((c (mapcar (lambda (v) (+ (* v 0.07) (* (- 1 v) 0.93))) rgb))
           (m excal--dark-filter-matrix)
           (dark (cl-loop for i below 3
                          collect (min 1.0 (max 0.0 (cl-loop for j below 3
                                                             sum (* (aref (aref m i) j)
                                                                    (nth j c))))))))
      (should (cl-every (lambda (a b) (< (abs (- a b)) 1e-6))
                        (excal--undo-dark-filter dark) rgb)))))

(ert-deftest excal-tools-test-eyedropper-applies ()
  "Picks go to the selection with the selection tool, else the style."
  (excal-tools-test--with-canvas
   (let ((a (excal-tools-test--box 0 0 10 10)))
     (setq excal--elements (list a))
     (excal--eyedropper-apply 'background "#123456" nil)
     (should (equal (excal--style-value 'backgroundColor) "#123456"))
     (excal--select (list a))
     (excal--eyedropper-apply 'stroke "#654321" nil)
     (should (equal (excal--get a 'strokeColor) "#654321"))
     ;; Meta swaps the target.
     (excal--eyedropper-apply 'stroke "#abcdef" t)
     (should (equal (excal--get a 'backgroundColor) "#abcdef")))))

(ert-deftest excal-tools-test-eyedropper-click ()
  "A click after `i' picks the pixel under it as the background."
  (excal-tools-test--with-canvas
   (setq excal--elements (list (excal-tools-test--box 50 50 100 100
                                                      (cons 'backgroundColor "#ffc9c9")
                                                      (cons 'fillStyle "solid"))))
   (excal--render)
   (let ((at (excal-test--posn 100 100)))
     (setq unread-command-events
           (list (list 'mouse-movement at) (list 'down-mouse-1 at) (list 'mouse-1 at)))
     (excal-eyedropper))
   (should (equal (excal--style-value 'backgroundColor) "#ffc9c9"))
   (should (null excal--eyedropper))))

(ert-deftest excal-tools-test-eyedropper-escape ()
  "Escape cancels the eye dropper."
  (excal-tools-test--with-canvas
   (setq unread-command-events (list 'escape))
   (excal-eyedropper 'stroke)
   (should (equal (excal--style-value 'strokeColor) "#1e1e1e"))))

;;;; Autoshape

(defun excal-tools-test--around (f n)
  "Return N points (X . Y) of F applied to angles around a circle."
  (cl-loop for i to n collect (funcall f (/ (* 2 float-pi i) n))))

(ert-deftest excal-tools-test-autoshape-recognizes ()
  "Closed strokes become the closest shape; straight ones lines."
  (let ((excal--zoom 1.0))
    (should (equal (car (excal--autoshape-recognize
                         (excal-tools-test--around
                          (lambda (a) (cons (+ 100 (* 80 (cos a))) (+ 100 (* 50 (sin a))))) 40)))
                   "ellipse"))
    (should (equal (car (excal--autoshape-recognize
                         '((0 . 0) (50 . 0) (100 . 0) (100 . 40) (100 . 80) (50 . 80)
                           (0 . 80) (0 . 40) (0 . 2))))
                   "rectangle"))
    (should (equal (car (excal--autoshape-recognize
                         '((50 . 0) (75 . 25) (100 . 50) (75 . 75) (50 . 100) (25 . 75)
                           (0 . 50) (25 . 25) (49 . 1))))
                   "diamond"))
    (should (equal (car (excal--autoshape-recognize '((0 . 0) (40 . 21) (80 . 39) (120 . 61))))
                   "line"))
    ;; A zig-zag is left as drawn.
    (should-not (excal--autoshape-recognize '((0 . 0) (30 . 60) (60 . 0) (90 . 60) (120 . 0))))))

(ert-deftest excal-tools-test-autoshape-tool ()
  "Drawing with the autoshape tool leaves the recognised shape selected."
  (excal-tools-test--with-canvas
   (excal-select-tool 'autoshape)
   (setq unread-command-events
         (append (mapcar (lambda (p) (list 'mouse-movement (excal-test--posn (car p) (cdr p))))
                         '((50 . 20) (80 . 20) (80 . 50) (80 . 80) (50 . 80) (20 . 80)
                           (20 . 50) (20 . 22)))
                 (list (list 'drag-mouse-1 (excal-test--posn 20 20) (excal-test--posn 20 22)))))
   (excal-mouse-down (list 'down-mouse-1 (excal-test--posn 20 20)))
   (should (= (length excal--elements) 1))
   (should (equal (excal--get (car excal--elements) 'type) "rectangle"))
   (should (equal excal--selection excal--elements))
   (should (eq excal--tool 'select))))

;;;; Lasso

(ert-deftest excal-tools-test-lasso-selection ()
  "Elements count only when their whole outline is inside the lasso."
  (excal-test--with-scene
   (let ((a (excal-tools-test--box 10 10 20 20))
         (b (excal-tools-test--box 50 10 20 20))
         (c (excal-tools-test--box 90 10 20 20)))
     (setq excal--elements (list a b c))
     (let ((lasso '((0 . 0) (80 . 0) (80 . 40) (0 . 40))))
       (should (equal (excal--lasso-selection lasso) (list a b))))
     ;; B grouped with C: the group is only partly inside.
     (excal--put b 'groupIds ["g"]) (excal--put c 'groupIds ["g"])
     (should (equal (excal--lasso-selection '((0 . 0) (80 . 0) (80 . 40) (0 . 40)))
                    (list a))))))

(ert-deftest excal-tools-test-lasso-drag ()
  "Control+meta drag from the selection tool draws a lasso."
  (excal-tools-test--with-canvas
   (let ((a (excal-tools-test--box 40 40 20 20))
         (b (excal-tools-test--box 150 40 20 20)))
     (setq excal--elements (list a b))
     (setq unread-command-events
           (append (mapcar (lambda (p) (list 'mouse-movement (excal-test--posn (car p) (cdr p))))
                           '((100 . 20) (100 . 100) (20 . 100) (20 . 25)))
                   (list (list 'drag-mouse-1 (excal-test--posn 20 20) (excal-test--posn 20 25)))))
     (excal-mouse-down (list 'C-M-down-mouse-1 (excal-test--posn 20 20)))
     (should (equal excal--selection (list a)))
     (should (null excal--lasso))
     (should (eq excal--tool 'select)))))

(ert-deftest excal-tools-test-preferred-lasso ()
  "With the lasso preferred, `v' chooses it."
  (excal-tools-test--with-canvas
   (excal-toggle-lasso)
   (excal-select-tool 'rectangle)
   (excal-select-tool 'select)
   (should (eq excal--tool 'lasso))
   (excal-toggle-lasso)
   (should (eq excal--tool 'select))))

;;;; Bucket fill

(ert-deftest excal-tools-test-bucket-fills-ring ()
  "The region between two nested squares is filled with a hole."
  (excal-tools-test--with-canvas
   (let ((outer (excal--make-element "line" 0 0 (cons 'roughness 0)
                                     (cons 'points [[0.0 0.0] [100.0 0.0] [100.0 100.0]
                                                    [0.0 100.0] [0.0 0.0]])))
         (inner (excal-tools-test--box 40 40 20 20)))
     (excal--linear-extent outer)
     (setq excal--elements (list outer inner))
     (let* ((fill (excal-bucket-fill-at '(20 . 20)))
            (points (excal--absolute-points fill)))
       (should (excal--bucket-fill-p fill))
       (should (equal (excal--get fill 'backgroundColor) "#ffc9c9"))
       (should (equal (excal--get fill 'strokeColor) "transparent"))
       (should (equal (car points) (car (last points))))
       ;; About 100x100 minus the 20x20 island.
       (should (< (abs (- (excal--polygon-area points) 9600)) 400))
       ;; Above the enclosing outline, below the island.
       (should (equal excal--elements (list outer fill inner)))
       ;; Filling the same region again recolours it.
       (excal-bucket-cycle-color)
       (should (eq (excal-bucket-fill-at '(20 . 20)) fill))
       (should (= (length excal--elements) 3))
       (should (equal (excal--get fill 'backgroundColor) "#b2f2bb"))))))

(ert-deftest excal-tools-test-bucket-fills-shape-inside ()
  "Filling inside a closed shape gives the shape the background."
  (excal-tools-test--with-canvas
   (let ((box (excal-tools-test--box 20 20 100 60)))
     (setq excal--elements (list box))
     (should (eq (excal-bucket-fill-at '(70 . 50)) box))
     (should (= (length excal--elements) 1))
     (should (equal (excal--get box 'backgroundColor) "#ffc9c9")))))

(ert-deftest excal-tools-test-bucket-bridges-gaps ()
  "Small gaps close the region; open space is refused."
  (excal-tools-test--with-canvas
   ;; Two open lines leaving a 4px gap at (0, 51)-(0, 55).
   (let ((a (excal--make-element "line" 0 0 (cons 'roughness 0)
                                 (cons 'points [[0.0 0.0] [100.0 0.0] [100.0 100.0]
                                                [0.0 100.0] [0.0 55.0]])))
         (b (excal--make-element "line" 0 0 (cons 'roughness 0)
                                 (cons 'points [[0.0 51.0] [0.0 0.0]]))))
     (excal--linear-extent a)
     (excal--linear-extent b)
     (setq excal--elements (list a b))
     (should (excal--bucket-fill-p (excal-bucket-fill-at '(50 . 50))))
     (should (null (excal-bucket-fill-at '(200 . 50)))))))

(ert-deftest excal-tools-test-bucket-tool ()
  "`b' again cycles the color; meta+click picks it; the tool stays."
  (excal-tools-test--with-canvas
   (setq excal--elements (list (excal-tools-test--box 20 20 100 60)))
   (excal-select-tool 'bucketfill)
   (excal-select-tool 'bucketfill)
   (should (equal excal--bucket-color "#b2f2bb"))
   (should (eq excal--tool 'bucketfill))
   (setq unread-command-events (list (list 'mouse-1 (excal-test--posn 70 50))))
   (excal-mouse-down (list 'down-mouse-1 (excal-test--posn 70 50)))
   (should (equal (excal--get (car excal--elements) 'backgroundColor) "#b2f2bb"))
   (should (eq excal--tool 'bucketfill))
   (excal--render)
   (setq unread-command-events (list (list 'M-mouse-1 (excal-test--posn 5 5))))
   (excal-mouse-down (list 'M-down-mouse-1 (excal-test--posn 5 5)))
   (should (equal excal--bucket-color "#ffffff"))))

(provide 'excal-tools-test)
;;; excal-tools-test.el ends here
