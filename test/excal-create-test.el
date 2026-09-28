;;; excal-create-test.el --- Creation tools and editor actions  -*- lexical-binding: t; -*-

(require 'ert)
(require 'excal)
(require 'excal-test)

(defmacro excal-create-test--in-window (&rest body)
  "Run BODY in a window-backed scene with a fresh current style."
  `(excal-test--in-window
    (setq excal--elements nil excal--tool-locked nil excal--multi-element nil)
    (excal--load-current-style nil)
    ,@body))

(defun excal-create-test--last ()
  "Return the most recently added element."
  (car (last excal--elements)))

;;;; Shapes

(ert-deftest excal-create-test-click-creates-nothing ()
  "A plain click with a shape tool leaves no element behind."
  (excal-create-test--in-window
   (setq excal--tool 'rectangle)
   (excal-test--drag 20 20 20 20)
   (should (null excal--elements))))

(ert-deftest excal-create-test-drag-shape-then-select ()
  "Dragging creates the shape, selects it and returns to the selection tool."
  (excal-create-test--in-window
   (setq excal--tool 'rectangle)
   (excal-test--drag 20 20 60 50)
   (let ((r (excal-create-test--last)))
     (should (equal (list (excal--get r 'x) (excal--get r 'y)
                          (excal--get r 'width) (excal--get r 'height))
                    '(20.0 20.0 40.0 30.0)))
     (should (equal excal--selection (list r)))
     (should (eq excal--tool 'select)))
   ;; Dragging up-left normalizes the box.
   (setq excal--tool 'ellipse)
   (excal-test--drag 100 100 80 70)
   (should (equal (list (excal--get (excal-create-test--last) 'x)
                        (excal--get (excal-create-test--last) 'height))
                  '(80.0 30.0)))))

(ert-deftest excal-create-test-shift-square-and-meta-center ()
  "Shift at the press draws a square; meta grows it from the center."
  (excal-create-test--in-window
   (setq excal--tool 'rectangle)
   (excal-test--drag 20 20 60 30 '(shift))
   (should (equal (list (excal--get (excal-create-test--last) 'width)
                        (excal--get (excal-create-test--last) 'height))
                  '(40.0 40.0)))
   (setq excal--tool 'diamond)
   (excal-test--drag 100 100 110 120 '(meta))
   (let ((d (excal-create-test--last)))
     (should (equal (list (excal--get d 'x) (excal--get d 'y)
                          (excal--get d 'width) (excal--get d 'height))
                    '(90.0 80.0 20.0 40.0))))))

(ert-deftest excal-create-test-tool-lock ()
  "With the tool locked, drawing keeps the tool."
  (excal-create-test--in-window
   (excal-toggle-tool-lock)
   (setq excal--tool 'rectangle)
   (excal-test--drag 20 20 60 50)
   (should (eq excal--tool 'rectangle))
   (excal-toggle-tool-lock)))

;;;; Lines and arrows

(ert-deftest excal-create-test-drag-arrow ()
  "A long drag draws a two-point arrow with the current arrowheads."
  (excal-create-test--in-window
   (setq excal--tool 'arrow)
   (excal-test--drag 10 10 110 60)
   (let ((a (excal-create-test--last)))
     (should (equal (excal--get a 'points) [[0.0 0.0] [100.0 50.0]]))
     (should (equal (excal--get a 'endArrowhead) "arrow"))
     (should (equal (excal--get a 'roundness) '((type . 2))))
     (should (eq excal--tool 'select)))))

(ert-deftest excal-create-test-shift-locks-angle ()
  "Shift snaps the arrow direction to 15 degrees."
  (excal-create-test--in-window
   (setq excal--tool 'line)
   (excal-test--drag 0 0 100 3 '(shift))
   (let ((p (aref (excal--get (excal-create-test--last) 'points) 1)))
     (should (< (abs (aref p 1)) 1e-9))
     (should (> (aref p 0) 99)))))

(ert-deftest excal-create-test-click-click-line ()
  "Short drags start click-click mode; clicking the last point finishes."
  (excal-create-test--in-window
   (setq excal--tool 'line)
   (excal-test--drag 10 10 12 10)
   (should excal--multi-element)
   (let ((line excal--multi-element))
     ;; The floating point follows the mouse.
     (excal-mouse-move (list 'mouse-movement (excal-test--posn 60 10)))
     (should (equal (aref (excal--get line 'points) 1) [50.0 0.0]))
     (excal-test--drag 60 10 60 10)
     (excal-test--drag 60 60 60 60)
     ;; Clicking the last committed point again finishes the line.
     (excal-test--drag 60 61 60 61)
     (should-not excal--multi-element)
     (should (equal (excal--get line 'points) [[0.0 0.0] [50.0 0.0] [50.0 50.0]]))
     (should (equal excal--selection (list line))))))

(ert-deftest excal-create-test-line-closes-loop ()
  "A line whose next point lands on its start closes and finishes."
  (excal-create-test--in-window
   (setq excal--tool 'line)
   (excal-test--drag 10 10 10 10)
   (excal-test--drag 60 10 60 10)
   (excal-test--drag 60 60 60 60)
   (excal-test--drag 11 11 11 11)
   (should-not excal--multi-element)
   (let ((pts (excal--get (excal-create-test--last) 'points)))
     (should (equal (aref pts (1- (length pts))) [0.0 0.0])))))

(ert-deftest excal-create-test-return-finishes-and-escape-discards-stub ()
  "RET finishes a multi-point arrow; an arrow with one point is dropped."
  (excal-create-test--in-window
   (setq excal--tool 'arrow)
   (excal-test--drag 10 10 10 10)
   (excal-test--drag 80 10 80 10)
   (excal-return)
   (should (= (length (excal--get (excal-create-test--last) 'points)) 2))
   (setq excal--tool 'arrow)
   (excal-test--drag 200 200 200 200)
   (excal-escape-dwim)
   (should (= (length excal--elements) 1))))

(ert-deftest excal-create-test-arrow-type-cycles ()
  "Choosing the arrow tool again cycles sharp, round and elbow arrows."
  (excal-create-test--in-window
   (excal-select-tool 'arrow)
   (excal-select-tool 'arrow)
   (should (equal (excal--style-value 'arrowType) "elbow"))
   (should (eq (excal--roundness-for "arrow") :null))
   (excal-select-tool 'arrow)
   (should (equal (excal--style-value 'arrowType) "sharp"))
   (should (eq (excal--roundness-for "arrow") :null))
   (excal-select-tool 'arrow)
   (should (equal (excal--roundness-for "arrow") '((type . 2))))))

(ert-deftest excal-create-test-freedraw-dot-keeps-tool ()
  "A freedraw click leaves a dot and the pen stays active."
  (excal-create-test--in-window
   (setq excal--tool 'freedraw)
   (excal-test--drag 30 30 30 30)
   (should (= (length (excal--get (excal-create-test--last) 'points)) 2))
   (should (eq excal--tool 'freedraw))
   (should (null excal--selection))))

;;;; Actions

(ert-deftest excal-create-test-flip ()
  "Flipping mirrors positions about the selection center and the angle."
  (excal-test--with-scene
   (setq excal--backend nil)
   (let ((a (excal-test--rect 0 0 (cons 'angle 0.3)))
         (b (excal-test--rect 90 0)))
     (setq excal--elements (list a b))
     (excal--select (list a b))
     (excal-flip-horizontal)
     (should (= (excal--get a 'x) 90.0))
     (should (= (excal--get b 'x) 0.0))
     (should (< (abs (- (excal--get a 'angle) (- (* 2 float-pi) 0.3))) 1e-9)))))

(ert-deftest excal-create-test-align-and-distribute ()
  "Units align to the selection box and spread evenly."
  (excal-test--with-scene
   (setq excal--backend nil)
   (let ((a (excal-test--rect 0 0)) (b (excal-test--rect 15 30)) (c (excal-test--rect 100 5)))
     (setq excal--elements (list a b c))
     (excal--select (list a b c))
     (excal-align-top)
     (should (equal (mapcar (lambda (e) (excal--get e 'y)) (list a b c)) '(0.0 0.0 0.0)))
     (excal-distribute-horizontally)
     ;; Span 0..110 with 30 of boxes: gaps of 40.
     (should (equal (mapcar (lambda (e) (excal--get e 'x)) (list a b c)) '(0.0 50.0 100.0))))))

(ert-deftest excal-create-test-lock ()
  "Locked elements are neither hit nor box-selected."
  (excal-test--with-scene
   (setq excal--backend nil)
   (let ((a (excal-test--rect 0 0 (cons 'backgroundColor "#ffc9c9"))))
     (setq excal--elements (list a))
     (excal--select (list a))
     (excal-toggle-lock)
     (should (eq (alist-get 'locked a) t))
     (should-not (excal--hit '(5.0 . 5.0)))
     (should-not (excal--marquee-selection '(-5 -5 50 50)))
     (excal-unlock-all)
     (should (eq (alist-get 'locked a) :false))
     (should (eq (excal--hit '(5.0 . 5.0)) a)))))

(ert-deftest excal-create-test-styles-fonts-convert ()
  "Copy/paste styles, font size steps and shape conversion."
  (excal-test--with-scene
   (setq excal--backend nil)
   (let ((a (excal-test--rect 0 0 (cons 'strokeColor "#e03131") (cons 'roundness '((type . 3)))))
         (b (excal-test--rect 20 0))
         (label (excal--make-text-element 0 30 "Hi")))
     (setq excal--elements (list a b label))
     (excal--select (list a))
     (excal-copy-styles)
     (excal--select (list b label))
     (excal-paste-styles)
     (should (equal (excal--get b 'strokeColor) "#e03131"))
     (should (equal (excal--get label 'strokeColor) "#e03131"))
     (excal-increase-font-size)
     (should (= (excal--get label 'fontSize) 22))
     (excal--select (list a))
     (excal-convert-type)
     (should (equal (excal--get a 'type) "diamond"))
     (excal-convert-type)
     (should (equal (excal--get a 'type) "ellipse"))
     (should (eq (alist-get 'roundness a) :null)))))

(ert-deftest excal-create-test-zoom-to-fit ()
  "Zoom to fit centers the scene and never zooms in past 100%."
  (excal-test--with-scene
   (setq excal--backend nil excal--canvas-size '(400 . 300))
   (let ((a (excal-test--rect 0 0)) (b (excal-test--rect 990 490)))
     (setq excal--elements (list a b))
     (excal-zoom-to-fit)
     (should (< (abs (- excal--zoom (* 0.9 (/ 400.0 1000)))) 1e-9))
     (should (< (abs (- (* excal--zoom (+ 500 excal--scroll-x)) 200)) 1e-6))
     (setq excal--elements (list a))
     (excal-zoom-to-fit)
     (should (= excal--zoom 1.0))
     (excal--select (list a))
     (excal-zoom-to-fit-selection)
     (should (> excal--zoom 1.0)))))

(provide 'excal-create-test)
;;; excal-create-test.el ends here
