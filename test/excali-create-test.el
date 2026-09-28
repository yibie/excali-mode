;;; excali-create-test.el --- Creation tools and editor actions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)
(require 'excali-test)

(defmacro excali-create-test--in-window (&rest body)
  "Run BODY in a window-backed scene with a fresh current style."
  `(excali-test--in-window
    (setq excali--elements nil excali--tool-locked nil excali--multi-element nil)
    (excali--load-current-style nil)
    ,@body))

(defun excali-create-test--last ()
  "Return the most recently added element."
  (car (last excali--elements)))

;;;; Shapes

(ert-deftest excali-create-test-click-creates-nothing ()
  "A plain click with a shape tool leaves no element behind."
  (excali-create-test--in-window
   (setq excali--tool 'rectangle)
   (excali-test--drag 20 20 20 20)
   (should (null excali--elements))))

(ert-deftest excali-create-test-drag-shape-then-select ()
  "Dragging creates the shape, selects it and returns to the selection tool."
  (excali-create-test--in-window
   (setq excali--tool 'rectangle)
   (excali-test--drag 20 20 60 50)
   (let ((r (excali-create-test--last)))
     (should (equal (list (excali--get r 'x) (excali--get r 'y)
                          (excali--get r 'width) (excali--get r 'height))
                    '(20.0 20.0 40.0 30.0)))
     (should (equal excali--selection (list r)))
     (should (eq excali--tool 'select)))
   ;; Dragging up-left normalizes the box.
   (setq excali--tool 'ellipse)
   (excali-test--drag 100 100 80 70)
   (should (equal (list (excali--get (excali-create-test--last) 'x)
                        (excali--get (excali-create-test--last) 'height))
                  '(80.0 30.0)))))

(ert-deftest excali-create-test-shift-square-and-meta-center ()
  "Shift at the press draws a square; meta grows it from the center."
  (excali-create-test--in-window
   (setq excali--tool 'rectangle)
   (excali-test--drag 20 20 60 30 '(shift))
   (should (equal (list (excali--get (excali-create-test--last) 'width)
                        (excali--get (excali-create-test--last) 'height))
                  '(40.0 40.0)))
   (setq excali--tool 'diamond)
   (excali-test--drag 100 100 110 120 '(meta))
   (let ((d (excali-create-test--last)))
     (should (equal (list (excali--get d 'x) (excali--get d 'y)
                          (excali--get d 'width) (excali--get d 'height))
                    '(90.0 80.0 20.0 40.0))))))

(ert-deftest excali-create-test-tool-lock ()
  "With the tool locked, drawing keeps the tool."
  (excali-create-test--in-window
   (excali-toggle-tool-lock)
   (setq excali--tool 'rectangle)
   (excali-test--drag 20 20 60 50)
   (should (eq excali--tool 'rectangle))
   (excali-toggle-tool-lock)))

;;;; Lines and arrows

(ert-deftest excali-create-test-drag-arrow ()
  "A long drag draws a two-point arrow with the current arrowheads."
  (excali-create-test--in-window
   (setq excali--tool 'arrow)
   (excali-test--drag 10 10 110 60)
   (let ((a (excali-create-test--last)))
     (should (equal (excali--get a 'points) [[0.0 0.0] [100.0 50.0]]))
     (should (equal (excali--get a 'endArrowhead) "arrow"))
     (should (equal (excali--get a 'roundness) '((type . 2))))
     (should (eq excali--tool 'select)))))

(ert-deftest excali-create-test-shift-locks-angle ()
  "Shift snaps the arrow direction to 15 degrees."
  (excali-create-test--in-window
   (setq excali--tool 'line)
   (excali-test--drag 0 0 100 3 '(shift))
   (let ((p (aref (excali--get (excali-create-test--last) 'points) 1)))
     (should (< (abs (aref p 1)) 1e-9))
     (should (> (aref p 0) 99)))))

(ert-deftest excali-create-test-click-click-line ()
  "Short drags start click-click mode; clicking the last point finishes."
  (excali-create-test--in-window
   (setq excali--tool 'line)
   (excali-test--drag 10 10 12 10)
   (should excali--multi-element)
   (let ((line excali--multi-element))
     ;; The floating point follows the mouse.
     (excali-mouse-move (list 'mouse-movement (excali-test--posn 60 10)))
     (should (equal (aref (excali--get line 'points) 1) [50.0 0.0]))
     (excali-test--drag 60 10 60 10)
     (excali-test--drag 60 60 60 60)
     ;; Clicking the last committed point again finishes the line.
     (excali-test--drag 60 61 60 61)
     (should-not excali--multi-element)
     (should (equal (excali--get line 'points) [[0.0 0.0] [50.0 0.0] [50.0 50.0]]))
     (should (equal excali--selection (list line))))))

(ert-deftest excali-create-test-line-closes-loop ()
  "A line whose next point lands on its start closes and finishes."
  (excali-create-test--in-window
   (setq excali--tool 'line)
   (excali-test--drag 10 10 10 10)
   (excali-test--drag 60 10 60 10)
   (excali-test--drag 60 60 60 60)
   (excali-test--drag 11 11 11 11)
   (should-not excali--multi-element)
   (let ((pts (excali--get (excali-create-test--last) 'points)))
     (should (equal (aref pts (1- (length pts))) [0.0 0.0])))))

(ert-deftest excali-create-test-return-finishes-and-escape-discards-stub ()
  "RET finishes a multi-point arrow; an arrow with one point is dropped."
  (excali-create-test--in-window
   (setq excali--tool 'arrow)
   (excali-test--drag 10 10 10 10)
   (excali-test--drag 80 10 80 10)
   (excali-return)
   (should (= (length (excali--get (excali-create-test--last) 'points)) 2))
   (setq excali--tool 'arrow)
   (excali-test--drag 200 200 200 200)
   (excali-escape-dwim)
   (should (= (length excali--elements) 1))))

(ert-deftest excali-create-test-arrow-type-cycles ()
  "Choosing the arrow tool again cycles sharp, round and elbow arrows."
  (excali-create-test--in-window
   (excali-select-tool 'arrow)
   (excali-select-tool 'arrow)
   (should (equal (excali--style-value 'arrowType) "elbow"))
   (should (eq (excali--roundness-for "arrow") :null))
   (excali-select-tool 'arrow)
   (should (equal (excali--style-value 'arrowType) "sharp"))
   (should (eq (excali--roundness-for "arrow") :null))
   (excali-select-tool 'arrow)
   (should (equal (excali--roundness-for "arrow") '((type . 2))))))

(ert-deftest excali-create-test-freedraw-dot-keeps-tool ()
  "A freedraw click leaves a dot and the pen stays active."
  (excali-create-test--in-window
   (setq excali--tool 'freedraw)
   (excali-test--drag 30 30 30 30)
   (should (= (length (excali--get (excali-create-test--last) 'points)) 2))
   (should (eq excali--tool 'freedraw))
   (should (null excali--selection))))

;;;; Actions

(ert-deftest excali-create-test-flip ()
  "Flipping mirrors positions about the selection center and the angle."
  (excali-test--with-scene
   (setq excali--backend nil)
   (let ((a (excali-test--rect 0 0 (cons 'angle 0.3)))
         (b (excali-test--rect 90 0)))
     (setq excali--elements (list a b))
     (excali--select (list a b))
     (excali-flip-horizontal)
     (should (= (excali--get a 'x) 90.0))
     (should (= (excali--get b 'x) 0.0))
     (should (< (abs (- (excali--get a 'angle) (- (* 2 float-pi) 0.3))) 1e-9)))))

(ert-deftest excali-create-test-align-and-distribute ()
  "Units align to the selection box and spread evenly."
  (excali-test--with-scene
   (setq excali--backend nil)
   (let ((a (excali-test--rect 0 0)) (b (excali-test--rect 15 30)) (c (excali-test--rect 100 5)))
     (setq excali--elements (list a b c))
     (excali--select (list a b c))
     (excali-align-top)
     (should (equal (mapcar (lambda (e) (excali--get e 'y)) (list a b c)) '(0.0 0.0 0.0)))
     (excali-distribute-horizontally)
     ;; Span 0..110 with 30 of boxes: gaps of 40.
     (should (equal (mapcar (lambda (e) (excali--get e 'x)) (list a b c)) '(0.0 50.0 100.0))))))

(ert-deftest excali-create-test-lock ()
  "Locked elements are neither hit nor box-selected."
  (excali-test--with-scene
   (setq excali--backend nil)
   (let ((a (excali-test--rect 0 0 (cons 'backgroundColor "#ffc9c9"))))
     (setq excali--elements (list a))
     (excali--select (list a))
     (excali-toggle-lock)
     (should (eq (alist-get 'locked a) t))
     (should-not (excali--hit '(5.0 . 5.0)))
     (should-not (excali--marquee-selection '(-5 -5 50 50)))
     (excali-unlock-all)
     (should (eq (alist-get 'locked a) :false))
     (should (eq (excali--hit '(5.0 . 5.0)) a)))))

(ert-deftest excali-create-test-styles-fonts-convert ()
  "Copy/paste styles, font size steps and shape conversion."
  (excali-test--with-scene
   (setq excali--backend nil)
   (let ((a (excali-test--rect 0 0 (cons 'strokeColor "#e03131") (cons 'roundness '((type . 3)))))
         (b (excali-test--rect 20 0))
         (label (excali--make-text-element 0 30 "Hi")))
     (setq excali--elements (list a b label))
     (excali--select (list a))
     (excali-copy-styles)
     (excali--select (list b label))
     (excali-paste-styles)
     (should (equal (excali--get b 'strokeColor) "#e03131"))
     (should (equal (excali--get label 'strokeColor) "#e03131"))
     (excali-increase-font-size)
     (should (= (excali--get label 'fontSize) 22))
     (excali--select (list a))
     (excali-convert-type)
     (should (equal (excali--get a 'type) "diamond"))
     (excali-convert-type)
     (should (equal (excali--get a 'type) "ellipse"))
     (should (eq (alist-get 'roundness a) :null)))))

(ert-deftest excali-create-test-zoom-to-fit ()
  "Zoom to fit centers the scene and never zooms in past 100%."
  (excali-test--with-scene
   (setq excali--backend nil excali--canvas-size '(400 . 300))
   (let ((a (excali-test--rect 0 0)) (b (excali-test--rect 990 490)))
     (setq excali--elements (list a b))
     (excali-zoom-to-fit)
     (should (< (abs (- excali--zoom (* 0.9 (/ 400.0 1000)))) 1e-9))
     (should (< (abs (- (* excali--zoom (+ 500 excali--scroll-x)) 200)) 1e-6))
     (setq excali--elements (list a))
     (excali-zoom-to-fit)
     (should (= excali--zoom 1.0))
     (excali--select (list a))
     (excali-zoom-to-fit-selection)
     (should (> excali--zoom 1.0)))))

(provide 'excali-create-test)
;;; excali-create-test.el ends here
