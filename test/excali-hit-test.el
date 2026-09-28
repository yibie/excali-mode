;;; excali-hit-test.el --- Hit testing  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)
(require 'excali-test)

(defun excali-hit-test--shape (type &rest props)
  "Return a 100x50 TYPE element at 0,0 with extra PROPS alist."
  (apply #'excali--make-element type 0 0 (cons 'width 100.0) (cons 'height 50.0)
         (cons 'strokeWidth 2) props))

(ert-deftest excali-hit-test-transparent-shapes-hit-on-stroke ()
  "Unfilled shapes are hit near the stroke only; filled ones inside too."
  (excali-test--with-scene
   (dolist (type '("rectangle" "ellipse" "diamond"))
     (let ((e (excali-hit-test--shape type)))
       (should-not (excali--hit-element-p e '(50.0 . 25.0)))
       (should-not (excali--hit-element-p e '(200.0 . 25.0)))
       (excali--put e 'backgroundColor "#ffc9c9")
       (should (excali--hit-element-p e '(50.0 . 25.0)))))
   (let ((r (excali-hit-test--shape "rectangle")))
     ;; On the stroke, and within the ~6.8 px threshold of it.
     (should (excali--hit-element-p r '(50.0 . 0.0)))
     (should (excali--hit-element-p r '(50.0 . 6.0)))
     (should-not (excali--hit-element-p r '(50.0 . 9.0)))
     ;; The threshold is in screen px: zooming in shrinks it in scene units.
     (setq excali--zoom 4.0)
     (should-not (excali--hit-element-p r '(50.0 . 3.0))))
   (let ((e (excali-hit-test--shape "ellipse")))
     (should (excali--hit-element-p e '(100.0 . 25.0)))
     ;; The box corner is well outside the ellipse outline.
     (should-not (excali--hit-element-p e '(2.0 . 2.0))))))

(ert-deftest excali-hit-test-rotated ()
  "Hit tests run in the element's rotated frame."
  (excali-test--with-scene
   (let ((r (excali-hit-test--shape "rectangle" (cons 'angle (/ float-pi 2))
                                   (cons 'backgroundColor "#ffc9c9"))))
     ;; A quarter turn about (50, 25) makes it 50 wide and 100 tall.
     (should (excali--hit-element-p r '(50.0 . -20.0)))
     (should-not (excali--hit-element-p r '(5.0 . 25.0))))))

(ert-deftest excali-hit-test-linear ()
  "Arrows hit on the stroke only; closed filled lines inside too."
  (excali-test--with-scene
   (let ((arrow (excali--make-element "arrow" 0 0 (cons 'points [[0.0 0.0] [100.0 0.0]]))))
     (should (excali--hit-element-p arrow '(50.0 . 3.0)))
     (should-not (excali--hit-element-p arrow '(50.0 . 20.0))))
   (let ((loop (excali--make-element
                "line" 0 0 (cons 'backgroundColor "#b2f2bb")
                (cons 'points [[0.0 0.0] [100.0 0.0] [100.0 100.0] [0.0 100.0] [0.0 0.0]]))))
     (should (excali--hit-element-p loop '(50.0 . 50.0)))
     (excali--put loop 'backgroundColor "transparent")
     (should-not (excali--hit-element-p loop '(50.0 . 50.0))))))

(ert-deftest excali-hit-test-text-and-bound-text ()
  "Text is hit anywhere in its box; bound text counts as its container."
  (excali-test--with-scene
   (let* ((box (excali-hit-test--shape "rectangle"
                                      (cons 'boundElements [((id . "t") (type . "text"))])))
          (label (excali--make-text-element 30 15 "Hi")))
     (excali--put label 'id "t")
     (excali--put label 'containerId (excali--get box 'id))
     (setq excali--elements (list box label))
     ;; A container with a label is draggable from inside.
     (should (eq (excali--hit '(10.0 . 25.0)) box))
     (should (eq (excali--hit '(35.0 . 25.0)) box))
     ;; Bound text is not box-selected on its own.
     (should (equal (excali--marquee-selection '(-10 -10 200 100)) (list box))))))

(ert-deftest excali-hit-test-topmost-half-threshold ()
  "A stroke barely touched on top yields to a clear hit below."
  (excali-test--with-scene
   (let ((below (excali-hit-test--shape "rectangle" (cons 'backgroundColor "#ffc9c9")))
         (above (excali--make-element "rectangle" 0 30 (cons 'width 100.0)
                                     (cons 'height 50.0) (cons 'strokeWidth 2))))
     (setq excali--elements (list below above))
     ;; 5 px from the upper stroke: within its threshold, not within half.
     (should (eq (excali--hit '(50.0 . 25.0)) below))
     (should (eq (excali--hit '(50.0 . 31.0)) above)))))

(provide 'excali-hit-test)
;;; excali-hit-test.el ends here
