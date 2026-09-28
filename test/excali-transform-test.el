;;; excali-transform-test.el --- Selection UI, handles and transforms  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)
(require 'excali-test)

(defun excali-transform-test--rect (x y w h &rest props)
  "Return a W by H rectangle at X, Y with extra PROPS alist."
  (apply #'excali--make-element "rectangle" x y
         (cons 'width (float w)) (cons 'height (float h)) props))

(defmacro excali-transform-test--with (bindings &rest body)
  "Bind BINDINGS to elements forming the scene at zoom 1, then run BODY."
  (declare (indent 1))
  `(excali-test--with-scene
    (let* ,bindings
      (setq excali--elements (list ,@(mapcar #'car bindings))
            excali--selection nil excali--editing-group nil excali--marquee nil)
      ,@body)))

(defun excali-transform-test--near (a b)
  "Return non-nil if numbers or conses A and B agree to 1e-6."
  (if (consp a)
      (and (excali-transform-test--near (car a) (car b))
           (excali-transform-test--near (cdr a) (cdr b)))
    (< (abs (- a b)) 1e-6)))

(defun excali-transform-test--corner (element corner)
  "Return the on-screen position of ELEMENT's CORNER (nw, ne, sw, se)."
  (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excali--element-box element)))
    (excali--rotate-point (cons (if (memq corner '(nw sw)) x1 x2)
                               (if (memq corner '(nw ne)) y1 y2))
                         (excali--box-center (excali--element-box element))
                         (excali--element-angle element))))

;;;; Handles

(ert-deftest excali-transform-test-handle-geometry ()
  "Handle positions follow getTransformHandlesFromCoords."
  (excali-transform-test--with ((r (excali-transform-test--rect 10 20 100 50)))
    (let ((handles (excali--transform-handles (excali--element-box r) 0.0 2)))
      ;; Spec worked example: margin 2 puts the nw square at [x1-8, x1].
      (should (equal (cdr (assq 'nw handles)) '(2.0 12.0 8.0 8.0)))
      (should (equal (cdr (assq 'se handles)) '(110.0 70.0 8.0 8.0)))
      ;; The rotation handle is centered and 16 px above the nw row.
      (should (equal (cdr (assq 'rotation handles)) '(56.0 -4.0 8.0 8.0)))
      (should-not (assq 'n handles)))
    ;; Multi-selections use margin 4.
    (let ((handles (excali--transform-handles '(0 0 10 10) 0.0 4)))
      (should (equal (car (cdr (assq 'nw handles))) -10.0)))))

(ert-deftest excali-transform-test-handle-at ()
  "Rotation, corners and the side band are found in upstream order."
  (excali-transform-test--with ((r (excali-transform-test--rect 10 20 100 50)))
    (excali--select (list r))
    (should (eq (excali--handle-at '(60.0 . 0.0)) 'rotation))
    (should (eq (excali--handle-at '(5.0 . 15.0)) 'nw))
    (should (eq (excali--handle-at '(114.0 . 74.0)) 'se))
    ;; The border band, 4 px outside the box, resizes sides.
    (should (eq (excali--handle-at '(60.0 . 16.0)) 'n))
    (should (eq (excali--handle-at '(114.0 . 45.0)) 'e))
    (should (eq (excali--handle-at '(60.0 . 74.0)) 's))
    (should (eq (excali--handle-at '(6.0 . 45.0)) 'w))
    (should-not (excali--handle-at '(60.0 . 45.0)))
    ;; Rotated a quarter turn, the n edge lies to the right.
    (excali--put r 'angle (/ float-pi 2))
    (should (eq (excali--handle-at '(89.0 . 45.0)) 'n))))

(ert-deftest excali-transform-test-two-point-line-has-no-handles ()
  "A lone two-point arrow shows its endpoints instead of a box."
  (excali-transform-test--with
      ((a (excali--make-element "arrow" 0 0 (cons 'points [[0.0 0.0] [50.0 0.0]]))))
    (excali--linear-extent a)
    (excali--select (list a))
    (should-not (excali--transform-target))
    ;; Its segment midpoint plus both ends.
    (should (equal (mapcar (lambda (v) (aref v 0)) (excali--overlay-natives))
                   '("ov-circle" "ov-circle" "ov-circle")))))

(ert-deftest excali-transform-test-overlays ()
  "Borders, group boxes, the multi-selection box and handles."
  (excali-transform-test--with
      ((a (excali-transform-test--rect 0 0 10 10))
       (b (excali-transform-test--rect 20 0 10 10 (cons 'groupIds ["g"])))
       (c (excali-transform-test--rect 40 0 10 10 (cons 'groupIds ["g"]))))
    (cl-flet ((kinds () (mapcar (lambda (v) (list (aref v 0) (aref v 17)))
                                (excali--overlay-natives))))
      (excali--select (list a))
      (should (equal (kinds) '(("ov-rect" "solid") ("ov-handle" "solid")
                               ("ov-handle" "solid") ("ov-handle" "solid")
                               ("ov-handle" "solid") ("ov-circle" "solid"))))
      ;; A group gets one dashed box; its members get no border of their own.
      (excali--select (excali--unit b))
      (should (equal (seq-take (kinds) 2) '(("ov-rect" "dashed") ("ov-rect" "dotted"))))
      (excali--select (list a) t)
      (should (equal (seq-count (lambda (k) (equal k '("ov-rect" "solid"))) (kinds)) 1))
      (setq excali--marquee '(0 0 5 5))
      (should (equal (car (last (kinds))) '("ov-rect" "solid"))))))

;;;; Resizing a lone element

(ert-deftest excali-transform-test-resize-single ()
  "Free, aspect-locked, from-center and flipping resizes."
  (excali-transform-test--with ((r (excali-transform-test--rect 0 0 100 50)))
    (let ((g (excali--snapshot-geometry r)))
      (cl-flet ((box () (list (excali--get r 'x) (excali--get r 'y)
                              (excali--get r 'width) (excali--get r 'height))))
        (excali--resize-single r g 'se '(130.0 . 60.0))
        (should (equal (box) '(0.0 0.0 130.0 60.0)))
        (excali--resize-single r g 'se '(200.0 . 60.0) t)
        (should (equal (box) '(0.0 0.0 200.0 100.0)))
        (excali--resize-single r g 'e '(120.0 . 25.0) nil t)
        (should (equal (box) '(-20.0 0.0 140.0 50.0)))
        ;; Dragging the w edge past e flips the box.
        (excali--resize-single r g 'w '(150.0 . 25.0))
        (should (equal (box) '(100.0 0.0 50.0 50.0)))))))

(ert-deftest excali-transform-test-resize-rotated-keeps-anchor ()
  "Resizing a rotated element keeps the opposite corner fixed on screen."
  (excali-transform-test--with
      ((r (excali-transform-test--rect 0 0 100 50 (cons 'angle 0.7))))
    (let ((anchor (excali-transform-test--corner r 'nw))
          (g (excali--snapshot-geometry r))
          (target (excali--rotate-point '(150.0 . 90.0) '(50.0 . 25.0) 0.7)))
      (excali--resize-single r g 'se target)
      (should (excali-transform-test--near (excali-transform-test--corner r 'nw) anchor))
      (should (excali-transform-test--near (excali-transform-test--corner r 'se) target))
      (should (excali-transform-test--near (excali--get r 'width) 150.0)))))

(ert-deftest excali-transform-test-resize-arrow-points ()
  "Point-based elements scale every point."
  (excali-transform-test--with
      ((a (excali--make-element "arrow" 0 0
                               (cons 'points [[0.0 0.0] [50.0 25.0] [100.0 0.0]]))))
    (excali--linear-extent a)
    (excali--resize-single a (excali--snapshot-geometry a) 'e '(200.0 . 10.0))
    (should (equal (excali--get a 'points) [[0.0 0.0] [100.0 25.0] [200.0 0.0]]))))

(ert-deftest excali-transform-test-resize-text ()
  "Text corners scale the font, anchored at the opposite corner."
  (excali-transform-test--with ((tx (excali--make-text-element 0 0 "Hello")))
    (let ((g (excali--snapshot-geometry tx)))
      (should (= (excali--get tx 'height) 25.0))
      (excali--resize-single tx g 'se (cons (excali--get tx 'width) 50.0))
      (should (= (excali--get tx 'fontSize) 40.0))
      (should (= (excali--get tx 'x) 0.0))
      (excali--resize-single tx g 'nw (cons 0.0 -25.0))
      (should (= (excali--get tx 'fontSize) 40.0))
      (should (= (+ (excali--get tx 'y) (excali--get tx 'height)) 25.0)))))

;;;; Several elements

(ert-deftest excali-transform-test-resize-multiple ()
  "Several elements scale within the common box."
  (excali-transform-test--with
      ((a (excali-transform-test--rect 0 0 10 10))
       (b (excali-transform-test--rect 30 30 10 10)))
    (let ((geometries (mapcar (lambda (e) (cons e (excali--snapshot-geometry e)))
                              (list a b))))
      (excali--resize-multiple geometries '(0 0 40 40) 'se '(80.0 . 80.0))
      (should (equal (list (excali--get b 'x) (excali--get b 'y) (excali--get b 'width))
                     '(60.0 60.0 20.0)))
      ;; Non-uniform when nothing forces the aspect ratio.
      (excali--resize-multiple geometries '(0 0 40 40) 'e '(80.0 . 20.0))
      (should (equal (list (excali--get b 'width) (excali--get b 'height)) '(20.0 10.0))))))

;;;; Rotation

(ert-deftest excali-transform-test-rotate-single ()
  "Rotation is absolute, zero with the handle straight up, shift snaps."
  (excali-transform-test--with ((r (excali-transform-test--rect 0 0 100 50)))
    (let ((g (excali--snapshot-geometry r)))
      (excali--rotate-single r g '(200.0 . 25.0))
      (should (excali-transform-test--near (excali--get r 'angle) (/ float-pi 2)))
      (excali--rotate-single r g '(50.0 . -100.0))
      (should (excali-transform-test--near (excali--get r 'angle) 0.0))
      ;; 50 degrees snaps to 45.
      (let ((a (degrees-to-radians 40)))
        (excali--rotate-single r g (cons (+ 50 (* 100 (cos a))) (- 25 (* 100 (sin a)))) t)
        (should (excali-transform-test--near (excali--get r 'angle)
                                            (degrees-to-radians 45)))))))

(ert-deftest excali-transform-test-rotate-multiple ()
  "Several elements orbit the common center and turn by the same angle."
  (excali-transform-test--with
      ((a (excali-transform-test--rect 0 0 10 10))
       (b (excali-transform-test--rect 30 0 10 10)))
    (let ((geometries (mapcar (lambda (e) (cons e (excali--snapshot-geometry e)))
                              (list a b))))
      ;; Common box (0 0 40 10), center (20 . 5); a quarter turn.
      (excali--rotate-multiple geometries '(20.0 . 5.0) '(40.0 . 5.0) '(20.0 . 25.0))
      (should (excali-transform-test--near (excali--box-center (excali--element-box b))
                                          '(20.0 . 20.0)))
      (should (excali-transform-test--near (excali--get a 'angle) (/ float-pi 2))))))

;;;; Gestures

(ert-deftest excali-transform-test-gesture-resize-from-edge ()
  "Dragging the border band of a selected rectangle resizes that side."
  (excali-test--in-window
   (let ((r (excali-transform-test--rect 10 10 50 30)))
     (setq excali--elements (list r))
     (excali--select (list r))
     ;; The e edge band sits at x = 60 + 4.
     (excali-test--drag 64 25 84 25)
     (should (equal (list (excali--get r 'x) (excali--get r 'width)) '(10.0 70.0)))
     (should (equal excali--selection (list r))))))

(ert-deftest excali-transform-test-gesture-rotate ()
  "Dragging the rotation handle rotates the selection."
  (excali-test--in-window
   (let ((r (excali-transform-test--rect 10 30 50 30)))
     (setq excali--elements (list r))
     (excali--select (list r))
     ;; Rotation handle center: (35, 30 - 4 - 16) = (35 . 10).
     (excali-test--drag 35 10 100 45)
     (should (excali-transform-test--near (excali--get r 'angle) (/ float-pi 2))))))

(provide 'excali-transform-test)
;;; excali-transform-test.el ends here
