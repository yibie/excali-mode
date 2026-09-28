;;; excal-transform-test.el --- Selection UI, handles and transforms  -*- lexical-binding: t; -*-

(require 'ert)
(require 'excal)
(require 'excal-test)

(defun excal-transform-test--rect (x y w h &rest props)
  "Return a W by H rectangle at X, Y with extra PROPS alist."
  (apply #'excal--make-element "rectangle" x y
         (cons 'width (float w)) (cons 'height (float h)) props))

(defmacro excal-transform-test--with (bindings &rest body)
  "Bind BINDINGS to elements forming the scene at zoom 1, then run BODY."
  (declare (indent 1))
  `(excal-test--with-scene
    (let* ,bindings
      (setq excal--elements (list ,@(mapcar #'car bindings))
            excal--selection nil excal--editing-group nil excal--marquee nil)
      ,@body)))

(defun excal-transform-test--near (a b)
  "Return non-nil if numbers or conses A and B agree to 1e-6."
  (if (consp a)
      (and (excal-transform-test--near (car a) (car b))
           (excal-transform-test--near (cdr a) (cdr b)))
    (< (abs (- a b)) 1e-6)))

(defun excal-transform-test--corner (element corner)
  "Return the on-screen position of ELEMENT's CORNER (nw, ne, sw, se)."
  (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excal--element-box element)))
    (excal--rotate-point (cons (if (memq corner '(nw sw)) x1 x2)
                               (if (memq corner '(nw ne)) y1 y2))
                         (excal--box-center (excal--element-box element))
                         (excal--element-angle element))))

;;;; Handles

(ert-deftest excal-transform-test-handle-geometry ()
  "Handle positions follow getTransformHandlesFromCoords."
  (excal-transform-test--with ((r (excal-transform-test--rect 10 20 100 50)))
    (let ((handles (excal--transform-handles (excal--element-box r) 0.0 2)))
      ;; Spec worked example: margin 2 puts the nw square at [x1-8, x1].
      (should (equal (cdr (assq 'nw handles)) '(2.0 12.0 8.0 8.0)))
      (should (equal (cdr (assq 'se handles)) '(110.0 70.0 8.0 8.0)))
      ;; The rotation handle is centered and 16 px above the nw row.
      (should (equal (cdr (assq 'rotation handles)) '(56.0 -4.0 8.0 8.0)))
      (should-not (assq 'n handles)))
    ;; Multi-selections use margin 4.
    (let ((handles (excal--transform-handles '(0 0 10 10) 0.0 4)))
      (should (equal (car (cdr (assq 'nw handles))) -10.0)))))

(ert-deftest excal-transform-test-handle-at ()
  "Rotation, corners and the side band are found in upstream order."
  (excal-transform-test--with ((r (excal-transform-test--rect 10 20 100 50)))
    (excal--select (list r))
    (should (eq (excal--handle-at '(60.0 . 0.0)) 'rotation))
    (should (eq (excal--handle-at '(5.0 . 15.0)) 'nw))
    (should (eq (excal--handle-at '(114.0 . 74.0)) 'se))
    ;; The border band, 4 px outside the box, resizes sides.
    (should (eq (excal--handle-at '(60.0 . 16.0)) 'n))
    (should (eq (excal--handle-at '(114.0 . 45.0)) 'e))
    (should (eq (excal--handle-at '(60.0 . 74.0)) 's))
    (should (eq (excal--handle-at '(6.0 . 45.0)) 'w))
    (should-not (excal--handle-at '(60.0 . 45.0)))
    ;; Rotated a quarter turn, the n edge lies to the right.
    (excal--put r 'angle (/ float-pi 2))
    (should (eq (excal--handle-at '(89.0 . 45.0)) 'n))))

(ert-deftest excal-transform-test-two-point-line-has-no-handles ()
  "A lone two-point arrow shows its endpoints instead of a box."
  (excal-transform-test--with
      ((a (excal--make-element "arrow" 0 0 (cons 'points [[0.0 0.0] [50.0 0.0]]))))
    (excal--linear-extent a)
    (excal--select (list a))
    (should-not (excal--transform-target))
    (should (equal (mapcar (lambda (v) (aref v 0)) (excal--overlay-natives))
                   '("ov-circle" "ov-circle")))))

(ert-deftest excal-transform-test-overlays ()
  "Borders, group boxes, the multi-selection box and handles."
  (excal-transform-test--with
      ((a (excal-transform-test--rect 0 0 10 10))
       (b (excal-transform-test--rect 20 0 10 10 (cons 'groupIds ["g"])))
       (c (excal-transform-test--rect 40 0 10 10 (cons 'groupIds ["g"]))))
    (cl-flet ((kinds () (mapcar (lambda (v) (list (aref v 0) (aref v 17)))
                                (excal--overlay-natives))))
      (excal--select (list a))
      (should (equal (kinds) '(("ov-rect" "solid") ("ov-handle" "solid")
                               ("ov-handle" "solid") ("ov-handle" "solid")
                               ("ov-handle" "solid") ("ov-circle" "solid"))))
      ;; A group gets one dashed box; its members get no border of their own.
      (excal--select (excal--unit b))
      (should (equal (seq-take (kinds) 2) '(("ov-rect" "dashed") ("ov-rect" "dotted"))))
      (excal--select (list a) t)
      (should (equal (seq-count (lambda (k) (equal k '("ov-rect" "solid"))) (kinds)) 1))
      (setq excal--marquee '(0 0 5 5))
      (should (equal (car (last (kinds))) '("ov-rect" "solid"))))))

;;;; Resizing a lone element

(ert-deftest excal-transform-test-resize-single ()
  "Free, aspect-locked, from-center and flipping resizes."
  (excal-transform-test--with ((r (excal-transform-test--rect 0 0 100 50)))
    (let ((g (excal--snapshot-geometry r)))
      (cl-flet ((box () (list (excal--get r 'x) (excal--get r 'y)
                              (excal--get r 'width) (excal--get r 'height))))
        (excal--resize-single r g 'se '(130.0 . 60.0))
        (should (equal (box) '(0.0 0.0 130.0 60.0)))
        (excal--resize-single r g 'se '(200.0 . 60.0) t)
        (should (equal (box) '(0.0 0.0 200.0 100.0)))
        (excal--resize-single r g 'e '(120.0 . 25.0) nil t)
        (should (equal (box) '(-20.0 0.0 140.0 50.0)))
        ;; Dragging the w edge past e flips the box.
        (excal--resize-single r g 'w '(150.0 . 25.0))
        (should (equal (box) '(100.0 0.0 50.0 50.0)))))))

(ert-deftest excal-transform-test-resize-rotated-keeps-anchor ()
  "Resizing a rotated element keeps the opposite corner fixed on screen."
  (excal-transform-test--with
      ((r (excal-transform-test--rect 0 0 100 50 (cons 'angle 0.7))))
    (let ((anchor (excal-transform-test--corner r 'nw))
          (g (excal--snapshot-geometry r))
          (target (excal--rotate-point '(150.0 . 90.0) '(50.0 . 25.0) 0.7)))
      (excal--resize-single r g 'se target)
      (should (excal-transform-test--near (excal-transform-test--corner r 'nw) anchor))
      (should (excal-transform-test--near (excal-transform-test--corner r 'se) target))
      (should (excal-transform-test--near (excal--get r 'width) 150.0)))))

(ert-deftest excal-transform-test-resize-arrow-points ()
  "Point-based elements scale every point."
  (excal-transform-test--with
      ((a (excal--make-element "arrow" 0 0
                               (cons 'points [[0.0 0.0] [50.0 25.0] [100.0 0.0]]))))
    (excal--linear-extent a)
    (excal--resize-single a (excal--snapshot-geometry a) 'e '(200.0 . 10.0))
    (should (equal (excal--get a 'points) [[0.0 0.0] [100.0 25.0] [200.0 0.0]]))))

(ert-deftest excal-transform-test-resize-text ()
  "Text corners scale the font, anchored at the opposite corner."
  (excal-transform-test--with ((tx (excal--make-text-element 0 0 "Hello")))
    (let ((g (excal--snapshot-geometry tx)))
      (should (= (excal--get tx 'height) 25.0))
      (excal--resize-single tx g 'se (cons (excal--get tx 'width) 50.0))
      (should (= (excal--get tx 'fontSize) 40.0))
      (should (= (excal--get tx 'x) 0.0))
      (excal--resize-single tx g 'nw (cons 0.0 -25.0))
      (should (= (excal--get tx 'fontSize) 40.0))
      (should (= (+ (excal--get tx 'y) (excal--get tx 'height)) 25.0)))))

;;;; Several elements

(ert-deftest excal-transform-test-resize-multiple ()
  "Several elements scale within the common box."
  (excal-transform-test--with
      ((a (excal-transform-test--rect 0 0 10 10))
       (b (excal-transform-test--rect 30 30 10 10)))
    (let ((geometries (mapcar (lambda (e) (cons e (excal--snapshot-geometry e)))
                              (list a b))))
      (excal--resize-multiple geometries '(0 0 40 40) 'se '(80.0 . 80.0))
      (should (equal (list (excal--get b 'x) (excal--get b 'y) (excal--get b 'width))
                     '(60.0 60.0 20.0)))
      ;; Non-uniform when nothing forces the aspect ratio.
      (excal--resize-multiple geometries '(0 0 40 40) 'e '(80.0 . 20.0))
      (should (equal (list (excal--get b 'width) (excal--get b 'height)) '(20.0 10.0))))))

;;;; Rotation

(ert-deftest excal-transform-test-rotate-single ()
  "Rotation is absolute, zero with the handle straight up, shift snaps."
  (excal-transform-test--with ((r (excal-transform-test--rect 0 0 100 50)))
    (let ((g (excal--snapshot-geometry r)))
      (excal--rotate-single r g '(200.0 . 25.0))
      (should (excal-transform-test--near (excal--get r 'angle) (/ float-pi 2)))
      (excal--rotate-single r g '(50.0 . -100.0))
      (should (excal-transform-test--near (excal--get r 'angle) 0.0))
      ;; 50 degrees snaps to 45.
      (let ((a (degrees-to-radians 40)))
        (excal--rotate-single r g (cons (+ 50 (* 100 (cos a))) (- 25 (* 100 (sin a)))) t)
        (should (excal-transform-test--near (excal--get r 'angle)
                                            (degrees-to-radians 45)))))))

(ert-deftest excal-transform-test-rotate-multiple ()
  "Several elements orbit the common center and turn by the same angle."
  (excal-transform-test--with
      ((a (excal-transform-test--rect 0 0 10 10))
       (b (excal-transform-test--rect 30 0 10 10)))
    (let ((geometries (mapcar (lambda (e) (cons e (excal--snapshot-geometry e)))
                              (list a b))))
      ;; Common box (0 0 40 10), center (20 . 5); a quarter turn.
      (excal--rotate-multiple geometries '(20.0 . 5.0) '(40.0 . 5.0) '(20.0 . 25.0))
      (should (excal-transform-test--near (excal--box-center (excal--element-box b))
                                          '(20.0 . 20.0)))
      (should (excal-transform-test--near (excal--get a 'angle) (/ float-pi 2))))))

;;;; Gestures

(ert-deftest excal-transform-test-gesture-resize-from-edge ()
  "Dragging the border band of a selected rectangle resizes that side."
  (excal-test--in-window
   (let ((r (excal-transform-test--rect 10 10 50 30)))
     (setq excal--elements (list r))
     (excal--select (list r))
     ;; The e edge band sits at x = 60 + 4.
     (excal-test--drag 64 25 84 25)
     (should (equal (list (excal--get r 'x) (excal--get r 'width)) '(10.0 70.0)))
     (should (equal excal--selection (list r))))))

(ert-deftest excal-transform-test-gesture-rotate ()
  "Dragging the rotation handle rotates the selection."
  (excal-test--in-window
   (let ((r (excal-transform-test--rect 10 30 50 30)))
     (setq excal--elements (list r))
     (excal--select (list r))
     ;; Rotation handle center: (35, 30 - 4 - 16) = (35 . 10).
     (excal-test--drag 35 10 100 45)
     (should (excal-transform-test--near (excal--get r 'angle) (/ float-pi 2))))))

;;; excal-transform-test.el ends here
