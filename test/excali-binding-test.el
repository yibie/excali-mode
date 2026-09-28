;;; excali-binding-test.el --- Arrow binding  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)
(require 'excali-test)

(defun excali-binding-test--box (x y &rest props)
  "Return a 100x60 rectangle at X, Y with stroke width 2 and PROPS."
  (apply #'excali--make-element "rectangle" x y (cons 'width 100.0) (cons 'height 60.0)
         (cons 'strokeWidth 2) props))

(defun excali-binding-test--near (a b &optional eps)
  "Return non-nil if conses or numbers A and B agree within EPS."
  (let ((eps (or eps 1e-6)))
    (if (consp a)
        (and (< (abs (- (car a) (car b))) eps) (< (abs (- (cdr a) (cdr b))) eps))
      (< (abs (- a b)) eps))))

(ert-deftest excali-binding-test-distance ()
  "The reach is 15 at zoom 1, grows when zoomed out, and caps at 30."
  (excali-test--with-scene
   (should (= (excali--max-binding-distance) 15.0))
   (setq excali--zoom 0.5)
   (should (= (excali--max-binding-distance) 20.0))
   (setq excali--zoom 0.1)
   (should (= (excali--max-binding-distance) 30.0))
   (setq excali--zoom 3.0)
   (should (= (excali--max-binding-distance) 15.0))))

(ert-deftest excali-binding-test-candidates ()
  "Near the border or inside binds; far away does not; opaque shapes occlude."
  (excali-test--with-scene
   (let ((box (excali-binding-test--box 100 100)))
     (setq excali--elements (list box))
     (should (eq (excali--binding-candidate '(90.0 . 130.0)) box))
     (should (eq (excali--binding-candidate '(150.0 . 130.0)) box))
     (should-not (excali--binding-candidate '(70.0 . 130.0)))
     ;; A filled shape on top hides the one below at points it covers.
     (let ((cover (excali-binding-test--box 120 110 (cons 'width 60.0) (cons 'height 40.0)
                                           (cons 'backgroundColor "#ffc9c9"))))
       (setq excali--elements (list box cover))
       (should (eq (excali--binding-candidate '(150.0 . 130.0)) cover))))))

(ert-deftest excali-binding-test-fixed-points ()
  "Fixed points are ratios of the unrotated box, rotated back to the scene."
  (excali-test--with-scene
   (let ((box (excali-binding-test--box 0 0 (cons 'angle (/ float-pi 2)))))
     (let* ((p '(50.0 . -10.0))
            (fixed (excali--fixed-point box p)))
       (should (excali-binding-test--near (excali--global-fixed-point box fixed) p))))))

(ert-deftest excali-binding-test-orbit ()
  "Orbit ends sit on the outline grown by the gap, facing the other end."
  (excali-test--with-scene
   (let ((box (excali-binding-test--box 100 100)))
     ;; From the center toward a point far left: exits at x1 - (5 + 1).
     (should (excali-binding-test--near
              (excali--orbit-point box '(150.0 . 130.0) '(0.0 . 130.0))
              '(94.0 . 130.0))))))

(defmacro excali-binding-test--scene (&rest body)
  "Run BODY in a window-backed scene with a 100x60 box at 100,100."
  `(excali-test--in-window
    (excali--load-current-style nil)
    (let ((box (excali-binding-test--box 100 100)))
      (setq excali--elements (list box) excali--tool-locked nil excali--multi-element nil)
      ,@body)))

(ert-deftest excali-binding-test-draw-arrow-binds ()
  "Drawing an arrow that ends near a shape binds and snaps it."
  (excali-binding-test--scene
   (setq excali--tool 'arrow)
   (excali-test--drag 10 130 95 130)
   (let ((arrow (car (last excali--elements))))
     (should (equal (alist-get 'elementId (excali--get arrow 'endBinding))
                    (excali--get box 'id)))
     (should (equal (alist-get 'mode (excali--get arrow 'endBinding)) "orbit"))
     (should-not (excali--get arrow 'startBinding))
     (should (equal (alist-get 'id (aref (excali--get box 'boundElements) 0))
                    (excali--get arrow 'id)))
     (should (excali-binding-test--near (excali--arrow-point arrow 1) '(94.0 . 130.0) 0.5))
     (should-not excali--binding-highlight))))

(ert-deftest excali-binding-test-moving-shape-drags-arrow ()
  "Moving a bound shape re-routes the arrow end."
  (excali-binding-test--scene
   (setq excali--tool 'arrow)
   (excali-test--drag 10 130 95 130)
   (let ((arrow (car (last excali--elements))))
     (setq excali--tool 'select)
     (excali--select (list box))
     (excali--nudge 0 50)
     ;; The end stays on the grown left edge, aimed from the moved fixed
     ;; point back toward the start, so it rises slightly above y = 180.
     (let ((end (excali--arrow-point arrow 1)))
       (should (excali-binding-test--near (car end) 94.0 0.5))
       (should (< 175.0 (cdr end) 180.0)))
     ;; The unbound start did not move.
     (should (excali-binding-test--near (excali--arrow-point arrow 0) '(10.0 . 130.0))))))

(ert-deftest excali-binding-test-drag-arrow-unbinds ()
  "A small drag keeps a lone bound arrow; a real drag moves and unbinds it."
  (excali-binding-test--scene
   (setq excali--tool 'arrow)
   (excali-test--drag 10 130 95 130)
   (let ((arrow (car (last excali--elements))))
     (setq excali--tool 'select)
     (excali--select (list arrow))
     (excali-test--drag 40 130 45 130)
     (should (excali--get arrow 'endBinding))
     (should (excali-binding-test--near (excali--arrow-point arrow 0) '(10.0 . 130.0)))
     (excali-test--drag 40 130 40 200)
     (should-not (excali--get arrow 'endBinding))
     (should (eq (alist-get 'boundElements box) :null)))))

(ert-deftest excali-binding-test-delete-shape-clears-binding ()
  "Deleting a bound shape leaves the arrow unbound."
  (excali-binding-test--scene
   (setq excali--tool 'arrow)
   (excali-test--drag 10 130 95 130)
   (let ((arrow (car (last excali--elements))))
     (excali--select (list box))
     (excali-delete-selected)
     (should-not (excali--get arrow 'endBinding)))))

(ert-deftest excali-binding-test-lines-do-not-bind ()
  "Lines never bind."
  (excali-binding-test--scene
   (setq excali--tool 'line)
   (excali-test--drag 10 130 95 130)
   (should-not (excali--get (car (last excali--elements)) 'endBinding))))

(ert-deftest excali-binding-test-orbit-default-inside-with-meta ()
  "Starting inside a shape orbits its outline; meta binds inside."
  (excali-binding-test--scene
   (setq excali--tool 'arrow)
   (excali-test--drag 150 130 300 130)
   (let ((arrow (car (last excali--elements))))
     (should (equal (alist-get 'mode (excali--get arrow 'startBinding)) "orbit"))
     ;; The start leaves the box on its grown right edge.
     (should (excali-binding-test--near (car (excali--arrow-point arrow 0)) 206.0 0.5)))
   (setq excali--tool 'arrow)
   (excali-test--drag 150 130 300 130 '(meta))
   (let ((arrow (car (last excali--elements))))
     (should (equal (alist-get 'mode (excali--get arrow 'startBinding)) "inside"))
     (should (excali-binding-test--near (excali--arrow-point arrow 0) '(150.0 . 130.0))))))

(provide 'excali-binding-test)
;;; excali-binding-test.el ends here
