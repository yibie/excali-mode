;;; excal-binding-test.el --- Arrow binding  -*- lexical-binding: t; -*-

(require 'ert)
(require 'excal)
(require 'excal-test)

(defun excal-binding-test--box (x y &rest props)
  "Return a 100x60 rectangle at X, Y with stroke width 2 and PROPS."
  (apply #'excal--make-element "rectangle" x y (cons 'width 100.0) (cons 'height 60.0)
         (cons 'strokeWidth 2) props))

(defun excal-binding-test--near (a b &optional eps)
  "Return non-nil if conses or numbers A and B agree within EPS."
  (let ((eps (or eps 1e-6)))
    (if (consp a)
        (and (< (abs (- (car a) (car b))) eps) (< (abs (- (cdr a) (cdr b))) eps))
      (< (abs (- a b)) eps))))

(ert-deftest excal-binding-test-distance ()
  "The reach is 15 at zoom 1, grows when zoomed out, and caps at 30."
  (excal-test--with-scene
   (should (= (excal--max-binding-distance) 15.0))
   (setq excal--zoom 0.5)
   (should (= (excal--max-binding-distance) 20.0))
   (setq excal--zoom 0.1)
   (should (= (excal--max-binding-distance) 30.0))
   (setq excal--zoom 3.0)
   (should (= (excal--max-binding-distance) 15.0))))

(ert-deftest excal-binding-test-candidates ()
  "Near the border or inside binds; far away does not; opaque shapes occlude."
  (excal-test--with-scene
   (let ((box (excal-binding-test--box 100 100)))
     (setq excal--elements (list box))
     (should (eq (excal--binding-candidate '(90.0 . 130.0)) box))
     (should (eq (excal--binding-candidate '(150.0 . 130.0)) box))
     (should-not (excal--binding-candidate '(70.0 . 130.0)))
     ;; A filled shape on top hides the one below at points it covers.
     (let ((cover (excal-binding-test--box 120 110 (cons 'width 60.0) (cons 'height 40.0)
                                           (cons 'backgroundColor "#ffc9c9"))))
       (setq excal--elements (list box cover))
       (should (eq (excal--binding-candidate '(150.0 . 130.0)) cover))))))

(ert-deftest excal-binding-test-fixed-points ()
  "Fixed points are ratios of the unrotated box, rotated back to the scene."
  (excal-test--with-scene
   (let ((box (excal-binding-test--box 0 0 (cons 'angle (/ float-pi 2)))))
     (let* ((p '(50.0 . -10.0))
            (fixed (excal--fixed-point box p)))
       (should (excal-binding-test--near (excal--global-fixed-point box fixed) p))))))

(ert-deftest excal-binding-test-orbit ()
  "Orbit ends sit on the outline grown by the gap, facing the other end."
  (excal-test--with-scene
   (let ((box (excal-binding-test--box 100 100)))
     ;; From the center toward a point far left: exits at x1 - (5 + 1).
     (should (excal-binding-test--near
              (excal--orbit-point box '(150.0 . 130.0) '(0.0 . 130.0))
              '(94.0 . 130.0))))))

(defmacro excal-binding-test--scene (&rest body)
  "Run BODY in a window-backed scene with a 100x60 box at 100,100."
  `(excal-test--in-window
    (excal--load-current-style nil)
    (let ((box (excal-binding-test--box 100 100)))
      (setq excal--elements (list box) excal--tool-locked nil excal--multi-element nil)
      ,@body)))

(ert-deftest excal-binding-test-draw-arrow-binds ()
  "Drawing an arrow that ends near a shape binds and snaps it."
  (excal-binding-test--scene
   (setq excal--tool 'arrow)
   (excal-test--drag 10 130 95 130)
   (let ((arrow (car (last excal--elements))))
     (should (equal (alist-get 'elementId (excal--get arrow 'endBinding))
                    (excal--get box 'id)))
     (should (equal (alist-get 'mode (excal--get arrow 'endBinding)) "orbit"))
     (should-not (excal--get arrow 'startBinding))
     (should (equal (alist-get 'id (aref (excal--get box 'boundElements) 0))
                    (excal--get arrow 'id)))
     (should (excal-binding-test--near (excal--arrow-point arrow 1) '(94.0 . 130.0) 0.5))
     (should-not excal--binding-highlight))))

(ert-deftest excal-binding-test-moving-shape-drags-arrow ()
  "Moving a bound shape re-routes the arrow end."
  (excal-binding-test--scene
   (setq excal--tool 'arrow)
   (excal-test--drag 10 130 95 130)
   (let ((arrow (car (last excal--elements))))
     (setq excal--tool 'select)
     (excal--select (list box))
     (excal--nudge 0 50)
     ;; The end stays on the grown left edge, aimed from the moved fixed
     ;; point back toward the start, so it rises slightly above y = 180.
     (let ((end (excal--arrow-point arrow 1)))
       (should (excal-binding-test--near (car end) 94.0 0.5))
       (should (< 175.0 (cdr end) 180.0)))
     ;; The unbound start did not move.
     (should (excal-binding-test--near (excal--arrow-point arrow 0) '(10.0 . 130.0))))))

(ert-deftest excal-binding-test-drag-arrow-unbinds ()
  "A small drag keeps a lone bound arrow; a real drag moves and unbinds it."
  (excal-binding-test--scene
   (setq excal--tool 'arrow)
   (excal-test--drag 10 130 95 130)
   (let ((arrow (car (last excal--elements))))
     (setq excal--tool 'select)
     (excal--select (list arrow))
     (excal-test--drag 40 130 45 130)
     (should (excal--get arrow 'endBinding))
     (should (excal-binding-test--near (excal--arrow-point arrow 0) '(10.0 . 130.0)))
     (excal-test--drag 40 130 40 200)
     (should-not (excal--get arrow 'endBinding))
     (should (eq (alist-get 'boundElements box) :null)))))

(ert-deftest excal-binding-test-delete-shape-clears-binding ()
  "Deleting a bound shape leaves the arrow unbound."
  (excal-binding-test--scene
   (setq excal--tool 'arrow)
   (excal-test--drag 10 130 95 130)
   (let ((arrow (car (last excal--elements))))
     (excal--select (list box))
     (excal-delete-selected)
     (should-not (excal--get arrow 'endBinding)))))

(ert-deftest excal-binding-test-lines-do-not-bind ()
  "Lines never bind."
  (excal-binding-test--scene
   (setq excal--tool 'line)
   (excal-test--drag 10 130 95 130)
   (should-not (excal--get (car (last excal--elements)) 'endBinding))))

(ert-deftest excal-binding-test-orbit-default-inside-with-meta ()
  "Starting inside a shape orbits its outline; meta binds inside."
  (excal-binding-test--scene
   (setq excal--tool 'arrow)
   (excal-test--drag 150 130 300 130)
   (let ((arrow (car (last excal--elements))))
     (should (equal (alist-get 'mode (excal--get arrow 'startBinding)) "orbit"))
     ;; The start leaves the box on its grown right edge.
     (should (excal-binding-test--near (car (excal--arrow-point arrow 0)) 206.0 0.5)))
   (setq excal--tool 'arrow)
   (excal-test--drag 150 130 300 130 '(meta))
   (let ((arrow (car (last excal--elements))))
     (should (equal (alist-get 'mode (excal--get arrow 'startBinding)) "inside"))
     (should (excal-binding-test--near (excal--arrow-point arrow 0) '(150.0 . 130.0))))))

(provide 'excal-binding-test)
;;; excal-binding-test.el ends here
