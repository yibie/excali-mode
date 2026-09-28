;;; excali-render-test.el --- Tests for shape rendering  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for the roughjs / Excalidraw shape port in src/excali-rough.c,
;; src/excali-shape.c and src/excali-freehand.c.  Expected geometry comes
;; from the upstream algorithms; the port was also checked op by op
;; against the real roughjs 4.6 and perfect-freehand on ~480 elements.
;;
;; `excali-render-test-write-sheet' writes a visual reference sheet of
;; every shape, fill, stroke style, roughness, roundness, arrowhead and
;; freedraw variant:
;;
;;   emacs -Q --batch -L . -L test -l test/excali-render-test.el \
;;     --eval '(excali-render-test-write-sheet "/tmp/sheet.png")'

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'excali)

(defun excali-render-test--el (type &rest props)
  "Return a TYPE element at 0, 0 with PROPS, an alternating KEY VALUE list."
  (let ((e (excali--make-element type 0 0)))
    (excali--put e 'seed 1)
    (while props
      (excali--put e (pop props) (pop props)))
    e))

(defun excali-render-test--points (points)
  "Return POINTS, a list of (X Y), as an Excalidraw points vector."
  (vconcat (mapcar (lambda (p) (vector (float (car p)) (float (cadr p)))) points)))

(defun excali-render-test--shape (element)
  "Return the generated shape of ELEMENT; see `excali-native-element-shape'."
  (let ((excali--native-cache (make-hash-table :test #'eq)))
    (excali-native-element-shape (excali--native-element element))))

(defun excali-render-test--drawables (element)
  "Return ELEMENT's drawables as a list of [SHAPE SETS FILL]."
  (append (aref (excali-render-test--shape element) 0) nil))

(defun excali-render-test--set (drawable type)
  "Return the ops of the first set of TYPE in DRAWABLE, as a list."
  (cl-loop for set across (aref drawable 1)
           when (equal (aref set 0) type) return (append (aref set 1) nil)))

(defun excali-render-test--render (elements &optional width height)
  "Render ELEMENTS into a new WIDTH x HEIGHT framebuffer and return it."
  (let ((fb (excali-native-fb-create (or width 300) (or height 200)))
        (excali--native-cache (make-hash-table :test #'eq)))
    (excali-native-fb-render fb 1.0 1.0 0.0 0.0
                            (vconcat (mapcar #'excali--native-element elements))
                            nil)
    fb))

;;; PRNG

(defun excali-render-test--random (seed count)
  "Return COUNT numbers of roughjs' Random for SEED, computed in Lisp."
  (let ((state (mod seed (ash 1 32))) (out nil))
    (dotimes (_ count (nreverse out))
      (setq state (mod (* 48271 state) (ash 1 32)))
      (push (/ (float (logand state #x7fffffff)) (float (ash 1 31))) out))))

(ert-deftest excali-render-test-random-matches-roughjs ()
  "The PRNG is roughjs' `Random': Math.imul(48271, seed) & 2^31-1 / 2^31."
  (let ((native (excali-native-rough-random 1 2)))
    (should (= (aref native 0) (/ 48271.0 2147483648.0)))
    (should (= (aref native 1) (/ 182605793.0 2147483648.0))))
  (dolist (seed '(1 7 12345 1968210823 2147483647))
    (should (equal (append (excali-native-rough-random seed 50) nil)
                   (excali-render-test--random seed 50)))))

;;; Determinism

(ert-deftest excali-render-test-deterministic ()
  "The same seed gives identical pixels; another seed changes them."
  (let* ((make (lambda (seed)
                 (list (excali-render-test--el
                        "rectangle" 'x 20.0 'y 20.0 'width 120.0 'height 90.0
                        'backgroundColor "#a5d8ff" 'fillStyle "hachure"
                        'roughness 2 'seed seed)
                       (excali-render-test--el
                        "arrow" 'x 160.0 'y 40.0 'width 100.0 'height 60.0
                        'points (excali-render-test--points '((0 0) (60 -20) (100 60)))
                        'roundness '((type . 2)) 'endArrowhead "triangle"
                        'roughness 2 'seed seed))))
         (a (excali-render-test--render (funcall make 42)))
         (b (excali-render-test--render (funcall make 42)))
         (c (excali-render-test--render (funcall make 43))))
    (should (= (excali-native-fb-diff a b) 0))
    (should (> (excali-native-fb-diff a c) 0))))

;;; Hachure

(defun excali-render-test--hachure-offsets (ops angle)
  "Return sorted distinct offsets of hachure lines OPS along the normal.
ANGLE is the hachure line rotation in degrees (hachureAngle + 90)."
  (let* ((a (degrees-to-radians (- angle)))
         (nx (- (sin a))) (ny (cos a))
         (offsets nil))
    (dolist (op ops)
      (when (equal (aref op 0) "move")
        (push (/ (round (* 1e6 (+ (* nx (aref op 1)) (* ny (aref op 2))))) 1e6)
              offsets)))
    (sort (delete-dups offsets) #'<)))

(ert-deftest excali-render-test-hachure-lines ()
  "Hachure is scan-line filled at -41 degrees with gap strokeWidth * 4."
  (let* ((rect (excali-render-test--el
                "rectangle" 'width 100.0 'height 80.0 'roughness 0
                'backgroundColor "#a5d8ff" 'fillStyle "hachure"))
         (d (car (excali-render-test--drawables rect)))
         (sketch (excali-render-test--set d "fillSketch"))
         (offsets (excali-render-test--hachure-offsets sketch 49)))
    ;; Each hachure line is two rough strokes (move + bcurveTo each).
    (should (= (length sketch) (* 4 (length offsets))))
    ;; Lines every 8 units across the 127.96-unit projected extent.
    (should (= (length offsets) 16))
    (cl-loop for (x y) on offsets while y
             do (should (< (abs (- (- y x) 8)) 1e-6)))
    ;; Cross-hatch adds a second pass at 90 degrees to the first.
    (excali--put rect 'fillStyle "cross-hatch")
    (let* ((d (car (excali-render-test--drawables rect)))
           (sketch (excali-render-test--set d "fillSketch"))
           (second (excali-render-test--hachure-offsets (nthcdr (* 4 16) sketch)
                                                       139)))
      (should (equal (excali-render-test--hachure-offsets
                      (seq-take sketch (* 4 16)) 49)
                     offsets))
      (should (= (length sketch) (* 4 (+ 16 (length second)))))
      (cl-loop for (x y) on second while y
               do (should (< (abs (- (- y x) 8)) 1e-6))))))

(ert-deftest excali-render-test-fill-sets ()
  "Solid fills are fillPath sets, pattern fills fillSketch, drawn first."
  (dolist (case '(("solid" "fillPath") ("hachure" "fillSketch")
                  ("zigzag" "fillSketch") ("cross-hatch" "fillSketch")))
    (dolist (type '("rectangle" "ellipse" "diamond"))
      (let* ((e (excali-render-test--el type 'width 100.0 'height 80.0
                                       'backgroundColor "#ffc9c9"
                                       'fillStyle (car case)))
             (sets (aref (car (excali-render-test--drawables e)) 1)))
        (should (equal (mapcar (lambda (s) (aref s 0)) sets)
                       (list (cadr case) "path"))))))
  ;; Transparent background: stroke only.
  (let ((sets (aref (car (excali-render-test--drawables
                          (excali-render-test--el "rectangle" 'width 50.0
                                                 'height 50.0)))
                    1)))
    (should (equal (mapcar (lambda (s) (aref s 0)) sets) '("path")))))

;;; Roundness

(defun excali-render-test--first-move (element)
  "Return the first move point (X Y) of ELEMENT's stroke."
  (let ((op (car (excali-render-test--set
                  (car (excali-render-test--drawables element)) "path"))))
    (list (aref op 1) (aref op 2))))

(ert-deftest excali-render-test-corner-radius ()
  "`getCornerRadius': adaptive 32 (or value) above 128, else 25%."
  ;; The rounded rectangle path starts at M r 0, and continuous paths
  ;; preserve vertices, so the first move is exactly (r, 0).
  (dolist (case '((300 200 ((type . 3)) 32.0)
                  (300 200 ((type . 3) (value . 12)) 12.0)
                  (100 80 ((type . 3)) 20.0)
                  (60 40 ((type . 2)) 10.0)
                  (60 40 ((type . 1)) 10.0)))
    (pcase-let ((`(,w ,h ,roundness ,r) case))
      (should (equal (excali-render-test--first-move
                      (excali-render-test--el "rectangle"
                                             'width (float w) 'height (float h)
                                             'roundness roundness))
                     (list r 0.0)))))
  ;; Rounded diamond: M topX+vr topY+hr with topX = floor(w/2)+1.
  (should (equal (excali-render-test--first-move
                  (excali-render-test--el "diamond" 'width 100.0 'height 80.0
                                         'roundness '((type . 2))))
                 (list (+ 51 (* 0.25 51)) (* 0.25 41.0)))))

(ert-deftest excali-render-test-linear-generators ()
  "Lines pick polygon, linearPath or curve like Excalidraw."
  (let ((square (excali-render-test--points '((0 0) (100 0) (100 100) (0 100) (0 0)))))
    (should (equal (aref (car (excali-render-test--drawables
                               (excali-render-test--el "line" 'points square
                                                      'width 100.0 'height 100.0)))
                         0)
                   "linearPath"))
    (should (equal (aref (car (excali-render-test--drawables
                               (excali-render-test--el "line" 'points square
                                                      'width 100.0 'height 100.0
                                                      'backgroundColor "#b2f2bb")))
                         0)
                   "polygon"))
    (should (equal (aref (car (excali-render-test--drawables
                               (excali-render-test--el "line" 'points square
                                                      'width 100.0 'height 100.0
                                                      'roundness '((type . 2)))))
                         0)
                   "curve"))))

(ert-deftest excali-render-test-polygon-fill ()
  "A closed line with a background is filled; an open one is not."
  (let* ((closed (excali-render-test--points '((0 0) (100 0) (100 100) (0 100) (0 0))))
         (open (excali-render-test--points '((0 0) (100 0) (100 100) (0 100) (0 60))))
         (make (lambda (points)
                 (excali-render-test--el "line" 'x 50.0 'y 50.0 'points points
                                        'width 100.0 'height 100.0
                                        'backgroundColor "#ff0000"
                                        'roughness 0)))
         (filled (excali-render-test--render (list (funcall make closed))))
         (blank (excali-render-test--render nil))
         (unfilled (excali-render-test--render (list (funcall make open)))))
    ;; The fill covers a large share of the frame.
    (should (> (excali-native-fb-mean-diff filled blank) 20))
    (should (< (excali-native-fb-mean-diff unfilled blank) 5))))

;;; Arrowheads

(defun excali-render-test--arrow (head &rest props)
  "Return a straight smooth arrow 0,0 -> 200,0 with HEAD at both ends."
  (apply #'excali-render-test--el "arrow"
         'points (excali-render-test--points '((0 0) (200 0)))
         'width 200.0 'height 0.0 'roughness 0
         'startArrowhead head 'endArrowhead head props))

(ert-deftest excali-render-test-arrowhead-drawables ()
  "Every arrowhead kind produces its upstream set of drawables."
  (dolist (case '(("arrow" 2) ("bar" 2) ("circle" 1) ("circle_outline" 1)
                  ("triangle" 1) ("triangle_outline" 1) ("diamond" 1)
                  ("diamond_outline" 1) ("cardinality_one" 1)
                  ("cardinality_many" 2) ("cardinality_one_or_many" 3)
                  ("cardinality_exactly_one" 2) ("cardinality_zero_or_one" 2)
                  ("cardinality_zero_or_many" 3)))
    (let ((ds (excali-render-test--drawables
               (excali-render-test--arrow (car case)))))
      ;; The shaft, then the start head, then the end head.
      (should (= (length ds) (1+ (* 2 (cadr case))))))))

(ert-deftest excali-render-test-arrowhead-geometry ()
  "Arrowhead points follow getArrowheadPoints sizes and angles."
  ;; With roughness 0 the shaft is exact, so the "arrow" head's first
  ;; line starts at the tip's base rotated by -20 degrees: size 25.
  (let* ((ds (excali-render-test--drawables (excali-render-test--arrow "arrow")))
         (end-line (nth 3 ds))
         (move (car (excali-render-test--set end-line "path")))
         (a (degrees-to-radians 20)))
    (should (< (abs (- (aref move 1) (- 200 (* 25 (cos a))))) 1e-9))
    (should (< (abs (- (aref move 2) (* 25 (sin a)))) 1e-9)))
  ;; Heads shrink to half of a short last segment.
  (let* ((e (excali-render-test--el "arrow"
                                   'points (excali-render-test--points '((0 0) (20 0)))
                                   'width 20.0 'height 0.0 'roughness 0
                                   'endArrowhead "bar"))
         (bar (nth 1 (excali-render-test--drawables e)))
         (move (car (excali-render-test--set bar "path"))))
    ;; "bar" is 90 degrees: the line starts 10 units off the shaft.
    (should (< (abs (- (aref move 1) 20)) 1e-9))
    (should (< (abs (- (abs (aref move 2)) 10)) 1e-9)))
  ;; Circles: diameter 15 + strokeWidth - 2, solid fill in stroke or
  ;; canvas colour.
  (let* ((ds (excali-render-test--drawables
              (excali-render-test--arrow "circle_outline" 'strokeWidth 4)))
         (head (nth 2 ds)))
    (should (equal (aref (nth 1 ds) 0) "circle"))
    (should (equal (aref head 2) "canvas"))
    (should (equal (mapcar (lambda (s) (aref s 0)) (aref head 1))
                   '("fillPath" "path")))
    (let ((xs (mapcar (lambda (op) (aref op 1))
                      (excali-render-test--set head "path"))))
      ;; roughness min(0.5, 0) = 0: an exact circle of diameter 17.
      (should (< (abs (- (- (apply #'max xs) (apply #'min xs)) 17)) 0.5))))
  (should (equal (aref (nth 1 (excali-render-test--drawables
                               (excali-render-test--arrow "triangle")))
                       2)
                 "stroke")))

(ert-deftest excali-render-test-arrowhead-dash ()
  "Heads are solid except lines on dotted arrows."
  (let ((ds (excali-render-test--drawables
             (excali-render-test--arrow "arrow" 'strokeStyle "dashed"))))
    ;; Dashed and dotted strokes are single-stroke.
    (should (= (length (excali-render-test--set (car ds) "path")) 2))))

;;; Freedraw

(defun excali-render-test--outline-count (element)
  "Return the number of outline points of freedraw ELEMENT."
  (/ (length (aref (excali-render-test--shape element) 1)) 4))

(defun excali-render-test--outline-max-y (element)
  "Return the largest |y - 10| among ELEMENT's outline points."
  (let ((o (aref (excali-render-test--shape element) 1)) (m 0))
    (cl-loop for i from 1 below (length o) by 4
             do (setq m (max m (abs (- (aref o i) 10)))))
    m))

(ert-deftest excali-render-test-freedraw-outline ()
  "perfect-freehand and laser-pointer outlines match upstream counts."
  ;; Keep coordinates away from 0: Excalidraw's two-decimal regex turns
  ;; tiny numbers like 1.7e-16 into 1.7 (reproduced by the port).
  (let* ((line (excali-render-test--points
                (cl-loop for i to 10 collect (list (+ 10 (* i 10)) 10))))
         (make (lambda (&rest props)
                 (apply #'excali-render-test--el "freedraw" 'points line
                        'width 100.0 'height 0.0 props))))
    ;; Counts from perfect-freehand's getStroke / LaserPointer with
    ;; Excalidraw's options for the same input.
    (should (= (excali-render-test--outline-count
                (excali-render-test--el "freedraw"
                                       'points (excali-render-test--points '((10 10)))
                                       'pressures [] 'simulatePressure t))
               44))
    (should (= (excali-render-test--outline-count
                (funcall make 'pressures [] 'simulatePressure t))
               62))
    (let ((high (funcall make 'pressures (make-vector 11 0.9)
                         'simulatePressure :false))
          (low (funcall make 'pressures (make-vector 11 0.1)
                        'simulatePressure :false))
          (constant (funcall make 'pressures [] 'simulatePressure t
                             'strokeOptions '((variability . "constant")
                                              (streamline . 0.5)))))
      (should (= (excali-render-test--outline-count high) 62))
      ;; Pressure widens the stroke: 7.80 vs 3.38 half-widths upstream.
      (should (< (abs (- (excali-render-test--outline-max-y high) 7.8)) 0.02))
      (should (< (abs (- (excali-render-test--outline-max-y low) 3.37)) 0.02))
      (should (= (excali-render-test--outline-count constant) 84))
      (should (< (abs (- (excali-render-test--outline-max-y constant) 2.8)) 0.02)))))

(ert-deftest excali-render-test-freedraw-fill ()
  "A closed freedraw loop gets a curve fill; its stroke is filled."
  (let* ((loop (excali-render-test--points
                (cl-loop for i to 30
                         collect (let ((a (* i (/ float-pi 15))))
                                   (list (+ 50 (* 40 (cos a)))
                                         (+ 50 (* 40 (sin a))))))))
         (e (excali-render-test--el "freedraw" 'points loop 'width 80.0 'height 80.0
                                   'backgroundColor "#ffec99" 'pressures []
                                   'simulatePressure t))
         (ds (excali-render-test--drawables e)))
    (should (= (length ds) 1))
    (should (equal (aref (car ds) 0) "curve"))
    ;; stroke: "none" on the fill curve: only a fill set.
    (should (equal (mapcar (lambda (s) (aref s 0)) (aref (car ds) 1))
                   '("fillPath")))))

;;; Bounds, rotation, opacity

(defun excali-render-test--op-points (shape)
  "Return all op coordinates of SHAPE as a list of (X . Y)."
  (let (out)
    (seq-doseq (d (aref shape 0))
      (seq-doseq (set (aref d 1))
        (seq-doseq (op (aref set 1))
          (cl-loop for i from 1 below (length op) by 2
                   do (push (cons (aref op i) (aref op (1+ i))) out)))))
    (let ((o (aref shape 1)))
      (cl-loop for i from 0 below (length o) by 2
               do (push (cons (aref o i) (aref o (1+ i))) out)))
    out))

(ert-deftest excali-render-test-bounds-conservative ()
  "Every generated point lies within the element's culling padding."
  (let ((elements
         (list (excali-render-test--el "rectangle" 'width 400.0 'height 30.0
                                      'roughness 2 'backgroundColor "#a5d8ff"
                                      'fillStyle "zigzag")
               (excali-render-test--el "ellipse" 'width 300.0 'height 200.0
                                      'roughness 2 'backgroundColor "#a5d8ff")
               (excali-render-test--el "diamond" 'width 120.0 'height 90.0
                                      'roughness 2 'roundness '((type . 2))
                                      'backgroundColor "#a5d8ff"
                                      'fillStyle "cross-hatch")
               (excali-render-test--el
                "arrow" 'width 100.0 'height 120.0 'roughness 2
                'points (excali-render-test--points '((0 0) (100 0) (0 120)))
                'roundness '((type . 2))
                'startArrowhead "cardinality_zero_or_many"
                'endArrowhead "diamond_outline")
               (excali-render-test--el
                "freedraw" 'width 60.0 'height 40.0 'strokeWidth 4
                'points (excali-render-test--points '((0 0) (30 40) (60 0)))
                'pressures [] 'simulatePressure t))))
    (dolist (e elements)
      (let* ((shape (excali-render-test--shape e))
             (pad (aref shape 3))
             (pts (excali--get e 'points))
             (xs (if pts (mapcar (lambda (p) (aref p 0)) pts)
                   (list 0 (excali--get e 'width))))
             (ys (if pts (mapcar (lambda (p) (aref p 1)) pts)
                   (list 0 (excali--get e 'height)))))
        (dolist (p (excali-render-test--op-points shape))
          (should (<= (- (apply #'min xs) pad) (car p) (+ (apply #'max xs) pad)))
          (should (<= (- (apply #'min ys) pad) (cdr p) (+ (apply #'max ys) pad))))))))

(ert-deftest excali-render-test-rotation-center ()
  "Point-based elements rotate about the centre of their curve bounds."
  (let* ((line (lambda (angle)
                 (excali-render-test--el
                  "line" 'x 200.0 'y 100.0 'width 100.0 'height 0.0
                  'points (excali-render-test--points '((0 0) (-100 0)))
                  'roughness 0 'strokeWidth 4 'angle angle)))
         (coords (aref (excali-render-test--shape (funcall line 0)) 2)))
    ;; x/y is the first point, not the box corner: the centre is 150.
    (should (< (abs (- (aref coords 4) 150)) 1e-9))
    ;; A half turn about that centre maps the line onto itself.
    (should (< (excali-native-fb-mean-diff
                (excali-render-test--render (list (funcall line 0)))
                (excali-render-test--render (list (funcall line float-pi))))
               0.05))))

(ert-deftest excali-render-test-opacity ()
  "Opacity composites the whole element at opacity / 100."
  (let* ((rect (lambda (opacity)
                 (excali-render-test--el "rectangle" 'width 300.0 'height 200.0
                                        'backgroundColor "#000000"
                                        'strokeColor "transparent"
                                        'roughness 0 'opacity opacity)))
         (blank (excali-render-test--render nil))
         (full (excali-native-fb-mean-diff
                (excali-render-test--render (list (funcall rect 100))) blank))
         (half (excali-native-fb-mean-diff
                (excali-render-test--render (list (funcall rect 50))) blank)))
    (should (< (abs (- (/ half full) 0.5)) 0.02))))

;;; Reference sheet

(defun excali-render-test-sheet-elements ()
  "Return elements covering every shape and style variant, laid out in a grid."
  (let ((out nil) (seed 100))
    (cl-flet ((add (type x y &rest props)
                (push (apply #'excali-render-test--el type 'x (float x) 'y (float y)
                             'seed (cl-incf seed) props)
                      out)))
      ;; Shapes x fills x stroke styles, per roughness and roundness.
      (let ((y 20))
        (dolist (type '("rectangle" "diamond" "ellipse"))
          (dolist (rnd (if (equal type "ellipse") '(nil) '(nil ((type . 3)))))
            (dolist (rough '(0 1 2))
              (let ((x 20))
                (dolist (fill '("solid" "hachure" "cross-hatch" "zigzag"))
                  (dolist (stroke '("solid" "dashed" "dotted"))
                    (add type x y 'width 90.0 'height 60.0 'roughness rough
                         'roundness (or rnd :null) 'strokeStyle stroke
                         'fillStyle fill 'backgroundColor "#a5d8ff")
                    (cl-incf x 110)))
                (cl-incf y 80))))))
      ;; Lines: straight, curved, closed polygon with fill, per roughness.
      (let ((y 1300))
        (dolist (rough '(0 1 2))
          (let ((x 20))
            (dolist (rnd '(nil ((type . 2))))
              (add "line" x y 'width 150.0 'height 60.0 'roughness rough
                   'roundness (or rnd :null)
                   'points (excali-render-test--points '((0 0) (50 60) (100 0) (150 60))))
              (cl-incf x 180)
              (add "line" x y 'width 80.0 'height 60.0 'roughness rough
                   'roundness (or rnd :null) 'backgroundColor "#b2f2bb"
                   'fillStyle (if rnd "hachure" "solid")
                   'points (excali-render-test--points '((0 0) (80 0) (60 60) (10 50) (0 0))))
              (cl-incf x 120))
            (dolist (stroke '("dashed" "dotted"))
              (add "arrow" x y 'width 150.0 'height 60.0 'roughness rough
                   'strokeStyle stroke 'roundness '((type . 2))
                   'endArrowhead "arrow"
                   'points (excali-render-test--points '((0 0) (70 60) (150 10))))
              (cl-incf x 180)))
          (cl-incf y 90)))
      ;; All arrowheads, straight and curved, at both ends.
      (let ((y 1600) (col 0))
        (dolist (head '("arrow" "bar" "circle" "circle_outline" "triangle"
                        "triangle_outline" "diamond" "diamond_outline"
                        "cardinality_one" "cardinality_many"
                        "cardinality_one_or_many" "cardinality_exactly_one"
                        "cardinality_zero_or_one" "cardinality_zero_or_many"))
          (let ((x (+ 20 (* col 330))))
            (add "arrow" x y 'width 130.0 'height 0.0
                 'startArrowhead head 'endArrowhead head
                 'points (excali-render-test--points '((0 0) (130 0))))
            (add "arrow" (+ x 160) y 'width 130.0 'height 40.0
                 'roundness '((type . 2)) 'strokeWidth 1
                 'startArrowhead head 'endArrowhead head
                 'points (excali-render-test--points '((0 0) (60 -30) (130 10)))))
          (setq col (1+ col))
          (when (= col 4) (setq col 0 y (+ y 70)))))
      ;; Freedraw: simulated pressure, real pressure, constant width, loop.
      (let* ((wave (cl-loop for i to 60
                            collect (list (* i 4) (* 20 (sin (/ i 6.0))))))
             (pts (excali-render-test--points wave))
             (x 20))
        (dolist (sw '(1 2 4))
          (add "freedraw" x 2000 'points pts 'width 240.0 'height 40.0
               'strokeWidth sw 'pressures [] 'simulatePressure t)
          (add "freedraw" x 2060 'points pts 'width 240.0 'height 40.0
               'strokeWidth sw 'simulatePressure :false
               'pressures (vconcat (cl-loop for i to 60
                                            collect (/ (+ 1 (sin (/ i 5.0))) 2))))
          (add "freedraw" x 2120 'points pts 'width 240.0 'height 40.0
               'strokeWidth sw 'pressures [] 'simulatePressure t
               'strokeOptions '((variability . "constant") (streamline . 0.5)))
          (cl-incf x 270))
        (let ((loop (excali-render-test--points
                     (cl-loop for i to 40
                              collect (let ((a (* i (/ float-pi 20))))
                                        (list (* 50 (cos a)) (* 35 (sin a))))))))
          (add "freedraw" 900 2060 'points loop 'width 100.0 'height 70.0
               'backgroundColor "#ffec99" 'fillStyle "hachure"
               'pressures [] 'simulatePressure t)
          (add "freedraw" 1050 2060 'points loop 'width 100.0 'height 70.0
               'backgroundColor "#ffc9c9" 'fillStyle "solid"
               'pressures [] 'simulatePressure t)))
      ;; Opacity and rotation.
      (add "rectangle" 20 2200 'width 120.0 'height 60.0 'opacity 40
           'backgroundColor "#e03131" 'fillStyle "solid")
      (add "arrow" 200 2230 'width 150.0 'height 0.0 'angle 0.6
           'points (excali-render-test--points '((0 0) (-150 0))))
      (add "diamond" 420 2200 'width 100.0 'height 60.0 'angle 0.4
           'roundness '((type . 2)) 'backgroundColor "#b2f2bb"))
    (nreverse out)))

(defun excali-render-test-write-sheet (file &optional scale)
  "Render `excali-render-test-sheet-elements' to FILE at SCALE (default 1)."
  (let* ((scale (or scale 1))
         (w (round (* 1360 scale))) (h (round (* 2300 scale)))
         (fb (excali-native-fb-create w h))
         (excali--native-cache (make-hash-table :test #'eq)))
    (excali-native-fb-render fb (float scale) 1.0 0.0 0.0
                            (vconcat (mapcar #'excali--native-element
                                             (excali-render-test-sheet-elements)))
                            nil)
    (excali-native-fb-write-png fb file)))

(ert-deftest excali-render-test-sheet-renders ()
  "The reference sheet renders every element, identically twice."
  (let* ((elements (excali-render-test-sheet-elements))
         (excali--native-cache (make-hash-table :test #'eq))
         (vec (vconcat (mapcar #'excali--native-element elements)))
         (a (excali-native-fb-create 1360 2300))
         (b (excali-native-fb-create 1360 2300)))
    (should (= (excali-native-fb-render a 1.0 1.0 0.0 0.0 vec nil)
               (length elements)))
    (excali-native-fb-render b 1.0 1.0 0.0 0.0 vec nil)
    (should (= (excali-native-fb-diff a b) 0))))

(provide 'excali-render-test)
;;; excali-render-test.el ends here
