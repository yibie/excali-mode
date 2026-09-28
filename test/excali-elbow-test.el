;;; excali-elbow-test.el --- Elbow arrows  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Expected points marked "traced" were derived by hand, stepping through
;; upstream's elbowArrow.ts (getElbowArrowData, generateDynamicAABBs,
;; calculateGrid, astar with its binary heap, post-processing).

(require 'ert)
(require 'excali)
(require 'excali-test)

(defun excali-elbow-test--arrow (x y points &rest props)
  "Return an elbow arrow at X, Y with local POINTS (a list of [X Y]) and PROPS."
  (apply #'excali--make-element "arrow" x y
         (cons 'points (vconcat (mapcar (lambda (p) (vector (float (aref p 0)) (float (aref p 1))))
                                        points)))
         (cons 'elbowed t) (cons 'startBinding :null) (cons 'endBinding :null)
         (cons 'startArrowhead :null) (cons 'endArrowhead "arrow")
         (cons 'fixedSegments :null) (cons 'startIsSpecial :null) (cons 'endIsSpecial :null)
         props))

(defun excali-elbow-test--box (x y &rest props)
  "Return a 100x100 rectangle at X, Y with stroke width 2 and PROPS."
  (apply #'excali--make-element "rectangle" x y (cons 'width 100.0) (cons 'height 100.0)
         (cons 'strokeWidth 2) props))

(defun excali-elbow-test--scene-points (arrow)
  "Return ARROW's points in scene coordinates as a list of (X Y)."
  (let ((x (excali--get arrow 'x)) (y (excali--get arrow 'y)))
    (mapcar (lambda (p) (list (+ x (aref p 0)) (+ y (aref p 1))))
            (excali--get arrow 'points))))

(defun excali-elbow-test--near (a b &optional eps)
  "Return non-nil if lists of numbers or points A and B agree within EPS."
  (let ((eps (or eps 1e-6)))
    (cond ((numberp a) (< (abs (- a b)) eps))
          ((and (consp a) (= (length a) (length b)))
           (cl-every (lambda (x y) (excali-elbow-test--near x y eps)) a b)))))

(defun excali-elbow-test--orthogonal-p (arrow)
  "Return non-nil if every segment of ARROW is horizontal or vertical.
Bound ends sit at fixed points kept off exactly 0.5, so a free end
facing one may be 0.01 off, as upstream."
  (let ((points (append (excali--get arrow 'points) nil)))
    (cl-loop for (a b) on points
             while b
             always (or (< (abs (- (aref a 0) (aref b 0))) 0.02)
                        (< (abs (- (aref a 1) (aref b 1))) 0.02)))))

(defmacro excali-elbow-test--with-scene (&rest body)
  "Run BODY in a scene buffer at zoom 1."
  `(excali-test--with-scene
    (setq excali--elements nil excali--selection nil)
    ,@body))

(defmacro excali-elbow-test--in-window (&rest body)
  "Run BODY in a window-backed scene drawing elbow arrows."
  `(excali-test--in-window
    (setq excali--elements nil excali--tool-locked nil excali--multi-element nil)
    (excali--load-current-style nil)
    (setf (alist-get 'arrowType excali--current-style) "elbow")
    ,@body))

;;;; Headings and helpers

(ert-deftest excali-elbow-test-vector-to-heading ()
  "Ties go left, then up, as upstream `vectorToHeading'."
  (should (eq (excali--vector-to-heading 1 0) 'right))
  (should (eq (excali--vector-to-heading 100 100) 'up))
  (should (eq (excali--vector-to-heading -100 -100) 'left))
  (should (eq (excali--vector-to-heading 0 0) 'left))
  (should (eq (excali--vector-to-heading 10 20) 'down))
  (should (eq (excali--vector-to-heading 100 90) 'right)))

(ert-deftest excali-elbow-test-heading-from-element ()
  "Search cones pick the side a point lies off."
  (excali-elbow-test--with-scene
   (let* ((box (excali-elbow-test--box 0 0))
          (aabb (excali--aabb-for-element box)))
     (should (eq (excali--heading-from-element box aabb [106.0 50.0]) 'right))
     (should (eq (excali--heading-from-element box aabb [50.0 -6.0]) 'up))
     (should (eq (excali--heading-from-element box aabb [50.0 106.0]) 'down))
     (should (eq (excali--heading-from-element box aabb [-6.0 50.0]) 'left)))))

(ert-deftest excali-elbow-test-binary-heap ()
  "The heap pops in score order."
  (let ((heap (excali--eheap-create)) (out nil))
    (dolist (f '(5 3 8 1 9 2 7 4 6 0 11 10 13 12 15 14 17 16))
      (let ((n (excali--enode-create [0.0 0.0] 0 0)))
        (setf (excali--enode-f n) f)
        (excali--eheap-push heap n)))
    (while (> (excali--eheap-size heap) 0)
      (push (excali--enode-f (excali--eheap-pop heap)) out))
    (should (equal (nreverse out) (number-sequence 0 17)))))

(ert-deftest excali-elbow-test-normalize-fixed-point ()
  "Fixed points avoid exactly 0.5 and are clamped."
  (should (equal (excali--normalize-fixed-point [0.5 0.2]) [0.5001 0.2]))
  (should (equal (excali--normalize-fixed-point [12 -20]) [10.0 -10.0]))
  (should (equal (excali--normalize-fixed-point nil) [0.5001 0.5001])))

(ert-deftest excali-elbow-test-corner-points ()
  "Collinear interior points go, as do points within 1 px of the previous."
  (should (equal (excali--elbow-corner-points
                  (list [0.0 0.0] [10.0 0.0] [20.0 0.0] [20.0 10.0] [20.0 30.0] [40.0 30.0]))
                 (list [0.0 0.0] [20.0 0.0] [20.0 30.0] [40.0 30.0])))
  (should (equal (excali--elbow-remove-short-segments
                  (list [0.0 0.0] [0.5 0.0] [10.0 0.0] [10.0 10.0]))
                 (list [0.0 0.0] [10.0 0.0] [10.0 10.0]))))

;;;; Routing

(ert-deftest excali-elbow-test-route-diagonal ()
  "Traced: a free arrow along the exact diagonal starts up, as upstream."
  (excali-elbow-test--with-scene
   (let ((a (excali-elbow-test--arrow 0 0 (list [0 0] [100 100]))))
     (setq excali--elements (list a))
     (excali--elbow-route-fresh a)
     (should (equal (excali-elbow-test--scene-points a)
                    '((0.0 0.0) (0.0 -2.0) (50.0 -2.0) (50.0 100.0) (100.0 100.0))))
     (should (eq (excali--get a 'fixedSegments) nil)))))

(ert-deftest excali-elbow-test-route-z-shape ()
  "A free arrow mostly to the right bends once in the middle."
  (excali-elbow-test--with-scene
   (let ((a (excali-elbow-test--arrow 10 20 (list [0 0] [100 90]))))
     (setq excali--elements (list a))
     (excali--elbow-route-fresh a)
     (should (equal (excali-elbow-test--scene-points a)
                    '((10.0 20.0) (60.0 20.0) (60.0 110.0) (110.0 110.0))))
     (should (= (excali--get a 'width) 100.0))
     (should (= (excali--get a 'height) 90.0)))))

(ert-deftest excali-elbow-test-route-between-bound-boxes ()
  "Traced: boxes side by side get a straight arrow between their gaps."
  (excali-elbow-test--with-scene
   (let* ((a (excali-elbow-test--box 0 0))
          (b (excali-elbow-test--box 300 0))
          (arrow (excali-elbow-test--arrow 106 50 (list [0 0] [188 0]))))
     (setq excali--elements (list a b arrow))
     (should (equal (excali--elbow-fixed-point a [106.0 50.0]) [1.06 0.5001]))
     (excali--elbow-bind-end arrow 'start a)
     (excali--elbow-bind-end arrow 'end b)
     (excali--elbow-route-fresh arrow)
     (should (excali-elbow-test--near (excali-elbow-test--scene-points arrow)
                                     '((106.0 50.01) (294.0 50.01)))))))

(ert-deftest excali-elbow-test-route-l-shape-into-top ()
  "Traced: from a box's right side into the top of a box below right.
The start leaves right to the dongle at x = 142 (the start box grown by
the gap and padding) and the path bends once, above the end box."
  (excali-elbow-test--with-scene
   (let* ((a (excali-elbow-test--box 0 0))
          (b (excali-elbow-test--box 150 200))
          (arrow (excali-elbow-test--arrow 106 50 (list [0 0] [94 144]))))
     (setq excali--elements (list a b arrow))
     (excali--elbow-bind-end arrow 'start a)
     (excali--elbow-bind-end arrow 'end b)
     (should (equal (alist-get 'fixedPoint (excali--get arrow 'endBinding)) [0.5001 -0.06]))
     (excali--elbow-route-fresh arrow)
     (should (excali-elbow-test--near (excali-elbow-test--scene-points arrow)
                                     '((106.0 50.01) (200.01 50.01) (200.01 194.0)))))))

(ert-deftest excali-elbow-test-route-around-offset-boxes ()
  "Bound ends leave their shapes along the side's heading and stay orthogonal."
  (excali-elbow-test--with-scene
   (let* ((a (excali-elbow-test--box 0 0))
          (b (excali-elbow-test--box 300 250))
          (arrow (excali-elbow-test--arrow 106 50 (list [0 0] [194 250]))))
     (setq excali--elements (list a b arrow))
     (excali--elbow-bind-end arrow 'start a)
     (excali--elbow-bind-end arrow 'end b)
     (excali--elbow-route-fresh arrow)
     (let ((points (excali-elbow-test--scene-points arrow)))
       (should (excali-elbow-test--orthogonal-p arrow))
       (should (excali-elbow-test--near (car points) '(106.0 50.01)))
       (should (excali-elbow-test--near (car (last points)) '(294.0 300.01)))
       ;; Leaves A to the right and enters B from the left.
       (should (> (car (nth 1 points)) 106.0))
       (should (< (car (nth (- (length points) 2) points)) 294.0))))))

(ert-deftest excali-elbow-test-route-avoids-shape ()
  "An arrow from a box's left side to a point on its right goes around it."
  (excali-elbow-test--with-scene
   (let* ((a (excali-elbow-test--box 0 0))
          (arrow (excali-elbow-test--arrow -6 50 (list [0 0] [306 0]))))
     (setq excali--elements (list a arrow))
     (excali--elbow-bind-end arrow 'start a)
     (excali--elbow-route-fresh arrow)
     (should (excali-elbow-test--orthogonal-p arrow))
     (let ((points (excali-elbow-test--scene-points arrow)))
       (should (excali-elbow-test--near (car (last points)) '(300.0 50.0)))
       ;; First leaves left, and no segment crosses the box.
       (should (< (car (nth 1 points)) -6.0))
       (cl-loop for (p q) on points while q
                do (let ((mx (/ (+ (car p) (car q)) 2)) (my (/ (+ (cadr p) (cadr q)) 2)))
                     (should-not (and (< 0 mx 100) (< 0 my 100)))))))))

;;;; Fixed segments

(ert-deftest excali-elbow-test-move-and-release-segment ()
  "Traced: moving the middle segment fixes it; dragging an end keeps it;
releasing it routes again."
  (excali-elbow-test--with-scene
   (let ((a (excali-elbow-test--arrow 0 0 (list [0 0] [100 90]))))
     (setq excali--elements (list a))
     (excali--elbow-route-fresh a)
     (should (= (excali--elbow-move-fixed-segment a 2 70 45) 2))
     (should (equal (excali-elbow-test--scene-points a)
                    '((0.0 0.0) (70.0 0.0) (70.0 90.0) (100.0 90.0))))
     (should (equal (excali--elbow-fixed-indices a) '(2)))
     (should (eq (alist-get 'startIsSpecial a) :false))
     ;; Dragging the end down keeps the fixed segment's x.
     (excali--elbow-update a (list :points (list [0.0 0.0] [100.0 120.0])))
     (should (equal (excali-elbow-test--scene-points a)
                    '((0.0 0.0) (70.0 0.0) (70.0 120.0) (100.0 120.0))))
     (let ((seg (aref (excali--get a 'fixedSegments) 0)))
       (should (equal (alist-get 'start seg) [70.0 0.0]))
       (should (equal (alist-get 'end seg) [70.0 120.0]))
       (should (= (alist-get 'index seg) 2)))
     ;; Releasing it routes afresh.
     (should (excali--elbow-delete-fixed-segment a 2))
     (should (eq (excali--get a 'fixedSegments) nil))
     (should (excali-elbow-test--orthogonal-p a))
     (should (equal (car (last (excali-elbow-test--scene-points a))) '(100.0 120.0))))))

(ert-deftest excali-elbow-test-move-first-segment-adds-points ()
  "Moving the first segment of a free arrow adds a segment in front."
  (excali-elbow-test--with-scene
   (let ((a (excali-elbow-test--arrow 0 0 (list [0 0] [100 90]))))
     (setq excali--elements (list a))
     (excali--elbow-route-fresh a)
     ;; Segment 1 is horizontal (0,0)-(50,0); move it down to y = 30.
     (let ((index (excali--elbow-move-fixed-segment a 1 25 30)))
       (should (= index 2))
       (should (equal (excali-elbow-test--scene-points a)
                      '((0.0 0.0) (0.0 30.0) (50.0 30.0) (50.0 90.0) (100.0 90.0))))
       (should (equal (excali--elbow-fixed-indices a) '(2)))
       ;; Moving it again uses the new index.
       (should (= (excali--elbow-move-fixed-segment a index 25 40) 2))
       (should (equal (excali-elbow-test--scene-points a)
                      '((0.0 0.0) (0.0 40.0) (50.0 40.0) (50.0 90.0) (100.0 90.0))))))))

(ert-deftest excali-elbow-test-release-between-fixed-segments ()
  "Traced: fixing three segments, then releasing the middle one re-routes
only the part between its fixed neighbours."
  (excali-elbow-test--with-scene
   (let ((a (excali-elbow-test--arrow 0 0 (list [0 0] [200 150]))))
     (setq excali--elements (list a))
     (excali--elbow-route-fresh a)
     (should (equal (excali-elbow-test--scene-points a)
                    '((0.0 0.0) (100.0 0.0) (100.0 150.0) (200.0 150.0))))
     (should (= (excali--elbow-move-fixed-segment a 2 120 0) 2))
     ;; Moving the last segment adds a point after it.
     (should (= (excali--elbow-move-fixed-segment a 3 0 100) 3))
     (should (equal (excali-elbow-test--scene-points a)
                    '((0.0 0.0) (120.0 0.0) (120.0 100.0) (200.0 100.0) (200.0 150.0))))
     ;; Moving the first segment adds a point before it.
     (should (= (excali--elbow-move-fixed-segment a 1 0 30) 2))
     (should (equal (excali-elbow-test--scene-points a)
                    '((0.0 0.0) (0.0 30.0) (120.0 30.0) (120.0 100.0) (200.0 100.0)
                      (200.0 150.0))))
     (should (equal (excali--elbow-fixed-indices a) '(2 3 4)))
     (should (excali--elbow-delete-fixed-segment a 3))
     (should (equal (excali-elbow-test--scene-points a)
                    '((0.0 0.0) (0.0 30.0) (120.0 30.0) (120.0 100.0) (200.0 100.0)
                      (200.0 150.0))))
     (should (equal (excali--elbow-fixed-indices a) '(2 4)))
     (should (equal (alist-get 'startIsSpecial a) :false)))))

(ert-deftest excali-elbow-test-renormalize ()
  "Renormalizing merges collinear segments and re-indexes fixed ones."
  (excali-elbow-test--with-scene
   (let* ((a (excali-elbow-test--arrow
              0 0 (list [0 0] [50 0] [50 40] [50 90] [100 90] [100 120])
              (cons 'fixedSegments
                    (vector (list (cons 'start [50.0 40.0]) (cons 'end [50.0 90.0])
                                  (cons 'index 3))
                            (list (cons 'start [50.0 90.0]) (cons 'end [100.0 90.0])
                                  (cons 'index 4)))))))
     (setq excali--elements (list a))
     (excali--elbow-apply a (excali--elbow-renormalize (excali--elbow-snapshot a)))
     (should (equal (excali-elbow-test--scene-points a)
                    '((0.0 0.0) (50.0 0.0) (50.0 90.0) (100.0 90.0) (100.0 120.0))))
     (should (equal (excali--elbow-fixed-indices a) '(2 3))))))

;;;; Following shapes

(ert-deftest excali-elbow-test-reroutes-when-shape-moves ()
  "Moving a bound shape re-routes the elbow arrow to its fixed point."
  (excali-elbow-test--with-scene
   (let* ((a (excali-elbow-test--box 0 0))
          (b (excali-elbow-test--box 300 0))
          (arrow (excali-elbow-test--arrow 106 50 (list [0 0] [188 0]))))
     (setq excali--elements (list a b arrow))
     (excali--elbow-bind-end arrow 'start a)
     (excali--elbow-bind-end arrow 'end b)
     (excali--elbow-route-fresh arrow)
     (excali--put b 'y 200.0)
     (excali--follow (list b))
     (let ((points (excali-elbow-test--scene-points arrow)))
       (should (excali-elbow-test--orthogonal-p arrow))
       (should (excali-elbow-test--near (car points) '(106.0 50.01)))
       (should (excali-elbow-test--near (car (last points)) '(294.0 250.01)))
       (should (> (length points) 2))))))

(ert-deftest excali-elbow-test-convert-arrow-type ()
  "Setting the arrow type converts selected arrows both ways."
  (excali-elbow-test--with-scene
   (excali--load-current-style nil)
   (let ((arrow (excali--make-element "arrow" 0 0 (cons 'points (vector [0.0 0.0] [50.0 20.0] [100.0 90.0]))
                                     (cons 'startBinding :null) (cons 'endBinding :null)
                                     (cons 'roundness '((type . 2))))))
     (setq excali--elements (list arrow) excali--selection (list arrow))
     (cl-letf (((symbol-function 'excali--render) #'ignore))
       (excali-set-style 'arrowType "elbow")
       (should (excali--elbow-p arrow))
       (should (eq (excali--get arrow 'roundness) nil))
       (should (equal (excali-elbow-test--scene-points arrow)
                      '((0.0 0.0) (50.0 0.0) (50.0 90.0) (100.0 90.0))))
       (should (equal (excali--shown-style-value 'arrowType) "elbow"))
       (excali-set-style 'arrowType "round")
       (should-not (excali--elbow-p arrow))
       (should (equal (excali--get arrow 'roundness) '((type . 2))))
       (should (= (length (excali--get arrow 'points)) 2))))))

;;;; Editor

(ert-deftest excali-elbow-test-draw-between-shapes ()
  "Drawing with the elbow arrow type binds both ends and routes."
  (excali-elbow-test--in-window
   (let ((a (excali-elbow-test--box 20 20)) (b (excali-elbow-test--box 320 20)))
     (setq excali--elements (list a b))
     (setq excali--tool 'arrow)
     (excali-test--drag 125 70 315 70)
     (let ((arrow (car (last excali--elements))))
       (should (excali--elbow-p arrow))
       (should (equal (alist-get 'elementId (excali--get arrow 'startBinding)) (excali--get a 'id)))
       (should (equal (alist-get 'elementId (excali--get arrow 'endBinding)) (excali--get b 'id)))
       (should (equal (alist-get 'mode (excali--get arrow 'endBinding)) "orbit"))
       (should (excali-elbow-test--near (excali-elbow-test--scene-points arrow)
                                       '((126.0 70.01) (314.0 70.01))))
       (should (equal excali--selection (list arrow)))
       ;; Selected elbow arrows have no transform handles.
       (should-not (excali--transform-target))))))

(ert-deftest excali-elbow-test-draw-free ()
  "An unbound elbow arrow is routed orthogonally from the press to the release."
  (excali-elbow-test--in-window
   (setq excali--tool 'arrow)
   (excali-test--drag 10 10 110 100)
   (let ((arrow (car (last excali--elements))))
     (should (excali--elbow-p arrow))
     (should (equal (excali-elbow-test--scene-points arrow)
                    '((10.0 10.0) (60.0 10.0) (60.0 100.0) (110.0 100.0)))))))

(ert-deftest excali-elbow-test-click-click-takes-two-points ()
  "In click-click mode an elbow arrow ends at its second click."
  (excali-elbow-test--in-window
   (setq excali--tool 'arrow)
   (excali-test--drag 10 10 12 10)
   (should excali--multi-element)
   (excali-test--drag 110 100 110 100)
   (should-not excali--multi-element)
   (let ((arrow (car (last excali--elements))))
     (should (equal (car (last (excali-elbow-test--scene-points arrow))) '(110.0 100.0)))
     (should (excali-elbow-test--orthogonal-p arrow)))))

(ert-deftest excali-elbow-test-drag-midpoint-and-double-click ()
  "Dragging a segment midpoint fixes the segment; double-clicking it releases it."
  (excali-elbow-test--in-window
   (let ((arrow (excali-elbow-test--arrow 10 10 (list [0 0] [100 90]))))
     (setq excali--elements (list arrow))
     (excali--elbow-route-fresh arrow)
     (excali--select (list arrow))
     ;; Middle segment (60,10)-(60,100), midpoint (60,55).
     (excali-test--drag 60 55 80 55)
     (should (equal (excali-elbow-test--scene-points arrow)
                    '((10.0 10.0) (80.0 10.0) (80.0 100.0) (110.0 100.0))))
     (should (equal (excali--elbow-fixed-indices arrow) '(2)))
     (should (equal excali--selection (list arrow)))
     ;; Double-click the fixed midpoint.
     (let ((posn (excali-test--posn 80 55)))
       (setq unread-command-events (list (list 'mouse-1 posn)))
       (excali-double-click (list 'double-mouse-1 posn)))
     (should (eq (excali--get arrow 'fixedSegments) nil))
     (should (equal (excali-elbow-test--scene-points arrow)
                    '((10.0 10.0) (60.0 10.0) (60.0 100.0) (110.0 100.0)))))))

(ert-deftest excali-elbow-test-drag-end-binds ()
  "Dragging an end onto a shape binds it and re-routes."
  (excali-elbow-test--in-window
   (let ((box (excali-elbow-test--box 200 20))
         (arrow (excali-elbow-test--arrow 10 70 (list [0 0] [100 0]))))
     (setq excali--elements (list box arrow))
     (excali--elbow-route-fresh arrow)
     (excali--select (list arrow))
     (excali-test--drag 110 70 196 70)
     (should (equal (alist-get 'elementId (excali--get arrow 'endBinding)) (excali--get box 'id)))
     (should (excali-elbow-test--near (car (last (excali-elbow-test--scene-points arrow)))
                                     '(194.0 70.01)))
     ;; Dragging it away unbinds.
     (excali-test--drag 194 70 150 150)
     (should-not (excali--get arrow 'endBinding))
     (should (excali-elbow-test--orthogonal-p arrow)))))

(ert-deftest excali-elbow-test-bound-arrow-does-not-move-alone ()
  "A lone bound elbow arrow stays put; with both shapes it moves along."
  (excali-elbow-test--in-window
   (let* ((a (excali-elbow-test--box 20 20))
          (b (excali-elbow-test--box 320 20))
          (arrow (excali-elbow-test--arrow 126 70 (list [0 0] [188 0]))))
     (setq excali--elements (list a b arrow))
     (excali--elbow-bind-end arrow 'start a)
     (excali--elbow-bind-end arrow 'end b)
     (excali--elbow-route-fresh arrow)
     (excali--select (list arrow))
     (let ((x (excali--get arrow 'x)))
       (excali-test--drag 200 70 200 150)
       (should (= (excali--get arrow 'x) x))
       (should (excali--get arrow 'startBinding)))
     ;; With both shapes selected it moves and stays bound.
     (excali--select (list a b arrow))
     (excali-test--drag 70 70 70 120)
     (should (excali-elbow-test--near (car (excali-elbow-test--scene-points arrow))
                                     '(126.0 120.01)))
     (should (excali--get arrow 'startBinding))
     (should (excali--get arrow 'endBinding)))))

(ert-deftest excali-elbow-test-overlays ()
  "A selected elbow arrow shows its two ends and one midpoint per segment."
  (excali-elbow-test--in-window
   (let ((arrow (excali-elbow-test--arrow 10 10 (list [0 0] [100 90]))))
     (setq excali--elements (list arrow))
     (excali--elbow-route-fresh arrow)
     (excali--select (list arrow))
     (should (= (length (excali--elbow-overlays arrow)) 5))
     (should-not (excali--transform-target)))))

(ert-deftest excali-elbow-test-no-point-editor ()
  "Elbow arrows have no point editor."
  (excali-elbow-test--in-window
   (let ((arrow (excali-elbow-test--arrow 10 10 (list [0 0] [100 90]))))
     (setq excali--elements (list arrow))
     (excali--select (list arrow))
     (excali-edit-linear t)
     (should-not excali--editing-linear))))

;;;; Restore

(ert-deftest excali-elbow-test-restore-reroutes-invalid-unbound ()
  "An unbound elbow arrow with a diagonal segment is routed on load,
keeping its ends and its version (restoreElements)."
  (excali-elbow-test--with-scene
   (let* ((raw `((id . "elbow") (type . "arrow") (x . 10) (y . 20)
                 (width . 100) (height . 50) (points . [[0 0] [100 50]])
                 (elbowed . t) (startBinding . :null) (endBinding . :null)
                 (index . "a0") (version . 7) (versionNonce . 42) (updated . 1)))
          (arrow (car (excali--restore-elements (list raw) :repair-bindings t))))
     (should (excali-elbow-test--orthogonal-p arrow))
     (should (> (length (excali--get arrow 'points)) 2))
     (let ((points (excali-elbow-test--scene-points arrow)))
       (should (excali-elbow-test--near (car points) '(10.0 20.0)))
       (should (excali-elbow-test--near (car (last points)) '(110.0 70.0))))
     (should (= (excali--get arrow 'version) 7))
     (should (= (excali--get arrow 'versionNonce) 42)))))

(ert-deftest excali-elbow-test-restore-keeps-valid-and-bound ()
  "Orthogonal or bound elbow arrows load untouched."
  (excali-elbow-test--with-scene
   (let* ((box `((id . "box") (type . "rectangle") (x . 200) (y . 0)
                 (width . 100) (height . 100)
                 (boundElements . [((id . "bound") (type . "arrow"))])))
          (valid `((id . "valid") (type . "arrow") (x . 0) (y . 0)
                   (points . [[0 0] [50 0] [50 40]]) (elbowed . t)))
          (bound `((id . "bound") (type . "arrow") (x . 0) (y . 0)
                   (points . [[0 0] [194 50]]) (elbowed . t)
                   (endBinding . ((elementId . "box") (fixedPoint . [-0.06 0.5])
                                  (mode . "orbit")))))
          (restored (excali--restore-elements (list box valid bound) :repair-bindings t)))
     (should (equal (excali--get (nth 1 restored) 'points) [[0 0] [50 0] [50 40]]))
     (should (equal (excali--get (nth 2 restored) 'points) [[0 0] [194 50]])))))

;;;; Resize and flip

(ert-deftest excali-elbow-test-multi-resize-scales-fixed-segments ()
  "Resizing with other elements scales the fixed segments with the points."
  (excali-elbow-test--with-scene
   (let ((a (excali-elbow-test--arrow 0 0 (list [0 0] [100 90])))
         (r (excali-elbow-test--box 200 0)))
     (setq excali--elements (list a r))
     (excali--elbow-route-fresh a)
     (excali--elbow-move-fixed-segment a 2 70 45)
     (let ((geometries (mapcar (lambda (e) (cons e (excali--snapshot-geometry e))) (list a r))))
       ;; Drag the east edge from 300 to 600: x doubles, y stays.
       (excali--resize-multiple geometries '(0.0 0.0 300.0 100.0) 'e '(600.0 . 50.0))
       (should (equal (excali-elbow-test--scene-points a)
                      '((0.0 0.0) (140.0 0.0) (140.0 90.0) (200.0 90.0))))
       (let ((seg (aref (excali--get a 'fixedSegments) 0)))
         (should (equal (alist-get 'start seg) [140.0 0.0]))
         (should (equal (alist-get 'end seg) [140.0 90.0]))
         (should (= (alist-get 'index seg) 2)))
       ;; Dragging further maps from the start, not from the last step.
       (excali--resize-multiple geometries '(0.0 0.0 300.0 100.0) 'e '(300.0 . 50.0))
       (should (equal (alist-get 'start (aref (excali--get a 'fixedSegments) 0))
                      [70.0 0.0]))))))

(ert-deftest excali-elbow-test-multi-resize-reroutes-bound ()
  "Without fixed segments a resized bound elbow arrow is routed again.
Fixed points are ratios of the shape, so the 6px gap doubles too."
  (excali-elbow-test--with-scene
   (let* ((a (excali-elbow-test--box 0 0))
          (b (excali-elbow-test--box 300 0))
          (arrow (excali-elbow-test--arrow 106 50 (list [0 0] [188 0]))))
     (setq excali--elements (list a b arrow))
     (excali--elbow-bind-end arrow 'start a)
     (excali--elbow-bind-end arrow 'end b)
     (excali--elbow-route-fresh arrow)
     (let ((geometries (mapcar (lambda (e) (cons e (excali--snapshot-geometry e)))
                               (list a b arrow))))
       (excali--resize-multiple geometries '(0.0 0.0 400.0 100.0) 'e '(800.0 . 50.0))
       (let ((points (excali-elbow-test--scene-points arrow)))
         (should (excali-elbow-test--orthogonal-p arrow))
         (should (excali-elbow-test--near (car points) '(212.0 50.01) 0.1))
         (should (excali-elbow-test--near (car (last points)) '(588.0 50.01) 0.1)))))))

(ert-deftest excali-elbow-test-flip-mirrors-fixed-segments ()
  "Flipping mirrors the fixed segments with the points."
  (excali-elbow-test--in-window
   (let ((a (excali-elbow-test--arrow 0 0 (list [0 0] [100 90]))))
     (setq excali--elements (list a))
     (excali--elbow-route-fresh a)
     (excali--elbow-move-fixed-segment a 2 70 45)
     (excali--select (list a))
     (excali-flip-horizontal)
     (should (excali-elbow-test--near (excali-elbow-test--scene-points a)
                                     '((100.0 0.0) (30.0 0.0) (30.0 90.0) (0.0 90.0))))
     (let ((seg (aref (excali--get a 'fixedSegments) 0)))
       (should (excali-elbow-test--near (append (alist-get 'start seg) nil) '(-70.0 0.0)))
       (should (excali-elbow-test--near (append (alist-get 'end seg) nil) '(-70.0 90.0)))))))

(ert-deftest excali-elbow-test-flip-rebinds-mirrored-ends ()
  "Flipping shapes with their arrow keeps it bound at mirrored fixed points."
  (excali-elbow-test--in-window
   (let* ((a (excali-elbow-test--box 0 0))
          (b (excali-elbow-test--box 300 200))
          (arrow (excali-elbow-test--arrow 106 50 (list [0 0] [188 200]))))
     (setq excali--elements (list a b arrow))
     (excali--elbow-bind-end arrow 'start a)
     (excali--elbow-bind-end arrow 'end b)
     (excali--elbow-route-fresh arrow)
     (excali--select (list a b arrow))
     (excali-flip-horizontal)
     (should (= (excali--get a 'x) 300.0))
     (should (= (excali--get b 'x) 0.0))
     (let ((points (excali-elbow-test--scene-points arrow))
           (start (excali--get arrow 'startBinding))
           (end (excali--get arrow 'endBinding)))
       (should (equal (alist-get 'elementId start) (excali--get a 'id)))
       (should (equal (alist-get 'elementId end) (excali--get b 'id)))
       (should (< (aref (alist-get 'fixedPoint start) 0) 0.5))
       (should (> (aref (alist-get 'fixedPoint end) 0) 0.5))
       (should (excali-elbow-test--orthogonal-p arrow))
       (should (excali-elbow-test--near (car points) '(294.0 50.0) 0.1))
       (should (excali-elbow-test--near (car (last points)) '(106.0 250.0) 0.1))
       ;; Moving a shape afterwards keeps the arrow on the mirrored side.
       (excali--put a 'y 10.0)
       (excali--follow (list a))
       (should (excali-elbow-test--near (car (excali-elbow-test--scene-points arrow))
                                       '(294.0 60.0) 0.1))))))

;;;; Hover

(ert-deftest excali-elbow-test-hover-highlight ()
  "Hovering a handle of the selected elbow arrow highlights it."
  (excali-elbow-test--in-window
   (let ((arrow (excali-elbow-test--arrow 10 10 (list [0 0] [100 90]))))
     (setq excali--elements (list arrow) excali--elbow-hover nil)
     (excali--elbow-route-fresh arrow)
     (excali--select (list arrow))
     (let ((mid (cdar (excali--elbow-midpoints arrow))))
       (should (excali--elbow-track-hover mid))
       (should (equal excali--elbow-hover mid))
       (should-not (excali--elbow-track-hover mid))
       (should (= (length (excali--elbow-overlays arrow)) 6))
       (should (member excali--elbow-hover-color
                       (mapcar (lambda (ov) (aref ov 7))
                               (excali--elbow-overlays arrow)))))
     ;; The end point.
     (excali--elbow-track-hover '(10.0 . 10.0))
     (should (equal excali--elbow-hover '(10.0 . 10.0)))
     ;; Away from every handle.
     (should (excali--elbow-track-hover '(500.0 . 500.0)))
     (should-not excali--elbow-hover)
     (should (= (length (excali--elbow-overlays arrow)) 5))
     ;; Unselected arrows get none.
     (excali--deselect)
     (should-not (excali--elbow-track-hover (cdar (excali--elbow-midpoints arrow)))))))

(provide 'excali-elbow-test)
;;; excali-elbow-test.el ends here
