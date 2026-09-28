;;; excal-elbow-test.el --- Elbow arrows  -*- lexical-binding: t; -*-

;; Expected points marked "traced" were derived by hand, stepping through
;; upstream's elbowArrow.ts (getElbowArrowData, generateDynamicAABBs,
;; calculateGrid, astar with its binary heap, post-processing).

(require 'ert)
(require 'excal)
(require 'excal-test)

(defun excal-elbow-test--arrow (x y points &rest props)
  "Return an elbow arrow at X, Y with local POINTS (a list of [X Y]) and PROPS."
  (apply #'excal--make-element "arrow" x y
         (cons 'points (vconcat (mapcar (lambda (p) (vector (float (aref p 0)) (float (aref p 1))))
                                        points)))
         (cons 'elbowed t) (cons 'startBinding :null) (cons 'endBinding :null)
         (cons 'startArrowhead :null) (cons 'endArrowhead "arrow")
         (cons 'fixedSegments :null) (cons 'startIsSpecial :null) (cons 'endIsSpecial :null)
         props))

(defun excal-elbow-test--box (x y &rest props)
  "Return a 100x100 rectangle at X, Y with stroke width 2 and PROPS."
  (apply #'excal--make-element "rectangle" x y (cons 'width 100.0) (cons 'height 100.0)
         (cons 'strokeWidth 2) props))

(defun excal-elbow-test--scene-points (arrow)
  "Return ARROW's points in scene coordinates as a list of (X Y)."
  (let ((x (excal--get arrow 'x)) (y (excal--get arrow 'y)))
    (mapcar (lambda (p) (list (+ x (aref p 0)) (+ y (aref p 1))))
            (excal--get arrow 'points))))

(defun excal-elbow-test--near (a b &optional eps)
  "Return non-nil if lists of numbers or points A and B agree within EPS."
  (let ((eps (or eps 1e-6)))
    (cond ((numberp a) (< (abs (- a b)) eps))
          ((and (consp a) (= (length a) (length b)))
           (cl-every (lambda (x y) (excal-elbow-test--near x y eps)) a b)))))

(defun excal-elbow-test--orthogonal-p (arrow)
  "Return non-nil if every segment of ARROW is horizontal or vertical.
Bound ends sit at fixed points kept off exactly 0.5, so a free end
facing one may be 0.01 off, as upstream."
  (let ((points (append (excal--get arrow 'points) nil)))
    (cl-loop for (a b) on points
             while b
             always (or (< (abs (- (aref a 0) (aref b 0))) 0.02)
                        (< (abs (- (aref a 1) (aref b 1))) 0.02)))))

(defmacro excal-elbow-test--with-scene (&rest body)
  "Run BODY in a scene buffer at zoom 1."
  `(excal-test--with-scene
    (setq excal--elements nil excal--selection nil)
    ,@body))

(defmacro excal-elbow-test--in-window (&rest body)
  "Run BODY in a window-backed scene drawing elbow arrows."
  `(excal-test--in-window
    (setq excal--elements nil excal--tool-locked nil excal--multi-element nil)
    (excal--load-current-style nil)
    (setf (alist-get 'arrowType excal--current-style) "elbow")
    ,@body))

;;;; Headings and helpers

(ert-deftest excal-elbow-test-vector-to-heading ()
  "Ties go left, then up, as upstream `vectorToHeading'."
  (should (eq (excal--vector-to-heading 1 0) 'right))
  (should (eq (excal--vector-to-heading 100 100) 'up))
  (should (eq (excal--vector-to-heading -100 -100) 'left))
  (should (eq (excal--vector-to-heading 0 0) 'left))
  (should (eq (excal--vector-to-heading 10 20) 'down))
  (should (eq (excal--vector-to-heading 100 90) 'right)))

(ert-deftest excal-elbow-test-heading-from-element ()
  "Search cones pick the side a point lies off."
  (excal-elbow-test--with-scene
   (let* ((box (excal-elbow-test--box 0 0))
          (aabb (excal--aabb-for-element box)))
     (should (eq (excal--heading-from-element box aabb [106.0 50.0]) 'right))
     (should (eq (excal--heading-from-element box aabb [50.0 -6.0]) 'up))
     (should (eq (excal--heading-from-element box aabb [50.0 106.0]) 'down))
     (should (eq (excal--heading-from-element box aabb [-6.0 50.0]) 'left)))))

(ert-deftest excal-elbow-test-binary-heap ()
  "The heap pops in score order."
  (let ((heap (excal--eheap-create)) (out nil))
    (dolist (f '(5 3 8 1 9 2 7 4 6 0 11 10 13 12 15 14 17 16))
      (let ((n (excal--enode-create [0.0 0.0] 0 0)))
        (setf (excal--enode-f n) f)
        (excal--eheap-push heap n)))
    (while (> (excal--eheap-size heap) 0)
      (push (excal--enode-f (excal--eheap-pop heap)) out))
    (should (equal (nreverse out) (number-sequence 0 17)))))

(ert-deftest excal-elbow-test-normalize-fixed-point ()
  "Fixed points avoid exactly 0.5 and are clamped."
  (should (equal (excal--normalize-fixed-point [0.5 0.2]) [0.5001 0.2]))
  (should (equal (excal--normalize-fixed-point [12 -20]) [10.0 -10.0]))
  (should (equal (excal--normalize-fixed-point nil) [0.5001 0.5001])))

(ert-deftest excal-elbow-test-corner-points ()
  "Collinear interior points go, as do points within 1 px of the previous."
  (should (equal (excal--elbow-corner-points
                  (list [0.0 0.0] [10.0 0.0] [20.0 0.0] [20.0 10.0] [20.0 30.0] [40.0 30.0]))
                 (list [0.0 0.0] [20.0 0.0] [20.0 30.0] [40.0 30.0])))
  (should (equal (excal--elbow-remove-short-segments
                  (list [0.0 0.0] [0.5 0.0] [10.0 0.0] [10.0 10.0]))
                 (list [0.0 0.0] [10.0 0.0] [10.0 10.0]))))

;;;; Routing

(ert-deftest excal-elbow-test-route-diagonal ()
  "Traced: a free arrow along the exact diagonal starts up, as upstream."
  (excal-elbow-test--with-scene
   (let ((a (excal-elbow-test--arrow 0 0 (list [0 0] [100 100]))))
     (setq excal--elements (list a))
     (excal--elbow-route-fresh a)
     (should (equal (excal-elbow-test--scene-points a)
                    '((0.0 0.0) (0.0 -2.0) (50.0 -2.0) (50.0 100.0) (100.0 100.0))))
     (should (eq (excal--get a 'fixedSegments) nil)))))

(ert-deftest excal-elbow-test-route-z-shape ()
  "A free arrow mostly to the right bends once in the middle."
  (excal-elbow-test--with-scene
   (let ((a (excal-elbow-test--arrow 10 20 (list [0 0] [100 90]))))
     (setq excal--elements (list a))
     (excal--elbow-route-fresh a)
     (should (equal (excal-elbow-test--scene-points a)
                    '((10.0 20.0) (60.0 20.0) (60.0 110.0) (110.0 110.0))))
     (should (= (excal--get a 'width) 100.0))
     (should (= (excal--get a 'height) 90.0)))))

(ert-deftest excal-elbow-test-route-between-bound-boxes ()
  "Traced: boxes side by side get a straight arrow between their gaps."
  (excal-elbow-test--with-scene
   (let* ((a (excal-elbow-test--box 0 0))
          (b (excal-elbow-test--box 300 0))
          (arrow (excal-elbow-test--arrow 106 50 (list [0 0] [188 0]))))
     (setq excal--elements (list a b arrow))
     (should (equal (excal--elbow-fixed-point a [106.0 50.0]) [1.06 0.5001]))
     (excal--elbow-bind-end arrow 'start a)
     (excal--elbow-bind-end arrow 'end b)
     (excal--elbow-route-fresh arrow)
     (should (excal-elbow-test--near (excal-elbow-test--scene-points arrow)
                                     '((106.0 50.01) (294.0 50.01)))))))

(ert-deftest excal-elbow-test-route-l-shape-into-top ()
  "Traced: from a box's right side into the top of a box below right.
The start leaves right to the dongle at x = 142 (the start box grown by
the gap and padding) and the path bends once, above the end box."
  (excal-elbow-test--with-scene
   (let* ((a (excal-elbow-test--box 0 0))
          (b (excal-elbow-test--box 150 200))
          (arrow (excal-elbow-test--arrow 106 50 (list [0 0] [94 144]))))
     (setq excal--elements (list a b arrow))
     (excal--elbow-bind-end arrow 'start a)
     (excal--elbow-bind-end arrow 'end b)
     (should (equal (alist-get 'fixedPoint (excal--get arrow 'endBinding)) [0.5001 -0.06]))
     (excal--elbow-route-fresh arrow)
     (should (excal-elbow-test--near (excal-elbow-test--scene-points arrow)
                                     '((106.0 50.01) (200.01 50.01) (200.01 194.0)))))))

(ert-deftest excal-elbow-test-route-around-offset-boxes ()
  "Bound ends leave their shapes along the side's heading and stay orthogonal."
  (excal-elbow-test--with-scene
   (let* ((a (excal-elbow-test--box 0 0))
          (b (excal-elbow-test--box 300 250))
          (arrow (excal-elbow-test--arrow 106 50 (list [0 0] [194 250]))))
     (setq excal--elements (list a b arrow))
     (excal--elbow-bind-end arrow 'start a)
     (excal--elbow-bind-end arrow 'end b)
     (excal--elbow-route-fresh arrow)
     (let ((points (excal-elbow-test--scene-points arrow)))
       (should (excal-elbow-test--orthogonal-p arrow))
       (should (excal-elbow-test--near (car points) '(106.0 50.01)))
       (should (excal-elbow-test--near (car (last points)) '(294.0 300.01)))
       ;; Leaves A to the right and enters B from the left.
       (should (> (car (nth 1 points)) 106.0))
       (should (< (car (nth (- (length points) 2) points)) 294.0))))))

(ert-deftest excal-elbow-test-route-avoids-shape ()
  "An arrow from a box's left side to a point on its right goes around it."
  (excal-elbow-test--with-scene
   (let* ((a (excal-elbow-test--box 0 0))
          (arrow (excal-elbow-test--arrow -6 50 (list [0 0] [306 0]))))
     (setq excal--elements (list a arrow))
     (excal--elbow-bind-end arrow 'start a)
     (excal--elbow-route-fresh arrow)
     (should (excal-elbow-test--orthogonal-p arrow))
     (let ((points (excal-elbow-test--scene-points arrow)))
       (should (excal-elbow-test--near (car (last points)) '(300.0 50.0)))
       ;; First leaves left, and no segment crosses the box.
       (should (< (car (nth 1 points)) -6.0))
       (cl-loop for (p q) on points while q
                do (let ((mx (/ (+ (car p) (car q)) 2)) (my (/ (+ (cadr p) (cadr q)) 2)))
                     (should-not (and (< 0 mx 100) (< 0 my 100)))))))))

;;;; Fixed segments

(ert-deftest excal-elbow-test-move-and-release-segment ()
  "Traced: moving the middle segment fixes it; dragging an end keeps it;
releasing it routes again."
  (excal-elbow-test--with-scene
   (let ((a (excal-elbow-test--arrow 0 0 (list [0 0] [100 90]))))
     (setq excal--elements (list a))
     (excal--elbow-route-fresh a)
     (should (= (excal--elbow-move-fixed-segment a 2 70 45) 2))
     (should (equal (excal-elbow-test--scene-points a)
                    '((0.0 0.0) (70.0 0.0) (70.0 90.0) (100.0 90.0))))
     (should (equal (excal--elbow-fixed-indices a) '(2)))
     (should (eq (alist-get 'startIsSpecial a) :false))
     ;; Dragging the end down keeps the fixed segment's x.
     (excal--elbow-update a (list :points (list [0.0 0.0] [100.0 120.0])))
     (should (equal (excal-elbow-test--scene-points a)
                    '((0.0 0.0) (70.0 0.0) (70.0 120.0) (100.0 120.0))))
     (let ((seg (aref (excal--get a 'fixedSegments) 0)))
       (should (equal (alist-get 'start seg) [70.0 0.0]))
       (should (equal (alist-get 'end seg) [70.0 120.0]))
       (should (= (alist-get 'index seg) 2)))
     ;; Releasing it routes afresh.
     (should (excal--elbow-delete-fixed-segment a 2))
     (should (eq (excal--get a 'fixedSegments) nil))
     (should (excal-elbow-test--orthogonal-p a))
     (should (equal (car (last (excal-elbow-test--scene-points a))) '(100.0 120.0))))))

(ert-deftest excal-elbow-test-move-first-segment-adds-points ()
  "Moving the first segment of a free arrow adds a segment in front."
  (excal-elbow-test--with-scene
   (let ((a (excal-elbow-test--arrow 0 0 (list [0 0] [100 90]))))
     (setq excal--elements (list a))
     (excal--elbow-route-fresh a)
     ;; Segment 1 is horizontal (0,0)-(50,0); move it down to y = 30.
     (let ((index (excal--elbow-move-fixed-segment a 1 25 30)))
       (should (= index 2))
       (should (equal (excal-elbow-test--scene-points a)
                      '((0.0 0.0) (0.0 30.0) (50.0 30.0) (50.0 90.0) (100.0 90.0))))
       (should (equal (excal--elbow-fixed-indices a) '(2)))
       ;; Moving it again uses the new index.
       (should (= (excal--elbow-move-fixed-segment a index 25 40) 2))
       (should (equal (excal-elbow-test--scene-points a)
                      '((0.0 0.0) (0.0 40.0) (50.0 40.0) (50.0 90.0) (100.0 90.0))))))))

(ert-deftest excal-elbow-test-release-between-fixed-segments ()
  "Traced: fixing three segments, then releasing the middle one re-routes
only the part between its fixed neighbours."
  (excal-elbow-test--with-scene
   (let ((a (excal-elbow-test--arrow 0 0 (list [0 0] [200 150]))))
     (setq excal--elements (list a))
     (excal--elbow-route-fresh a)
     (should (equal (excal-elbow-test--scene-points a)
                    '((0.0 0.0) (100.0 0.0) (100.0 150.0) (200.0 150.0))))
     (should (= (excal--elbow-move-fixed-segment a 2 120 0) 2))
     ;; Moving the last segment adds a point after it.
     (should (= (excal--elbow-move-fixed-segment a 3 0 100) 3))
     (should (equal (excal-elbow-test--scene-points a)
                    '((0.0 0.0) (120.0 0.0) (120.0 100.0) (200.0 100.0) (200.0 150.0))))
     ;; Moving the first segment adds a point before it.
     (should (= (excal--elbow-move-fixed-segment a 1 0 30) 2))
     (should (equal (excal-elbow-test--scene-points a)
                    '((0.0 0.0) (0.0 30.0) (120.0 30.0) (120.0 100.0) (200.0 100.0)
                      (200.0 150.0))))
     (should (equal (excal--elbow-fixed-indices a) '(2 3 4)))
     (should (excal--elbow-delete-fixed-segment a 3))
     (should (equal (excal-elbow-test--scene-points a)
                    '((0.0 0.0) (0.0 30.0) (120.0 30.0) (120.0 100.0) (200.0 100.0)
                      (200.0 150.0))))
     (should (equal (excal--elbow-fixed-indices a) '(2 4)))
     (should (equal (alist-get 'startIsSpecial a) :false)))))

(ert-deftest excal-elbow-test-renormalize ()
  "Renormalizing merges collinear segments and re-indexes fixed ones."
  (excal-elbow-test--with-scene
   (let* ((a (excal-elbow-test--arrow
              0 0 (list [0 0] [50 0] [50 40] [50 90] [100 90] [100 120])
              (cons 'fixedSegments
                    (vector (list (cons 'start [50.0 40.0]) (cons 'end [50.0 90.0])
                                  (cons 'index 3))
                            (list (cons 'start [50.0 90.0]) (cons 'end [100.0 90.0])
                                  (cons 'index 4)))))))
     (setq excal--elements (list a))
     (excal--elbow-apply a (excal--elbow-renormalize (excal--elbow-snapshot a)))
     (should (equal (excal-elbow-test--scene-points a)
                    '((0.0 0.0) (50.0 0.0) (50.0 90.0) (100.0 90.0) (100.0 120.0))))
     (should (equal (excal--elbow-fixed-indices a) '(2 3))))))

;;;; Following shapes

(ert-deftest excal-elbow-test-reroutes-when-shape-moves ()
  "Moving a bound shape re-routes the elbow arrow to its fixed point."
  (excal-elbow-test--with-scene
   (let* ((a (excal-elbow-test--box 0 0))
          (b (excal-elbow-test--box 300 0))
          (arrow (excal-elbow-test--arrow 106 50 (list [0 0] [188 0]))))
     (setq excal--elements (list a b arrow))
     (excal--elbow-bind-end arrow 'start a)
     (excal--elbow-bind-end arrow 'end b)
     (excal--elbow-route-fresh arrow)
     (excal--put b 'y 200.0)
     (excal--follow (list b))
     (let ((points (excal-elbow-test--scene-points arrow)))
       (should (excal-elbow-test--orthogonal-p arrow))
       (should (excal-elbow-test--near (car points) '(106.0 50.01)))
       (should (excal-elbow-test--near (car (last points)) '(294.0 250.01)))
       (should (> (length points) 2))))))

(ert-deftest excal-elbow-test-convert-arrow-type ()
  "Setting the arrow type converts selected arrows both ways."
  (excal-elbow-test--with-scene
   (excal--load-current-style nil)
   (let ((arrow (excal--make-element "arrow" 0 0 (cons 'points (vector [0.0 0.0] [50.0 20.0] [100.0 90.0]))
                                     (cons 'startBinding :null) (cons 'endBinding :null)
                                     (cons 'roundness '((type . 2))))))
     (setq excal--elements (list arrow) excal--selection (list arrow))
     (cl-letf (((symbol-function 'excal--render) #'ignore))
       (excal-set-style 'arrowType "elbow")
       (should (excal--elbow-p arrow))
       (should (eq (excal--get arrow 'roundness) nil))
       (should (equal (excal-elbow-test--scene-points arrow)
                      '((0.0 0.0) (50.0 0.0) (50.0 90.0) (100.0 90.0))))
       (should (equal (excal--shown-style-value 'arrowType) "elbow"))
       (excal-set-style 'arrowType "round")
       (should-not (excal--elbow-p arrow))
       (should (equal (excal--get arrow 'roundness) '((type . 2))))
       (should (= (length (excal--get arrow 'points)) 2))))))

;;;; Editor

(ert-deftest excal-elbow-test-draw-between-shapes ()
  "Drawing with the elbow arrow type binds both ends and routes."
  (excal-elbow-test--in-window
   (let ((a (excal-elbow-test--box 20 20)) (b (excal-elbow-test--box 320 20)))
     (setq excal--elements (list a b))
     (setq excal--tool 'arrow)
     (excal-test--drag 125 70 315 70)
     (let ((arrow (car (last excal--elements))))
       (should (excal--elbow-p arrow))
       (should (equal (alist-get 'elementId (excal--get arrow 'startBinding)) (excal--get a 'id)))
       (should (equal (alist-get 'elementId (excal--get arrow 'endBinding)) (excal--get b 'id)))
       (should (equal (alist-get 'mode (excal--get arrow 'endBinding)) "orbit"))
       (should (excal-elbow-test--near (excal-elbow-test--scene-points arrow)
                                       '((126.0 70.01) (314.0 70.01))))
       (should (equal excal--selection (list arrow)))
       ;; Selected elbow arrows have no transform handles.
       (should-not (excal--transform-target))))))

(ert-deftest excal-elbow-test-draw-free ()
  "An unbound elbow arrow is routed orthogonally from the press to the release."
  (excal-elbow-test--in-window
   (setq excal--tool 'arrow)
   (excal-test--drag 10 10 110 100)
   (let ((arrow (car (last excal--elements))))
     (should (excal--elbow-p arrow))
     (should (equal (excal-elbow-test--scene-points arrow)
                    '((10.0 10.0) (60.0 10.0) (60.0 100.0) (110.0 100.0)))))))

(ert-deftest excal-elbow-test-click-click-takes-two-points ()
  "In click-click mode an elbow arrow ends at its second click."
  (excal-elbow-test--in-window
   (setq excal--tool 'arrow)
   (excal-test--drag 10 10 12 10)
   (should excal--multi-element)
   (excal-test--drag 110 100 110 100)
   (should-not excal--multi-element)
   (let ((arrow (car (last excal--elements))))
     (should (equal (car (last (excal-elbow-test--scene-points arrow))) '(110.0 100.0)))
     (should (excal-elbow-test--orthogonal-p arrow)))))

(ert-deftest excal-elbow-test-drag-midpoint-and-double-click ()
  "Dragging a segment midpoint fixes the segment; double-clicking it releases it."
  (excal-elbow-test--in-window
   (let ((arrow (excal-elbow-test--arrow 10 10 (list [0 0] [100 90]))))
     (setq excal--elements (list arrow))
     (excal--elbow-route-fresh arrow)
     (excal--select (list arrow))
     ;; Middle segment (60,10)-(60,100), midpoint (60,55).
     (excal-test--drag 60 55 80 55)
     (should (equal (excal-elbow-test--scene-points arrow)
                    '((10.0 10.0) (80.0 10.0) (80.0 100.0) (110.0 100.0))))
     (should (equal (excal--elbow-fixed-indices arrow) '(2)))
     (should (equal excal--selection (list arrow)))
     ;; Double-click the fixed midpoint.
     (let ((posn (excal-test--posn 80 55)))
       (setq unread-command-events (list (list 'mouse-1 posn)))
       (excal-double-click (list 'double-mouse-1 posn)))
     (should (eq (excal--get arrow 'fixedSegments) nil))
     (should (equal (excal-elbow-test--scene-points arrow)
                    '((10.0 10.0) (60.0 10.0) (60.0 100.0) (110.0 100.0)))))))

(ert-deftest excal-elbow-test-drag-end-binds ()
  "Dragging an end onto a shape binds it and re-routes."
  (excal-elbow-test--in-window
   (let ((box (excal-elbow-test--box 200 20))
         (arrow (excal-elbow-test--arrow 10 70 (list [0 0] [100 0]))))
     (setq excal--elements (list box arrow))
     (excal--elbow-route-fresh arrow)
     (excal--select (list arrow))
     (excal-test--drag 110 70 196 70)
     (should (equal (alist-get 'elementId (excal--get arrow 'endBinding)) (excal--get box 'id)))
     (should (excal-elbow-test--near (car (last (excal-elbow-test--scene-points arrow)))
                                     '(194.0 70.01)))
     ;; Dragging it away unbinds.
     (excal-test--drag 194 70 150 150)
     (should-not (excal--get arrow 'endBinding))
     (should (excal-elbow-test--orthogonal-p arrow)))))

(ert-deftest excal-elbow-test-bound-arrow-does-not-move-alone ()
  "A lone bound elbow arrow stays put; with both shapes it moves along."
  (excal-elbow-test--in-window
   (let* ((a (excal-elbow-test--box 20 20))
          (b (excal-elbow-test--box 320 20))
          (arrow (excal-elbow-test--arrow 126 70 (list [0 0] [188 0]))))
     (setq excal--elements (list a b arrow))
     (excal--elbow-bind-end arrow 'start a)
     (excal--elbow-bind-end arrow 'end b)
     (excal--elbow-route-fresh arrow)
     (excal--select (list arrow))
     (let ((x (excal--get arrow 'x)))
       (excal-test--drag 200 70 200 150)
       (should (= (excal--get arrow 'x) x))
       (should (excal--get arrow 'startBinding)))
     ;; With both shapes selected it moves and stays bound.
     (excal--select (list a b arrow))
     (excal-test--drag 70 70 70 120)
     (should (excal-elbow-test--near (car (excal-elbow-test--scene-points arrow))
                                     '(126.0 120.01)))
     (should (excal--get arrow 'startBinding))
     (should (excal--get arrow 'endBinding)))))

(ert-deftest excal-elbow-test-overlays ()
  "A selected elbow arrow shows its two ends and one midpoint per segment."
  (excal-elbow-test--in-window
   (let ((arrow (excal-elbow-test--arrow 10 10 (list [0 0] [100 90]))))
     (setq excal--elements (list arrow))
     (excal--elbow-route-fresh arrow)
     (excal--select (list arrow))
     (should (= (length (excal--elbow-overlays arrow)) 5))
     (should-not (excal--transform-target)))))

(ert-deftest excal-elbow-test-no-point-editor ()
  "Elbow arrows have no point editor."
  (excal-elbow-test--in-window
   (let ((arrow (excal-elbow-test--arrow 10 10 (list [0 0] [100 90]))))
     (setq excal--elements (list arrow))
     (excal--select (list arrow))
     (excal-edit-linear t)
     (should-not excal--editing-linear))))

(provide 'excal-elbow-test)
;;; excal-elbow-test.el ends here
