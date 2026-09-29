;;; excali-cursor-test.el --- Pointer shapes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Expected cursors follow upstream's cursor.ts and the hover rules of
;; App.tsx handleCanvasPointerMove.

(require 'ert)
(require 'excali)
(require 'excali-test)

(defmacro excali-cursor-test--with-rect (var &rest body)
  "Run BODY with a scene holding one 100x50 rectangle at 10,20 bound to VAR."
  (declare (indent 1))
  `(excali-test--with-scene
    (let ((,var (excali--make-element "rectangle" 10 20
                                     (cons 'width 100.0) (cons 'height 50.0)
                                     (cons 'backgroundColor "#a5d8ff"))))
      (setq excali--elements (list ,var) excali--selection nil
            excali--scroll-x 0.0 excali--scroll-y 0.0
            excali--tool 'select excali--theme 'light
            excali--cursor-view nil excali--cursor-view-shown nil)
      ,@body)))

(ert-deftest excali-cursor-test-resize-cursor-directions ()
  "Edges and corners map like `getCursorForResizingElement'."
  (excali-cursor-test--with-rect rect
    (should (eq (excali--resize-cursor 'n rect) 'ns-resize))
    (should (eq (excali--resize-cursor 's rect) 'ns-resize))
    (should (eq (excali--resize-cursor 'e rect) 'ew-resize))
    (should (eq (excali--resize-cursor 'w rect) 'ew-resize))
    (should (eq (excali--resize-cursor 'nw rect) 'nwse-resize))
    (should (eq (excali--resize-cursor 'se rect) 'nwse-resize))
    (should (eq (excali--resize-cursor 'ne rect) 'nesw-resize))
    (should (eq (excali--resize-cursor 'sw rect) 'nesw-resize))
    (should (eq (excali--resize-cursor 'rotation rect) 'grab))
    (should (eq (excali--resize-cursor 'ne nil) 'nesw-resize))))

(ert-deftest excali-cursor-test-resize-cursor-rotates ()
  "Rotation steps the cursor by 45 degrees; mirrored boxes swap diagonals."
  (excali-cursor-test--with-rect rect
    (excali--put rect 'angle (/ float-pi 4))
    (should (eq (excali--resize-cursor 'n rect) 'nesw-resize))
    (should (eq (excali--resize-cursor 'e rect) 'nwse-resize))
    (should (eq (excali--resize-cursor 'nw rect) 'ns-resize))
    (excali--put rect 'angle (/ float-pi 2))
    (should (eq (excali--resize-cursor 'n rect) 'ew-resize))
    ;; Just under 22.5 degrees rounds back to no step.
    (excali--put rect 'angle 0.39)
    (should (eq (excali--resize-cursor 'n rect) 'ns-resize))
    ;; Close to a full turn wraps around.
    (excali--put rect 'angle (* 7 (/ float-pi 4)))
    (should (eq (excali--resize-cursor 'n rect) 'nwse-resize))
    (excali--put rect 'angle 0)
    (excali--put rect 'width -100.0)
    (should (eq (excali--resize-cursor 'nw rect) 'nesw-resize))
    (should (eq (excali--resize-cursor 'ne rect) 'nwse-resize))))

(ert-deftest excali-cursor-test-rotated-element-handles ()
  "Hovering a rotated element's handle gives the rotated cursor."
  (excali-cursor-test--with-rect rect
    (excali--put rect 'angle (/ float-pi 2))
    (setq excali--selection (list rect))
    ;; The top edge's midpoint, rotated 90 degrees about the center (60, 45),
    ;; lies right of the center.
    (pcase-let ((`(,x . ,y) (excali--rotate-point '(60.0 . 20.0) '(60.0 . 45.0)
                                                  (/ float-pi 2))))
      (should (eq (excali--cursor-at (cons x y)) 'ew-resize)))))

(ert-deftest excali-cursor-test-multi-selection-box ()
  "A multi-selection's common box uses unrotated cursors; its gaps move."
  (excali-cursor-test--with-rect rect
    (let ((other (excali--make-element "rectangle" 200 20
                                      (cons 'width 100.0) (cons 'height 50.0))))
      (excali--put rect 'angle (/ float-pi 4))
      (setq excali--elements (list rect other)
            excali--selection (list rect other))
      (pcase-let ((`(,x1 ,y1 ,x2 ,_y2) (excali--selection-bounds)))
        (should (eq (excali--cursor-at (cons (/ (+ x1 x2) 2) (- y1 4))) 'ns-resize))
        ;; Between the two elements, inside the box.
        (should (eq (excali--cursor-at (cons 150.0 45.0)) 'move))))))

(ert-deftest excali-cursor-test-tools ()
  "Each tool has its cursor."
  (excali-cursor-test--with-rect _rect
    (setq excali--tool 'hand)
    (should (eq (excali--cursor-at '(60.0 . 45.0)) 'grab))
    (setq excali--tool 'eraser)
    (should (eq (excali--cursor-at '(60.0 . 45.0)) 'eraser))
    (setq excali--theme 'dark)
    (should (eq (excali--cursor-at '(60.0 . 45.0)) 'eraser-dark))
    (dolist (tool '(rectangle diamond ellipse arrow line freedraw frame stickynote))
      (setq excali--tool tool)
      (should (eq (excali--cursor-at '(300.0 . 300.0)) 'crosshair)))))

(ert-deftest excali-cursor-test-text-tool-over-text ()
  "The text tool shows the I-beam over text, a crosshair elsewhere."
  (excali-cursor-test--with-rect _rect
    (let ((text (excali--make-element "text" 200 200 (cons 'width 80.0) (cons 'height 25.0)
                                     (cons 'text "hello") (cons 'containerId :null))))
      (setq excali--elements (list text) excali--tool 'text)
      (should (eq (excali--cursor-at '(240.0 . 212.0)) 'text))
      (should (eq (excali--cursor-at '(20.0 . 20.0)) 'crosshair)))))

(ert-deftest excali-cursor-test-locked-element-is-default ()
  "Locked elements cannot be moved, so they show the default cursor."
  (excali-cursor-test--with-rect rect
    (excali--put rect 'locked t)
    (should (eq (excali--cursor-at '(60.0 . 45.0)) 'default))))

(ert-deftest excali-cursor-test-link-icon-is-pointer ()
  "A linked element's icon shows the pointer."
  (excali-cursor-test--with-rect rect
    (excali--put rect 'link "https://example.com")
    (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excali--link-icon-box rect)))
      (should (eq (excali--cursor-at (cons (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))) 'pointer)))))

(ert-deftest excali-cursor-test-line-points-are-pointer ()
  "A selected line's points show the pointer; the line itself moves."
  (excali-cursor-test--with-rect _rect
    (let ((line (excali--make-element "line" 0 0
                                     (cons 'points (vector [0.0 0.0] [100.0 50.0] [0.0 100.0]))
                                     (cons 'width 100.0) (cons 'height 100.0))))
      (setq excali--elements (list line) excali--selection (list line))
      (should (eq (excali--cursor-at '(100.0 . 50.0)) 'pointer))
      (should (eq (excali--cursor-at '(50.0 . 25.0)) 'move)))))

(ert-deftest excali-cursor-test-drawing-points-crosshair ()
  "While a line's points are being clicked out, the crosshair stays."
  (excali-cursor-test--with-rect rect
    (let ((excali--multi-element rect))
      (should (eq (excali--cursor-at '(60.0 . 45.0)) 'crosshair)))))

(defun excali-cursor-test--map-pointer (map point)
  "Return the pointer MAP shows at scene POINT."
  (let* ((xy (excali--window-xy point))
         (entry (lookup-image-map map (car xy) (cdr xy))))
    (plist-get (nth 2 entry) 'pointer)))

(defun excali-cursor-test--busy-scene ()
  "Fill the scene with elements of every hit rule; select the first."
  (let ((filled (excali--make-element "rectangle" 40 40 (cons 'width 160.0)
                                     (cons 'height 90.0) (cons 'backgroundColor "#a5d8ff")
                                     (cons 'link "https://example.com")))
        (hollow (excali--make-element "ellipse" 260 40 (cons 'width 140.0)
                                     (cons 'height 100.0)))
        (turned (excali--make-element "diamond" 60 200 (cons 'width 120.0)
                                     (cons 'height 80.0) (cons 'angle 0.5)
                                     (cons 'backgroundColor "#ffec99")))
        (arrow (excali--make-element "arrow" 250 220
                                    (cons 'points (vector [0.0 0.0] [120.0 40.0] [60.0 110.0]))
                                    (cons 'width 120.0) (cons 'height 110.0)))
        (locked (excali--make-element "rectangle" 420 200 (cons 'width 60.0)
                                     (cons 'height 60.0) (cons 'backgroundColor "#b2f2bb")
                                     (cons 'locked t))))
    (setq excali--elements (list filled hollow turned arrow locked)
          excali--selection (list filled))
    filled))

(ert-deftest excali-cursor-test-map-agrees-with-cursor-at ()
  "Away from area edges, the map shows what `excali--cursor-at' chooses."
  (dolist (view '((1.0 0.0 . 0.0) (2.0 -30.0 . -10.0)))
    (excali-cursor-test--with-rect _rect
      (setq excali--zoom (car view)
            excali--scroll-x (cadr view) excali--scroll-y (cddr view)
            excali--canvas-size '(1200 . 900) excali--pixel-scale 1.0)
      (excali-cursor-test--busy-scene)
      (let ((map (excali--pointer-map))
            (checked 0)
            (margin (/ 3.0 excali--zoom))
            (want (lambda (x y) (excali--emacs-pointer (excali--cursor-at (cons x y))))))
        (cl-loop for x from 0.0 to 520.0 by 17.0 do
                 (cl-loop for y from 0.0 to 360.0 by 13.0 do
                          (let ((expected (funcall want x y))
                                (w (excali--window-xy (cons x y))))
                            ;; Only points the window shows can be under the mouse.
                            (when (and (>= (car w) 0) (>= (cdr w) 0)
                                       (cl-every (lambda (d) (eq (funcall want (+ x (car d)) (+ y (cdr d)))
                                                            expected))
                                            (list (cons margin 0) (cons (- margin) 0)
                                                  (cons 0 margin) (cons 0 (- margin)))))
                              (cl-incf checked)
                              (should (equal (list x y (excali-cursor-test--map-pointer
                                                        map (cons x y)))
                                             (list x y expected)))))))
        (should (> checked 300))))))

(ert-deftest excali-cursor-test-map-per-tool ()
  "Other tools show their shape everywhere; text shows the I-beam on text."
  (excali-cursor-test--with-rect rect
    (let ((text (excali--make-element "text" 200 200 (cons 'width 80.0) (cons 'height 25.0)
                                     (cons 'text "hi") (cons 'fontSize 20))))
      (setq excali--elements (list rect text))
      (setq excali--tool 'hand)
      (should (eq (excali-cursor-test--map-pointer (excali--pointer-map) '(60.0 . 45.0)) 'hand))
      (setq excali--tool 'text)
      (let ((map (excali--pointer-map)))
        (should (eq (excali-cursor-test--map-pointer map '(240.0 . 212.0)) 'text))
        (should (eq (excali-cursor-test--map-pointer map '(60.0 . 45.0)) 'arrow)))
      (setq excali--tool 'rectangle)
      (should (eq (excali-cursor-test--map-pointer (excali--pointer-map) '(60.0 . 45.0)) 'arrow)))))

(ert-deftest excali-cursor-test-map-ring-for-transparent-shapes ()
  "A transparent shape's hot spot is its outline, not its inside."
  (excali-cursor-test--with-rect rect
    (excali--put rect 'backgroundColor "transparent")
    (let ((map (excali--pointer-map)))
      (should (eq (excali-cursor-test--map-pointer map '(10.0 . 45.0)) 'hand))
      (should (eq (excali-cursor-test--map-pointer map '(60.0 . 45.0)) 'arrow)))))

(ert-deftest excali-cursor-test-map-ids ()
  "Every hot spot has the canvas id, the prefix key its clicks carry."
  (excali-cursor-test--with-rect _rect
    (excali-cursor-test--busy-scene)
    (should (seq-every-p (lambda (entry) (eq (nth 1 entry) 'excali-canvas))
                         (excali--pointer-map)))))

(ert-deftest excali-cursor-test-hot-spot-clicks-reach-commands ()
  "Clicks over hot spots arrive prefixed and still run the mode's commands.
This is the path GUI events take: Emacs puts the hot spot's id in the
event position, and `read-key-sequence' turns it into a prefix key."
  (excali-cursor-test--with-rect _rect
    (excali-mode)
    (set-window-buffer (selected-window) (current-buffer))
    (let* ((window (selected-window))
           (posn (list window 'excali-canvas '(30 . 40) 0 nil 1 '(0 . 0) nil '(30 . 40) '(1 . 1)))
           (read (lambda (event)
                   (let ((unread-command-events (list event)))
                     (read-key-sequence nil)))))
      ;; A GUI session marks these when it first makes such events.
      (dolist (sym '(down-mouse-1 C-M-down-mouse-1 double-down-mouse-1 down-mouse-2
                     mouse-1 double-mouse-1 mouse-3))
        (put sym 'event-kind 'mouse-click))
      (dolist (case `((down-mouse-1 . excali-mouse-down)
                      (C-M-down-mouse-1 . excali-mouse-down)
                      (double-down-mouse-1 . excali-double-click)
                      (down-mouse-2 . excali-mouse-pan)
                      (mouse-1 . ignore) (double-mouse-1 . ignore) (mouse-3 . ignore)))
        (let ((keys (funcall read (list (car case) posn))))
          (should (equal (list (aref keys 0) (event-basic-type (aref keys 1))
                               (event-modifiers (aref keys 1)))
                         (list 'excali-canvas (event-basic-type (car case))
                               (event-modifiers (car case)))))
          (should (eq (key-binding keys t) (cdr case)))))
      ;; Motion is not prefixed, but its area is the hot spot's id.
      (put 'mouse-movement 'event-kind 'mouse-movement)
      (let ((keys (funcall read (list 'mouse-movement posn))))
        (should (eq (key-binding keys t) 'excali-mouse-move))
        (should (excali--canvas-area-p (event-start (aref keys 0)))))
      ;; Such events still give window coordinates.
      (should (equal (excali--event-window-xy (list 'down-mouse-1 posn)) '(30 . 40))))))

(ert-deftest excali-cursor-test-update-sets-map-in-place ()
  "The map goes into each surface's spec in place, translated, only on change."
  (excali-cursor-test--with-rect _rect
    (insert " ")
    (set-buffer-modified-p nil)
    (let ((whole (excali--make-canvas 100 100))
          (tile (excali--make-canvas 50 50)))
      (should (eq (cadr whole) :map))
      (setq excali--pointer-surfaces (list (cons whole (cons 0 0)) (cons tile (cons 40 30))))
      (excali--update-pointer)
      (let ((map (plist-get (cdr whole) :map)))
        (should map)
        (should (eq (excali-cursor-test--map-pointer map '(60.0 . 45.0)) 'hand))
        ;; The tile's copy is shifted by its offset.
        (let ((entry (lookup-image-map (plist-get (cdr tile) :map) (- 60 40) (- 45 30))))
          (should (eq (plist-get (nth 2 entry) 'pointer) 'hand)))
        ;; Nothing changed: the same map stays.
        (excali--update-pointer)
        (should (eq (plist-get (cdr whole) :map) map))
        (setq excali--tool 'hand)
        (excali--update-pointer)
        (should-not (eq (plist-get (cdr whole) :map) map)))
      (should-not (buffer-modified-p)))))

(ert-deftest excali-cursor-test-native-view-owns-pointer ()
  "With the module view shown, the map stays `arrow' and the module is told."
  (excali-cursor-test--with-rect _rect
    (let ((calls nil))
      (cl-letf (((symbol-function 'excali-native-cursor-set)
                 (lambda (_view name) (push name calls) t)))
        (setq excali--cursor-view 'fake excali--cursor-view-shown t)
        (should (equal (mapcar (lambda (e) (plist-get (nth 2 e) 'pointer)) (excali--pointer-map))
                       '(arrow)))
        (excali--set-pointer 'nwse-resize)
        (should (equal calls '("nwse-resize")))
        (should (eq excali--cursor 'nwse-resize))))))

(ert-deftest excali-cursor-test-module-knows-every-cursor ()
  "Every cursor Elisp can choose has a native shape."
  (skip-unless (fboundp 'excali-native-cursor-known-p))
  (dolist (cursor (mapcar #'car excali--cursor-fallbacks))
    (should (excali-native-cursor-known-p (symbol-name cursor))))
  (should-not (excali-native-cursor-known-p "no-such-cursor")))

(provide 'excali-cursor-test)
;;; excali-cursor-test.el ends here
