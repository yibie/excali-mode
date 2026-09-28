;;; excal-cursor-test.el --- Pointer shapes  -*- lexical-binding: t; -*-

;; Expected cursors follow upstream's cursor.ts and the hover rules of
;; App.tsx handleCanvasPointerMove.

(require 'ert)
(require 'excal)
(require 'excal-test)

(defmacro excal-cursor-test--with-rect (var &rest body)
  "Run BODY with a scene holding one 100x50 rectangle at 10,20 bound to VAR."
  (declare (indent 1))
  `(excal-test--with-scene
    (let ((,var (excal--make-element "rectangle" 10 20
                                     (cons 'width 100.0) (cons 'height 50.0)
                                     (cons 'backgroundColor "#a5d8ff"))))
      (setq excal--elements (list ,var) excal--selection nil
            excal--scroll-x 0.0 excal--scroll-y 0.0
            excal--tool 'select excal--theme 'light
            excal--cursor-view nil excal--cursor-view-shown nil)
      ,@body)))

(ert-deftest excal-cursor-test-resize-cursor-directions ()
  "Edges and corners map like `getCursorForResizingElement'."
  (excal-cursor-test--with-rect rect
    (should (eq (excal--resize-cursor 'n rect) 'ns-resize))
    (should (eq (excal--resize-cursor 's rect) 'ns-resize))
    (should (eq (excal--resize-cursor 'e rect) 'ew-resize))
    (should (eq (excal--resize-cursor 'w rect) 'ew-resize))
    (should (eq (excal--resize-cursor 'nw rect) 'nwse-resize))
    (should (eq (excal--resize-cursor 'se rect) 'nwse-resize))
    (should (eq (excal--resize-cursor 'ne rect) 'nesw-resize))
    (should (eq (excal--resize-cursor 'sw rect) 'nesw-resize))
    (should (eq (excal--resize-cursor 'rotation rect) 'grab))
    (should (eq (excal--resize-cursor 'ne nil) 'nesw-resize))))

(ert-deftest excal-cursor-test-resize-cursor-rotates ()
  "Rotation steps the cursor by 45 degrees; mirrored boxes swap diagonals."
  (excal-cursor-test--with-rect rect
    (excal--put rect 'angle (/ float-pi 4))
    (should (eq (excal--resize-cursor 'n rect) 'nesw-resize))
    (should (eq (excal--resize-cursor 'e rect) 'nwse-resize))
    (should (eq (excal--resize-cursor 'nw rect) 'ns-resize))
    (excal--put rect 'angle (/ float-pi 2))
    (should (eq (excal--resize-cursor 'n rect) 'ew-resize))
    ;; Just under 22.5 degrees rounds back to no step.
    (excal--put rect 'angle 0.39)
    (should (eq (excal--resize-cursor 'n rect) 'ns-resize))
    ;; Close to a full turn wraps around.
    (excal--put rect 'angle (* 7 (/ float-pi 4)))
    (should (eq (excal--resize-cursor 'n rect) 'nwse-resize))
    (excal--put rect 'angle 0)
    (excal--put rect 'width -100.0)
    (should (eq (excal--resize-cursor 'nw rect) 'nesw-resize))
    (should (eq (excal--resize-cursor 'ne rect) 'nwse-resize))))

(ert-deftest excal-cursor-test-rotated-element-handles ()
  "Hovering a rotated element's handle gives the rotated cursor."
  (excal-cursor-test--with-rect rect
    (excal--put rect 'angle (/ float-pi 2))
    (setq excal--selection (list rect))
    ;; The top edge's midpoint, rotated 90 degrees about the center (60, 45),
    ;; lies right of the center.
    (pcase-let ((`(,x . ,y) (excal--rotate-point '(60.0 . 20.0) '(60.0 . 45.0)
                                                  (/ float-pi 2))))
      (should (eq (excal--cursor-at (cons x y)) 'ew-resize)))))

(ert-deftest excal-cursor-test-multi-selection-box ()
  "A multi-selection's common box uses unrotated cursors; its gaps move."
  (excal-cursor-test--with-rect rect
    (let ((other (excal--make-element "rectangle" 200 20
                                      (cons 'width 100.0) (cons 'height 50.0))))
      (excal--put rect 'angle (/ float-pi 4))
      (setq excal--elements (list rect other)
            excal--selection (list rect other))
      (pcase-let ((`(,x1 ,y1 ,x2 ,_y2) (excal--selection-bounds)))
        (should (eq (excal--cursor-at (cons (/ (+ x1 x2) 2) (- y1 4))) 'ns-resize))
        ;; Between the two elements, inside the box.
        (should (eq (excal--cursor-at (cons 150.0 45.0)) 'move))))))

(ert-deftest excal-cursor-test-tools ()
  "Each tool has its cursor."
  (excal-cursor-test--with-rect _rect
    (setq excal--tool 'hand)
    (should (eq (excal--cursor-at '(60.0 . 45.0)) 'grab))
    (setq excal--tool 'eraser)
    (should (eq (excal--cursor-at '(60.0 . 45.0)) 'eraser))
    (setq excal--theme 'dark)
    (should (eq (excal--cursor-at '(60.0 . 45.0)) 'eraser-dark))
    (dolist (tool '(rectangle diamond ellipse arrow line freedraw frame stickynote))
      (setq excal--tool tool)
      (should (eq (excal--cursor-at '(300.0 . 300.0)) 'crosshair)))))

(ert-deftest excal-cursor-test-text-tool-over-text ()
  "The text tool shows the I-beam over text, a crosshair elsewhere."
  (excal-cursor-test--with-rect _rect
    (let ((text (excal--make-element "text" 200 200 (cons 'width 80.0) (cons 'height 25.0)
                                     (cons 'text "hello") (cons 'containerId :null))))
      (setq excal--elements (list text) excal--tool 'text)
      (should (eq (excal--cursor-at '(240.0 . 212.0)) 'text))
      (should (eq (excal--cursor-at '(20.0 . 20.0)) 'crosshair)))))

(ert-deftest excal-cursor-test-locked-element-is-default ()
  "Locked elements cannot be moved, so they show the default cursor."
  (excal-cursor-test--with-rect rect
    (excal--put rect 'locked t)
    (should (eq (excal--cursor-at '(60.0 . 45.0)) 'default))))

(ert-deftest excal-cursor-test-link-icon-is-pointer ()
  "A linked element's icon shows the pointer."
  (excal-cursor-test--with-rect rect
    (excal--put rect 'link "https://example.com")
    (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excal--link-icon-box rect)))
      (should (eq (excal--cursor-at (cons (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))) 'pointer)))))

(ert-deftest excal-cursor-test-line-points-are-pointer ()
  "A selected line's points show the pointer; the line itself moves."
  (excal-cursor-test--with-rect _rect
    (let ((line (excal--make-element "line" 0 0
                                     (cons 'points (vector [0.0 0.0] [100.0 50.0] [0.0 100.0]))
                                     (cons 'width 100.0) (cons 'height 100.0))))
      (setq excal--elements (list line) excal--selection (list line))
      (should (eq (excal--cursor-at '(100.0 . 50.0)) 'pointer))
      (should (eq (excal--cursor-at '(50.0 . 25.0)) 'move)))))

(ert-deftest excal-cursor-test-drawing-points-crosshair ()
  "While a line's points are being clicked out, the crosshair stays."
  (excal-cursor-test--with-rect rect
    (let ((excal--multi-element rect))
      (should (eq (excal--cursor-at '(60.0 . 45.0)) 'crosshair)))))

(ert-deftest excal-cursor-test-fallback-pointer ()
  "Without the module view, cursors fall back to `pointer' shapes."
  (excal-cursor-test--with-rect _rect
    (insert " ")
    (dolist (case '((default . arrow) (move . hand) (grab . hand) (text . text)
                    (ns-resize . nhdrag) (ew-resize . hdrag) (nwse-resize . hdrag)
                    (crosshair . arrow) (eraser . arrow)))
      (excal--set-pointer (car case))
      (should (eq excal--cursor (car case)))
      (should (eq (get-text-property (point-min) 'pointer) (cdr case))))))

(ert-deftest excal-cursor-test-native-view-keeps-emacs-arrow ()
  "With the module view shown, Emacs' own pointer stays `arrow'."
  (excal-cursor-test--with-rect _rect
    (insert " ")
    (let ((calls nil))
      (cl-letf (((symbol-function 'excal-native-cursor-set)
                 (lambda (_view name) (push name calls) t)))
        (setq excal--cursor-view 'fake excal--cursor-view-shown t)
        (excal--set-pointer 'nwse-resize)
        (excal--set-pointer 'nwse-resize)
        (should (equal calls '("nwse-resize" "nwse-resize")))
        (should (eq (get-text-property (point-min) 'pointer) 'arrow))
        ;; A name the module rejects falls back to Emacs' shapes.
        (cl-letf (((symbol-function 'excal-native-cursor-set) (lambda (_v _n) nil)))
          (excal--set-pointer 'ew-resize)
          (should (eq (get-text-property (point-min) 'pointer) 'hdrag)))))))

(ert-deftest excal-cursor-test-module-knows-every-cursor ()
  "Every cursor Elisp can choose has a native shape."
  (skip-unless (fboundp 'excal-native-cursor-known-p))
  (dolist (cursor (mapcar #'car excal--cursor-fallbacks))
    (should (excal-native-cursor-known-p (symbol-name cursor))))
  (should-not (excal-native-cursor-known-p "no-such-cursor")))

(provide 'excal-cursor-test)
;;; excal-cursor-test.el ends here
