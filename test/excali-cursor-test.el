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

(ert-deftest excali-cursor-test-fallback-pointer ()
  "Without the module view, cursors fall back to `pointer' shapes."
  (excali-cursor-test--with-rect _rect
    (insert " ")
    (dolist (case '((default . arrow) (move . hand) (grab . hand) (text . text)
                    (ns-resize . nhdrag) (ew-resize . hdrag) (nwse-resize . hdrag)
                    (crosshair . arrow) (eraser . arrow)))
      (excali--set-pointer (car case))
      (should (eq excali--cursor (car case)))
      (should (eq (get-text-property (point-min) 'pointer) (cdr case))))))

(ert-deftest excali-cursor-test-native-view-keeps-emacs-arrow ()
  "With the module view shown, Emacs' own pointer stays `arrow'."
  (excali-cursor-test--with-rect _rect
    (insert " ")
    (let ((calls nil))
      (cl-letf (((symbol-function 'excali-native-cursor-set)
                 (lambda (_view name) (push name calls) t)))
        (setq excali--cursor-view 'fake excali--cursor-view-shown t)
        (excali--set-pointer 'nwse-resize)
        (excali--set-pointer 'nwse-resize)
        (should (equal calls '("nwse-resize" "nwse-resize")))
        (should (eq (get-text-property (point-min) 'pointer) 'arrow))
        ;; A name the module rejects falls back to Emacs' shapes.
        (cl-letf (((symbol-function 'excali-native-cursor-set) (lambda (_v _n) nil)))
          (excali--set-pointer 'ew-resize)
          (should (eq (get-text-property (point-min) 'pointer) 'hdrag)))))))

(ert-deftest excali-cursor-test-module-knows-every-cursor ()
  "Every cursor Elisp can choose has a native shape."
  (skip-unless (fboundp 'excali-native-cursor-known-p))
  (dolist (cursor (mapcar #'car excali--cursor-fallbacks))
    (should (excali-native-cursor-known-p (symbol-name cursor))))
  (should-not (excali-native-cursor-known-p "no-such-cursor")))

(provide 'excali-cursor-test)
;;; excali-cursor-test.el ends here
