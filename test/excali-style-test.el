;;; excali-style-test.el --- The style panels and right-button panning  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Colors and shades follow upstream's colors.ts and its color picker;
;; which properties the panel offers follows getShapeActionPredicates.

(require 'ert)
(require 'excali)
(require 'excali-test)

(defmacro excali-style-test--with-box (var &rest body)
  "Run BODY with a selected rectangle bound to VAR and a fresh style."
  (declare (indent 1))
  `(excali-test--in-window
    (excali--load-current-style nil)
    (let ((,var (excali--apply-current-style
                 (excali--make-element "rectangle" 10 20 (cons 'width 100.0)
                                       (cons 'height 50.0)))))
      (setq excali--elements (list ,var) excali--selection (list ,var))
      ,@body)))

(ert-deftest excali-style-test-picker-colors-and-shades ()
  "Colors come at the picker's shade; 1..5 pick shades of the current family."
  (excali-style-test--with-box box
    ;; Stroke starts at shade 4, the darkest.
    (excali-style--strokeColor-red)
    (should (equal (excali--get box 'strokeColor) "#e03131"))
    (excali-style--strokeColor-blue)
    (should (equal (excali--get box 'strokeColor) "#1971c2"))
    (excali-style--strokeColor-shade-3)
    (should (equal (excali--get box 'strokeColor) "#4dabf7"))
    ;; The families now show at that shade.
    (excali-style--strokeColor-green)
    (should (equal (excali--get box 'strokeColor) "#69db7c"))
    ;; Fixed colors have no shades.
    (excali-style--strokeColor-black)
    (should (equal (excali--get box 'strokeColor) "#1e1e1e"))
    (should-not (excali--color-family "#1e1e1e"))
    (excali-style--strokeColor-shade-2)
    (should (equal (excali--get box 'strokeColor) "#1e1e1e"))
    ;; Backgrounds start at shade 1.
    (excali-style--backgroundColor-yellow)
    (should (equal (excali--get box 'backgroundColor) "#ffec99"))
    (excali-style--backgroundColor-transparent)
    (should (equal (excali--get box 'backgroundColor) "transparent"))
    ;; A hex code.
    (excali-style--backgroundColor-hex "#123abc")
    (should (equal (excali--get box 'backgroundColor) "#123abc"))
    ;; New elements take the choices too.
    (should (equal (excali--style-value 'strokeColor) "#1e1e1e"))))

(ert-deftest excali-style-test-picker-matches-upstream-order ()
  "Fifteen colors on q..b in upstream's order, each an upstream color."
  (should (= (length excali--color-picker-entries) 15))
  (should (equal (append excali--color-picker-keys nil)
                 '("q" "w" "e" "r" "t" "a" "s" "d" "f" "g" "z" "x" "c" "v" "b")))
  (should (equal (excali--picker-color 'red 4) "#e03131"))
  (should (equal (excali--picker-color 'bronze 1) "#eaddd7"))
  (should (equal (excali--color-family "#FFC9C9") '(red . 1))))

(ert-deftest excali-style-test-choice-commands ()
  "Each choice sets its value; other values are read and clamped."
  (excali-style-test--with-box box
    (excali-style--fillStyle-1)
    (should (equal (excali--get box 'fillStyle) "hachure"))
    (excali-style--strokeWidth-3)
    (should (= (excali--get box 'strokeWidth) 4))
    (excali-style--strokeStyle-2)
    (should (equal (excali--get box 'strokeStyle) "dashed"))
    (excali-style--roughness-1)
    (should (= (excali--get box 'roughness) 0))
    (excali-style--roundness-1)
    (should (eq (alist-get 'roundness box) :null))
    (excali-style--opacity-6)
    (should (= (excali--get box 'opacity) 50))
    (excali-style--opacity-other 250)
    (should (= (excali--get box 'opacity) 100))))

(ert-deftest excali-style-test-labels-follow-upstream ()
  "The panel speaks upstream's words."
  (should (equal (car (rassoc "round" (excali--style-choices 'arrowType))) "Curved arrow"))
  (should (equal (car (rassoc 36 (excali--style-choices 'fontSize))) "Very large"))
  (should (equal (mapcar #'car (seq-take (excali--style-choices 'fontFamily) 3))
                 '("Hand-drawn" "Normal" "Code"))))

(ert-deftest excali-style-test-shown-properties ()
  "The panel offers what the selection or the drawing tool takes."
  (excali-style-test--with-box box
    ;; A rectangle: shape properties, no arrow or text ones.
    (should (excali--style-shown-p 'strokeColor))
    (should (excali--style-shown-p 'roundness))
    (should-not (excali--style-shown-p 'arrowType))
    (should-not (excali--style-shown-p 'fontFamily))
    ;; Fill only under a background.
    (excali-set-style 'backgroundColor "transparent")
    (should-not (excali--style-shown-p 'fillStyle))
    (excali-set-style 'backgroundColor "#a5d8ff")
    (should (excali--style-shown-p 'fillStyle))
    ;; An arrow selected.
    (let ((arrow (excali--make-element "arrow" 0 0 (cons 'points (vector [0.0 0.0] [50.0 0.0])))))
      (setq excali--selection (list arrow))
      (should (excali--style-shown-p 'arrowType))
      (should (excali--style-shown-p 'endArrowhead))
      (should-not (excali--style-shown-p 'roundness)))
    ;; Nothing selected: the text tool offers text properties.
    (setq excali--selection nil excali--tool 'text)
    (should (excali--style-shown-p 'fontSize))
    (should-not (excali--style-shown-p 'strokeWidth))
    ;; Nothing selected, selection tool: everything, for new elements.
    (setq excali--tool 'select)
    (should (excali--style-shown-p 'arrowType))
    (should (excali--style-shown-p 'fontFamily))
    (ignore box)))

(ert-deftest excali-style-test-panels-are-prefixes ()
  "The style panel and every sub-panel are transient prefixes."
  (dolist (name '(excali-style excali-style-strokeColor excali-style-backgroundColor
                  excali-style-fillStyle excali-style-strokeWidth excali-style-strokeStyle
                  excali-style-roughness excali-style-roundness excali-style-opacity
                  excali-style-arrowType excali-style-startArrowhead excali-style-endArrowhead
                  excali-style-fontFamily excali-style-fontSize excali-style-textAlign
                  excali-style-verticalAlign))
    (should (commandp name))
    (should (get name 'transient--layout))))

(ert-deftest excali-style-test-right-drag-pans ()
  "Dragging with the right button pans the view."
  (excali-test--in-window
   (excali-mode)
   (setq excali--scroll-x 0.0 excali--scroll-y 0.0)
   (should (eq (key-binding [down-mouse-3]) 'excali-mouse-pan))
   (let ((start (excali-test--posn 100 100)))
     (setq unread-command-events
           (list (list 'mouse-movement (excali-test--posn 130 110))
                 (list 'mouse-movement (excali-test--posn 160 140))
                 (list 'mouse-3 (excali-test--posn 160 140))))
     (excali-mouse-pan (list 'down-mouse-3 start)))
   (should (= excali--scroll-x 60.0))
   (should (= excali--scroll-y 40.0))
   (should (null unread-command-events))))

(provide 'excali-style-test)
;;; excali-style-test.el ends here
