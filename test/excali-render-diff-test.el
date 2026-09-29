;;; excali-render-diff-test.el --- Repainting only what changed  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;; After every kind of change, the framebuffer that `excali--render'
;; repainted in part must match a full render of the scene.  Clipped
;; repaints may differ from full ones by a few antialiasing levels.

(require 'ert)
(require 'excali)
(require 'excali-test)

(defconst excali-render-diff-test--tolerance 16
  "Largest channel difference allowed between partial and full repaints.")

(defun excali-render-diff-test--matches-full-p ()
  "Repaint what changed, then compare with a full render of the scene."
  (excali--render)
  (let ((full (excali-native-fb-create (car excali--canvas-size) (cdr excali--canvas-size))))
    (excali-native-fb-render full excali--pixel-scale excali--zoom
                            excali--scroll-x excali--scroll-y
                            (excali--visible-elements) nil
                            (excali--canvas-color) (eq excali--theme 'dark))
    (<= (excali-native-fb-diff excali--fb full) excali-render-diff-test--tolerance)))

(defmacro excali-render-diff-test--scene (&rest body)
  "Run BODY in a window-backed scene with a 600x400 framebuffer."
  `(excali-test--in-window
    (excali-mode)
    (setq excali--backend nil excali--canvas-size '(600 . 400)
          excali--fb (excali-native-fb-create 600 400)
          excali--rendered-items nil excali--rendered-key nil)
    ,@body))

(ert-deftest excali-render-diff-test-partial-repaints-match-full ()
  "Selecting, moving, restyling, reordering, deleting and adding all repaint right."
  (excali-render-diff-test--scene
   (let ((a (excali-test--rect 40 40 (cons 'width 120.0) (cons 'height 80.0)
                               (cons 'backgroundColor "#a5d8ff")))
         (b (excali--make-element "ellipse" 220 60 (cons 'width 100.0) (cons 'height 70.0)
                                  (cons 'backgroundColor "#ffc9c9")))
         (c (excali-test--rect 120 90 (cons 'width 90.0) (cons 'height 90.0)
                               (cons 'backgroundColor "#b2f2bb"))))
     (setq excali--elements (list a b c))
     (excali--render 'full)
     (let ((steps
            (list (cons "select" (lambda () (excali--select (list a))))
                  (cons "move" (lambda ()
                                 (excali--put a 'x 60.0) (excali--touch a)))
                  (cons "restyle" (lambda ()
                                    (excali--put b 'backgroundColor "#ffec99")
                                    (excali--touch b)))
                  (cons "reorder" (lambda ()
                                    (setq excali--elements (list c a b))))
                  (cons "reselect" (lambda () (excali--deselect) (excali--select (list b c))))
                  (cons "delete" (lambda ()
                                   (setq excali--elements (list a b))
                                   (excali--deselect)))
                  (cons "add" (lambda ()
                                (setq excali--elements
                                      (append excali--elements
                                              (list (excali-test--rect 400 250
                                                                       (cons 'width 60.0)
                                                                       (cons 'height 60.0)))))))
                  (cons "frame" (lambda ()
                                  (setq excali--elements
                                        (cons (excali--make-element
                                               "frame" 20 20 (cons 'width 300.0)
                                               (cons 'height 200.0) (cons 'name "F"))
                                              excali--elements))))
                  (cons "background" (lambda ()
                                       (setf (alist-get 'viewBackgroundColor
                                                        (alist-get 'appState excali--doc))
                                             "#fffce8"))))))
       (dolist (step steps)
         (funcall (cdr step))
         (should (equal (list (car step) (excali-render-diff-test--matches-full-p))
                        (list (car step) t))))))))

(ert-deftest excali-render-diff-test-nothing-changed-draws-nothing ()
  "A render of an unchanged scene repaints nothing."
  (excali-render-diff-test--scene
   (setq excali--elements (list (excali-test--rect 10 10)))
   (excali--render 'full)
   (let ((drawn nil))
     (cl-letf* ((render (symbol-function 'excali-native-fb-render))
                ((symbol-function 'excali-native-fb-render)
                 (lambda (&rest args) (setq drawn t) (apply render args))))
       (excali--render)
       (should-not drawn)
       ;; A change repaints only around it.
       (excali--put (car excali--elements) 'x 20.0)
       (excali--touch (car excali--elements))
       (should (consp (excali--changed-damage excali--rendered-items
                                              (excali--visible-elements))))
       (excali--render)
       (should drawn)))))

(provide 'excali-render-diff-test)
;;; excali-render-diff-test.el ends here
