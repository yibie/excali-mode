;;; excali-theme-test.el --- Dark theme  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)
(require 'excali-test)

(ert-deftest excali-theme-test-dark-canvas ()
  "White becomes Excalidraw's #121212 dark canvas through the filter."
  (let ((dark (excali-native-fb-create 20 20))
        (reference (excali-native-fb-create 20 20)))
    (excali-native-fb-render dark 1.0 1.0 0.0 0.0 [] nil nil t)
    (excali-native-fb-render reference 1.0 1.0 0.0 0.0 [] nil "#121212" nil)
    (should (<= (excali-native-fb-diff dark reference) 1))))

(ert-deftest excali-theme-test-strokes-lighten ()
  "Dark strokes come out light in the dark theme."
  (excali-test--with-scene
   (let* ((line (excali--make-element "line" 2 10 (cons 'points [[0.0 0.0] [16.0 0.0]])
                                     (cons 'strokeWidth 4) (cons 'roughness 0)
                                     (cons 'strokeColor "#1e1e1e")))
          (dark (excali-native-fb-create 20 20))
          (canvas (excali-native-fb-create 20 20)))
     (setq excali--elements (list line))
     (excali-native-fb-render canvas 1.0 1.0 0.0 0.0 [] nil nil t)
     (excali-native-fb-render dark 1.0 1.0 0.0 0.0 (excali--visible-elements) nil nil t)
     ;; #1e1e1e -> about #d3d3d3 on #121212: a large difference.
     (should (> (excali-native-fb-diff dark canvas) 150)))))

(ert-deftest excali-theme-test-toggle ()
  "Toggling switches the theme and the selection color."
  (excali-test--with-scene
   (setq excali--backend nil)
   (should (equal (excali--selection-color) "#6965db"))
   (excali-toggle-theme)
   (should (eq excali--theme 'dark))
   (should (equal (excali--selection-color) "#b4b0ff"))
   (excali-toggle-theme)
   (should (eq excali--theme 'light))))

(provide 'excali-theme-test)
;;; excali-theme-test.el ends here
