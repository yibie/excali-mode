;;; excal-theme-test.el --- Dark theme  -*- lexical-binding: t; -*-

(require 'ert)
(require 'excal)
(require 'excal-test)

(ert-deftest excal-theme-test-dark-canvas ()
  "White becomes Excalidraw's #121212 dark canvas through the filter."
  (let ((dark (excal-native-fb-create 20 20))
        (reference (excal-native-fb-create 20 20)))
    (excal-native-fb-render dark 1.0 1.0 0.0 0.0 [] nil nil t)
    (excal-native-fb-render reference 1.0 1.0 0.0 0.0 [] nil "#121212" nil)
    (should (<= (excal-native-fb-diff dark reference) 1))))

(ert-deftest excal-theme-test-strokes-lighten ()
  "Dark strokes come out light in the dark theme."
  (excal-test--with-scene
   (let* ((line (excal--make-element "line" 2 10 (cons 'points [[0.0 0.0] [16.0 0.0]])
                                     (cons 'strokeWidth 4) (cons 'roughness 0)
                                     (cons 'strokeColor "#1e1e1e")))
          (dark (excal-native-fb-create 20 20))
          (canvas (excal-native-fb-create 20 20)))
     (setq excal--elements (list line))
     (excal-native-fb-render canvas 1.0 1.0 0.0 0.0 [] nil nil t)
     (excal-native-fb-render dark 1.0 1.0 0.0 0.0 (excal--visible-elements) nil nil t)
     ;; #1e1e1e -> about #d3d3d3 on #121212: a large difference.
     (should (> (excal-native-fb-diff dark canvas) 150)))))

(ert-deftest excal-theme-test-toggle ()
  "Toggling switches the theme and the selection color."
  (excal-test--with-scene
   (setq excal--backend nil)
   (should (equal (excal--selection-color) "#6965db"))
   (excal-toggle-theme)
   (should (eq excal--theme 'dark))
   (should (equal (excal--selection-color) "#b4b0ff"))
   (excal-toggle-theme)
   (should (eq excal--theme 'light))))

(provide 'excal-theme-test)
;;; excal-theme-test.el ends here
