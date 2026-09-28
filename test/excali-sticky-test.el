;;; excali-sticky-test.el --- Sticky notes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)
(require 'excali-test)

(ert-deftest excali-sticky-test-seeded-random ()
  "seededRandom matches upstream mulberry32 (reference values from JS
integer semantics)."
  (should (equal (excali-native-seeded-random 1 3)
                 [0.6270739405881613 0.002735721180215478 0.5274470399599522]))
  (should (equal (excali-native-seeded-random 2147483647 2)
                 [0.4290980885270983 0.12713524978607893])))

(ert-deftest excali-sticky-test-footer ()
  "The footer shows day and month, and the year when not this year."
  (let* ((this-year (decoded-time-year (decode-time)))
         (ms (lambda (y) (* 1000.0 (float-time (encode-time (list 0 0 12 27 9 y)))))))
    (should (equal (excali--sticky-footer
                    (list (cons 'type "stickynote") (cons 'width 250.0)
                          (cons 'created (funcall ms this-year))))
                   "27 Sep"))
    (should (equal (excali--sticky-footer
                    (list (cons 'type "stickynote") (cons 'width 250.0)
                          (cons 'created (funcall ms 2020))))
                   "27 Sep 2020"))
    ;; Too narrow for the year.
    (should (equal (excali--sticky-footer
                    (list (cons 'type "stickynote") (cons 'width 100.0)
                          (cons 'created (funcall ms 2020))))
                   "27 Sep"))))

(ert-deftest excali-sticky-test-render ()
  "A sticky note draws its shadow and paper color."
  (excali-test--with-scene
   (let ((note (excali--make-element "stickynote" 20 20 (cons 'width 120.0)
                                    (cons 'height 120.0) (cons 'backgroundColor "#ffdf6b")
                                    (cons 'roughness 1) (cons 'roundness '((type . 2)))))
         (fb (excali-native-fb-create 200 200))
         (white (excali-native-fb-create 200 200))
         (plain (excali-native-fb-create 200 200)))
     (excali-native-fb-render white 1.0 1.0 0.0 0.0 [] nil)
     (setq excali--elements (list note))
     (excali-native-fb-render fb 1.0 1.0 0.0 0.0 (excali--visible-elements) nil)
     ;; Yellow paper differs strongly from white.
     (should (> (excali-native-fb-diff fb white) 100))
     ;; Roughness 2 curls a corner: the picture changes.
     (excali--put note 'roughness 2)
     (remhash note excali--native-cache)
     (excali-native-fb-render plain 1.0 1.0 0.0 0.0 (excali--visible-elements) nil)
     (should (> (excali-native-fb-diff fb plain) 0)))))

(ert-deftest excali-sticky-test-tool ()
  "A click with the sticky tool places a 250 px note centered there."
  (excali-test--in-window
   (excali--load-current-style nil)
   (setq excali--elements nil excali--tool 'stickynote excali--tool-locked nil)
   (cl-letf (((symbol-function 'excali-edit-text) #'ignore))
     (excali-test--drag 300 300 300 300)
     (let ((note (car (last excali--elements))))
       (should (equal (excali--get note 'type) "stickynote"))
       (should (equal (list (excali--get note 'x) (excali--get note 'y)
                            (excali--get note 'width) (excali--get note 'baseHeight))
                      '(175.0 175.0 250.0 250.0)))
       (should (equal excali--selection (list note)))
       (should (eq excali--tool 'select)))
     ;; A small drag still yields the minimum size.
     (setq excali--tool 'stickynote)
     (excali-test--drag 10 10 40 30)
     (should (= (excali--get (car (last excali--elements)) 'width) 75.0)))))

(provide 'excali-sticky-test)
;;; excali-sticky-test.el ends here
