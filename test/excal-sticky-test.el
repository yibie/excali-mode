;;; excal-sticky-test.el --- Sticky notes  -*- lexical-binding: t; -*-

(require 'ert)
(require 'excal)
(require 'excal-test)

(ert-deftest excal-sticky-test-seeded-random ()
  "seededRandom matches upstream mulberry32 (reference values from JS
integer semantics)."
  (should (equal (excal-native-seeded-random 1 3)
                 [0.6270739405881613 0.002735721180215478 0.5274470399599522]))
  (should (equal (excal-native-seeded-random 2147483647 2)
                 [0.4290980885270983 0.12713524978607893])))

(ert-deftest excal-sticky-test-footer ()
  "The footer shows day and month, and the year when not this year."
  (let* ((this-year (decoded-time-year (decode-time)))
         (ms (lambda (y) (* 1000.0 (float-time (encode-time (list 0 0 12 27 9 y)))))))
    (should (equal (excal--sticky-footer
                    (list (cons 'type "stickynote") (cons 'width 250.0)
                          (cons 'created (funcall ms this-year))))
                   "27 Sep"))
    (should (equal (excal--sticky-footer
                    (list (cons 'type "stickynote") (cons 'width 250.0)
                          (cons 'created (funcall ms 2020))))
                   "27 Sep 2020"))
    ;; Too narrow for the year.
    (should (equal (excal--sticky-footer
                    (list (cons 'type "stickynote") (cons 'width 100.0)
                          (cons 'created (funcall ms 2020))))
                   "27 Sep"))))

(ert-deftest excal-sticky-test-render ()
  "A sticky note draws its shadow and paper color."
  (excal-test--with-scene
   (let ((note (excal--make-element "stickynote" 20 20 (cons 'width 120.0)
                                    (cons 'height 120.0) (cons 'backgroundColor "#ffdf6b")
                                    (cons 'roughness 1) (cons 'roundness '((type . 2)))))
         (fb (excal-native-fb-create 200 200))
         (white (excal-native-fb-create 200 200))
         (plain (excal-native-fb-create 200 200)))
     (excal-native-fb-render white 1.0 1.0 0.0 0.0 [] nil)
     (setq excal--elements (list note))
     (excal-native-fb-render fb 1.0 1.0 0.0 0.0 (excal--visible-elements) nil)
     ;; Yellow paper differs strongly from white.
     (should (> (excal-native-fb-diff fb white) 100))
     ;; Roughness 2 curls a corner: the picture changes.
     (excal--put note 'roughness 2)
     (remhash note excal--native-cache)
     (excal-native-fb-render plain 1.0 1.0 0.0 0.0 (excal--visible-elements) nil)
     (should (> (excal-native-fb-diff fb plain) 0)))))

(ert-deftest excal-sticky-test-tool ()
  "A click with the sticky tool places a 250 px note centered there."
  (excal-test--in-window
   (excal--load-current-style nil)
   (setq excal--elements nil excal--tool 'stickynote excal--tool-locked nil)
   (cl-letf (((symbol-function 'excal-edit-text) #'ignore))
     (excal-test--drag 300 300 300 300)
     (let ((note (car (last excal--elements))))
       (should (equal (excal--get note 'type) "stickynote"))
       (should (equal (list (excal--get note 'x) (excal--get note 'y)
                            (excal--get note 'width) (excal--get note 'baseHeight))
                      '(175.0 175.0 250.0 250.0)))
       (should (equal excal--selection (list note)))
       (should (eq excal--tool 'select)))
     ;; A small drag still yields the minimum size.
     (setq excal--tool 'stickynote)
     (excal-test--drag 10 10 40 30)
     (should (= (excal--get (car (last excal--elements)) 'width) 75.0)))))

(provide 'excal-sticky-test)
;;; excal-sticky-test.el ends here
