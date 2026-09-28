;;; excali-erase-test.el --- Eraser and links  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)
(require 'excali-test)

(defmacro excali-erase-test--scene (&rest body)
  "Run BODY in a window-backed scene with a fresh style."
  `(excali-test--in-window
    (excali--load-current-style nil)
    (setq excali--elements nil excali--tool-locked nil excali--multi-element nil
          excali--erase-marked nil excali--previous-tool 'select)
    ,@body))

(ert-deftest excali-erase-test-erases-groups-and-labels ()
  "The eraser takes whole outermost groups and containers with labels."
  (excali-erase-test--scene
   (let* ((a (excali-test--rect 10 10 (cons 'groupIds ["g"])))
          (b (excali-test--rect 200 10 (cons 'groupIds ["g"])))
          (box (excali--make-element "rectangle" 10 100 (cons 'width 80.0)
                                    (cons 'height 40.0) (cons 'strokeWidth 2)))
          (label (progn (setq excali--elements (list a b box))
                        (excali--add-bound-text box)))
          (keep (excali-test--rect 300 300)))
     (excali--set-text label "x")
     (setq excali--elements (append excali--elements (list keep)))
     (setq excali--tool 'eraser)
     ;; Sweep across a's left edge and the box's top edge.
     (excali-test--drag 10 5 10 100)
     (should (eq (excali--get a 'isDeleted) t))
     (should (eq (excali--get b 'isDeleted) t))
     (should (eq (excali--get box 'isDeleted) t))
     (should (eq (excali--get label 'isDeleted) t))
     (should-not (excali--get keep 'isDeleted)))))

(ert-deftest excali-erase-test-skips-locked-and-fades ()
  "Locked elements survive; marked elements render faded."
  (excali-erase-test--scene
   (let ((locked (excali-test--rect 10 10 (cons 'locked t)))
         (plain (excali-test--rect 100 10)))
     (setq excali--elements (list locked plain))
     (setq excali--erase-marked (list plain))
     (should (= (aref (excali--native-element plain) 15) 20.0))
     (setq excali--erase-marked nil)
     (setq excali--tool 'eraser)
     (excali-test--drag 10 5 10 30)
     (should-not (excali--get locked 'isDeleted)))))

(ert-deftest excali-erase-test-toggle-tools ()
  "Choosing the eraser or hand again returns to the previous tool."
  (excali-erase-test--scene
   (excali-select-tool 'rectangle)
   (excali-select-tool 'eraser)
   (should (eq excali--tool 'eraser))
   (excali-select-tool 'eraser)
   (should (eq excali--tool 'rectangle))
   (excali-select-tool 'hand)
   (excali-select-tool 'hand)
   (should (eq excali--tool 'rectangle))))

(ert-deftest excali-erase-test-locked-tool-selects-nothing ()
  "With the tool locked, new elements are not selected."
  (excali-erase-test--scene
   (setq excali--tool-locked t excali--tool 'rectangle)
   (excali-test--drag 20 20 60 60)
   (should (eq excali--tool 'rectangle))
   (should (null excali--selection))))

(ert-deftest excali-erase-test-links ()
  "Links are set on the selection; element links select their target."
  (excali-erase-test--scene
   (let ((a (excali-test--rect 10 10)) (b (excali-test--rect 300 200)))
     (setq excali--elements (list a b) excali--canvas-size '(400 . 300))
     (excali--select (list a))
     (excali-set-link (format "?element=%s" (excali--get b 'id)))
     (should (equal (excali--element-link-target (excali--get a 'link)) (excali--get b 'id)))
     (excali--deselect)
     ;; Clicking the icon of an unselected element follows the link.
     (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excali--link-icon-box a)))
       (let ((mx (round (/ (+ x1 x2) 2))) (my (round (/ (+ y1 y2) 2))))
         (should (eq (excali--link-at (cons (float mx) (float my))) a))
         (excali-test--drag mx my mx my)))
     (should (equal excali--selection (list b)))
     ;; Selected elements do not open links from their icon.
     (excali--select (list a))
     (pcase-let ((`(,x1 ,y1 ,_ ,_) (excali--link-icon-box a)))
       (should-not (excali--link-at (cons x1 y1))))
     (excali-set-link "")
     (should (eq (alist-get 'link a) :null)))))

(provide 'excali-erase-test)
;;; excali-erase-test.el ends here
