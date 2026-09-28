;;; excal-erase-test.el --- Eraser and links  -*- lexical-binding: t; -*-

(require 'ert)
(require 'excal)
(require 'excal-test)

(defmacro excal-erase-test--scene (&rest body)
  "Run BODY in a window-backed scene with a fresh style."
  `(excal-test--in-window
    (excal--load-current-style nil)
    (setq excal--elements nil excal--tool-locked nil excal--multi-element nil
          excal--erase-marked nil excal--previous-tool 'select)
    ,@body))

(ert-deftest excal-erase-test-erases-groups-and-labels ()
  "The eraser takes whole outermost groups and containers with labels."
  (excal-erase-test--scene
   (let* ((a (excal-test--rect 10 10 (cons 'groupIds ["g"])))
          (b (excal-test--rect 200 10 (cons 'groupIds ["g"])))
          (box (excal--make-element "rectangle" 10 100 (cons 'width 80.0)
                                    (cons 'height 40.0) (cons 'strokeWidth 2)))
          (label (progn (setq excal--elements (list a b box))
                        (excal--add-bound-text box)))
          (keep (excal-test--rect 300 300)))
     (excal--set-text label "x")
     (setq excal--elements (append excal--elements (list keep)))
     (setq excal--tool 'eraser)
     ;; Sweep across a's left edge and the box's top edge.
     (excal-test--drag 10 5 10 100)
     (should (eq (excal--get a 'isDeleted) t))
     (should (eq (excal--get b 'isDeleted) t))
     (should (eq (excal--get box 'isDeleted) t))
     (should (eq (excal--get label 'isDeleted) t))
     (should-not (excal--get keep 'isDeleted)))))

(ert-deftest excal-erase-test-skips-locked-and-fades ()
  "Locked elements survive; marked elements render faded."
  (excal-erase-test--scene
   (let ((locked (excal-test--rect 10 10 (cons 'locked t)))
         (plain (excal-test--rect 100 10)))
     (setq excal--elements (list locked plain))
     (setq excal--erase-marked (list plain))
     (should (= (aref (excal--native-element plain) 15) 20.0))
     (setq excal--erase-marked nil)
     (setq excal--tool 'eraser)
     (excal-test--drag 10 5 10 30)
     (should-not (excal--get locked 'isDeleted)))))

(ert-deftest excal-erase-test-toggle-tools ()
  "Choosing the eraser or hand again returns to the previous tool."
  (excal-erase-test--scene
   (excal-select-tool 'rectangle)
   (excal-select-tool 'eraser)
   (should (eq excal--tool 'eraser))
   (excal-select-tool 'eraser)
   (should (eq excal--tool 'rectangle))
   (excal-select-tool 'hand)
   (excal-select-tool 'hand)
   (should (eq excal--tool 'rectangle))))

(ert-deftest excal-erase-test-locked-tool-selects-nothing ()
  "With the tool locked, new elements are not selected."
  (excal-erase-test--scene
   (setq excal--tool-locked t excal--tool 'rectangle)
   (excal-test--drag 20 20 60 60)
   (should (eq excal--tool 'rectangle))
   (should (null excal--selection))))

(ert-deftest excal-erase-test-links ()
  "Links are set on the selection; element links select their target."
  (excal-erase-test--scene
   (let ((a (excal-test--rect 10 10)) (b (excal-test--rect 300 200)))
     (setq excal--elements (list a b) excal--canvas-size '(400 . 300))
     (excal--select (list a))
     (excal-set-link (format "?element=%s" (excal--get b 'id)))
     (should (equal (excal--element-link-target (excal--get a 'link)) (excal--get b 'id)))
     (excal--deselect)
     ;; Clicking the icon of an unselected element follows the link.
     (pcase-let ((`(,x1 ,y1 ,x2 ,y2) (excal--link-icon-box a)))
       (let ((mx (round (/ (+ x1 x2) 2))) (my (round (/ (+ y1 y2) 2))))
         (should (eq (excal--link-at (cons (float mx) (float my))) a))
         (excal-test--drag mx my mx my)))
     (should (equal excal--selection (list b)))
     ;; Selected elements do not open links from their icon.
     (excal--select (list a))
     (pcase-let ((`(,x1 ,y1 ,_ ,_) (excal--link-icon-box a)))
       (should-not (excal--link-at (cons x1 y1))))
     (excal-set-link "")
     (should (eq (alist-get 'link a) :null)))))

(provide 'excal-erase-test)
;;; excal-erase-test.el ends here
