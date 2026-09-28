;;; excali-frame-test.el --- Frame behavior  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)
(require 'excali-test)

(defmacro excali-frame-test--scene (&rest body)
  "Run BODY in a window-backed scene with a fresh style."
  `(excali-test--in-window
    (excali--load-current-style nil)
    (setq excali--elements nil excali--tool-locked nil excali--multi-element nil
          excali--grid-enabled nil excali--objects-snap-enabled nil)
    ,@body))

(ert-deftest excali-frame-test-new-frame-adopts-contents ()
  "A new frame adopts what is completely inside, whole groups only."
  (excali-frame-test--scene
   (let* ((inside (excali-test--rect 50 50))
          (partial (excali-test--rect 195 50))
          (g1 (excali-test--rect 60 100 (cons 'groupIds ["g"])))
          (g2 (excali-test--rect 300 100 (cons 'groupIds ["g"]))))
     (setq excali--elements (list inside partial g1 g2))
     (setq excali--tool 'frame)
     (excali-test--drag 20 20 200 200)
     (let ((frame (car (seq-filter #'excali--frame-p excali--elements))))
       (should frame)
       (should (equal (excali--get frame 'strokeColor) "#bbb"))
       (should (equal (excali--frame-children frame) (list inside)))
       ;; The child sits right below the frame in z-order.
       (should (eq (cadr (memq inside excali--elements)) frame))
       (should (equal (excali-frame-name frame) "Frame"))))))

(ert-deftest excali-frame-test-draw-inside-joins ()
  "Elements drawn inside a frame belong to it."
  (excali-frame-test--scene
   (setq excali--tool 'frame)
   (excali-test--drag 20 20 200 200)
   (let ((frame (car excali--elements)))
     (setq excali--tool 'rectangle)
     (excali-test--drag 50 50 80 80)
     (should (equal (excali--get (car (last excali--elements)) 'frameId)
                    (excali--get frame 'id)))
     (setq excali--tool 'rectangle)
     (excali-test--drag 300 300 330 330)
     (should-not (excali--get (car (last excali--elements)) 'frameId)))))

(ert-deftest excali-frame-test-moving-frame-moves-children ()
  "Dragging a frame by its border carries its children."
  (excali-frame-test--scene
   (let ((child (excali-test--rect 50 50)))
     (setq excali--elements (list child))
     (setq excali--tool 'frame)
     (excali-test--drag 20 20 200 200)
     (let ((frame (seq-find #'excali--frame-p excali--elements)))
       (excali--deselect)
       (excali--select (list frame))
       ;; Press inside the selected frame's box (its border would resize).
       (excali-test--drag 100 100 130 120)
       (should (= (excali--get frame 'x) 50.0))
       (should (= (excali--get child 'x) 80.0))))))

(ert-deftest excali-frame-test-drag-in-and-out ()
  "Dropping an element on a frame adds it; dragging it off removes it."
  (excali-frame-test--scene
   (let ((loose (excali-test--rect 300 300 (cons 'backgroundColor "#ffc9c9"))))
     (setq excali--elements (list loose))
     (setq excali--tool 'frame)
     (excali-test--drag 20 20 200 200)
     (let ((frame (seq-find #'excali--frame-p excali--elements)))
       (setq excali--tool 'select)
       (excali-test--drag 305 305 105 105)
       (should (equal (excali--get loose 'frameId) (excali--get frame 'id)))
       (excali-test--drag 105 105 405 405)
       (should-not (excali--get loose 'frameId))))))

(ert-deftest excali-frame-test-delete-releases-children ()
  "Deleting a frame keeps its children, released and selected."
  (excali-frame-test--scene
   (let ((child (excali-test--rect 50 50)))
     (setq excali--elements (list child))
     (setq excali--tool 'frame)
     (excali-test--drag 20 20 200 200)
     (let ((frame (seq-find #'excali--frame-p excali--elements)))
       (excali--deselect)
       (excali--select (list frame))
       (excali-delete-selected)
       (should (eq (excali--get frame 'isDeleted) t))
       (should-not (excali--get child 'isDeleted))
       (should-not (excali--get child 'frameId))
       (should (equal excali--selection (list child)))))))

(ert-deftest excali-frame-test-never-coselected ()
  "Selecting a frame drops its children from the selection."
  (excali-frame-test--scene
   (let ((child (excali-test--rect 50 50)))
     (setq excali--elements (list child))
     (setq excali--tool 'frame)
     (excali-test--drag 20 20 200 200)
     (let ((frame (seq-find #'excali--frame-p excali--elements)))
       (excali--deselect)
       (excali--select (list frame child))
       (should (equal excali--selection (list frame)))
       ;; No rotation handle for frames.
       (should-not (assq 'rotation (excali--transform-handles
                                    (excali--element-box frame) 0.0 2 nil
                                    (not (plist-get (excali--transform-target) :rotation)))))))))

(provide 'excali-frame-test)
;;; excali-frame-test.el ends here
