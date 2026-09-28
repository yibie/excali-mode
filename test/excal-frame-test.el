;;; excal-frame-test.el --- Frame behavior  -*- lexical-binding: t; -*-

(require 'ert)
(require 'excal)
(require 'excal-test)

(defmacro excal-frame-test--scene (&rest body)
  "Run BODY in a window-backed scene with a fresh style."
  `(excal-test--in-window
    (excal--load-current-style nil)
    (setq excal--elements nil excal--tool-locked nil excal--multi-element nil
          excal--grid-enabled nil excal--objects-snap-enabled nil)
    ,@body))

(ert-deftest excal-frame-test-new-frame-adopts-contents ()
  "A new frame adopts what is completely inside, whole groups only."
  (excal-frame-test--scene
   (let* ((inside (excal-test--rect 50 50))
          (partial (excal-test--rect 195 50))
          (g1 (excal-test--rect 60 100 (cons 'groupIds ["g"])))
          (g2 (excal-test--rect 300 100 (cons 'groupIds ["g"]))))
     (setq excal--elements (list inside partial g1 g2))
     (setq excal--tool 'frame)
     (excal-test--drag 20 20 200 200)
     (let ((frame (car (seq-filter #'excal--frame-p excal--elements))))
       (should frame)
       (should (equal (excal--get frame 'strokeColor) "#bbb"))
       (should (equal (excal--frame-children frame) (list inside)))
       ;; The child sits right below the frame in z-order.
       (should (eq (cadr (memq inside excal--elements)) frame))
       (should (equal (excal-frame-name frame) "Frame"))))))

(ert-deftest excal-frame-test-draw-inside-joins ()
  "Elements drawn inside a frame belong to it."
  (excal-frame-test--scene
   (setq excal--tool 'frame)
   (excal-test--drag 20 20 200 200)
   (let ((frame (car excal--elements)))
     (setq excal--tool 'rectangle)
     (excal-test--drag 50 50 80 80)
     (should (equal (excal--get (car (last excal--elements)) 'frameId)
                    (excal--get frame 'id)))
     (setq excal--tool 'rectangle)
     (excal-test--drag 300 300 330 330)
     (should-not (excal--get (car (last excal--elements)) 'frameId)))))

(ert-deftest excal-frame-test-moving-frame-moves-children ()
  "Dragging a frame by its border carries its children."
  (excal-frame-test--scene
   (let ((child (excal-test--rect 50 50)))
     (setq excal--elements (list child))
     (setq excal--tool 'frame)
     (excal-test--drag 20 20 200 200)
     (let ((frame (seq-find #'excal--frame-p excal--elements)))
       (excal--deselect)
       (excal--select (list frame))
       ;; Press inside the selected frame's box (its border would resize).
       (excal-test--drag 100 100 130 120)
       (should (= (excal--get frame 'x) 50.0))
       (should (= (excal--get child 'x) 80.0))))))

(ert-deftest excal-frame-test-drag-in-and-out ()
  "Dropping an element on a frame adds it; dragging it off removes it."
  (excal-frame-test--scene
   (let ((loose (excal-test--rect 300 300 (cons 'backgroundColor "#ffc9c9"))))
     (setq excal--elements (list loose))
     (setq excal--tool 'frame)
     (excal-test--drag 20 20 200 200)
     (let ((frame (seq-find #'excal--frame-p excal--elements)))
       (setq excal--tool 'select)
       (excal-test--drag 305 305 105 105)
       (should (equal (excal--get loose 'frameId) (excal--get frame 'id)))
       (excal-test--drag 105 105 405 405)
       (should-not (excal--get loose 'frameId))))))

(ert-deftest excal-frame-test-delete-releases-children ()
  "Deleting a frame keeps its children, released and selected."
  (excal-frame-test--scene
   (let ((child (excal-test--rect 50 50)))
     (setq excal--elements (list child))
     (setq excal--tool 'frame)
     (excal-test--drag 20 20 200 200)
     (let ((frame (seq-find #'excal--frame-p excal--elements)))
       (excal--deselect)
       (excal--select (list frame))
       (excal-delete-selected)
       (should (eq (excal--get frame 'isDeleted) t))
       (should-not (excal--get child 'isDeleted))
       (should-not (excal--get child 'frameId))
       (should (equal excal--selection (list child)))))))

(ert-deftest excal-frame-test-never-coselected ()
  "Selecting a frame drops its children from the selection."
  (excal-frame-test--scene
   (let ((child (excal-test--rect 50 50)))
     (setq excal--elements (list child))
     (setq excal--tool 'frame)
     (excal-test--drag 20 20 200 200)
     (let ((frame (seq-find #'excal--frame-p excal--elements)))
       (excal--deselect)
       (excal--select (list frame child))
       (should (equal excal--selection (list frame)))
       ;; No rotation handle for frames.
       (should-not (assq 'rotation (excal--transform-handles
                                    (excal--element-box frame) 0.0 2 nil
                                    (not (plist-get (excal--transform-target) :rotation)))))))))

(provide 'excal-frame-test)
;;; excal-frame-test.el ends here
