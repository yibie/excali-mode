;;; excali-flowchart-test.el --- Flowchart creation and navigation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Expected positions follow upstream's placeCluster: nodes copy the
;; parent's size, sit one 100px gap away along the direction, and the
;; cluster is centered on the parent across it, sliding off connected
;; nodes.

(require 'ert)
(require 'excali)
(require 'excali-test)

(defmacro excali-flowchart-test--with-node (var &rest body)
  "Run BODY in a scene holding a selected 100x100 rectangle bound to VAR."
  (declare (indent 1))
  `(excali-test--in-window
    (setq excali--elements nil excali--flowchart-pending nil excali--flowchart-navigator nil)
    (excali--load-current-style nil)
    (let ((,var (excali--make-element "rectangle" 0 0 (cons 'width 100.0) (cons 'height 100.0)
                                     (cons 'strokeColor "#e03131")
                                     (cons 'backgroundColor "#ffc9c9")
                                     (cons 'roundness '((type . 3))))))
      (setq excali--elements (list ,var))
      (excali--select (list ,var))
      (excali--history-reset)
      ,@body)))

(defun excali-flowchart-test--run (command)
  "Run COMMAND as the command loop would, hooks included."
  (let ((this-command command) (last-input-event ?x))
    (run-hooks 'pre-command-hook)
    (excali--flowchart-pre-command)
    (funcall command)
    (excali--commit)))

(defun excali-flowchart-test--nodes ()
  "Return the non-arrow elements after the first, in scene order."
  (seq-remove (lambda (e) (equal (excali--get e 'type) "arrow")) (cdr excali--elements)))

(defun excali-flowchart-test--xy (elements)
  "Return the (X Y) of ELEMENTS."
  (mapcar (lambda (e) (list (excali--get e 'x) (excali--get e 'y))) elements))

(ert-deftest excali-flowchart-test-create-right ()
  "Mod+Right adds a copy one gap to the right, joined by a bound elbow arrow."
  (excali-flowchart-test--with-node rect
    (excali-flowchart-test--run #'excali-flowchart-right)
    (let* ((node (car (excali-flowchart-test--nodes)))
           (arrow (seq-find #'excali--elbow-p excali--elements)))
      (should (equal (excali-flowchart-test--xy (list node)) '((200.0 0.0))))
      (should (equal (excali--get node 'type) "rectangle"))
      (should (= (excali--get node 'width) 100))
      (should (equal (excali--get node 'backgroundColor) "#ffc9c9"))
      (should (equal (excali--get node 'roundness) '((type . 3))))
      (should (equal (excali--binding-element-id arrow 'start) (excali--get rect 'id)))
      (should (equal (excali--binding-element-id arrow 'end) (excali--get node 'id)))
      (should (equal (excali--get arrow 'strokeColor) "#e03131"))
      (should (null (excali--get arrow 'startArrowhead)))
      ;; The arrow runs from the right edge of RECT to the left edge of NODE.
      (let ((points (let ((x (excali--get arrow 'x)) (y (excali--get arrow 'y)))
                      (mapcar (lambda (p) (cons (+ x (aref p 0)) (+ y (aref p 1))))
                              (excali--get arrow 'points)))))
        (should (< (abs (- (car (car points)) 100)) 10))
        (should (< (abs (- (car (car (last points))) 200)) 10)))
      ;; Pending: RECT stays selected and history holds.
      (should (equal excali--selection (list rect)))
      (should (= (length excali--undo-stack) 1)))))

(ert-deftest excali-flowchart-test-commit-selects-first-and-records-once ()
  "Another command commits: the first node is selected, one undo step."
  (excali-flowchart-test--with-node _rect
    (excali-flowchart-test--run #'excali-flowchart-right)
    (excali-flowchart-test--run #'excali-flowchart-right)
    (excali-flowchart-test--run #'ignore)
    (should (null excali--flowchart-pending))
    (should (equal excali--selection (list (car (excali-flowchart-test--nodes)))))
    (should (= (length excali--undo-stack) 2))
    (excali-undo)
    (should (= (length excali--elements) 1))))

(ert-deftest excali-flowchart-test-pointer-motion-keeps-pending ()
  "Moving the pointer does not release Mod."
  (excali-flowchart-test--with-node _rect
    (excali-flowchart-test--run #'excali-flowchart-right)
    (let ((this-command 'excali-mouse-move)
          (last-input-event '(mouse-movement nil)))
      (excali--flowchart-pre-command))
    (should excali--flowchart-pending)))

(ert-deftest excali-flowchart-test-repeat-grows-siblings ()
  "Repeating a direction grows a cluster that keeps shown nodes in place."
  (excali-flowchart-test--with-node _rect
    (excali-flowchart-test--run #'excali-flowchart-right)
    (excali-flowchart-test--run #'excali-flowchart-right)
    (should (equal (excali-flowchart-test--xy (excali-flowchart-test--nodes))
                   '((200.0 0.0) (200.0 200.0))))
    (excali-flowchart-test--run #'excali-flowchart-right)
    (should (equal (excali-flowchart-test--xy (excali-flowchart-test--nodes))
                   '((200.0 -200.0) (200.0 0.0) (200.0 200.0))))
    (should (= (length (seq-filter #'excali--elbow-p excali--elements)) 3))))

(ert-deftest excali-flowchart-test-direction-change-restarts ()
  "Another direction replaces the pending nodes with one there."
  (excali-flowchart-test--with-node rect
    (excali-flowchart-test--run #'excali-flowchart-right)
    (excali-flowchart-test--run #'excali-flowchart-right)
    (excali-flowchart-test--run #'excali-flowchart-down)
    (should (equal (excali-flowchart-test--xy (excali-flowchart-test--nodes))
                   '((0.0 200.0))))
    (should (= (length (excali--get rect 'boundElements)) 1))))

(ert-deftest excali-flowchart-test-avoids-connected-nodes ()
  "A second child slides past the first one along the cross axis."
  (excali-flowchart-test--with-node rect
    (excali-flowchart-test--run #'excali-flowchart-right)
    (excali-flowchart-test--run #'ignore)
    (excali--deselect)
    (excali--select (list rect))
    (excali-flowchart-test--run #'excali-flowchart-right)
    (should (equal (excali-flowchart-test--xy (excali-flowchart-test--nodes))
                   '((200.0 0.0) (200.0 200.0))))))

(ert-deftest excali-flowchart-test-sticky-note-keeps-base-height ()
  "Sticky notes clone as sticky notes."
  (excali-flowchart-test--with-node rect
    (excali--put rect 'type "stickynote")
    (excali--put rect 'baseHeight 100.0)
    (excali-flowchart-test--run #'excali-flowchart-left)
    (let ((node (car (excali-flowchart-test--nodes))))
      (should (equal (excali--get node 'type) "stickynote"))
      (should (equal (excali--get node 'baseHeight) 100.0))
      (should (equal (excali-flowchart-test--xy (list node)) '((-200.0 0.0)))))))

(ert-deftest excali-flowchart-test-joins-parent-frame ()
  "Nodes overlapping the parent's frame join it."
  (excali-flowchart-test--with-node rect
    (let ((frame (excali--make-element "frame" -50 -50 (cons 'width 600.0)
                                      (cons 'height 300.0) (cons 'name :null))))
      (setq excali--elements (list rect frame))
      (excali--put rect 'frameId (excali--get frame 'id))
      (excali-flowchart-test--run #'excali-flowchart-right)
      (should (seq-every-p (lambda (e) (equal (excali--get e 'frameId) (excali--get frame 'id)))
                           (remq frame excali--elements)))
      ;; Frame children sit below the frame.
      (should (eq (car (last excali--elements)) frame)))))

(ert-deftest excali-flowchart-test-navigate ()
  "Alt+Arrow walks along the links and cycles same-level nodes."
  (excali-flowchart-test--with-node rect
    (excali-flowchart-test--run #'excali-flowchart-right)
    (excali-flowchart-test--run #'excali-flowchart-right)
    (excali-flowchart-test--run #'ignore)
    (pcase-let ((`(,a ,b) (excali-flowchart-test--nodes)))
      (excali--deselect)
      (excali--select (list rect))
      (excali-flowchart-test--run #'excali-flowchart-navigate-right)
      (should (equal excali--selection (list a)))
      ;; From A, left leads back to RECT.
      (excali-flowchart-test--run #'excali-flowchart-navigate-left)
      (should (equal excali--selection (list rect)))
      ;; Repeating right from RECT cycles A, B.
      (excali-flowchart-test--run #'excali-flowchart-navigate-right)
      (excali-flowchart-test--run #'excali-flowchart-navigate-right)
      (should (equal excali--selection (list b))))))

(ert-deftest excali-flowchart-test-needs-one-node ()
  "Nothing happens without a single flowchart node selected."
  (excali-flowchart-test--with-node _rect
    (excali--deselect)
    (excali-flowchart-test--run #'excali-flowchart-right)
    (should (= (length excali--elements) 1))))

(provide 'excali-flowchart-test)
;;; excali-flowchart-test.el ends here
