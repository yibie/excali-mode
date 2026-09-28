;;; excal-flowchart-test.el --- Flowchart creation and navigation  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Expected positions follow upstream's placeCluster: nodes copy the
;; parent's size, sit one 100px gap away along the direction, and the
;; cluster is centered on the parent across it, sliding off connected
;; nodes.

(require 'ert)
(require 'excal)
(require 'excal-test)

(defmacro excal-flowchart-test--with-node (var &rest body)
  "Run BODY in a scene holding a selected 100x100 rectangle bound to VAR."
  (declare (indent 1))
  `(excal-test--in-window
    (setq excal--elements nil excal--flowchart-pending nil excal--flowchart-navigator nil)
    (excal--load-current-style nil)
    (let ((,var (excal--make-element "rectangle" 0 0 (cons 'width 100.0) (cons 'height 100.0)
                                     (cons 'strokeColor "#e03131")
                                     (cons 'backgroundColor "#ffc9c9")
                                     (cons 'roundness '((type . 3))))))
      (setq excal--elements (list ,var))
      (excal--select (list ,var))
      (excal--history-reset)
      ,@body)))

(defun excal-flowchart-test--run (command)
  "Run COMMAND as the command loop would, hooks included."
  (let ((this-command command) (last-input-event ?x))
    (run-hooks 'pre-command-hook)
    (excal--flowchart-pre-command)
    (funcall command)
    (excal--commit)))

(defun excal-flowchart-test--nodes ()
  "Return the non-arrow elements after the first, in scene order."
  (seq-remove (lambda (e) (equal (excal--get e 'type) "arrow")) (cdr excal--elements)))

(defun excal-flowchart-test--xy (elements)
  "Return the (X Y) of ELEMENTS."
  (mapcar (lambda (e) (list (excal--get e 'x) (excal--get e 'y))) elements))

(ert-deftest excal-flowchart-test-create-right ()
  "Mod+Right adds a copy one gap to the right, joined by a bound elbow arrow."
  (excal-flowchart-test--with-node rect
    (excal-flowchart-test--run #'excal-flowchart-right)
    (let* ((node (car (excal-flowchart-test--nodes)))
           (arrow (seq-find #'excal--elbow-p excal--elements)))
      (should (equal (excal-flowchart-test--xy (list node)) '((200.0 0.0))))
      (should (equal (excal--get node 'type) "rectangle"))
      (should (= (excal--get node 'width) 100))
      (should (equal (excal--get node 'backgroundColor) "#ffc9c9"))
      (should (equal (excal--get node 'roundness) '((type . 3))))
      (should (equal (excal--binding-element-id arrow 'start) (excal--get rect 'id)))
      (should (equal (excal--binding-element-id arrow 'end) (excal--get node 'id)))
      (should (equal (excal--get arrow 'strokeColor) "#e03131"))
      (should (null (excal--get arrow 'startArrowhead)))
      ;; The arrow runs from the right edge of RECT to the left edge of NODE.
      (let ((points (let ((x (excal--get arrow 'x)) (y (excal--get arrow 'y)))
                      (mapcar (lambda (p) (cons (+ x (aref p 0)) (+ y (aref p 1))))
                              (excal--get arrow 'points)))))
        (should (< (abs (- (car (car points)) 100)) 10))
        (should (< (abs (- (car (car (last points))) 200)) 10)))
      ;; Pending: RECT stays selected and history holds.
      (should (equal excal--selection (list rect)))
      (should (= (length excal--undo-stack) 1)))))

(ert-deftest excal-flowchart-test-commit-selects-first-and-records-once ()
  "Another command commits: the first node is selected, one undo step."
  (excal-flowchart-test--with-node _rect
    (excal-flowchart-test--run #'excal-flowchart-right)
    (excal-flowchart-test--run #'excal-flowchart-right)
    (excal-flowchart-test--run #'ignore)
    (should (null excal--flowchart-pending))
    (should (equal excal--selection (list (car (excal-flowchart-test--nodes)))))
    (should (= (length excal--undo-stack) 2))
    (excal-undo)
    (should (= (length excal--elements) 1))))

(ert-deftest excal-flowchart-test-pointer-motion-keeps-pending ()
  "Moving the pointer does not release Mod."
  (excal-flowchart-test--with-node _rect
    (excal-flowchart-test--run #'excal-flowchart-right)
    (let ((this-command 'excal-mouse-move)
          (last-input-event '(mouse-movement nil)))
      (excal--flowchart-pre-command))
    (should excal--flowchart-pending)))

(ert-deftest excal-flowchart-test-repeat-grows-siblings ()
  "Repeating a direction grows a cluster that keeps shown nodes in place."
  (excal-flowchart-test--with-node _rect
    (excal-flowchart-test--run #'excal-flowchart-right)
    (excal-flowchart-test--run #'excal-flowchart-right)
    (should (equal (excal-flowchart-test--xy (excal-flowchart-test--nodes))
                   '((200.0 0.0) (200.0 200.0))))
    (excal-flowchart-test--run #'excal-flowchart-right)
    (should (equal (excal-flowchart-test--xy (excal-flowchart-test--nodes))
                   '((200.0 -200.0) (200.0 0.0) (200.0 200.0))))
    (should (= (length (seq-filter #'excal--elbow-p excal--elements)) 3))))

(ert-deftest excal-flowchart-test-direction-change-restarts ()
  "Another direction replaces the pending nodes with one there."
  (excal-flowchart-test--with-node rect
    (excal-flowchart-test--run #'excal-flowchart-right)
    (excal-flowchart-test--run #'excal-flowchart-right)
    (excal-flowchart-test--run #'excal-flowchart-down)
    (should (equal (excal-flowchart-test--xy (excal-flowchart-test--nodes))
                   '((0.0 200.0))))
    (should (= (length (excal--get rect 'boundElements)) 1))))

(ert-deftest excal-flowchart-test-avoids-connected-nodes ()
  "A second child slides past the first one along the cross axis."
  (excal-flowchart-test--with-node rect
    (excal-flowchart-test--run #'excal-flowchart-right)
    (excal-flowchart-test--run #'ignore)
    (excal--deselect)
    (excal--select (list rect))
    (excal-flowchart-test--run #'excal-flowchart-right)
    (should (equal (excal-flowchart-test--xy (excal-flowchart-test--nodes))
                   '((200.0 0.0) (200.0 200.0))))))

(ert-deftest excal-flowchart-test-sticky-note-keeps-base-height ()
  "Sticky notes clone as sticky notes."
  (excal-flowchart-test--with-node rect
    (excal--put rect 'type "stickynote")
    (excal--put rect 'baseHeight 100.0)
    (excal-flowchart-test--run #'excal-flowchart-left)
    (let ((node (car (excal-flowchart-test--nodes))))
      (should (equal (excal--get node 'type) "stickynote"))
      (should (equal (excal--get node 'baseHeight) 100.0))
      (should (equal (excal-flowchart-test--xy (list node)) '((-200.0 0.0)))))))

(ert-deftest excal-flowchart-test-joins-parent-frame ()
  "Nodes overlapping the parent's frame join it."
  (excal-flowchart-test--with-node rect
    (let ((frame (excal--make-element "frame" -50 -50 (cons 'width 600.0)
                                      (cons 'height 300.0) (cons 'name :null))))
      (setq excal--elements (list rect frame))
      (excal--put rect 'frameId (excal--get frame 'id))
      (excal-flowchart-test--run #'excal-flowchart-right)
      (should (seq-every-p (lambda (e) (equal (excal--get e 'frameId) (excal--get frame 'id)))
                           (remq frame excal--elements)))
      ;; Frame children sit below the frame.
      (should (eq (car (last excal--elements)) frame)))))

(ert-deftest excal-flowchart-test-navigate ()
  "Alt+Arrow walks along the links and cycles same-level nodes."
  (excal-flowchart-test--with-node rect
    (excal-flowchart-test--run #'excal-flowchart-right)
    (excal-flowchart-test--run #'excal-flowchart-right)
    (excal-flowchart-test--run #'ignore)
    (pcase-let ((`(,a ,b) (excal-flowchart-test--nodes)))
      (excal--deselect)
      (excal--select (list rect))
      (excal-flowchart-test--run #'excal-flowchart-navigate-right)
      (should (equal excal--selection (list a)))
      ;; From A, left leads back to RECT.
      (excal-flowchart-test--run #'excal-flowchart-navigate-left)
      (should (equal excal--selection (list rect)))
      ;; Repeating right from RECT cycles A, B.
      (excal-flowchart-test--run #'excal-flowchart-navigate-right)
      (excal-flowchart-test--run #'excal-flowchart-navigate-right)
      (should (equal excal--selection (list b))))))

(ert-deftest excal-flowchart-test-needs-one-node ()
  "Nothing happens without a single flowchart node selected."
  (excal-flowchart-test--with-node _rect
    (excal--deselect)
    (excal-flowchart-test--run #'excal-flowchart-right)
    (should (= (length excal--elements) 1))))

(provide 'excal-flowchart-test)
;;; excal-flowchart-test.el ends here
