;;; excali-linear-test.el --- Point editor  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excali)
(require 'excali-test)

(defmacro excali-linear-test--with-line (points &rest body)
  "Run BODY in a window-backed scene holding a selected line with POINTS."
  (declare (indent 1))
  `(excali-test--in-window
    (excali--load-current-style nil)
    (let ((line (excali--make-element "line" 10 10 (cons 'points ,points))))
      (excali--linear-extent line)
      (setq excali--elements (list line))
      (excali--select (list line))
      ,@body)))

(ert-deftest excali-linear-test-drag-endpoint-without-editing ()
  "A lone selected line's points can be dragged without edit mode."
  (excali-linear-test--with-line [[0.0 0.0] [100.0 0.0]]
    (excali-test--drag 110 10 110 60)
    (should (equal (excali--get line 'points) [[0.0 0.0] [100.0 50.0]]))
    (excali-test--drag 10 10 30 20)
    (should (equal (list (excali--get line 'x) (excali--get line 'y)) '(30.0 20.0)))
    (should (equal (excali--get line 'points) [[0.0 0.0] [80.0 40.0]]))))

(ert-deftest excali-linear-test-midpoint-inserts ()
  "Dragging a segment midpoint inserts a point there."
  (excali-linear-test--with-line [[0.0 0.0] [100.0 0.0]]
    (excali-test--drag 60 10 60 40)
    (should (equal (excali--get line 'points) [[0.0 0.0] [50.0 30.0] [100.0 0.0]]))))

(ert-deftest excali-linear-test-edit-mode ()
  "Edit mode selects points, appends with meta and deletes points."
  (excali-linear-test--with-line [[0.0 0.0] [100.0 0.0] [100.0 100.0]]
    (excali-return)
    (should (eq excali--editing-linear line))
    ;; No box or handles while editing.
    (should-not (excali--transform-target))
    (excali-test--drag 110 10 110 10)
    (excali-test--drag 110 110 110 110 '(shift))
    (should (equal (sort (copy-sequence excali--selected-points) #'<) '(1 2)))
    (excali-test--drag 200 50 200 50 '(meta))
    (should (= (length (excali--get line 'points)) 4))
    (setq excali--selected-points '(1 2))
    (excali-delete-selected)
    (should (equal (excali--get line 'points) [[0.0 0.0] [190.0 40.0]]))
    (should-not (excali--get line 'isDeleted))
    (excali-escape-dwim)
    (should-not excali--editing-linear)))

(ert-deftest excali-linear-test-press-elsewhere-leaves-editor ()
  "Pressing away from the edited line leaves edit mode."
  (excali-linear-test--with-line [[0.0 0.0] [100.0 0.0] [100.0 100.0]]
    (excali-edit-linear)
    (excali-test--drag 300 300 300 300)
    (should-not excali--editing-linear)
    (should-not excali--selection)))

(ert-deftest excali-linear-test-rotated-points-stay-put ()
  "Moving one point of a rotated line leaves the others on screen."
  (excali-linear-test--with-line [[0.0 0.0] [100.0 0.0] [100.0 100.0]]
    (excali--put line 'angle 0.5)
    (let* ((before (excali--linear-scene-points line))
           (moved (list (car before) (cadr before)
                        (cons (+ (car (nth 2 before)) 30) (cdr (nth 2 before))))))
      (excali--set-linear-scene-points line moved)
      (cl-mapc (lambda (a b)
                 (should (< (abs (- (car a) (car b))) 1e-9))
                 (should (< (abs (- (cdr a) (cdr b))) 1e-9)))
               (excali--linear-scene-points line) moved))))

(ert-deftest excali-linear-test-drag-arrow-end-binds ()
  "Dragging an arrow end onto a shape binds it."
  (excali-test--in-window
   (excali--load-current-style nil)
   (let ((box (excali--make-element "rectangle" 200 0 (cons 'width 100.0)
                                   (cons 'height 60.0) (cons 'strokeWidth 2)))
         (arrow (excali--make-element "arrow" 10 30 (cons 'points [[0.0 0.0] [80.0 0.0]]))))
     (excali--linear-extent arrow)
     (setq excali--elements (list box arrow))
     (excali--select (list arrow))
     (excali-test--drag 90 30 195 30)
     (should (equal (alist-get 'elementId (excali--get arrow 'endBinding))
                    (excali--get box 'id)))
     (should (< (abs (- (car (excali--arrow-point arrow 1)) 194.0)) 0.5)))))

(provide 'excali-linear-test)
;;; excali-linear-test.el ends here
