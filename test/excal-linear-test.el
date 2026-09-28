;;; excal-linear-test.el --- Point editor  -*- lexical-binding: t; -*-

(require 'ert)
(require 'excal)
(require 'excal-test)

(defmacro excal-linear-test--with-line (points &rest body)
  "Run BODY in a window-backed scene holding a selected line with POINTS."
  (declare (indent 1))
  `(excal-test--in-window
    (excal--load-current-style nil)
    (let ((line (excal--make-element "line" 10 10 (cons 'points ,points))))
      (excal--linear-extent line)
      (setq excal--elements (list line))
      (excal--select (list line))
      ,@body)))

(ert-deftest excal-linear-test-drag-endpoint-without-editing ()
  "A lone selected line's points can be dragged without edit mode."
  (excal-linear-test--with-line [[0.0 0.0] [100.0 0.0]]
    (excal-test--drag 110 10 110 60)
    (should (equal (excal--get line 'points) [[0.0 0.0] [100.0 50.0]]))
    (excal-test--drag 10 10 30 20)
    (should (equal (list (excal--get line 'x) (excal--get line 'y)) '(30.0 20.0)))
    (should (equal (excal--get line 'points) [[0.0 0.0] [80.0 40.0]]))))

(ert-deftest excal-linear-test-midpoint-inserts ()
  "Dragging a segment midpoint inserts a point there."
  (excal-linear-test--with-line [[0.0 0.0] [100.0 0.0]]
    (excal-test--drag 60 10 60 40)
    (should (equal (excal--get line 'points) [[0.0 0.0] [50.0 30.0] [100.0 0.0]]))))

(ert-deftest excal-linear-test-edit-mode ()
  "Edit mode selects points, appends with meta and deletes points."
  (excal-linear-test--with-line [[0.0 0.0] [100.0 0.0] [100.0 100.0]]
    (excal-return)
    (should (eq excal--editing-linear line))
    ;; No box or handles while editing.
    (should-not (excal--transform-target))
    (excal-test--drag 110 10 110 10)
    (excal-test--drag 110 110 110 110 '(shift))
    (should (equal (sort (copy-sequence excal--selected-points) #'<) '(1 2)))
    (excal-test--drag 200 50 200 50 '(meta))
    (should (= (length (excal--get line 'points)) 4))
    (setq excal--selected-points '(1 2))
    (excal-delete-selected)
    (should (equal (excal--get line 'points) [[0.0 0.0] [190.0 40.0]]))
    (should-not (excal--get line 'isDeleted))
    (excal-escape-dwim)
    (should-not excal--editing-linear)))

(ert-deftest excal-linear-test-press-elsewhere-leaves-editor ()
  "Pressing away from the edited line leaves edit mode."
  (excal-linear-test--with-line [[0.0 0.0] [100.0 0.0] [100.0 100.0]]
    (excal-edit-linear)
    (excal-test--drag 300 300 300 300)
    (should-not excal--editing-linear)
    (should-not excal--selection)))

(ert-deftest excal-linear-test-rotated-points-stay-put ()
  "Moving one point of a rotated line leaves the others on screen."
  (excal-linear-test--with-line [[0.0 0.0] [100.0 0.0] [100.0 100.0]]
    (excal--put line 'angle 0.5)
    (let* ((before (excal--linear-scene-points line))
           (moved (list (car before) (cadr before)
                        (cons (+ (car (nth 2 before)) 30) (cdr (nth 2 before))))))
      (excal--set-linear-scene-points line moved)
      (cl-mapc (lambda (a b)
                 (should (< (abs (- (car a) (car b))) 1e-9))
                 (should (< (abs (- (cdr a) (cdr b))) 1e-9)))
               (excal--linear-scene-points line) moved))))

(ert-deftest excal-linear-test-drag-arrow-end-binds ()
  "Dragging an arrow end onto a shape binds it."
  (excal-test--in-window
   (excal--load-current-style nil)
   (let ((box (excal--make-element "rectangle" 200 0 (cons 'width 100.0)
                                   (cons 'height 60.0) (cons 'strokeWidth 2)))
         (arrow (excal--make-element "arrow" 10 30 (cons 'points [[0.0 0.0] [80.0 0.0]]))))
     (excal--linear-extent arrow)
     (setq excal--elements (list box arrow))
     (excal--select (list arrow))
     (excal-test--drag 90 30 195 30)
     (should (equal (alist-get 'elementId (excal--get arrow 'endBinding))
                    (excal--get box 'id)))
     (should (< (abs (- (car (excal--arrow-point arrow 1)) 194.0)) 0.5)))))

(provide 'excal-linear-test)
;;; excal-linear-test.el ends here
