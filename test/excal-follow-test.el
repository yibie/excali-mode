;;; excal-follow-test.el --- Labels and arrows follow their shapes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 yibie
;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'excal)
(require 'excal-test)

(defun excal-follow-test--center (e)
  "Return the center of element E's box."
  (excal--box-center (excal--element-box e)))

(defun excal-follow-test--near (a b &optional eps)
  "Return non-nil if conses A and B agree within EPS (default 0.5)."
  (let ((eps (or eps 0.5)))
    (and (< (abs (- (car a) (car b))) eps) (< (abs (- (cdr a) (cdr b))) eps))))

(defmacro excal-follow-test--with-label (&rest body)
  "Run BODY in a window-backed scene: BOX with bound text LABEL \"Hello\"."
  `(excal-test--in-window
    (excal--load-current-style nil)
    (let* ((box (excal--make-element "rectangle" 100 100 (cons 'width 120.0)
                                     (cons 'height 60.0) (cons 'strokeWidth 2)))
           (label (progn (setq excal--elements (list box))
                         (excal--add-bound-text box))))
      (excal--set-text label "Hello")
      (excal--refresh-bound-text box)
      ,@body)))

(ert-deftest excal-follow-test-label-moves-with-container ()
  "Dragging a container carries its label along."
  (excal-follow-test--with-label
   (should (excal-follow-test--near (excal-follow-test--center label)
                                    (excal-follow-test--center box)))
   (excal--select (list box))
   (excal-test--drag 150 130 190 160)
   (should (= (excal--get box 'x) 140.0))
   (should (excal-follow-test--near (excal-follow-test--center label)
                                    (excal-follow-test--center box)))
   ;; Nudging too.
   (excal-nudge-right-large)
   (should (excal-follow-test--near (excal-follow-test--center label)
                                    (excal-follow-test--center box)))))

(ert-deftest excal-follow-test-label-refits-on-resize ()
  "Resizing a container keeps its label centered inside."
  (excal-follow-test--with-label
   (excal--select (list box))
   ;; The se corner handle sits just outside (220, 160).
   (excal-test--drag 223 163 323 203)
   (should (> (excal--get box 'width) 200))
   (should (excal-follow-test--near (excal-follow-test--center label)
                                    (excal-follow-test--center box)))))

(ert-deftest excal-follow-test-delete-container-deletes-label ()
  "Deleting a container deletes its label."
  (excal-follow-test--with-label
   (excal--select (list box))
   (excal-delete-selected)
   (should (eq (excal--get label 'isDeleted) t))))

(ert-deftest excal-follow-test-arrow-label-follows-binding ()
  "An arrow's label moves when a bound shape moves the arrow."
  (excal-test--in-window
   (excal--load-current-style nil)
   (let* ((box (excal--make-element "rectangle" 300 100 (cons 'width 100.0)
                                    (cons 'height 60.0) (cons 'strokeWidth 2)))
          (arrow (excal--apply-current-style
                  (excal--make-element "arrow" 50 130 (cons 'points [[0.0 0.0] [240.0 0.0]])))))
     (excal--linear-extent arrow)
     (setq excal--elements (list box arrow))
     (excal--bind-end arrow 'end box '(295.0 . 130.0))
     (excal--update-arrow arrow)
     (let ((label (excal--add-bound-text arrow)))
       (excal--set-text label "go")
       (excal--refresh-bound-text arrow)
       (let ((before (excal-follow-test--center label)))
         (excal--select (list box))
         (excal--nudge 0 100)
         ;; The label sits on the re-routed arrow, so it moved down.
         (should (> (cdr (excal-follow-test--center label)) (+ (cdr before) 20))))))))

(ert-deftest excal-follow-test-text-side-resize-rewraps ()
  "Dragging a text element's side re-wraps it at a fixed width."
  (excal-test--in-window
   (excal--load-current-style nil)
   (let ((text (excal--make-text-element 10 10 "one two three four five")))
     (setq excal--elements (list text))
     (excal--select (list text))
     (let* ((g (excal--snapshot-geometry text))
            (w (excal--get text 'width))
            (h (excal--get text 'height)))
       (excal--resize-single text g 'e (cons (+ 10 (/ w 2)) 15.0))
       (should (eq (alist-get 'autoResize text) :false))
       (should (< (excal--get text 'width) w))
       (should (> (excal--get text 'height) h))
       (should (string-match-p "\n" (excal--get text 'text)))))))

(provide 'excal-follow-test)
;;; excal-follow-test.el ends here
